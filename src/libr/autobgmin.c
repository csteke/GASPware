/*
 * tracknbgmin.c - Bidirectional Iterative Symmetric-Minimum Peak Stripping Background Filter
 *
 * Enhanced High-Accuracy, Multiplet-Aware Optimized Version.
 * Preserves the original Fortran 77-compatible C function prototype: autobgmin_
 *
 * Key Innovations & Optimizations:
 * 1. Two-Stage Bidirectional Architecture:
 *    - Stage 1: Iterative accumulation across max_iters sweeps. Forward sweep sweeps i
 *      from left to right, peeling buf[i] in-place against backwards chords. Backward sweep
 *      sweeps j from right to left, peeling buf_r[j] in-place against forwards chords.
 *    - Stage 2: Symmetric-minimum "Kill Spikes" pass executed directly on sb. Because
 *      photopeak tops have been stripped in Stage 1, chords on sb easily bridge across
 *      residual shoulder humps and multiplet valleys, pulling them down to the true continuum.
 * 2. Multiplet-Bridging Search Window Floor (win_floor = 16):
 *    - When small base windows (e.g. m = 4) are specified, win_floor prevents the search window
 *      from collapsing below the physical chord length needed to span dense multiplets (15-25 ch).
 *    - If the user specifies m >= 16 (e.g. m = 40), the user parameter takes precedence.
 * 3. Strict Physical Count Bounding (Zero Violations):
 *    - Strictly enforces that estimated continuum counts NEVER exceed raw spectrum counts
 *      in any channel: 0.0f <= sb0[i] <= sp0[i] for all i in [0, num_channels - 1].
 * 4. Automatic ADC Threshold / Discriminator Cutoff Management:
 *    - Automatically detects the active ADC threshold channel i_adc where valid counts begin.
 *    - Channels below i_adc are assigned sb0 = 0.0f.
 *    - Boundary reflection across start_ch prevents chords from probing into dead zero-count channels.
 * 5. Continuity Filter:
 *    - Gentle boundary-preserving 3-point binomial smoothing [0.25, 0.5, 0.25] eliminates
 *      discrete chord-switching kinks without smearing or broadening peak shoulders.
 * 6. Memory Safety & Standards:
 *    - Single contiguous allocation cleanly deallocated with free().
 *    - Double-precision IEEE 754 floating point arithmetic.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/*
 * Detect active ADC conversion threshold:
 * Identifies where spectrum counts sustainably rise above zero.
 */
static int detect_adc_threshold(const float *sp, int n)
{
    for (int i = 0; i < n - 6; i++) {
        /* Look for 4 consecutive non-zero channels with sum >= 12.0 counts */
        if (sp[i] > 0.0f && sp[i + 1] > 0.0f && sp[i + 2] > 0.0f) {
            double sum = (double)sp[i] + (double)sp[i + 1] + (double)sp[i + 2] + (double)sp[i + 3];
            if (sum >= 12.0) {
                return i;
            }
        }
    }
    return 0;
}

/*
 * Boundary-preserving 3-point binomial smoothing [0.25, 0.5, 0.25]:
 * Smooths out discrete chord-length switching steps while strictly preserving
 * the underlying continuum trend.
 */
static void smooth_array_3pt(double *arr, int start, int end)
{
    if (end <= start) return;
    int len = end - start + 1;
    double *tmp = (double*)malloc((size_t)len * sizeof(double));
    if (!tmp) return;

    for (int i = start; i <= end; i++) {
        double prev = (i > start) ? arr[i - 1] : arr[start];
        double curr = arr[i];
        double next = (i < end) ? arr[i + 1] : arr[end];
        tmp[i - start] = 0.25 * prev + 0.5 * curr + 0.25 * next;
    }

    for (int i = start; i <= end; i++) {
        arr[i] = tmp[i - start];
    }

    free(tmp);
}

void autobgmin_(const float *sp0, float *sb0, const int *n, const int *istart, const int *iend,
                const int *m, const int *itmax, const float *fstep)
{
    /* 1. Input parameter validation and default configurations */
    if (!sp0 || !sb0 || !n || *n <= 0) return;

    const int num_channels = *n;
    const int base_m = (m && *m >= 1) ? *m : 4;
    const int max_iters = (itmax && *itmax >= 1) ? *itmax : 2;
    const float step_factor = (fstep && *fstep >= 0.0f) ? *fstep : 0.05f;

    /* Minimum chord window floor to bridge dense multiplets */
    const int win_floor = (base_m >= 16) ? base_m : 16;

    /* 2. ADC conversion / threshold cutoff management */
    int i_adc = detect_adc_threshold(sp0, num_channels);
    int user_start = (istart && *istart > 0) ? *istart : 0;
    int start_ch = (user_start > i_adc) ? user_start : i_adc;
    int end_ch = (iend && *iend >= start_ch && *iend < num_channels) ? *iend : (num_channels - 1);

    /* 3. Padding dimensions with symmetric reflection boundaries */
    int max_win = base_m + (int)(step_factor * (end_ch - start_ch));
    if (max_win < win_floor) max_win = win_floor;
    const int pad_left = max_win + 64;
    const int pad_right = max_win + 64;
    const int lbuf = num_channels + pad_left + pad_right;

    /* Contiguous allocation for 6 double-precision buffers: sp, sb, buf, buf_r, sm, sm_r */
    double *raw_mem = (double*)calloc(6 * (size_t)lbuf, sizeof(double));
    if (!raw_mem) {
        fprintf(stderr, "ERROR: Cannot allocate memory in autobgmin_\n");
        return;
    }

    double *sp    = raw_mem;
    double *sb    = raw_mem + lbuf;
    double *buf   = raw_mem + 2 * lbuf;
    double *buf_r = raw_mem + 3 * lbuf;
    double *sm    = raw_mem + 4 * lbuf;
    double *sm_r  = raw_mem + 5 * lbuf;

    /* Copy raw data into active region */
    for (int i = 0; i < num_channels; i++) {
        sp[pad_left + i] = (double)sp0[i];
    }

    /* Mirror lower boundary across start_ch so chords never probe into dead 0-count zone */
    for (int k = 1; k <= pad_left; k++) {
        int mirror_ch = start_ch + k;
        if (mirror_ch > end_ch) mirror_ch = end_ch;
        sp[pad_left + start_ch - k] = sp[pad_left + mirror_ch];
    }

    /* Mirror upper boundary across end_ch */
    for (int k = 1; k <= pad_right; k++) {
        int mirror_ch = end_ch - k;
        if (mirror_ch < start_ch) mirror_ch = start_ch;
        sp[pad_left + end_ch + k] = sp[pad_left + mirror_ch];
    }

    for (int i = 0; i < lbuf; i++) {
        buf[i] = buf_r[i] = sp[i];
    }

    const int active_start = pad_left + start_ch;
    const int active_end = pad_left + end_ch;

    /* 4. Stage 1: Iterative Bidirectional Non-Linear Peak Stripping */
    for (int iter = 0; iter < max_iters; iter++) {
        /* Pre-smooth probe buffers using 3-point binomial kernel [0.25, 0.5, 0.25] */
        sm[0] = buf[0];
        sm_r[0] = buf_r[0];
        for (int i = 1; i < lbuf - 1; i++) {
            sm[i]   = 0.25 * buf[i - 1]   + 0.5 * buf[i]   + 0.25 * buf[i + 1];
            sm_r[i] = 0.25 * buf_r[i - 1] + 0.5 * buf_r[i] + 0.25 * buf_r[i + 1];
        }
        sm[lbuf - 1] = buf[lbuf - 1];
        sm_r[lbuf - 1] = buf_r[lbuf - 1];

        int j = active_end;
        for (int i = active_start; i <= active_end; i++, j--) {
            int delta_i = i - active_start;
            int delta_j = j - active_start;

            int win   = base_m + (int)(step_factor * delta_i);
            int win_r = base_m + (int)(step_factor * delta_j);
            if (win < win_floor)   win = win_floor;
            if (win_r < win_floor) win_r = win_floor;

            double rmin   = 0.5 * (sm[i - 1]   + sm[i + 1]);
            double rmin_r = 0.5 * (sm_r[j - 1] + sm_r[j + 1]);

            int r = (win < win_r) ? win : win_r;
            for (int ii = 2; ii <= r; ii++) {
                double chord   = 0.5 * (sm[i - ii]   + sm[i + ii]);
                if (chord < rmin) rmin = chord;

                double chord_r = 0.5 * (sm_r[j - ii] + sm_r[j + ii]);
                if (chord_r < rmin_r) rmin_r = chord_r;
            }

            if (win > r) {
                for (int ii = r + 1; ii <= win; ii++) {
                    double chord = 0.5 * (sm[i - ii] + sm[i + ii]);
                    if (chord < rmin) rmin = chord;
                }
            } else if (win_r > r) {
                for (int ii = r + 1; ii <= win_r; ii++) {
                    double chord_r = 0.5 * (sm_r[j - ii] + sm_r[j + ii]);
                    if (chord_r < rmin_r) rmin_r = chord_r;
                }
            }

            /* Peak stripping downwards */
            if (buf[i] > rmin)     buf[i] = rmin;
            if (buf_r[j] > rmin_r) buf_r[j] = rmin_r;
        }

        /* Accumulate peeled continuum increment into sb */
        for (int i = active_start; i <= active_end; i++) {
            sb[i] += 0.5 * (buf[i] + buf_r[i]);
            double rem = sp[i] - sb[i];
            buf[i] = buf_r[i] = (rem > 0.0) ? rem : 0.0;
        }

        /* Mirror padding for subsequent iteration */
        for (int k = 1; k <= pad_left; k++) {
            int mirror_ch = start_ch + k;
            if (mirror_ch > end_ch) mirror_ch = end_ch;
            buf[pad_left + start_ch - k] = buf_r[pad_left + start_ch - k] = buf[pad_left + mirror_ch];
        }
        for (int k = 1; k <= pad_right; k++) {
            int mirror_ch = end_ch - k;
            if (mirror_ch < start_ch) mirror_ch = start_ch;
            buf[pad_left + end_ch + k] = buf_r[pad_left + end_ch + k] = buf[pad_left + mirror_ch];
        }
    }

    /* 5. Stage 2: Second Non-Linear Symmetric-Minimum Pass ("Kill Spikes")
     * Evaluates chords directly on sb where primary peak tops are already removed,
     * stripping residual shoulder humps and multiplet valley overestimation. */
    for (int k = 1; k <= pad_left; k++) {
        int mirror_ch = start_ch + k;
        if (mirror_ch > end_ch) mirror_ch = end_ch;
        sb[pad_left + start_ch - k] = sb[pad_left + mirror_ch];
    }
    for (int k = 1; k <= pad_right; k++) {
        int mirror_ch = end_ch - k;
        if (mirror_ch < start_ch) mirror_ch = start_ch;
        sb[pad_left + end_ch + k] = sb[pad_left + mirror_ch];
    }
    for (int i = 0; i < lbuf; i++) {
        buf[i] = buf_r[i] = sb[i];
    }

    /* Pre-smooth sb for the Kill Spikes pass */
    sm[0] = buf[0];
    sm_r[0] = buf_r[0];
    for (int i = 1; i < lbuf - 1; i++) {
        sm[i]   = 0.25 * buf[i - 1]   + 0.5 * buf[i]   + 0.25 * buf[i + 1];
        sm_r[i] = 0.25 * buf_r[i - 1] + 0.5 * buf_r[i] + 0.25 * buf_r[i + 1];
    }
    sm[lbuf - 1] = buf[lbuf - 1];
    sm_r[lbuf - 1] = buf_r[lbuf - 1];

    int j = active_end;
    for (int i = active_start; i <= active_end; i++, j--) {
        int delta_i = i - active_start;
        int delta_j = j - active_start;

        int win   = base_m + (int)(step_factor * delta_i);
        int win_r = base_m + (int)(step_factor * delta_j);
        if (win < win_floor)   win = win_floor;
        if (win_r < win_floor) win_r = win_floor;

        double rmin   = 0.5 * (sm[i - 1]   + sm[i + 1]);
        double rmin_r = 0.5 * (sm_r[j - 1] + sm_r[j + 1]);

        int r = (win < win_r) ? win : win_r;
        for (int ii = 2; ii <= r; ii++) {
            double chord   = 0.5 * (sm[i - ii]   + sm[i + ii]);
            if (chord < rmin) rmin = chord;

            double chord_r = 0.5 * (sm_r[j - ii] + sm_r[j + ii]);
            if (chord_r < rmin_r) rmin_r = chord_r;
        }

        if (win > r) {
            for (int ii = r + 1; ii <= win; ii++) {
                double chord = 0.5 * (sm[i - ii] + sm[i + ii]);
                if (chord < rmin) rmin = chord;
            }
        } else if (win_r > r) {
            for (int ii = r + 1; ii <= win_r; ii++) {
                double chord_r = 0.5 * (sm_r[j - ii] + sm_r[j + ii]);
                if (chord_r < rmin_r) rmin_r = chord_r;
            }
        }

        if (buf[i] > rmin)     buf[i] = rmin;
        if (buf_r[j] > rmin_r) buf_r[j] = rmin_r;
    }

    for (int i = active_start; i <= active_end; i++) {
        sb[i] = 0.5 * (buf[i] + buf_r[i]);
    }

    /* 6. Gentle continuity filter to eliminate discrete chord-switching kinks */
    smooth_array_3pt(sb, active_start, active_end);

    /* 7. Strict physical constraint: output must NEVER exceed raw spectrum (sb <= sp)
     * and below ADC threshold, counts are 0.0f. */
    for (int i = 0; i < start_ch; i++) {
        sb0[i] = 0.0f;
    }
    for (int i = start_ch; i <= end_ch; i++) {
        double val = sb[pad_left + i];
        if (val > (double)sp0[i]) val = (double)sp0[i];
        if (val < 0.0) val = 0.0;
        sb0[i] = (float)val;
    }
    for (int i = end_ch + 1; i < num_channels; i++) {
        sb0[i] = sp0[i];
    }

    free(raw_mem);
}

