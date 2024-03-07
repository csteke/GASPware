#include <stdio.h>

#define TRUE      1
#define FALSE     0

/* Maximum Values intrinsic to Yale acquistion */
#define BLOCKSIZE 16384
/* ADC/QDC numbered 0-based, so numbered 0..(number of ADCs/QDCs - 1) */
#define MAX_ADC 64
#define MAX_QDC 64
#define QDC_RANGE 2048
#define ADC_RANGE 4096

#define VSN_CLOCK  24
#define IS_CLOCK_VSN(x)  ( x == 24 )

#define VSN_QDC_MIN  0
#define VSN_QDC_MAX  6
#define IS_QDC_VSN(x)  (( x >= 0 ) && ( x <= 6 ))

#define VSN_ADC_MIN   7
#define VSN_ADC_MAX  14
#define IS_ADC_VSN(x)  (( x >= 7 ) && ( x <= 14 ))

/* Binary Masks and number of bits to shift for extracting bit-pattern */
#define HIB_MSK 0x8000  /* ADC or QDC header: high bit */ 
#define HIB_SHR 15
#define VSN_MSK 0x00FF  /* ADC or QDC header: VSN number */ 

#define CWDC_MSK 0x7800 /* clock header word: data word count */
#define CWDC_SHR 11

#define QWDC_MSK 0x7800 /* QDC header word: data word count for QDC */
#define QWDC_SHR 11
#define QGRP_MSK 0x7800 /* QDC data word: group number */
#define QGRP_SHR 11
#define QDAT_MSK 0x07FF /* QDC data word: data */

#define AWDC_MSK 0x0F00 /* ADC header word: counts data words for ADC */
#define AWDC_SHR 8
#define ADCP_MSK 0x00FF /* ADC pattern word: pattern byte */
#define AGRP_MSK 0x7000 /* ADC data word: group number */
#define AGRP_SHR 12
#define ADAT_MSK 0x0FFF /* ADC data word: data*/


#define MODULE_BAD  -1
#define MODULE_CLOCK 0
#define MODULE_QDC 1
#define MODULE_ADC 2

/* event header variables */
static int header_type;

#define HEADER_QDC0 0
#define HEADER_CLOCK 1

/* event status */
#define EVENT_BOUNDARY -4
#define EVENT_BADMODULE -3
#define EVENT_NOHEADER -2
#define EVENT_FAILEDVALIDITY -1
#define EVENT_GOOD 0

#define LOOKUP_COAX    0
#define LOOKUP_CLOV   30
    /* Warning if you change LOOKUP_CLOV:  There may still be some hard-coded "3"s 
       in the clover processing, so check for this if you ever want to change LOOKUP_CLOV. */
#define LOOKUP_LEPS  150
#define LOOKUP_PART  200
#define LOOKUP_HEX   300

/* Maximum values of detector numbers allowed for in standard spectra */
#define MAX_HEX 32
#define MAX_PART 16
#define MAX_COAX 19
#define MAX_LEPS 5
#define MAX_CLOV 9
#define MAX_LEAF 4   /*would be 7 if sidechannels were used */


#define I2 short int

/*  Global variables from Yale package  ..............*/
int vsn;
int group_no;
int word_count;
int module_type;
int module_length;
static I2 *next_event;

/* Fold variables */
static int clock_fold, qdc_fold, adc_fold; 
static int coax_ti_fold, coax_en_fold;
static int clov_ti_fold, clov_en_fold; 
static int leps_ti_fold, leps_en_fold; 
static int part_ti_fold, part_en_fold; 
static int coax_fold, leaf_fold[MAX_CLOV];
static int part_fold, leps_fold; 
static int part_QDCs_used = 1;

/* Data words, Energies, Times, etc. */ 

static I2 clock_time_4096;        

static unsigned int clock_data_low, clock_data_high;

static I2 hex_data[MAX_HEX], hex_fold;

static I2 clov_en_id[MAX_ADC],clov_ti_id[MAX_QDC]; 
static I2 clov_en_data[MAX_ADC],clov_ti_data[MAX_QDC]; 

static I2 coax_en_id[MAX_COAX],coax_ti_id[MAX_COAX]; 
static I2 coax_en_data[MAX_COAX],coax_ti_data[MAX_COAX]; 

static I2 leps_en_id[MAX_LEPS],leps_ti_id[MAX_LEPS]; 
static I2 leps_en_data[MAX_LEPS],leps_ti_data[MAX_LEPS]; 

static I2 part_en_id[MAX_PART],part_ti_id[MAX_PART]; 
static I2 part_en_data[MAX_PART],part_ti_data[MAX_PART]; 

static I2 qdc_id[MAX_QDC], adc_id[MAX_ADC]; 
static I2 qdc_data[MAX_QDC], adc_data[MAX_ADC]; 
static int qdc_lookup[MAX_QDC], adc_lookup[MAX_ADC]; 

static I2 leaf_id[MAX_CLOV][MAX_LEAF]; 
static I2 leaf_energy[MAX_CLOV][MAX_LEAF],leaf_time[MAX_CLOV][MAX_LEAF];

static I2 clov_id[MAX_CLOV],clov_energy[MAX_CLOV],clov_time[MAX_CLOV];

static I2 coax_id[MAX_COAX],coax_energy[MAX_COAX],coax_time[MAX_COAX]; 

static I2 leps_id[MAX_LEPS],leps_energy[MAX_LEPS],leps_time[MAX_LEPS];

static I2 part_id[MAX_PART],part_energy[MAX_PART],part_time[MAX_PART];

/*  .................... END of globals from Yale pack */


/* Assignments of ADC and QDC channels to detectors */

/* Here the ADC and QDC channels are related to the detector numbers */
/* For each detector the corresponsing ADC or QDC number is given */
/* If a detector is not used enter -1 */


/* Clovers */

static int clov_adc_id[MAX_CLOV][MAX_LEAF]= 
{{4,5,6,7},{8,9,10,11},{12,13,14,15},{16,17,18,19},{24,25,26,27},
 {32,33,34,35},{36,37,38,39},{-1,-1,-1,-1},{-1,-1,-1,-1}};


/* EXAMPLE: {{20,21,22,23},{24,25,26,27},{-1,-1,-1,-1},{-1,-1,-1,-1},{-1,-1,-1,-1},
   {-1,-1,-1,-1},{-1,-1,-1,-1},{-1,-1,-1,-1},{-1,-1,-1,-1}};*/

static int clov_qdc_id[MAX_CLOV][MAX_LEAF]= 
{{8,9,10,11},{12,13,14,15},{32,33,34,35},{4,5,6,7},{44,45,46,47},
 {36,37,38,39},{40,41,42,43},{-1,-1,-1,-1},{-1,-1,-1,-1}};


/* EXAMPLE: {{20,21,22,23},{24,25,26,27},{-1,-1,-1,-1},{-1,-1,-1,-1},{-1,-1,-1,-1},
   {-1,-1,-1,-1},{-1,-1,-1,-1},{-1,-1,-1,-1},{-1,-1,-1,-1}};*/

/* Coaxial detectors */

static int coax_adc_id[MAX_COAX]= 
{3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};

/* EXAMPLE: {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};*/

static int coax_qdc_id[MAX_COAX]= 
{1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};

/* EXAMPLE: {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};*/

/* LEPS detectors */

static int leps_adc_id[MAX_LEPS]=
{-1,-1,-1,-1,-1};
static int leps_qdc_id[MAX_LEPS]=
{-1,-1,-1,-1,-1};

/* Particle detectors */

static int part_adc_id[MAX_PART]={40,41,42,43,44,45,46,47,-1,-1,-1,-1,-1,-1,-1,-1};
static int part_qdc_id[MAX_PART]={-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};

/* Were QDCs used for the particle detectors?
   If QDCs were *not* used, set particle_QDCs_used to 0.
   If QDCs were used, set particle_QDCs_used to 1. */

/* Hex detectors (for fold-spectrum) */

static int hex_qdc_id[MAX_HEX]={-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};




/***** GSORT interface function and required declarations *****/

#define MAXDET 256

typedef struct {
	short int Id;
	short int Energy;
	short int Time;
	short int dummy;
  }YALE_Detector;

typedef struct {
	short int Coax_fold;
	short int Clover_fold;
	short int Leps_fold;
	short int Part_fold;
	YALE_Detector   Coax[MAXDET];
	YALE_Detector   Clover[MAXDET];
	YALE_Detector   Leps[MAXDET];
	YALE_Detector   Part[MAXDET];
   } YALE_Event;


int format_event(I2 *ibuf, I2 *end);
short int *get_yalevent_( short int **, short int **, YALE_Event *, int *);


/****** End GSORT declarations *********************************/

/******  GSORT data decoding interface *************************/

 void init_yale_lookup_( void ) {
   
    int i,j;
    
    /* INITIALIZE ADC/QDC correlation lookup table */

    /* Zero out tables */
    for (i=0; i<MAX_QDC; i++) qdc_lookup[i] = 0;
    for (i=0; i<MAX_ADC; i++) adc_lookup[i] = 0;
    
    /* LOOKUP TABLE FOR COAXIAL DETECTORS*/
    for (i=0; i<MAX_COAX; i++){
      if((coax_qdc_id[i] != -1) && (coax_adc_id[i] != -1) ){
	qdc_lookup[coax_qdc_id[i]] = LOOKUP_COAX + i+1;
	adc_lookup[coax_adc_id[i]] = LOOKUP_COAX + i+1;
      }
    }
    
    /* LOOKUP TABLE FOR CLOVER DETECTORS*/
    for (i=0; i<MAX_CLOV; i++){
      for (j=0; j<MAX_LEAF; j++){
	if((clov_qdc_id[i][j] != -1) && (clov_adc_id[i][j] != -1) ){
	  qdc_lookup[clov_qdc_id[i][j]] = LOOKUP_CLOV + i*10 + j + 1;
	  adc_lookup[clov_adc_id[i][j]] = LOOKUP_CLOV + i*10 + j + 1;
	}
      }
    }
    
    /* LOOKUP TABLE FOR LEPS DETECTORS*/
    for (i=0; i<MAX_LEPS; i++){
      if((leps_qdc_id[i] != -1) && (leps_adc_id[i] != -1) ){
	qdc_lookup[leps_qdc_id[i]] = LOOKUP_LEPS +i+1;
	adc_lookup[leps_adc_id[i]] = LOOKUP_LEPS +i+1;
      }
    }
    
    /* LOOKUP TABLE FOR PARTICLE DETECTORS*/
    for (i=0; i<MAX_PART; i++){
      if (part_QDCs_used == 0) {
	/* particle QDCs not used */
	 if( TRUE && (part_adc_id[i] != -1)){
		adc_lookup[part_adc_id[i]] = LOOKUP_PART +i+1;
	 }
      } else {
	/* particle QDCs used */
	 if((part_qdc_id[i] != -1) && (part_adc_id[i] != -1)){
		adc_lookup[part_adc_id[i]] = LOOKUP_PART +i+1;
		qdc_lookup[part_qdc_id[i]] = LOOKUP_PART +i+1;
	 }
      }
    }
    
    /* LOOKUP TABLE FOR HEX_ARRAY*/
    for (i=0; i<MAX_HEX; i++){
      if(hex_qdc_id[i] != -1) {
	qdc_lookup[hex_qdc_id[i]] = LOOKUP_HEX+i+1;
      }
    }
    
}

short int *get_yalevent_( short int **buf_data, short int **buf_end, YALE_Event *ev, int *EvStatus){

   int ii,jj,kk;
   int tmpId, Seen[4*MAX_CLOV], dummy;

   *EvStatus = format_event( *buf_data, *buf_end );
   
/*   printf(" EvStatus: %d  \n",*EvStatus);fflush(stdout);*/
   if( *EvStatus == EVENT_GOOD ){
       for( ii = 0; ii < coax_fold; ii++ ){
       		ev->Coax[ii].Id = coax_id[ii];
		ev->Coax[ii].Energy = coax_energy[ii];
		ev->Coax[ii].Time = coax_time[ii];
	}
	ev->Coax_fold = coax_fold;
	
	kk = 0;
	for( ii = 0; ii < 4*MAX_CLOV; ii++ ) Seen[ii] = 0;
	for( ii = 0; ii < MAX_CLOV; ii++ ){
   		for( jj = 0; jj < leaf_fold[ii]; jj++){
			tmpId = ii*4+leaf_id[ii][jj];
			dummy = leaf_energy[ii][jj]+leaf_time[ii][jj];		
			if( (!Seen[tmpId]) && dummy ){
				ev->Clover[kk].Id = tmpId;
				ev->Clover[kk].Energy = leaf_energy[ii][jj];
				ev->Clover[kk].Time = leaf_time[ii][jj];
				Seen[tmpId] = 1; kk++ ;
			}
   		}
	}
	ev->Clover_fold = kk;
	
       for( ii = 0; ii < leps_fold; ii++ ){
       		ev->Leps[ii].Id = leps_id[ii];
		ev->Leps[ii].Energy = leps_energy[ii];
		ev->Leps[ii].Time = leps_time[ii];
	}
	ev->Leps_fold = leps_fold;
	
       for( ii = 0; ii < part_fold; ii++ ){
       		ev->Part[ii].Id = part_id[ii];
		ev->Part[ii].Energy = part_energy[ii];
		ev->Part[ii].Time = part_time[ii];
	}
	ev->Part_fold = part_fold;
    }
    
    switch ( *EvStatus ){
    	case EVENT_GOOD:  *EvStatus = 1;
			   if( (ev->Coax_fold + ev->Clover_fold + ev->Leps_fold + ev->Part_fold) == 0)*EvStatus = -1;
			   break; 
	case EVENT_BOUNDARY:  *EvStatus = 0; break; 
	default :  *EvStatus = -1; break; 
    }
/*    */
    return next_event;
}



/*  Functions from Yale package ............................................
 *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
 *   read_module_header() -- subroutine that reads header for a single CAMAC module from the buffer
 *   returns: description of module (MODULE_BAD, MODULE_CLOCK, MODULE_QDC, MODULE_ADC)
 *   sets global variables:
 *      vsn -- vsn of module
 *      group_no -- for QDC, first group number (to allow check for QDC 0)
 *      word_count -- data words in module
 *      module_length -- number of words in module (= header length + word count)                     
 */

int read_module_header (I2* ibuf, I2* end) {


  /* Check high bit and extract VSN */
  vsn = *ibuf & VSN_MSK;

  if ( ((*ibuf & HIB_MSK) >> HIB_SHR) != 1 ) {
#ifdef DEBUG
    printf ("High bit not set.\n");
#endif
    return MODULE_BAD;
  };

  /*********************************/
  /* identify clock                */
  /*********************************/  
  if (vsn == 24) {   
    word_count = (*ibuf & CWDC_MSK) >> CWDC_SHR;
    module_length = 1 + word_count;
    if(word_count != 2) 
       return MODULE_BAD;
    return MODULE_CLOCK;
  }
  /*********************************/
  /* identify QDC                  */
  /*********************************/  
  else if ((vsn >= 0) && (vsn <= 6 )) {  
    word_count = (*ibuf & QWDC_MSK) >> QWDC_SHR;
    module_length = 1 + word_count;
    if(word_count > 16) 
       return MODULE_BAD;
    /* get group number so main loop can identify QDC0 */
    group_no = (*(ibuf + 1) & QGRP_MSK) >> QGRP_SHR;   
    /*    if((vsn == 0) && (group_no == 0) )
	  group_no = (*(ibuf + 2) & QGRP_MSK) >> QGRP_SHR;   */
    return MODULE_QDC;

  } 
  /*********************************/
  /* identify ADC                  */
  /*********************************/
  else if ((vsn >= 7) && (vsn <= 20 )) {   /* module is an adc */
    word_count = (*ibuf & AWDC_MSK) >> AWDC_SHR;
    module_length = 2 + word_count;
    
    if(word_count > 8) 
      return MODULE_BAD;

    return MODULE_ADC;
  } 
  /*********************************/
  /* identify unknown module       */
  /*********************************/
  else {
    return MODULE_BAD;
  }

}
/*................................ END of read_module_header */

/*
 *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
 *   read_module_data() -- subroutine that reads data for a single CAMAC module from the buffer
 *   returns: 0 
 *   implicit parameters: module_type, vsn
 *   sets global variables: 
 *      clock_fold, qdc_fold, adc_fold,  various data arrays
 */

int read_module_data(ibuf, end) I2 *ibuf,*end; {
  int i;
  
  /*********************************/
  /* process clock                 */
  /*********************************/  
  if (module_type == MODULE_CLOCK) {  
    if ((ibuf+word_count) < end) {
      ibuf++;
      clock_data_low = *ibuf;
      ibuf++;
      clock_data_high = *ibuf;
    };
    clock_fold++;

  }
  /*********************************/
  /* process QDC                   */
  /*********************************/  
  else if (module_type == MODULE_QDC) {  
    for (i=0; i<word_count; i++){
      ibuf++;
      qdc_id[qdc_fold]=((*ibuf & QGRP_MSK) >> QGRP_SHR) + 16 * vsn;
      qdc_data[qdc_fold]=(*ibuf & QDAT_MSK);
#ifdef DEBUG
      printf("qdc-id: %5d , qdc-data: %5d\n", qdc_id[qdc_fold], qdc_data[qdc_fold]);
#endif
      
      /* Check range of ID and DATA, then increment Raw spectra/fold*/
      if(((qdc_data[qdc_fold] > 0) && (qdc_data[qdc_fold] < QDC_RANGE)) &&
         ((qdc_id[qdc_fold] >= 0) && (qdc_id[qdc_fold] < MAX_QDC))) qdc_fold++;
    }
    
  } 
  /*********************************/
  /* process ADC                   */
  /*********************************/
  else if (module_type == MODULE_ADC) {   /* module is an adc */

    ibuf++;  /* skip pattern word */
    for (i=0; i<word_count; i++){
      ibuf++;
      adc_id[adc_fold]=((*ibuf & AGRP_MSK) >> AGRP_SHR) + 8 * (vsn-7);
      adc_data[adc_fold]=(*ibuf & ADAT_MSK);
#ifdef DEBUG
      printf("adc-id: %5d , adc-data: %5d\n", adc_id[adc_fold], adc_data[adc_fold]);
#endif

      /* Check range of ID and DATA, then increment Raw spectra/fold*/
      if(((adc_data[adc_fold] > 0) && (adc_data[adc_fold] < ADC_RANGE)) &&
         ((adc_id[adc_fold] >= 0) && (adc_id[adc_fold] < MAX_ADC))) adc_fold++;
    }
  }
  return 0;
}
/*................................ END of read_module_data */

/*
 *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
 *   format_event() -- This subroutine provides the general event formatting  for the
 *   WNSL acquisition system.
 *   returns: integer status of event (EVENT_BOUNDARY, EVENT_FAILEDVALIDITY, EVENT_BADMODULE, EVENT_GOOD)
 *   sets global variables:
 *       next_event -- pointer to first word of next event
 *<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
 */

int format_event(I2 *ibuf, I2 *end) 
{
  
  int i, j, diff;
  int module_count;
  I2 id, det_id, t_id, e_id;
  I2 a,b,fold;
  
  diff=end-ibuf;
#ifdef DEBUG
  printf("\n***** new data block ***** ibuf: %5d end: %5d diff: %5d \n", ibuf, end, diff);
#endif
  
  /*********************************/
  /* initialize for event          */
  /*********************************/
  
  /* reinitialize for beginning of event */
  clock_fold=0;
  qdc_fold=0;
  adc_fold=0;
  
  /*********************************/
  /* loop through modules in event */
  /*********************************/


  module_count = 0;
 next_module:


  while(*ibuf == 0) {
  	ibuf++;
	if (ibuf > end)return EVENT_BOUNDARY;
  }
  /* avoid overrunning block boundary when identifying module */  
  if (ibuf > end)return EVENT_BOUNDARY;
  
  
  /* identify next module */    
  module_count++;
  module_type = read_module_header(ibuf,end);
#ifdef DEBUG
  printf("module: ibuf %i, vsn %i, type %i\n", (int) ibuf, vsn, module_type);
#endif
  
  /* if bad module, abort event */
  if (module_type == MODULE_BAD) {
#ifdef DEBUG
    printf ("Bad event  (Press enter...) *****************\n");
    getchar();
#endif 
    next_event = ibuf + 1;   /* next event: try next word */
    return EVENT_BADMODULE;
  };
  
  
  /* check whether module's data will overrun block boundary  */
  
  if (ibuf + module_length - 1 > end) return EVENT_BOUNDARY;

  /* read module's data */
  if (module_count == 1) {
    /*****************************************
     * First module can be:                
     *   clock -- fine (starts event)
     *   QDC 0 -- fine (starts event if no clock)
     *   QDC non-0, adc -- BAD EVENT
     *****************************************/
#ifdef DEBUG
  printf("group_no: %5d \n",group_no);
#endif
    
    if (module_type == MODULE_CLOCK) {
      header_type = HEADER_CLOCK;
      read_module_data (ibuf, end);
    }
    else if ((module_type == MODULE_QDC) && (group_no == 0)) {   /* change from 1*/
      header_type = HEADER_QDC0;
      read_module_data (ibuf, end);
#ifdef DEBUG
  printf("\n***** event start recognized ***** \n");
#endif
    }
    else {
      /* This was not a header.  Skip module and quit as bad event.*/
      next_event = ibuf + module_length;      /* next event: next module */
      return EVENT_NOHEADER;
    }
  }
  else {
    /*****************************************
     * Later modules can be:                
     *   clock -- EVENT OVER
     *   QDC 0 -- if does *not* immediately follow clock, EVENT OVER
     *            if does immediately follow clock, fine (completes expected sequence: clock + qdc0)
     *   QDC non-0, adc -- fine
     *****************************************/
    if (module_type == MODULE_CLOCK) goto end_of_modules;
    else if (module_type == MODULE_QDC) {
      /* qdc0 is next event header unless it immediately follows a clock */
      if (group_no == 0)     /* change from 1*/
	if (!( (module_count == 2) && (header_type == HEADER_CLOCK))) goto end_of_modules;

      read_module_data(ibuf,end);
    }
    else if (module_type == MODULE_ADC) read_module_data(ibuf,end);
  }
  
  /* module was good, so advance ibuf to point to next module */
  ibuf += module_length;	 

  goto next_module;

 end_of_modules:
  
#ifdef DEBUG
  printf("Module is header of next event.\n");
#endif
  
  next_event = ibuf;           /* all modules were good, so next event will start with next module */

  
#ifdef DEBUG
  printf("*************** validity checks ****************\n");
#endif
  
  /* Perform overall validity checks on event */          
  
  if ((qdc_fold >= MAX_QDC-17) || (adc_fold >=MAX_ADC-9)) return EVENT_FAILEDVALIDITY;
  
#ifdef DEBUG
  printf("Passed.\n");
#endif

  /*********************************/
  /* format event                  */
  /*********************************/
  
#ifdef DEBUG
  printf("*************** process clock ****************\n");
#endif
  
  if (clock_fold == 1) {
    
    /* put clock time in range 0..4095 by taking 12 most significant bits */       
    /* In the resulting matrix,  */
    /*          time/channel  = time/tick * ticks/channel = T * 2^20 */
    /*        where T is the clock oscillator period (= 1/frequency) */

    clock_time_4096 = clock_data_high >> 4;
    
#ifdef DEBUG
    printf("clock time: %i (full scale 4095) \n", clock_time_4096);
#endif
  }

  /***************************************************/
  /* Correlate ADC/QDC data to logical detector      */
  /***************************************************/

  
#ifdef DEBUG
  printf("*************** detector identification ****************\n");
#endif

  /***************************************************/
  /* Now all QDC data is correlated to its detectors */
  /***************************************************/

  coax_ti_fold = clov_ti_fold = leps_ti_fold = hex_fold = part_ti_fold = 0;
  for (i=0; i<qdc_fold; i++){
    id=qdc_id[i];
    det_id = qdc_lookup[id];
#ifdef DEBUG
    printf("qdc_id: %5d - det_id: %5d \n", id, det_id);
#endif
    if ((det_id > 0) && (det_id <= MAX_COAX)) {          /* COAX time */
      if((qdc_data[i] >0) && (qdc_data[i] < QDC_RANGE)){
	coax_ti_data[coax_ti_fold] = qdc_data[i];
	coax_ti_id[coax_ti_fold] = det_id;
#ifdef DEBUG
	printf("hit: %5d, coax-time: %5d \n",coax_ti_fold,coax_ti_data[coax_ti_fold] );
#endif
	coax_ti_fold++;
      }
    }
    
    else if ((det_id > LOOKUP_CLOV) && (det_id < LOOKUP_LEPS)) {        /* CLOVER time*/
      if((qdc_data[i] >0) && (qdc_data[i] < QDC_RANGE)) {
	clov_ti_data[clov_ti_fold] = qdc_data[i];
	clov_ti_id[clov_ti_fold] = det_id;
#ifdef DEBUG
	printf("hit: %5d, clov-time: %5d \n",clov_ti_fold,clov_ti_data[clov_ti_fold] );
#endif
	clov_ti_fold++;
      }
    }
    
    else if ((det_id > LOOKUP_LEPS) && (det_id < LOOKUP_PART)) {       /* LEPS time*/
      leps_ti_data[leps_ti_fold] = qdc_data[i];
      leps_ti_id[leps_ti_fold] = det_id;
#ifdef DEBUG
      printf("hit: %5d, coax-time: %5d \n",leps_ti_fold,leps_ti_data[leps_ti_fold] );
#endif
      leps_ti_fold++;
    }
    
    else if ((det_id > LOOKUP_PART) && (det_id < LOOKUP_HEX)) {       /* Particle time*/
      part_ti_data[part_ti_fold] = qdc_data[i];
      part_ti_id[part_ti_fold] = det_id;
#ifdef DEBUG
      printf("hit: %5d, coax-time: %5d \n",part_ti_fold,part_ti_data[part_ti_fold] );
#endif
      part_ti_fold++;
    }
    
    else if (det_id > LOOKUP_HEX) {                          /* HEX Array energies*/
      if((qdc_data[i] >0) && (qdc_data[i] < QDC_RANGE)){    
	hex_data[hex_fold]= qdc_data[i];
	hex_fold++;
      }
    }
  }
#ifdef DEBUG
  printf("****Time folds *********\n");
  printf("coax-time-fold: %5d \n", coax_ti_fold);
  printf("clov-time-fold: %5d \n", clov_ti_fold);
#endif

  
  /***************************************************/
  /* Now all ADC_data is correlated to its detectors */
  /***************************************************/
  coax_en_fold = clov_en_fold = leps_en_fold = part_en_fold = 0;
  
  for (i=0; i<adc_fold; i++) {
    id=adc_id[i];
    det_id = adc_lookup[id];
    
#ifdef DEBUG
    printf("adc_id: %5d - det_id: %5d \n", id, det_id);
#endif
    if ((det_id > 0) && (det_id <= MAX_COAX)) {                    /* COAX energy */
      if((adc_data[i] >0) && (adc_data[i] < ADC_RANGE)){
	coax_en_data[coax_en_fold] = adc_data[i];
	coax_en_id[coax_en_fold] = det_id;
#ifdef DEBUG
	printf("hit: %5d, coax-energy: %5d \n",coax_en_fold,coax_en_data[coax_en_fold] );
#endif
	
	coax_en_fold++;
      }
    }
    
    else if ((det_id > LOOKUP_CLOV) && (det_id < LOOKUP_LEPS)) {                 /* CLOVER energy*/
      if((adc_data[i] >0) && (adc_data[i] < ADC_RANGE)){
	clov_en_data[clov_en_fold] = adc_data[i];
	clov_en_id[clov_en_fold] = det_id;
#ifdef DEBUG
	printf("hit: %5d, clov-energy: %5d \n",clov_en_fold,clov_en_data[clov_en_fold] );
#endif
	clov_en_fold++;
	
      }
    }
    
    else if ((det_id > LOOKUP_LEPS) && (det_id < LOOKUP_PART)) {                /* LEPS energy*/
      if((adc_data[i] >0) && (adc_data[i] < ADC_RANGE)){
	leps_en_data[leps_en_fold] = adc_data[i];
	leps_en_id[leps_en_fold] = det_id;
#ifdef DEBUG
	printf("hit: %5d, leps-energy: %5d \n",leps_en_fold,leps_en_data[leps_en_fold] );
#endif
	
	leps_en_fold++;
	
      }
    }
    
    else if ((det_id > LOOKUP_PART) && (det_id <LOOKUP_HEX)) {                    /* particle detector */
      if ((adc_data[i] > 0) && (adc_data[i]< ADC_RANGE)){
	part_en_data[part_en_fold] = adc_data[i];
	part_en_id[part_en_fold] = det_id;

	part_en_fold++;

      }
    }
  }
  
#ifdef DEBUG
  printf("**** Energy folds *********\n");
  printf("coax-energy-fold: %5d \n", coax_en_fold);
  printf("clov-energy-fold: %5d \n", clov_en_fold);
#endif

  /*********************************/
  /*  CORRELATE ENERGIES AND TIMES */
  /*********************************/

  
  
  /******************/
  /* LEPS detectors */
  /******************/

  leps_fold = 0;
  for(i=0; i<leps_ti_fold; i++) {
    t_id = leps_ti_id[i];
    for(j=0; j<leps_en_fold; j++) {
      e_id = leps_en_id[j];
      if((t_id == e_id)) {
	leps_id[leps_fold] = t_id - LOOKUP_LEPS -1;    /* leps_id goes from 0 to MAX_LEPS-1*/
	leps_energy[leps_fold] = leps_en_data[j];
	
	if((leps_ti_data[i] > 0) &&
	   (leps_ti_data[i] < QDC_RANGE) &&
	   (leps_energy[leps_fold] > 0) &&
	   (leps_energy[leps_fold] < ADC_RANGE)) {
	  leps_time[leps_fold] = leps_ti_data[i];
	  
#ifdef DEBUG
	  printf("**** LEPS Detectors *********\n");
	  printf("ID: %5d Energy: %5d Time: %5d \n",leps_id[leps_fold],leps_energy[leps_fold], leps_time[leps_fold]);
#endif
	  leps_fold++;
	  
	  leps_ti_id[i] = -1;   /* Avoid to have it twice in the event */
	  leps_en_id[j] = -2;
	  j = leps_en_fold + 1;
	}
      }
    }
  }
  

  
  /***********************/
  /* PARTICLE detectors  */
  /***********************/

  if (part_QDCs_used == 1) {

    /*************************************/
    /* PARTICLE detectors -- QDCs used   */
    /*************************************/

    part_fold = 0;
    for(i=0; i<part_ti_fold; i++) {
      t_id = part_ti_id[i];
      for(j=0; j<part_en_fold; j++) {
        e_id = part_en_id[j];
        if((t_id == e_id)) {
       	part_id[part_fold] = t_id-LOOKUP_PART-1; /* part_id goes from 0 to MAX_PART-1*/
       	part_energy[part_fold] = part_en_data[j];
       	
       	if((part_ti_data[i] > 0) &&
       	   (part_ti_data[i] < QDC_RANGE) &&
       	   (part_energy[part_fold] > 0) &&
       	   (part_energy[part_fold] < ADC_RANGE)) {
       	  part_time[part_fold] = part_ti_data[i];
       	  
#ifdef DEBUG
       	  printf("**** PARTICLE Detectors *********\n");
       	  printf("ID: %5d Energy: %5d Time: %5d \n",part_id[part_fold],part_energy[part_fold], part_time[part_fold]);
#endif
       	  part_fold++;
       	  
       	}
        }
      }
    }
  
  } else {
      
    /***********************************************/
    /* PARTICLE detectors -- when QDCs *not* used  */
    /***********************************************/
    /* adapted from code above by removing loop over particle times */

    part_fold = 0;
      for(j=0; j<part_en_fold; j++) {
        e_id = part_en_id[j];
       	part_id[part_fold] = e_id - LOOKUP_PART - 1; /* part_id goes from 0 to MAX_PART-1*/
       	part_energy[part_fold] = part_en_data[j];
       	

       	if((part_energy[part_fold] > 0) &&
       	   (part_energy[part_fold] < ADC_RANGE)) {
       	  part_time[part_fold] = 0;    /* put 0 as time in lieu of an actual value */
       	  
#ifdef DEBUG
       	  printf("**** PARTICLE Detectors *********\n");
       	  printf("ID: %5d Energy: %5d Time: %5d \n",part_id[part_fold],part_energy[part_fold], part_time[part_fold]);
#endif
       	  part_fold++;
       	}
        }
  } /* no QDCs */


  
  /******************/
  /* COAX detectors */
  /******************/

  coax_fold = 0;
  for(i=0; i<coax_ti_fold; i++) {
    t_id = coax_ti_id[i];
    for(j=0; j<coax_en_fold; j++) {
      e_id = coax_en_id[j];
      if((t_id == e_id)) {
	coax_id[coax_fold] = t_id - LOOKUP_COAX - 1;  /* coax_id goes from 0 to MAX_COAX-1*/
	coax_energy[coax_fold] = coax_en_data[j];
	
	if((coax_ti_data[i] > 0) &&
	   (coax_ti_data[i] < QDC_RANGE) &&
	   (coax_energy[coax_fold] > 0) &&
	   (coax_energy[coax_fold] < ADC_RANGE)) {
	  coax_time[coax_fold] = coax_ti_data[i];
	  
#ifdef DEBUG
	  printf("**** Coaxial Detectors *********\n");
	  printf("ID: %5d Energy: %5d Time: %5d \n",coax_id[coax_fold],coax_energy[coax_fold], coax_time[coax_fold]);
#endif
	  coax_fold++;
	  
	  coax_ti_id[i] = -1;   /* Avoid to have it twice in the event */
	  coax_en_id[j] = -2;
	  j = coax_en_fold + 1;
	}
      }
    }
  }
  


  /********************/
  /* CLOVER detectors */
  /********************/
  
  /* Initialize Clover variables */

  for(i=0;i<MAX_CLOV;i++){
    leaf_fold[i]=0;
    clov_energy[i] = clov_id[i] = clov_time[i] = 0;
    for(j=0;j<MAX_LEAF;j++){
      leaf_id[i][j]=leaf_energy[i][j]=leaf_time[i][j]=0;
    }
  }
  
  for(i=0; i<clov_ti_fold; i++) {
    t_id = clov_ti_id[i];
    for(j=0; j<clov_en_fold; j++) {
      e_id = clov_en_id[j];
      if(t_id == e_id){                       /* Energy an Time present */
	a      = (t_id / 10);
	b      = t_id - (10 * a) - 1;      /* leaf_id goes from 0 to (MAX_LEAF-1)) */
	det_id = (a - 3)*4 + b;                /* Spectrum-Id from 0 to (MAX_CLOV*MAX_LEAF)-1 */
#ifdef DEBUG
	printf("**** Clover ID *********\n");
	printf("det_id: %5d a: %5d b: %5d spec-id: %5d \n",t_id,a,b,det_id);
#endif
	id = a - 3;                        /* clover_id goes from 0 to (MAX_CLOV-1))*/
	
	if((id >= 0) && (id < MAX_CLOV) && (b >= 0) && (b < MAX_LEAF)){
	  fold = leaf_fold[id];
#ifdef DEBUG
	  printf("clover-id: %5d leaf_id: %5d fold: %5d\n",id,b,fold);
#endif
	  if((clov_ti_data[i] > 0) &&
	     (clov_ti_data[i] < QDC_RANGE) &&
	     (clov_en_data[i]> 0 ) &&
	     (clov_en_data[i]< ADC_RANGE )){
	    leaf_id[id][fold]     = b;         /* leaf_id goes from 0 to (MAX_LEAF-1)) */
	    leaf_energy[id][fold] = clov_en_data[j];
	    leaf_time[id][fold]   = clov_ti_data[i];
	    
#ifdef DEBUG
	    printf("**** Clover Leafs *********\n");
	    printf("ID: %5d Energy: %5d Time: %5d \n",leaf_id[id][fold],leaf_energy[id][fold], leaf_time[id][fold]);
#endif
	    leaf_fold[id]++;

	    clov_ti_id[i] = -1;   /* Avoid to have it twice in the event */
	    clov_en_id[j] = -2;
	    j = clov_en_fold + 1;
	  }
	}
      }
    }
  }
  
  return EVENT_GOOD;
}
