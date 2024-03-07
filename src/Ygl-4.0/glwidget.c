/***************************************************************************

  glwidget.c: button and slider widgets for the GL graphics language.
  
  Copyright (C) 1994 Michael Staats (michael@hal6000.thp.Uni-Duisburg.DE)
  
  Free according GNU Public License.  
 
 ***************************************************************************/
/* #define DEBUG /**/

#define YGLVERS 2.6

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stddef.h>
#include <math.h>
#include <string.h>

#include <X11/Ygl.h>

#include <glwidget.h>

static gl_slider *firstslider = NULL, *actslider = NULL;
static gl_button *firstbutton = NULL, *actbutton = NULL;

static void   setslider(int x, gl_slider *sl);
static void   drawslider(gl_slider *sl);
static int    updateslider(int x, int y, gl_slider *sl);
static double update_slider_value(gl_slider *sl);
static void   drawbutton(gl_button *bt);
static int    updatebutton(int x, int y, gl_button *bt);
static void   redraw_sliders();
static void   redraw_buttons();
static int    check_vers();
static void   locate_slider(gl_slider *sl);
static void   locate_button(gl_button *bt);

static int    check_vers() {
    char gvers[255];
    char *c;
    double atof(), v; 
    
    gversion(gvers);
#ifdef DEBUG
    puts(gvers);
#endif
    for (c = gvers; *c != '-' && *c != 0; c++);
    if (strncmp(gvers, "Ygl", 3) == 0 && (v = atof(c+1)) < YGLVERS) {
	fprintf(stderr, "Sorry, you need at least Ygl-%g\n", YGLVERS);
	fprintf(stderr, "You seem to have Ygl-%g\n", v);
	exit(1);
    }
    return(TRUE);
}

void redraw_widgets() {
    redraw_sliders();
    redraw_buttons();
} 

int update_widgets(short val) {
    gl_slider *sl = firstslider;
    gl_button *bt = firstbutton;
    Int16 x, y;
    int flag = FALSE, sliderchange = FALSE;

#ifdef DEBUG
    printf("update_widgets called with val = %d\n", (int)val);
    if (qtest()) {
	int dev; 
	dev = qread(&x);
	printf("update_widgets: qtest() for x is true. dev = %d, val = %d\n",
	       dev, (int)x);
    } else {
	x = getvaluator(MOUSEX);
	printf("update_widgets: qtest() for x is FALSE!. x = %d\n", (int)x);
    }
    if (qtest()) {
	int dev; 
	dev = qread(&y);
	printf("update_widgets: qtest() for y is true. dev = %d, val = %d\n",
	       dev, (int)y);
    } else {
	y = getvaluator(MOUSEY);
	printf("update_widgets: qtest() for y is FALSE!. y = %d\n", (int)y);
    }
#else
    if (qtest()) qread(&x); else x = getvaluator(MOUSEX);
    if (qtest()) qread(&y); else y = getvaluator(MOUSEY);
#endif
    if (val == 0) return(FALSE);
       
    while (sl != NULL && !flag) {
	switch (updateslider(x, y, sl)) {
	  case -1:    sliderchange = TRUE; break;
	  case FALSE: flag = FALSE;        break;
	  default:    flag = TRUE;         break;
	}
	sl = sl->nextp;
    }

    while (bt != NULL && !flag) {
	flag = updatebutton(x, y, bt);
	bt = bt->nextp;
    }
    return(flag||sliderchange);
}


/********************** SLIDERS ******************************************/


gl_slider *create_gl_slider(double x, double y, double len, double height, double thick,
			    int fg, int bg, int dark, int light,
			    int txfg, int txtbg,
			    double min, double max, double step, double initvalue,
			    double *valuep, char *format, 
			    void (*callback)(gl_slider *, double)) {

    gl_slider *newslider;
    
    if ((newslider = (gl_slider *)malloc(sizeof(gl_slider))) == NULL) 
      return(NULL);
    
    if (firstslider == NULL) {
	/* initialization */
	check_vers();
	qdevice(LEFTMOUSE);
	tie(LEFTMOUSE, MOUSEX, MOUSEY);
	firstslider = actslider = newslider;
    } 
	
    actslider->nextp = newslider;
    actslider = newslider;
    
    actslider->nextp  = NULL;
    actslider->win    = winget();
    actslider->x      = x;
    actslider->y      = y;
    actslider->len    = len;
    actslider->height = height;
    actslider->thick  = thick;
    actslider->fg     = fg;
    actslider->bg     = bg;
    actslider->dark   = dark;
    actslider->light  = light;
    actslider->txfg   = txfg;
/*    actslider->txbg   = -1;*/
    actslider->txbg   =( txtbg > -1)?txtbg:-1; 
    actslider->min    = min;
    actslider->max    = max;
    actslider->step   = step;
    actslider->value  = initvalue;
    actslider->valuep = valuep;
    if (valuep != NULL) *valuep = initvalue;
    actslider->format = format;
    if (actslider->format != NULL) {
	actslider->buf    = (char *)malloc(strlen(format) + 51);
	*(actslider->buf) = ' ';
	*(actslider->buf + 1) = 0;
    }
    actslider->callback = callback;

    drawslider(actslider);
    
    return(actslider);
}

static void   locate_slider(gl_slider *sl) {
    /* awful hacks to get coordinates in screencoords */

    Screencoord xs, ys;
    Int32 xo, yo;

    getorigin(&xo, &yo);

    cmov2(sl->x, sl->y);
    getcpos(&xs, &ys);
    sl->scx0 = (int)xs - xo;
    sl->scy1 = (int)ys - yo;
    
    cmov2(sl->x + sl->len, sl->y + sl ->height);
    getcpos(&xs, &ys);
    sl->scx1 = (int)xs - xo;
    sl->scy0 = (int)ys - yo;
    
    cmov2(sl->x + sl->thick, sl->y);
    getcpos(&xs, &ys);
    sl->sc_thick = (int)xs - xo - sl->scx0;

}        

void destroy_gl_slider(gl_slider *sld) {
    gl_slider *sl = firstslider, *osl, *np;

    osl = sl;
    while (sl != NULL) {
#ifdef DEBUG
	printf("dest_sl: format = %s ", sl->format);
#endif
	np = sl->nextp;
	if (sl == sld) {
#ifdef DEBUG
	    printf("destroying\n");
#endif
	    osl->nextp = sl->nextp;
	    if (firstslider == sl) firstslider = sl->nextp;
	    sl->fg = sl->bg;
	    drawslider(sl);
	    if (sl->buf != NULL) free(sl->buf);
	    free(sl);
	    np = NULL;
	}
#ifdef DEBUG
	puts("");
#endif
	osl = sl;
	sl = np;
    }
#ifdef DEBUG
    sl = firstslider;
    while (sl!=NULL) {
	printf("after destroy slider: %s\n", sl->format);
	sl=sl->nextp;
    }
#endif
}

static void redraw_sliders() {
    gl_slider *sl = firstslider;
    while (sl != NULL) {
	drawslider(sl);
	sl = sl->nextp;
    }
}

static void drawslider(gl_slider *sl) {
    Int32 lw = getlwidth();
    Int32 cl = getcolor();
    Int32 oriwin = winget();
    Int32 co;
    int i;
    Screencoord x, y;
    Int32 xo, yo;
    
    Coord Border[4][2], x1, x2, y1, y2;
    
    winset(sl->win);
    linewidth(1);
   
    update_slider_value(sl);
    locate_slider(sl);
    
    if (sl->txbg == -1) {
	/* first time, get bg color */
	cmov2(sl->x + sl->len + sl->thick, sl->y);
	getcpos(&x, &y);
	getorigin(&xo, &yo);
	x -= xo;
	y -= yo;
	i = lrectread(x, y, x, y, &co);
	sl->txbg = co;
#ifdef DEBUG
	printf("textbg = %d, bytes = %d x %d (soll %g) y %d (soll %g)\n",
	       co,i,x,sl->x + sl->len + sl->thick,
	       y,sl->y);	
#endif
    }
    color(sl->bg);
    rectf(sl->x, sl->y, sl->x+sl->thick+sl->len, sl->y+sl->height);
    
    color(sl->dark);
    Border[0][0] = sl->x;        		Border[0][1] = sl->y;
    Border[1][0] = sl->x+2;      		Border[1][1] = sl->y+2;
    Border[2][0] = sl->x+2;        		Border[2][1] = sl->y+sl->height;
    Border[3][0] = sl->x;   			Border[3][1] = sl->y+sl->height;
    polf2( 4, Border );
    
    Border[0][0] = sl->x+sl->thick+sl->len;     Border[0][1] = sl->y+sl->height;
    Border[1][0] = sl->x+sl->thick+sl->len-2;   Border[1][1] = sl->y+sl->height-2;
    Border[2][0] = sl->x;        		Border[2][1] = sl->y+sl->height-2;
    Border[3][0] = sl->x;   			Border[3][1] = sl->y+sl->height;
    polf2( 4, Border );
    
    color(sl->light);
    Border[0][0] = sl->x;        		Border[0][1] = sl->y;
    Border[1][0] = sl->x+2;      		Border[1][1] = sl->y+2;
    Border[2][0] = sl->x+sl->thick+sl->len;     Border[2][1] = sl->y+2;
    Border[3][0] = sl->x+sl->thick+sl->len;   	Border[3][1] = sl->y;
    polf2( 4, Border );
    
    Border[0][0] = sl->x+sl->thick+sl->len;     Border[0][1] = sl->y+sl->height;
    Border[1][0] = sl->x+sl->thick+sl->len-2;   Border[1][1] = sl->y+sl->height-2;
    Border[2][0] = sl->x+sl->thick+sl->len-2;   Border[2][1] = sl->y;
    Border[3][0] = sl->x+sl->thick+sl->len;   	Border[3][1] = sl->y;
    polf2( 4, Border );
    

    color(sl->fg);
/*    rect (sl->x, sl->y, sl->x+sl->thick+sl->len, sl->y+sl->height); */   
    rectf(sl->x + sl->len * (sl->value - sl->min)/(sl->max - sl->min)+2,
	  sl->y+2,
	  sl->x + sl->len * (sl->value - sl->min)/(sl->max - sl->min) + sl->thick-2,
	  sl->y + sl->height-2);
    x1 = sl->x + sl->len * (sl->value - sl->min)/(sl->max - sl->min)+2;
    x2 = sl->x + sl->len * (sl->value - sl->min)/(sl->max - sl->min) + sl->thick-2;
    y1 = sl->y+2;
    y2 = sl->y + sl->height-2;

    color(sl->light);
    Border[0][0] = x1;        		Border[0][1] = y1;
    Border[1][0] = x1+2;      		Border[1][1] = y1+2;
    Border[2][0] = x1+2;        	Border[2][1] = y2;
    Border[3][0] = x1;   		Border[3][1] = y2;
    polf2( 4, Border );
    
    Border[0][0] = x2;     		Border[0][1] = y2;
    Border[1][0] = x2-2;   		Border[1][1] = y2-2;
    Border[2][0] = x1;        		Border[2][1] = y2-2;
    Border[3][0] = x1;   		Border[3][1] = y2;
    polf2( 4, Border );
    
    color(sl->dark);
    Border[0][0] = x1;        		Border[0][1] = y1;
    Border[1][0] = x1+2;      		Border[1][1] = y1+2;
    Border[2][0] = x2;     		Border[2][1] = y1+2;
    Border[3][0] = x2;   		Border[3][1] = y1;
    polf2( 4, Border );
    
    Border[0][0] = x2;     		Border[0][1] = y2;
    Border[1][0] = x2-2;   		Border[1][1] = y2-2;
    Border[2][0] = x2-2;   		Border[2][1] = y1;
    Border[3][0] = x2;   		Border[3][1] = y1;
    polf2( 4, Border );
    
    
    
    
    if (sl->buf != NULL) {
	color(sl->txbg);
	cmov2(sl->x + sl->len + sl->thick, sl->y);
	charstr(sl->buf);
	color(sl->txfg);
	sprintf(sl->buf + 1, sl->format, sl->value);
	cmov2(sl->x + sl->len + sl->thick, sl->y);
	charstr(sl->buf);
    }
    
    winset(oriwin);
    color(cl);
    linewidth(lw);
    
    sleep(0);
}

static void setslider(int x, gl_slider *sl) {
    double oldvalue;
    
    oldvalue = sl->value;
    sl->value = sl->min + 
      (sl->max - sl->min) * (x - sl->scx0 - sl->sc_thick/2) / 
	(1.0 * sl->scx1 - 1.0 * sl->scx0);
    sl->value = sl->step * floor(0.5 + sl->value / sl->step);
    if (sl->value < sl->min) sl->value = sl->min;
    if (sl->value > sl->max) sl->value = sl->max;
    if (sl->valuep != NULL) *(sl->valuep) = sl->value;
    if( oldvalue != sl->value )drawslider(sl);
}

static int updateslider(int x, int y, gl_slider *sl) {
    Int32 xo, yo;
    Int16 xx, yy;
    double orival;
    Int32  oriwin = winget();
    int dev;
    
    orival = sl->value;
    update_slider_value(sl);
    winset(sl->win);

    getorigin(&xo, &yo);
    x -= xo;
    y -= yo;

#ifdef DEBUG
    printf("updateslider: slider label %s - ", sl->buf);
#endif    
    locate_slider(sl);
    
    if (x < sl->scx0 || y > sl->scy0 ||
	x > sl->scx1 + sl->sc_thick  ||	y < sl->scy1) {
	winset(oriwin);
	if (orival != sl->value) {
	    /* This is used when the click is not for this slider, but
	       update_slider_value() has changed the value... */
	    if (sl->callback != NULL) sl->callback(sl, sl->value);
#ifdef DEBUG
	    puts("return(-1)");
#endif
	    return(-1);
	} else {
#ifdef DEBUG
	    puts("return(FALSE)");
#endif
	    return(FALSE);
	}
    }
	
    while (qtest() == 0 || (dev = qread(&xx)) != LEFTMOUSE) {
	/* getorigin(&xo, &yo); */
	setslider(getvaluator(MOUSEX) - xo, sl);
	/* usleep(100000);*/
    } 
#ifdef DEBUG
    printf("updateslider: got an event, dev = %d val = %d\n", dev, (int)xx);
    if (qtest()) {
	int dev; 
	dev = qread(&xx);
	printf("updateslider: qtest() for x is true. dev = %d, val = %d\n",
	       dev, (int)xx);
    } else {
	xx = getvaluator(MOUSEX);
	printf("updateslider: qtest() for x is FALSE!. x = %d\n", (int)xx);
    }
    if (qtest()) {
	int dev; 
	dev = qread(&yy);
	printf("updateslider: qtest() for y is true. dev = %d, val = %d\n",
	       dev, (int)yy);
    } else {
	yy = getvaluator(MOUSEY);
	printf("updateslider: qtest() for y is FALSE!. y = %d\n", (int)yy);
    }
    setslider((int)(xx-xo), sl);
#else
    if (qtest()) qread(&xx);
    if (qtest()) {
	qread(&yy);
	setslider((int)(xx-xo), sl);
    } else 
      fprintf(stderr, 
	      "updateslider: This should not happen, tie() seems to fail.\n");
#endif
    winset(oriwin);
    if (sl->value != orival) {
	if (sl->callback != NULL) sl->callback(sl, sl->value);
	return(TRUE);
#ifdef DEBUG
	puts("return(TRUE)");
#endif
    } else {
#ifdef DEBUG
	puts("return(FALSE)");
#endif
	return(FALSE);
    }
}

static double update_slider_value(gl_slider *sl) {
    if (sl->valuep != NULL) sl->value = *(sl->valuep);
    sl->value = sl->step * floor(0.5 + sl->value / sl->step);
    if (sl->value < sl->min) sl->value = sl->min;
    if (sl->value > sl->max) sl->value = sl->max;
    if (sl->valuep != NULL) *(sl->valuep) = sl->value;
    return(sl->value);
}


/**************************** BUTTONS ********************************/
gl_button *create_gl_button(double x, double y, double width, double height,
			    int fg, int bg, int active_c, int *state, int type,
			    char *label, void (*callback)(gl_button *, int)) {
    gl_button *newbutton;

    if ((newbutton = (gl_button *)malloc(sizeof(gl_button))) == NULL) 
      return(NULL);
    
    if (firstbutton == NULL) {
	/* initialization */
	check_vers();
	qdevice(LEFTMOUSE);
	tie(LEFTMOUSE, MOUSEX, MOUSEY);
	firstbutton = actbutton = newbutton;
    } 
	
    actbutton->nextp = newbutton;
    actbutton = newbutton;
    
    actbutton->nextp    = NULL;
    actbutton->win      = winget();
    actbutton->x        = x;
    actbutton->y        = y;
    actbutton->width    = width;
    actbutton->height   = height;
    actbutton->fg       = fg;
    actbutton->bg       = bg;
    actbutton->active_c = active_c;
    actbutton->type     = type;
    actbutton->changed     = FALSE;

    if (state != NULL)  actbutton->state = state;
    else {
	if ((actbutton->state = (int *)malloc(sizeof(int))) == NULL) return(NULL);
	*(actbutton->state) = 0;
    }
	
    if (label != NULL) {
	if ((actbutton->label = (char *)malloc(strlen(label) + 2)) != NULL) {
	    *(actbutton->label) = ' ';
	    strcpy(actbutton->label + 1, label);
	}
    } else actbutton->label = NULL;
    actbutton->callback = callback;

    drawbutton(actbutton);
    
    return(actbutton);
}

static void   locate_button(gl_button *bt) {
    /* awful hacks to get coordinates in screencoords */

    Screencoord xs, ys;
    Int32 xo, yo;
    
    getorigin(&xo, &yo);

    cmov2(bt->x, bt->y);
    getcpos(&xs, &ys);
    bt->scx0 = (int)xs - xo;
    bt->scy1 = (int)ys - yo;
    
#ifdef DEBUG
    printf("locate_button: getcpos gives x = %d y = %d for %g, %g\n",
	   (int)xs, (int)ys, bt->x, bt->y);
#endif
    
    cmov2(bt->x + bt->width, bt->y + bt ->height);
    getcpos(&xs, &ys);
    bt->scx1 = (int)xs - xo;
    bt->scy0 = (int)ys - yo;
}        


void destroy_gl_button(gl_button *but) {
    gl_button *bt = firstbutton, *obt, *np;
    
    obt = bt;
    while (bt != NULL) {
#ifdef DEBUG
	printf("dest_but: label = %s ", bt->label);
#endif
	np = bt->nextp;
	if (bt == but) {
#ifdef DEBUG
	    printf("destroying\n");
#endif
	    np = bt->nextp;
	    obt->nextp = bt->nextp;
	    if (firstbutton == bt) firstbutton = bt->nextp;
	    bt->fg = bt->bg;
	    *(bt->state) = 0;
	    drawbutton(bt);
	    if (bt->label != NULL) free(bt->label);
	    free(bt);
	    np = NULL;
	}
#ifdef DEBUG
	puts("");
#endif
	obt = bt;
	bt = np;
    }
#ifdef DEBUG
    bt = firstbutton;
    while (bt!=NULL) {
	printf("after destroy button: %s\n", bt->label);
	bt=bt->nextp;
    }
#endif
}

static void redraw_buttons() {
    gl_button *bt = firstbutton;
    while (bt != NULL) {
	drawbutton(bt);
	bt = bt->nextp;
    }
}

static void drawbutton(gl_button *bt) {
    Int32 lw = getlwidth();
    Int32 cl = getcolor();
    Int32 oriwin = winget();
    int cfg, bg;
    
    winset(bt->win);
    linewidth(1);
    locate_button(bt);
    
    cfg = *(bt->state)?bt->bg:bt->fg;
    bg  = *(bt->state)?((bt->type == BUTTON)?bt->fg:bt->active_c):bt->bg;
	
    color(bg);
    rectf(bt->x, bt->y, bt->x + bt->width, bt->y + bt->height);
    
    color(bt->fg);
    rect (bt->x, bt->y, bt->x + bt->width, bt->y + bt->height);

    if (bt->label != NULL) {
	color(cfg);
	cmov2(bt->x, bt->y + 0.05 * bt->height);
	charstr(bt->label);
    }
    
    winset(oriwin);
    color(cl);
    linewidth(lw);
    
    sleep(0);
}

static int updatebutton(int x, int y, gl_button *bt) {
    Int32 xo, yo;
    Int16 xx, yy;
    Int32  oriwin = winget();
    int oldstate;

#ifdef DEBUG
    printf("updatebutton: button with label %s - ", bt->label);
#endif
    
    winset(bt->win);
    locate_button(bt);
    
    getorigin(&xo, &yo);
    x -= xo;
    y -= yo;
    if (x < bt->scx0 || y > bt->scy0 ||
	x > bt->scx1 ||	y < bt->scy1) {
	winset(oriwin);
	bt->changed = FALSE;
#ifdef DEBUG
	printf("x = %d y = %d, btscx0 = %d btscx1 = %d btscy0 = %d btscy1 = %d\n",
	       x, y, bt->scx0,  bt->scx1, bt->scy0,  bt->scy1);
	puts("click not inside - return(FALSE)");
#endif
	return(FALSE);
    }
    oldstate = *(bt->state);
    *(bt->state) = (bt->type == BUTTON)?TRUE:!oldstate;
    drawbutton(bt);
    
    while (qread(&xx) != LEFTMOUSE); /* wait for LEFTMOUSE release */

    if (qtest()) qread(&xx); else {
	xx = getvaluator(MOUSEX);      
	fprintf(stderr, 
		"updatebutton: This should not happen, tie() seems to fail.\n");
    }
    if (qtest()) qread(&yy); else {
	yy = getvaluator(MOUSEY);
	fprintf(stderr, 
		"updatebutton: This should not happen, tie() seems to fail.\n");
    }
    getorigin(&xo, &yo);
    x = xx - xo;
    y = yy - yo;

    if (!(bt->changed = !(
			  x < bt->scx0 || y > bt->scy0 ||
			  x > bt->scx1 || y < bt->scy1
			  )
	  )) *(bt->state) = oldstate;

    if (bt->type == BUTTON) {
	*(bt->state) = FALSE;
	usleep(50000);
    }
    drawbutton(bt);
    if (oriwin != bt->win) winset(oriwin);

    if (bt->callback != NULL && bt->changed) {
	bt->callback(bt, *(bt->state));
    }
#ifdef DEBUG
    printf("return(%s)\n", bt->changed?"true":"false");
#endif
    return(bt->changed);
}

