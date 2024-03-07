/***************************************************************************

  glwidget.h: button and slider widgets for the GL graphics language.
  
  Copyright (C) 1994 Michael Staats (michael@hal6000.thp.Uni-Duisburg.DE)
  
  Free according GNU Public License.  
 
 ***************************************************************************/

/*****   DEFINES  ******/

#define BUTTON 0
#define CHECKBOX 1

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif


/*****   TYPEDEFS ******/

typedef struct gl_slider gl_slider;
struct gl_slider{
    int bg, fg;
    Int32 win;
    double x, y, len, height, thick;
    int scx0, scx1, scy0, scy1, sc_thick;
    double min, max, step;
    double value, *valuep;
    gl_slider *nextp;
    Int32 txbg;
    char *format, *buf;
    void (*callback)(gl_slider *, double);
};

typedef struct gl_button gl_button;
struct gl_button{
    double x, y, width, height;
    int scx0 ,scx1, scy0, scy1;
    int win, bg, fg, active_c;
    int type, *state, changed;
    char *label;
    gl_button *nextp;
    void (*callback)(gl_button *, int);
};

/*********** generic functions ******/
void redraw_widgets();
int  update_widgets(short val);
/*********** BUTTONFUNCS ************/

gl_button *create_gl_button(double x, double y, double width, double height,
			    int fg, int bg, int active_c, int *state, int type,
			    char *label, void (*callback)(gl_button *, int));

void      destroy_gl_button(gl_button *but);

/*********** SLIDERFUNCS ************/

gl_slider *create_gl_slider(double x, double y, double len, 
			    double height, double thick,
			    int fg, int bg,
			    double min, double max, double step, 
			    double initvalue,
			    double *valuep, char *format, 
			    void (*callback)(gl_slider *, double));

void      destroy_gl_slider(gl_slider *sld);


