/***************************************************************************

  simple.c: button and slider widgets demo for the GL graphics language.
  
  Copyright (C) 1994 Michael Staats (michael@hal6000.thp.Uni-Duisburg.DE)
  
  Free according GNU Public License.  
 
 ***************************************************************************/

#include <stddef.h>
#include <math.h>
#include <stdio.h>

#include <X11/Ygl.h>

#include "../glwidget.h"

gl_slider *sl1, *sl2;
gl_button *bt1, *bt2;

void quit();
void callback1(gl_slider *sl, double v);
void destroy();

int main() {
    Int16 val;
    Device dev;
    int w;
    double val2;
    int bval = TRUE;
    
    minsize(400,200);
    w = winopen("bla");
    qdevice(KEYBD);
    qdevice(REDRAW);
    qenter(REDRAW, w);
    color(BLACK);
    clear();       /* set the bg color before creating sliders !!! */
    
    sl1 = create_gl_slider(10.0, 20.0, 350.0, 20.0, 10.0, WHITE, BLACK,
			   0.0, 100.0, 0.1, 50.0, NULL, "%g", NULL);
    
    sl2 = create_gl_slider(10.0, 120.0, 350.0, 20.0, 10.0, WHITE, BLACK,
			   -10.0, 10.0, 2.0, 0.0, &val2, "%g", callback1);
    
    create_gl_button(10.0, 150.0, 35.0, 20.0, WHITE, BLACK, 0, NULL, BUTTON,
		     "Quit", quit);
    
    bt1 = create_gl_button(60.0, 150.0, 30.0, 20.0, WHITE, BLACK, BLUE, &bval, 
			   CHECKBOX, "???", NULL);
    
    bt2 = create_gl_button(110.0, 150.0, 35.0, 20.0, YELLOW, BLACK, 0, NULL, BUTTON,
			   "NoOp", NULL);
    
    create_gl_button(160.0, 150.0, 65.0, 20.0, RED, BLACK, 0, NULL, BUTTON,
		     "Destroy", destroy);
    
    while ((dev = qread(&val))) switch(dev) {
      case LEFTMOUSE:
	if (update_widgets(val)) {
	    printf("slider 1: %g\tslider2: %g\n",sl1->value, val2);
	    printf("button2: %s\n", bval?"on":"off");
	}
	break;
      case REDRAW:
	color(BLACK);
	clear();
	redraw_widgets();
	break;
      case KEYBD:
	switch(val) {
	  case 'q':
	  case '\033':
	    gexit();
	    exit(0);
	    break;
	  case '+':
	    val2 += 2;
	    redraw_widgets();
	    break;
	  case '-':
	    val2 -= 2;
	    redraw_widgets();
	    break;
	}
	break;
    }
    return(0);
}
	
void quit() {
    gexit();
    exit(0);
}

void callback1(gl_slider *sl, double v) {
    printf("callback: slider value = %g\n", v);
}

void destroy() {
    static done = FALSE;
    if (!done) {
	destroy_gl_slider(sl1);
	destroy_gl_slider(sl2);
	destroy_gl_button(bt1);
	destroy_gl_button(bt2);
	done = TRUE;
    } else puts("already destroyed....");
}
