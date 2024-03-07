/***************************************************************************

  lissa.c: demo for button and slider widgets for the GL graphics language.
  
  Copyright (C) 1994 Michael Staats (michael@hal6000.thp.Uni-Duisburg.DE)
  
  Free according GNU Public License.  
 
 ***************************************************************************/

#include <stddef.h>
#include <math.h>
#include <stdio.h>

#define COLOR RED

#include <X11/Ygl.h>

#include "../glwidget.h"
#define SIZE 700

#define ROUND(x) ((int)(floor((x)+0.5)))

#define SQR(a)  ((a)*(a))

int win;
gl_slider *w1sl, *w2sl;

void quit() {
    gexit();
    exit(0);
}

void dogrid(gl_button *bt, int grid) {
    w1sl->step = w2sl->step = grid?0.25:0.00001;
}

main(int argc, char *argv[])
{
    int i, cbg, bcfg, cred, cblue, col;
    double lwd = 7.0;
    double v,t, dt, x, y, w1, w2, phi, vc;
    double A1, A2;
    int planes, ControlWin, grid = TRUE;

    dt  = 0.1;
    phi = 0.0;
    w1  = 1;
    w2  = 1;
    A1  = 1.4;
    A2  = 1.4;
    
    vc = sqrt(SQR(A1*w1) + SQR(A1*w2));

    putenv("YGL_FT=0"); 
    
    prefsize(420, 195);
    ControlWin = winopen("Controls");
    loadXfont(4711, "-*-helvetica-*-r-*-*-*-140-*-*-*-*-*-*");
    font(4711);
    qdevice(KEYBD);
    qdevice(LEFTMOUSE);
    qdevice(REDRAW);
    unqdevice(INPUTCHANGE);
    color(WHITE); clear();
    
    planes = getplanes();
    if (planes > 7) {
	cbg = 125;
	mapcolor(cbg, 200, 200, 200);
	bcfg = 126;
	mapcolor(bcfg, 0, 150, 0);	
    } else cbg = WHITE;
	
    if (planes <=2) {
	cred = cblue = BLACK;
	bcfg = WHITE;
    } else {
	cred = RED;
	cblue = BLUE;
	bcfg = GREEN;
    }
      
    w1sl = create_gl_slider(10.0, 130.0, 300.0, 10.0, 10.0,  BLACK, cbg,
			    0.0, 4.0, 0.25, w1, &w1, "w1 = %g", NULL);
    w2sl = create_gl_slider(10.0, 110.0, 300.0, 10.0, 10.0, BLACK, cbg, 
			    0.0, 4.0, 0.25, w2, &w2, "w2 = %g", NULL);
    create_gl_slider(10.0, 90.0, 300.0, 10.0, 10.0, BLACK, cbg,
		     0.0, 1.571, 0.001, phi, &phi, "phi = %g", NULL);
    
    create_gl_slider(10.0, 70.0, 300.0, 10.0, 10.0, BLACK, cbg, 
		     0.0, 1.7, 0.1, A1, &A1, "A1 = %g", NULL);
    create_gl_slider(10.0, 50.0, 300.0, 10.0, 10.0, BLACK, cbg, 
		     0.0, 1.7, 0.1, A2, &A2, "A2 = %g", NULL);
    
    
    create_gl_slider(10.0, 30.0, 300.0, 10.0, 10.0, cblue, cbg,
		     1.0, 15.0, 1.0, lwd, &lwd, "LB = %g", NULL);
    create_gl_slider(10.0, 10.0, 300.0, 10.0, 10.0, cblue, cbg,
		     0.0, 0.2, 0.0001, dt, &dt, "dt = %g", NULL);
    
    create_gl_button(10.0, 173.0, 55.0, 17.0, cred, cbg, 0, NULL, BUTTON,
		     "Quit", quit);
    create_gl_button(80.0, 173.0, 80.0, 17.0, cblue, cbg, 0, NULL, BUTTON,
		     "Redraw", NULL);


    create_gl_button(175.0, 173.0, 75.0, 17.0, cblue, cbg, bcfg, &grid, 
		     CHECKBOX, "w-Grid", dogrid);

    minsize(300, 300);
    keepaspect(SIZE, SIZE);
    win = winopen("Lissajous");
    winposition(10, SIZE, 30, SIZE);

    ortho2(-1.5, 1.5, -1.5, 1.5);
    qdevice(KEYBD);
    qdevice(LEFTMOUSE);
    qdevice(REDRAW);
    qdevice(WINQUIT);
    
    if (planes > 7) {
	for (i = 128; i < 256; i++) {
	    mapcolor(i, (Int16)(255 - 0.6 * 2 * (i-128)), 
		     (Int16)(0.5 * 2 *(i-128)),
		     (Int16)(0.8 * 2 *(i-128)));
	}
	col = FALSE;
    }
    else col = (planes <= 2)?WHITE:RED; 
    gconfig();

    qreset();
//    qenter(REDRAW, (Int16)win);
    
    t = 0.0;
    winset(win);
    linewidth((Int16)lwd);
    x = sin(w1*t);
    y = cos(w2*t+phi);
    move2(x, y);
    
    while (1) {
	while (qtest()) {
	    short val;
	    switch(qread(&val)) {
	      case LEFTMOUSE:
		if (update_widgets(val)) ;
 		break;
		
	      case REDRAW:
		winset(ControlWin);
		color(WHITE);
		clear();
		redraw_widgets();
		color(BLACK);
		cmov2(10, 150);
		charstr("x = A1 sin(w1 t); y = A2 cos(w2 t + phi); t += dt");
		winpop();
		winset(win);
		reshapeviewport();
		color(BLACK);
		x = A1*sin(w1*t);
		y = A2*cos(w2*t+phi);			
		move2(x, y);
		clear();
		linewidth((Int16)lwd);
		vc = sqrt(SQR(A1*w1) + SQR(A2*w2));
		qreset();
		break;

	      case WINQUIT:
		quit();
		break;
		
	      case KEYBD:
		switch (val) {
		  case '\033':
		  case 'q':
		    quit();
		    break;
		  case 'r':
		    winset(win);qenter(REDRAW,(Int16) win);
		    break;
		}
	    }
	}
	x = A1*sin(w1*t);
	y = A2*cos(w2*t+phi);
	v = sqrt(SQR(A1*w1*cos(w1*t))+SQR(A2*w2*sin(w2*t+phi)));

	color( (col?col:((int)(128.0 + 127.0 * v/vc)))%256 );
	draw2(x, y);
	t += dt;
    }
}
    
