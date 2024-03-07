#
#    Ygl: Run 2d-GL programs with standard X11 routines.
#    (C) Fred Hucht 1993-97
#    EMail: fred@thp.Uni-Duisburg.DE
#
#    $Id: Makefile.std,v 3.4 1996-07-18 18:38:53+02 fred Exp fred $

# Uncomment next line to use X11 bindings (faster, but only 2d)
X11		= -DX11

# Uncomment next line to use OpenGL bindings (slower than X11, but also 3d)
#OGL		= -DOGL

# Uncomment next two lines to include FORTRAN bindings
#FBO		= fortran.o
#FBH		= X11/Yfgl.h

# Uncomment next line to use DoubleBuffer extension with X11
DOUBLEBUF 	= -DDOUBLEBUF

# Uncomment next line to use MultiBuffer extension with X11
MULTIBUF 	= -DMULTIBUF

# Uncomment next two lines to prepend "ygl_" to all function names 
#YGL_PREFIX	= -DYGL_PREFIX
#PH		= X11/Yglprefix.h
#TARGET		= libYglp.a



linux: CDEBUGFLAGS = -mpreferred-stack-boundary=2 -march=i686
linux: CC = gcc
linux: all

intel: CDEBUGFLAGS = -O2
intel: CC = icc @../../etc/icc.cfg
intel: all

sun: all
sun: CDEBUGFLAGS	= -xO2 -xtarget=native
sun: CC=cc 

digital: all
digital:= CDEBUGFLAGS	= -O3 -arch host
digital:=CC=cc 

OBJS		= ygl.o draw.o misc.o font.o queue.o color.o menu.o gl2ppm.o glwidget.o $(FBO)
TARGET          = libYgl.a

COPTS = $(X11) $(OGL) $(DOUBLEBUF) $(MULTIBUF) $(HASXMU)

# End of configuration

CFLAGS		= -I. $(CDEBUGFLAGS) $(COPTS) $(YGL_PREFIX)

all: 	$(TARGET)

.c.o:	
	$(CC) -c $(CFLAGS) $<

X11/Yglprefix.h:	makeYglprefix X11/Ygl.h
	./makeYglprefix > $@

X11/Ygltypes.h:		makeYgltypes
	./makeYgltypes > $@

$(OBJS):	header.h config.h X11/Ygl.h X11/Ygltypes.h glwidget.h $(PH)

$(TARGET):	$(OBJS)
	/bin/rm -f $@
	ar rv $@ $(OBJS)
	ranlib $@

install:	$(TARGET)
	/bin/cp $(TARGET) /usr/local/lib/$(TARGET) 
	ranlib /usr/local/lib/$(TARGET)
	/bin/cp X11/Ygl.h X11/Ygltypes.h $(PH) $(FBH) /usr/include/X11

clean:
	/bin/rm -f *.o *~ $(TARGET) X11/Yglprefix.h X11/Ygltypes.h makeYgltypes

etags:
	etags *.[ch]
