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



pgi: CDEBUGFLAGS = 
pgi: CC = pgcc
pgi: all

intel: CDEBUGFLAGS = $(GS_OPT)
intel: CC = icc @../../etc/icc.cfg
intel: all

gnu: CC  = $(GNU_CC)  -Wno-deprecated-declarations
gnu: CDEBUGFLAGS = $(GS_OPT)
gnu: all

sstudio: CDEBUGFLAGS = $(GS_OPT)
sstudio: CC = suncc
sstudio: all

sun: all
sun: CDEBUGFLAGS	= $(GS_OPT)
sun: CC=cc 

digital: all
digital:= CDEBUGFLAGS	= -O3 -arch host
digital:=CC=cc 

OBJS		= ygl.o draw.o misc.o font.o queue.o color.o menu.o gl2ppm.o glwidget.o $(FBO)
TARGET          = libYgl.a

COPTS = $(X11) $(OGL) $(DOUBLEBUF) $(MULTIBUF) $(HASXMU)

# End of configuration

ifeq ($(wildcard /opt/X11/include),)
	OPTX11=
else
	OPTX11=-I/opt/X11/include
endif


CFLAGS		= -I. $(OPTX11) $(CDEBUGFLAGS) $(COPTS) $(YGL_PREFIX)

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
