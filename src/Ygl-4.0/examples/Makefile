#
#    Ygl: Run 3d-GL programs with standard X11 routines.
#    (C) Fred Hucht 1993-97
#    EMail: fred@thp.Uni-Duisburg.DE
#
#    $Id: Makefile.std,v 3.3 1997-03-13 16:19:21+01 fred Exp fred $



ifeq ($(wildcard /usr/X11R6/lib64),)
	LIBX64=
else
	LIBX64=-L/usr/X11R6/lib64
endif

ifeq ($(wildcard /usr/X11R6/lib),)
	LIBX=
else
	LIBX=-L/usr/X11R6/lib
endif

# Uncomment next line to use X11 bindings
X11LIB	= $(LIBX64) $(LIBX) -lX11 

# Uncomment next line to use OpenGL bindings
#OGLIB	= -lGL -lGLU

# Uncomment next line to use DoubleBuffer or MultiBuffer extension with X11
MBUFLIB	= -lXext


linux: CDEBUGFLAGS = -mpreferred-stack-boundary=2 -march=i686
linux: CC = gcc
linux: all

intel: CDEBUGFLAGS = -O2
intel: CC = icc @../../../etc/icc.cfg
intel: all

sun: all
sun: CDEBUGFLAGS	= -xO2 -xtarget=native
sun: CC=cc 

digital: all
digital:= CDEBUGFLAGS	= -O3 -arch host
digital:=CC=cc 

ifeq ($(wildcard /opt/X11/include),)
	OPTX11=
else
	OPTX11=-I/opt/X11/include
endif

CDEBUGFLAGS = -O2

COPTS = $(OPTX11)


# *************************** End of configuration *************************

.SUFFIXES: .gl

CFLAGS= $(CDEBUGFLAGS) $(COPTS) -I..

REQUIREDLIBS 	= -L.. -lYgl $(MBUFLIB) $(X11LIB) $(OGLIB)

GLTARGETS	= coltest.gl lines.gl rgbtest.gl smile.gl popup.gl lmbind.gl
YGLTARGETS	= coltest    lines    rgbtest    smile    popup
#    lmbind

all: 	$(YGLTARGETS)
gl: 	$(GLTARGETS)

.c:
	$(CC) $(CFLAGS) -o $@ $< $(REQUIREDLIBS) -lm

.c.gl:
	$(CC) $(CFLAGS) -o $@ $< ../gl2ppm.o -lgl -lm

clean:
	/bin/rm -f *.o *~ $(YGLTARGETS) $(GLTARGETS) *.gif smile_xlf smile_f77 smile_f2c smile_f77.c

smile_f2c: smile_f77.f
	f2c smile_f77.f
	$(CC) $(CFLAGS) -o $@ smile_f77.c -lf2c $(REQUIREDLIBS)

smile_f77: smile_f77.f
	$(FC) $(FFLAGS) -o $@ smile_f77.f $(REQUIREDLIBS)

smile_xlf: smile_xlf.f
	xlf $(FFLAGS) -o $@ smile_xlf.f $(REQUIREDLIBS)
