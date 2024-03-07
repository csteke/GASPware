P00 = trackn

P02 = glwlib
P03 = inter_isl
P04 = effcor


LIBS = ../libr/libinter.a

ifeq ($(wildcard /opt/X11/include),)
	OPTX11=
else
	OPTX11=-I/opt/X11/include
endif

INCLUDE = $(OPTX11) -I../Ygl-4.0

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

LinuxLIB = $(LIBX64) $(LIBX) -L../libr -linter -L../Ygl-4.0 -lYgl -lX11 -lXext -lpthread
DigitalLIB = -L/usr/X11R6/lib -L../libr -linter -L../Ygl-4.0 -lYgl -lX11 -lXext
SunLIB=-L/usr/X11R6/lib -L../libr -linter -L../Ygl-4.0 -lYgl -lX11 -lXext -lposix4

############################### PGI compilers for Linux/ix86 and MacOS
pgi: FOR = pgf77 $(GS_ONLINE) 
pgi: CC  = pgcc   $(GS_ONLINE) 
pgi: OPT = -Mextend -Mdollar,X  $(GS_OPT)
pgi: LIB = $(LinuxLIB)
pgi: all

############################### Intel compilers on Linux/ix86 and MacOS
intel: FOR = ifort @../../etc/ifc.cfg $(GS_ONLINE) 
intel: CC  = icc @../../etc/icc.cfg   $(GS_ONLINE) $(GS_OPT)
intel: OPT =  $(GS_OPT)
intel: LIB = $(LinuxLIB)
intel: all

############################### GCC(>6.x) compilers on Linux/ix86 and MacOS
gnu: FOR = $(GNU_FOR)  $(GS_ONLINE) 
gnu: CC  = $(GNU_CC)   $(GS_ONLINE) $(GS_OPT)
gnu: OPT =  $(GS_OPT)
gnu: LIB = $(LinuxLIB)
gnu: all

############################### Solaris Studio FORTRAN compiler for Linux/ix86
sstudio: FOR = sunf90 -silent -f77 -ftrap=%none -e -D__sun_c__  $(GS_ONLINE) 
sstudio: CC  = suncc   $(GS_ONLINE) $(GS_OPT)
sstudio: OPT = $(GS_OPT)
sstudio: LIB = $(LinuxLIB)
sstudio: all

################################ Sun 
sun: all
sun: FOR = f77  -silent    $(GS_ONLINE) $(GS_OPT)
sun:  CC = cc -D__sun_c__  $(GS_ONLINE) $(GS_OPT)
sun: OPT = -e -D__sun_c__
sun: LIB = $(SunLIB)

############################### DEC OSF/Alpha
digital : all
digital:=FOR = f77 -O3 -DDigital -arch host $(GS_ONLINE) 
digital:= CC = cc  -DDigital -arch host  $(GS_ONLINE) 
digital:=OPT = -extend_source
digital:=LIB = $(DigitalLIB)

all: $(P00)

$(P00):$(P00).o   $(P02).o $(P03).o $(P04).o $(LIBS)
	$(FOR) $(OPT) -o x$(P00) $(P00).o   $(P02).o $(P03).o $(P04).o $(LIB)

$(P00).o:$(P00).F $(P00).inc
	$(FOR) $(OPT) -c $(P00).F


$(P02).o:$(P02).c
	$(CC) -c $(P02).c $(INCLUDE)

$(P03).o:../libr/$(P03).c
	$(CC) -c ../libr/$(P03).c $(INCLUDE) 

$(P04).o:$(P04).F $(P04).inc
	$(FOR) $(OPT) -c $(P04).F

clean:
	rm -rf core $(P00) $(P00).o $(P01).o $(P02)*.o $(P03).o $(P04).o 



