P00 = cmat
P01 = glwContour
P02 = Autoban

LIBS = ../libr/libinter.a ../libr/inter_isl.o ../libr/inter_inpx.o


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

LIB = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o \
$(LIBX64) $(LIBX) -L../Ygl-4.0 -lYgl -lX11 -lXext -lpthread

DigitalLIB = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o \
-L/usr/X11R6/lib  -L../Ygl-4.0 -lYgl -lX11 -lXext

SunLIB =  -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o \
-L/usr/X11R6/lib  -L../Ygl-4.0 -lYgl -lX11 -lXext -lposix4

ifeq ($(wildcard /opt/X11/include),)
	OPTX11=
else
	OPTX11=-I/opt/X11/include
endif

INCLUDE = $(OPTX11) -I../Ygl-4.0


############################### PGI compilers for Linux/ix86 and MacOS
pgi: FOR = pgf77
pgi: CC  = pgcc  
pgi: OPT = -Mextend  -Mdollar,X $(GS_OPT)
pgi: $(P00)

################################ Intel compilers for Linux/ix86 and MacOS
intel: FOR = ifort @../../etc/ifc.cfg 
intel: CC  = icc @../../etc/icc.cfg  $(GS_OPT) 
intel: OPT =  $(GS_OPT)
intel: $(P00)

############################### GCC(>6.x) compilers on Linux/ix86 and MacOS
gnu: FOR = $(GNU_FOR)  
gnu: CC  = $(GNU_CC)    $(GS_OPT) 
gnu: OPT = $(GS_OPT)
gnu: $(P00)

############################### Solaris Studio FORTRAN compiler for Linux/ix86
sstudio: FOR = sunf90 -silent -f77 -ftrap=%none
sstudio: CC  = suncc $(GS_OPT)
sstudio: OPT = -e -D__sun_c__  $(GS_OPT)
sstudio : $(P00)


################################ Sun 
sun: $(P00)
sun: FOR = f77 -silent
sun: CC  = cc -D__sun_c__ $(GS_OPT) 
sun: OPT = -e -D__sun_c__ $(GS_OPT)
sun: LIB = $(SunLIB)

############################### DEC OSF/Alpha
digital : $(P00)
digital:= FOR = f77
digital:= CC  = cc  -DDigital -arch host
digital:= OPT = -extend_source -DDigital -arch host
digital:= LIB = $(DigitalLIB)


$(P00): $(P00).o $(P01).o $(P02).o $(LIBS)
	$(FOR) $(OPT) -o $(P00) $(P00).o $(P01).o $(P02).o  $(LIB)

$(P00).o: $(P00).F $(P00).inc
	$(FOR) $(OPT) -c $(P00).F

$(P01).o: $(P01).c
	$(CC) -c $(INCLUDE) $(P01).c

$(P02).o: $(P02).c
	$(CC) -c $(INCLUDE) $(P02).c

clean:
	rm -rf core $(P00) $(P00)*.o $(P01).o $(P02).o

