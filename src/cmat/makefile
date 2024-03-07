P00 = cmat
P01 = glwContour
P02 = Autoban

LIBS = ../libr/libinter.a ../libr/inter_isl.o ../libr/inter_inpx.o


LIB = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o \
-L/usr/X11R6/lib64 -L/usr/X11R6/lib -L../Ygl-4.0 -lYgl -lX11 -lXext -lpthread

DigitalLIB = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o \
-L/usr/X11R6/lib  -L../Ygl-4.0 -lYgl -lX11 -lXext

SunLIB =  -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o \
-L/usr/X11R6/lib  -L../Ygl-4.0 -lYgl -lX11 -lXext -lposix4

INCLUDE = -I../Ygl-4.0


############################### Linux is the default choice
linux: FOR = pgf77
linux: CC  = gcc  -mpreferred-stack-boundary=2 -march=i686
linux: OPT = -Mextend  -Mdollar,X $(GS_OPT)
linux : $(P00)

################################ Intel compilers for Linux/ix86 
intel: FOR = ifort @../../etc/ifc.cfg
intel: CC  = icc @../../etc/icc.cfg  -O2
intel: OPT =  $(GS_OPT)
intel: $(P00)

################################ Sun 
sun: $(P00)
sun: FOR = f77
sun: CC  = cc -D__sun_c__ -xO2 -xtarget=native
sun: OPT = -e -D__sun_c__ -xpp=cpp -xO2 -xtarget=native
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

