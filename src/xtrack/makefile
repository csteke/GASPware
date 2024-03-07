P00 = trackn

P02 = glwlib
P03 = inter_isl
P04 = effcor


LIBS = ../libr/libinter.a

INCLUDE = -I../Ygl-4.0

LinuxLIB = -L/usr/X11R6/lib64 -L/usr/X11R6/lib -L../libr -linter -L../Ygl-4.0 -lYgl -lX11 -lXext -lpthread
DigitalLIB = -L/usr/X11R6/lib -L../libr -linter -L../Ygl-4.0 -lYgl -lX11 -lXext
SunLIB=-L/usr/X11R6/lib -L../libr -linter -L../Ygl-4.0 -lYgl -lX11 -lXext -lposix4

############################### Linux is the default choice
linux: FOR = pgf77 $(GS_ONLINE) 
linux: CC  = gcc  -mpreferred-stack-boundary=2 -march=i686  $(GS_ONLINE) 
linux: OPT = -Mextend -Mdollar,X  $(GS_OPT)
linux: LIB = $(LinuxLIB)
linux: all

############################### Intel compilers on Linux/ix86
intel: FOR = ifort @../../etc/ifc.cfg $(GS_ONLINE) 
intel: CC  = icc @../../etc/icc.cfg  -O2 $(GS_ONLINE) 
intel: OPT =  $(GS_OPT)
intel: LIB = $(LinuxLIB)
intel: all

################################ Sun 
sun: all
sun: FOR = f77 -xO2 -xtarget=native -xpp=cpp $(GS_ONLINE) 
sun:  CC = cc -D__sun_c__ -xO2 -xtarget=native  $(GS_ONLINE) 
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



