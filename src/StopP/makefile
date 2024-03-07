P00 = stopp
P01 = zieglerch

LIBS = ../libr/libinter.a ../libr/inter_isl.o ../libr/inter_inpx.o

LinuxLIB   = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -lpthread
SunLIB     = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -lposix4
DigitalLIB = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -laio

############################### Linux is the default choice
linux: FOR = pgf77 -Mextend  -Mdollar,X  $(GS_OPT)
linux: CC  = gcc  
linux: LIB = $(LinuxLIB)
linux: all

############################### Intel compilers on Linux/ix86
intel: FOR = ifort @../../etc/ifc.cfg  $(GS_OPT)
intel: CC  = icc @../../etc/icc.cfg  -O2
intel: LIB = $(LinuxLIB)
intel: all

################################ Sun 
sun:  all
sun: FOR = f77 -e -D__sun_c__ -xpp=cpp
sun:  CC = cc -D__sun_c__
sun: LIB = $(SunLIB)

############################### DEC OSF/Alpha
digital : all
digital:=FOR = f77 -extend_source -DDigital
digital:= CC = cc -DDigital
digital:=LIB = $(DigitalLIB)

OPT=-c

all: $(P00)



$(P00): $(P00).o $(P01).o $(LIBS)
	$(FOR)  $(P00).o $(P01).o $(LIB)  -o $(P00)

$(P00).o:$(P00).F 
	$(FOR) $(OPT) $(P00).F

$(P01).o:$(P01).F $(INCL)
	$(FOR) $(OPT) $(P01).F


clean:
	rm -rf core $(P00) *.o


