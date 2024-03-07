P00 = list_tape
P01 = tapetotape

LIBS = ../libr/libinter.a ../libr/inter_isl.o ../libr/inter_inpx.o

OPT = -c

LinuxLIB   = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -lpthread
SunLIB     = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -lxnet -lposix4
DigitalLIB = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -laio -lxnet


############################### Linux is the default choice
linux: FOR = pgf77 -Mextend  -Mdollar,X $(GS_OPT) $(GS_AIO)
linux: CC  = gcc   -mpreferred-stack-boundary=2 -march=i686 $(GS_AIO)
linux: LIB = $(LinuxLIB)
linux: all

############################### Intel compilers on Linux/ix86
intel: FOR = ifort @../../etc/ifc.cfg  $(GS_OPT) $(GS_AIO)
intel: CC  = icc @../../etc/icc.cfg  -O2 $(GS_AIO)
intel: LIB = $(LinuxLIB)
intel: all

################################ Sun 
sun: all
sun: FOR = f77 -e -D__sun_c__  -xpp=cpp -xtarget=native -xO2 $(GS_AIO)
sun:  CC = cc -D__sun_c__ -xtarget=native -xO2 $(GS_AIO)
sun: LIB = $(SunLIB)


############################### DEC OSF/Alpha
digital : all
digital:=FOR = f77 -extend_source -DDigital -arch host $(GS_AIO)
digital:= CC = cc -DDigital -arch host $(GS_AIO)
digital:=LIB = $(DigitalLIB)



all:$(P00).o $(P01).o $(LIBS)
	$(FOR)  -o $(P00) $(P00).o    $(LIB)
	$(FOR)  -o $(P01) $(P01).o  $(LIB)

$(P00).o: $(P00).F   $(LIBS)
	$(FOR) $(OPT) $(P00).F

$(P01).o: $(P01).F $(LIBS)
	$(FOR) $(OPT) $(P01).F

clean:
	rm -rf core $(P00) $(P01) *.o


