P00 = sadd

LIBS = ../libr/libinter.a ../libr/inter_isl.o ../libr/inter_inpx.o

LinuxLIB   = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -lpthread
SunLIB     = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -lposix4
DigitalLIB = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o 

############################### Linux is the default choice
linux: FOR = pgf77 -Mextend  -Mdollar,X $(GS_OPT) $(GS_ONLINE)
linux: LIB = $(LinuxLIB)
linux: all

############################### Intel compilers on Linux/ix86
intel: FOR = ifort @../../etc/ifc.cfg $(GS_OPT) $(GS_ONLINE)
intel: LIB = $(LinuxLIB)
intel: all

################################ Sun 
sun: all
sun: FOR = f77 -e -D__sun_c__ -xpp=cpp -xO2 -xtarget=native $(GS_ONLINE)
sun: LIB = $(SunLIB)

############################### DEC OSF/Alpha
digital : all
digital:=FOR = f77 -extend_source -DDigital -arch host $(GS_ONLINE)
digital:=LIB = $(DigitalLIB)

all: $(P00)

$(P00): $(P00).o $(LIBS)
	$(FOR) -o $(P00) $(P00).o  $(LIB)

$(P00).o: $(P00).F $(P00).inc
	$(FOR) -c $(P00).F

clean:
	rm -rf core $(P00) $(P00)*.o

