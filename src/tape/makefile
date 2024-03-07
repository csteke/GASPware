P00 = list_tape
P01 = tapetotape

LIBS = ../libr/libinter.a ../libr/inter_isl.o ../libr/inter_inpx.o

OPT = -c

LinuxLIB   = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -lpthread
SunLIB     = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -lxnet -lposix4
DigitalLIB = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -laio -lxnet


############################### PGI compilers for Linux/ix86 and MacOS
pgi: FOR = pgf77 -Mextend  -Mdollar,X $(GS_OPT) $(GS_AIO)
pgi: CC  = pgcc    $(GS_AIO)
pgi: LIB = $(LinuxLIB)
pgi: all

############################### Intel compilers on Linux/ix86 and MacOS
intel: FOR = ifort @../../etc/ifc.cfg  $(GS_OPT) $(GS_AIO)
intel: CC  = icc @../../etc/icc.cfg    $(GS_OPT) $(GS_AIO)
intel: LIB = $(LinuxLIB)
intel: all

############################### GCC(>6.x) compilers on Linux/ix86 and MacOS
gnu: FOR = $(GNU_FOR)   $(GS_OPT) $(GS_AIO)
gnu: CC  = $(GNU_CC)    $(GS_OPT) $(GS_AIO)
gnu: LIB = $(LinuxLIB)
gnu: all

############################### Solaris Studio FORTRAN compiler for Linux/ix86
sstudio: FOR = sunf90 -silent -f77 -ftrap=%none -e -D__sun_c__ -xpp=cpp  $(GS_OPT) $(GS_AIO)
sstudio: CC  = suncc  $(GS_OPT)  $(GS_AIO)
sstudio: LIB = $(LinuxLIB)
sstudio: all

################################ Sun 
sun: all
sun: FOR = f77 -silent -e -D__sun_c__  $(GS_OPT) $(GS_AIO)
sun:  CC = cc     -D__sun_c__  $(GS_OPT) $(GS_AIO)
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


