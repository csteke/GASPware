P00 = gsort
P01 = gsortcom0
P02 = gsortcom1
P03 = gsortcom2
P04 = gsortioev-new
P05 = gsortmisc
P06 = gs_ebevent
P07 = gs_gsphevent
P08 = gs_yalevent
P09 = tndlib
P10 = gs_GSRevent
P11 = gsort-utils
P12 = gs_ganilevent

U1  = gsortuser1
U2  = gsortuser2
U3  = gsortuser3
U4  = gsortuser4
U5  = gsortuser5
U6  = gsortuser6
U7  = gsortuser7
U8  = gsortuser8
U9  = gsortuser9
UC  = gsortuser-c

INCL = $(P00).inc $(P00).def $(P00).par

LIBS = ../libr/libinter.a ../libr/inter_isl.o ../libr/inter_inpx.o

OPT = -c

LinuxLIB   = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -lpthread
SunLIB     = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -lposix4
DigitalLIB = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -laio

############################### PGI compilers for Linux/ix86 and MacOS
pgi: FOR = pgf77 -Mextend  -Mdollar,X  $(GS_OPT) $(GS_AIO)
pgi: CC  = pgcc    $(GS_AIO)
pgi: LIB = $(LinuxLIB)
pgi:all

############################### Intel compilers on Linux/ix86 and MacOS
intel: FOR = ifort @../../etc/ifc.cfg  $(GS_OPT) $(GS_AIO)
intel: CC  = icc @../../etc/icc.cfg  $(GS_OPT)   $(GS_AIO)
intel: LIB = $(LinuxLIB)
intel :all

############################### GCC(>6.x) compilers on Linux/ix86 and MacOS
gnu: FOR = $(GNU_FOR) $(GS_OPT) $(GS_AIO)
gnu: CC  = $(GNU_CC)  $(GS_OPT)   $(GS_AIO)
gnu: LIB = $(LinuxLIB)
gnu :all

############################### Solaris Studio FORTRAN compiler for Linux/ix86
sstudio: FOR = sunf90 -silent -f77 -ftrap=%none  -e -D__sun_c__  $(GS_OPT) $(GS_AIO)
sstudio: CC  = suncc  $(GS_AIO) $(GS_OPT)
sstudio: LIB = $(LinuxLIB)
sstudio: all

################################ Sun 
sun:  all
sun: FOR = f77 -silent -e -D__sun_c__ $(GS_OPT)   $(GS_AIO)
sun: CC =  cc     -D__sun_c__ $(GS_OPT)   $(GS_AIO)
sun: LIB = $(SunLIB)

############################### DEC OSF/Alpha
digital : all
digital:=FOR = f77 -extend_source -DDigital -arch host  $(GS_AIO)
digital:= CC = cc -DDigital -arch host  $(GS_AIO)
digital:=LIB = $(DigitalLIB)

all: $(P00) mat_stop

$(P00): $(P00).o $(P01).o $(P02).o $(P03).o $(P04).o $(P05).o $(P06).o $(P07).o $(P08).o $(P09).o $(P10).o $(P11).o $(P12).o \
	$(U1).o $(U2).o $(U3).o $(U4).o $(U5).o $(U6).o $(U7).o $(U8).o $(U9).o $(UC).o \
	$(LIBS)
	$(FOR)  $(P00).o $(P01).o $(P02).o $(P03).o $(P04).o $(P05).o $(P06).o $(P07).o $(P08).o $(P09).o $(P10).o $(P11).o $(P12).o \
	$(U1).o $(U2).o $(U3).o $(U4).o $(U5).o $(U6).o $(U7).o $(U8).o $(U9).o $(UC).o \
	$(LIB)  -o $(P00)

$(P00).o:$(P00).F $(INCL)
	$(FOR) $(OPT)  $(P00).F

$(P01).o:$(P01).F $(INCL)
	$(FOR) $(OPT)  $(P01).F

$(P02).o:$(P02).F $(INCL)
	$(FOR) $(OPT)  $(P02).F

$(P03).o:$(P03).F $(INCL)
	$(FOR) $(OPT)  $(P03).F

$(P04).o:$(P04).F $(INCL)
	$(FOR) $(OPT)  $(P04).F

$(P05).o:$(P05).F $(INCL)
	$(FOR) $(OPT)  $(P05).F

$(P06).o:$(P06).c
	$(CC) $(OPT) $(P06).c
	
$(P07).o:$(P07).c
	$(CC) $(OPT) $(P07).c

$(P08).o:$(P08).c
	$(CC) $(OPT) $(P08).c

$(P09).o:$(P09).c
	$(CC) $(OPT) $(P09).c

$(P10).o:$(P10).c
	$(CC) $(OPT) $(P10).c

$(P11).o:$(P11).c
	$(CC) $(OPT) $(P11).c

$(P12).o:$(P12).c
	$(CC) $(OPT) $(P12).c

$(U1).o:$(U1).F $(INCL)
	$(FOR) $(OPT)  $(U1).F

$(U2).o:$(U2).F $(INCL)
	$(FOR) $(OPT)  $(U2).F

$(U3).o:$(U3).F $(INCL)
	$(FOR) $(OPT)  $(U3).F

$(U4).o:$(U4).F $(INCL)
	$(FOR) $(OPT)  $(U4).F

$(U5).o:$(U5).F $(INCL)
	$(FOR) $(OPT)  $(U5).F

$(U6).o:$(U6).F $(INCL)
	$(FOR) $(OPT)  $(U6).F

$(U7).o:$(U7).F $(INCL)
	$(FOR) $(OPT)  $(U7).F

$(U8).o:$(U8).F $(INCL)
	$(FOR) $(OPT)  $(U8).F

$(U9).o:$(U9).F $(INCL)
	$(FOR) $(OPT)  $(U9).F

$(UC).o:$(UC).c
	$(CC) $(OPT) $(UC).c

clean:
	rm -rf core $(P00) *.o

mat_stop:mat_stop.F 
	$(FOR) -o mat_stop mat_stop.F 

