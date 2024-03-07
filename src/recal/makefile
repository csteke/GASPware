P00 = cob
P01 = corr
P02 = diff
P03 = doppl
P04 = gain
P05 = test
P06 = time
P07 = readDBinfo
P08 = ebrecal_cob

OPT = -c

LIBS = ../libr/libinter.a ../libr/inter_isl.o ../libr/inter_inpx.o

LinuxLIB   = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -lpthread
SunLIB     = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o -lposix4
DigitalLIB = -L../libr -linter ../libr/inter_isl.o ../libr/inter_inpx.o

############################### Linux is the default choice
linux: FOR = pgf77 -Mextend  -Mdollar,X $(GS_OPT) $(GS_ONLINE)
linux: CC  = gcc   -mpreferred-stack-boundary=2 -march=i686 $(GS_ONLINE)
linux: LIB = $(LinuxLIB)
linux: all

############################### Intel compilers on Linux/ix86
intel: FOR = ifort @../../etc/ifc.cfg $(GS_OPT) $(GS_ONLINE)
intel: CC  = icc @../../etc/icc.cfg  -O2 $(GS_ONLINE)
intel: LIB = $(LinuxLIB)
intel: all

################################ Sun 
sun: all
sun: FOR = f77 -e -D__sun_c__ -xpp=cpp -xO2 -xtarget=native $(GS_ONLINE)
sun:  CC = cc -D__sun_c__ -xO2 -xtarget=native $(GS_ONLINE)
sun: LIB = $(SunLIB)

############################### DEC OSF/Alpha
digital : all
digital:=FOR = f77 -extend_source -DDigital -arch host $(GS_ONLINE)
digital:= CC = cc -DDigital -arch host $(GS_ONLINE)
digital:=LIB = $(DigitalLIB)


all:recal_$(P00) recal_$(P01) recal_$(P02) recal_$(P03) \
    recal_$(P04) recal_$(P05) recal_$(P06) #$(P08)


recal_$(P00): recal_$(P00).o $(P07).o $(LIBS)
	$(FOR) -o recal_$(P00) recal_$(P00).o $(P07).o $(LIB)
recal_$(P00).o: recal_$(P00).F 
	$(FOR) $(OPT) recal_$(P00).F

recal_$(P01): recal_$(P01).o $(LIBS)
	$(FOR) -o recal_$(P01) recal_$(P01).o  $(LIB)
recal_$(P01).o: recal_$(P01).F 
	$(FOR) $(OPT) recal_$(P01).F

recal_$(P02): recal_$(P02).o $(LIBS)
	$(FOR) -o recal_$(P02) recal_$(P02).o  $(LIB)
recal_$(P02).o: recal_$(P02).F 
	$(FOR) $(OPT) recal_$(P02).F

recal_$(P03): recal_$(P03).o $(LIBS)
	$(FOR) -o recal_$(P03) recal_$(P03).o  $(LIB)
recal_$(P03).o: recal_$(P03).F 
	$(FOR) $(OPT) recal_$(P03).F

recal_$(P04): recal_$(P04).o $(LIBS)
	$(FOR) -o recal_$(P04) recal_$(P04).o  $(LIB)
recal_$(P04).o: recal_$(P04).F 
	$(FOR) $(OPT) recal_$(P04).F

recal_$(P05): recal_$(P05).o $(LIBS)
	$(FOR) -o recal_$(P05) recal_$(P05).o  $(LIB)
recal_$(P05).o: recal_$(P05).F 
	$(FOR) $(OPT) recal_$(P05).F

recal_$(P06): recal_$(P06).o $(LIBS)
	$(FOR) -o recal_$(P06) recal_$(P06).o  $(LIB)
recal_$(P06).o: recal_$(P06).F 
	$(FOR) $(OPT) recal_$(P06).F

$(P07).o: $(P07).c
	$(CC) -c $(P07).c

#$(P08): $(P08).o $(P07).o $(LIBS)
#	$(FOR) -o $(P08) $(P08).o $(P07).o $(LIB)
#$(P08).o: $(P08).F 
#	$(FOR) $(OPT) $(P08).F

clean:
	rm -rf core *.o recal_$(P00) recal_$(P01) recal_$(P02) \
	            recal_$(P03) recal_$(P04) recal_$(P05) \
	            recal_$(P06) #$(P08)


