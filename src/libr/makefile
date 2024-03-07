CMAT = ../cmat/
TAPE = ../tape/

P00 = inter_inp
P01 = inter_drws
P02 = inter_rm3d
P03 = inter_phys
P04 = inter_spec
P05 = inter_sys
P06 = inter_term
P07 = inter_var1
P08 = inter_var2
P09 = inter_vms
P10 = inter_c
P11 = inter_cmath
P12 = inter_isl
P13 = inter_inpx
P14 = lineget
P15 = inter_cspec

P20 = displib
P21 = iadlib
P22 = laslib
P23 = banlib
P24 = tkt
P25 = tapelib
P26 = mt
P27 = tekmodes

P30 = ivflib
P31 = complib
P32 = memclib
P33 = mlmlib
P34 = cmtlib
P35 = cmtinc
P36 = cmtspe

P40 = autobgmin


############################### PGI compilers for Linux/ix86 and MacOS
pgi: FOR = pgf77 -Mextend  -Mdollar,X -c $(GS_OPT) $(GS_ONLINE) $(GS_AIO)
pgi: CC  = pgcc  -c  $(GS_ONLINE) $(GS_AIO)
pgi: all

############################### Intel compilers on Linux/ix86 and 
intel: FOR = ifort @../../etc/ifc.cfg  -c $(GS_OPT) $(GS_ONLINE) $(GS_AIO)
intel: CC  = icc @../../etc/icc.cfg -c    $(GS_OPT) $(GS_ONLINE) $(GS_AIO)
intel: all

############################### GCC(>6.x) compilers on Linux/ix86 and MacOS
gnu: FOR = $(GNU_FOR) -c $(GS_OPT) $(GS_ONLINE) $(GS_AIO)
gnu: CC  = $(GNU_CC)  -c    $(GS_OPT) $(GS_ONLINE) $(GS_AIO)
gnu: all

############################### Solaris Studio FORTRAN compiler for Linux/ix86
sstudio: FOR = sunf90 -silent -f77 -ftrap=%none -e -D__sun_c__  -c $(GS_OPT) $(GS_ONLINE) $(GS_AIO)
sstudio: CC  = suncc  -c $(GS_ONLINE) $(GS_AIO) $(GS_OPT)
sstudio: all


################################ Sun 
sun: all
sun: FOR = f77 -silent -e -D__sun_c__  $(GS_OPT) -c $(GS_ONLINE) $(GS_AIO)
sun: CC = cc -D__sun_c__       $(GS_OPT) -c $(GS_ONLINE) $(GS_AIO)

############################### DEC OSF/Alpha
digital : all
digital:=FOR = f77 -extend_source -c -DDigital -arch host $(GS_ONLINE) $(GS_AIO)
digital:= CC = cc -c -DDigital -arch host $(GS_ONLINE) $(GS_AIO)



all:     $(P00).o $(P01).o $(P02).o $(P03).o $(P04).o $(P05).o $(P06).o $(P07).o \
         $(P08).o $(P09).o $(P10).o  $(P11).o $(P12).o $(P13).o $(P14).o $(P15).o\
         $(P20).o $(P21).o $(P22).o $(P23).o $(P24).o $(P25).o $(P26).o $(P27).o \
         $(P30).o $(P31).o $(P32).o $(P33).o $(P34).o $(P35).o $(P36).o $(P40).o

$(P00).o:$(P00).F
	$(FOR) $(P00).F
	ar  -rs libinter.a $(P00).o

$(P01).o:$(P01).F
	$(FOR) $(P01).F
	ar  -rs libinter.a $(P01).o

$(P02).o:$(P02).F
	$(FOR) $(P02).F
	ar  -rs libinter.a $(P02).o

$(P03).o:$(P03).F
	$(FOR) $(P03).F
	ar  -rs libinter.a $(P03).o

$(P04).o:$(P04).F
	$(FOR) $(P04).F
	ar  -rs libinter.a $(P04).o

$(P05).o:$(P05).F
	$(FOR) $(P05).F
	ar  -rs libinter.a $(P05).o

$(P06).o:$(P06).F
	$(FOR) $(P06).F
	ar  -rs libinter.a $(P06).o

$(P07).o:$(P07).F
	$(FOR) $(P07).F
	ar  -rs libinter.a $(P07).o

$(P08).o:$(P08).F
	$(FOR) $(P08).F
	ar  -rs libinter.a $(P08).o

$(P09).o:$(P09).F
	$(FOR) $(P09).F
	ar  -rs libinter.a $(P09).o

$(P10).o:$(P10).c
	$(CC) $(P10).c
	ar  -rs libinter.a $(P10).o

$(P11).o:$(P11).c
	$(CC) $(P11).c
	ar  -rs libinter.a $(P11).o

$(P12).o:$(P12).c
	$(CC) $(P12).c

$(P13).o:$(P13).c
	$(CC) $(P13).c

$(P14).o:$(P14).c
	$(CC) $(P14).c
	ar  -rs libinter.a $(P14).o

$(P15).o:$(P15).c
	$(CC) $(P15).c
	ar  -rs libinter.a $(P15).o


$(P20).o:$(P20).F
	$(FOR) $(P20).F
	ar  -rs libinter.a $(P20).o

$(P21).o:$(P21).F
	$(FOR) $(P21).F
	ar  -rs libinter.a $(P21).o

$(P22).o:$(P22).F
	$(FOR) $(P22).F
	ar  -rs libinter.a $(P22).o

$(P23).o:$(P23).F
	$(FOR) $(P23).F
	ar  -rs libinter.a $(P23).o

$(P24).o:$(P24).F $(P24).inc
	$(FOR) $(P24).F
	ar  -rs libinter.a $(P24).o

$(P25).o:$(TAPE)$(P25).F
	$(FOR) $(TAPE)$(P25).F
	ar  -rs libinter.a $(P25).o

$(P26).o:$(TAPE)$(P26).c
	$(CC) -I$(TAPE) $(TAPE)$(P26).c
	ar  -rs libinter.a $(P26).o

$(P27).o:$(P27).c
	$(CC) $(P27).c
	ar  -rs libinter.a $(P27).o

$(P30).o:$(CMAT)$(P30).F $(CMAT)$(P30).def $(CMAT)$(P30).par
	cp $(CMAT)$(P30).F ./$(P30).F
	cp $(CMAT)$(P30).def .
	cp $(CMAT)$(P30).par .
	$(FOR) $(P30).F
	ar  -rs libinter.a $(P30).o
	rm -f $(P30).F  $(P30).def  $(P30).par 

$(P31).o:$(CMAT)$(P31).c
	cp $(CMAT)$(P31).c .
	$(CC) $(P31).c
	ar  -rs libinter.a $(P31).o
	rm -f $(P31).c

$(P32).o:$(CMAT)$(P32).F $(CMAT)$(P32).inc
	cp -f $(CMAT)$(P32).F ./$(P32).F
	cp -f $(CMAT)$(P32).inc .
	$(FOR) $(P32).F
	ar  -rs libinter.a $(P32).o
	rm -f $(P32).F $(P32).inc

$(P33).o:$(CMAT)$(P33).c
	cp -f $(CMAT)$(P33).c .
	$(CC) $(P33).c
	ar  -rs libinter.a $(P33).o
	rm -f $(P33).c

$(P34).o:$(CMAT)$(P34).F $(CMAT)$(P34).inc $(CMAT)$(P34).def $(CMAT)$(P34).par
	cp -f $(CMAT)$(P34).F ./$(P34).F
	cp -f $(CMAT)$(P34).inc .
	cp -f $(CMAT)$(P34).def .
	cp -f $(CMAT)$(P34).par .
	$(FOR)  $(P34).F
	ar  -rs libinter.a $(P34).o
	rm -f $(P34).F $(P34).inc $(P34).def $(P34).par
	
$(P35).o:$(CMAT)$(P35).F $(CMAT)$(P34).inc $(CMAT)$(P34).def $(CMAT)$(P34).par
	cp -f $(CMAT)$(P35).F ./$(P35).F
	cp -f $(CMAT)$(P34).inc .
	cp -f $(CMAT)$(P34).def .
	cp -f $(CMAT)$(P34).par .
	$(FOR) $(P35).F
	ar  -rs libinter.a $(P35).o
	rm -f $(P35).F $(P34).inc $(P34).def $(P34).par

$(P36).o:$(CMAT)$(P36).F $(CMAT)$(P34).inc $(CMAT)$(P34).def $(CMAT)$(P34).par
	cp -f $(CMAT)$(P36).F ./$(P36).F
	cp -f $(CMAT)$(P34).inc .
	cp -f $(CMAT)$(P34).def .
	cp -f $(CMAT)$(P34).par .
	$(FOR) $(P36).F
	ar  -rs libinter.a $(P36).o
	rm -f $(P36).F $(P34).inc $(P34).def $(P34).par

$(P40).o:$(P40).c
	$(CC) $(P40).c
	ar  -rs libinter.a $(P40).o

clean:
	rm -f *.o *.a *~

