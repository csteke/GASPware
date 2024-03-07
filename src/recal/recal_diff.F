	program recaldiff

c  calcola la differenza fra due diverse ricalibrazioni 

	PARAMETER (MAXGER=256)
	PARAMETER (MAXORD=10)
	PARAMETER (MAXRES=8192)
	character calf1*60
	character calf2*60
	logical*1 spekwrite,scritte
	real	coef1(0:maxord)
	real	coef2(0:maxord)
	real	spek(0:MAXRES-1)
	CHARACTER*60 SPEKNAM
	logical*1 inp_not
	external inp_not
#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif

	call xinpmode(1)

	call inp_str('File_1 con i coefficienti',calf1)
	OPEN(UNIT=21,FILE=CALF1,STATUS='OLD')
	call inp_str('File_2 con i coefficienti',calf2)
	OPEN(UNIT=22,FILE=CALF2,STATUS='OLD')

	nchan1=0
	nchan2=MAXRES-1
	call inp_i2('Intervallo di canali da calcolare ',nchan1,nchan2)
	nchan1=max(nchan1,0)
	nchan2=min(nchan2,MAXRES-1)
	if(nchan1.gt.nchan2) call swapI(nchan1,nchan2)
	nchanw=nchan2/1024 +1
	spekwrite=inp_not('Scrivi gli spettri differenza')
	scritte=inp_not('Scritte estese')

10	READ(21,*,ERR=30,END=30) I1TAP,I1ADC,N1CO,(coef1(J),J=0,N1Co-1)
	N1ORD=N1CO-1
	if(I1ADC.lt.0 .or. I1ADC.ge.MAXGER .or.
	1  N1ORD.le.0 .or. N1ORD.ge.MAXORD) then
	   write(6,*) 'Error reading  ',calf1
	   call exit
	endif
	ngiro=0

20	READ(22,*,ERR=30,END=35) I2TAP,I2ADC,N2CO,(coef2(J),J=0,N2Co-1)
	N2ORD=N2CO-1
	if(I2ADC.lt.0 .or. I2ADC.ge.MAXGER .or.
	1  N2ORD.le.0 .or. N2ORD.ge.MAXORD) then
	   write(6,*) 'Error reading  ',calf2
	   call exit
	endif
	if(i2tap.ne.i1tap .or. i2adc.ne.i1adc) goto 20
	if(scritte) then
#if defined( __GFORTRAN__ )
	  write(dyn_format,'(a,i0,a)')'(I5,I5,I3,F10.3,F10.6,',N1ORD,'G14.6)'
	  write(6,dyn_format(1:len(dyn_format))) I1TAP,I1ADC,N1ORD,(Coef1(j),j=0,N1ORD)
	  write(dyn_format,'(a,i0,a)')'(5x,5x,I3,F10.3,F10.6,',N2ORD,'G14.6)'
	  write(6,dyn_format(1:len(dyn_format))) N2ORD,(Coef2(j),j=0,N2ORD)
#else
	  write(6,'(I5,I5,I3,F10.3,F10.6,<N1ORD>G14.6)') I1TAP,I1ADC,N1ORD,(Coef1(j),j=0,N1ORD)
	  write(6,'(5x,5x,I3,F10.3,F10.6,<N2ORD>G14.6)') N2ORD,(Coef2(j),j=0,N2ORD)
#endif
	endif
	DIFFMAX=0
	IIMAX=0
	do ii=nchan1,nchan2
	  XX=II
	  DIFFII=POL(XX,COEF1,N1CO)-POL(XX,COEF2,N2CO)
	  IF(ABS(DIFFII).GT.ABS(DIFFMAX)) THEN
		DIFFMAX=DIFFII
		IIMAX=II
	  ENDIF
	  SPEK(II)=DIFFII*1000
	ENDDO
	WRITE(SPEKNAM,'(''DIFF'',I2.2,''.'',I4.4)')I1ADC,I1TAP		
	LSPEKNAM=LENGTHC(SPEKNAM)
	if(spekwrite) CALL WRITESPEC(SPEKNAM,SPEK,'L',nchanw,KV)
	if(kv.le.0) then
	  write(6,'(i4,i4,''    Diffmax='',f12.3,''  at chan='',I5)')
	1	i1tap,i1adc,DIFFMAX,IIMAX
	else
	  nkap=(kv/100+1023)/1024
	  write(6,'(i4,i3,''    Diffmax='',f12.3,''  at chan='',I5,5x,a,i1)')
	1	i1tap,i1adc,DIFFMAX,IIMAX,SPEKNAM(1:LSPEKNAM)//'|L:',nkap
	endif
	goto 10

30	call exit

35	if(ngiro.gt.0)then
#if defined( __GFORTRAN__ )
	   write(dyn_format,'(a,i0,a)')'(I5,I5,I3,F10.3,F10.6,',N1ORD,'G14.6)'
	   write(6,dyn_format(1:len(dyn_format))) I1TAP,I1ADC,N1ORD,(Coef1(j),j=0,N1ORD)
#else
	   write(6,'(I5,I5,I3,F10.3,F10.6,<N1ORD>G14.6)') I1TAP,I1ADC,N1ORD,(Coef1(j),j=0,N1ORD)
#endif
	   write(6,*) 'coefficients not found  in  ',calf2
	   goto 10
	endif
	ngiro=ngiro+1
	rewind(22)
	goto 20

	end
