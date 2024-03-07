	program recal_corr

	PARAMETER (MAXGER=256)
	PARAMETER (MAXORD=10)
	character calflin*60
	character calfcur*60
	character calfout*60

	real	coefcur(0:maxord,0:maxger-1)
	integer ordcur(0:maxger-1)
	real	temp(0:300)
	real*8	tab(0:maxord,0:maxord),TTAB
	logical inp_yes
	external inp_yes
#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif
	call xinpmode(1)

	dispc=1.

	write(6,*)
	write(6,*) '   The program modifies a file with recalibration coefficients'
	write(6,*) '   according to the coefficients contained in another file'
	write(6,*)

	if(inp_yes('Recalibration with pol. degree higher than 1')) goto 1000
	write(6,*)
 
c  Corregge i coefficienti di ricalibrazione LINEARI della prima passata
c  sulla base dei coefficienti POLINOMIALI ottenuti nella seconda passata

	calflin='callin.cal'
	calfcur='calcur.cal'
	calfout='ecal.cal'
	call inp_str('File with linear recalibration coefficients',calflin)
	OPEN(UNIT=21,FILE=CALFlin,STATUS='OLD')
	call inp_str('File containing the modifying  coefficients',calfcur)
	OPEN(UNIT=22,FILE=CALFcur,STATUS='OLD')
	call inp_r1('keV/chan of the spectra used to produce the  modifying coefficients ',dispc)
	call inp_str('File with the modified coefficients ',calfout)
	OPEN(UNIT=23,FILE=CALFout,STATUS='NEW')

10	READ(22,*,ERR=20,END=20) IcTAP,IcADC,NcCO,(temp(J),J=0,NcCo-1)
	NcORD=NcCO-1
	if(IcADC.ge.0 .and. IcADC.lt.MAXGER .and.
	1  NcORD.gt.0 .and. NcORD.lt.MAXORD) then
	  ordcur(IcADC)=NcORD
	  do ii=0,NcORD
	    coefcur(ii,IcADC)=temp(ii)
	  enddo
#if defined( __GFORTRAN__ )
	  write(dyn_format,'(a,i0,a)')'(I5,I3,F10.3,F10.6,',NcORD,'G14.6)'
	  write(6,dyn_format(1:len(dyn_format))) IcADC,NcORD+1,(Temp(j),j=0,NcORD)
#else
	  write(6,101) IcADC,NcORD+1,(Temp(j),j=0,NcORD)
101	  format(I5,I3,F10.3,F10.6,<NcORD>G14.6)
#endif
	else
	  write(6,*) 'Error reading  ',calfcur
	  call exit
	endif
	goto 10

20	READ(21,*,ERR=30,END=30) IlTAP,IlADC,NlCO,(temp(J),J=0,NlCo-1)
	NlORD=NlCO-1
	if(IlADC.ge.0 .and. IlADC.lt.MAXGER .and. NlORD.eq.1) then
#if defined( __GFORTRAN__ )
	  write(dyn_format,'(a,i0,a)')'(I5,I5,I3,F10.3,F10.6,',NlORD,'G14.6)'
	  write(6,dyn_format(1:len(dyn_format))) IlTAP,IlADC,NlORD+1,(Temp(j),j=0,NlORD)
#else
	  write(6,102) IlTAP,IlADC,NlORD+1,(Temp(j),j=0,NlORD)
102	  format(I5,I5,I3,F10.3,F10.6,<NlORD>G14.6)
#endif
	  do ii=0,NlORD
	  temp(ii)=temp(ii)/dispc
	  enddo
	  do ii=0,maxord
	  do jj=0,maxord
	    tab(jj,ii)=0
	  enddo
	  enddo
	  tab(0,0)=1
	  NcORD=ordcur(IlADC)
	  if(NcORD.gt.0) then
	    do jj=1,NcORD
	    do ii=0,jj-1
	      TTAB=TAB(jj-1,ii)
	      TAB(jj,ii  )=TAB(jj,ii  )+TTAB*TEMP(0)
	      TAB(jj,ii+1)=TAB(jj,ii+1)+TTAB*TEMP(1)
	    enddo
	    enddo
	    do ii=0,NcORD
	      TTAB=0
	      do jj=ii,NcORD
	        TTAB=TTAB+coefcur(jj,IlADC)*TAB(jj,ii)
	      enddo
	      Temp(ii)=TTAB
	    enddo
#if defined( __GFORTRAN__ )
	    write(dyn_format,'(a,i0,a)')'(5X,5X,I3,F10.3,F10.6,',NcORD,'G14.6)'
	    write(6, dyn_format(1:len(dyn_format)))  NcORD+1,(Temp(j),j=0,NcORD)
	    write(dyn_format,'(a,i0,a)')'(I5,I5,I3,F10.3,F10.6,',NcORD,'G14.6)'
	    WRITE(23,dyn_format(1:len(dyn_format))) IlTAP,IlADC,NcORD+1,(Temp(j),j=0,NcORD)
#else
	    write(6, 103)             NcORD+1,(Temp(j),j=0,NcORD)
	    WRITE(23,104) IlTAP,IlADC,NcORD+1,(Temp(j),j=0,NcORD)
103	    format(5X,5X,I3,F10.3,F10.6,<NcORD>G14.6)
104	    format(I5,I5,I3,F10.3,F10.6,<NcORD>G14.6)
#endif
	  else
	     write(6,'(10X,A)') 'Missing modifying coefficients'
	  endif
	else
	  write(6,*) 'Error reading  ',calflin
	  call exit
	endif
	goto 20

30	call exit


c  Corregge i coefficienti di ricalibrazione POLINOMIALI della prima passata
c  sulla base dei coefficienti LINEARI ottenuti nella seconda passata

1000	calfcur='sources.cal'
	calflin='shifts.cal'
	calfout='ecal.cal'
	call inp_str('File with the coefficients from sources ',calfcur)
	OPEN(UNIT=21,FILE=CALFcur,STATUS='OLD')
	call inp_str('File with the shift coefficients',calflin)
	OPEN(UNIT=22,FILE=CALFlin,STATUS='OLD')
	call inp_r1('keV/chan of the spectra used to produce the shifts',dispc)
	call inp_str('File with the modified coefficients ',calfout)
	OPEN(UNIT=23,FILE=CALFout,STATUS='NEW')

1010	READ(21,*,ERR=1015,END=1015) IcTAP,IcADC,NcCO,(temp(J),J=0,NcCo-1)
	NcORD=NcCO-1
	if(IcADC.ge.0 .and. IcADC.lt.MAXGER .and.
	1  NcORD.gt.0 .and. NcORD.lt.MAXORD) then
	  ordcur(IcADC)=NcORD
	  do ii=0,NcORD
	    coefcur(ii,IcADC)=temp(ii)
	  enddo
#if defined( __GFORTRAN__ )
	  write(dyn_format,'(a,i0,a)')'(I5,I3,F10.3,F10.6,',NcORD,'G14.6)'
	  write(6,dyn_format(1:len(dyn_format))) IcADC,NcORD+1,(Temp(j),j=0,NcORD)
#else
	  write(6,105) IcADC,NcORD+1,(Temp(j),j=0,NcORD)
105	  format(I5,I3,F10.3,F10.6,<NcORD>G14.6)
#endif
	else
	  write(6,*) 'Error reading  ',calfcur
	  call exit
	endif
	goto 1010

1015	do jj=0,MAXGER-1
	do ii=0,MAXORD
	  coefcur(ii,jj)=coefcur(ii,jj)/dispc
	enddo
	enddo

1020	READ(22,*,ERR=30,END=1030) IlTAP,IlADC,NlCO,(temp(J),J=0,NlCo-1)
	NlORD=NlCO-1
	if(IlADC.ge.0 .and. IlADC.lt.MAXGER .and. NlORD.eq.1) then
#if defined( __GFORTRAN__ )
	  write(dyn_format,'(a,i0,a)')'(I5,I5,I3,F10.3,F10.6,',NlORD,'G14.6)'
	  write(6,dyn_format(1:len(dyn_format))) IlTAP,IlADC,NlORD+1,(Temp(j),j=0,NlORD)
#else
	  write(6,106) IlTAP,IlADC,NlORD+1,(Temp(j),j=0,NlORD)
106	  format(I5,I5,I3,F10.3,F10.6,<NlORD>G14.6)
#endif
	  NcORD=ordcur(IlADC)
	  if(NcORD.gt.0) then
	    do ii=0,NcORD
	      TAB(ii,0)=coefcur(ii,IlADC)*temp(1)
	    enddo
	    TAB(0,0)=TAB(0,0)+temp(0)
	    do ii=0,NcORD
	      Temp(ii)=TAB(ii,0)
	    enddo
#if defined( __GFORTRAN__ )
	    write(dyn_format,'(a,i0,a)')'(5X,5X,I3,F10.3,F10.6,',NcORD,'G14.6)'
	    write(6, dyn_format(1:len(dyn_format))) NcORD+1,(Temp(j),j=0,NcORD)
	    write(dyn_format,'(a,i0,a)')'(I5,I5,I3,F10.3,F10.6,',NcORD,'G14.6)'
	    WRITE(23,dyn_format(1:len(dyn_format))) IlTAP,IlADC,NcORD+1,(Temp(j),j=0,NcORD)
#else
	    write(6, 107)             NcORD+1,(Temp(j),j=0,NcORD)
	    WRITE(23,108) IlTAP,IlADC,NcORD+1,(Temp(j),j=0,NcORD)
107	    format(5X,5X,I3,F10.3,F10.6,<NcORD>G14.6)
108	    format(I5,I5,I3,F10.3,F10.6,<NcORD>G14.6)
#endif
	  else
	     write(6,'(10X,A)') 'Missing source coefficients'
	  endif
	else
	  write(6,*) 'Error reading  ',calflin
	  call exit
	endif
	goto 1020

1030	call exit

	end
