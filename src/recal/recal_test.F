	PROGRAM recaltest

	PARAMETER (MAXRES=32768)
	PARAMETER (MAXGER=256)
	PARAMETER (MAXNCHAN=MAXRES*MAXGER)
	PARAMETER (MAXORD=10)
	PARAMETER (MAXRUNS=10000)

	real   fout(0:MAXNCHAN-1)
	real   ftot(0:MAXNCHAN-1)
	common/spectra/ fout,ftot
	logical*1 inp_yes
	external inp_yes

! produce proiezioni ricalibrate secondo coefficienti polinomiali letti da file

	call xinpmode(1)

	write(6,*)
	write(6,*) '  Program to recalibrate spectra using polynomial coefficients'
	write(6,*)

	if(inp_yes('Using RUN# dependent recalibration coefficients')) then
	   call recal_individual
	else
	   call recal_norun
	endif

	call exit(0)

	end

	subroutine recal_individual

	PARAMETER (MAXRES=8192)
	PARAMETER (MAXGER=256)
	PARAMETER (MAXNCHAN=MAXRES*MAXGER)
	PARAMETER (MAXORD=10)
	PARAMETER (MAXRUNS=10000)

	real   fout(0:MAXNCHAN-1)
	real   ftot(0:MAXNCHAN-1)
	common/spectra/ fout,ftot

	integer run(0:MAXRUNS-1)
	real   temp(0:MAXORD)
	real  fspek(0:MAXRES-1)
	real   sumtot(0:MAXRES-1)

	character*60 calfile
	character fname*60
	character cfn*60,rcfn*60
	logical*1 individual
	character*1 fmti,fmto
	logical*1 inp_not

	CALL inp_str('File with the recal. coefficients',calfile)
	lcalfile=lengthc(calfile)
	LUNC=4
	OPEN(UNIT=LUNC,FILE=calfile,FORM='FORMATTED',STATUS='OLD')

	write(6,*)
	write(6,*)'  The program generates spectra names in the form of nnn.rrrr'
	write(6,*)'  rrrr and nnn are RUN# and ADC# taken from '//calfile(1:lcalfile)
	cfn=' '
	call inp_str('Please give the part of the filename preceding  nnn.rrrr (if any) ',cfn)
	lcfn=lengthc(cfn)
	if(lcfn.lt.0) call exit(0)

	IIM=INDEX(cfn(1:lcfn),'#')
	IF(IIM.LE.0) IIM=INDEX(cfn(1:lcfn),'%')
	if(iim.gt.1) then
	  rcfn='R'//cfn(1:iim-1)//cfn(iim+1:)
	elseif(iim.eq.1) then
	  rcfn='R'
	else
	  rcfn='R'//cfn(1:lcfn)
	endif	     
	lrcfn=lengthc(rcfn)

	fmti='L'
	call inp_ch('Format (UIRLAH)',fmti)
	call str_toupper(fmti)
	fmto=fmti
	if(fmto.eq.'H' .OR. fmto.eq.'G') fmto='L'
	nchan=4096
	call inp_i1('# of channels',nchan)
	nkap=(nchan+1023)/1024
	nchan=nkap*1024
	dispr=1.
	call inp_r1('keV/chan of the recalibrated spectra',dispr)

	individual=inp_not('Write individual spectra')

	mmaxadc=-1
	do ii=0,maxruns-1
	  run(ii)=-1
	enddo
10	READ(LUNC,*,ERR=13,END=13) IlTAP,IlADC
	if(iltap.ge.0 .and. iltap.lt.maxruns) run(iltap)=1
	mmaxadc=max(mmaxadc,IlADC)
	goto 10
13	ntchan=nchan*(mmaxadc+1)
	if(ntchan.gt.MAXNCHAN) then
	   write(6,*) 'Packed  spectra length is ',ntchan
	   write(6,*) 'Maximum allowed length is ',MAXNCHAN
	   call ansi_bell(6)
	   call exit(0)
	endif
	write(6,*) 'Length of packed spectra is',(ntchan+1023)/1024,' K'
	rewind(lunc)

	do irun=0,maxruns
	  if(run(irun).ne.1) goto 40
	  do ii=0,ntchan-1
	    fout(ii)=0.
	  enddo
20	  READ(LUNC,*,ERR=30,END=30) IlTAP,IlADC,NlCO,(temp(J),J=0,NlCo-1)
	  if(iltap.ne.irun) goto 20
	  NlORD=NlCO-1
	  if(IlADC.ge.0 .and. IlADC.lt.MAXGER) then
	    if(NlORD.gt.0 .and. NlORD.le.MAXORD) then
C	      write(6,'(I5,I5,I3,F10.3,F10.6,<NlORD>G14.6)') IlTAP,IlADC,NlCO,(Temp(j),j=0,NlORD)
	      write(fname,'(''#'',i3.3,''.'',i4.4)') Iladc,Iltap
	      LFNAME=MAX(1,LENGTHC(FNAME))
	      if(lcfn.gt.0) fname=cfn(1:lcfn)//fname(1:lfname)
	      LFNAME=MAX(1,LENGTHC(FNAME))
	      write(6,*) 'Reading   '//fname(1:lfname)
	      CALL READSPEC(FNAME,fSPEK,fmti,nchan,KV)
	      IF(KV.LE.0) THEN
	        write(6,*) fname(1:lfname),'   Read error',kv
	        goto 20
	      ENDIF
	      nchoffs=iladc*nchan
	      call shiftspek(temp,NlORD+1,fspek,fout(nchoffs),nchan,dispr)
	    endif
	  endif
	  goto 20
30	  rewind(lunc)
	  if(individual) then
	    write(fname,'(A,i4.4)') rcfn(1:lrcfn)//'.',irun
	    lfname=lengthc(fname)
	    CALL writeSPEC(fname,fout,fmto,ntchan,KV)
	    if(kv.gt.0) then
	      write(6,*) 'Writing   '//fname(:lfname)
	    else
	      call ansi_bell(6)
	      write(6,*) fname(:lfname)//'   Write error'
	    endif
	    write(6,*)
	  endif
	  do ii=0,ntchan-1
	    ftot(ii)=ftot(ii)+fout(ii)
	  enddo
40	enddo

	fname=rcfn(1:lrcfn)//'.SUM'
	lfname=lengthc(fname)
	CALL writeSPEC(fname,ftot,fmto,ntchan,KV)
	if(kv.gt.0) then
	  write(6,*) fname(:lfname)//'   Sum over RUN number'
	else
	  call ansi_bell(6)
	  write(6,*) fname(:lfname)//'   Write error'
	endif

	do ii=0,nchan-1
	   sumtot(ii)=0
	enddo
	do ii=0,mmaxadc
	  nchoffs=ii*nchan
	  do jj=0,nchan-1
	     sumtot(jj)=sumtot(jj)+ftot(nchoffs+jj)
	  enddo
	enddo
	fname='R'//fname(1:lfname)
	lfname=lengthc(fname)
	CALL writeSPEC(fname,sumtot,fmto,nchan,KV)
	if(kv.gt.0) then
	  write(6,*) fname(:lfname)//'  Sum over ADC and RUN number'
	else
	  call ansi_bell(6)
	  write(6,*) fname(:lfname)//'   Write error'
	endif

	return

	END

	subroutine recal_norun

	PARAMETER (MAXRES=8192)
	PARAMETER (MAXGER=256)
	PARAMETER (MAXNCHAN=MAXRES*MAXGER)
	PARAMETER (MAXORD=10)
	PARAMETER (MAXRUNS=10000)

	real   fout(0:MAXNCHAN-1)
	real   ftot(0:MAXNCHAN-1)
	common/spectra/ fout,ftot

	real  temp(0:maxord)
	real coefs(0:MAXORD,0:MAXGER)
	integer ncoefs(0:maxger)
	character*60 calfile
	character*60 specfile
	character fname1*60,fname2*64
	character*1 fmti,fmto
#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif

	CALL inp_str('File with the calibration coefficients',calfile)
	LUNC=21
	OPEN(UNIT=LUNC,FILE=calfile,FORM='FORMATTED',STATUS='OLD')

	write(6,*) '  The program assumes that the spectra of the different ADC''s'
	write(6,*) '  are packed together into a single file'

	CALL inp_str('File containing the filenames of the packed spectra ',specfile)
	LUNS=22

	OPEN(UNIT=LUNS,FILE=specfile,FORM='FORMATTED',STATUS='OLD')

	fmti='L'
	call inp_ch('Format (UIRLAH)',fmti)
	call str_toupper(fmti)
	fmto=fmti
	if(fmto.eq.'H' .OR. fmto.eq.'G') fmto='L'
	nchan=4096
	call inp_i1('# di canali',nchan)
	nkap=(nchan+1023)/1024
	nchan=nkap*1024
	dispr=1.
	call inp_r1('keV/chan of the recalibrated spectra',dispr)

	maxadc=-1
20	READ(LUNC,*,ERR=30,END=32) IlTAP,IlADC,NlCO,(temp(J),J=0,NlCo-1)
	NlORD=NlCO-1
#if defined( __GFORTRAN__ )
	write(dyn_format,'(a,i0,a)')'(I5,I5,I3,F10.3,F10.6,',NlORD,'G14.6)'
	write(6,dyn_format(1:len(dyn_format))) IlTAP,IlADC,NlCO,(Temp(j),j=0,NlORD)
#else
	write(6,1001) IlTAP,IlADC,NlCO,(Temp(j),j=0,NlORD)
1001	format(I5,I5,I3,F10.3,F10.6,<NlORD>G14.6)
#endif
	if((IlADC.ge.0 .and. IlADC.lt.MAXGER) .and. (NlORD.gt.0 .and. NlORD.le.MAXORD)) then
	   do ii=0,NlCo-1
	      coefs(ii,IlADC)=temp(ii)
	   enddo
	   ncoefs(IlADC)=NlCo
	   maxadc=max(maxadc,IlADC)
	else
	   write(6,*) 'Error reading calibration coefficients'
	   call exit(0)
	endif
	goto 20

30	write(6,*) 'Error reading   ',calfile
	call exit(0)

32	nchantot=nchan*(maxadc+1)
	READ(LUNS,'(a)',err=40,end=42) fname1
	fname2=fname1
	lfname2=lengthc(fname2)
	write(6,*) 'Reading   ',fname2(1:lfname2)
	CALL READSPEC(fname2,fout,fmti,nchantot,KV)
	IF(KV.LE.0) THEN
	   write(6,*) fname2(1:lfname2),'   Read error',kv
	   call ansi_bell(6)
	   goto 32
	ENDIF
	
	do ii=0,maxadc
	  call shiftspek(coefs(0,ii),ncoefs(ii),fout(ii*nchan),ftot(ii*nchan),nchan,dispr)
	enddo
	fname2='R'//fname1
	lfname2=lengthc(fname2)
	write(6,*) 'Writing   ',fname2(1:lfname2)
	CALL WRITESPEC(FNAME2,ftot,fmto,nchantot,KV)
	IF(KV.LE.0) THEN
	   write(6,*) fname2(1:lfname2),'   Write error',kv
	   call ansi_bell(6)
	   goto 32
	ENDIF
	goto 32

40	write(6,*) 'Error reading   ',specfile
	return

42	return

	END

	SUBROUTINE SHIFTSPEK(a,nord,spek1,spek2,nchan,efact)

	DIMENSION spek1(0:nchan-1),spek2(0:nchan-1)
	DIMENSION a(0:nord-1)
	DIMENSION rp(0:9)
	logical*1   forward

	ilim(i1,i2)=max(0,min(i1,i2))

	if(nord.LE.0 .OR. nord.GT.10 .OR. efact.EQ.0) return
	
	do ii=0,nord-1
	  rp(ii)=a(ii)/efact
	enddo
	nchan1=nchan-1

	do ii=0,nchan1
	  spek2(ii)=0.
	enddo

	yyl=pol(0.,rp,nord)
	do jj=0,nchan1
	  yyr=pol(jj+1.,rp,nord)
	  if(yyl.le.yyr) then
	    forward=.true.
	  else
	    forward=.false.
	    xx=yyl
	    yyl=yyr
	    yyr=xx
	  endif
	  iyl=yyl
	  iyr=yyr
	  xx=spek1(jj)
	  if(iyr.eq.iyl) then
	    kk=ilim(iyl,nchan1)
	    spek2(kk)=spek2(kk)+xx
	  else
	    xx=xx/(yyr-yyl)
	    kk=ilim(iyl,nchan1)
	    spek2(kk)=spek2(kk)+xx*(iyl+1-yyl)
	    do j=iyl+1,iyr-1
	      kk=ilim(j,nchan1)
	      spek2(kk)=spek2(kk)+xx
	    enddo
	    kk=ilim(iyr,nchan1)
	    spek2(kk)=spek2(kk)+xx*(yyr-iyr)
	  endif
	  if(forward) yyl=yyr
	enddo

	return

	end
