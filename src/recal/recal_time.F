	PROGRAM TRECAL

	PARAMETER (MAXRES=32768)

	real fspek(-2*maxres:2*maxres)
	integer*4 ncicli

	DIMENSION PM(0:5)

	CHARACTER*60 SPEclist
	character*60 fname 

	character*60  fncoefs,fnres
	character*1 fmt,ach
	character*8 form	/'UIRLAHGE'/

	logical*1 list		/.false./
	logical*1 readtapnum	/.true./
	logical*1 readadcnum	/.true./
	logical*1 writadcnum	/.false./
	logical*1 wrtcoefs	/.true./
	logical*1 baricentro	/.false./
	logical*1 writeres	/.false./
	logical*1 display		/.false./
	logical*1 multiple	/.true./

	logical*1 inp_not
	external  inp_not
	
	integer filexist
	external filexist
	
	integer get_adc_number
	integer get_tape_number
	external get_adc_number
	external get_tape_number

#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif


	call xinpmode(1)
	xfactor=1.0
	iadcold=-1

	write(6,*)
	write(6,*)'  The program finds the position of the time peak of time spectra spectra'
	write(6,*)'  and creates a recalibration file containing the corresponding shifts'
	write(6,*)

	if(inp_not('Giving a list of spectra')) then
1010	  CALL INP_STR('File containing the filenames of the time spectra',speclist)
	  list=.true.
	  LISTLUN=4
	  if( filexist( SPECLIST ) .eq. 0 )then
	     OPEN(UNIT=LISTLUN,FILE=SPECLIST,FORM='FORMATTED',STATUS='OLD')
	  else
	     write(6,*)' ERROR: file '//SPECLIST(1:max(1,lengthc(SPECLIST)))//' does non exist'
	     call flush(6)
	     goto 1010
	  endif
	  fmt='L'
	  call inp_ch('Format (UIRLA) [default '//fmt//']',fmt)
	  call str_toupper(fmt)
	  iform=index(form,fmt)
	  if(iform.le.0) stop 'Invalid format'
	  nchan=4096
	  call inp_i1('# of channels in each spectrum',nchan)
	  nkap=(nchan+1023)/1024
	  nchan=nkap*1024
	else
	  fname='GE_TIME#00'
	  fmt='H'
	  iform=6
	  nchan=4096
	  nkap=4
	  call readdat(1,FNAME,fspek(0),nchan,iform,kv)
	  IF(KV.LE.0) THEN
	    lfname=max(1,lengthc(fname))
	    write(6,*) fname(1:lfname),'   Read error',kv
	    call exit(0)
	  ELSE
	    iform=mod(kv,100)
	    fmt=form(iform:iform)
	    nkap=(kv/100+1023)/1024
	    nchan=nkap*1024
	  endif
	  list=.false.
	endif

	iwlc=0.01*nchan
	iwhc=0.99*nchan
	CALL INP_I2('Region to be searched for the maximum',IWLC,IWHC)
	NCAN=IWHC-IWLC+1

	iposfin=nchan/2
	call inp_i1('Final position of the prompt peak',iposfin)

	ach='M'
	call inp_ch('Use (M)aximum channel or (C)enter of mass of time peaks [M] ',ach)
	if(ach.eq.'c' .OR. ach.eq.'C') baricentro=.TRUE.
	if(baricentro) call inp_r1('How many FWHM to use for center of mass calculation',xfactor)

	itap=1
	call inp_ask('Does the filename contain the proper RUN# ',readtapnum)
	if(.not.readtapnum) then
	   call inp_i1('Please give the RUN number',ITAP)
	   if(itap.le.0) call exit(0)
	endif
	call inp_ask('Does the filename contain the proper ADC# ',readadcnum)
	if(readadcnum) then
	   writadcnum=.false.
	else
	   call inp_i1('Please give the ADC number',IADC)
	   writadcnum=.true.
	endif
	
	call inp_ask('Write out Time Calibration coefficents',Wrtcoefs)
	if(Wrtcoefs) then
	   FNcoefs='TSHIFTS.CAL'
	   lfncoefs=max(1,lengthc(fncoefs))
	   write(6,*) 'File name will be    '//fncoefs(1:lfncoefs)
	   luc=45
	   OPEN(UNIT=luc,FILE=FNcoefs,FORM='FORMATTED',STATUS='UNKNOWN')
	endif

	call inp_ask('Write out results of calculations     ',Writeres)
	if(Writeres) then
	   FNres='TIMECAL.DAT'
	   lfnres=max(1,lengthc(fnres))
	   write(6,*) 'File name will be    '//fnres(1:lfnres)
	   lur=46
	   OPEN(UNIT=lur,FILE=FNres,FORM='FORMATTED',STATUS='UNKNOWN')
	endif

*	call inp_ask('Want to display the shifted spectra',display)
*	if(display) then
*	   write(6,*) 'Please enter graphic mode and then press <RET>'
*	   call inp_i1(' ',iii)
*	endif

	call inp_ask('Want to cycle on the ADCs',multiple)
	if(multiple) then
	  writadcnum=.true.
	  ncicli=40
	  call inp_i1('How many times',ncicli)
	  nldot=1
	  if(ncicli.ge.10 ) nldot=nldot+1
	  if(ncicli.ge.100) nldot=nldot+1
	endif

1	if(list) READ(LISTLUN,'(A)',ERR=200,END=200) FNAME
	if(writadcnum) then
#if defined( __PGI )
	  ldir=rindex(fname,'/')
#else
	  ldir=index(fname,'/', BACK=.TRUE.)
#endif
	  ldot=index(fname(ldir+1:),'.')
	  if(ldot.le.0) then
	    ldot=lengthc(fname)+1
	  else
	    ldot=ldot+ldir
	  endif
	  if(ldot.gt.nldot) then
#if defined( __GFORTRAN__ )
	    write(dyn_format,'(a,i0,a,i0,a)')'(i',nldot,'.',nldot,')'
	    write(fname(ldot-nldot:ldot-1),dyn_format(1:len(dyn_format))) iadc
#else
	    write(fname(ldot-nldot:ldot-1),101) iadc
101	    format(i<nldot>.<nldot>)
#endif
	  else
	   lfname=lengthc(fname)
	   write(6,*) 'No place to write the ADC# in  ',fname(1:lfname)
	   call exit(0)
	  endif
	endif
	LFNAME=MAX(1,LENGTHC(FNAME))
	if( LFNAME .ge. 60 ) then
	   fname(60:60)='\0'
	else
	   fname(LFNAME+1:LFNAME+1) = '\0'
	endif

	CALL READSPEC(FNAME,fSPEK(0),fmt,nchan,KV)
	IF(KV.LE.0) THEN
	   write(6,*) fname(1:lfname),'   Read error',kv
	   if(list) goto 1
	   goto 200
	ENDIF


	if(readadcnum) then
	   iadc = get_adc_number( fname )
	   if( iadc .lt. 0 ) then
	     write(6,*) ' ERROR decoding ADC number in ',fname(1:lfname)
	     call ansi_bell(6)
	     call lib$wait(0.1)
	     if(list) goto 1
	     goto 200
	    endif
	endif

	if(readtapnum) then
	   itap = get_tape_number( fname )
	   if( itap .lt. 0 ) then
	     write(6,*) ' ERROR decoding RUN number in ',fname(1:lfname)
	     call ansi_bell(6)
	     call lib$wait(0.1)
	     if(list) goto 1
	     goto 200
	    endif
	endif

*	if(readadcnum) then
*	   ldir=index(fname,'#')
*	   ldot=index(fname(ldir+1:),'.')
*	   ldot=index(fname,'.')
*	   if( ldir .gt. 0)then
*	      if(ldir .lt. ldot)nldot=ldot-ldir-1
*	   endif
*	   ldot=ldot+ldir
*	   if(ldot.gt.nldot) read(fname(ldot-nldot:ldot-1),102) iadc
*102	   format(i)
*	endif
*
*	if(readtapnum) then
*	   ldir=index(fname,']')
*	   ldot=index(fname(ldir+1:),'.')
*	   LEXT=ldot+ldir
*	   lcol=index(fname(lext+1:),';')
*	   if(lcol.gt.0) then
*		ll2=lcol+lext-1
*	   else
*		ll2=lfname
*	   endif
*	   READ(FNAME(LEXT+1:Ll2),*) ITAP
*	endif

	maxch=iwlc
	xmax=fspek(iwlc)
	do ii=iwlc,iwhc
	   if(fspek(ii).gt.xmax) then
		xmax=fspek(ii)
		maxch=ii
	   endif
	enddo
	ipo=iposfin-maxch

	CALL MOMENTR(fspek(IWLC),NCAN,PM,3)
	P0=PM(0)
	IF(P0.GT.0) THEN
	   P1=PM(1)+IWLC
	   VAR=PM(2)
	   IF(VAR.LT.0) VAR=0
	   XFWHM=SQRT(VAR)*2.355
	else 
	   write(6,*) 'error with file  ',fname(1:lfname)
	   call ansi_bell(6)
	   call lib$wait(0.1)
	   if(list) goto 1
	   goto 200
	ENDIF
	iistart=max(int(maxch-xfactor*xfwhm),iwlc)
	iiend  =min(int(maxch+xfactor*xfwhm),iwhc)
	iican  =iiend-iistart+1
	CALL MOMENTR(fspek(iistart),iiCAN,PM,3)
	P0=PM(0)
	IF(P0.GT.0) THEN
	   P1=PM(1)+Iistart
	   VAR=PM(2)
	   IF(VAR.LT.0) VAR=0
	   XFWHM=SQRT(VAR)*2.355
	else 
	   write(6,*) 'error with file  ',fname(1:lfname)
	   call ansi_bell(6)
*	   call lib$wait(2.)
	   if(list) goto 1
	   goto 200
	ENDIF

	if(display) then
	   call initt(960)
	   CALL DRAW_RSPEC(fspek(0),nchan,100,900,100,600,0,0.9)
	   call vecmod
	   call home
	   call anmode
	endif
#if defined( __GFORTRAN__ )
	WRITE(6,'(1x,a,4x,i0,3(4x,g0))') fname(1:lfname),maxch,p1+0.5,p0,xfwhm
#else
	WRITE(6,'(1x,a,i,3g)') fname(1:lfname),maxch,p1+0.5,p0,xfwhm
#endif
	if(display) call lib$wait(0.1)

	if(baricentro) ipo=iposfin-(p1+0.5)
	if(wrtcoefs)WRITE(luc,'(I5,I5,I5,I12,I5)')ITAP,IADC,2,ipo,1
#if defined( __GFORTRAN__ )
	if(writeres)WRITE(lur,'(1X,A,4x,I0,3(4x,G0))')fname(1:lfname),maxch,p1+0.5,p0,xfwhm
#else
	if(writeres)WRITE(lur,'(1X,A,I,3G)')fname(1:lfname),maxch,p1+0.5,p0,xfwhm
#endif
	if(list) goto 1

200	if(multiple) then
	  iadc=iadc+1
	  ncicli=ncicli-1
	  if(ncicli.le.0) call exit(0)
	  if(list) rewind(listlun)
	  goto 1
	endif

	CALL EXIT(0)

	END
