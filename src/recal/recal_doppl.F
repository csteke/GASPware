	program recal_doppler

!	applica una correzione doppler ai coefficenti di ricalibrazione
!	rivelatori agli angoli standard di GASP

	PARAMETER (MAXGER=256)
	PARAMETER (MAXORD=10)

	real theta(0:maxger-1)
	real phi(0:maxger-1)
	real temp(0:maxord)
	character*60 calfilei
	character*60 calfileo
	character*60 gsdir

	character*1 apparato /'G'/

#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif

	call xinpmode(1)
	call getenv('GSDIR',gsdir)
	lgsdir=lengthc(gsdir)
	call inp_ch('Using (G)asp or (E)uroball  [G]',apparato)
	call str_toupper(apparato)
	if(apparato.eq.'G') then
CVMS	  call get_detector_angles(theta,phi,40,'GASP:NUMBERING.GE',6)
	  call get_detector_angles(theta,phi,40,gsdir(1:lgsdir)//'/NUMBERING.GE',6)
	else
CVMS	  call get_detector_angles(theta,phi,239,'GASP:EBNUMBERING.GE',6)
	  call get_detector_angles(theta,phi,239,gsdir(1:lgsdir)//'/EBNUMBERING.GE',6)
	endif

	call inp_str('File with the old recal. coefficients',calfilei)
	LUNI=31
	OPEN(UNIT=LUNI,FILE=calfilei,FORM='FORMATTED',STATUS='OLD')

	vsuc=0
	call inp_r1('Recoil [+|-] v/c (%)  ',vsuc)
	avsuc=abs(vsuc/100)
	if(avsuc.ge.0.5) stop 'Please give a sensible value'

	dispersion=1
C	call inp_r1('Value of keV/chan you want to have [1] ',dispersion)
C	if(dispersion.le.0) stop 'Please give a sensible value'

	calfileo=calfilei
	call inp_str('File with the new recal. coefficients',calfileo)
	LUNO=32
	OPEN(UNIT=LUNo,FILE=calfileo,FORM='FORMATTED',STATUS='NEW')

20	READ(LUNI,*,END=30) IlTAP,IlADC,NlCO,(temp(J),J=0,NlCo-1)
	NlORD=NlCO-1
	if(IlADC.ge.0 .and. IlADC.lt.MAXGER) then
	if(NlORD.gt.0 .and. NlORD.le.MAXORD) then
#if defined( __GFORTRAN__ )
	   write(dyn_format,'(a,i0,a)')'(I5,I5,I4,F10.3,F10.6,',NlORD,'G13.5)'
	   write(6,dyn_format(1:len(dyn_format))) IlTAP,IlADC,NlCO,(Temp(j),j=0,NlORD)
#else
	   write(6,'(I5,I5,I4,F10.3,F10.6,<NlORD>G13.5)') IlTAP,IlADC,NlCO,(Temp(j),j=0,NlORD)
#endif
C	   cfact=(1+avsuc*cosd(theta(iladc)))				! non relativistic
	   cfact=sqrt(1-avsuc*avsuc)/(1-avsuc*cosd(theta(iladc)))	! relativistic
	   cfact=cfact*dispersion
	   if(vsuc.GT.0) cfact=1./cfact
	   do ii=0,NlCo-1
		temp(ii)=temp(ii)*cfact
	   enddo
#if defined( __GFORTRAN__ )
	   write(dyn_format,'(a,i0,a)')'(14x,F10.3,F10.6,',NlORD,'G13.5)'
	   write(6,dyn_format(1:len(dyn_format))) (Temp(j),j=0,NlORD)
	   write(dyn_format,'(a,i0,a)')'(I5,I5,I3,F10.3,F10.6,',nlco,'G14.6)'
	   WRITE(LUNO,dyn_format(1:len(dyn_format))) IlTAP,IlADC,NlCO,(temp(J),J=0,NlCo-1)
#else
	   write(6,'(14x,F10.3,F10.6,<NlORD>G13.5)') (Temp(j),j=0,NlORD)
	   WRITE(LUNO,'(I5,I5,I3,F10.3,F10.6,<nlco>G14.6)') IlTAP,IlADC,NlCO,(temp(J),J=0,NlCo-1)
#endif
	endif
	endif
	goto 20

30	call exit

	END

	subroutine get_detector_angles(th,ph,nn,angfile,lun)

	integer nn,lun
	real th(0:nn-1)
	real ph(0:nn-1)
	character angfile*(*) 

	character*80 line
	integer inlu /-1/
	logical*1 inp_yes

	if(inlu.le.0) call lib$get_lun(INLU)

	langfile=max(1,lengthc(angfile))

6	open(unit=INLU,file=angfile,status='old',READONLY,ERR=7)
	nga=0
	             write(  6,*) 'Reading the angles of the detectors from  '//angfile(1:langfile)
	if(lun.ne.6) write(lun,*) 'Reading the angles of the detectors from  '//angfile(1:langfile)
	goto 8

7	write(6,*) 'Error reading  '//angfile(1:langfile)
	if(inp_yes('Retry')) goto 6
	call exit

8	read(INLU,'(A)',end=10) line 
	lline=lengthc(line)
	if(lline.gt.0) then
	  iriv=index(line,'Riv#')
	  if(iriv.gt.0) then
	    read(line,'(6x,i4)')ii
	    if(ii.ge.0 .AND. ii.lt.nn) then
	      read(line,'(6x,i4,10x,f10.4,12x,f10.4)') ii,th(ii),ph(ii)
	      nga=nga+1
	      write(lun,'(i6,''   Theta='',f10.4,''     Phi='',f10.4)') ii,th(ii),ph(ii)
	    endif
	  endif
	endif
	goto 8

10	close(unit=inlu)
	call lib$free_lun(INLU)
	inlu=-1
	
	if(nga.lt.nn) then
	               write(  6,*) 'W A R N I N G  only',nga,' detector''s angles found' 
	  if(lun.ne.6) write(lun,*) 'W A R N I N G  only',nga,' detector''s angles found' 
	endif

	return

	end
