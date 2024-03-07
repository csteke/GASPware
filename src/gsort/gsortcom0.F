CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       GS0_... definizione      C
C       GS1_... lista            C
C       GS3_... inizializzazione C
C       GS3_... esecuzione       C
C       GS4_... finalizzazione   C
C       GS5_... opzionale        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCC Format e definizioni CCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine gs0_FORMAT

#include "gsort.inc"

	synt(1)='FORMAT  type [i1 i2 ...]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(ntipi.gt.0 .OR. EXISTS(0)) call gs_syntax('Must be given before defining detectors')
	
	if( lengthn(comline) .eq. 0 )call gs_syntax('Must give the format type')
	if( lengthn(comline) .gt. 0 )call gs_syntax('Must give a valid format type string')
	
	jjdmmy = istr_ch2up(comline,comsort)
	
		
	if( comsort(1:5) .eq. 'GASP2' )then
		dataformat = GSPN
		call gs0_F_GASP2001
	
	elseif( comsort(1:4) .eq. 'GASP' )then
		dataformat = GASP

	elseif( comsort(1:8) .eq. 'EUROBALL' )then
		dataformat = EURO
		call gs0_F_EUROBALL
	
	elseif( comsort(1:8) .eq. 'PRISMA' )then
		dataformat = PRISMA
		call gs0_F_PRISMA
	
	elseif( comsort(1:11) .eq. 'GAMMASPHERE' )then
		dataformat = GSPH
		call gs0_F_GAMMASPHERE
	
	elseif( comsort(1:3) .eq. '8PI' )then
		dataformat = PI8
	
	elseif( comsort(1:4) .eq. 'YALE' )then
		dataformat = YALE
		call init_yale_lookup
	
	elseif( comsort(1:6) .eq. 'TANDEM' )then
		dataformat = TNDB
		call gs0_F_TANDEM

	elseif( comsort(1:3) .eq. 'GSR' )then
		dataformat = GSR
	
	elseif( comsort(1:5) .eq. 'GANIL' )then
		dataformat = GANIL
		call gs0_F_GANIL
	
	else
		call gs_syntax('Unknown data format')
	endif
	
	return
	end


	subroutine gs0_F_EUROBALL

#include "gsort.inc"

	euromap(0)=0
	do ii=1,NDTYPE
	  euromap(ii)=-1
	end do
	
	if(lengthn(comline).NE.0) then
	  do ii=1,NDTYPE
	    if(lengthn(comline).gt.0) then
	      jj=istr_i1(comline,euromap(ii))
	      if(euromap(ii).gt.31) call gs_syntax('valid EUROBALL format range is 1-31')
	    elseif(lengthn(comline).lt.0) then
	      jj=istr_ch2up(comline,comsort)
	      if    (comsort(1:3).EQ.'CLU') then
	        euromap(ii)=1
	      elseif(comsort(1:3).EQ.'CLO') then
	        euromap(ii)=2
	      elseif(comsort(1:2).EQ.'TA' ) then
	        euromap(ii)=3
	      elseif(comsort(1:1).EQ.'N'  ) then
	        euromap(ii)=5
	      elseif(comsort(1:11).EQ.'ANCILLARY_V') then
	        euromap(ii)=5
	      elseif(comsort(1:4).EQ.'ISIS'  ) then
	        euromap(ii)=6
	      elseif(comsort(1:11).EQ.'ANCILLARY_F') then
	        euromap(ii)=6
	      elseif(comsort(1:2).EQ.'TR' ) then
	        euromap(ii)=8
	      elseif(comsort(1:6).EQ.'MASTER'  ) then
	        euromap(ii)=7
	      elseif(comsort(1:6) .EQ. 'IB_RAW'  ) then
	        euromap(ii)=9
	      elseif(comsort(1:6) .EQ. 'IB_SUM'  ) then
	        euromap(ii)=10
	      elseif(comsort(1:6) .EQ. 'PULSOR'  ) then
	        euromap(ii)=11
	      elseif(comsort(1:8) .EQ. 'TOTAL_GE'  ) then
	        euromap(ii)=13
	      elseif(comsort(1:4) .EQ. 'EUCL'  ) then
	        euromap(ii)=14
	      elseif(comsort(1:4) .EQ. 'DIAM'  ) then
	        euromap(ii)=15
	      else
	        call gs_syntax('Unrecognized detector identifier')
 	      endif
	    endif
	  end do
	else
	  euromap(1)=1	! CLUSTER
	  euromap(2)=2	! CLOVER
	  euromap(3)=3	! TAPERED
	  euromap(4)=6	! ISIS
	  euromap(5)=5	! Nwall
	  euromap(6)=8	! TRIGGER
	  euromap(7)=9  ! InnerBall RAW
	  euromap(8)=10 ! InnerBall SM
	  euromap(9)=7  ! MasterTrigger - feb. 2000
	  euromap(10)=14 ! EUCLIDES
	  euromap(11)=15 ! DIAMANT or NWall in Strasbourg
	  euromap(12)=13 ! TOTAL_GE
	  euromap(13)=11 ! Pulsor - feb. 2001
	endif
	dataformat=EURO

	return

	end


	subroutine gs0_F_PRISMA

#include "gsort.inc"

*	euromap(0)=0
	do ii=0,NDTYPE
	  euromap(ii)=-1
	end do
	
	if(lengthn(comline).NE.0) then
	  do ii=1,NDTYPE
	    if(lengthn(comline).gt.0) then
	      jj=istr_i1(comline,euromap(ii))
	      if(euromap(ii).gt.31) call gs_syntax('valid PRISMA format range is 1-31')
	    elseif(lengthn(comline).lt.0) then
	      jj=istr_ch2up(comline,comsort)
	      if    (comsort(1:3).EQ.'CLU') then
	        euromap(ii)=1
	      elseif(comsort(1:3).EQ.'CLO') then
	        euromap(ii)=2
	      elseif(comsort(1:2).EQ.'TA' ) then
	        euromap(ii)=3
	      elseif(comsort(1:2).EQ.'DA' ) then
	        euromap(ii)=4
	      elseif(comsort(1:1).EQ.'N'  ) then
	        euromap(ii)=5
	      elseif(comsort(1:11).EQ.'ANCILLARY_V') then
	        euromap(ii)=5
	      elseif(comsort(1:4).EQ.'ISIS'  ) then
	        euromap(ii)=6
	      elseif(comsort(1:11).EQ.'ANCILLARY_F') then
	        euromap(ii)=6
	      elseif(comsort(1:2).EQ.'TR' ) then
	        euromap(ii)=8
	      elseif(comsort(1:6).EQ.'MASTER'  ) then
	        euromap(ii)=7
	      elseif(comsort(1:6) .EQ. 'IB_RAW'  ) then
	        euromap(ii)=9
	      elseif(comsort(1:6) .EQ. 'TITRIS'  ) then
	        euromap(ii)=10
	      elseif(comsort(1:7) .EQ. 'MONITOR'  ) then
	        euromap(ii)=11
	      elseif(comsort(1:3) .EQ. 'MCP'  ) then
	        euromap(ii)=12
	      elseif(comsort(1:4) .EQ. 'PPAC'  ) then
	        euromap(ii)=13
	      elseif(comsort(1:2) .EQ. 'IC' .and. comsort(1:3) .NE. 'ICS') then
	        euromap(ii)=14
	      elseif(comsort(1:6) .EQ. 'ICSIDE'  ) then
	        euromap(ii)=15
	      else
	        call gs_syntax('Unrecognized detector identifier')
 	      endif
	    endif
	  end do
	else
	  euromap(1)=1	! CLUSTER
	  euromap(2)=2	! CLOVER
	  euromap(3)=3	! TAPERED
	  euromap(4)=4	! TAPERED
	  euromap(5)=6	! ISIS
	  euromap(6)=5	! Nwall
	  euromap(7)=8	! TRIGGER
	  euromap(8)=9  ! InnerBall RAW
	  euromap(9)=10 ! InnerBall SM
	  euromap(10)=7  ! MasterTrigger - feb. 2000
	  euromap(11)=14 ! EUCLIDES
	  euromap(12)=15 ! DIAMANT or NWall in Strasbourg
	  euromap(13)=13 ! TOTAL_GE
	  euromap(14)=11 ! Pulsor - feb. 2001
	  euromap(15)=12
	endif
	dataformat=PRISMA

	return

	end



	subroutine gs0_F_GASP2001

#include "gsort.inc"


	do ii=0,NDTYPE
	  euromap(ii)=-1
	  gaspmap(ii)=-1
	end do
	
	if(lengthn(comline).NE.0) then
	  do ii=0,NDTYPE
	    if(lengthn(comline).gt.0) then
	      jj=istr_i1(comline,euromap(ii))
	      if(euromap(ii).gt.14) call gs_syntax('valid GASP2001 format range is 11-14')
	    elseif(lengthn(comline).lt.0) then
	      jj=istr_ch2up(comline,comsort)
	      if    (comsort(1:3).EQ.'HEA' ) then
	        euromap(ii)=11
	      elseif(comsort(1:3).EQ.'GER' ) then
	        euromap(ii)=12
	      elseif(comsort(1:4).EQ.'ISIS') then
	        euromap(ii)=13
	      elseif(comsort(1:1).EQ.'N'   ) then
	        euromap(ii)=14
	      else
	        call gs_syntax('Unrecognized detector identifier')
 	      endif
	    endif
	  end do
	else
	  euromap(0)=11	! HEADER
	  euromap(1)=12	! GERMANIUM
	  euromap(2)=13	! ISIS
	  euromap(3)=14	! N-RING
	endif
	dataformat=GSPN

	return

	end


	subroutine gs0_F_GAMMASPHERE

#include "gsort.inc"


	do ii=0,NDTYPE
	  euromap(ii)=-1
	end do
	euromap(0) = 0
	
	if(lengthn(comline).NE.0) then
	  do ii=1,NDTYPE
	    if(lengthn(comline).gt.0) then
	      jj=istr_i1(comline,euromap(ii))
	      if(euromap(ii).gt.14) call gs_syntax('valid GAMMASPHERE format range is 0-3')
	    elseif(lengthn(comline).lt.0) then
	      jj=istr_ch2up(comline,comsort)

	      if(comsort(1:3).EQ.'GER' ) then
	        euromap(ii)=1
	      elseif(comsort(1:5).EQ.'MICRO') then
	        euromap(ii)=2
	      elseif(comsort(1:6).EQ.'NSHELL'   ) then
	        euromap(ii)=3
	      else
	        call gs_syntax('Unrecognized detector identifier')
 	      endif

	    endif
	  end do
	else
	  euromap(0)=0	! HEADER
	  euromap(1)=1	! GERMANIUM
	  euromap(2)=2	! MICROBALL
	  euromap(3)=3	! NSHELL
	endif
	dataformat=GSPH

	return

	end


	subroutine gs0_F_TANDEM
	
#include "gsort.inc"

	if( lengthn(comline) .gt. 0 )then
		call gs_syntax('Invalid argument, no arg. or PRESORTED accepted')

	elseif(  lengthn(comline) .lt. 0 ) then
		jj=istr_ch2up(comline,comsort)
		if(comsort(1:9).EQ.'PRESORTED' ) then
			tnd.presorted = .TRUE.
		else
			call gs_syntax('Invalid argument, no arg. or PRESORTED accepted')
		endif

	else
		tnd.presorted = .FALSE.
	endif
	
	dataformat = TNDB
	
	return
	end		
	
	subroutine gs0_F_GANIL

#include "gsort.inc"
         
	integer ganil_init
	external ganil_init 

	do ii=0,NDTYPE
	  euromap(ii)=-1
	end do
	
	if(lengthn(comline).NE.0) then
	  do ii=1,NDTYPE
	    if(lengthn(comline).gt.0) then
	      jj=istr_i1(comline,euromap(ii))
	      if(euromap(ii).gt.31) call gs_syntax('valid PRISMA format range is 1-31')
	    elseif(lengthn(comline).lt.0) then
	      jj=istr_ch2up(comline,comsort)
	      if    (comsort(1:4).EQ.'TRIG') then
	        euromap(ii)=1
	      elseif(comsort(1:4).EQ.'CLOV') then
	        euromap(ii)=2
	      elseif(comsort(1:4).EQ.'SEGM') then
	        euromap(ii)=3
	      elseif(comsort(1:4).EQ.'DIAM') then
	        euromap(ii)=4
	      elseif(comsort(1:4).EQ.'NWAL') then
	        euromap(ii)=5
	      elseif(comsort(1:4).EQ.'NTAC') then
	        euromap(ii)=6
	      else
	        call gs_syntax('Unrecognized detector identifier')
 	      endif
	    endif
	  end do
	else
	  euromap(1)=1	! TRIGGER
	  euromap(2)=2	! CLOVER
	  euromap(3)=3	! SEGMENTS
	  euromap(4)=4	! DIAMANT
	  euromap(5)=5	! NWALL
	  euromap(6)=6	! NTAC
	endif
	dataformat=GANIL
	
	if( ganil_init() .ne. 1 )then
	   write(*,*)' ERROR : cannot allocate internal buffers in GANIL_INIT'
	   call exit(0)
	endif

	return
	end
***************************************************************************************************************
	subroutine gs0_GASP

#include "gsort.inc"

	synt(1)='GASP'
	 
	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(ntipi.gt.0 .OR. EXISTS(0)) call gs_syntax('Must be given before defining detectors')
	
	dataformat=GASP

	return

	end

	
	subroutine gs0_EUROBALL

#include "gsort.inc"

	synt(1)='EUROBALL  [i1 i2 ... ]'
	 
	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(ntipi.gt.0 .OR. EXISTS(0)) call gs_syntax('Must be given before defining detectors')

	euromap(0)=0
	do ii=1,NDTYPE
	  euromap(ii)=-1
	end do
	
	if(lengthn(comline).NE.0) then
	  do ii=1,NDTYPE
	    if(lengthn(comline).gt.0) then
	      jj=istr_i1(comline,euromap(ii))
	      if(euromap(ii).gt.31) call gs_syntax('valid EUROBALL format range is 1-31')
	    elseif(lengthn(comline).lt.0) then
	      jj=istr_ch2up(comline,comsort)
	      if    (comsort(1:3).EQ.'CLU') then
	        euromap(ii)=1
	      elseif(comsort(1:3).EQ.'CLO') then
	        euromap(ii)=2
	      elseif(comsort(1:2).EQ.'TA' ) then
	        euromap(ii)=3
	      elseif(comsort(1:1).EQ.'N'  ) then
	        euromap(ii)=5
	      elseif(comsort(1:11).EQ.'ANCILLARY_V') then
	        euromap(ii)=5
	      elseif(comsort(1:4).EQ.'ISIS'  ) then
	        euromap(ii)=6
	      elseif(comsort(1:11).EQ.'ANCILLARY_F') then
	        euromap(ii)=6
	      elseif(comsort(1:2).EQ.'TR' ) then
	        euromap(ii)=8
	      elseif(comsort(1:6).EQ.'MASTER'  ) then
	        euromap(ii)=7
	      elseif(comsort(1:6) .EQ. 'IB_RAW'  ) then
	        euromap(ii)=9
	      elseif(comsort(1:6) .EQ. 'IB_SUM'  ) then
	        euromap(ii)=10
	      elseif(comsort(1:6) .EQ. 'PULSOR'  ) then
	        euromap(ii)=11
	      elseif(comsort(1:8) .EQ. 'TOTAL_GE'  ) then
	        euromap(ii)=13
	      elseif(comsort(1:4) .EQ. 'EUCL'  ) then
	        euromap(ii)=14
	      elseif(comsort(1:4) .EQ. 'DIAM'  ) then
	        euromap(ii)=15
	      else
	        call gs_syntax('Unrecognized detector identifier')
 	      endif
	    endif
	  end do
	else
	  euromap(1)=1	! CLUSTER
	  euromap(2)=2	! CLOVER
	  euromap(3)=3	! TAPERED
	  euromap(4)=6	! ISIS
	  euromap(5)=5	! Nwall
	  euromap(6)=8	! TRIGGER
	  euromap(7)=9  ! InnerBall RAW
	  euromap(8)=10 ! InnerBall SM
	  euromap(9)=7  ! MasterTrigger - feb. 2000
	  euromap(10)=14 ! EUCLIDES
	  euromap(11)=15 ! DIAMANT or NWall in Strasbourg
	  euromap(12)=13 ! TOTAL_GE
	  euromap(13)=11 ! Pulsor - feb. 2001
	endif
	dataformat=EURO

	return

	end


	subroutine gs0_8PI

#include "gsort.inc"

	synt(1)='8PI'
	 
	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(ntipi.gt.0 .OR. EXISTS(0)) call gs_syntax('Must be given before defining detectors')

	dataformat=PI8

	return

	end

	subroutine gs0_GAMMASPHERE

#include "gsort.inc"

	synt(1)='GAMMASPHERE'
	 
	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(ntipi.gt.0 .OR. EXISTS(0)) call gs_syntax('Must be given before defining detectors')

	dataformat=GSPH

	return

	end


	subroutine gs0_YALE

#include "gsort.inc"

	synt(1)='YALE'
	 
	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(ntipi.gt.0 .OR. EXISTS(0)) call gs_syntax('Must be given before defining detectors')
	
	dataformat=YALE
	call init_yale_lookup

	return

	end



	subroutine gs0_GASP2001

#include "gsort.inc"

	synt(1)='GASP2001  [i1 i2 ... ]'
	 
	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(ntipi.gt.0 .OR. EXISTS(0)) call gs_syntax('Must be given before defining detectors')

	do ii=0,NDTYPE
	  euromap(ii)=-1
	  gaspmap(ii)=-1
	end do
	
	if(lengthn(comline).NE.0) then
	  do ii=0,NDTYPE
	    if(lengthn(comline).gt.0) then
	      jj=istr_i1(comline,euromap(ii))
	      if(euromap(ii).gt.14) call gs_syntax('valid GASP2001 format range is 11-14')
	    elseif(lengthn(comline).lt.0) then
	      jj=istr_ch2up(comline,comsort)
	      if    (comsort(1:3).EQ.'HEA' ) then
	        euromap(ii)=11
	      elseif(comsort(1:3).EQ.'GER' ) then
	        euromap(ii)=12
	      elseif(comsort(1:4).EQ.'ISIS') then
	        euromap(ii)=13
	      elseif(comsort(1:1).EQ.'N'   ) then
	        euromap(ii)=14
	      else
	        call gs_syntax('Unrecognized detector identifier')
 	      endif
	    endif
	  end do
	else
	  euromap(0)=11	! HEADER
	  euromap(1)=12	! GERMANIUM
	  euromap(2)=13	! ISIS
	  euromap(3)=14	! N-RING
	endif
	dataformat=GSPN

	return

	end

	subroutine gs0_header

#include "gsort.inc"

	synt(1)='HEADER ch #Pars Resolution{#Pars} [PLUS #Pars+ Resolution{#Pars+}]'
	 
	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(EXISTS(0)) call gs_syntax('HEADER already defined')

	ii=lengthn(comline)
	if(ii.ne.-1) call gs_syntax('A single character for the name expected here')
	jjdmmy = istr_ch2up(comline,comsort)
	if(comsort(1:1).eq.'H') call gs_syntax('Identifier H is reserved for HGATE')
	if(comsort(1:1).eq.'P') call gs_syntax('Identifier P is reserved for PAIRS')
	do ii=0,ntipi-1
	  if(comsort(1:1).eq.dname(ii)) call gs_syntax('Name must be unique')
	end do
	dname(0)=comsort(1:1)

	ii=lengthn(comline)
	if(ii.gt.0) then
	  ii=istr_i1(comline,NDPAR(0))
	  if(ii.ne.1) call gs_syntax('Number of HEADER parameters expected')
	  TDPAR(0)=NDPAR(0)
	  if(TDPAR(0).gt.MAXPAR) call gs_syntax1('Maximum number of HEADER parameters is',MAXPAR)
	  ii=istr_ia(comline,PARRES(0,0),NDPAR(0))
	  if(ii.ne.NDPAR(0)) call gs_syntax('Wrong number of resolution parameters')
	elseif(ii.eq.0) then
	  call gs_syntax('Missing input')
	endif
	ii=lengthn(comline)
	if(ii.gt.0) then
	  call gs_syntax('PLUS expected here')
	elseif(ii.lt.0) then
	  jjdmmy = istr_ch2up(comline,comsort)
	  lcomsort=lengthc(comsort)
	  if(comsort(1:lcomsort).ne.'PLUS') call gs_syntax('PLUS expected here')
	  ii=istr_i1(comline,MDPAR(0))
	  if(ii.ne.1) call gs_syntax('Missing command parameters')
	  TDPAR(0)=NDPAR(0)+MDPAR(0)
	  if(TDPAR(0).gt.MAXPAR) call gs_syntax1('Maximum number of HEADER parameters is',MAXPAR)
	  ii=istr_ia(comline,PARRES(NDPAR(0),0),MDPAR(0))
	  if(ii.ne.MDPAR(0)) call gs_syntax('Wrong number of resolution parameters')
	endif

	NDETS(0)=1
	NSEGS(0)=1
	NITEMS(0)=1
	gaspmap(0)=euromap(ntipi)
	EXISTS(0)=.true.

	return

	end

	subroutine gs0_detector

#include "gsort.inc"

	synt(1)='DETECTOR ch #detectors #Pars Resolution{#pars} [PLUS #Pars+ Resolution{#Pars+}]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(ntipi.ge.NDTYPE) call gs_syntax1('Max. number of detector types is',NDTYPE)

	ii=lengthn(comline)
	if(ii.ne.-1) call gs_syntax('A single character for the name expected here')
	jjdmmy = istr_ch2up(comline,comsort)
	if(comsort(1:1).eq.'H') call gs_syntax('Identifier H is reserved for HGATE')
	if(comsort(1:1).eq.'P') call gs_syntax('Identifier P is reserved for PAIRS')
	do ii=0,ntipi
	  if(comsort(1:1).eq.dname(ii)) call gs_syntax('Name must be unique')
	end do
	ntipi=ntipi+1
	dname(ntipi)=comsort(1:1)

	ii=istr_i1(comline,NDETS(ntipi))
	if(ii.ne.1) call gs_syntax('Number of detectors expected')
	if(NDETS(ntipi).gt.MAXDET) call gs_syntax1('Max. number of detectors is',MAXDET)
	NSEGS(ntipi)=ISEG				! non e' un rivelatore composito
	NITEMS(ntipi)=NDETS(ntipi)*NSEGS(ntipi)
	if(NITEMS(ntipi).gt.MAXDET) call gs_syntax1('Max. number of detectors is',MAXDET)

	ii=lengthn(comline)
	if(ii.gt.0) then
	  ii=istr_i1(comline,NDPAR(ntipi))
	  if(ii.ne.1) call gs_syntax('Number of DETECTOR parameters expected')
	  TDPAR(ntipi)=NDPAR(ntipi)
	  if(TDPAR(ntipi).gt.MAXPAR) call gs_syntax1('Maximum number of DETECTOR parameters is',MAXPAR)
	  ii=istr_ia(comline,PARRES(0,ntipi),NDPAR(ntipi))
	  if(ii.ne.NDPAR(ntipi)) call gs_syntax('Wrong number of resolution parameters')
	elseif(ii.eq.0) then
	  call gs_syntax('Missing input')
	endif
	ii=lengthn(comline)
	if(ii.gt.0) then
	  call gs_syntax('PLUS expected here')
	elseif(ii.lt.0) then
	  jjdmmy = istr_ch2up(comline,comsort)
	  lcomsort=lengthc(comsort)
	  if(comsort(1:lcomsort).ne.'PLUS') call gs_syntax('PLUS expected here')
	  ii=istr_i1(comline,MDPAR(ntipi))
	  if(ii.ne.1) call gs_syntax('Missing command parameters')
	  TDPAR(ntipi)=NDPAR(ntipi)+MDPAR(ntipi)
	  if(TDPAR(ntipi).gt.MAXPAR) call gs_syntax1('Maximum number of DETECTOR parameters is',MAXPAR)
	  ii=istr_ia(comline,PARRES(NDPAR(ntipi),ntipi),MDPAR(ntipi))
	  if(ii.ne.MDPAR(ntipi)) call gs_syntax('Wrong number of resolution parameters')
	endif

	if((dataformat.eq.EURO .or. dataformat.eq.PRISMA) .and. NDPAR(ntipi).gt.0) then
	  if(euromap(ntipi).lt.0) call gs_syntax('Group number not been defined for this detector')
	  if(euromap(ntipi).eq.1 .AND. NSEGS(ntipi).ne.CSEG) call gs_syntax('Supposed to be a Cluster') 
	  if(euromap(ntipi).eq.2 .AND. NSEGS(ntipi).ne.QSEG) call gs_syntax('Supposed to be a Clover') 
	  if(euromap(ntipi).eq.3 .AND. NSEGS(ntipi).ne.TSEG) call gs_syntax('Supposed to be a Tapered') 
C	  if(euromap(ntipi).eq.4 .AND. NSEGS(ntipi).ne.CSEG) call gs_syntax('Supposed to be a Cluster-full') 
	endif

	EXISTS(ntipi)=.true.

	if( EXISTS(0) )then
	   gaspmap(ntipi) = euromap(ntipi)
	else
	   gaspmap(ntipi) = euromap(ntipi-1)
	endif

	return

	end

	subroutine gs0_cdetector

#include "gsort.inc"
	
	integer N_COMM_P

	synt(1)='CDETECTOR ch #detectors [#CommonPars Res{#CommonPars}] SEG #Segments #Pars Res{#Pars} [PLUS #Pars+ Res{#Pars+}]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

C	if(dataformat .NE. EURO) call gs_syntax('Composite detectors only for EUROBALL style data') 

	if(ntipi.ge.NDTYPE) call gs_syntax1('Max. number of detector types is',NDTYPE)

	ii=lengthn(comline)
	if(ii.ne.-1) call gs_syntax('A single character for the name expected here')
	jjdmmy = istr_ch2up(comline,comsort)
	if(comsort(1:1).eq.'H') call gs_syntax('Identifier H is reserved for HGATE')
	if(comsort(1:1).eq.'P') call gs_syntax('Identifier P is reserved for PAIRS')
	do ii=0,ntipi
	  if(comsort(1:1).eq.dname(ii)) call gs_syntax('Name must be unique')
	end do
	ntipi=ntipi+1
	dname(ntipi)=comsort(1:1)

	ii=istr_i1(comline,NDETS(ntipi))
	if(ii.ne.1) call gs_syntax('Number of detectors expected')
	if(NDETS(ntipi).gt.MAXDET) call gs_syntax1('Max. number of detectors is',MAXDET)
		
	N_COMM_P = 0
	ii = istr_i1( comline, N_COMM_P )
	
	if( ii .eq. 1 ) then
		ii=istr_ia(comline,PARRES(0,ntipi),N_COMM_P)
		if(ii.ne.N_COMM_P) call gs_syntax('Wrong number of resolution parameters')
	endif
	jjdmmy = istr_ch2up(comline,comsort)
	lcomsort = lengthc( comsort )
	if(comsort(1:lcomsort).ne.'SEG') call gs_syntax('SEG expected here')
	
	ii=istr_i1(comline,NSEGS(ntipi))
	if(ii.ne.1) call gs_syntax('Number of segments expected')
	NITEMS(ntipi)=NDETS(ntipi)*NSEGS(ntipi)
	if(NITEMS(ntipi).gt.MAXDET) call gs_syntax1('Max. number of detectors*segments is',MAXDET)

	ii=lengthn(comline)
	if(ii.gt.0) then
	  ii=istr_i1(comline,NDPAR(ntipi))
	  if(ii.ne.1) call gs_syntax('Number of DETECTOR parameters expected')
	  TDPAR(ntipi)=NDPAR(ntipi)
	  if(TDPAR(ntipi).gt.MAXPAR-N_COMM_P) call gs_syntax1('Maximum number of DETECTOR parameters is',MAXPAR)
	  ii=istr_ia(comline,PARRES(N_COMM_P,ntipi),NDPAR(ntipi))
	  if(ii.ne.NDPAR(ntipi)) call gs_syntax('Wrong number of resolution parameters')
	elseif(ii.eq.0) then
	  call gs_syntax('Missing input')
	endif
	
	NDPAR( ntipi ) = NDPAR( ntipi ) + N_COMM_P
	NCPAR( ntipi ) = N_COMM_P
	TDPAR(ntipi)=NDPAR(ntipi)
	
	ii=lengthn(comline)
	if(ii.gt.0) then
	  call gs_syntax('PLUS expected here')
	elseif(ii.lt.0) then
	  jjdmmy = istr_ch2up(comline,comsort)
	  lcomsort=lengthc(comsort)
	  if(comsort(1:lcomsort).ne.'PLUS') call gs_syntax('PLUS expected here')
	  ii=istr_i1(comline,MDPAR(ntipi))
	  if(ii.ne.1) call gs_syntax('Missing command parameters')
	  TDPAR(ntipi)=NDPAR(ntipi)+MDPAR(ntipi)
	  if(TDPAR(ntipi).gt.MAXPAR) call gs_syntax1('Maximum number of DETECTOR parameters is',MAXPAR)
	  ii=istr_ia(comline,PARRES(NDPAR(ntipi),ntipi),MDPAR(ntipi))
	  if(ii.ne.MDPAR(ntipi)) call gs_syntax('Wrong number of resolution parameters')
	endif
	
	if(dataformat.eq.EURO .and. NDPAR(ntipi).gt.0) then
	  if(euromap(ntipi).le.0) call gs_syntax('Group number not been defined for this detector')
	  if(euromap(ntipi).eq.1 .AND. NSEGS(ntipi).ne.CSEG) call gs_syntax('Supposed to be a Cluster') 
	  if(euromap(ntipi).eq.2 .AND. NSEGS(ntipi).ne.QSEG) call gs_syntax('Supposed to be a Clover') 
	  if(euromap(ntipi).eq.3 .AND. NSEGS(ntipi).ne.TSEG) call gs_syntax('Supposed to be a Tapered') 
C	  if(euromap(ntipi).eq.4 .AND. NSEGS(ntipi).ne.CSEG) call gs_syntax('Supposed to be a Cluster-full') 
	endif
	
	EXISTS(ntipi)=.true.

	return

	end

	subroutine gs0_rawfoldmin

#include "gsort.inc"

	synt(1)='RAWFOLDMIN  foldmin {For every defined detector}'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	iin=istr_ia(comline,FOLDMIN(1),NTIPI)

	if(iin.ne.NTIPI) call gs_syntax('Wrong number of parameters')

	return

	end

	subroutine gs1_rawfoldmin

#include "gsort.inc"

	write(LLUN,*)
	do ji=1,NTIPI
	  if(foldmin(ji).ne.0) then
	  write(LLUN,'('' Minimum accepted fold for detector  '',A,i3)') DNAME(ji)//'   is',foldmin(ji)
	  endif
	end do

	return

	end



	subroutine gs0_hgatedef

#include "gsort.inc"

c	record/hashdat/dat
c	pointer (hashpnt,dat)

	integer hind,hpar,hres,hlen,hngates
	logical*1hfromfile,hsameforall
	real*4    dist2(0:MAXRES)
	character*72 hfile

	logical*1getmem
	
	synt(1)='HGATEDEF Pn #gates'
	synt(2)='         Wl Wh {#gates lines}'
	synt(3)='       or'
	synt(4)='HGATEDEF Pn file_with_gates  [Individual|Same_for_all]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(hashgiven.ge.MAXHASH) call gs_syntax1('Max. number of HGATEDEF is',MAXHASH)
	call gs_getindpar(HIND,HPAR,HRES)
	if(.not.EXISTS(HIND)) call gs_syntax('Detector for the HGATE is not defined')
	if(HPAR.lt.0 .or. HPAR .ge. TDPAR(HIND)) call gs_syntax('Non existing parameter')
	iin=lengthn(comline)
	if(iin.gt.0) then
	  jjdmmy = istr_i1(comline,hngates)
	  IF(hngates.LT.1) call gs_syntax('Wrong number of gates')
	  hsameforall=.true.
	  hfromfile=.false.
	  iin=lengthn(comline)
	  do ii=0,MAXRES-1
	    dist2(ii)=-1.0
	  end do
	  do ii=1,hngates
	    call gs_readline
	    jj=istr_i2(comline,l1,l2)
	    if(jj.ne.2) call gs_syntax('Wrong number of parameters')
	    if(l1.lt.0 .or.l1.ge.MAXRES) call gs_syntax('Illegal gate value')
	    if(l2.lt.0 .or.l2.ge.MAXRES) call gs_syntax('Illegal gate value')
	    if(l1.gt.l2) call gs_syntax('Illegal gate values')
	    do ll=l1,l2
	      dist2(ll)=1.0
	    end do
	    write(LU1,'(4i8)') l1,l2
	  end do
	elseif(iin.lt.0) then
	  hfromfile=.true.
	  call istr_ch(comline,hfile)
	  call filetype(hfile,'hash')
	  hsameforall=.true.
	  iin=lengthn(comline)
	  do while(iin.lt.0)
	    jjdmmy = istr_ch2up(comline,comsort)
	    if(comsort(1:2).EQ.'I') then
	      hsameforall=.false.
	    elseif(comsort(1:2).EQ.'S') then
	      hsameforall=.true.
	    else
	      call gs_syntax('INDIVIDUAL/SAME_FOR_ALL expected')
	    endif
	    iin=lengthn(comline)
	  end do
	else
	  call gs_syntax('Missing parameters')
	endif

	hlen=6 + MAXDET + 72/4
	if(hsameforall) then
	  hlen=hlen + HRES
	else
	  hlen=hlen + HRES * NITEMS(HIND)
	endif 
	
	if(.not.getmem(4*hlen,hashaddr(hashgiven))) Stop 'Error allocating memory for HGATE'

	call gs5_hgatedef(%val(hashaddr(hashgiven)),hind,hpar,hres,hsameforall,hfromfile,hfile,dist2,hngates)

c	hashpnt=hashaddr(hashgiven)
c	dat.ind=hind
c	dat.par=hpar
c	dat.res=hres
c	dat.which=hashgiven
c	dat.sameforall=hsameforall
c	dat.fromfile=hfromfile
c	if(hfromfile) then
c	  dat.file=hfile
c	else
c	  call gs_setdist2(dist2,dat.dist2,hres)
c	  dat.ngates(0)=hngates
c	endif

	hashgiven=hashgiven+1
	
	return

	end

	subroutine gs1_hgatedef(dat)

#include "gsort.inc"
	
	record/hashdat/dat
#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif

	list=' '
	lgh=1
	if(dat.which .GE. 10) lgh=2
#if defined( __GFORTRAN__ )
	write(dyn_format,'(i4)')lgh
	dyn_format = adjustl(dyn_format)
	write(list,dyn_format)' HGATE   H',dat.which,'  on'
#else
	write(list,4201)' HGATE   H',dat.which,'  on'
4201	format(A,i<lgh>,A)
#endif
	llist=lengthc(list)+3
	call gs_putindpar(dat.ind,dat.par)

	if(dat.fromfile) then
	  lhfile=lengthc(dat.file)
	  if(dat.sameforall) then
	    write(list(llist:),'(i5,A)') dat.ngates(0),'    values taken from  '//dat.file(1:lhfile)
	  else
	    write(list(llist:),'(5x,A)') '    values taken from  '//dat.file(1:lhfile)
	    call gs_writeline(LLUN,list,INDENT)
	    write(LLUN,*) ' Number of gates for each detector is:'
	    do ii=0,NITEMS(dat.ind)-1,8
	      write(LLUN,'(8i8)') (dat.ngates(jj),jj=ii,min(NITEMS(dat.ind)-1,ii+7))
	    end do
	  endif
	else
	  write(list(llist:),'(i5,A)') dat.ngates(0),'  value(s) defined'
	  call gs_writeline(LLUN,list,INDENT)
	endif

	return

	end

	subroutine gs2_hgatedef(dat)

#include "gsort.inc"
	
	record/hashdat/dat
#if defined( __GFORTRAN__ )
	character*64 dyn_format
	logical*1 inp_yes
	external inp_yes
#endif

	if(init_mode.ne.0) return

	if(.NOT.dat.fromfile) return

	lfile=lengthc(dat.file)
	lgp=1
	if(dat.which .GE. 10) lgp=2
#if defined( __GFORTRAN__ )
	write(dyn_format,'(i4)')lgp
	dyn_format = adjustl(dyn_format)
	WRITE(LU1,*)
	WRITE(LU1,dyn_format) ' Reading HGATE data for H',dat.which,'  from '//dat.file(1:lfile)
	WRITE(LU2,*)
	WRITE(LU2,dyn_format) ' HGATE data for H',dat.which,' taken from '//dat.file(1:lfile)
#else
4201	format(A,I<lgp>,A)
	WRITE(LU1,*)
	WRITE(LU1,4201) ' Reading HGATE data for H',dat.which,'  from '//dat.file(1:lfile)
	WRITE(LU2,*)
	WRITE(LU2,4201) ' HGATE data for H',dat.which,' taken from '//dat.file(1:lfile)
#endif
10	open(unit=gs2lun,file=dat.file,status='old',readonly,err=120)
	if(dat.sameforall) then
	  call gs_findrunadc(gs2lun,-1,-1,iok)
	  if(iok.gt.0) then
	    do ll=0,dat.res-1
	      dat.dist2(ll)=-1.0
	    end do
	    nval=0
20	    read(gs2lun,*,err=25,end=25) l1,l2
	    if(l1.LT.0 .OR. l1.GE.dat.res) goto 140
	    if(l2.LT.0 .or. l2.GE.dat.res) goto 140
	    if(l1.GT.l2) goto 140
	    nval=nval+1
	    do ll=l1,l2
	     dat.dist2(ll)=1.0
	    end do
	    goto 20
	  endif
25	  if(nval.eq.0) goto 130
	  dat.ngates(0)=nval
	  WRITE(LU1,'(I6,A)') nval,'  HGATEs found'
	  WRITE(LU2,'(I6,A)') nval,'  HGATEs found'
	  call gs_setdist2(dat.dist2,dat.dist2,dat.res)
	else
	  do ii=0,dat.res*NITEMS(dat.ind)-1
	    dat.dist2(ii)=-1.0
	  end do
	  nntot=0
	  do ii=0,NITEMS(dat.ind)-1
	    rewind(gs2lun)
	    call gs_findrunadc(gs2lun,-1,ii,iok)
	    nval=0
	    if(iok.gt.0) then
30	      read(gs2lun,*,err=35,end=35) l1,l2
	      if(l1.LT.0 .or. l1.GE.dat.res) goto 140
	      if(l2.LT.0 .or. l2.GE.dat.res) goto 140
	      if(l1.GT.l2) goto 140
	      nval=nval+1
	      do ll=dat.res*ii+l1,dat.res*ii+l2
	        dat.dist2(ll)=1.0
	      end do
	      goto 30
	    endif
35	    dat.ngates(ii)=nval
	    WRITE(LU1,'(I6,A,I5)') nval,' HGATEs found for detector',ii
	    WRITE(LU2,'(I6,A,I5)') nval,' HGATEs found for detector',ii
	    nntot=nntot+nval
	    call gs_setdist2(dat.dist2(dat.res*ii),dat.dist2(dat.res*ii),dat.res)
	  end do
	  if(nntot.eq.0) goto 130
	endif
	close(gs2lun)
	return
	
120	write(6,*) 'Error opening   ',dat.file(1:lfile)
	goto 200

130	write(6,*) 'No data found in  ',dat.file(1:lfile)
	close(gs2lun)
	goto 200

140	write(6,*) 'Illegal HGATE value',l1,l2
	close(gs2lun)
	goto 200

200	if(inp_yes('Retry')) goto 10
	stop 'Error reading HGATE data'

	end

	subroutine gs3_hgate(dat)

#include "gsort.inc"

	integer hphase
	record/hashdat/dat

	integer ind,par,res

	ind=dat.ind
	par=dat.par
	res=dat.res

	if(dat.sameforall) then
	  hntrue=0
	  do jx=DOFF(IND),DOFF(IND)+NDET(IND)-1
	    hltrue(jx)=.FALSE.
	    idat=det(jx).ival(par)
	    if(idat.ge.0 .and. idat.lt.res) then
	      dd=dat.dist2(idat)
	      if(dd .GE. 0.0) then
	        hdist1(jx)=dd
	        hltrue(jx)=.TRUE.
	        hntrue=hntrue+1
	      endif
	    endif
	  end do
	else
	  do jx=DOFF(IND),DOFF(IND)+NDET(IND)-1
	    hltrue(jx)=.false.
	    idat=det(jx).ival(par)
	    if(idat.ge.0 .and. idat.lt.res) then
	      id=det(jx).id
	      dd=dat.dist2(idat+res*id)
	      if(dd .GE. 0.0) then
	        hdist1(jx)=dd
	        hltrue(jx)=.true.
	        hntrue=hntrue+1
	      endif
	    endif
	  end do
	endif

	return
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!! fidandosi di hltrue e hdist1 settati con gs3_hgate(dat) !!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	entry gs3_hgate_sph(dat)

	ind=dat.ind
	par=dat.par
	res=dat.res

	mm=0
	rr2=0.0
	do jx=DOFF(ind),DOFF(ind)+NDET(ind)-1
	  if(hltrue(jx)) then
	    hdist2(mm)=hdist1(jx)
	    rr2=rr2+hdist2(mm)
	    mm=mm+1
	  endif
	end do
	if(rr2.LE.1.0) then
	  hstrue=mm
	  return
	endif

	do ii=1,mm-1				! ordina hdist2 con straight insert method
	  dd=hdist2(ii)
	  ll=0					! cerca il punto di inserzione
	  lr=ii-1
	  do while(ll.LE.lr)
	    lm=(ll+lr)/2
	    if(dd.GE.hdist2(lm)) then
	      ll=lm+1
	    else
	      lr=lm-1
	    endif
	  end do
	  if(ll.ne.ii) then
	    do jj=ii,ll+1,-1			! shift
	      hdist2(jj)=hdist2(jj-1)
	    end do
	    hdist2(ll)=dd			! insert
	  endif
	end do

	do ii=mm-1,0,-1
	  rr2=rr2-hdist2(ii)
	  if(rr2.LE.1.0) then
	    hstrue=ii
	    return
	  endif
	end do
	hstrue=0

	return

	end


	subroutine gs5_hgatedef(dat,hind,hpar,hres,hsameforall,hfromfile,hfile,dist2,hngates)

#include "gsort.inc"

	record/hashdat/dat
c	pointer (hashpnt,dat)

	integer hind,hpar,hres,hlen,hngates
	logical*1hfromfile,hsameforall
	real*4    dist2(0:MAXRES)
	character*72 hfile

c	hashpnt=hashaddr(hashgiven)
	dat.ind=hind
	dat.par=hpar
	dat.res=hres
	dat.which=hashgiven
	dat.sameforall=hsameforall
	dat.fromfile=hfromfile
	if(hfromfile) then
	  dat.file=hfile
	else
	  call gs_setdist2(dist2,dat.dist2,hres)
	  dat.ngates(0)=hngates
	endif

	return

	end

	subroutine gs0_pairdef

#include "gsort.inc"

c	record/pairsdat/dat
c	pointer (pairspnt,dat)

	character*72 pfile
	
	logical*1 getmem

	synt(1)='PAIRDEF file_with_index_of_pairs'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(pairsgiven.ge.MAXPAIRS) call gs_syntax1('Max. number of pairs is',MAXPAIRS)
	call istr_ch(comline,pfile)
	if(lengthc(pfile).lt.1) call gs_syntax('Filename expected')

	ibsize=4 + 72 + 4*MAXDET*MAXDET
	if(.not.getmem(ibsize,pairsaddr(pairsgiven))) Stop 'Error allocating memory for PAIRS'

	call gs5_pairdef(%val(pairsaddr(pairsgiven)),pfile)

c	pairspnt=pairsaddr(pairsgiven)
c	dat.file=pfile

	pairsgiven=pairsgiven+1

	return

	end

	subroutine gs1_pairdef(nn,dat)

#include "gsort.inc"

	integer nn
	record/pairsdat/dat
#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif

	write(LLUN,*)
	lgp=1
	if(nn.ge.10) lgp=2
	lfile=lengthc(dat.file)
#if defined( __GFORTRAN__ )
	write(dyn_format,'(i4)')lgp
	dyn_format = adjustl(dyn_format)
	write(LLUN,dyn_format) ' PAIRS  (P',nn,')    taken from   '//dat.file(1:lfile)
#else
4201	format(A,I<lgp>,A)
	write(LLUN,4201) ' PAIRS  (P',nn,')    taken from   '//dat.file(1:lfile)
#endif
	return

	end

	subroutine gs2_pairdef(nn,dat)

#include "gsort.inc"

	integer nn
	record/pairsdat/dat
#if defined( __GFORTRAN__ )
	character*64 dyn_format
	logical*1 inp_yes
	external inp_yes
#endif

	if(init_mode.ne.0) return

	lfile=lengthc(dat.file)
	lgp=1
	if(nn.ge.10) lgp=2

#if defined( __GFORTRAN__ )
	write(dyn_format,'(i4)')lgp
	dyn_format = adjustl(dyn_format)
	WRITE(LU1,*)
	WRITE(LU1,'(A,I'//dyn_format//',A)') ' Reading PAIRS data for P',nn,'  from '//dat.file(1:lfile)
	WRITE(LU2,*)
	WRITE(LU2,'(A,I'//dyn_format//',A)') ' PAIRS data for P',nn,' taken from '//dat.file(1:lfile)
#else
4201	format(A,I<lgp>,A)
	WRITE(LU1,*)
	WRITE(LU1,4201) ' Reading PAIRS data for P',nn,'  from '//dat.file(1:lfile)
	WRITE(LU2,*)
	WRITE(LU2,4201) ' PAIRS data for P',nn,' taken from '//dat.file(1:lfile)
#endif

10	open(unit=gs2lun,file=dat.file,status='old',readonly,err=120)
	do i2=0,MAXDET-1
	do i1=0,MAXDET-1
	  dat.pind(i1,i2)=-1
	end do
	end do

	dat.indmax=0
	nval=0
20	read(gs2lun,*,err=130,end=100) i1,i2,ind
	if(i1.ge.0 .and. i1.lt.MAXDET .and.
	1  i2.ge.0 .and. i2.lt.MAXDET .and. ind.ge.0) then
	  dat.pind(i1,i2)=ind
	  dat.indmax=max(dat.indmax,ind)
	  write(LU2,'(1x,3i4)') i1,i2,ind
	  nval=nval+1
	else
	  goto 140
	endif
	goto 20

100	write(LU1,'(A,i4,A,i6,A)') ' Index max is',dat.indmax,'   out of',nval,'  values given'
	write(LU2,'(A,i4,A,i6,A)') ' Index max is',dat.indmax,'   out of',nval,'  values given'
	return

120	write(6,*) 'Error opening   ',dat.file(1:lfile)
	goto 200

130	write(6,*) 'Error reading   ',dat.file(1:lfile)
	goto 200

140	write(6,*) 'Illegal PAIRDEF value',i1,i2,ind
	goto 200

200	if(inp_yes('Retry')) goto 10
	stop 'Error reading PAIRDEF data'

	end

	subroutine gs5_pairdef(dat,pfile)

#include "gsort.inc"

	record/pairsdat/dat
c	pointer (pairspnt,dat)

	character*72 pfile
c	pairspnt=pairsaddr(pairsgiven)
	dat.file=pfile

	return

	end
