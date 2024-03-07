	subroutine gs_define_istream(nfiles,ncassette)

#include "gsort.inc"
	integer tnd_getnf
	external tnd_getnf
       integer fileopenro
       external fileopenro

	character iodev*1	/'D'/

	break=.true.

	if(dataformat .eq. tndb)IODEV='D'

	write(6,*)
5	if(dataformat .ne. tndb)then
	   if( iio.lun .le. 0 ) then
		iic=inp_ch('Get events from Disk_file or from Tape [D]',IODEV)
		if(iic.lt.0) call exit(0)
	   else
	        IODEV = 'D'
	   endif
	endif
	
	call str_toupper(iodev)
	iio.datafile=IODEV.eq.'D' .OR. IODEV.eq.'V'
	iio.virtuale=IODEV.eq.'V'

	if(IODEV.eq.'D') then
	 if( dataformat .eq. TNDB )then
	  call inp_str('Path to the event file(s)',iio.name)
	  tnd.current=0
	  tnd.nfiles=tnd_getnf(iio.name)
	  if(tnd.nfiles .lt. 1)stop ' %%%ERROR - no data to sort ! '
	  if(IODEV .ne. 'D' .and. iodev .ne. 'd')stop '%%% - Sort from tape currently not implemented for TANDEM data'
	  nfiles=1
**          call inp_ask('Automatic RUN numbering    ',autonumber)
	  autonumber = .false.
	  lasttape=.true.
	  tapedismount=.false.
	  iio.datafile=.true.
	 else
	  call inp_str('Filename of the event file',iio.name)
	  iio.wlen = 32
	  if( dataformat .eq. GASP ) iio.wlen = 16
	  if( dataformat .eq. GSPH ) iio.wlen = 16
	  call inp_I1('Record size [kb]',iio.wlen)
	  iio.wlen = iio.wlen*512
	  
#if ( defined( __linux__ ) || defined(__APPLE__) )&& defined(USE_AIO)
	  if( iio.mtch .gt. 0 ) call mt_close( iio.mtch )
          iio.mtch = fileopenro( iio.name )
	  iio.lun = iio.mtch
          if( iio.mtch .lt. 0) goto 5
#else
	  if( iio.lun .gt. 0 ) close( iio.lun )
	  if(iio.lun.le.0) call lib$get_lun(iio.lun)
#  if defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER)
	  open(iio.lun,file=iio.name,status='old',readonly,access='direct',recl=iio.wlen/2,err=5)
#  else
	 open(iio.lun,file=iio.name,status='old',readonly,access='direct',recl=iio.wlen*2,err=5)
#  endif
#endif
	  nfiles=1
*	  CALL INP_I1('Number of records to analyse ',iio.maxrec)
*	  if(iio.maxrec.le.0) return
*	  autonumber=.true.
	  lasttape=.true.
	  call inp_ask('Is this the last file of the analysis',LASTTAPE)
	  tapedismount=.false.
	 endif
	elseif(IODEV.eq.'V') then
	  iio.name=' '
	  iio.vrun=1
	  iio.vevents=1000
	  call inp_i2('Vrun, Vevents',iio.vrun,iio.vevents)
	  nfiles=1
	  CALL INP_I1('Number of records to analyse ',iio.maxrec)
	  if(iio.maxrec.le.0) return
	  autonumber=.true.
	  lasttape=.true.
	  tapedismount=.false.
	else
	  write(6,*) 'Definition of the  I N P U T  tape'
CVMS	  if(lengthc(iio.name).lt.1) iio.name='MKA500:'
*NM	  if(lengthc(iio.name).lt.1) then
CVMS
	     call getenv('TAPE',iio.name)
CVMS
#ifdef Digital
	     if(lengthc(iio.name).lt.1) iio.name='/dev/nrmt0h'
#endif
#ifdef sun
	     if(lengthc(iio.name).lt.1) iio.name='/dev/rmt/0mbn'
#endif
#ifdef __linux__
	     if(lengthc(iio.name).lt.1) iio.name='/dev/nst0'
#endif
#ifdef __APPLE__
	     if(lengthc(iio.name).lt.1) iio.name='/dev/nrst0'
#endif

CVMS
*NM	  endif
	  CALL TAPE_ASSIGN (iio.name,iio.mtch)
	  CALL TAPE_CONTROL(iio.name,iio.mtch)
CVMS	  CALL STR_TOUPPER(TAPEDEVR)

	  call inp_ask('Using the tape autoloader',autoloader)
	  if(autoloader) then
	    call inp_i1('How many tapes',ncassette)
	    if(ncassette.le.0) return
	    tapedismount=.false.
	    autonumber=.true.
	    lasttape=.true.
	  else
	    call inp_ask('Automatic RUN numbering    ',autonumber)
	  endif
	  if(autonumber) then
	    CALL INP_I1('Maximum number of runs for a tape  ',NFILES)
	    if(nfiles.le.0) return
	    CALL INP_I1('Maximum number of records for a run',iio.maxrec)
	    if(iio.maxrec.le.0) return
	  endif
	  if(.not.autoloader) then
	    call inp_ask('Is this the last tape of the analysis',LASTTAPE)
	    call inp_ask('Dismount&Unload the tape at the end  ',tapedismount)
	  endif
	endif

	break=.false.
	return

	end

	subroutine gs_get_run_number(nseen)

#include "gsort.inc"

	integer nseen

	CHARACTER PROMPT*40
	CHARACTER AUTOSTRING*256		! Nastri multipli

	integer lheadonline,lheadoffline
	PARAMETER (LHEADONLINE=80,LHEADOFFLINE=256)
	
	integer ganil_get_run_no
	external ganil_get_run_no

#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif
	break=.true.
	
	if(autonumber) goto 50
	
10	iio.run=iio.run+1			! ask the user
	WRITE(LU1,*)
	WRITE(PROMPT,'(''Run  number ['',I4,'' ]'')') iio.run
	LP=MAX(1,LENGTHC(PROMPT))
	JRUN=iio.run
	CALL INP_I1(PROMPT(1:LP),JRUN)
	IF(JRUN.gt.0) then
	  iio.run=JRUN
	  iio.maxrec=1000000
	  CALL INP_I1('Number of records',iio.maxrec)
	  IF(iio.maxrec.gt.0) break=.false.
	endif
	return

50	WRITE(LU1,*)

	if(iio.virtuale) then
	  NSEEN=iio.wlen*2
	  iio.run=iio.vrun
	  WRITE(6,'(a,i5)') ' This is RUN#',iio.run
	  break=.false.
	  return
	endif
	
	IF(DATAFORMAT.EQ.EURO .OR. DATAFORMAT.eq.PRISMA ) then		! assumed to be ANSI TAPES with run# coded in HDR1 record
60	  call gs_get_header_record(NSEEN)
	  IF(NSEEN.EQ.0) goto 60
	  IF(NSEEN.LT.0) return
	  IF(NSEEN.NE.LHEADONLINE) then
	    write(LU1,*) 'Tape not properly positioned to decode run number'
	    goto 10
	  endif
	  CALL gs_list_bytes(ievheader,16,LU1)
	  AUTOSTRING=' '
	  CALL BYTES_TO_CHAR(ievheader,autostring,16)
	  IF(AUTOSTRING(1:4).EQ.'VOL1') goto 60
	  IF(AUTOSTRING(1:4).EQ.'HDR1') then
	    AUTOSTRING=AUTOSTRING(5:10)
	    la=lengthn(autostring)
	    dowhile(la.lt.0)
	      AUTOSTRING=AUTOSTRING(2:)
	      la=lengthn(autostring)
	    enddo
	    if(la.gt.0) then
	      do ii=la,1,-1
#if defined( __GFORTRAN__ )
	        write(dyn_format,'(a,i0,a)')'(I',ii,')'
		read(autostring(1:ii),dyn_format(1:len(dyn_format)),err=65) nn
#else
	        read(autostring(1:ii),4201,err=65) nn
#endif
	        iio.run= MOD(abs(nn),10000)
	        WRITE(6,'(a,i5)') ' This is RUN#',iio.run
		write(lu1,*) 'Skipping rest of header'
		call tape_skipfil(iio.mtch,1)
	        BREAK=.FALSE.
	        RETURN
65	        continue		
	      enddo
	    endif
	    write(LU1,*) 'Could not decode run number'
	    write(lu1,*) 'Skipping header records'
	    call tape_skipfil(iio.mtch,1)
	    goto 10
	  ENDIF
	  IF(AUTOSTRING(1:4).EQ.'HDR2') then
	    write(lu1,*) 'Backspacing to HDR1'
	    call tape_skiprec(iio.mtch,-2)
	    goto 60
	  endif
	  IF(AUTOSTRING(1:3).EQ.'EOF') then
	    write(lu1,*) 'Skipping EOF records'
	    call tape_skipfil(iio.mtch,1)
	    goto 60
	  endif
	  goto 100
	ENDIF
#if !defined( __GFORTRAN__ )
4201	format(I<ii>)
#endif	  
	
	if( DATAFORMAT .eq. GANIL )then
4301	   call gs_get_header_record( NSEEN )
	   iitmp = ganil_get_run_no( ievheader )
	   
	   if( iitmp .eq. -3 )then
	      CALL gs_list_bytes(ievheader,MIN(NSEEN,256),LU1)
	      call gs_get_header_record( NSEEN )
	      iitmp = ganil_get_run_no( ievheader )
	   endif
	   
	   if( iitmp .ge. 0 )then
	      iio.run = iitmp
	      break=.false.
	      return
	   elseif( iitmp .eq. -2 )then
	      CALL gs_list_bytes(ievheader,MIN(NSEEN,256),LU1)
	      write(LU1,*) 'Tape not properly positioned to decode run number'
	      goto 10
	   elseif( iitmp .eq. -3 )then
	      goto 4301
	   else
	      CALL gs_list_bytes(ievheader,MIN(NSEEN,256),LU1)
	      write(LU1,*) 'Could not decode run number'
	      goto 10
	   endif
	   
	endif
	   
	
	
	if(iio.datafile) then			! from file
#if defined(USE_AIO)
701	  call gs_get_header_record(NSEEN)
	  IF(NSEEN.EQ.0) NSEEN=-3
	  IF(NSEEN.le.0) return
#else	  
	  read(iio.lun,rec=1,err=100) (ievheader(ii),ii=1,iio.wlen)
	  NSEEN=iio.wlen*2
#endif	
	else					! from tape
70	  call gs_get_header_record(NSEEN)
	  IF(NSEEN.EQ.0) goto 70
	  IF(NSEEN.lt.0) return
	endif
	
	if( dataformat .eq. GSPN ) then
	  AUTOSTRING=' '
	  CALL BYTES_TO_CHAR(ievheader,autostring,256)
	  CALL gs_list_bytes(ievheader,MIN(NSEEN,256),LU1)
	  IF( AUTOSTRING(7:8).NE.'HG' .and. AUTOSTRING(7:8).NE.'GH' )	then
	    write(LU1,*) 'Tape not properly positioned to decode run number'
	    goto 10
	  else
	    read( autostring(141:145), '( i5 )', err= 1001)iio.run
	    WRITE(6,'(a,i5)') ' T h i s   is   R U N #',iio.run
	    BREAK=.FALSE.
	    RETURN
1001	    write(LU1,*) 'Error decoding run number'
	    goto 10
	  endif
	endif
	
	AUTOSTRING=' '
	CALL BYTES_TO_CHAR(ievheader,autostring,16)
	CALL gs_list_bytes(ievheader,MIN(NSEEN,256),LU1)
	  
	
	IF(NSEEN.GT.LHEADOFFLINE) THEN		! assumed to be GASP style numbering
	  IF( AUTOSTRING(7:8).NE.'HG' .AND. AUTOSTRING(7:8).NE.'DG' .AND. AUTOSTRING(7:8).NE.'XG') goto 100
#ifdef _GW_BIG_ENDIAN
	  call swap_bytes(ievheader(3),2)
#endif
	  iio.run=ievheader(3)
	  IF(iio.run.LT.0) goto 100
	  iio.run= MOD(iio.run,10000)
	  WRITE(6,'(a,i5)') ' This is RUN#',iio.run
	  BREAK=.FALSE.
	  RETURN
	ENDIF
    
	IF(NSEEN.EQ.LHEADOFFLINE) THEN		! assumed to be old MIPAD numbering style
	  IF(AUTOSTRING(9:13).NE.'#TAPE') goto 100
	  READ(AUTOSTRING,'(I4)',ERR=100) iio.run
	  IF(iio.run.LT.0) goto 100
	  iio.run= MOD(iio.run,10000)
	  WRITE(6,'(a,i5)') ' This is RUN#',iio.run
	  BREAK=.FALSE.
	  RETURN
	ENDIF
	
100	WRITE(LU1,*) 'Syncronization lost'
	return

	end
	
	subroutine gs_get_header_record(nseen)	! essenzialmente una TAPE_READ_SYNC

#include "gsort.inc"

	integer tape_read

	integer nseen

	nseen=0
	ISTAT=TAPE_READ(ievheader,iio.wlen*2,iio.mtch,iio.flag,iio.iosb)

CVMS	IF(.NOT.ISTAT) return

	icont=1
	if(iio.datafile) then
	    CALL DISK_SYNCR(iio.flag,iio.rec,nseen,iio.iosb,iio.name,iio.mtch,icont)
	else
	    CALL TAPE_SYNCR(iio.flag,iio.rec,nseen,iio.iosb,iio.name,iio.mtch,icont)
        endif

CVMS	if(nseen.eq.-2) nseen=-3


	return

	end

	subroutine gs_readline

#include "gsort.inc"

10	lcomline=-1
	read(inplu,'(A)',end=20) comline
	call str_tab2blank(comline)
	lcomline=lengthc(comline)
	if(lcomline.lt.1) then
	  write(lu1,*)		! empty line
	  goto 10
	else
	  write(lu1,'(1x,a)') comline(1:lcomline)
	endif

	call str_trim(comline,0)
	lcomline=lengthc(comline)
	if(comline(1:1).eq.'*' .or.
	1  comline(1:1).eq.'!' .or.
	1  comline(1:1).eq.'%' .or.
	1  comline(1:1).eq.'/') goto 10		! comline is commented

20	return

	end

	subroutine gs_writeline(LUN,list,indent)

	PARAMETER (LINELENGTH=110)

	integer lun,indent
	character*(*) list

#if defined( __GFORTRAN__ )
	character*64 dyn_format
	
	write(dyn_format,'(a,i0,a)')'(',indent,'x,a)'
#endif

	llist=lengthc(list)
	if(llist.le.0) return

	if(llist.LE.LINELENGTH) then
	  write(lun,'(1x,a)') list(1:llist)
	else
	  write(lun,'(1x,a)') list(1:LINELENGTH-2)//'-'
	  lstep=LINELENGTH-indent-1
	  do ii=LINELENGTH-1,llist,lstep
#if defined( __GFORTRAN__ )
	    write(lun,dyn_format(1:len(dyn_format))) list(ii:ii+lstep-1)//'-'
#else
	    write(lun,4201) list(ii:ii+lstep-1)//'-'
#endif
	  enddo
	endif
	list=' '
#if !defined( __GFORTRAN__ )
4201	format(<indent>x,a)
#endif
	return

	end

	subroutine gs_syntax(errline)

#include "gsort.inc"

	character errline*(*)
#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif

	if(.not.syntax) then
	  call ansi_bell(6)
	  if(whichcom.gt.0) then
	    jjn=lengthc(comline)
	    nnn=lcomline-jjn
#if defined( __GFORTRAN__ )
	    if(nnn.gt.0) then
	      write(dyn_format,'(a,i0,a)')'(1x,',nnn,'(''-''),''^'')'
	      write(6,dyn_format(1:len(dyn_format)))
	    endif
	  endif
#else
	    if(nnn.gt.0) write(6,4201)
	  endif
#endif
	  lerrline=max(1,lengthc(errline))
	  write(6,*) errline(1:lerrline)
	endif
#if !defined( __GFORTRAN__ )
4201	format(1x,<nnn>('-'),'^')
#endif

	lsynt=0
	do ii=1,nsynt
	  lsynt=lsynt+lengthc(synt(ii))
	enddo

	if(lsynt.gt.0) then
	  write(6,*)
	  write(6,*) '         The syntax of this command is'
	  write(6,*)
	  do ii=1,nsynt
	    lsynt=lengthc(synt(ii))
	    if(lsynt.gt.0) write(6,'(1x,a)') synt(ii)(1:lsynt)
	  enddo
	  write(6,*)
	endif

	if(.not.syntax) call exit(0)
	return

	end

	subroutine gs_syntax1(errline,ival)

#include "gsort.inc"

	character errline*(*)
	integer ival
#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif

	call ansi_bell(6)

	if(whichcom.gt.0) then
	  jjn=lengthc(comline)
	  nnn=lcomline-jjn
#if defined( __GFORTRAN__ )
	  if(nnn.gt.0) then
	    write(dyn_format,'(a,i0,a)')'(1x,',nnn,'(''-''),''^'')'
	    write(6,dyn_format(1:len(dyn_format)))
	  endif
	endif
#else
	  if(nnn.gt.0) write(6,4201)
	endif
4201	format(1x,<nnn>('-'),'^')
#endif
	lerrline=max(1,lengthc(errline))
	write(6,*) errline(1:lerrline),ival

	lsynt=0
	do ii=1,nsynt
	  lsynt=lsynt+lengthc(synt(ii))
	enddo

	if(lsynt.gt.0) then
	  write(6,*)
	  write(6,*) '         The syntax of this command is'
	  write(6,*)
	  do ii=1,nsynt
	    lsynt=lengthc(synt(ii))
	    if(lsynt.gt.0) write(6,'(1x,a)') synt(ii)(1:lsynt)
	  enddo
	  write(6,*)
	endif

	call exit(0)

	end

	subroutine gs_syntax2(errline,ival1,ival2)

#include "gsort.inc"

	character errline*(*)
	integer ival1,ival2
#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif
	call ansi_bell(6)

	if(whichcom.gt.0) then
	  jjn=lengthc(comline)
	  nnn=lcomline-jjn
#if defined( __GFORTRAN__ )
	  if(nnn.gt.0) then
	    write(dyn_format,'(a,i0,a)')'(1x,',nnn,'(''-''),''^'')'
	    write(6,dyn_format(1:len(dyn_format)))
	  endif
	endif
#else
	  if(nnn.gt.0) write(6,4201)
	endif
4201	format(1x,<nnn>('-'),'^')
#endif

	lerrline=max(1,lengthc(errline))
	write(6,*) errline(1:lerrline),ival1,ival2

	lsynt=0
	do ii=1,nsynt
	  lsynt=lsynt+lengthc(synt(ii))
	enddo

	if(lsynt.gt.0) then
	  write(6,*)
	  write(6,*) '         The syntax of this command is'
	  write(6,*)
	  do ii=1,nsynt
	    lsynt=lengthc(synt(ii))
	    if(lsynt.gt.0) write(6,'(1x,a)') synt(ii)(1:lsynt)
	  enddo
	  write(6,*)
	endif

	call exit(0)

	end

        subroutine gs_getname(names,nnames)

#include "gsort.inc"

        character*1 names(0:1)

        ii=lengthn(comline)
        if(ii.ne.-1) call gs_syntax('A single alphabetic character for the name expected')
        call istr_ch2up(comline,comsort)
        do ii=0,nnames-2
          if(comsort(1:1).eq.names(ii)) call gs_syntax('Name must be unique')
        enddo
        names(nnames-1)=comsort(1:1)

        return

        end

	subroutine gs_getmatmode(matmode,msdim)

#include "gsort.inc"

	integer matmode,msdim
	character*256 mcomline
	logical*1 found
	
	if(comline(1:1).eq.'(') then
	  matmode=1
	  iip=index(comline,')')
	  if(iip.le.0) call gs_syntax('Closing ) for symmetrized part expected')
	elseif(comline(1:1).eq.'{') then
	  matmode=2
	  iip=index(comline,'}')
	  if(iip.le.0) call gs_syntax('Closing } for half_symmetrized part expected')
	else
	  matmode=0
	  msdim=0
	  return
	endif
 
	mcomline=comline
	comline(1:1)=' '
	comline(iip:) = ' '
	
	call gs_getindpar(mind,mpar,ires)
	msdim=1

	lcomline=lengthc(comline)
	do while(lcomline.ne.0)
	  call gs_getindpar(jind,jpar,ires)
	  if(jind.ne.mind) call gs_syntax('Must be same index')
	  if(jpar.ne.mpar) call gs_syntax('Must be same parameter')
	  msdim=msdim+1
	  lcomline=lengthc(comline)
	enddo
	if(msdim.lt.2) call gs_syntax('There should be at least 2 Id''s in () or {}')

	comline=mcomline
	comline(1:1)=' '
	comline(iip:iip) = ' '

	return

	end

	subroutine gs_getind(ind)

#include "gsort.inc"

	integer ind
	
	call istr_ch2up(comline,comsort)
	lcomsort=lengthc(comsort)
	if(lcomsort.ne.1) call gs_syntax('Detector identifier expected')
	do ind=0,ntipi
	 if(comsort(1:1).eq.dname(ind)) return
	enddo
	call gs_syntax('Not an Identifier')
	return

	end

	subroutine gs_putind(ind)

#include "gsort.inc"

	integer ind
	
	write(list(llist:),'(a)') dname(ind)
	llist=lengthc(list)

	return

	end

	subroutine gs_getindpar(ind,par,range)

#include "gsort.inc"

	integer ind,par,range
	
	call istr_ch2up(comline,comsort)
	lcomsort=lengthc(comsort)
	if(lcomsort.lt.2) call gs_syntax('Pn (P=identifier; n=parameter_number) expected')
	do ind=0,ntipi
	  if(comsort(1:1).eq.dname(ind)) then
	    ii=TDPAR(ind)
	    iin=lengthn(comsort(2:))
	    if(iin.gt.0) then
	      read(comsort(2:lcomsort),*,err=20) par
	      if(par.lt.0 .or. par.ge.ii) call gs_syntax1('Parameter_number must be between 0 and',ii-1)
	      range=PARRES(par,ind)
	    elseif(iin.eq.-1) then
	      if(comsort(2:2).eq.'I') par=-1
	      if(ind.eq.0) call gs_syntax('Parameter HI meaningless')
	      range=NDETS(ind)
	    else
	      call gs_syntax('Invalid parameter')
	    endif
	    return
	  endif
	enddo
	call gs_syntax('Not an Identifier')
	return

20	call gs_syntax('Error reading parameter number')
	return

	end

	logical*1 function gs_testindpar(ind,par,range)

#include "gsort.inc"

	integer ind,par,range
	
	lcomsort=abs(lengthn(comline))
	if(lcomsort.lt.2) then
	  gs_testindpar = .FALSE.
	  return
	endif

	comsort = comline(1:lcomsort)
	call str_toupper(comsort)

	do ind=0,ntipi
	  if(comsort(1:1).eq.dname(ind)) then
	    ii=TDPAR(ind)
	    iin=lengthn(comsort(2:))
	    if(iin.gt.0) then
	      read(comsort(2:lcomsort),*,err=20) par
	      if(par.lt.0 .or. par.ge.ii) call gs_syntax1('Parameter_number must be between 0 and',ii-1)
	      range=PARRES(par,ind)
	    elseif(iin.eq.-1) then
	      if(comsort(2:2).eq.'I') par=-1
	      if(ind.eq.0) call gs_syntax('Parameter HI meaningless')
	      range=NDETS(ind)
	    else
	      gs_testindpar = .FALSE.
	      return
	    endif
	    gs_testindpar = .TRUE.
	    return
	  endif
	enddo

20	gs_testindpar = .FALSE.
	return

	end


	subroutine gs_putindpar(ind,par)

#include "gsort.inc"

	integer ind,par
	
	if(par.eq.-1) then
	  write(list(llist:),'(A)') dname(ind)//'I'
	else
	  lgp=1
	  if(par.GE.10) lgp=2
	  write(list(llist:),4201) dname(ind),par
	endif
	llist=lengthc(list)
#if defined( __GFORTRAN__ )
4201	format(A,i0)
#else
4201	format(A,i<lgp>)
#endif
	
	return

	end

	subroutine gs_getpair(pair,range)

#include "gsort.inc"
	
c	record/pairsdat/dat
c	pointer (pairspnt,dat)

	integer pair,range
	
	call istr_ch2up(comline,comsort)
	lcomsort=lengthc(comsort)
	if(lcomsort.lt.2 .OR. comsort(1:1).ne.'P') call gs_syntax('Pn (n=which_pair) expected')
	comsort=comsort(2:)
	lcomsort=lengthn(comsort)
	if(lcomsort.le.0) call gs_syntax('Pair_number expected')
	read(comsort,*,err=20) pair
	if(pair.lt.0 .or. pair.ge.pairsgiven) call gs_syntax1('Pair_number must be between 0 and',pairsgiven-1)

	call gs__getpair(%val(pairsaddr(pair)),range)

c	pairspnt=pairsaddr(pair)
c	range=dat.indmax

	return

20	call gs_syntax('Error reading Pair_number')
	return

	end

	subroutine gs__getpair(dat,range)

#include "gsort.inc"
	
	record/pairsdat/dat
c	pointer (pairspnt,dat)

	integer range
	
	range=dat.indmax

	return

	end

	subroutine gs_resstep(res,step,nn)

#include "gsort.inc"

	logical*1 lres,lstep

	integer nn,res(nn),step(nn)

	lres =.false.
	lstep=.false.
	iin=lengthn(comline)

	dowhile(iin.lt.0)
	  if(comline(1:1).EQ.'R'.OR. comline(1:1).EQ.'r') then
	    if(lres) call gs_syntax(' Res already given')
	    lres=.true.
	    call istr_ch(comline,comsort)
	    ii=istr_ia(comline,res,nn)
	    if(ii.ne.nn) call gs_syntax1('Number of Res parameters must be ',nn)
	    do ii=1,nn
	      IF(RES(ii).le.0) call gs_syntax('Res value must be positive')
	      IF(RES(ii).GT.MAXRES) call gs_syntax1('Res value cannnot exceed ',MAXRES)
	    enddo
	  elseif(comline(1:1).EQ.'S' .OR. comline(1:1).EQ.'s') then
	    if(lstep) call gs_syntax(' Step already given')
	    lstep=.true.
	    call istr_ch(comline,comsort)
	    ii=istr_ia(comline,step,nn)
	    if(ii.ne.nn) call gs_syntax1('Number of Step parameters must be ',nn)
	    do ii=1,nn
	      IF(STEP(ii).le.0) call gs_syntax('Step value must be positive')
	      IF(STEP(ii).gt.RES(ii)) call gs_syntax1('Step value cannot exceed ',res(ii))
	    enddo
	  else
	    return
	  endif
	  iin=lengthn(comline)
	enddo

	return

	end


	subroutine gs_putres(res,nn)

#include "gsort.inc"

	integer res(*),nn

#if defined( __GFORTRAN__ )
	character*64 dyn_format
	
	write(dyn_format, '(a,i0,a)')'(''RES'',',nn,',i5)'
	write(list(llist:),dyn_format(1:len(dyn_format))) (res(ii),ii=1,nn)
#else

	write(list(llist:),4201) (res(ii),ii=1,nn)
4201	format('RES ',<nn>i5)
#endif
	llist=lengthc(list)

	return

	end

	subroutine gs_putstep(step,nn)

#include "gsort.inc"

	integer step(*),nn

#if defined( __GFORTRAN__ )
	character*64 dyn_format
	
	write(dyn_format, '(a,i0,a)')'(''STEP'',',nn,',i4)'
	write(list(llist:),dyn_format(1:len(dyn_format))) (step(ii),ii=1,nn)
#else
	write(list(llist:),4201) (step(ii),ii=1,nn)
4201	format('STEP',<nn>i4)
#endif
	llist=lengthc(list)

	return

	end

	subroutine gs_gethash(ihash,nhash,hind,hpar,hspherical)

#include "gsort.inc"

	integer ihash,nhash,hind,hpar
	logical*1 hspherical,sameforall

c	record/hashdat/dat
c	pointer (hashpnt,dat)

	if((comline(1:1).eq.'H' .OR. comline(1:1).eq.'h')) then
	  call istr_ch2up(comline,comsort)
	  lcomsort=lengthc(comsort)
	  if(lcomsort.lt.2) call gs_syntax('Hn (n=which_hgate) expected')
	  ii=istr_i1(comsort(2:),ihash)
	  if(ii.ne.1) call gs_syntax('Hn (n=which_hgate) expected')
	  if(ihash.LT.0 .OR. ihash.GE.hashgiven) call gs_syntax1('Must be between 0 and',hashgiven)
	  nhash=1
	  iin=lengthn(comline)
	  if(iin.gt.0) ii=istr_i1(comline,nhash)

	  call gs__gethash(%val(hashaddr(ihash)),hind,hpar,sameforall)

c	  hashpnt=hashaddr(ihash)
c	  hind=dat.ind
c	  hpar=dat.par
c	  sameforall=dat.sameforall

	  hspherical=.FALSE.
	  iin=lengthn(comline)
	  if(iin.LT.0) then
	    call istr_ch2up(comline,comsort)
	    if(comsort(1:1).EQ.'C') then
	      hspherical=.FALSE.
	    elseif(comsort(1:1).EQ.'S') then
	      hspherical=.TRUE.
	    else
	      call gs_syntax('Cubic or Spherical expected')
	    endif
	  endif
	  if(nhash.lt.2) hspherical=.FALSE.
	  if(hspherical .AND. .NOT.sameforall) call gs_syntax('Spherical HGATES only if same gates on all detectors')
	else
	  ihash=-1
	  nhash=0
	endif

	return

	end

	subroutine gs__gethash(dat,hind,hpar,sameforall)

#include "gsort.inc"

	integer ihash,nhash,hind,hpar
	logical*1 hspherical,sameforall

	record/hashdat/dat

c	pointer (hashpnt,dat)
c	hashpnt=hashaddr(ihash)

	hind=dat.ind
	hpar=dat.par
	sameforall=dat.sameforall

	return

	end


	subroutine gs_puthash(nhash,ihash)

#include "gsort.inc"

	if(nhash.le.0) return

	lph=1
	if(ihash.GE.10) lph=2
	write(list(llist:),4201) ihash,' applied',nhash,' time(s)' 
	llist=lengthc(list)
#if defined( __GFORTRAN__ )
4201	format('H',I0,A,I3,A)
#else
4201	format('H',I<lph>,A,I3,A)
#endif
	return

	end


	subroutine gs_setdist2(gate,dist2,nchan)

	include 'gsort.par'

	real*4 gate(0:1),dist2(0:1)
	integer nchan

	logical*1 ingate(0:MAXRES-1)

	do ii=0,nchan-1
	  ingate(ii)=gate(ii) .GE. 0.0
	  dist2(ii)=-1.0
	end do

	i1=-1
	i2=-1
	do ii=0,nchan-1
	  if(i1.LT.0) then
	    if(ingate(ii)) i1=ii
	  else
	    if(.NOT.ingate(ii)) then
              i2=ii-1
	      gcenter=(i1+i2+1.0)/2.
	      gaxis=(i2-i1+1.0)/2.
*	      write(*,*) gcenter, gaxis
	      if(i2.GE.i1) then
	        do jj=i1,i2
	          dd=(gcenter-(jj+0.5))/gaxis
	          dist2(jj)=dd*dd
*	      write(*,*)i1,i2,jj,dd*dd
	        end do
	      endif
	      i1=-1
	      i2=-1
	    endif
	  endif
	end do
	if(i2.GT.0) then
	  gcenter=(i1+i2+1)/2.
	  gaxis=(i2-i1)/2.
	  if(i2.GT.i1) then
	    do jj=i1,i2
	      dd=(gcenter-jj+0.5)/gaxis
	      dist2(jj)=dd*dd
	    end do
	  endif
	  i1=-1
	  i2=-1
	endif
	
	return

	end

	subroutine gs_getfoldgate(iind,fl,fh,same)

#include "gsort.inc"

	integer iind,fl,fh
	logical*1 same

	if(iind.EQ.0) then
	  same=.TRUE.
	  fl=1
	  fh=1
	  return
	endif

	same=.FALSE.
	fl=1
	fh=nitems(iind)
	mf=fh

	iin=lengthn(comline)
	if(iin.gt.0) then
	  iin=istr_i2(comline,fl,fh)
	  if(iin.ne.2)  call gs_syntax('Wrong number of parameters')
	  if(fl.LT.0 .OR. fl.GT.mf) call gs_syntax('illegal value')
	  if(fh.LT.0 .OR. fh.GT.mf) call gs_syntax('illegal value')
	  if(fl.GT.fh) call gs_syntax('illegal value')
	  same=.FALSE.
	elseif(iin.lt.0) then
	  call istr_ch2up(comline,comsort)
	  if(comsort(1:1).NE.'S') call gs_syntax('SameFold expected')
	  fl=-1
	  fh=MAXDET
	  same=.TRUE.
	else
	  call gs_syntax('Missing fold parameters')
	endif  

	return

	end

	subroutine gs_putfoldgate(iind,fl,fh,same)

#include "gsort.inc"

	integer iind,fl,fh
	logical*1 same

	if(iind.GT.0) then
	  if(same) then
	    write(list(llist:),'('' [SameFold]'')')
	  else
	    write(list(llist:),'('' ['',i2,i3,'' ]'')') fl,fh
	  endif
	endif
	llist=lengthc(list)

	return

	end

	subroutine gs_getinout(inside)

#include "gsort.inc"

	logical*1 inside
	character*1 ch1

	iin=lengthn(comline)
	if(iin.lt.0) then
	  ch1=comline(1:1)
	  if(ch1.EQ.'i' .OR. ch1.eq.'I') then
	    inside=.TRUE.
	    call istr_ch2up(comline,comsort)
	  elseif(ch1.EQ.'o' .OR. ch1.eq.'O') then
	    inside=.FALSE.
	    call istr_ch2up(comline,comsort)
	  endif
	else
	  inside=.TRUE.
	endif  

	return

	end

	subroutine gs_putinout(inside)

#include "gsort.inc"

	logical*1 inside

	if(inside) then
	  write(list(llist:),'(a)') 'IN '
	else
	  write(list(llist:),'(a)') 'OUT'
	endif
	llist=lengthc(list)

	return

	end

	subroutine gs_get2ilim(l1,l2,la,lb)

#include "gsort.inc"

	integer l1,l2
	integer la,lb

	nn=istr_i2(comline,l1,l2)
	if(nn.ne.2) call gs_syntax('Wrong number of parameters')
	if(l1.LT.la .OR. l1.GT.lb  .OR.
	1  l2.LT.la .OR. l2.GT.lb  .OR.
	1  l1.GT.l2) call gs_syntax('Illegal gate values')

	return

	end

	subroutine gs_getrunmode(sign)

#include "gsort.inc"

	logical*1 sign

	iin=lengthn(comline)
	if(iin.ge.0) then
	  sign=.FALSE.
	else
	  call istr_ch2up(comline,comsort)
	  if(comsort(1:1) .EQ. 'R') then
	    sign=.TRUE.
	  elseif(comsort(1:1) .EQ. 'N') then
	    sign=.FALSE.
	  else
	    call gs_syntax('NORUN or RUN expected')
	  endif
	endif  

	return

	end

	subroutine gs_putrunmode(sign)

#include "gsort.inc"

	logical*1 sign

	if(sign) then
	  list(llist:)='RUN'
	else
	  list(llist:)='NORUN'
	endif
	llist=lengthc(list)

	return

	end

	subroutine gs_getbanres(bres)

#include "gsort.inc"

	integer bres(2)

	iin=lengthn(comline)
	if(iin.ge.0) call gs_syntax('RES Rx Ry expected')
	call istr_ch2up(comline,comsort)
	if(comsort(1:1) .NE. 'R') call gs_syntax('RES Rx Ry expected')
	iin=istr_ia(comline,bres,2)
	if(iin.ne.2)  call gs_syntax('Wrong number of resolution parameters')
	if(bres(1).LT.MINRES .OR. bres(1).GT.MAXRES .or.
	1  bres(2).LT.MINRES .OR. bres(2).GT.MAXRES) call gs_syntax2('bananares limits are ',MINRES,MAXRES)

	return

	end

	subroutine gs_putbanres(bres)

#include "gsort.inc"

	integer bres(2)

	write(list(llist:),'(''RES'',2i6)') bres(1),bres(2)
	llist=lengthc(list)

	return

	end

	subroutine gs_getxbanres(bres)

#include "gsort.inc"

	integer bres(2)

	iin=lengthn(comline)
	if(iin.ge.0) call gs_syntax('RES Rx expected')
	call istr_ch2up(comline,comsort)
	if(comsort(1:1) .NE. 'R') call gs_syntax('RES Rx expected')
	iin=istr_ia(comline,bres(1),1)
	if(iin.ne.1)  call gs_syntax('Resolution parameter is missing')
	if(bres(1).LT.MINRES .OR. bres(1).GT.MAXRES) call gs_syntax2('bananares limits are ',MINRES,MAXRES)

	return

	end

	subroutine gs_putxbanres(bres)

#include "gsort.inc"

	integer bres(2)

	write(list(llist:),'(''RES'',i6)') bres(1)
	llist=lengthc(list)

	return

	end


	subroutine gs_checkorder(ind,par,ndim,nind)

	integer ndim,nind
	integer ind(ndim),par(ndim)

	logical*1 contiguo

	do ii=1,ndim-1
	  iind=ind(ii)
	  contiguo=.true.
	  do jj=ii+1,ndim
	    if(ind(jj).eq.iind) then
	      if(.not.contiguo) call gs_syntax('Similar parameter types must be contiguous')
	    else
	      contiguo=.false.
	    endif
	  enddo
	enddo

	do ii=1,ndim-1
	  iind=ind(ii)
	  ipar=par(ii)
	  contiguo=.true.
	  do jj=ii+1,ndim
	    if(ind(jj).eq.iind .and. par(jj).eq.ipar) then
	      if(.not.contiguo) call gs_syntax('Similar parameter types must be contiguous')
	    else
	      contiguo=.false.
	    endif
	  enddo
	enddo

	call gs_countvals(ind,ndim,nind)

	return

	end
	
	subroutine gs_countvals(iarr,ndim,nvals)

	integer ndim,nvals
	integer iarr(ndim)

	nvals=1
	do ii=2,ndim
	  if(iarr(ii).ne.iarr(ii-1)) nvals=nvals+1
	enddo

	return

	end

	subroutine gs_checkdetector(ji)

#include "gsort.inc"
	integer ji

	jj=DOFF(ji)
	np=TDPAR(ji)
	do jx=DOFF(ji),DOFF(ji)+NDET(ji)-1
	  if(det(jx).id .ge. 0) then
	    if(jx.ne.jj) then
	      det(jj).id=det(jx).id
	      do jp=0,np-1
	        det(jj).ival(jp)=det(jx).ival(jp)
	        det(jj).xval(jp)=det(jx).xval(jp)
	      enddo
	    endif
	    jj=jj+1
	  endif
	enddo
	NDET(ji)=jj-DOFF(ji)

	return

	end

	subroutine gs_findrunadc(lun,run,adc,iok)

	integer lun,run,adc,iok

	character*80 line

	iok=1
	if(run.lt.0) then
	  if(adc.lt.0) then
5	    read(lun,*,err=5,end=100) ix,iy ! strip-off all comments
	    backspace(lun)
	    return
	  else
10	    read(lun,'(a)',err=10,end=100) line
	    call str_toupper(line)
	    ladc=index(line,'ADC')
	    if(ladc.le.0) goto 10
	    read(line(ladc+3:),*,err=10,end=10) iadc
	    if(iadc.ne.adc) goto 10
	    return
	  endif
	else
20	  read(lun,'(a)',err=20,end=100) line
	  call str_toupper(line)
	  lrun=index(line,'RUN')
	  if(lrun.le.0) goto 20
	  read(line(lrun+3:),*,err=20,end=20) irun
	  if(irun.ne.run) goto 20
	  if(adc.lt.0) return
	  ladc=index(line,'ADC')
	  if(ladc.le.0) goto 20
	  read(line(ladc+3:),*,err=20,end=20) iadc
	  if(iadc.ne.adc) goto 20
	  return
	endif

100	iok=0
	return

	end

	subroutine gs_init_statistics

#include "gsort.inc"
	logical*1 getmem

	statsize=(MAXDET+1)*NDTYPE*(NSTATCOM+1)
	if(.not.getmem(4*statsize,stataddr)) Stop 'Error allocating memory for STATISTICS'

	return

	end

	subroutine gs_print_statistics(ILU,stat)

#include "gsort.inc"
	record/statdat/stat

	integer total(0:100)
	logical*1 nonzero

#if defined( __GFORTRAN__ )
	character*64 dyn_f1, dyn_f2, dyn_f3
	
	write( dyn_f1(1:len(dyn_f1)), '(a,i0,a)')'(2x,a,',NSTATCOM+1,'(a,i1))'
	write( dyn_f2(1:len(dyn_f2)), '(a,i0,a)')'(3x,a,'' fold '',i2,',NSTATCOM+1,'i11)'
	write( dyn_f3(1:len(dyn_f3)), '(a,i0,a)')'(3x,a,'' Total '',',NSTATCOM+1,'i11)'
#else
4201	format(2x,a,<NSTATCOM+1>(a,i1))
4202	format(3x,a,' fold ',i2,<NSTATCOM+1>i11)
4203	format(3x,a,' Total ',<NSTATCOM+1>i11)
#endif

	do ji=1,NTIPI
#if defined( __GFORTRAN__ )
	    WRITE(ILU,dyn_f1) dname(ji)//'-Statistics    RAW',('      STAT',ii,ii=1,NSTATCOM)
#else
	    WRITE(ILU,4201) dname(ji)//'-Statistics    RAW',('      STAT',ii,ii=1,NSTATCOM)
#endif
	    do ii=0,NSTATCOM
	      total(ii)=0
	    enddo
	    do nn=0,MAXDET
	      nonzero=.false.
	      do ii=0,NSTATCOM
	        if(STAT.FOLD(nn,ji,ii).gt.0) then
	          nonzero=.true.
	          total(ii)=total(ii)+STAT.FOLD(nn,ji,ii)
	        endif
	      enddo
#if defined( __GFORTRAN__ )
	      if(nonzero) WRITE(ILU,dyn_f2)dname(ji), nn,(STAT.FOLD(nn,ji,ii),ii=0,NSTATCOM)
#else
	      if(nonzero) WRITE(ILU,4202)dname(ji), nn,(STAT.FOLD(nn,ji,ii),ii=0,NSTATCOM)
#endif
	    enddo
	    nonzero=.false.
	    do ii=0,NSTATCOM
	      if(total(ii).gt.0) nonzero=.true.
	    enddo	
#if defined( __GFORTRAN__ )
	    if(nonzero)   WRITE(ILU,dyn_f3) dname(ji), (total(ii),ii=0,NSTATCOM)
#else
	    if(nonzero)   WRITE(ILU,4203) dname(ji), (total(ii),ii=0,NSTATCOM)
#endif
	enddo

	return

	end

	subroutine gs_matopen(mat)

! Apre la matrice compressa su disco. Se non la trova o se non e'
! consistente ne crea una nuova.
! Inizializza i buffers di incremento

#include "gsort.inc"
	record/matrix/mat

	logical*1 cmt_open,cmt_new,cmt_info,cmt_close,cmt_incr_init
	logical file_opened
	
	integer filexist
	external filexist

	integer  lengthc
	external lengthc

	integer res(MAXMATDIM),step(MAXMATDIM)

	write(lu1,*)

	if( filexist( mat.file ) .eq. 0 ) then
		inquire( file=mat.file, opened=file_opened )
		if( file_opened ) then
			write(lu1,*) 'GSORT ERROR: Matrix '//mat.file(1:max(1,lengthc(mat.file)))//' is already opened'
			stop
		endif
	else
		goto 100
	endif

	if(.not.cmt_open(mat.file,1,mat.cmtbase)) then
	   call ansi_bell(6)
	   write(lu1,*) 'Error opening old Matrix'
	   call ansi_bell(6)
	   goto 100
	endif
	call cmt_show(%val(mat.cmtbase),lu1)

	if(.not.cmt_info(%val(mat.cmtbase),-1,ndim,mode)) continue
	if(.not.cmt_info(%val(mat.cmtbase),-2,res,step)) continue

	if(ndim.ne. mat.ndim .or. mode.ne.mat.matmode) goto 90
	do ii=1,ndim
	  if(res(ii) .ne. mat.res(ii)) goto 90
	  if(step(ii).ne. mat.step(ii)) goto 90
	enddo
	goto 200

90	call ansi_bell(6)
	write(lu1,*) 'The old Matrix is not consistent with the requests!'
	if(.not.cmt_close(%val(mat.cmtbase),' ')) continue

100	write(lu1,*) 'Will create a new Matrix'
	if(.not.cmt_new(mat.file,mat.ndim,mat.res,mat.step,mat.matmode)) stop 'Error creating Matrix_file'
	if(.not.cmt_open(mat.file,1,mat.cmtbase)) stop 'Error re-opening Matrix_file'
	call cmt_show(%val(mat.cmtbase),lu1)

200	if(.not.cmt_incr_init(%val(mat.cmtbase))) stop 'Error initializing increment buffers'

	return

	end

	subroutine gs_matclose(mat)

! Svuota i buffers di incremento e riporta le proiezioni
! Chiude la matrice compressa su disco

#include "gsort.inc"
	record/matrix/mat

	integer lfn

	integer lengthc
	logical*1 cmt_info,cmt_incr_finit,cmt_close

	lfn=max(1,lengthc(mat.file))
	if(.not.cmt_info(%val(mat.cmtbase),-3,ii,nn)) write(6,*) 'Error Getting Number of Segments'
	write(6,'(a,i7,a)') ' Flushing',ii,'  Memory Buffers for '//mat.file(1:lfn)
	if(.not.cmt_incr_finit(%val(mat.cmtbase))) write(6,*) 'Error Flushing Memory Buffers'

	if(.not.cmt_close(%val(mat.cmtbase),mat.file)) write(6,*) 'Error closing Matrix'

	return

	end

	subroutine gs_get_angles(th,ph,nn,angfile,lun)

#if defined( __GFORTRAN__ )
	logical*1 inp_yes
	external inp_yes
#endif

	integer nn,lun
	real th(0:nn-1)
	real ph(0:nn-1)
	character angfile*(*) 

	character*80 line
	integer inlu /-1/

	if(inlu.le.0) call lib$get_lun(INLU)

	langfile=max(1,lengthc(angfile))

6	open(unit=INLU,file=angfile,status='old',READONLY,ERR=7)
	nga=0
	write(6,*)
	write(6,*) 'Reading angles of the detectors from  '//angfile(1:langfile)
	if(lun.ne.6) write(lun,*) 'Reading angles of the detectors from  '//angfile(1:langfile)
	goto 8

7	write(6,*) 'Error reading  '//angfile(1:langfile)
	if(inp_yes('Retry')) goto 6
	call exit(0)

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


	subroutine gs_test_stop(stopsort,lu1,lu2)

	logical*1 stopsort
	integer lu1,lu2
	
	integer inlu
	data inlu /-1/

! Se il file STOP_MATRIX esiste viene settata la flag corrispondente
! Il file viene poi cancellato

	if(inlu.le.0) call lib$get_lun(inlu)
	OPEN(inlu,FILE='STOP_MATRIX',STATUS='OLD',ERR=100)
	lulu=lu1
	do ii=1,2
	  if(lulu.gt.0) then
	    WRITE(lulu,*) '*************************************************'
	    WRITE(lulu,*) '********** gsort stopped with mat_stop **********'
	    WRITE(lulu,*) '*************************************************'
	  endif
	 lulu=lu2
	enddo
#if defined( __GFORTRAN__ )
	CLOSE(inlu,STATUS='DELETE')
#else
	CLOSE(inlu,DISPOSE='DELETE')
#endif
	stopsort=.true.
	return

100	stopsort=.false.
	RETURN

	END

	subroutine gs_swap_bytes(IBUF,nwords)

	INTEGER*2 IBUF(1)

	integer*2 word
	byte      byte1,byte2(2)
	equivalence (byte2,word)

	do ii=1,nwords
	    word=ibuf(ii)
	    byte1=byte2(1)
	    byte2(1)=byte2(2)
	    byte2(2)=byte1
	    ibuf(ii)=word
	enddo

	return

	end

	subroutine gs_list_bytes(BUFFER,Nchars,LU)

	BYTE BUFFER(*)			! BUFFER LETTURA NASTRO
	CHARACTER CODE*64,CH*1

	DO JJ=1,Nchars,64
	   ICO=0
	   DO II=JJ,JJ+63
		ICO=ICO+1
		CH=CHAR(BUFFER(II))
		IF(CH.LT.' ' .OR. CH.GT.'~') CH='.'
		CODE(ICO:ICO)=CH
	   ENDDO
	   WRITE(lu,'(5x,A)') CODE
	ENDDO
	IF(LU.EQ.6) WRITE(6,*)
	RETURN

	END

	subroutine gs_statusline(secondi,nseen)

#include "gsort.inc"

	real*4 secondi
	integer*8 nincr_p(MAXMATDIM),nincrt
	integer ievents_p,oevents_p,flushed_p
	save    ievents_p,oevents_p,flushed_p

	if(secondi.le.0) then
	  do ii=1,MAXMATDIM
	    nincr_p(ii)=0
	  enddo
	  iio.evcount=0
	  oio.evcount=0
	  ievents_p=0
	  oevents_p=0
	  flushed_p=0
	  return
	endif

	call FINIT_RUN(0)
	
	if( secondi .lt. 0.200 ) then
	  nprint = 10*nprint
	  return
	endif
	
	xprint=nprint
	ievents_p=iio.evcount-ievents_p
	oevents_p=oio.evcount-oevents_p
	flushed_p=flushed-flushed_p
	nincrt=0
	do ii=1,MAXMATDIM
	  nincr_p(ii)=nincr(ii)-nincr_p(ii)
	  nincrt=nincrt+nincr_p(ii)
	enddo

	write(list,121) iio.rec,nprint/secondi,nseen/2,int(ievents_p/xprint),nincrt/xprint
	llist=lengthc(list)+1

	if(oev.defined) then
	  WRITE(list(llist:),122) int(oevents_p/xprint),oio.rec
	else
	  WRITE(list(llist:),123) flushed_p/secondi
	endif

	llist=lengthc(list)
*#ifdef Digital
	write(LU1,'(a3,1x,A)')char(27)//char(91)//char(65), list(1:llist)
*#else
*	write(LU1,'(''\r'',1x,A,$)') list(1:llist)
*#endif
	call flush(LU1)
	IF(MOD(iio.REC,NPRINT2).EQ.0) then
		write(LU2,'(1x,A)') list(1:llist)
		call flush( LU2 )
	endif
	
	ievents_p=iio.evcount
	oevents_p=oio.evcount
	flushed_p=flushed
	do ii=1,MAXMATDIM
	  nincr_p(ii)=nincr(ii)
	enddo

121	FORMAT('Rec#',I7,' (',f6.1,'/s)',I6,'w',I6,'ev/r',F10.2,'inc/r')
122	FORMAT(I6,'oev/r',I7,'rec_out')
123	FORMAT(f6.1,'sw/s')


	if( secondi .lt. 2.0 ) nprint = nprint+100
	if( secondi .lt. 1.0 ) nprint = nprint+100

	return

	end



	subroutine gs_obsolete

#include "gsort.inc"
	logical*1 getmem

	statsize=(MAXDET+1)*NDTYPE*(NSTATCOM+1)
	if(.not.getmem(4*statsize,stataddr)) Stop 'Error allocating memory for STATISTICS'

	return

	end

#if defined(__IFC) || defined(__INTEL_COMPILER)

	subroutine GS_INT_RUN
#include "gsort.inc"
	logical*1 stopsort
	common /STOP_CTRL/stopsort
	
	
	CALL ANSI_BELL(LU1)
	stopsort = .TRUE.

	END
#endif
	
	real*8 function gs_sint( x )
	
	real*8 x
	real*8 tmp
	logical initialized
	real*8  step, t(0:4000)
	save step, t, initialized
	
	if( .NOT.initialized ) then
	   step = 0.090D0
	   tmp = 0.000D0
	   do ii = 0, 4000
              t(ii) = dsind(tmp)
	      tmp = tmp + step
	   enddo
	   initialized = .TRUE.
	endif
	
	if( dabs(x) .GT. 360.0D0 )then
	  ii = x/360.00D0
	  tmp = ii
	  tmp = x - tmp*360.00D0
	else
	  tmp = x
	endif
	
	ii = dabs(tmp/step)
	tmp = t(ii) + (t(ii+1)-t(ii))*(dabs(tmp)/step - ii)
	if( x .GT. 0.00D0 ) then
	   gs_sint = tmp
	else
	   gs_sint = -tmp
	endif
	
	return
	end
	
	real*8 function gs_cost( x )
	
	real*8 x
	real*8 tmp
	logical initialized
	real*8  step, t(0:4000)
	save step, t, initialized
	
	if( .NOT.initialized ) then
	   step = 0.090D0
	   tmp = 0.000D0
	   do ii = 0, 4000
              t(ii) = dcosd(tmp)
	      tmp = tmp + step
	   enddo
	   initialized = .TRUE.
	endif
	
	if( dabs(x) .GT. 360.0D0 )then
	  ii = x/360.00D0
	  tmp = ii
	  tmp = x - tmp*360.00D0
	else
	  tmp = x
	endif
	
	ii = dabs(tmp/step)
	gs_cost = t(ii) + (t(ii+1)-t(ii))*(dabs(tmp)/step - ii)
	
	return
	end
	
	   
