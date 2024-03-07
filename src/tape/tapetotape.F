	PROGRAM TapeToTape

#define GO_UP_ONE_LINE char(27)//char(91)//char(65)

!	COPIA nastro-nastro
!	con la possibilita' di aggiornare il run number dell'ONLINE

	PARAMETER (MTFLAGR=1)		! FLAG PER SINCRONIZZAZIONE NASTRO)
CVMS	INTEGER*2 IOSBR(4)		! DI LETTURA
	INTEGER*4 IOSBR(4)		! DI LETTURA
CVMS	INTEGER*2 MTCHR
	INTEGER   MTCHR
CVMS	CHARACTER*10 TAPEDEVR	/'MKA400:'/
#if defined(Digital)
	CHARACTER*80 TAPEDEVR	/'/dev/nrmt0h'/
#elif defined(sun)
	CHARACTER*80 TAPEDEVR	/'/dev/rmt/0mbn'/
#elif defined(__linux__)
	CHARACTER*80 TAPEDEVR	/'/dev/nst0'/
#elif defined(__APPLE__)
	CHARACTER*80 TAPEDEVR	/'/dev/nrst0'/
#else
*   error "Undefined platform"
	CHARACTER*80 TAPEDEVR	/'/dev/nrst0'/
#endif

	PARAMETER (MTFLAGW=2)		! FLAG PER SINCRONIZZAZIONE NASTRO
CVMS	INTEGER*2 IOSBW(4)		! DI SCRITTURA
	INTEGER*4 IOSBW(4)		! DI SCRITTURA
	INTEGER*2 MTCHW
CVMS	CHARACTER*10 TAPEDEVW	/'Mka500:'/
#if defined(Digital)
	CHARACTER*80 TAPEDEVW	/'/dev/nrmt1h'/
#elif defined(sun)
	CHARACTER*80 TAPEDEVW	/'/dev/rmt/1mbn'/
#elif defined(__linux__)
	CHARACTER*80 TAPEDEVW	/'/dev/nst1'/
#elif defined(__APPLE__)
	CHARACTER*80 TAPEDEVW	/'/dev/nrst1'/
#else
*   error "Undefined platform"
	CHARACTER*80 TAPEDEVW	/'/dev/nrst1'/
#endif

	PARAMETER (MBYTER=32*1024)	  ! Physical block length in byte READ
	PARAMETER (MWORDR=MBYTER/2)	  ! logical*1 block length in words READ
	PARAMETER (MBLOCKS=1)		  ! #of blocks/logical*1 record
	PARAMETER (NBYTEW=32*1024)	  ! Phys.&logical*1 record length  WRITE
	PARAMETER (NWORDW=NBYTEW/2)	  !             "

	integer*2 buffrw(2*MWORDR)		! BUFFER r/w

	logical*1 INP_NOT
	external INP_NOT
	logical*1 newrun,askoneof,setdg,setxg,firstrecord
	character*80 prompt
	
CVMS	
*	OPEN(UNIT=6,CARRIAGECONTROL='FORTRAN',STATUS='UNKNOWN')
	call xinpmode(1)

	WRITE(6,*)
	WRITE(6,*) '          Program to copy one tape to another'
	WRITE(6,*) '          mantaining the same file structure'
	WRITE(6,*)

CVMS
	call getenv('TAPE1',prompt)
CVMS
	if(lengthc(prompt) .GT. 0 ) then
CVMS
	  tapedevr=prompt
CVMS
	else
CVMS
	  call getenv('TAPE',prompt)
CVMS
	  if(lengthc(prompt) .GT. 0 ) tapedevr=prompt
CVMS
	endif

	WRITE(6,*)
	WRITE(6,*) 'Tape unit with the original data'
	CALL TAPE_ASSIGN(TAPEDEVR,MTCHR)
	CALL TAPE_CONTROL(TAPEDEVR,MTCHR)

CVMS
	call getenv('TAPE2',prompt)
CVMS
	if(lengthc(prompt) .GT. 0 ) tapedevw=prompt

	WRITE(6,*)
	WRITE(6,*) 'Tape unit to be written'
	CALL TAPE_ASSIGN (TAPEDEVW,MTCHW)
	CALL TAPE_CONTROL(TAPEDEVW,MTCHW)

	itapnum=0
	nfile=0

	write(6,*) 'The next 3 questions are meaningful only for GASP data tapes'

	newrun=inp_not('Update the ONLINE run number')

	npack=1
	call inp_i1('Number of input records do pack together',npack)
	if(npack.le.0) npack=1

	setdg=inp_not('Update the "DG" field on the record header')
	if(.not.setdg) setxg=inp_not('Update the "XG" field on the record header')



	ifilestot=10
	call inp_i1('Number of files to copy',ifilestot)

	irecmax=10000000
	if(ifilestot.eq.1) then
	  call inp_i1('Number of records to copy',irecmax)
	  askoneof=.true.
	else
	  askoneof=inp_not('Want to be asked on EOF')
	endif

	prompt=' '
	firstrecord=.true.

10	nfile=nfile+1
	if(nfile.gt.ifilestot) goto 40
	write(6,*) ' Copying file#',nfile
	write(6,*)
	if(newrun) then
	  ITAPNUM=MOD(ITAPNUM+1,1000)
	  WRITE(PROMPT,'(''Run number ['',I4,'' ]'')') ITAPNUM
	  LENPR=MAX(LENGTHC(PROMPT),1)
	  CALL INP_I1(PROMPT(1:LENPR),ITAPNUM)
	  if(itapnum.lt.0) goto 40
	endif
	irecr=0
	irecw=0

20	nraddr=1
	do np=1,npack
	  CALL TAPE_READ(buffRW(nraddr),MBYTER,MTCHR,MTFLAGR,IOSBR)
	  CALL TAPE_SYNCR(MTFLAGR,IRECR,NSEENR,IOSBR,TAPEDEVR,MTCHR,1)
	  if(nseenr.eq.0) then
	    goto 20
	  elseif(nseenr.eq.-2) then
	    if(irecw.gt.0) write(6,*) irecw,' Records copied'
	    firstrecord=.true.
	    if(askoneof) then
	      CALL TAPE_CONTROL(TAPEDEVR,MTCHR)
	      CALL TAPE_CONTROL(TAPEDEVW,MTCHW)
	      if(inp_not('Continue reading')) goto 20
	    endif
	    write(6,*) 'Writing EOF on   ',Tapedevw
	    CALL TAPE_WRITEOF(MTCHW)
	    goto 10
	  elseif(nseenr.eq.-3) then
	    if(irecw.gt.0) write(6,*) irecw,' Records copied'
	    write(6,*) 'Writing EOF on   ',Tapedevw
	    CALL TAPE_WRITEOF(MTCHW)
	    goto 40
	  elseif(nseenr.le.0) then
	    CALL TAPE_CONTROL(TAPEDEVR,MTCHR)
	    CALL TAPE_CONTROL(TAPEDEVW,MTCHW)
	    if(inp_not('Continue')) goto 20
	    goto 40
	  endif
	  nraddr=nraddr+nseenr/2
	  irecr=irecr+1
	enddo
	nwbytes=(nraddr-1)*2

	if(newrun) buffrw(3)=itapnum
	if(firstrecord) then
	  firstrecord=.false.
	else 
	  if(setdg) buffrw(4)='4744'X
	  if(setxg) buffrw(4)='4758'X
	endif
	CALL TAPE_WRITE(BUFFRW,nwbytes,MTCHW,MTFLAGW,IOSBW)
	CALL TAPE_SYNCW(MTFLAGW,IRECW,ISTW,IOSBW,TAPEDEVW,MTCHW,0)
	IF(ISTW.LT.0) then
	  write(6,*) 'Error writing record',irecw
	  GOTO 40
	ENDIF
	irecw=irecw+1
	if(mod(irecw,10).eq.0) then
	  if(irecw.gt.0) WRITE(6,'(a3,2i10)')GO_UP_ONE_LINE, IRECR,IRECW
	endif
	if(irecr.lt.irecmax) goto 20
	WRITE(6,'(a3,2i10)')GO_UP_ONE_LINE, IRECR,IRECW
	write(6,*) 'No EOF written on  ',tapedevw
	CALL TAPE_CONTROL(TAPEDEVW,MTCHW)

40	if(irecw.eq.0) nfile=nfile-1
	if(nfile.le.0) then
	   Write(6,'(a)') '   No Files copied'
	elseif(nfile.eq.1) then
	   Write(6,'(a)') '   1  File  copied'
	else
	   Write(6,'(1x,i5,a)') nfile,' Files copied'
	endif
	CALL TAPE_CONTROL(TAPEDEVR,MTCHR)
	CALL TAPE_CONTROL(TAPEDEVW,MTCHW)

	call exit

	end
