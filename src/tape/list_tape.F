	PROGRAM LISTTAPE

C  LISTA I RECORDS SU UN NASTRO DATI LNL

	PARAMETER (NBYTE=32768)		! MAX. LUNGHEZZA RECORDS SU NASTRO

	PARAMETER (MTFLAG=1)		! FLAG PER SINCRONIZZAZIONE NASTRO
	INTEGER*4 IOSB(4)
	INTEGER*4 MTCH,NSEEN,ISTAT
	INTEGER TAPE_READ
	EXTERNAL TAPE_READ
	CHARACTER TAPEDEV*64

	INTEGER*2 IBUFR(NBYTE)		! BUFFER LETTURA NASTRO
	
	LOGICAL LONG	/.FALSE./
	LOGICAL DECIMAL	/.FALSE./
	LOGICAL SWAPBYTES /.FALSE./

	call xinpmode(1)
*	OPEN(6,CARRIAGECONTROL='FORTRAN',STATUS='UNKNOWN')
	
	MTCH = 0
	WRITE(6,*)
#ifdef Digital
	TAPEDEV='/dev/nrmt0h'
#endif
#ifdef sun
	TAPEDEV='/dev/rmt/0mbn'
#endif
#ifdef __linux__
	TAPEDEV='/dev/nst0'
#endif
#ifdef __APPLE__
	TAPEDEV='/dev/nrst0'
#endif
	CALL TAPE_ASSIGN(TAPEDEV,MTCH)  ! SCELTA DELL' UNITA` NASTRO

	MAXRECS=1000000

1	CALL TAPE_CONTROL(TAPEDEV,MTCH) 
	IREC=0
	CALL INP_I1('Number of records (<0 to stop)',MAXRECS)
	IF(MAXRECS.LE.0) CALL EXIT(0)

	call inp_ask('Long listing',long)
	IF(LONG) THEN
	  call inp_ask('Decimal',decimal)
	  call inp_ask('Swap bytes',swapbytes)
	  if(decimal) then
	    IDEC=1
	  else
	    idec=0
	  endif
	  call inp_i1('Number of events',nev)
	ENDIF
	
	IF (MTCH.EQ.0) THEN
	  WRITE(6,'(''  : Tape unit not opened'')')
	  GOTO 1
	ENDIF
	write(6,*)
	
10	IF(IREC.GE.MAXRECS) GOTO 1

	
	ISTAT=TAPE_READ(IBUFR,NBYTE,MTCH,MTFLAGD,IOSB)
C	if(ISTAT.EQ.0) goto 1
	CALL TAPE_SYNCR(MTFLAG,IREC,NSEEN,IOSB,TAPEDEV,MTCH,1)

	IF(NSEEN.EQ.0) GOTO 10
	IF(NSEEN.LT.0) GOTO 1

	IREC=IREC+1
	IF(NSEEN.EQ.256) THEN
	   WRITE(6,20) IREC,NSEEN/2
20	   format(' Rec#',2I6,2Hw ,8I7)
	   CALL LISTHEAD(IBUFR,NSEEN)
	   WRITE(6,*)
	   GOTO 10
	ENDIF
	IF(LONG) THEN
	   WRITE(6,'('' Rec#'',2I6,''w'')') IREC,NSEEN/2
	   if(nseen.gt.0) then
	     if(swapbytes) call swap_bytes1(ibufr,nseen/2)
	     CALL LISTLNL(IBUFR,NSEEN,IDEC,NEV)
	   endif
	ELSE
	   WRITE(6,'(a3,''Rec#'',2I6,''w'')')char(27)//char(91)//char(65), IREC,NSEEN/2
	   call flush(6)
	ENDIF
	GOTO 10

	END

	subroutine swap_bytes1(IBUF,nwords)

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

	SUBROUTINE LISTHEAD(BUFFER,NBYTES)

	BYTE BUFFER(NBYTES)		! BUFFER LETTURA NASTRO
	CHARACTER CODE*16,CH*1

	DO JJ=1,NBYTES,16
	   ICO=0
	   DO II=JJ,JJ+15
		ICO=ICO+1
		CH=CHAR(BUFFER(II))
		IF(CH.LT.' ' .OR. CH.GT.'~') CH='.'
		CODE(ICO:ICO)=CH
	   ENDDO
	   WRITE(6,'(5X,16Z3,5X,A)') (BUFFER(II),II=JJ,JJ+15),CODE
	ENDDO
	WRITE(6,*)
	RETURN

	END

	SUBROUTINE LISTLNL(IBUFF,NBYTES,IDEC,NEV)

	INTEGER*2 IBUFF(1)
	INTEGER*2 IDAT,nm_mask

	ILAST=NBYTES/2
	LEV=0

	WRITE(6,'(8X,8I8)') (IBUFF(II),II=1,16)
	IPNT=17
15	IF(IPNT.GT.ILAST) GOTO 25
	IDAT=IBUFF(IPNT)
	IPNT=IPNT+1
	IF(IDAT.GE.0) GOTO 15

16	IP1=IPNT-1			! SW
17	IF(IPNT.GT.ILAST) GOTO 25
	IDAT=IBUFF(IPNT)
	IPNT=IPNT+1
*	IF(IDAT.GE.0) GOTO 17
	nm_mask = 'F000'X
	if(iand(idat,nm_mask) .ne. nm_mask)goto 17
	IP2=IPNT-2
	NDAT=IP2-IP1+1
	IF(IDEC.NE.1) THEN
	   WRITE(6,'(I6,Z5,14Z5,/,(11X,14Z5))') IP1,(IBUFF(II),II=IP1,IP2)
	ELSE
	   WRITE(6,'(I6,Z5,14I10,/,(11X,14I9))') IP1,(IBUFF(II),II=IP1,IP2)
	ENDIF
	LEV=LEV+1
	IF(LEV.GE.NEV .AND. NEV.GT.0) RETURN
	GOTO 16

25	RETURN

	END
