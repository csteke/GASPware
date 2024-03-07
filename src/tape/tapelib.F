	SUBROUTINE TAPE_CONTROL(TAPEDEV,MTCH)

	CHARACTER TAPEDEV*(*)
	INTEGER*4 MTCH,IERR,mtflagd,irec,maxbyte
	
	PARAMETER (MTFLAGD=3)
	INTEGER*4 IOSB(4)
*	PARAMETER (MAXBYTE=64*1024-4)
	PARAMETER (MAXBYTE=32*1024)
	INTEGER*2 BUFFD(MAXBYTE/2)
	
	INTEGER TAPE_READ,TAPE_WRITE

	INTEGER*4 NSEEN,NCHD
	logical MSTATUS

	DATA NCHD/1024/

	IF(TAPEDEV(1:2).EQ.'NL') RETURN

	ltapedev=max(lengthc(tapedev),1)
	WRITE(6,*) 'You can now operate on ',TAPEDEV(1:ltapedev)
	WRITE(6,*) '   (1) SKIP_RECORDS        (5) DISMOUNT/NOUNLOAD'
	WRITE(6,*) '   (2) SKIP_FILES          (6) MOUNT/FOREIGN'
	WRITE(6,*) '   (3) WRITE_1_EOF         (7) DUMP ONE RECORD'
	WRITE(6,*) '   (4) REWIND              (8) GO TO EndOfData'

1	IFUN=0
	CALL INP_MODE(0)
	CALL INP_I1('Input  Action,Value or <CR> ',IFUN)
	IF(IFUN.EQ.0) THEN
	  WRITE(6,*)
	  RETURN
	ELSEIF(IFUN.EQ.1) THEN 
	  NN=0
	  CALL INP_SHOWBUFF(IL)
	  IF(IL.GT.0) THEN	! dopo lo 1 ci sono dei numeri
	    CALL INP_MODE(2)
	    CALL INP_I1('Number of records to skip ',NN)
	    CALL INP_MODE(0)
	  ENDIF
	  CALL TAPE_SKIPREC(MTCH,NN)
	ELSEIF(IFUN.EQ.2) THEN 
	  NN=0
	  CALL INP_SHOWBUFF(IL)
	  IF(IL.GT.0) THEN	! dopo il 2 ci sono dei numeri
	    CALL INP_MODE(2)
	    CALL INP_I1('Number of files to skip ',NN)
	    CALL INP_MODE(0)
	  ENDIF
	  CALL TAPE_SKIPFIL(MTCH,NN)
	ELSEIF(IFUN.EQ.3) THEN 
	  CALL TAPE_WRITEOF(MTCH)
	ELSEIF(IFUN.EQ.4) THEN 
	  CALL TAPE_REWIND(MTCH)
	ELSEIF(IFUN.EQ.5) THEN 
	  IUNL=0
	  CALL INP_SHOWBUFF(IL)
	  IF(IL.NE.0) IUNL=1
	  CALL TAPE_DISMOUNT(TAPEDEV,MTCH,IUNL)
	ELSEIF(IFUN.EQ.6) THEN 
	  CALL INP_SHOWBUFF(IL)
	  IF(IL.LT.0) THEN	! dopo il 6 sono stati battuti dei caratteri
	    CALL INP_MODE(2)
	    IL=MAX(1,LENGTHC(TAPEDEV))
	    CALL INP_STR('Input tape unit name ['//TAPEDEV(1:IL)//']',TAPEDEV)
	    CALL INP_MODE(0)
CVMS	    IF(INDEX(TAPEDEV,':').EQ.0) THEN
CVMS	      I=INDEX(TAPEDEV,' ')
CVMS	      TAPEDEV(I:I)=':'
CVMS	    ENDIF
	  ENDIF
	  CALL TAPE_MOUNTFOR(TAPEDEV,MTCH,MSTATUS)
	ELSEIF(IFUN.EQ.7)  THEN 
	  CALL INP_SHOWBUFF(IL)
	  IF(IL.GT.0) THEN	! dopo il 7 ci sono dei numeri
	    CALL INP_MODE(2)
	    CALL INP_I1('Number of bytes to dump ',NCHD)
	    CALL INP_MODE(0)
	  ENDIF
	  irec=1
	  ISTAT=TAPE_READ(BUFFD,MAXBYTE,MTCH,MTFLAGD,IOSB)
C	  if(istat.eq.1) then
	    CALL TAPE_SYNCR(MTFLAGD,irec,NSEEN,IOSB,TAPEDEV,MTCH,1)
	    if(NSEEN.gt.0) then
	      write(6,*) 'Record length',NSEEN,' Bytes'
	      NBY=min(NSEEN,NCHD)
	      write(6,*) 'Byte Dump of ',NBY,' of them'
	      call TAPE_LIST(BUFFD,NBY)
	    endif
C	  endif
	ELSEIF(IFUN.EQ.8)  THEN 
	  IF(MTCH.GT.0) THEN
	    call MT_EOD(MTCH, IERR)
	  else
	    write(6,*)'>>> tape not yet oppened'
C 	    IUNL=9
C	    CALL TAPE_DISMOUNT(TAPEDEV,MTCH,IUNL)
C	    MTCH=0
	  ENDIF
C	  CALL EXIT
        else
	  return
	ENDIF
	GOTO 1

	END

	SUBROUTINE TAPE_ASSIGN(TAPEDEV,MTCH)

	CHARACTER TAPEDEV*(*)
	INTEGER*4 MTCH

	IL=MAX(1,LENGTHC(TAPEDEV))
	CALL INP_STR('Input tape unit name ['//TAPEDEV(1:IL)//']',TAPEDEV)
	if(lengthc(tapedev).EQ.1) then
#ifdef Digital
	   if(tapedev(1:1).eq.'0') tapedev='/dev/nrmt0h'
	   if(tapedev(1:1).eq.'1') tapedev='/dev/nrmt1h'
	   if(tapedev(1:1).eq.'2') tapedev='/dev/nrmt2h'
	   if(tapedev(1:1).eq.'3') tapedev='/dev/nrmt3h'
	   if(tapedev(1:1).eq.'4') tapedev='/dev/nrmt4h'
	   if(tapedev(1:1).eq.'5') tapedev='/dev/nrmt5h'
	   if(tapedev(1:1).eq.'6') tapedev='/dev/nrmt6h'
	   if(tapedev(1:1).eq.'7') tapedev='/dev/nrmt7h'
	   if(tapedev(1:1).eq.'8') tapedev='/dev/nrmt8h'
	   if(tapedev(1:1).eq.'9') tapedev='/dev/nrmt9h'
#endif
#ifdef sun
	   if(tapedev(1:1).eq.'0') tapedev='/dev/rmt/0mbn'
	   if(tapedev(1:1).eq.'1') tapedev='/dev/rmt/1mbn'
	   if(tapedev(1:1).eq.'2') tapedev='/dev/rmt/2mbn'
	   if(tapedev(1:1).eq.'3') tapedev='/dev/rmt/3mbn'
	   if(tapedev(1:1).eq.'4') tapedev='/dev/rmt/4mbn'
	   if(tapedev(1:1).eq.'5') tapedev='/dev/rmt/5mbn'
	   if(tapedev(1:1).eq.'6') tapedev='/dev/rmt/6mbn'
	   if(tapedev(1:1).eq.'7') tapedev='/dev/rmt/7mbn'
	   if(tapedev(1:1).eq.'8') tapedev='/dev/rmt/8mbn'
	   if(tapedev(1:1).eq.'9') tapedev='/dev/rmt/9mbn'
#endif
#ifdef __linux__
	   if(tapedev(1:1).eq.'0') tapedev='/dev/nst0'
	   if(tapedev(1:1).eq.'1') tapedev='/dev/nst1'
	   if(tapedev(1:1).eq.'2') tapedev='/dev/nst2'
	   if(tapedev(1:1).eq.'3') tapedev='/dev/nst3'
	   if(tapedev(1:1).eq.'4') tapedev='/dev/nst4'
	   if(tapedev(1:1).eq.'5') tapedev='/dev/nst5'
	   if(tapedev(1:1).eq.'6') tapedev='/dev/nst6'
	   if(tapedev(1:1).eq.'7') tapedev='/dev/nst7'
	   if(tapedev(1:1).eq.'8') tapedev='/dev/nst8'
	   if(tapedev(1:1).eq.'9') tapedev='/dev/nst9'
#endif
#ifdef __APPLE__
	   if(tapedev(1:1).eq.'0') tapedev='/dev/nrst0'
	   if(tapedev(1:1).eq.'1') tapedev='/dev/nrst1'
	   if(tapedev(1:1).eq.'2') tapedev='/dev/nrst2'
	   if(tapedev(1:1).eq.'3') tapedev='/dev/nrst3'
	   if(tapedev(1:1).eq.'4') tapedev='/dev/nrst4'
	   if(tapedev(1:1).eq.'5') tapedev='/dev/nrst5'
	   if(tapedev(1:1).eq.'6') tapedev='/dev/nrst6'
	   if(tapedev(1:1).eq.'7') tapedev='/dev/nrst7'
	   if(tapedev(1:1).eq.'8') tapedev='/dev/nrst8'
	   if(tapedev(1:1).eq.'9') tapedev='/dev/nrst9'
#endif

	endif

	RETURN

	END

	SUBROUTINE TAPE_SKIPREC(MTCH,NRECORD)

	INTEGER*4 MTCH,ISTAT,NSKIP,NTOT,NRECORD,IERR

	NTOT=0
	DOWHILE(NRECORD.NE.0)
	  IF(NRECORD.GT.32767) THEN
	    NSKIP=30000
	  ELSEIF(NRECORD.LT.-32768) THEN
	    NSKIP=-30000
	  ELSE
	    NSKIP=NRECORD
	  ENDIF

	  IF (NSKIP.LT.0) THEN 
	    CALL MT_BR(MTCH,-NSKIP,ISTAT)
	    IF(ISTAT.EQ.-1) THEN
	      CALL TAPE_ERRMES(IERR)
	      RETURN
	    ENDIF
	  ELSE
	    CALL MT_FR(MTCH,NSKIP,ISTAT)
	    IF(ISTAT.EQ.-1) THEN
	      CALL TAPE_ERRMES(IERR)
	      RETURN
	    ENDIF
	  ENDIF
	
	  IF(NTOT.EQ.0) THEN
	    NTOT=NTOT+NSKIP
	    IF (NSKIP.GT.0) THEN 
	      WRITE(6,'(a3,I10,''   Record(s) skipped forward'')')char(27)//char(91)//char(65), NTOT
	    ELSE
	      WRITE(6,'(a3,I10,''   Record(s) skipped backward'')')char(27)//char(91)//char(65), -NTOT
	    ENDIF
	  ELSE
	    NTOT=NTOT+NSKIP
	    IF (NSKIP.GT.0) THEN 
	      WRITE(6,'(a3,I10,''   Record(s) skipped forward'')')char(27)//char(91)//char(65), NTOT
	    ELSE
	      WRITE(6,'(a3,I10,''   Record(s) skipped backward'')')char(27)//char(91)//char(65), -NTOT
	    ENDIF
	  ENDIF
	  NRECORD=NRECORD-NSKIP
	ENDDO

	RETURN

	END

	SUBROUTINE TAPE_SKIPFIL(MTCH,NFILE)

	INTEGER*4 MTCH,ISTAT,JFILE,IERR

	IF(NFILE.EQ.0) RETURN

	WRITE(6,*)
 	JFILE=1

	DO II=1,ABS(NFILE)
	  IF(NFILE.GT.0) THEN
	    CALL MT_FF(MTCH,JFILE,ISTAT)
	    IF(ISTAT.EQ.-1) GOTO 1
	    WRITE(6,'(a3,I7,''   File(s) skipped forward'')')char(27)//char(91)//char(65), II*JFILE
	  ELSE
	    CALL MT_BF(MTCH,JFILE,ISTAT)
	    IF(ISTAT.EQ.-1) GOTO 1
	    WRITE(6,'(a3,I7,''   File(s) skipped backward'')')char(27)//char(91)//char(65), II*JFILE
	  ENDIF
	ENDDO
	RETURN

1	WRITE(6,*)
	CALL TAPE_ERRMES(IERR)

	RETURN

	END

	SUBROUTINE TAPE_WRITEOF(MTCH)

	INTEGER*4 MTCH,ISTAT,IERR

	CALL MT_WEOF(MTCH,ISTAT)
	IF(ISTAT.EQ.-1) THEN
	  CALL TAPE_ERRMES(IERR)
	  RETURN
	ENDIF
	
	RETURN

	END

	SUBROUTINE TAPE_REWIND(MTCH)

	INTEGER*4 MTCH,ISTAT,IERR

	CALL MT_REW(MTCH,ISTAT)
	IF(ISTAT.EQ.-1) THEN
	  CALL TAPE_ERRMES(IERR)
	  RETURN
	ENDIF
	
	RETURN

	END

	SUBROUTINE TAPE_DISMOUNT(TAPEDEV,MTCH,IUNL)

	CHARACTER TAPEDEV*(*)
	INTEGER*4 MTCH,ISTAT,IERR

	if(iunl.eq.0) then
	  CALL MT_REW(MTCH,ISTAT)
	  IF(ISTAT.EQ.-1) THEN
	    CALL TAPE_ERRMES(IERR)
*	    RETURN
	  ENDIF
	  CALL MT_CLOSE(MTCH)
	  MTCH=0
	elseif(iunl.eq.9) then
	  CALL MT_CLOSE(MTCH)
	  MTCH=0
	else
	  CALL MT_REWUL(MTCH,ISTAT)
	  IF(ISTAT.EQ.-1) THEN
*	    CALL TAPE_ERRMES(IERR)
*	    RETURN
	    write(6,*)' >>>> ERROR - tape not unloaded'
	    write(6,*)' >>>> Unload by hand and after press <CR> to continue'
	  ENDIF
	  CALL MT_CLOSE(MTCH)
	  MTCH=0
	endif

	RETURN

	END

	SUBROUTINE TAPE_MOUNTFOR(TAPEDEV,MTCH,MSTATUS)

	CHARACTER TAPEDEV*(*)
	INTEGER*4 MTCH,S_ERR,H_ERR,ISTAT,CSTAT
	INTEGER*4 TAPENAM(10)
	INTEGER*4 STATUS
	logical MSTATUS

	INTEGER IL

	MSTATUS=.FALSE.
	
	IF (MTCH.GT.0) THEN
	  IL=MAX(1,LENGTHC(TAPEDEV))
	  WRITE(6,'(1x,A)') TAPEDEV(1:IL)//'  is already opened'
	  MSTATUS=.TRUE.
	  RETURN
	ENDIF
		    
	IL=MAX(1,LENGTHC(TAPEDEV))
	IF(IL.LE.0) THEN
	  CALL INP_STR('Please input name of tape unit ',TAPEDEV)
	  IL=MAX(1,LENGTHC(TAPEDEV))
	ENDIF
	
	CALL FCASEFIX(TAPEDEV)
	IL=MAX(1,LENGTHC(TAPEDEV))
	CALL CHAR_TO_BYTES(TAPEDEV,TAPENAM,IL)
	
	CALL MT_OPENRW(TAPENAM,MTCH)
	IF(MTCH.LE.0) THEN
	  CALL MT_OPENRO(TAPENAM,MTCH)
	  IF(MTCH.LE.0) THEN
	    CALL TAPE_ERRMES(STATUS)
	    RETURN 
	  ENDIF
#ifdef sun
	  WRITE(6,'(4x,A)') TAPEDEV(1:IL)//'  opened for READONLY'
	else
	  WRITE(6,'(4x,A)') TAPEDEV(1:IL)//'  opened for READ/WRITE'
#endif
	ENDIF
#if defined(__linux__) || defined(__APPLE__)	
*	call mt_setblk(MTCH,32768,ierr)
	call mt_setblk(MTCH,0,ierr)
*	WRITE(6,'(1x,A)')' Block size for '//TAPEDEV(1:IL)//'  set to 32768 bytes'	
#endif	
*#ifdef sun	
*	call mt_setblk(MTCH,32768,ierr)
*#endif	
	CALL TAPE_STATUS(MTCH,S_ERR,H_ERR,ISTAT,CSTAT,IERR)
#ifdef Digital
	IF(ISTAT.EQ.0) THEN
	  WRITE(6,'(1x,A)') TAPEDEV(1:IL)//'  opened for READ/WRITE'
	  MSTATUS=.TRUE.
	ELSEIF(ISTAT.EQ.8) THEN
	  WRITE(6,'(1x,A)') TAPEDEV(1:IL)//'  opened for READONLY'
	  MSTATUS=.TRUE.
	ELSEIF(ISTAT.EQ.9) THEN
	  WRITE(6,'(1x,A)') TAPEDEV(1:IL)//'  opened for READONLY (BOT)'
	  MSTATUS=.TRUE.
	ELSEIF(ISTAT.EQ.1) THEN
	  WRITE(6,'(1x,A)') TAPEDEV(1:IL)//'  opened for READ/WRITE (BOT)'
	  MSTATUS=.TRUE.
	ENDIF
#endif
#ifdef sun
	if( istat .eq. 0 ) then
	  if( s_err+h_err .eq. 0 )then
	    WRITE(6,'(4x,A)') TAPEDEV(1:IL)//'  opened at BeginOfTape'
	  else
	    WRITE(6,'(4x,A,i3,a,i12)') TAPEDEV(1:IL)//'  opened at file# ',s_err,' record# ',h_err
	  endif	  
	  MSTATUS=.TRUE.
	endif
#endif
#ifdef __linux__
	IF(ISTAT.EQ.0) THEN
	  WRITE(6,'(1x,A)') TAPEDEV(1:IL)//'  opened for READ/WRITE'
	  MSTATUS=.TRUE.
	ELSEIF(ISTAT.EQ.8) THEN
	  WRITE(6,'(1x,A)') TAPEDEV(1:IL)//'  opened for READONLY'
	  MSTATUS=.TRUE.
	ELSEIF(ISTAT.EQ.9) THEN
	  WRITE(6,'(1x,A)') TAPEDEV(1:IL)//'  opened for READONLY (BOT)'
	  MSTATUS=.TRUE.
	ELSEIF(ISTAT.EQ.1) THEN
	  WRITE(6,'(1x,A)') TAPEDEV(1:IL)//'  opened for READ/WRITE (BOT)'
	  MSTATUS=.TRUE.
	ENDIF
#endif

#ifdef __APPLE__
	IF(ISTAT.EQ.0) THEN
	  WRITE(6,'(1x,A)') TAPEDEV(1:IL)//'  opened'
	  MSTATUS=.TRUE.
	ENDIF
#endif
	RETURN

	END
		
	INTEGER FUNCTION TAPE_READ(IBUF,NBYTE,MTCH,MTFLAG,IOSB)

	INTEGER*4 MTCH,IERR
	
	INTEGER*4 IOSB(4)
	INTEGER*2 IBUF(NBYTE/2)

	INTEGER*4 NSEEN,NBYTES,ISTAT
	
	IOSB(1)=0
	IOSB(2)=0
	IOSB(3)=0
	IOSB(4)=0

	NBYTES=NBYTE
	
        CALL MT_READ(MTCH,IBUF,NBYTES,NSEEN,ISTAT)

        IOSB(1)=ISTAT
	IOSB(2)=NSEEN
	IF(ISTAT.EQ.0) THEN	! read is OK
	  TAPE_READ=1
	ELSE
	  TAPE_READ=0
	ENDIF
 
	RETURN
	
	END

	SUBROUTINE TAPE_SYNCR(MTFLAG,IREC,NSEEN,IOSB,TAPEDEV,MTCH,IGO)

C	AL RITORNO	NSEEN >0  NUMERO DI BYTES LETTI 
C			NSEEN  0  DOPO ERRORE CONTINUA
C			NSEEN -1  ERRORE NON CONTINUARE
C			NSEEN -2  EOF
C			NSEEN -3  EOT
 
	INTEGER*4 mtflag,irec,IOSB(4)
	CHARACTER TAPEDEV*(*)
	INTEGER*4 MTCH,NSEEN,IERR,ISTAT

	INTEGER NEOF		! Conta gli EOF per trovare logical*1 EOT
	DATA NEOF /0/
	save NEOF

	logical*1 INP_YES
	external inp_yes

	NSEEN=IOSB(2)

	IF(IOSB(1).NE.0) CALL SEGNALA(IOSB(1))

	IF(IOSB(1).EQ.0) THEN	! read is OK
	  if(IOSB(2).GT.0) THEN
	    NEOF=0
	    RETURN
	  ENDIF
	ENDIF

	IF(IOSB(2).EQ.0 .OR. (IOSB(1).eq.28 .AND. iosb(2).eq.-1) )then
	     NEOF=NEOF+1
	     iosb(2) = 0
	endif

	IF(NEOF.GE.2) THEN
	  WRITE(6,'(/,''  : EOT  reading record #'',I8/)') IREC+1
	  CALL ANSI_BELL(6)
	  NSEEN=-3
	  NEOF=0
*	  call mt_cse(mtch,ierr)
	  RETURN
	ENDIF

	IF(IOSB(2).EQ.0) THEN
	  WRITE(6,'(/,''  : EOF  reading record #'',I8)') IREC+1
	  CALL ANSI_BELL(6)
	  NSEEN=-2
*	  call mt_cse(mtch,ierr)
	  IF(IGO.EQ.0) THEN
	    IF(INP_YES('Continue reading this tape')) then
	    	NSEEN=0
	    else
	  	NSEEN=-3
	  	NEOF=0
*	  	call mt_cse(mtch,ierr)
	    endif
	  ENDIF
	  RETURN
	ENDIF

	NSEEN=-1		! ERRORE
	NEOF=0
	IREC=IREC+1
	WRITE(6,'(/,''  : Error reading record #'',I8)')IREC
	CALL ANSI_BELL(6)
*	call mt_cse(mtch,ierr)
	CALL TAPE_CONTROL(TAPEDEV,MTCH)
	
	IF(INP_YES('Continue reading this tape')) then
	    NSEEN=0
	else
	    NSEEN=-3
	    NEOF=0
*	    call mt_cse(mtch,ierr)
	endif

	RETURN

	END

	INTEGER FUNCTION TAPE_WRITE(IBUF,NBYTE,MTCH,MTFLAG,IOSB)

	INTEGER*4 MTCH
	INTEGER*2 IBUF(NBYTE/2)		! BUFFER LETTURA NASTRO
	INTEGER*4 IOSB(4)

	INTEGER*4 NSEEN,NBYTES,ISTAT

	IOSB(1)=0
	IOSB(2)=0
	IOSB(3)=0
	IOSB(4)=0
	
	NBYTES=NBYTE
	
        CALL MT_WRITE(MTCH,IBUF,NBYTES,NSEEN,ISTAT)
        
        IOSB(1)=ISTAT
	IOSB(2)=NSEEN
	IF(ISTAT.EQ.0) THEN	! write is OK
	  TAPE_WRITE=1
	ELSE
	  TAPE_WRITE=0
	  CALL SEGNALA(ISTAT)
	ENDIF
	
	RETURN

	END

	SUBROUTINE TAPE_SYNCW(MTFLAG,IREC,IFLAG,IOSB,TAPEDEV,MTCH,IGO)

C	AL RITORNO	IFLAG  1  OK
C			IFLAG  0  DOPO ERRORE CONTINUA
C			IFLAG -1  ERRORE NON CONTINUARE
C			IFLAG -3  EOT

	INTEGER*4 IOSB(4)
	CHARACTER TAPEDEV*(*)
	INTEGER*4 MTCH,NSEEN,IERR,ISTAT

	logical*1 INP_NOT

	NSEEN=IOSB(2)
	ISTAT=IOSB(1)

	IF(ISTAT.NE.0) CALL SEGNALA(ISTAT)

	IF(ISTAT.EQ.0) THEN	! write is OK
	  if(nseen.gt.0) then
	    IFLAG=1
	    RETURN
	  endif
	ENDIF

	IF(NSEEN.EQ.0) THEN
	  WRITE(6,'(''  : EOT  writing record #'',I8/)') IREC+1
	  CALL ANSI_BELL(6)
	  IFLAG=-3
	  RETURN
	ENDIF

	IFLAG=-1
	IREC=IREC+1
	IF(NSEEN.EQ.-1) THEN
	  WRITE(6,*) '  : Error writing record #',IREC
	  CALL ANSI_BELL(6)
	  CALL SEGNALA(ISTAT)
	ENDIF

	IF(IGO.EQ.0) THEN
	  CALL TAPE_CONTROL(TAPEDEV,MTCH)
	  IF(INP_NOT('Continue writing this tape')) IFLAG=0
	ENDIF

	RETURN

	END

	SUBROUTINE TAPE_LIST(BUFFER,Nchars)

!  Dump di Nchars caratteri del buffer (si usa di solito per i records
!  di testa)

	BYTE BUFFER(Nchars)		! BUFFER LETTURA NASTRO
	CHARACTER CODE*64,CH*1

	DO JJ=1,Nchars,64
	  ICO=0
	  DO II=JJ,JJ+63
	    ICO=ICO+1
	    CH=CHAR(BUFFER(II))
	    IF(CH.LT.' ' .OR. CH.GT.'~') CH='.'
	    CODE(ICO:ICO)=CH
	  ENDDO
	  WRITE(6,'(5X,A)') CODE
	ENDDO
	WRITE(6,*)
	RETURN

	END


	SUBROUTINE TAPE_STATUS(MTCH,S_ERR,H_ERR,STAT,CSTAT,IERR)

	INTEGER*4 MTCH
	INTEGER*4 S_ERR,H_ERR,STAT,CSTAT,IERR,ISTAT
*	CHARACTER DEVNAM*(*)
	
	CALL dev_getSTATUS(MTCH,S_ERR,H_ERR,STAT,CSTAT)
	IF(STAT.EQ.-1) THEN
	  CALL TAPE_ERRMES(IERR)
	  RETURN
	ENDIF
	
	RETURN 
	
	END	
	
	SUBROUTINE TAPE_ERRMES(IERR)
	
	INTEGER*4 IERR
	
	CALL MT_ERRMES(IERR)
	
	RETURN
	
	END


#ifdef USE_AIO		
	INTEGER FUNCTION TAPE_AIOREAD(IBUF,NBYTE,MTCH,MTFLAG,IOSB)

	INTEGER*4 MTCH,IERR

	INTEGER*4 IOSB(4)
	INTEGER*2 IBUF(NBYTE/2)

	INTEGER*4 NSEEN,NBYTES,ISTAT

	IOSB(1)=0
	IOSB(2)=0
	IOSB(3)=0
	IOSB(4)=0

	NBYTES=NBYTE

        CALL MT_AIOREAD(MTCH,IBUF,NBYTES,NSEEN,ISTAT)

	tape_aioread = 1
	RETURN

	END



	INTEGER FUNCTION TAPE_AIOCOMPLETE(IBUF,NBYTE,MTCH,MTFLAG,IOSB)

	INTEGER*4 MTCH,IERR

	INTEGER*4 IOSB(4)
	INTEGER*2 IBUF(NBYTE/2)

	INTEGER*4 NSEEN,ISTAT

	IOSB(1)=0
	IOSB(2)=0
	IOSB(3)=0
	IOSB(4)=0

        CALL MT_AIOCOMPLETE(mtch,NSEEN,ISTAT)

        IOSB(1)=ISTAT
	IOSB(2)=NSEEN
	IF(ISTAT.EQ.0) THEN	! read is OK
	  TAPE_AIOCOMPLETE=1
	ELSE
	  TAPE_AIOCOMPLETE=0
	ENDIF

	RETURN

	END

#endif

	SUBROUTINE DISK_SYNCR(MTFLAG,IREC,NSEEN,IOSB,TAPEDEV,MTCH,IGO)

C	AL RITORNO	NSEEN >0  NUMERO DI BYTES LETTI 
C			NSEEN  0  DOPO ERRORE CONTINUA
C			NSEEN -1  ERRORE NON CONTINUARE
C			NSEEN -2  EOF
C			NSEEN -3  EOT
 
	INTEGER*4 mtflag,irec,IOSB(4)
	CHARACTER TAPEDEV*(*)
	INTEGER*4 MTCH,NSEEN,IERR,ISTAT

	INTEGER NEOF		! Conta gli EOF per trovare logical*1 EOT
	DATA NEOF /0/
	save NEOF

	logical*1 INP_YES
	external inp_yes

	NSEEN=IOSB(2)

	IF(IOSB(1).NE.0) CALL SEGNALA(IOSB(1))

	IF(IOSB(1).EQ.0) THEN	! read is OK
	  if(IOSB(2).GT.0) THEN
	    NEOF=0
	    RETURN
	  ENDIF
	ENDIF


	IF(IOSB(2).EQ.0) THEN
	  WRITE(6,'(/,''  : EOF  reading record #'',I8)') IREC+1
	  CALL ANSI_BELL(6)
	  NSEEN=-3
	ENDIF

	RETURN

	END
