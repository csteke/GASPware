# 1 "inter_sys.F"
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "inter_sys.F" 2 
	
	SUBROUTINE SPAWN(DCL,ISUC)

C	SPAWN DI UN SOTTOPROCESSO COME DA COMANDO DCL

C	CHARACTER*(*) DCL
C
C	ISUC=-1
C	IL=LENGTHC(DCL)
C	IF (IL.EQ.0) THEN
C		IL=INP_CH('DCL-Command',DCL)
C		IF (IL.LE.0) GOTO 100
C	ENDIF
C???	ISTATUS=Lib$Spawn('$ '//DCL)
C	IF (.NOT.ISTATUS) THEN
C		TYPE *,' Dcl couldn''t spawn the subprocess '
C		GOTO 100
C	ENDIF
C
C99	ISUC=0
C
100	RETURN
C
	END

	logical*1 function GETMEM(nbytes,ibase)

	integer*4 nbytes

	integer*8 ibase
	integer*8 gw_malloc

	external gw_malloc

	ibase=gw_malloc(nbytes)
	GETMEM=ibase.ne.0

	return

	end

	logical*1 function FREEMEM(nbytes,ibase)

	integer*4 nbytes

	integer*8 ibase

	external gw_free

	call gw_free(ibase)
	FREEMEM=.true.

	return

	end


	SUBROUTINE TIMESTAMP(LUN,string)

	CHARACTER CDATE*9,CTIME*8
	character*(*) string

	CALL DATE(CDATE)
	CALL TIME(CTIME)
	lstring=lengthc(string)
	if(lstring.gt.0) then
	  WRITE(LUN,'(1X,A,2X,A,2X,A)') CDATE,CTIME,string(1:lstring)
	else
	  WRITE(LUN,'(1X,A,2X,A)') CDATE,CTIME
	endif

	return

	end


c	SUBROUTINE CPU_SECONDS(TIME)
c
C	CALCOLA IL TEMPO DI CPU UTILIZZATO (IN SECONDI)
c
c	INTEGER*2 IBUFFER(6)
c	INTEGER*4 LBUFFER(3)
c	EQUIVALENCE(IBUFFER,LBUFFER)
c
c	INTEGER I100
c
c	INCLUDE '($JPIDEF)'
c
c	IBUFFER(1)=4
c	IBUFFER(2)=JPI$_CPUTIM
c	LBUFFER(2)=%LOC(I100)
c	LBUFFER(3)=0
c
c	CALL SYS$GETJPIW(,,,IBUFFER,,,)
c
c	TIME=I100/100.
c
c	END

	SUBROUTINE SEGNALA(ISTAT)

C	MESSAGGIO A SECONDA DEL VALORE DI ISTAT

C???	IF(.NOT.ISTAT) THEN
C	   JSTAT = IAND(ISTAT,'0FFFFFFF8'X)	! Strip off old severity
C	   JSTAT=JSTAT+3			! Convert to informational
C	   CALL LIB$SIGNAL(%VAL(JSTAT))		! Signal and continue
C	   CALL ANSI_BELL(6)
C	ENDIF

	RETURN
	
	END
