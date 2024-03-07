CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C$+									C
C			ROUTINES DI USO GENERALE			C
C$-									C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	FUNCTION LENGTHC(TEXT)

C	CALCOLA LA LUNGHEZZA DELLA STRINGA TEXT ESCLUDENDO I CARATTERI
C		BLANK, TAB E NULL FINALI

	CHARACTER TEXT*(*)

	LT=LEN(TEXT)
	DO II=LT,1,-1
	  IF(TEXT(II:II).NE.' ' .AND.
	1    TEXT(II:II).NE.'	' .AND.
	1    TEXT(II:II).NE.CHAR(0)) THEN
	    LENGTHC=II
	    RETURN
	  ENDIF
	ENDDO

	LENGTHC=0
	RETURN

	END

	FUNCTION LENGTHZ(TEXT)

C	CALCOLA LA LUNGHEZZA DELLA STRINGA TEXT DELIMITATA TIPO ASCIIZ

	byte TEXT(*)

	ii=0
	DOwhile(text(ii+1).ne.0)
	  ii=ii+1
	ENDDO

	LENGTHZ=ii
	RETURN

	END


	INTEGER FUNCTION LENGTHN(BB)

C	DETERMINA LA LUNGHEZZA DEL PROSSIMO TOKEN

	CHARACTER*(*) BB
	CHARACTER*3	SEPARATOR	/' ,	'/

	LCC=0

	CALL KILLBLA(BB)
	LBB=LENGTHC(BB)
	DO LL=1,LBB
	  IF(INDEX(SEPARATOR,BB(LL:LL)).GT.0) GOTO 100
	  LCC=LCC+1
	ENDDO

100	IF(LCC.NE.0) THEN
	  IBB=ICHAR(BB(1:1))
	  IF(IBB.GT.57 .OR. IBB.LT.43) LCC=-LCC
	  IF(IBB.EQ.44 .OR. IBB.EQ.47) LCC=-LCC
	ENDIF
	LENGTHN=LCC

	RETURN

	END

	FUNCTION LENGTHBC(TEXT,LT)

C	CALCOLA LA LUNGHEZZA DEL VETTORE BYTE TEXT ESCLUDENDO I CARATTERI
C		BLANK, TAB E NULL FINALI

#if defined( __GFORTRAN__ )
	CHARACTER*(*) TEXT
#else
	BYTE TEXT(LT)
#endif

	IF(LT.LE.0) THEN
	  LENGTHBC=0
	  RETURN
	ENDIF

#if defined( __GFORTRAN__ )
	II=VERIFY(TEXT(1:LT)," 0	", .TRUE.)
	LENGTHBC=II
	RETURN
#else
	DO II=LT,1,-1
	  IF(TEXT(II).NE.' ' .AND.
	1    TEXT(II).NE.'	' .AND.
	1    TEXT(II).NE.0 ) THEN
	    LENGTHBC=II
	    RETURN
	  ENDIF
	ENDDO
#endif
	LENGTHBC=0
	RETURN

	END

	FUNCTION LENGTHBZ(TEXT,LT)

C	CALCOLA LA LUNGHEZZA DEL VETTORE BYTE TEXT DI TIPO ASCIIZ

	BYTE TEXT(*)

	DO II=1,LT
	  IF(TEXT(II).EQ.0) THEN
	    LENGTHBZ=II-1
	    RETURN
	  ENDIF
	ENDDO

	LENGTHBZ=LT
	RETURN

	END

	integer function ntokens(bb)

c	trova quanti items ci sono nella stringa 

	character*(*) bb
	character*3	separator	/' ,	'/
	integer previous,this
	
	lbb=lengthc(bb)
	nitems=0
	previous=1
	do ll=1,lbb
	  this=index(separator,bb(ll:ll))
	  if(this.eq.2) then		! virgola
	    if(previous.gt.0 .and. previous.le.3) nitems=nitems+1
	  elseif(this.le.0) then	! carattere generico
	    if(previous.gt.0 .and. previous.le.3) nitems=nitems+1
	  endif
	  previous=this
	enddo

	ntokens=nitems

	return

	end

	FUNCTION INDEXB(TEXT,LT,STRING,LS)

C	INDIRIZZO DI STRING IN TEXT  (COME INDEX PER CHARACTER)

	BYTE TEXT(LT),STRING(LS)

	INDEXB=0
	IF(LT.LE.0) GOTO 200
	IF(LS.LE.0.OR.LS.GT.LT) GOTO 200
	DO II=1,LT-LS+1
		IF(TEXT(II).EQ.STRING(1)) THEN
			K=2
			DO JJ=II+1,II+LS-1
				IF(TEXT(JJ).NE.STRING(K)) GO TO 10
				K=K+1
			ENDDO
			INDEXB=II
			GOTO 200
		ENDIF
10	ENDDO

200	RETURN

	END

	SUBROUTINE STR_KILLBLA(TEXT)
	ENTRY KILLBLA(TEXT)

C	TOGLIE I BLANKS E I TAB INIZIALI DALLA STRINGA TEXT

	CHARACTER TEXT*(*)

	LT=LEN(TEXT)
	DO II=1,LT
	  IF(TEXT(II:II).NE.' ' .AND. TEXT(II:II).NE.'	') THEN
	    if(ii.ne.1) TEXT=TEXT(II:)
	    RETURN
	  ENDIF
	ENDDO

	RETURN

	END

	SUBROUTINE STR_TOUPPER(CH)

C	CONVERSIONE A MAIUSCOLE DELLA STRINGA CH

	CHARACTER*(*) CH

	DO II=1,LEN(CH)
	   IF(ICHAR( CH(II:II) ) .GE. ICHAR('a') .AND.
	1     ICHAR( CH(II:II) ) .LE. ICHAR('z') )
	1	CH(II:II)=CHAR( ICHAR( CH(II:II) )-ICHAR('a')+ICHAR('A') )
	ENDDO

	RETURN

	END

	SUBROUTINE STR_TOLOWER(CH)

C	CONVERSIONE A MINUSCOLE DELLA STRINGA CH

	CHARACTER*(*) CH

	DO II=1,LEN(CH)
	   IF(ICHAR( CH(II:II) ) .GE. ICHAR('A') .AND.
	1     ICHAR( CH(II:II) ) .LE. ICHAR('Z') )
	1	CH(II:II)=CHAR( ICHAR( CH(II:II) )-ICHAR('A')+ICHAR('a') )
	ENDDO

	RETURN

	END

	SUBROUTINE STR_INCREM(NAME)

C	INCREMENTA LA STRINGA

	CHARACTER NAME*(*),CH*1

	I1=1
	I2=LENGTHC(NAME)

20	IF(I2.LT.I1) RETURN

	CH=NAME(I2:I2)
	IF(CH.EQ.'9') THEN
		NAME(I2:I2)='0'
		I2=I2-1
		GOTO 20
	ELSE IF(CH.EQ.'Z') THEN
		NAME(I2:I2)='A'
		I2=I2-1
		GOTO 20
	ELSE IF(CH.EQ.'z') THEN
		NAME(I2:I2)='a'
		I2=I2-1
		GOTO 20
	ELSE IF(CH.LE.' ') THEN
		I2=I2-1
		GOTO 20
	ELSE
		NAME(I2:I2)=CHAR(ICHAR(CH)+1)
	ENDIF

	RETURN

	END

	SUBROUTINE STR_DECREM(NAME)

C	DECREMENTA LA STRINGA

	CHARACTER NAME*(*),CH*1

	I1=1
	I2=LENGTHC(NAME)

20	IF(I2.LT.I1) RETURN

	CH=NAME(I2:I2)
	IF(CH.EQ.'0') THEN
		NAME(I2:I2)='9'
		I2=I2-1
		GOTO 20
	ELSE IF(CH.EQ.'A') THEN
		NAME(I2:I2)='Z'
		I2=I2-1
		GOTO 20
	ELSE IF(CH.EQ.'a') THEN
		NAME(I2:I2)='z'
		I2=I2-1
		GOTO 20
	ELSE IF(CH.LE.' ') THEN
		I2=I2-1
		GOTO 20
	ELSE
		NAME(I2:I2)=CHAR(ICHAR(CH)-1)
	ENDIF

	RETURN

	END

	SUBROUTINE STR_TAB2BLANK(BB)

	CHARACTER*(*) BB

	LBB=LENGTHC(BB)
	DO II=1,LBB
	 IF    (BB(II:II) .EQ. CHAR(9)) BB(II:II)=' '	! tab to blank
	ENDDO

	RETURN

	END

	SUBROUTINE STR_TRIM(BB,II)

!	STRIP II CHARACTERS FROM BB AND
!	KILL LEADING BLANKS AND TABS

	CHARACTER*(*) BB
	INTEGER II

	LBB=LEN(BB)

	DO JJ=II+1,LBB
	  IF(BB(JJ:JJ).NE.' ' .and. BB(JJ:JJ).NE.CHAR(9)) THEN
	    BB=BB(JJ:)
	    RETURN
	  ENDIF
	ENDDO
	BB=' '

	RETURN

	END

	subroutine str_toint(CH,base,ival,npos)

C	decodifica un intero dalla parte numerica della stringa
C	se la base (2...10)   e' positiva calcola in avanti
C	se la base e' negativa parte dalla fine e va a rovescio

	character*(*) ch
	integer base,ival,npos

	ival=0
	npos=0

	ib=abs(base)
	if(ib.lt.2 .or .ib.gt.10) return

	ich0=ichar('0')
	ichb=ich0+ib-1

	if(base.gt.0) then
	  do i=1,len(ch)
	    ichi=ichar(ch(i:i))
	    if(ichi.ge.ich0 .AND. ichi.le.ichb) then
	      ival=ival*ib + ichi-ich0
	      npos=npos+1
	    else
	      return
	    endif
	  enddo
	else
	  if=1
	  do i=len(ch),1,-1
	    ichi=ichar(ch(i:i))
	    if(ichi.ge.ich0 .AND. ichi.le.ichb) then
	      ival=(ichi-ich0)*if + ival
	      if=if*ib
	      npos=npos+1
	    else
	      return
	    endif
	  enddo
	endif

	return

	end

	subroutine filetype(file,type)

	character*(*) file,type

*	ldot=index(file,'.')
	ltype=lengthc(type)
*	if(ldot.le.0) then
	lfile=lengthc(file)
	if(lfile .le. ltype)file=file(1:lfile)//'.'//type(1:ltype)
	if(file(lfile-ltype+1:lfile) .ne. type(1:ltype))file=file(1:lfile)//'.'//type(1:ltype)
*	  file=file(1:lfile)//'.'//type
*	endif

	return

	end

	subroutine str_c2f(cc,cf)

	character*(*) cc,cf

	lcc=lengthz(cc)
	do ii=1,lcc
	  cf(ii:ii)=cc(ii:ii)
	enddo

	return

	end

	subroutine str_f2c(cf,cc)

	character*(*) cf,cc

	lcf=len(cf)
	do ii=1,lcf
	  cc(ii:ii)=cf(ii:ii)
	enddo

	return

	end
