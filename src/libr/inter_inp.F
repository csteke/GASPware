CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C$+									C
C			   Package INP_...				C
C$-									C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C$+   Package INP_...
C
C  E' essenzialmente una riscrittura del pacchetto INTER
C  (scritto da  Walter Mueller a Colonia nel lontano 1978 circa)
C  togliendo tutti i riferimenti alle variabili tipo BYTE
C  e introducendo la possibilita' di pipelining in input
C
C  Da utilizzare per I/O interattivo in programmi FORTRAN
C
C  Il primo parametro nella chiamata e' (quando ha senso) una stringa
C  di prompt seguita dagli altri eventuali parametri
C
C  Se una variabile di input non viene battuta il suo valore precedente viene
C  mantenuto
C
C  In generale (esclusa la INP_QUOTE) rispondendo alla richiesta di input con
C  ? vengono listati i valori attuali delle variabili'
C
C  Pipelining:	MODE=0  escluso
C		MODE=1	abilitato  con  prompts su LUOUT
C		MODE=2	   "      senza      "
C
C ROUTINES implementate
C
C  INP_YES	Funzione logica per porre domande <CR>=.TRUE.
C  INP_NOT	              "                   <CR>=.FALSE.
C  INP_ASK	Subroutine    "   (seleziona tra INP_YES e INP_NOT)
C  INP_MSG	Manda un messaggio su LUOUT
C  INP_I1...8	Input 1...8 interi
C  INP_IA	Input vettore di interi
C  INP_R1...8	Input 1...8  reali
C  INP_RA	Input vettore di reali
C  INP_D1...8	Input 1...8  reali doppia precisione
C  INP_DA	Input vettore di reali doppia precisione
C  INP_CH	Input una stringa contigua di caratteri
C  INP_STR	Input una riga di caratteri
C  INP_SPNAME	Input di nome, formato e lunghezza di uno spettro
C  INP_CMD	Input un comando
C  INP_CMDH	Input un comando con facilita di HISTORY RECALL
C  INP_CMDIA	Input un comando e un vettore di interi
C  INP_CMDRA	Input un comando e un vettore di reali
C  INP_CMDDA	Input un comando e un vettore di reali doppia precisione
C Le routines elencate fino a questo punto si possono chiamare anche come
C	 FUNCTION nel qual caso ritornano il valore di IIC
C
C  INP_SETLUN	Per definire LUIN e LUOUT (default 5,6)
C  INP_SHOWLUN	Mostra LUIN e LUOUT
C  INP_HOWMANY	Quanti valori sono stati convertiti nell'ultima chiamata
C  INP_SHOWBUFF	Quanti caratteri ancora da decodificare (<0 se il primo e' alfa)
C  INP_SETBUFF	Setta il buffer come se fosse letto da  LUIN
C  INP_GETBUFF	Ritorna il contenuto del buffer
C  INP_MODE	Setta il modo di pipelining
C
C  Le altre INP_000_... sono usate internamente
C
C
C  Significato delle variabili nel COMMON INP_COM
C
C	CHARACTER*256 BB,CC
C	DOUBLE PRECISION DDD
C	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
C*	DATA MODE,LUIN,LUOUT/0,5,6/
C
C  BB		buffer di lettura
C  LBB		# di caratteri ancora in BB
C  CC		buffer di lavoro
C  LCC		# di caratteri in CC
C  IIC		quanti items sono stati battuti (-1 = CTRL_Z)
C		        "  caratteri se di tipo stringa
C  IFLAG	flag : 1=O.K.  ;  0=finito	;  -1=errore
C  DDD		valore del numero ottenuto (doppia precisione)
C  LUIN		Input  LU
C  LUOUT	Output LU (Se LUOUT=0 i prompts non vengono presentati)
C
C$-

	LOGICAL*1	FUNCTION INP_YES(TEXT)

	CHARACTER*(*)	TEXT

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,' [Y] ? ')
	CALL INP_000_LIN
	IOK=1
	IF(LBB.GT.0) THEN
	   CALL INP_000_LOG(IOK)
	   IF(IFLAG.LT.0) GOTO 10
	ENDIF
	INP_YES=IOK.EQ.1

	RETURN

	END

	LOGICAL*1	FUNCTION INP_NOT(TEXT)

	CHARACTER*(*)	TEXT

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,' [N] ? ')
	CALL INP_000_LIN
	IOK=0
	IF(LBB.GT.0) THEN
	   CALL INP_000_LOG(IOK)
	   IF(IFLAG.LT.0) GOTO 10
	ENDIF
	INP_NOT=IOK.EQ.1

	RETURN

	END

	LOGICAL	FUNCTION INP_ASK(TEXT,LVAL)

	CHARACTER*(*)	TEXT
	logical*1 LVAL, inp_yes, inp_not
	external inp_yes, inp_not

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	IF(LVAL) THEN
	   LVAL=INP_YES(TEXT)
	ELSE
	   LVAL=INP_NOT(TEXT)
	ENDIF
	INP_ASK=IIC

	RETURN

	END

	FUNCTION INP_MSG(TEXT)

	CHARACTER*(*)	TEXT

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	II=LEN(TEXT)
	IZ=LENGTHZ(TEXT)
	IF(IZ.LT.II) II=IZ
	IF(II.LE.0 ) GOTO 100

	IF(LUOUT.GT.0) WRITE(LUOUT,'(1X,A,$)') TEXT(1:II)

100	INP_MSG=II
	RETURN

	END

	FUNCTION INP_IA(TEXT,IA,N)

	CHARACTER*(*)	TEXT
	INTEGER	IA(N)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,' : ')
	CALL INP_000_LIN
	IF(IIC.EQ.-1) GOTO 100
	CALL INP_000_INT(IA,N)
	IF(IFLAG.LT.0) GOTO 10

100	INP_IA=IIC
	RETURN

	END

	FUNCTION INP_I1(TEXT,II1)

	CHARACTER*(*)	TEXT

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	INTEGER II(8)

	NII=1
	ASSIGN 101 TO IGO
	GOTO 1

	ENTRY 	INP_I2(TEXT,II1,II2)
	NII=2
	ASSIGN 102 TO IGO
	GOTO 2

	ENTRY 	INP_I3(TEXT,II1,II2,II3)
	NII=3
	ASSIGN 103 TO IGO
	GOTO 3

	ENTRY 	INP_I4(TEXT,II1,II2,II3,II4)
	NII=4
	ASSIGN 104 TO IGO
	GOTO 4

	ENTRY 	INP_I5(TEXT,II1,II2,II3,II4,II5)
	NII=5
	ASSIGN 105 TO IGO
	GOTO 5

	ENTRY 	INP_I6(TEXT,II1,II2,II3,II4,II5,II6)
	NII=6
	ASSIGN 106 TO IGO
	GOTO 6

	ENTRY 	INP_I7(TEXT,II1,II2,II3,II4,II5,II6,II7)
	NII=7
	ASSIGN 107 TO IGO
	GOTO 7

	ENTRY 	INP_I8(TEXT,II1,II2,II3,II4,II5,II6,II7,II8)
	NII=8
	ASSIGN 108 TO IGO
	GOTO 8

8	II(8)=II8
7	II(7)=II7
6	II(6)=II6
5	II(5)=II5
4	II(4)=II4
3	II(3)=II3
2	II(2)=II2
1	II(1)=II1

#if defined( __GFORTRAN__ )
	idmmy = INP_IA(TEXT,II,NII)
#else
	CALL INP_IA(TEXT,II,NII)
#endif
	GOTO IGO

108	II8=II(8)
107	II7=II(7)
106	II6=II(6)
105	II5=II(5)
104	II4=II(4)
103	II3=II(3)
102	II2=II(2)
101	II1=II(1)

	INP_I1=IIC
	RETURN

	END

	FUNCTION INP_RA(TEXT,RA,N)

	CHARACTER*(*)	TEXT
	real*4 RA(N)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,' : ')
	CALL INP_000_LIN
	IF(IIC.EQ.-1) GOTO 100
	CALL INP_000_REA(RA,N)
	IF(IFLAG.LT.0) GOTO 10

100	INP_RA=IIC
	RETURN

	END

	FUNCTION INP_R1(TEXT,RR1)

	CHARACTER*(*)	TEXT

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	real*4 RR(8), RR1,RR2,RR3,RR4,RR5,RR6,RR7,RR8

	NRR=1
	ASSIGN 101 TO IGO
	GOTO 1

	ENTRY 	INP_R2(TEXT,RR1,RR2)
	NRR=2
	ASSIGN 102 TO IGO
	GOTO 2

	ENTRY 	INP_R3(TEXT,RR1,RR2,RR3)
	NRR=3
	ASSIGN 103 TO IGO
	GOTO 3

	ENTRY 	INP_R4(TEXT,RR1,RR2,RR3,RR4)
	NRR=4
	ASSIGN 104 TO IGO
	GOTO 4

	ENTRY 	INP_R5(TEXT,RR1,RR2,RR3,RR4,RR5)
	NRR=5
	ASSIGN 105 TO IGO
	GOTO 5

	ENTRY 	INP_R6(TEXT,RR1,RR2,RR3,RR4,RR5,RR6)
	NRR=6
	ASSIGN 106 TO IGO
	GOTO 6

	ENTRY 	INP_R7(TEXT,RR1,RR2,RR3,RR4,RR5,RR6,RR7)
	NRR=7
	ASSIGN 107 TO IGO
	GOTO 7

	ENTRY 	INP_R8(TEXT,RR1,RR2,RR3,RR4,RR5,RR6,RR7,RR8)
	NRR=8
	ASSIGN 108 TO IGO
	GOTO 8

8	RR(8)=RR8
7	RR(7)=RR7
6	RR(6)=RR6
5	RR(5)=RR5
4	RR(4)=RR4
3	RR(3)=RR3
2	RR(2)=RR2
1	RR(1)=RR1

#if defined( __GFORTRAN__ )
	idmmy = INP_RA(TEXT,RR,NRR)
#else
	CALL INP_RA(TEXT,RR,NRR)
#endif
	GOTO IGO

108	RR8=RR(8)
107	RR7=RR(7)
106	RR6=RR(6)
105	RR5=RR(5)
104	RR4=RR(4)
103	RR3=RR(3)
102	RR2=RR(2)
101	RR1=RR(1)

	INP_R1=IIC
	RETURN

	END

	FUNCTION INP_DA(TEXT,DA,N)

	CHARACTER*(*)	TEXT
	DOUBLE PRECISION DA(N)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,' : ')
	CALL INP_000_LIN
	IF(IIC.EQ.-1) GOTO 100
	CALL INP_000_DEA(DA,N)
	IF(IFLAG.LT.0) GOTO 10

100	INP_DA=IIC
	RETURN

	END

	FUNCTION INP_D1(TEXT,DD1)

	CHARACTER*(*)	TEXT

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	DOUBLE PRECISION DD1,DD2,DD3,DD4,DD5,DD6,DD7,DD8
	DOUBLE PRECISION  DD(8)

	NDD=1
	ASSIGN 101 TO IGO
	GOTO 1

	ENTRY 	INP_D2(TEXT,DD1,DD2)
	NDD=2
	ASSIGN 102 TO IGO
	GOTO 2

	ENTRY 	INP_D3(TEXT,DD1,DD2,DD3)
	NDD=3
	ASSIGN 103 TO IGO
	GOTO 3

	ENTRY 	INP_D4(TEXT,DD1,DD2,DD3,DD4)
	NDD=4
	ASSIGN 104 TO IGO
	GOTO 4

	ENTRY 	INP_D5(TEXT,DD1,DD2,DD3,DD4,DD5)
	NDD=5
	ASSIGN 105 TO IGO
	GOTO 5

	ENTRY 	INP_D6(TEXT,DD1,DD2,DD3,DD4,DD5,DD6)
	NDD=6
	ASSIGN 106 TO IGO
	GOTO 6

	ENTRY 	INP_D7(TEXT,DD1,DD2,DD3,DD4,DD5,DD6,DD7)
	NDD=7
	ASSIGN 107 TO IGO
	GOTO 7

	ENTRY 	INP_D8(TEXT,DD1,DD2,DD3,DD4,DD5,DD6,DD7,DD8)
	NDD=8
	ASSIGN 108 TO IGO
	GOTO 8

8	DD(8)=DD8
7	DD(7)=DD7
6	DD(6)=DD6
5	DD(5)=DD5
4	DD(4)=DD4
3	DD(3)=DD3
2	DD(2)=DD2
1	DD(1)=DD1

#if defined( __GFORTRAN__ )
	idmmy = INP_DA(TEXT,DD,NDD)
#else
	CALL INP_DA(TEXT,DD,NDD)
#endif
	GOTO IGO

108	DD8=DD(8)
107	DD7=DD(7)
106	DD6=DD(6)
105	DD5=DD(5)
104	DD4=DD(4)
103	DD3=DD(3)
102	DD2=DD(2)
101	DD1=DD(1)

	INP_D1=IIC
	RETURN

	END

	FUNCTION INP_CH(TEXT,CH)

	CHARACTER*(*)	TEXT
	CHARACTER*(*)	CH

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,'  ')
	CALL INP_000_LIN
	IF(IIC.EQ.-1) GOTO 100
	CALL INP_000_GET
	IF(LCC.EQ.1 .AND. CC(1:1).EQ.'?') THEN
		IF(LUOUT.GT.0) WRITE(LUOUT,'(1X,A)') CH
		LBB=0
		GOTO 10
	ENDIF
	IIC=0
	IF(LCC.GT.0) THEN
		IIC=MIN(LEN(CH),LCC)
		CH=CC(1:IIC)
	ENDIF	

100	INP_CH=IIC
	RETURN

	END

	FUNCTION INP_STR(TEXT,CH)

	CHARACTER*(*)	TEXT
	CHARACTER*(*)	CH

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,'  ')
	CALL INP_000_LIN
	IF(IIC.EQ.-1) GOTO 100
	IF(LBB.EQ.1 .AND. BB(1:1).EQ.'?') THEN
		IF(LUOUT.GT.0) WRITE(LUOUT,'(1X,A)') CH
		LBB=0
		GOTO 10
	ENDIF
	IIC=0
	IF(LBB.GT.0) THEN
		IIC=MIN(LEN(CH),LBB)
		CH=BB(1:IIC)
		LBB=0
	ENDIF	

100	INP_STR=IIC
	RETURN

	END

	FUNCTION INP_WORD(TEXT,CH)

	CHARACTER*(*)	TEXT
	CHARACTER*(*)	CH

	CHARACTER*256 WWTMP

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,'  ')
	CALL INP_000_LIN
	IF(IIC.EQ.-1) GOTO 100
	IF(LBB.EQ.1 .AND. BB(1:1).EQ.'?') THEN
		IF(LUOUT.GT.0) WRITE(LUOUT,'(1X,A)') CH
		LBB=0
		GOTO 10
	ENDIF
	IIC=0
	IF(LBB.GT.0) THEN
		IIC=MIN(LEN(CH),LBB)
		if( index(BB(1:LBB), ' ').gt.0 ) IIC = MIN( IIC, index(BB(1:LBB), ' ')-1 )
		if( index(BB(1:LBB), ',').gt.0 ) IIC = MIN( IIC, index(BB(1:LBB), ',')-1 )
		CH=BB(1:IIC)
		if( IIC .lt. LBB .and. IIC .lt. LEN(CH) )then
		     IIC = IIC+1
		     do while ( BB(IIC:IIC) .eq. ' ' .or. BB(IIC:IIC) .eq. ',')
		       IIC = IIC+1
		     enddo
		     WWTMP = BB(IIC:LBB)
		     BB = WWTMP
		     LBB=LBB-IIC+1
		     if( LBB .lt. 0 )LBB = 0
		else
		     LBB = 0
		endif
	ENDIF	

100	INP_WORD=IIC
	RETURN

	END

	subroutine INP_WORD_RESET


	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	LBB = 0
	
	end
	

	FUNCTION INP_SPNAME(TEXT,FILENAME,FORM,NKAP)

	CHARACTER*(*)	TEXT
	CHARACTER*(*)	FILENAME,FORM

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,'  ')
	CALL INP_000_LIN
	IF(IIC.EQ.-1) GOTO 100
	CALL INP_000_SPN(FILENAME,FORM,NKAP)
	IF(IFLAG.LT.0) GOTO 10

100	INP_SPNAME=IIC
	RETURN

	END

	FUNCTION INP_CMD(TEXT,COM,NCOM,NC)

	CHARACTER*(*) TEXT
	CHARACTER*(*) COM(NCOM)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,'> ')
	CALL INP_000_LIN
	IF(IIC.EQ.-1) GOTO 100
	IIC = 0
!	IF(LENGTHC(BB).LT.1) GOTO 10
	CALL INP_000_CMD(COM,NCOM,NC)
	IF(IFLAG.LT.0) THEN
	  IF(LUOUT.GT.0) GOTO 10
	  IIC=0
	ENDIF

100	INP_CMD=IIC
	RETURN

	END


	FUNCTION INP_CMDH(TEXT,COM,NCOM,NC)

	CHARACTER*(*) TEXT
	CHARACTER*(*) COM(NCOM)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/


	lbb = 0
10	CALL INP_000_MSG(TEXT,'> ')
*	CALL INP_000_LIN
	iic = lineget(BB,256)
	LBB = IIC
	IF(IIC.EQ.-1) GOTO 100
	IIC = 0
!	IF(LENGTHC(BB).LT.1) GOTO 10
	CALL INP_000_CMD(COM,NCOM,NC)
	IF(IFLAG.LT.0) THEN
	  IF(LUOUT.GT.0) GOTO 10
	  IIC=0
	ENDIF

100	INP_CMDH=IIC
	RETURN

	END

	FUNCTION INP_CMDIA(TEXT,COM,NCOM,NC,IA,N)

	CHARACTER*(*)	TEXT
	CHARACTER*(*) COM(NCOM)
	INTEGER IA(N)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,'> ')
	CALL INP_000_LIN
	IF(IIC.EQ.-1) GOTO 100
	CALL INP_000_CMD(COM,NCOM,NC)
	IF(IFLAG.LT.0) THEN
	  IF(LUOUT.GT.0) GOTO 10
	  IIC=0
	  GOTO 100
	ENDIF

	CALL INP_000_INT(IA,N)
	IF(IFLAG.LT.0) GOTO 10

100	INP_CMDIA=IIC
	RETURN

	END

	FUNCTION INP_CMDRA(TEXT,COM,NCOM,NC,RA,N)

	CHARACTER*(*) TEXT
	CHARACTER*(*) COM(NCOM)
	real*4 RA(N)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,'> ')
	CALL INP_000_LIN
	IF(IIC.EQ.-1) GOTO 100
	CALL INP_000_CMD(COM,NCOM,NC)
	IF(IFLAG.LT.0) THEN
	  IF(LUOUT.GT.0) GOTO 10
	  IIC=0
	  GOTO 100
	ENDIF

	CALL INP_000_REA(RA,N)
	IF(IFLAG.LT.0) GOTO 10

100	INP_CMDRA=IIC
	RETURN

	END

	FUNCTION INP_CMDDA(TEXT,COM,NCOM,NC,DA,N)

	CHARACTER*(*) TEXT
	CHARACTER*(*) COM(NCOM)
	DOUBLE PRECISION DA(N)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

10	CALL INP_000_MSG(TEXT,'> ')
	CALL INP_000_LIN
	IF(IIC.EQ.-1) GOTO 100
	CALL INP_000_CMD(COM,NCOM,NC)
	IF(IFLAG.LT.0) THEN
	  IF(LUOUT.GT.0) GOTO 10
	  IIC=0
	  GOTO 100
	ENDIF

	CALL INP_000_DEA(DA,N)
	IF(IFLAG.LT.0) GOTO 10

100	INP_CMDDA=IIC
	RETURN

	END

	SUBROUTINE INP_SETLUN(LUI,LUO)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	LUIN=LUI
	LUOUT=LUO

	RETURN

	END

	SUBROUTINE INP_SHOWLUN(LUI,LUO)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	LUI=LUIN
	LUO=LUOUT

	RETURN

	END

	SUBROUTINE INP_HOWMANY(II)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	II=IIC

	RETURN

	END

	SUBROUTINE INP_SETBUFF(STRING)

	CHARACTER*(*) STRING

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	LBB=MIN(255,lengthc(string))
	IF(LBB.GT.0) BB(1:LBB)=STRING(1:LBB)

	RETURN

	END

	SUBROUTINE INP_SHOWBUFF(NCHAR)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	NCHAR=LBB
	IF(NCHAR.LT.0) NCHAR=0
	IF(LBB.GT.0) THEN
		IBB=ICHAR(BB(1:1))
		IF(IBB.GT.57 .OR. IBB.LT.43) NCHAR=-NCHAR
		IF(IBB.EQ.44 .OR. IBB.EQ.47) NCHAR=-NCHAR
	ENDIF

	RETURN

	END

	SUBROUTINE INP_GETBUFF(STRING,LS)

	CHARACTER*(*) STRING

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	II=MIN(lengthc(bb),LEN(STRING))
	IF(II.GT.0) STRING(1:LBB)=BB(1:LBB)
	LS=LBB

	RETURN

	END

	SUBROUTINE INP_MODE(MODO)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	MODE=MODO
	IF(MODE.LE.0) MODE=0
	IF(MODE.GT.2) MODE=2

	IF(MODE.EQ.0) LBB=0

	RETURN

	END

	subroutine xinpmode( modo )
	
        logical*1 xinput
	common /inp_xinput/xinput
	
	if ( modo .gt. 0 )then
	    xinput=.TRUE.

#if (defined(__IFC) || defined(__INTEL_COMPILER))&& defined(__APPLE__)
	   if( .not.ISATTY(5) ) xinput = .false.
#endif

	else
	    xinput=.FALSE.
	endif
	
	return
	end
	
	


	SUBROUTINE INP_000_MSG(TEXT,DELIM)

	CHARACTER*(*)	TEXT,DELIM

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	IF(LUOUT.LE.0) RETURN

	II=LEN(TEXT)
	IZ=LENGTHZ(TEXT)
	IF(IZ.LT.II) II=IZ

	IF(MODE.LE.0 .OR. LBB.LE.0) THEN
		IF(II.GT.0) WRITE(LUOUT,'(1X,A,$)') TEXT(1:II)//DELIM
	ELSEIF(MODE.EQ.1) THEN
		IF(II.GT.0) WRITE(LUOUT,'(1X,A)') TEXT(1:II)//DELIM//BB(1:LBB)
		IF(II.LE.0) WRITE(LUOUT,'(1X,A)') DELIM//BB(1:LBB)
	ENDIF

	RETURN

	END

	SUBROUTINE INP_000_LIN

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	logical*1 xinput
	common /inp_xinput/xinput
	external inpx
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/
	
	integer*4 lbb1

	IIC=0
	
	
*	if( xinput .and. (mode.eq.0) ) then 
*	    call inpx(bb,lbb)
*	    if ( lbb .eq. -1 ) then
*	       iic = -1
*	     else
*	        iflag = 0
*	     endif
*	     return
*	endif
	if( xinput .and. (luin.eq.5) ) then 
	    IF(MODE.EQ.0) LBB=0
	    IF(LBB.LE.0) then
	        call inpx(bb,lbb1)
		lbb=lbb1
	    endif
	    if ( lbb .eq. -1 ) then
	       iic = -1
	     else
	        iflag = 0
	     endif
	     return
	endif
	
	IF(MODE.EQ.0) LBB=0
	
#if defined( __GFORTRAN__ )
        IF(LBB.LE.0) READ(LUIN,'(A)',END=99,ERR=99) BB	
	LBB = len(BB)
#else
	IF(LBB.LE.0) READ(LUIN,'(Q,A)',END=99,ERR=99) LBB,BB
#endif
	IFLAG=0
	RETURN

99	LBB=-1
	IIC=-1
	RETURN

	END

	SUBROUTINE INP_000_GET

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	CHARACTER*7	SEPARATOR	/'''"|= ,	'/
	CHARACTER*1	CH1

	IFLAG=0
	LCC=0
	CALL INP_000_BLA
	IF(LBB.LE.0) RETURN

	IFLAG=1
	DO WHILE(LBB.GT.0)
		CH1=BB(1:1)
		CALL INP_000_SHF(1)
		IF(INDEX(SEPARATOR,CH1).GT.0) GOTO 100
		LCC=LCC+1
		CC(LCC:LCC)=CH1
	ENDDO

100	CALL INP_000_BLA
	RETURN

	END

	SUBROUTINE INP_000_BLA

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

1	IF(LBB.GT.0 .AND. ( BB(1:1).EQ.' ' .OR. BB(1:1).EQ.'	' ) ) THEN
		CALL INP_000_SHF(1)
		GOTO 1
	ENDIF
	RETURN

	END

	SUBROUTINE INP_000_SHF(IZ)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	DO I=1,IZ
		IF(LBB.LE.0) RETURN
		BB=BB(2:)
		LBB=LBB-1
	ENDDO

	RETURN

	END

	SUBROUTINE INP_000_LOG(IOK)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	CHARACTER*7	TRUE	/'YSJODT1'/
	CHARACTER*3	FALSE	/'NF0'/
	CHARACTER*1	CH1
	CHARACTER*52 MM(2)
	DATA MM/' Answer should be  Yes,Si,True,1 or Not,<CR>,False,0',
	1	' Answer should be  Yes,<CR>,Si,True,1 or Not,False,0'/

	CALL INP_000_GET
	IF(LCC.LE.0) RETURN

	CH1=CC(1:1)
	CALL STR_TOUPPER(CH1)
	IF(INDEX(FALSE,CH1).GT.0)  THEN
	   IOK=0
	ELSE IF(INDEX( TRUE,CH1).GT.0) THEN
	   IOK=1
	ELSE
	   IMES=1
	   IF(IOK.EQ.1) IMES=2 
	   IF(LUOUT.GT.0) WRITE(LUOUT,'(A)') MM(IMES)
	   IFLAG=-1
	   LBB=0
	ENDIF

	RETURN

	END

	SUBROUTINE INP_000_INT(IA,N)

	INTEGER IA(N)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	IIC=0
	IF(LBB.EQ.1 .AND. BB(1:1).EQ.'?') THEN
#if defined( __GFORTRAN__ )
		IF(LUOUT.GT.0) WRITE(LUOUT,'(1X,4I10)') (IA(II),II=1,N)
#else
		IF(LUOUT.GT.0) WRITE(LUOUT,'(1X,4I)') (IA(II),II=1,N)
#endif
		IFLAG=-1
		LBB=0
		RETURN
	ENDIF
	DO I=1,N
	   CALL INP_000_NXT
	   IF(IFLAG.LE.0) RETURN
	   IF(IFLAG.EQ.1) THEN
		IF(DDD.GT.2.147483647E9.OR.DDD.LT.-2.147483648E9) THEN
		   IFLAG=-1
		   LBB=0
		   IIC=-1
		   IF(LUOUT.GT.0) WRITE(LUOUT,
	1		'('' Integer too big '',F20.0)') DDD
		   RETURN
		ENDIF
		IA(I)=DDD
	   ENDIF
	   IIC=IIC+1
	ENDDO
	RETURN

	END

	SUBROUTINE INP_000_REA(RA,N)

	real*4 RA(N)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	IIC=0
	IF(LBB.EQ.1 .AND. BB(1:1).EQ.'?') THEN
#if defined( __GFORTRAN__ )
		IF(LUOUT.GT.0) WRITE(LUOUT,'(1X,4G16.3)') (RA(II),II=1,N)
#else
		IF(LUOUT.GT.0) WRITE(LUOUT,'(1X,4G)') (RA(II),II=1,N)
#endif
		IFLAG=-1
		LBB=0
		RETURN
	ENDIF
	DO I=1,N
		CALL INP_000_NXT
		IF(IFLAG.LE.0) RETURN
		IF(IFLAG.EQ.1) RA(I)=DDD
		IIC=IIC+1
	ENDDO
	RETURN

	END

	SUBROUTINE INP_000_DEA(DA,N)

	DOUBLE PRECISION DA(N)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	IIC=0
	IF(LBB.EQ.1 .AND. BB(1:1).EQ.'?') THEN
		IF(LUOUT.GT.0) WRITE(LUOUT,*) (DA(II),II=1,N)
		IFLAG=-1
		LBB=0
		RETURN
	ENDIF
	DO I=1,N
		CALL INP_000_NXT
		IF(IFLAG.LE.0) RETURN
		IF(IFLAG.EQ.1) DA(I)=DDD
		IIC=IIC+1
	ENDDO
	RETURN

	END

	SUBROUTINE INP_000_NXT

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	INTEGER		I4

	DDD=0.
	CALL INP_000_GET
	IF(IFLAG.LE.0) RETURN

100	IF(LCC.EQ.0) THEN
		IFLAG=2
		RETURN
	ENDIF

#if defined( __GFORTRAN__ )
	READ(CC(1:LCC),*,ERR=200) I4
#else
	READ(CC(1:LCC),'(I)',ERR=200) I4
#endif
	DDD=I4
	RETURN

200	IF(INDEX(CC(1:LCC),'.') .EQ. 0) THEN
		LCC=LCC+1
		CC(LCC:LCC)='.'
	ENDIF
#if defined( __GFORTRAN__ )
	READ(CC(1:LCC),*,ERR=300) DDD
#else
	READ(CC(1:LCC),'(E)',ERR=300) DDD
#endif
	RETURN

300	IF(LUOUT.GT.0) WRITE(LUOUT,
	1	'('' Conversion-error in : '',A)') CC(1:LCC)
	IFLAG=-1
	LBB=0
	RETURN

	END

	SUBROUTINE INP_000_SPN(FNAME,FORM,NKAP)

	CHARACTER*(*) FNAME,FORM

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	CHARACTER*1  CH
	CHARACTER*128 DIRF,NAMF,TYPF,FORF
	CHARACTER*128 DIRB,NAMB,TYPB,FORB
	CHARACTER*128 SAV

	IFLAG=0
	CALL INP_000_BLA
	IF(LBB.LE.0) RETURN
	
	IFLAG=-1
	IF(LBB.EQ.1 .AND. BB(1:1).EQ.'?') THEN
	   IF(LUOUT.GT.0) THEN
		LL=MAX(1,LENGTHC(FNAME))
		WRITE(LUOUT,'(1X,A,2X,A,I3)') FNAME(1:LL),FORM(1:1),NKAP
	   ENDIF
	   LBB=0
	   RETURN
	ENDIF

	LL=INDEX(BB(1:LBB),' ')-1
	IF(LL.LE.0) LL=LBB

	LM=INDEX(BB(1:LBB),'	')-1
	IF(LM.LE.0) LM=LBB
	LL=MIN(LL,LM)

	CALL FNDECOMPOSE(BB(1:LL),DIRB,LDIB,NAMB,LNAB,TYPB,LTYB,FORB,LFOB)

	ISPEC=0
	IF(INDEX(NAMB,'+') .GT.0 ) ISPEC=1
	IF(INDEX(NAMB,'-') .GT.0 ) ISPEC=1
	IF(INDEX(NAMB,'=') .GT.0 ) ISPEC=1
	IF(INDEX(NAMB,'#') .GT.0 ) ISPEC=1
	IF(INDEX(TYPB,'+') .GT.0 ) ISPEC=1
	IF(INDEX(TYPB,'-') .GT.0 ) ISPEC=1
	IF(INDEX(TYPB,'=') .GT.0 ) ISPEC=1

	IF(ISPEC.EQ.1) THEN
	   CALL FNDECOMPOSE(FNAME,DIRF,LDIF,NAMF,LNAF,TYPF,LTYF,FORF,LFOF)
	   IF(LDIB.EQ.0) THEN
		DIRB=DIRF
		LDIB=LDIF
	   ENDIF
	   IF(LNAB.EQ.0) THEN
		NAMB=NAMF
		LNAB=LNAF
	   ELSEIF(INDEX('#',NAMB(1:1)).GT.0) THEN
		lnafn=index(namf(1:lnaf),'#')
		if(lnafn.le.0) then
		  IF(LUOUT.GT.0) WRITE(LUOUT,'(a)')
	1	  ' Original filename does not contain # '
		  GOTO 99
		endif
		NAMB=NAMF(1:lnafn)//NAMB(2:lnab)
		lnab=lnafn+lnab-1				
	   ELSEIF(INDEX('+-=',NAMB(1:1)).GT.0) THEN
		SAV=NAMB
		LSAV=LNAB
		NAMB=NAMF
		LNAB=LNAF
		DO II=1,LSAV
			CH=SAV(II:II)
			IF(CH.EQ.'+') THEN
				CALL STR_INCREM(NAMB)
			ELSEIF(CH.EQ.'-') THEN
				CALL STR_DECREM(NAMB)
			ELSEIF(CH.EQ.'=') THEN
				CONTINUE
			ELSE				! ERROR
				LSAV=MAX(1,LSAV)
				IF(LUOUT.GT.0) WRITE(LUOUT,
	1			'('' Error in  '',A)') SAV(1:LSAV)
				GOTO 99
			ENDIF
		ENDDO
	   ENDIF
	   IF(LTYB.EQ.0) THEN
		TYPB=TYPF
		LTYB=LTYF
	   ELSEIF(INDEX('+-=',TYPB(1:1)).GT.0) THEN
		SAV=TYPB
		LSAV=LTYB
		TYPB=TYPF
		LTYB=LTYF
		DO II=1,LSAV
			CH=SAV(II:II)
			IF(CH.EQ.'+') THEN
				CALL STR_INCREM(TYPB)
			ELSEIF(CH.EQ.'-') THEN
				CALL STR_DECREM(TYPB)
			ELSEIF(CH.EQ.'=') THEN
				CONTINUE
			ELSE				! ERROR
				LSAV=MAX(1,LSAV)
				IF(LUOUT.GT.0) WRITE(LUOUT,
	1			'('' Error in  '',A)') '.'//SAV(1:LSAV)
				GOTO 99
			ENDIF
		ENDDO
	   ENDIF
	   IF(LFOB.EQ.0) THEN
		FORB=FORF
		LFOB=LFOF
	   ENDIF
	ENDIF

	CH=FORM(1:1)
	NN=NKAP
	CALL SPFORM(FORB,CH,NN)
	IF(NN.LT.0) THEN
	   LFOB=MAX(1,LFOB)
	   IF(LUOUT.GT.0) WRITE(LUOUT,'('' Error in  '',A)') '|'//FORB(1:LFOB)
	   GOTO 99
	ENDIF
	FORM=CH
	NKAP=NN

	CALL FNRICOMPOSE(FNAME,DIRB,LDIB,NAMB,LNAB,TYPB,LTYB,' ',0)

	CALL INP_000_SHF(LL+1)
	IIC=3
	IFLAG=0
	RETURN

99	LBB=0
	RETURN

	END

	SUBROUTINE INP_000_CMD(COM,NCOM,NC)

	CHARACTER*256 BB,CC
	DOUBLE PRECISION DDD
	COMMON /INP_COM/ DDD,BB,LBB,CC,LCC,IIC,IFLAG,MODE,LUIN,LUOUT
*	DATA MODE,LUIN,LUOUT/0,5,6/

	CHARACTER*(*)	COM(NCOM)
	CHARACTER*256	LOCCOM

	NC=0
	IFLAG=0
	IF(LBB.EQ.0) RETURN

	IF(LBB.LT.0) THEN
		CC='$EOF'
		LCC=4
	ELSE
		CALL INP_000_GET
		IF(LCC.EQ.0) RETURN
		CALL STR_TOUPPER(CC(1:LCC))
	ENDIF

	NNC=0
	DO NC=NCOM,1,-1
	   LOCCOM=COM(NC)
	   CALL KILLBLA(LOCCOM)
	   CALL STR_TOUPPER(LOCCOM)
	   IF( INDEX( LOCCOM,CC(1:LCC) ) .EQ. 1 ) THEN
		IF( LENGTHC( LOCCOM ) .EQ. LCC ) RETURN
		NNC=NC
	   ENDIF
	ENDDO
	NC=NNC
	IF(NC.GT.0) RETURN

	IF(CC(1:1) .EQ. '?' .AND. LUOUT.GT.0 ) THEN
		WRITE(LUOUT,'('' Command-list: '')')
		DO I=1,NCOM
			IF(COM(I).NE.' ') WRITE(LUOUT,'(1X,A)') COM(I)
		ENDDO
		IFLAG=-1
		LBB=0
		RETURN
	ENDIF

	IF(LUOUT.GT.0) WRITE(LUOUT,'('' Illegal command : '',A)') CC(1:LCC)
	IFLAG=-1
	LBB=0

	RETURN

	END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C$+									C
C			   Package ISTR_...				C
C$-									C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C$+   Package ISTR_...
C
C  routines di lettura di comandi e variabili da stringa
C  a differenza di INP_... i valori non dati somo messi a 0 (se numeri)
C  o ' ' (se stringhe)
C
C ROUTINES (functions) implementate
C
C  istr_ch	Input una stringa contigua di caratteri
C  istr_ch2up	Input una stringa contigua di caratteri resi maiuscoli
C  istr_cmd	Input un comando
C  istr_ii	Input di un intero
C  istr_ia	Input vettore di interi
C  istr_i1...8	Input 1...8 interi
C  istr_rr	Input di un reale
C  istr_ra	Input vettore di reali
C  istr_r1...8	Input 1...8  reali
C  istr_dd	Input di un reale doppia precisione
C  istr_da	Input vettore di reali doppia precisione
C  istr_d1...8	Input 1...8  reali doppia precisione

	function istr_ch(bb,ch)

	character*(*) bb,ch

	ltoken=abs(lengthn(bb))

	ch=' '
	
	if(ltoken.GT.0) then
	  ch=bb(1:ltoken)
	  call str_trim(bb,ltoken+1)
	endif
	istr_ch=ltoken

	return

	end

	function istr_ch2up(bb,ch)

	character*(*) bb,ch

	ltoken=abs(lengthn(bb))

	ch=' '
	
	if(ltoken.GT.0) then
	  ch=bb(1:ltoken)
	  call str_toupper(ch)
	  call str_trim(bb,ltoken+1)
	endif
	istr_ch2up=ltoken

	return

	end

	function istr_cmd(bb,com,ncom,nnc)

	character bb*(*)
	character com(ncom)*(*)
	character loccom*(80)

	nnc=0
	ltoken=abs(lengthn(bb))
	if(ltoken.eq.0) goto 100

	nnc=0
	do ii=ncom,1,-1
	  loccom=com(ii)
	  lcom=abs(lengthn(loccom))
	  if(lcom.gt.0) then
	    if( index(loccom(1:lcom),bb(1:ltoken) ) .eq. 1 ) then
	      nnc=ii
	      if(lcom .eq. ltoken) goto 100	! exact match
	    endif
	  endif
	enddo

100	if(nnc.gt.0) then
	  call str_trim(bb,ltoken+1)
	else
	  ltoken=-ltoken
	endif
	istr_cmd=ltoken

	return

	end

	function istr_ii(bb,ii)

	parameter (DMININT=-2147483648.0)
	parameter (DMAXINT= 2147483647.0)

	character*(*) bb
	integer ii
	integer iii
	double precision ddd

	ii=0

	ltoken=lengthn(bb)
	if(ltoken.le.0) goto 100

#if defined( __GFORTRAN__ )
	read(bb(1:ltoken),*,err=10) iii
#else
	read(bb(1:ltoken),'(i)',err=10) iii
#endif
	ii=iii
	goto 100

10	continue
#if defined( __GFORTRAN__ )
	read(bb(1:ltoken),*,err=20) ddd
#else
	read(bb(1:ltoken),'(d)',err=20) ddd
#endif
	if(ddd.GT.DMAXINT .OR. ddd.LT.DMININT) then
	  ltoken=-1
	else
	  ii=ddd
	endif
	goto 100

20	ltoken=-ltoken

100	if(ltoken.gt.0) call str_trim(bb,ltoken+1)
	istr_ii=ltoken

	return

	end

	function istr_ia(bb,ia,n)

	character*(*) bb
	integer	n,ia(n)

	do ii=1,n
	  ia(ii)=0
	enddo
	
	nn=0
	do ii=1,n
	  iic=istr_ii(bb,ia(ii))
	  if(iic.lt.0) then
	    nn=-nn
	    goto 100
	  endif
	  if(iic.gt.0) nn=nn+1
	enddo

100	istr_ia=nn

	return

	end

	function istr_i1(bb,ii1)

	character*(*) bb
	integer ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8
	integer ii(8)

	nii=1
	assign 101 to igo
	goto 1

	entry 	istr_i2(bb,ii1,ii2)
	nii=2
	assign 102 to igo
	goto 2

	entry 	istr_i3(bb,ii1,ii2,ii3)
	nii=3
	assign 103 to igo
	goto 3

	entry 	istr_i4(bb,ii1,ii2,ii3,ii4)
	nii=4
	assign 104 to igo
	goto 4

	entry 	istr_i5(bb,ii1,ii2,ii3,ii4,ii5)
	nii=5
	assign 105 to igo
	goto 5

	entry 	istr_i6(bb,ii1,ii2,ii3,ii4,ii5,ii6)
	nii=6
	assign 106 to igo
	goto 6

	entry 	istr_i7(bb,ii1,ii2,ii3,ii4,ii5,ii6,ii7)
	nii=7
	assign 107 to igo
	goto 7

	entry 	istr_i8(bb,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8)
	nii=8
	assign 108 to igo
	goto 8

8	ii(8)=ii8
7	ii(7)=ii7
6	ii(6)=ii6
5	ii(5)=ii5
4	ii(4)=ii4
3	ii(3)=ii3
2	ii(2)=ii2
1	ii(1)=ii1

	istr_i1=istr_ia(bb,ii,nii)
	goto igo

108	ii8=ii(8)
107	ii7=ii(7)
106	ii6=ii(6)
105	ii5=ii(5)
104	ii4=ii(4)
103	ii3=ii(3)
102	ii2=ii(2)
101	ii1=ii(1)

	return

	end

	function istr_rr(bb,rr)

	character*(*) bb
	real*4 rr
	real*4 rrr
	double precision ddd

	rr=0.
	
	ltoken=lengthn(bb)
	if(ltoken.lt.0) goto 100

	read(bb(1:ltoken),*,err=10) rrr
	rr=rrr
	goto 100

10      continue
#if defined( __GFORTRAN__ )
	read(bb(1:ltoken),*,err=20) ddd
#else
	read(bb(1:ltoken),'(d)',err=20) ddd
#endif
	rr=ddd
	goto 100

20	ltoken=-1

100	if(ltoken.gt.0) call str_trim(bb,ltoken+1)
	istr_rr=ltoken
	return

	end

	function istr_ra(bb,ra,n)

	character*(*) bb
	integer n
	real*4 ra(n)

	do ii=1,n
	  ra(ii)=0.0
	enddo
	
	nn=0
	do ii=1,n
	  iic=istr_rr(bb,ra(ii))
	  if(iic.lt.0) then
	    nn=-nn
	    goto 100
	  endif
	  if(iic.gt.0) nn=nn+1
	enddo

100	istr_ra=nn

	return

	end

	function istr_r1(bb,rr1)

	character*(*) bb

	real*4 rr1,rr2,rr3,rr4,rr5,rr6,rr7,rr8
	real*4 rr(8)

	nrr=1
	assign 101 to igo
	goto 1

	entry 	istr_r2(bb,rr1,rr2)
	nrr=2
	assign 102 to igo
	goto 2

	entry 	istr_r3(bb,rr1,rr2,rr3)
	nrr=3
	assign 103 to igo
	goto 3

	entry 	istr_r4(bb,rr1,rr2,rr3,rr4)
	nrr=4
	assign 104 to igo
	goto 4

	entry 	istr_r5(bb,rr1,rr2,rr3,rr4,rr5)
	nrr=5
	assign 105 to igo
	goto 5

	entry 	istr_r6(bb,rr1,rr2,rr3,rr4,rr5,rr6)
	nrr=6
	assign 106 to igo
	goto 6

	entry 	istr_r7(bb,rr1,rr2,rr3,rr4,rr5,rr6,rr7)
	nrr=7
	assign 107 to igo
	goto 7

	entry 	istr_r8(bb,rr1,rr2,rr3,rr4,rr5,rr6,rr7,rr8)
	nrr=8
	assign 108 to igo
	goto 8

8	rr(8)=rr8
7	rr(7)=rr7
6	rr(6)=rr6
5	rr(5)=rr5
4	rr(4)=rr4
3	rr(3)=rr3
2	rr(2)=rr2
1	rr(1)=rr1

	istr_r1=istr_ra(bb,rr,nrr)
	goto igo

108	rr8=rr(8)
107	rr7=rr(7)
106	rr6=rr(6)
105	rr5=rr(5)
104	rr4=rr(4)
103	rr3=rr(3)
102	rr2=rr(2)
101	rr1=rr(1)

	return

	end

	function istr_dd(bb,dd)

	character*(*) bb
	double precision dd
	double precision ddd

	dd=0.0
	
	ltoken=lengthn(bb)
	if(ltoken.le.0) goto 100

	read(bb(1:ltoken),*,err=10) ddd
	dd=ddd
	goto 100

10	ltoken=-ltoken

100	if(ltoken.gt.0) call str_trim(bb,ltoken+1)
	istr_dd=ltoken

	return

	end

	function istr_da(bb,da,n)

	character*(*) bb
	integer n
	double precision da(n)

	do ii=1,n
	  da(ii)=0.0
	enddo
	
	nn=0
	do ii=1,n
	  iic=istr_dd(bb,da(ii))
	  if(iic.lt.0) then
            nn=-nn
	    goto 100
	  endif
	  if(iic.gt.0) nn=nn+1
	enddo

100	istr_da=nn

	return

	end

	function istr_d1(bb,dd1)

	character*(*) bb

	double precision dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8
	double precision  dd(8)

	ndd=1
	assign 101 to igo
	goto 1

	entry 	istr_d2(bb,dd1,dd2)
	ndd=2
	assign 102 to igo
	goto 2

	entry 	istr_d3(bb,dd1,dd2,dd3)
	ndd=3
	assign 103 to igo
	goto 3

	entry 	istr_d4(bb,dd1,dd2,dd3,dd4)
	ndd=4
	assign 104 to igo
	goto 4

	entry 	istr_d5(bb,dd1,dd2,dd3,dd4,dd5)
	ndd=5
	assign 105 to igo
	goto 5

	entry 	istr_d6(bb,dd1,dd2,dd3,dd4,dd5,dd6)
	ndd=6
	assign 106 to igo
	goto 6

	entry 	istr_d7(bb,dd1,dd2,dd3,dd4,dd5,dd6,dd7)
	ndd=7
	assign 107 to igo
	goto 7

	entry 	istr_d8(bb,dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8)
	ndd=8
	assign 108 to igo
	goto 8

8	dd(8)=dd8
7	dd(7)=dd7
6	dd(6)=dd6
5	dd(5)=dd5
4	dd(4)=dd4
3	dd(3)=dd3
2	dd(2)=dd2
1	dd(1)=dd1

	istr_d1=istr_da(bb,dd,ndd)
	goto igo

108	dd8=dd(8)
107	dd7=dd(7)
106	dd6=dd(6)
105	dd5=dd(5)
104	dd4=dd(4)
103	dd3=dd(3)
102	dd2=dd(2)
101	dd1=dd(1)

	return

	end
