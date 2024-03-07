	SUBROUTINE ZIEGLERCH(ZP,AP,ZB,AB,EP,STE,IFLAG,C1,C2,STNUC,STLSS)

	DIMENSION SIMBHE(92),ZETAHE(92),C1HE(92),C2HE(92),AHE(9,92)
	DIMENSION SIMBH(92),ZETAH(92),C1H(92),C2H(92),AH(12,92)
	CHARACTER SIMBHE*2, SIMBH*2, SIMBPRO*2, SIMBOLI*2
	DIMENSION SIMBOLI(92),ZETA(92)
	EQUIVALENCE (SIMBOLI(1),SIMBH(1)),(ZETA(1),ZETAH(1))
	INTEGER ZETA, ZETAH, ZETAHE
*	DIMENSION EVET(100),COEFVET(100)

	character*128 gsdir
	integer*4 lgsdir
	common /PATH/gsdir,lgsdir

	DATA INIT /1/
	DATA PI/3.141593/

C	ZP = Carica del proiettile ( in cariche elementari).
C	AP = Massa del proiettile (amu; =1 per protone, =4 per l'alfa).
C	ZB e AB: come sopra per il bersaglio.
C	EP = Energia del proiettile in keV.
C	STE = Stopping elettronico in eV/[1E15 Atomi/cm2].
C	IFLAG = 1, uscita normale; = 0, uscita abnormale.
C	C1 coefficiente per passare da STE alle unita` [keV/micron].
C	C2 coefficiente per passare da STE alle unita` [eV/microgrammi/cm2].
C	STNUC Stopping nucleare.
C	STLSS Stopping totale in unita` LSS

C	Versione che contiene la correzione degli stopping elettronici, in modo
C	empirico, riferendosi a quelli di Chalk River.
C	Quando e` chiamata per la prima volta legge i files con i dati
C	per correggere i risultati per portarli su quelli di Chalk River
C	(DATI$TLGD) con un'interpolazione se il bersaglio e` il Gadolinio.

	IF(INIT.EQ.0) GOTO 10	! Coefficienti gia letti

        OPEN(UNIT=1,STATUS='OLD',FILE=gsdir(1:lgsdir)//'ziegdat.dat')
*	DO J=1,4
*		READ(1,*)
*	ENDDO	
	DO J=1,92
		READ(1,12) SIMBHE(J),ZETAHE(J)
     		READ(1,*) (AHE(K,J),K=1,9)
	ENDDO
12	FORMAT(A2,2X,I2)
	DO J=1,4
		READ(1,*)
	ENDDO	
	DO J=1,92
	     	READ(1,12) SIMBH( J),ZETAH( J)
		READ(1,*) (AH(K, J),K=1,12), C1H( J),C2H( J)
	ENDDO
	CLOSE(UNIT=1)

*NM        OPEN(UNIT=1,STATUS='OLD',FILE=gsdir(1:lgsdir)//'scoef.dat')
*NM	READ(1,*)NDIM,(EVET(I),I=1,NDIM),(COEFVET(I),I=1,NDIM)
*NM	CLOSE(UNIT=1)

	INIT=0

10 	IZ=ZB
	IFLAG=1
	EKEVAMU=EP/AP
	EMEVAMU=EKEVAMU/1000

	SIMBPRO='H '
	IF(ZP.EQ.2..AND.AP.EQ.4.) THEN	! Calcolo per le particelle alfa.
		DO K=1,9		! Ci sono i coefficienti?
			IF (AHE(K,IZ).NE.0.) GOTO 101
		ENDDO
		GOTO 99

101		E=EP
		SLOW= AHE(1,IZ)*E**AHE(2,IZ)
		SHIGH=(AHE(3,IZ)*1000./E)*LOG(1+(AHE(4,IZ)*1000./E)
	1		 +(AHE(5,IZ)*E/1000.))
		STE=1./((1./SLOW)+(1./SHIGH))
		GOTO 2000
	ENDIF


C	Si tratta di un protone o di uno ione pesante

	COEFPES=1.
	DO K=1,12		! Ci sono i coefficienti?
		IF(AH(K,IZ).NE.0.) GOTO 102
	ENDDO
	GOTO 99

102	IF(EKEVAMU.LE.200.) THEN				! LSS
		E=200.
		IF(AP.NE.1) THEN
			BFAC=.886*SQRT(E/25)/(ZP**.6666667)
			AFAC=BFAC+.0378*SIN(PI*BFAC/2)
			ZIBARZH = 1-EXP(-AFAC)*(1.034-0.1777*EXP(-ZP*.08114))
			COEFPES=ZIBARZH**2*ZP**2
		ENDIF
		SLOW=AH(2,IZ)*E**(.45)
		SHIGH=(AH(3,IZ)/E)*LOG(1.+(AH(4,IZ)/E)+AH(5,IZ)*E)
		STE=COEFPES/((1./SLOW)+(1./SHIGH)) ! stopping a 200KeV/AMU
		STE=STE*SQRT(EKEVAMU)/SQRT(E)
		GOTO 2000
	ENDIF

	IF(AP.NE.1) THEN
		BFAC=.886*SQRT(EP/AP/25)/(ZP**.6666667)
		AFAC=BFAC+.0378*SIN(PI*BFAC/2)
		ZIBARZH = 1-EXP(-AFAC)*(1.034-0.1777*EXP(-ZP*.08114))
		COEFPES=ZIBARZH**2*ZP**2
	ENDIF

	IF(EKEVAMU.GT.200. .AND. EKEVAMU.LT.1000.) THEN		! LOW
		E=EKEVAMU
		SLOW=AH(2,IZ)*E**(.45)
		SHIGH=(AH(3,IZ)/E)*LOG(1.+(AH(4,IZ)/E)+AH(5,IZ)*E)
		STE=COEFPES/((1./SLOW)+(1./SHIGH))
	ELSE IF(EKEVAMU.GE.1000.) THEN				! HIGH
		BETA2=1-(938.2/(EMEVAMU+938.2))**2
		SUM=0
		DO  K=0,4
			SUM=SUM+AH(K+8,IZ)*(LOG(EKEVAMU)**K)
		ENDDO
		STE=(AH(6,IZ)/BETA2)*(LOG(AH(7,IZ)*BETA2/(1.-BETA2))-BETA2)-SUM
		STE=COEFPES*STE
	ENDIF

2000	C1=C1H(IZ)
	C2=C2H(IZ)

C	Calcolo dello stopping nucleare (Ziegler J. of App. Phys.31 (1977) 544)

	DD3=2./3
	Z2TERZI=ZP**DD3+ZB**DD3
	EPSI=32.53*AB*EP/(ZP*ZB*(AP+AB)*SQRT(Z2TERZI))

	IF(EPSI .LT. (.01)) THEN
		STNUC=1.59*SQRT(EPSI)
	ELSE IF(EPSI .GE. (.01) .AND. EPSI .LE. (10.)) THEN
		STNUC=1.7*SQRT(EPSI)*LOG(EPSI+2.718282)/
	1		(1.+6.8*EPSI+3.4*EPSI**1.5)
	ELSE IF(EPSI .GT. (10.)) THEN
		STNUC=LOG(.47*EPSI)/(2*EPSI)
	ENDIF


	COEFLSS=8.462*ZP*ZB*AP/((AP+AB)*SQRT(Z2TERZI))
	STNUC=STNUC*COEFLSS

*NM	IF(ABS(ZB-64.).LT.0.1 .AND. EMEVAMU.LE.4.) THEN	! Interpolazione se Gd
*NM		CALL INTERPOL(NDIM,3,EMEVAMU,COEFFIC,EVET,COEFVET)
*NM		STE=STE*COEFFIC
*NM	ENDIF

	STLSS=(STNUC+STE)/COEFLSS
	RETURN

99	TYPE '('' Mancano i dati del '',A2,''; provvedi !!!'')', SIMBOLI(IZ)
	IFLAG=0
	RETURN

	END
