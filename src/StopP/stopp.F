	PROGRAM STOP

	DIMENSION Z(20),A(20),NATOM(20),ST(20),STNN(20),RNATOM(20)
	DIMENSION E(2049),STTOT(2049),X(2049),VEL(2049),T(2049),STN(2049)
	DIMENSION XMU(2049),VSUV0(2049)
	CHARACTER NOME*10
	logical*1 QUOTE
	logical*1 inp_ask, inp_not
	external inp_ask, inp_not

	character*128 gsdir
	integer*4 lgsdir
	common /PATH/gsdir,lgsdir


	call xinpmode(1)
	call getenv('GSDIR',gsdir)
	lgsdir=lengthc(gsdir)
	if(lgsdir.le.0) stop 'Please set the environment variable GSDIR'
	if(gsdir(lgsdir:lgsdir) .ne. '/')gsdir=gsdir(1:lgsdir)//'/'
	lgsdir=lengthc(gsdir)



10	CALL INP_R2('$ Z,A  del proiettile (0 per finire)  ',ZP,AP)
	IF(ZP.EQ.0) STOP
	IF(ZP.EQ.2.AND.AP.EQ.4) THEN
		CALL INP_I1('$ de/dx del He (1). Con carica effic. (2)  ',IEFF)
		IF(IEFF.EQ.2) AP=4.0001
		ENDIF
	CALL INp_r1('$ Energia del proiettile (keV)  ',EP)
	CALL INp_r1('$ Passo in energia per la stampa (keV)  ',STEP)
	CALL inp_str('$ Nome del bersaglio  ',NOME)
	CALL INp_r1('$ Densita` (g/cm3) del bersaglio  ',DENS)
	CALL INp_i1('$ Numero di atomi diversi nel bersaglio  ',NATOMS)	
	IF(NATOMS.LT.1) NATOMS=1

	ASUM=0

	DO N=1,NATOMS
		write(*,100) N
100		FORMAT('  Z, A , #  di atomi (per molecola) dell''elemento ',I2,$)
		CALL INp_r3 (' ',Z(N),A(N),RNATOM(N))
		IF(RNATOM(N).LE.0) RNATOM(N)=1
		NATOM(N)=RNATOM(N)
		ASUM= ASUM + A(N)*NATOM(N)
	ENDDO

	
	NPASSI=EP/STEP+1
	IF(NPASSI.GT.2049) THEN
		STEP=EP/2048
		NPASSI=2049
		ENDIF
	DO JJ=1,NPASSI
		E(JJ)=EP - (JJ-1)*STEP
	ENDDO

	DO 1000 JJ=1,NPASSI
	STPOW=0
	STNUCL=0

	DO N=1,NATOMS
		IF(E(JJ).NE.0) THEN
			CALL ZIEGLERCH(ZP,AP,Z(N),A(N),E(JJ),STE,IFLAG,C1,C2,STNUC,STLSS)
		ELSE
			IFLAG=1
			STE=0
			STNUC=0
			STLSS=0
		ENDIF
		IF(IFLAG.NE.1) THEN
			write(*,998) Z(N),A(N)
998			FORMAT(' Non ci sono i coefficienti per  lo stopping di Z=',I2,' e A=',I2)
			GOTO 10
		ENDIF
		ST(N)=(STE+STNUC)*NATOM(N)
		STNN(N)=STNUC*NATOM(N)
		STPOW=STPOW+ST(N)
		STNUCL=STNUCL+STNN(N)
	ENDDO

C	STTOT(JJ) = STPOW in unita` eV/[E15Atomi/cm3]
C	STTOT(JJ)=.6022*STPOW/ASUM in unita` keV/mmg/cm2

	STN(JJ)=.6022*STNUCL/ASUM
1000	STTOT(JJ)=.6022*STPOW/ASUM

C	Calcolo delle distanze percorse (in mmg/cm2), delle velocita`
C	(in SQRT(keV/amu)) .
	T(1)=0
	X(1)=0
	VEL(1)= SQRT((E(2)+E(1))/AP)

	DO JJ=2,NPASSI-1
		X(JJ)=X(JJ-1)+2*(E(JJ-1)-E(JJ))/(STTOT(JJ-1)+STTOT(JJ))
		VEL(JJ)=SQRT((E(JJ+1)+E(JJ))/AP)
	ENDDO

	JJ=NPASSI
	X(JJ)=X(JJ-1)+2*(E(JJ-1)-E(JJ))/(STTOT(JJ-1)+STTOT(JJ))
	VEL(JJ)=SQRT(E(JJ-1)/AP)

C	Per avere le veloc. in m/sec,i tempi in fsec. e le distanze in micron.
	VEL(1)=VEL(1)*3.1E5
	VSUV0(1)=VEL(1)*137/2.998E8
	DO JJ=2,NPASSI
		VEL(JJ)=VEL(JJ)*3.1E5
		VSUV0(JJ)=VEL(JJ)*137/2.998E8
		XMU(JJ)=X(JJ)/(100*DENS)
		T(JJ)= T(JJ-1) +1.E9*(XMU(JJ)-XMU(JJ-1))/VEL(JJ) 
	ENDDO

452	IZP=ZP
	IAP=AP
**	write(*,200) IZP,IAP,NOME,(Z(I),A(I),NATOM(I),I=1,NATOMS),DENS

	write(*,200) IZP,IAP,NOME
	do I=1,NATOMS
	  write(*,201)Z(I),A(I),NATOM(I)
	enddo
	write(*,202) DENS
**200	FORMAT(////' Rallentamento dello ione con Z=',I3,', A=',I3,',nel ',A10,/,<NATOMS>(' (Z= 'F8.4,'  A=',F10.4,
**     2'  PESO STECH.=',I3,')',/),' la cui densita` e` : ',F5.2,' g/cm3',//)

200	FORMAT(////' Rallentamento dello ione con Z=',I3,', A=',I3,',nel ',A10)
201     format(' (Z= 'F8.4,'  A=',F10.4,'  PESO STECH.=',I3,')')
202     format(' la cui densita` e` : ',F5.2,' g/cm3',//)

	write(*,300)
300	FORMAT(' ENERGIA    TEMPO  DISTANZA  DISTANZA dE/dx(El) dE/dx(Nc)  v/v0',/,'  [keV]',
     1'      [fs]  [ug/cm2]  [micron]    [ keV/ug/cm2 ]'//)

	DO JJ=1,NPASSI
		write(*,400) E(JJ),T(JJ),X(JJ),XMU(JJ),STTOT(JJ)-STN(JJ),STN(JJ),VSUV0(JJ)
	ENDDO
400		FORMAT(1X,F7.0,2F9.1,4F9.2)

500	IF(inp_not('$ Vuoi scrivere su  stopp.out')) THEN
		OPEN(UNIT=8,FILE='stopp.out',STATUS='unknown')
		WRITE(8,200)IZP,IAP,NOME,(Z(I),A(I),NATOM(I),I=1,NATOMS),DENS
		WRITE(8,300)
		DO JJ=1,NPASSI
			WRITE(8,400) E(JJ),T(JJ),X(JJ),XMU(JJ),STTOT(JJ)-STN(JJ),STN(JJ),VSUV0(JJ)
		ENDDO
		CLOSE(UNIT=8)
		ENDIF
	if( inp_not('$ Vuoi fare un altro calcolo ') )GOTO 10
	END
