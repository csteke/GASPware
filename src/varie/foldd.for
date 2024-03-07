	PROGRAM FOLDd

C	Calcola (con un metodo ricorsivo) la distribuzione di fold di
C	M(ULT) transizioni viste da
C	N(RIV) rivelatori di efficenza         EFFgtot
C                         con un rapporto P/T  PTuns  non soppresso
C                         con un rapporto P/T  PTsup  soppresso

	PARAMETER MAXFOLD=50
	REAL*8    PROBMIN
	PARAMETER (PROBMIN=1.0D-10)

	REAL*8 ZOLD(0:MAXFOLD,0:MAXFOLD,0:MAXFOLD)
	REAL*8 ZNEW(0:MAXFOLD,0:MAXFOLD,0:MAXFOLD)
                                             
	REAL*8 EFFgtot,EFFgpic,EFFgsup,EFFgcom,EFFgrej
	REAL*8 PgPic,PgCom,PgRej,PgSup,Voidg
	REAL*8 EFFntot,EFFnpic,EFFnsup,EFFncom,EFFnrej
	REAL*8 PnPic,PnCom,PnRej,PnSup,Voidn
	REAL*8 PTuns,PTsup
	REAL*8 POLD,PNEW
	REAL*8 Xpic,Xcom,Xrej,Xpar

	DIMENSION PROBtot(0:MAXFOLD)
	DIMENSION PROBpic(0:MAXFOLD)
	DIMENSION PROBsup(0:MAXFOLD)
	DIMENSION PM(10)

	REAL*8 DBINOMIAL

	OMEGAG=.25
	OMEGAN=.25
	PSUTuns=30
	PSUTsup=55
	NRIV=40
	MULTG=20
	MULTN=0

	open(6,carriagecontrol='FORTRAN',status='OLD')


1	IIC=INP_I2('Multiplicity for gammas and neutrons ',MULTG,MULTN)
	IF(IIC.LT.0) CALL EXIT
	MULTG=ABS(MULTG)
	IF(MULTG.LT.1) GOTO 1
	MULTG=ABS(MULTG)
	IF((MULTG+MULTN).GT.MAXFOLD) STOP 'Total Multiplicity is too big'
	IF((MULTG+MULTN).LE.0) STOP 'Total Multiplicity is too small'

	IIC=INP_I1('Number of detectors',NRIV)
	IF(IIC.LT.0) CALL EXIT
	NRIV=ABS(NRIV)
	IIMAX=MIN(MULTG+MULTN,NRIV)

	IIC=INP_R1('Individual Total gamma efficency   (%)',OMEGAG)
	IF(IIC.LT.0) CALL EXIT
	IIC=INP_R2('P/T unsuppressed, suppressed       (%)',PSUTuns,PSUTsup)
	IF(IIC.LT.0) CALL EXIT
	PSUTuns=ABS(PSUTuns)
	PSUTsup=ABS(PSUTsup)
	If(PSUTuns.gt.100 .or. PSUTsup.gt.100) goto 1
	EFFgtot=OMEGAG/100.
	IF(EFFgtot*Nriv.gt.1. .or. EFFgtot.le.0 ) then
	    write(6,*) 'Check the gamma efficency',EFFgtot
	    goto 1
	endif
	EFFgpic=EFFgtot*PSUTuns/100.
	IF(PSUTsup.GT.PSUTuns) THEN
	    EFFgsup=EFFgpic/PSUTsup*100.
	    EFFgrej=EFFgtot-EFFgsup
	ELSE
	    PSUTsup=PSUTuns
	    EFFgsup=EFFgtot
	    EFFgrej=0
	ENDIF
	EFFgcom=EFFgsup-EFFgpic
	PgPic=EFFgpic/EFFgtot
	PgCom=EFFgcom/EFFgtot
	PgRej=EFFgrej/EFFgtot
	PgSup=EFFgsup/EFFgtot
	Voidg=1-NRIV*EFFgtot

	if(MULTN.gt.0) then
	  IIC=INP_R1('Individual Total neutron efficency (%)',OMEGAN)
	  EFFntot=OMEGAn/100.
	  IF(EFFntot*Nriv.gt.1. .or. EFFntot.le.0 ) then
	    write(6,*) 'Check the neutron efficency',EFFntot
	    goto 1
	  endif
	  EFFnpic=0
	  EFFnsup=EFFntot*PgSup/(PgSup+PgRej)
	  EFFnrej=EFFntot*PgRej/(PgSup+PgRej)
	  EFFncom=EFFnsup-EFFnpic
	  PnPic=EFFnpic/EFFntot
	  PnCom=EFFncom/EFFntot
	  PnRej=EFFnrej/EFFntot
	  PnSup=EFFnsup/EFFntot
	  Voidn=1-NRIV*EFFntot
	endif

	ZNEW(0,0,0)=1.0D5
	MAXJGAM=0
	write(6,*)
	DO IGAM=1,MULTG+MULTN
	   if(IGAM.le.MULTG) then
		EFFtot=EFFgtot
		PPic=PgPic
		PCom=PgCom
		PRej=PgRej
		PSup=PgSup
		Void=Voidg
	   else
		EFFtot=EFFntot
		PPic=PnPic
		PCom=PnCom
		PRej=PnRej
		PSup=PnSup
		Void=Voidn
	   endif
	   JGAM=IGAM-1
	   MAXJGAM=MIN(MAXJGAM,JGAM)
	   xx=0
	   DO IRIV=0,MAXJGAM
	    DO IPIC=0,IRIV
	     DO ICOM=0,IRIV-IPIC
		PNEW=ZNEW(IRIV,IPIC,ICOM)
		IF(PNEW.LT.PROBMIN) PNEW=0.0D0
		ZOLD(IRIV,IPIC,ICOM)=PNEW
		xx=xx+PNEW
	     ENDDO
	    ENDDO
	   ENDDO
	   MAXIGAM=MIN(MAXJGAM+1,IGAM)
	   DO IRIV=0,MAXIGAM
	    DO IPIC=0,IRIV
	     DO ICOM=0,IRIV-IPIC
		ZNEW(IRIV,IPIC,ICOM)=0.
	     ENDDO
	    ENDDO
	   ENDDO
	   WRITE(6,'(1H+,2I4,F)')IGAM,MAXJGAM,XX
	   MAXJGAM=0
	   DO IRIV=0,JGAM
	    Jriv=max(0,Iriv-1)
	    DO IPIC=0,IRIV
	     Jpic=max(0,Ipic-1)
	     DO ICOM=0,IRIV-IPIC
		Jcom=max(0,ICom-1)
		IREJ=IRIV-IPIC-ICOM
		POLD=ZOLD(IRIV,IPIC,ICOM)
		IF(POLD.GT.0) THEN
		  MAXJGAM=IRIV+1
		  PNEW=POLD*Void
		  ZNEW(IRIV,IPIC,ICOM)=ZNEW(IRIV,IPIC,ICOM)+PNEW
		  IF(IRIV.GT.0) THEN
		    Pnew=POLD*EFFtot
		    If(IPIC.GT.0) then
			Xpar=Pnew*IPIC
			Xcom=Xpar*PSup
			ZNEW(Iriv,Jpic,Icom+1)=ZNEW(Iriv,Jpic,Icom+1)+XCom
			Xrej=Xpar*PRej
			ZNEW(Iriv,Jpic,Icom  )=ZNEW(Iriv,Jpic,Icom  )+XRej
		    endif
		    If(ICOM.GT.0) then
			Xpar=Pnew*ICOM
			Xcom=Xpar*PSup
			ZNEW(Iriv,Ipic,Icom)=ZNEW(Iriv,Ipic,Icom)+XCom
			Xrej=Xpar*PRej
			ZNEW(Iriv,Ipic,Jcom)=ZNEW(Iriv,Ipic,Jcom)+XRej
		    endif
		    If(IREJ.GT.0) then
			Xpar=Pnew*IREJ
			ZNEW(Iriv,Ipic,Icom)=ZNEW(Iriv,Ipic,Icom)+XPar
		    endif
		  ENDIF
		  NNEW=NRIV-IRIV
		  IF(NNEW.GT.0) then
		    PNEW=POLD*NNEW*EFFtot
		    Xpic=PNEW*PPic
		    ZNEW(Iriv+1,Ipic+1,Icom  )=ZNEW(Iriv+1,Ipic+1,Icom  )+Xpic
		    Xcom=PNEW*PCom
		    ZNEW(Iriv+1,Ipic  ,Icom+1)=ZNEW(Iriv+1,Ipic  ,Icom+1)+Xcom
		    Xrej=PNEW*PRej
		    ZNEW(Iriv+1,Ipic  ,Icom  )=ZNEW(Iriv+1,Ipic  ,Icom  )+Xrej
		  ENDIF
		ENDIF
	     ENDDO
	    ENDDO
	   ENDDO
	ENDDO

	DO II=0,MULTG+MULTN
	   Probtot(ii)=0
	   Probpic(ii)=0
	   Probsup(ii)=0
	ENDDO
	jjmax=min(iimax,maxjgam)
	DO Iriv=0,jjmax
	 DO Ipic=0,Iriv
	  DO Icom=0,Iriv-Ipic
	   Irej=Iriv-Ipic-Icom
	   Xx=ZNEW(Iriv,Ipic,Icom)
	   Probpic(Ipic)=Probpic(Ipic)+Xx
	   Probsup(Ipic+Icom)=Probsup(Ipic+Icom)+Xx
	   Probtot(Ipic+Icom+Irej)=Probtot(Ipic+Icom+Irej)+Xx
	  ENDDO
	 ENDDO
	ENDDO

	PTOTtot=0
	PTOTpic=0
	PTOTsup=0
	DO II=0,IIMAX
	   PTOTtot=PTOTtot+Probtot(ii)
	   PTOTpic=PTOTpic+Probpic(ii)
	   PTOTsup=PTOTsup+Probsup(ii)
	ENDDO

c	Pmax=MAX(PTOTtot,PTOTpic,PTOTsup)
c	IF(PMAX.EQ.0) CALL EXIT

	WRITE(6,*)
	WRITE(6,'(''  # of Detectors    ='',I5)') NRIV

	WRITE(6,'(''  Gamma Multiplicity='',I5)') MULTG
	WRITE(6,'(''  Eff.(Total      ) ='',F8.3,''%  ==> Tot.='',F8.3,''%    P/T='',F7.2,1H%)')
	1	EFFgtot*100,NRIV*EFFgtot*100,PsuTuns
	WRITE(6,'(''  Eff.(Total Supp.) ='',F8.3,''%  ==> Tot.='',F8.3,''%    P/T='',F7.2,1H%)')
	1	EFFgsup*100,NRIV*EFFgsup*100,PSUTsup
	WRITE(6,'(''  Eff.(Peak       ) ='',F8.3,''%  ==> Tot.='',F8.3,1H%)')
	1	EFFgpic*100,NRIV*EFFgpic*100

	If(Multn.gt.0) then
	   WRITE(6,'(''  Neutron Multiplicity='',I5)') MULTn
	   WRITE(6,'(''  Eff.(Total      ) ='',F8.3,''%  ==> Tot.='',F8.3,1H%)')
	1	EFFntot*100,NRIV*EFFntot*100
	   WRITE(6,'(''  Eff.(Total Supp.) ='',F8.3,''%  ==> Tot.='',F8.3,1H%)')
	1	EFFnsup*100,NRIV*EFFnsup*100
	endif

	write(6,*)' The Fold distribution is normalized to 100000.'

	DO II=0,jjMAX
	   Xmax=MAX(Probtot(ii),Probpic(ii),Probsup(ii))
	   IF(Xmax.ge.0.1) WRITE(6,
	1	'(''  k='',I4,''    Tot.('',i2,'')='',F8.1,''    Supp.('',i2,'')='',F8.1,''    Peak('',i2,'')='',F8.1)')
	1	II,II,Probtot(II),II,Probsup(II),II,Probpic(II)
	ENDDO

	CALL MOMENTR(Probtot(0),jjmax+1,PM,3)
	P0=PM(1)
	IF(P0.GT.0) THEN
	   P1Tot=PM(2)-1
	   P2Tot=2.355*sqrt(PM(3))
	else
	   p1Tot=0
	   p2Tot=0
	endif
	CALL MOMENTR(Probsup(0),jjmax+1,PM,3)
	P0=PM(1)
	IF(P0.GT.0) THEN
	   P1sup=PM(2)-1
	   P2sup=2.355*sqrt(PM(3))
	else
	   p1sup=0
	   p2sup=0
	endif
	CALL MOMENTR(Probpic(0),jjmax+1,PM,3)
	P0=PM(1)
	IF(P0.GT.0) THEN
	   P1Pic=PM(2)-1
	   P2pic=2.355*sqrt(PM(3))
	else
	   p1Pic=0
	   p2Pic=0
	endif
	WRITE(6,'(''  Mean Fold Total'',f12.2,4x,''Suppr.'',f12.2,4x,''Peak '',f12.2)') P1Tot,P1sup,P1Pic
c	WRITE(6,'(''  FWHM      Total'',f12.2,4x,''Supp.'',f12.2,4x,''Peak  '',f12.2)') P2Tot,P2sup,P2Pic

	Tottot=0
	Totsup=0
	Totpic=0
	DO II=1,MULTG+MULTN
	   Tottot=Tottot+ii*PROBtot(ii)
	   Totsup=Totsup+ii*PROBsup(ii)
	   Totpic=Totpic+ii*PROBpic(ii)
	ENDDO
	Tottot=Tottot/Nriv
	Totsup=Totsup/Nriv
	Totpic=Totpic/Nriv
	Xmax=MAX(Tottot,Totsup,Totpic)
	IF(Xmax.gt.0.1) WRITE(6,'(''  Countrate '',''Total'',F12.1,''    Suppr.'',F12.1,''    Peak '',F12.1)')
	1	Tottot,Totsup,Totpic

	do im=2,4
	  Tottot=0
	  Totsup=0
	  Totpic=0
	  DO II=Im,MULTG+MULTN
	   Tottot=Tottot+PROBtot(ii)
	   Totsup=Totsup+PROBsup(ii)
	   Totpic=Totpic+PROBpic(ii)
	  ENDDO
	  Xmax=MAX(Tottot,Totsup,Totpic)
	  IF(Xmax.gt.0.1) WRITE(6,'(''  Folds>='',I1,''  Total'',F12.0,''    Suppr.'',F12.0,''    Peak '',F12.0)')
	1	Im,Tottot,Totsup,Totpic
	enddo

	do im=2,4
	  Tottot=0
	  Totsup=0
	  Totpic=0
	  DO II=2,jjMAX
	   Tottot=Tottot+PROBtot(ii)*DBINOMIAL(ii,Im)
	   Totsup=Totsup+PROBsup(ii)*DBINOMIAL(ii,Im)
	   Totpic=Totpic+PROBpic(ii)*DBINOMIAL(ii,Im)
	  ENDDO
	  Xmax=MAX(Tottot,Totsup,Totpic)
	  IF(Xmax.gt.0.1) WRITE(6,'(''  Unfold>='',I1,'' Total'',F12.0,''    Suppr.'',F12.0,''    Peak '',F12.0)')
	1	Im,Tottot,Totsup,Totpic
	enddo

	WRITE(6,*)
	GOTO 1

	END

	DOUBLE PRECISION FUNCTION DBINOMIAL(M,N)

	DOUBLE PRECISION XX,SU,GIU

	XX=0
	MN=M-N
	IF(MN.GE.0) THEN
		XX=1
		NFACT=MIN(N,MN)
		IF(NFACT.GT.0) THEN
			SU=M+1
			GIU=NFACT+1
			DO II=1,NFACT
				SU=SU-1
				GIU=GIU-1
				XX=(XX/GIU)*SU
			ENDDO
		ENDIF
	ENDIF
	DBINOMIAL=XX
	RETURN

	END
