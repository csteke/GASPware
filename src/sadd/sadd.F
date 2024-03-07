	PROGRAM SADD

#include "sadd.inc"

	CHARACTER*80 CMD(40)

	CMD( 1)='NEW                     Restart program'
	CMD( 2)='INPUT  spec.name        Input spectrum'
	CMD( 3)='DISPLAY k1 k2           Simple spectrum display (k_wise)'
	CMD( 4)='ADD  spec.name [factor] Sum spectra channel by channel'
	CMD( 5)='ADDFACTOR factor        Sum constant'
	CMD( 6)='MULT spec.name          Multiply spectra channel by channel'
	CMD( 7)='MULTFACTOR factor       Multiply by constant'
	CMD( 8)='DIV spec.name           Divide spectra channel by channel'
	CMD( 9)='DIVFACTOR               Divide by constant '
	CMD(10)='SQRT                    Sqare root of channel value'
	CMD(11)='SHIFT p0,p1,...         Polynomial recalibration'
	CMD(12)='SMOOTH                  Ch(i)= ( ch(i-1) + 2*ch(i) + ch(i+1) )/4'
	CMD(13)='LIST from,to            List of channels'
	CMD(14)='OUTPUT spec.name        Write spectrum'
	CMD(15)='SERVICE n               n times execution of defined command sequence'
	CMD(16)='DEFINE_SERVICE          Definition of command sequence'
	CMD(17)='STATUS                  Show some program parameters'
	CMD(18)='CHANNEL_SET nchan val   Set channel nchan to val'
	CMD(19)='STORE  n                Store spectrum in internal register n'
	CMD(20)='RECALL n                Recall     "'
	CMD(21)='SPAWN  command          Execute a DCL command '
	CMD(22)='DERIVATIVE              Derivative of spectrum'
	CMD(23)='SHIFT_LUT filename      Recalibration according to Look Up Table'
	CMD(24)='SHIFT_SQRT              Recalibration as a square root compression'
	CMD(25)='SHIFT_SQUARE            Recalibration as a square expansion'
	CMD(26)='MULT_CHANNEL            Multiply spectrum by channel number'
	CMD(27)='DIV_CHANNEL             Divide            "'
	CMD(28)='SWAP_BYTE_LW            Swap Bytes of a long integer spectrum (I/O as real)'
	CMD(29)='PACK_CHANNELS  n        Pack n channels together '
	CMD(30)='MADD n [+|-|=.+|=.-]    Add n spectra cycling on file name (taken from input)'
	CMD(31)='EXIT                    Exit program'
	CMD(32)='QUIT                    Exit program'
	N_COMMANDS=32


CVMS	OPEN(UNIT=10,FILE='SADD.LOG',STATUS='NEW')
	OPEN(UNIT=10,FILE='SADD.LOG',STATUS='UNKNOWN')

	call xinpmode(1)
	CALL SADDNEW

50	IF(ISUC.LT.0) THEN
	  CALL INP_MODE(0)
	  CALL INP_MSG(' Operation aborted\n')
	  ICOUNT=0	  ! ON ERROR STOP SERVICE
	ENDIF

	CALL INP_MODE(1)

	IF(ICOUNT.GT.0) THEN	  ! INPUT FROM SERVICE
60	  ISVC=ISVC+1
	  IF(ISVC.EQ.1) 	WRITE(6,*) ICOUNT
	  IF(ISVC.GT.NSVC) THEN
	    ISVC=0
	    ICOUNT=ICOUNT-1
	    IF(ICOUNT.EQ.0) GOTO 50	! FINITO
	    GOTO 60
	  ENDIF
	  CALL INP_SETBUFF(SERV(ISVC))
	  CALL INP_MODE(2)
	ENDIF

	IIC=INP_CMDH('SADD',CMD,N_COMMANDS,NC)
	IF(IIC.EQ.-1) CALL EXIT(0)

	ISUC=-1
	CALL INP_MODE(2)	! enable pipelining
	IF(NC.EQ.1) THEN
	  CALL SADDNEW
	ELSEIF(NC.EQ.2) THEN
	  CALL INPUT
	ELSEIF(NC.EQ.3) THEN
	  CALL DISPLAY
	ELSEIF(NC.EQ.4) THEN
	  CALL OPERFILE(1)
	ELSEIF(NC.EQ.5) THEN
	  CALL OPERFACTOR(1)
	ELSEIF(NC.EQ.6) THEN
	  CALL OPERFILE(2)
	ELSEIF(NC.EQ.7) THEN
	  CALL OPERFACTOR(2)
	ELSEIF(NC.EQ.8) THEN
	  CALL OPERFILE(3)
	ELSEIF(NC.EQ.9) THEN
	  CALL OPERFACTOR(3)
	ELSEIF(NC.EQ.10) THEN
	  CALL SQRTSP
	ELSEIF(NC.EQ.11) THEN
	  CALL SHIFT
	ELSEIF(NC.EQ.12) THEN
	  CALL SMOOTH
	ELSEIF(NC.EQ.13) THEN
	  CALL LIST
	ELSEIF(NC.EQ.14) THEN
	  CALL OUTPUT
	ELSEIF(NC.EQ.15) THEN
	  IF(ICOUNT.GT.0) GOTO 50
	  CALL SERVICE
	ELSEIF(NC.EQ.16) THEN
	  IF(ICOUNT.GT.0) GOTO 50
	  CALL SERVDEF
	ELSEIF(NC.EQ.17) THEN
	  CALL SHOW_STATUS(6)
	ELSEIF(NC.EQ.18) THEN
	  CALL CHANNELSET
	ELSEIF(NC.EQ.19) THEN
	  CALL STORESPEC
	ELSEIF(NC.EQ.20) THEN
	  CALL RECALLSPEC
	ELSEIF(NC.EQ.21) THEN
	  CALL SADDSPAWN
	ELSEIF(NC.EQ.22) THEN
	  CALL DERIVATIVE
	ELSEIF(NC.EQ.23) THEN
	  CALL SHIFT_LUT
	ELSEIF(NC.EQ.24) THEN
	  CALL SHIFT_SQRT
	ELSEIF(NC.EQ.25) THEN
	  CALL SHIFT_SQUARE
	ELSEIF(NC.EQ.26) THEN
	  CALL MULT_CHANNEL
	ELSEIF(NC.EQ.27) THEN
	  CALL DIV_CHANNEL
	ELSEIF(NC.EQ.28) THEN
	  CALL SWAP_BYTE_LW
	ELSEIF(NC.EQ.29) THEN
	  CALL PACK_CHANNELS
	ELSEIF(NC.EQ.30) THEN
	  CALL MADD
	ELSEIF(NC.EQ.31 .OR. NC.EQ.32) THEN
	  CLOSE(UNIT=10)
	  CALL EXIT(0)
	ELSE
	  ISUC=1
	ENDIF

	GOTO 50

	END

	SUBROUTINE SADDNEW

#include "sadd.inc"

	FORMI='L'
	FORMA='L'
	FORMO='L'
	KLI=8
	KLA=8
	KLO=8
	KLCH=KLI*1024
	DO II=0,KLCH-1
	   RB1(II)=0
	   RB2(II)=0
	ENDDO
	IREG=1
	DO JJ=0,MAXREGIS
	   DO II=0,KLCH-1
	  RBSTOR(II,JJ)=0
	   ENDDO
	ENDDO

	WRITE(6,30)
	CALL SHOW_STATUS(6)
	WRITE(10,30)
	CALL SHOW_STATUS(10)
30	FORMAT(///' SADD  VAX/VMS   V2.1   2-FEB-1989')

99	ISUC=0
100	RETURN

	END

	SUBROUTINE SHOW_STATUS(LUN)

#include "sadd.inc"

	CHARACTER*12 CH12
	character*4 nm_fmt(2)
	

	LNKAP=1
	IF(KLI.GT. 9) LNKAP=2
	IF(KLI.GT.99) LNKAP=3

	if( LENGTHC(FNINP) .eq. 0 )  FNINP = 'file_name'
	LFNAM=LENGTHC(FNINP)

	CH12='INPUT'
	write(nm_fmt(1),'(i4)')lfnam
	write(nm_fmt(2),'(i4)')lnkap

#if defined(__IFC) || defined(__INTEL_COMPILER)
	WRITE(LUN,15)CH12,FNINP,FORMI,KLI
#else
	WRITE(LUN,'(1X,A12,4X,A'//nm_fmt(1)//',''|'',A1,'':'',I'//nm_fmt(2)//')')  CH12,FNINP,FORMI,KLI
#endif

	LNKAP=1
	IF(KLO.GT. 9) LNKAP=2
	IF(KLO.GT.99) LNKAP=3

	if( LENGTHC(FNOUT) .eq. 0 ) FNOUT = 'file_name'
	LFNAM=LENGTHC(FNOUT)

	CH12='OUTPUT'
	write(nm_fmt(1),'(i4)')lfnam
	write(nm_fmt(2),'(i4)')lnkap
*	WRITE(LUN,'(1X,A12,4X,A'//nm_fmt(1)//',''|'',A1,'':'',I'//nm_fmt(2)//')')  CH12,FNOUT,FORMO,KLO
#if defined(__IFC) || defined(__INTEL_COMPILER)
	WRITE(LUN,15)CH12,FNOUT,FORMO,KLO
#else
	WRITE(LUN,'(1X,A12,4X,A'//nm_fmt(1)//',''|'',A1,'':'',I'//nm_fmt(2)//')')  CH12,FNOUT,FORMO,KLO
#endif

	LNKAP=1
	IF(KLA.GT. 9) LNKAP=2
	IF(KLA.GT.99) LNKAP=3

	if( LENGTHC(FNADD) .eq. 0 ) FNADD = 'file_name'
	LFNAM=LENGTHC(FNADD)

	CH12='ADD-MULT-DIV'
	write(nm_fmt(1),'(i4)')lfnam
	write(nm_fmt(2),'(i4)')lnkap
*	WRITE(LUN,'(1X,A12,4X,A'//nm_fmt(1)//',''|'',A1,'':'',I'//nm_fmt(2)//')')  CH12,FNADD,FORMA,KLA
#if defined(__IFC) || defined(__INTEL_COMPILER)
	WRITE(LUN,15)CH12,FNADD,FORMA,KLI
#else
	WRITE(LUN,'(1X,A12,4X,A'//nm_fmt(1)//',''|'',A1,'':'',I'//nm_fmt(2)//')')  CH12,FNADD,FORMA,KLA
#endif


#if defined(__IFC) || defined(__INTEL_COMPILER)
15	FORMAT(1X,A12,4X,A<LFNAM>,'|',A1,':',I<LNKAP>)
#endif

	if( FNINP .eq. 'file_name' ) FNINP = ' '
	if( FNOUT .eq. 'file_name' ) FNOUT = ' '
	if( FNADD .eq. 'file_name' ) FNADD = ' '

99	ISUC=0
100	RETURN

	END

	SUBROUTINE INPUT

#include "sadd.inc"
	character*4 nm_fmt(2)

	CALL INP_SHOWBUFF(IL)
	IF(IL.NE.0) CALL INP_SPNAME('Filename[|Format:Length]',FNINP,FORMI,KLI)

	KLCH=KLI*KAPPA
	CALL READSPEC(FNINP,RB1,FORMI,KLCH,KV)
	IF(KV.LE.0) GOTO 100
	KLI=(KV+KAPPA-1)/KAPPA

	LNKAP=1
	IF(KLI.GT. 9) LNKAP=2
	IF(KLI.GT.99) LNKAP=3
	LFNAM=LENGTHC(FNINP)
	write(nm_fmt(1),'(i4)')lfnam
	write(nm_fmt(2),'(i4)')lnkap
	WRITE(10,'('' INPUT '',4X,A'//nm_fmt(1)//',''|'',A1,'':'',I'//nm_fmt(2)//')') FNINP,FORMI,KLI
	WRITE(6,'('' INPUT '',4X,A'//nm_fmt(1)//',''|'',A1,'':'',I'//nm_fmt(2)//')')  FNINP,FORMI,KLI
*15	FORMAT(' INPUT ',4X,A<LFNAM>,'\',A1,':',I<LNKAP>)

99	ISUC=0

100	RETURN

	END

	SUBROUTINE OPERFILE(ITYP)

#include "sadd.inc"

	CHARACTER*80	CH80
	CHARACTER*1	CH1
	logical*1 norm
	character*5 nm_fmt1,nm_fmt2
	real*4 fak

	CALL INP_SHOWBUFF(IL)
	IF(IL.NE.0)CALL INP_SPNAME('Filename[|Format:Length]',FNADD,FORMA,KLA)
	KLCH=KLA*KAPPA

	IF(ITYP.EQ.1) THEN
	  FAK=1.00E0	  ! solo per ADD
	  norm=.false.
	  CALL INP_SHOWBUFF(IL)
	  IF(IL.GT.0) THEN
	    CALL INP_R1('Factor',FAK)
	  ELSEIF(il.lt.0) then
	    call inp_ch(' ',ch80)
	    norm=.true.
	  ENDIF
	ENDIF


	IF(FNADD(1:1).EQ.'#') THEN	  ! DATI DA REGISTRO
	  READ(FNADD(2:2),'(I1)',ERR=100) IST
	  IF(IST.LT.0 .OR. IST.GT.MAXREGIS) GOTO 100
	  DO II=0,KLCH-1
	  RB2(II)=RBSTOR(II,IST)
	  ENDDO
	ELSE	    	  ! DATI DA FILE
	  CALL READSPEC(FNADD,RB2,FORMA,KLCH,KV)
	  IF(KV.LE.0) GOTO 100
	ENDIF
	KLA=(KLCH+KAPPA-1)/KAPPA
	KLCH=KLA*KAPPA

	LNKAP=1
	IF(KLA.GT. 9) LNKAP=2
	IF(KLA.GT.99) LNKAP=3
	LFNAM=LENGTHC(FNADD)
	IF(FNADD(1:1).EQ.'#') THEN
	  WRITE(CH80,'(''Register#'',I1)') IST
	  CH1='R'
	ELSE
	  CH80=FNADD
	  CH1=FORMA
	ENDIF
	LFNAM=LENGTHC(CH80)
	LNKAP=1
	IF(KLA.GT. 9) LNKAP=2
	IF(KLA.GT.99) LNKAP=3

	GOTO (10,20,30) ITYP

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ADD !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

10	if(norm) then
	  area1=0
	  area2=0
	  do jh=0,klch-1
	    area1=area1+rb1(jh)
	    area2=area2+rb2(jh)
	  enddo
	  if(area2.ne.0) then
	    fak=-abs(area1/area2)
	  else
	    fak=-1.
	  endif
	endif

	DO JH=0,KLCH-1
	  RB1(JH)=RB1(JH)+FAK*RB2(JH)
	ENDDO
	WRITE(10,15) FAK,CH80(1:lfnam),CH1,KLA
	WRITE(6,15)  FAK,CH80(1:lfnam),CH1,KLA
#if defined(__GFORTRAN__)
15	FORMAT(' ADD',4X,'Factor:',G0,5X,A,'|',A1,':',I1)
#else
15	FORMAT(' ADD',4X,'Factor:',G,5X,A,'|',A1,':',I1)
#endif
	GOTO 99

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MULT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
20	DO JH=0,KLCH-1
	  RB1(JH)=RB1(JH)*RB2(JH)
	ENDDO
	WRITE(10,25) CH80(1:lfnam),CH1,KLA
	WRITE(6,25)  CH80(1:lfnam),CH1,KLA
25	FORMAT(' MUL',4X,A,'|',A1,':',I1)
	GOTO 99

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DIV !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
30	DO JH=0,KLCH-1
	  XX=RB2(JH)
	  IF(XX.EQ.0.) XX=1.
	  RB1(JH)=RB1(JH)/XX
	ENDDO
	WRITE(10,35) CH80(1:lfnam),CH1,KLA
	WRITE(6,35)  CH80(1:lfnam),CH1,KLA
35	FORMAT(' DIV',4X,A,'|',A1,':',I1)


99	ISUC=0
100	RETURN

	END

	SUBROUTINE OPERFACTOR(ITYP)

#include "sadd.inc"

	KLCH=KLI*KAPPA
	ICH1=0
	ICH2=KLCH-1

	CALL INP_R1('Factor,From_chan,To_chan',FAK)
	CALL INP_SHOWBUFF(IL)
	IF(IL.GT.0) CALL INP_I1('From_chan',ICH1)
	CALL INP_SHOWBUFF(IL)
	IF(IL.GT.0) CALL INP_I1('To_chan',ICH2)
	
	GOTO (10,20,30) ITYP

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ADDF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
10	DO JH=ICH1,ICH2
	  RB1(JH)=RB1(JH)+FAK
	ENDDO
	IF(ICH1.NE.1 .OR. ICH2.NE.KLCH) THEN
	  WRITE(10,14) FAK,ICH1,ICH2
	  WRITE( 6,14) FAK,ICH1,ICH2
#if defined(__GFORTRAN__)
14	  FORMAT(' ADD ',3X,'Factor:',G0,'   [',I6,'  --',I6,'  ]')
#else
14	  FORMAT(' ADD ',3X,'Factor:',G,'   [',I6,'  --',I6,'  ]')
#endif
	ELSE
	  WRITE(10,15) FAK
	  WRITE( 6,15) FAK
#if defined(__GFORTRAN__)
15	  FORMAT(' ADD ',3X,'Factor:',G0)
#else
15	  FORMAT(' ADD ',3X,'Factor:',G)
#endif
	ENDIF
	GOTO 99

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MULT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
20	DO  JH=ICH1,ICH2
	  RB1(JH)=RB1(JH)*FAK
	ENDDO
	IF(ICH1.NE.1 .OR. ICH2.NE.KLCH) THEN
	  WRITE(10,24) FAK,ICH1,ICH2
	  WRITE( 6,24) FAK,ICH1,ICH2
#if defined(__GFORTRAN__)
24	  FORMAT(' MULT',3X,'Factor:',G0,'   [',I6,'  --',I6,'  ]')
#else
24	  FORMAT(' MULT',3X,'Factor:',G,'   [',I6,'  --',I6,'  ]')
#endif
	ELSE
	  WRITE(10,25) FAK
	  WRITE(6,25)  FAK
#if defined(__GFORTRAN__)
25	  FORMAT(' MULT',3X,'Factor:',G0)
#else
25	  FORMAT(' MULT',3X,'Factor:',G)
#endif
	ENDIF
	GOTO 99

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DIVF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
30	IF(FAK.EQ.0.) GOTO 100
	DO JH=ICH1,ICH2
	  RB1(JH)=RB1(JH)/FAK
	ENDDO
	IF(ICH1.NE.1 .OR. ICH2.NE.KLCH) THEN
	  WRITE(10,34) FAK,ICH1,ICH2
	  WRITE( 6,34) FAK,ICH1,ICH2
#if defined(__GFORTRAN__)
34	  FORMAT(' DIV ',3X,'Factor:',G0,'   [',I6,'  --',I6,'  ]')
#else
34	  FORMAT(' DIV ',3X,'Factor:',G,'   [',I6,'  --',I6,'  ]')
#endif
	ELSE
	  WRITE(10,35) FAK
	  WRITE(6,35)  FAK
#if defined(__GFORTRAN__)
35	  FORMAT(' DIV ',3X,'Factor:',G0)
#else
35	  FORMAT(' DIV ',3X,'Factor:',G)
#endif
	ENDIF


99	ISUC=0
100	RETURN

	END

	SUBROUTINE MULT_CHANNEL

#include "sadd.inc"

	KLCH=KLI*KAPPA
	DO  JH=0,KLCH-1
	  RB1(JH)=RB1(JH)*JH
	ENDDO
	WRITE(10,25)
	WRITE(6,25)
25	FORMAT(' MULTIPLY BY CHANNEL NUMBER')

99	ISUC=0
100	RETURN

	END

	SUBROUTINE DIV_CHANNEL

#include "sadd.inc"

	KLCH=KLI*KAPPA
	RB1(0)=0.
	DO  JH=1,KLCH-1
	  RB1(JH)=RB1(JH)/JH
	ENDDO
	WRITE(10,25)
	WRITE(6,25)
25	FORMAT(' DIVIDE BY CHANNEL NUMBER')

99	ISUC=0
100	RETURN

	END

	SUBROUTINE SQRTSP

#include "sadd.inc"

	DO JH=0,KLI*KAPPA-1
	  XX=RB1(JH)
	  IF(XX.LT.0) XX=0
	  RB1(JH)=SQRT(XX)
	ENDDO

	WRITE(10,15)
	WRITE(6,15)
15	FORMAT(' SQRT')

99	ISUC=0
100	RETURN

	END

	SUBROUTINE SHIFT

#include "sadd.inc"

	DIMENSION RP(5)
	logical*1 forward

	ILIM(I1,I2)=MAX(0,MIN(I1,I2))

!	IF(BB(1).EQ.'@') THEN
!	  CALL IN0SHF(1)
!	  CALL IN0BLA
!	  IF(IL.LE.0) GOTO 1
!	  BB(IL+1)=0
!	  OPEN(UNIT=1,NAME=BB,STATUS='OLD',READONLY,ERR=1)
!	  READ(1,*,ERR=1,END=1) (RP(II),II=1,5)
!	  CLOSE (UNIT=1)
!	ELSE

1	CALL INP_RA('A0,A1,A2,A3,A4',RP,5)

	NORD=5
	DO II=5,1,-1
	  IF(RP(II).NE.0) GOTO 10
	  NORD=NORD-1
	ENDDO
10	IF(NORD.LE.0) GOTO 100

	KLCH=KLI*KAPPA
	KLCH1=KLCH-1
	DO II=0,KLCH1
	  RB2(II)=0.
	ENDDO
	XKLCH=KLCH

	YY1=POL(0.,RP,NORD)
	DO  JJ=0,KLCH1
	  YY2=POL(JJ+1.,RP,NORD)
	  IF(YY1.LE.YY2) THEN
	    FORWARD=.TRUE.
	  ELSE
	    FORWARD=.FALSE.
	    XX=YY1
	    YY1=YY2
	    YY2=XX
	  ENDIF
	  IY1=YY1
	  IY2=YY2
	  XX=RB1(JJ)
	  IF(IY2.EQ.IY1) THEN
	    KK=ILIM(IY1,KLCH1)
	    RB2(KK)=RB2(KK)+XX
	  ELSE
	    XX=XX/(YY2-YY1)
	    KK=ILIM(IY1,KLCH1)
	    RB2(KK)=RB2(KK)+XX*(IY1+1-YY1)
	    DO J=IY1+1,IY2-1
	      KK=ILIM(J,KLCH1)
	      RB2(KK)=RB2(KK)+XX
	    ENDDO
	    KK=ILIM(IY2,KLCH1)
	    RB2(KK)=RB2(KK)+XX*(YY2-IY2)
	  ENDIF
	  IF(FORWARD) YY1=YY2
	ENDDO

50	CALL VMOVR(RB2,1,RB1,1,KLCH)
	WRITE(10,15) (RP(II),II=1,NORD)
	WRITE(6,15)  (RP(II),II=1,NORD)
15	FORMAT(' SHIFT',2X,5G12.5)

99	ISUC=0
100	RETURN

	END
	
	SUBROUTINE SHIFT_LUT

#include "sadd.inc"

	character line*256
	CHARACTER*60 FNLUT

	integer inlu	/0/

	call INP_CH('Filename of the Look Up Table',FNLUT)
	if(inlu.le.0) call lib$get_lun(inlu)

	OPEN(UNIT=inlu,FILE=FNLUT,STATUS='OLD',READONLY,err=100)

	KLCH=KLI*KAPPA
	CALL VMOVR(0.,0,RB3,1,KLCH)

	ival=1
	ipos=0

	READ(inlu,'(a)',END=100) line
	read(line,*,ERR=10) ican
	goto 12
	
10	READ(inlu,'(a)',END=20) line
11	read(line,*,ERR=20) ican
12	do ii=ipos,ican
	  rb3(ii)=ival
	enddo
	write(6,'(''  ['',2i6,'' ] ===> '',i6)') ipos,ican,ival
	if(ival.lt.klch) GOTO 10

20	close(inlu)

	CALL VMOVR(0.,0,RB2,1,KLCH)

	do ii=0,klch-1
	  ival=rb3(ii)
	  if(ival.ne.0) rb2(ival)=rb2(ival)+rb1(ii)
	enddo

	CALL VMOVR(RB2,1,RB1,1,KLCH)

	lfnlut=lengthc(fnlut)
	WRITE(10,15) fnlut(1:lfnlut)
	WRITE(6,15)  fnlut(1:lfnlut)
15	FORMAT(' SHIFT_LUT  ',A)

99	ISUC=0
100	RETURN

	END
	
	SUBROUTINE SHIFT_SQRT

#include "sadd.inc"

	KLCH=KLI*KAPPA
	CALL VMOVR(0.,0,RB2,1,KLCH)

	XFACT=KLCH/SQRT(FLOAT(KLCH))
	XKLCH=KLCH
	YY1=0.
	DO  JJ=0,KLCH-1
	  XX=JJ
	  YY2=SQRT(XX)*XFACT
	  IY1=YY1
	  IY2=YY2
	  XX=RB1(JJ)
	  IF(IY2.EQ.IY1) THEN
	  KK=MAX(0,MIN(IY1,KLCH-1))
	  RB2(KK)=RB2(KK)+XX
	  ELSE
	  XX=XX/(YY2-YY1)
	  KK=MAX(0,MIN(IY1,KLCH-1))
	  RB2(KK)=RB2(KK)+XX*(IY1+1-YY1)
	  KK=MAX(0,MIN(IY2,KLCH-1))
	  RB2(KK)=RB2(KK)+XX*(YY2-IY2)
	  IF((IY2-IY1).GT.1) THEN
	    DO J=IY1+1,IY2-1
	      KK=MAX(0,MIN(J,KLCH-1))
	      RB2(KK)=RB2(KK)+XX
	    ENDDO
	  ENDIF
	  ENDIF
	  YY1=YY2
	ENDDO

50	CALL VMOVR(RB2,1,RB1,1,KLCH)
	WRITE(10,15) XFACT
	WRITE(6,15)  XFACT
#if defined(__GFORTRAN__)
15	FORMAT(' SHIFT_SQRT * ',G0)
#else
15	FORMAT(' SHIFT_SQRT * ',G)
#endif

99	ISUC=0
100	RETURN

	END
	
	SUBROUTINE SHIFT_SQUARE

#include "sadd.inc"

	KLCH=KLI*KAPPA
	CALL VMOVR(0.,0,RB2,1,KLCH)

	XFACT=FLOAT(KLCH)**2/KLCH
	XKLCH=KLCH
	YY1=0.
	DO  JJ=0,KLCH-1
	  XX=JJ
	  YY2=XX*XX/XFACT
	  IY1=YY1
	  IY2=YY2
	  XX=RB1(JJ)
	  IF(IY2.EQ.IY1) THEN
	  KK=MAX(0,MIN(IY1,KLCH-1))
	  RB2(KK)=RB2(KK)+XX
	  ELSE
	  XX=XX/(YY2-YY1)
	  KK=MAX(0,MIN(IY1,KLCH-1))
	  RB2(KK)=RB2(KK)+XX*(IY1+1-YY1)
	  KK=MAX(0,MIN(IY2,KLCH-1))
	  RB2(KK)=RB2(KK)+XX*(YY2-IY2)
	  IF((IY2-IY1).GT.1) THEN
	    DO J=IY1+1,IY2-1
	      KK=MAX(0,MIN(J,KLCH-1))
	      RB2(KK)=RB2(KK)+XX
	    ENDDO
	  ENDIF
	  ENDIF
	  YY1=YY2
	ENDDO

50	CALL VMOVR(RB2,1,RB1,1,KLCH)
	WRITE(10,15) XFACT
	WRITE(6,15)  XFACT
#if defined(__GFORTRAN__)
15	FORMAT(' SHIFT_SQUARE / ',G0)
#else
15	FORMAT(' SHIFT_SQUARE / ',G)
#endif

99	ISUC=0
100	RETURN

	END
	
	SUBROUTINE SMOOTH

#include "sadd.inc"

	KLCH=KLI*KAPPA
	CALL VMOVR(RB1,1,RB2,1,KLCH)
	X=RB2(0)
	Y=RB2(0)
	DO J=0,KLCH-2
	  Z=RB2(J+1)
	  RB1(J)=(X+2.*Y+Z)/4.
	  X=Y
	  Y=Z
	ENDDO
	Z=RB2(KLCH-1)
	RB1(KLCH-1)=(X+2.*Y+Z)/4.

	WRITE(10,15)
	WRITE(6,15)
15	FORMAT(' SMOOTH')

99	ISUC=0
100	RETURN

	END

	SUBROUTINE DERIVATIVE

#include "sadd.inc"

	KLCH=KLI*KAPPA
	CALL VMOVR(RB1,1,RB2,1,KLCH)
	RB1(0)=RB2(1)-RB2(0)
	DO J=1,KLCH-2
	  RB1(J)=(RB2(J+1)-RB2(J-1))/2
	ENDDO
	RB1(KLCH-1)=RB2(KLCH-1)-RB2(KLCH-2)

	WRITE(10,15)
	WRITE(6,15)
15	FORMAT(' DERIVATIVE')

99	ISUC=0
100	RETURN

	END

	SUBROUTINE LIST

#include "sadd.inc"

1	CALL INP_I2('Channel_from,Channel_to',ICH1,ICH2)
	KLCH=KLI*KAPPA
	ICH1=MAX(0,MIN(ICH1,KLCH-1))
	ICH2=MAX(0,MIN(ICH2,KLCH-1))
	IF(ICH2.LT.ICH1) ICH2=ICH1
	DO ICH=ICH1,ICH2
	  WRITE(6,*)ICH,RB1(ICH)
	ENDDO

99	ISUC=0
100	RETURN

	END

	SUBROUTINE CHANNELSET

#include "sadd.inc"
	real xch /-1./

	KLCH=KLI*KAPPA

1	XCH=XCH+1
	CALL INP_R2('$Channel, New_value',XCH,VAL)

	ICH=XCH
	IF(ICH.LT.0 .OR. ICH.GE.KLCH) GOTO 100
	XCH=ICH

	WRITE(10,15) ICH,RB1(ICH),VAL
	WRITE(6,15)  ICH,RB1(ICH),VAL
#if defined(__GFORTRAN__)
15	FORMAT(' CHANNEL',I8,5X,'Was',G0,5X,'Is',G0)
#else
15	FORMAT(' CHANNEL',I8,5X,'Was',G,5X,'Is',G)
#endif
	RB1(ICH)=VAL

99	ISUC=0
100	RETURN

	END

	SUBROUTINE SWAP_BYTE_LW

#include "sadd.inc"
	
	INTEGER LONG
	BYTE    BYTE(4)
	EQUIVALENCE (LONG,BYTE)

	KLCH=KLI*KAPPA
	DO J=0,KLCH-1
	  LONG=RB1(J)
	  CALL SWAPB(BYTE(1),BYTE(4))
	  CALL SWAPB(BYTE(2),BYTE(3))
	  RB1(J)=LONG
	ENDDO

	WRITE(10,15)
	WRITE(6,15)
15	FORMAT(' SWAP_BYTE_LW')

99	ISUC=0
100	RETURN

	END

	SUBROUTINE PACK_CHANNELS

#include "sadd.inc"

	KLCH=KLI*KAPPA
	IF(npack.lt.2) npack=2
	CALL INP_I1('Number of channels to pack together',npack)
	IF(npack.lt.2 .or. npack.gt.klch) goto 100
	
	jj=0
	do ii=0,klch-1,npack
	  sum=0
	  do ll=ii,ii+npack-1
	    sum=sum+rb1(ll)
	  enddo
	  rb1(jj)=sum
	  jj=jj+1
	enddo	
	do ii=jj,klch-1
	  rb1(ii)=0.
	enddo

	WRITE(10,15) npack
	WRITE(6,15)  npack
#if defined(__GFORTRAN__)
15	FORMAT(' PACK_CHANNELS  ',i0)
#else
15	FORMAT(' PACK_CHANNELS  ',i)
#endif

99	ISUC=0
100	RETURN

	END
	
	SUBROUTINE MADD

#include "sadd.inc"

	CHARACTER*80	CH80
	CHARACTER*1	CH1

	integer ntimes /1/
	integer tipo /1/

	CALL INP_SHOWBUFF(IL)
	IF(IL.GT.0) THEN
	  CALL INP_i1('How many spectra',ntimes)
	ENDIF
	CALL INP_SHOWBUFF(IL)
	IF(IL.ne.0) THEN
	   NN = INP_str('Filename to cycle',ch80)
	   if(ch80(1:1).eq.'+') then
	     tipo=1
	   elseif(ch80(1:1).eq.'-') then
	     tipo=2
	   elseif(ch80(1:3).eq.'=.+') then
	     tipo=3
	   elseif(ch80(1:3).eq.'=.-') then
	     tipo=4
	   else
	     goto 100
	   endif
	endif

	fnadd=fninp
	klch=kli*kappa
	do ii=1,ntimes
	  if(tipo.eq.1) then
	    call fnincrem(fnadd)
	  elseif(tipo.eq.2) then
	    call fndecrem(fnadd)
	  elseif(tipo.eq.3) then
	    call fnincrem_type(fnadd)
	  elseif(tipo.eq.4) then
	    call fndecrem_type(fnadd)
	  endif
	  
	  CALL READSPEC(FNADD,RB2,FORMA,KLCH,KV)
	  IF(KV.LE.0) GOTO 100
	  KLA=(KLCH+KAPPA-1)/KAPPA
	  KLCH=KLA*KAPPA

	  LNKAP=1
	  IF(KLA.GT. 9) LNKAP=2
	  IF(KLA.GT.99) LNKAP=3
	  LFNAM=LENGTHC(FNADD)
	  CH80=FNADD
	  CH1=FORMA
	  LFNAM=LENGTHC(CH80)
	  LNKAP=1
	  IF(KLA.GT. 9) LNKAP=2
	  IF(KLA.GT.99) LNKAP=3

	  DO JH=0,KLCH-1
	    RB1(JH)=RB1(JH) + RB2(JH)
	  ENDDO
	  WRITE(10,15) CH80(1:lfnam),CH1,KLA
	  WRITE(6,15)  CH80(1:lfnam),CH1,KLA
15	  FORMAT(' ADD',4X,A,'|',A1,':',I1)
	enddo

99	ISUC=0
100	RETURN

	END

	SUBROUTINE DISPLAY

#include "sadd.inc"

	K1=1
	K2=KLI
	CALL INP_SHOWBUFF(IL)
	IF(IL.GT.0) CALL INP_I2('K1, K2',K1,K2)
	K1=MAX(1,MIN(K1,KLI))
	K2=MAX(1,MIN(K2,KLI))
	IF(K2.LT.K1) K2=K1

	YMIN=1.E30
	YMAX=-1.E30
	K1CH=(K1-1)*KAPPA
	K2CH=K2*KAPPA-1
	DO J=K1CH,K2CH
	  YMIN=MIN(YMIN,RB1(J))
	  YMAX=MAX(YMAX,RB1(J))
	ENDDO
	YSCA=YMAX-YMIN
	IF(YSCA.EQ.0.) YSCA=1.
	YSCA=770./YSCA
	Y0=10.-YMIN*YSCA
	IDX=K2-K1
	CALL INITT(960)
	IX=1
	IY=Y0
	CALL MOVABS(IX,IY)
	DO J=K1CH,K2CH,IDX+1
	  YY=0.
	  DO IJ=J,J+IDX
	  IF(ABS(YY).LT.ABS(RB1(IJ))) YY=RB1(IJ)
	  ENDDO
	  IY=IFIX(Y0+YY*YSCA)
	  CALL DRWABS(IX,IY)
	  IX=IX+1
	ENDDO

	CALL HOME
	CALL ANMODE
	WRITE(6,15) K1,K2,YMIN,YMAX
#if defined(__GFORTRAN__)
15	FORMAT('+K1=',I3,5X,'K2=',I3,5X,'Ymin=',G0,5X,'Ymax=',G0)
#else
15	FORMAT('+K1=',I3,5X,'K2=',I3,5X,'Ymin=',G,5X,'Ymax=',G)
#endif

99	ISUC=0
100	RETURN

	END

	SUBROUTINE OUTPUT

#include "sadd.inc"
	character*4 nm_fmt(2)

	CALL INP_SHOWBUFF(IL)
	IF(IL.NE.0)CALL INP_SPNAME('Filename[|Format:Length]',FNOUT,FORMO,KLO)

	KLCH=KLO*KAPPA
	CALL WRITESPEC(FNOUT,RB1,FORMO,KLCH,KV)
	IF(KV.LE.0) GOTO 100
	KLO=(KV+KAPPA-1)/KAPPA

	LNKAP=1
	IF(KLO.GT. 9) LNKAP=2
	IF(KLO.GT.99) LNKAP=3
	LFNAM=LENGTHC(FNOUT)
	write(nm_fmt(1),'(i4)')lfnam
	write(nm_fmt(2),'(i4)')lnkap
	WRITE(10,'('' OUT '',4X,A'//nm_fmt(1)//',''|'',A1,'':'',I'//nm_fmt(2)//')') FNOUT(1:lfnam),FORMO,KLO
	WRITE(6,'('' OUT '',4X,A'//nm_fmt(1)//',''|'',A1,'':'',I'//nm_fmt(2)//')')  FNOUT(1:lfnam),FORMO,KLO
*15	FORMAT(' OUT',4X,A,'|',A1,':',I1)

99	ISUC=0
100	RETURN

	END

	SUBROUTINE STORESPEC

#include "sadd.inc"
	character*16 chreg /'Register (0...3)'/

	CALL INP_SHOWBUFF(IL)
	IF(IL.GT.0) THEN	  ! Register number presente
	  JREG=IREG
	  write(chreg(15:15),'(i1)') MAXREGIS
	  CALL INP_I1(chreg,JREG)
	  IF(JREG.LT.0 .OR. JREG.GT.MAXREGIS) GOTO 100
	  IREG=JREG
	ENDIF

	DO JH=0,KLI*KAPPA-1
	  RBSTOR(JH,IREG)=RB1(JH)
	ENDDO

	WRITE(10,15)IREG
	WRITE(6,15)IREG
15	FORMAT(' STORE',I1)

99	ISUC=0
100	RETURN

	END

	SUBROUTINE RECALLSPEC

#include "sadd.inc"
	character*16 chreg /'Register (0...3)'/

	CALL INP_SHOWBUFF(IL)
	IF(IL.GT.0) THEN	  ! Register number presente
	  JREG=IREG
	  write(chreg(15:15),'(i1)') MAXREGIS
	  CALL INP_I1(chreg,JREG)
	  IF(JREG.LT.0 .OR. JREG.GT.MAXREGIS) GOTO 100
	  IREG=JREG
	ENDIF

	DO JH=0,KLI*KAPPA-1
	  RB1(JH)=RBSTOR(JH,IREG)
	ENDDO

	WRITE(10,15)IREG
	WRITE(6,15)IREG
15	FORMAT(' RECALL',I1)

99	ISUC=0
100	RETURN

	END

	SUBROUTINE SADDSPAWN

#include "sadd.inc"

	CHARACTER*80	DCLCOM
	
	ISUC=-1

	IIC=INP_STR('Shell command>',DCLCOM)
	IF(IIC.LT.0) GOTO 99
	IL=LENGTHC(DCLCOM)
	IF(IL.GT.0) THEN
	  call Lib$Spawn(DCLCOM)
	ELSE
	  call Lib$Spawn()
	ENDIF

*	IF (.NOT.ISTATUS) THEN
*	  write( *,* )' DCL couldn''t spawn the subprocess '
*	  GOTO 100
*	ENDIF

99	ISUC=0
	
100	RETURN

	END

	SUBROUTINE SERVDEF

#include "sadd.inc"

	CHARACTER*80	LINE

	CALL INP_MSG('    Service definition ')
	NSVC=0
	DO I=1,20
1	  LINE=' '
	  CALL INP_MODE(0)
	  NN=INP_STR('DEF> ',LINE)
	  IF(NN.LE.0) GOTO 99
	  IF(LINE(1:2).EQ.'??') THEN
	  DO II=1,NSVC
	    WRITE(6,'(1X,A)') SERV(II)
	  ENDDO
	  GOTO 1
	  ENDIF
	  NSVC=NSVC+1
	  SERV(NSVC)=LINE(1:NN)//' '
	ENDDO

99	IF(NSVC.GT.0) ISUC=0
100	RETURN

	END

	SUBROUTINE SERVICE

#include "sadd.inc"

	IF(NSVC.EQ.0) GOTO 100

	ICOUNT=1
	CALL INP_I1('Count',ICOUNT)
	IF(ICOUNT.LE.0) GOTO 100
	ISVC=0

99	ISUC=0
100	RETURN

	END
