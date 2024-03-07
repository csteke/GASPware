# 1 "gsort.F"
	PROGRAM GSORT
C
C $Id: gsort.for,v 6.11 1999/10/07 13:20:08 nicu Exp nicu $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       GS0_... definizione      (GETINPUT)  C
C       GS1_... lista            (LISTPROG)  C
C       GS3_... inizializzazione (INIT_RUN)  C
C       GS3_... esecuzione       (EVANA)     C
C       GS4_... finalizzazione   (FINIT_RUN) C
C       GS5_... opzionale                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


# 1 "./gsort.inc" 1 
# 1 "./gsort.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./gsort.def" 2 
# 1 "./gsort.par" 1 
	INTEGER LU1		! LU per messaggi
	PARAMETER (LU1=6)
	INTEGER LU2		! LU per Logbook
	PARAMETER (LU2=7)
	INTEGER INDENT		! incolonnamento listato programma
	PARAMETER (INDENT=20)
*	INTEGER NPRINT		! intervallo per stampa status su LU1
*	PARAMETER (NPRINT=400)
	INTEGER NPRINT2		! intervallo per stampa status su LU2
	PARAMETER (NPRINT2=2000)
	INTEGER NSTOP		! intervallo per check di mat_stop
	PARAMETER (NSTOP=1000)
	INTEGER NSYNT		! # righe per la sintassi
	PARAMETER (NSYNT=5)

	INTEGER MAXRES		! Risoluzione massima gestita
	PARAMETER (MAXRES=32768)
	INTEGER MINRES		! Risoluzione minima gestita
	PARAMETER (MINRES=16)
	INTEGER MAXDET		! MAX # di rivelatori
	PARAMETER (MAXDET=256)
	INTEGER MAXPAR		! MAX # di parametri/rivelatore
	PARAMETER (MAXPAR=16)
	INTEGER NDTYPE		! Numero di tipi di rivelatore
	PARAMETER (NDTYPE=16)
	REAL PSTEP
	PARAMETER (PSTEP=359./1024.)

	INTEGER NOFORMAT	! No format defined
	PARAMETER (NOFORMAT=-1)
	INTEGER GASP		! Dati tipo GASP
	PARAMETER (GASP=1)
	INTEGER EURO		! Dati tipo Euroball
	PARAMETER (EURO=2)
	INTEGER PI8		! Dati tipo 8Pi-Berkeley
	PARAMETER (PI8=3)
	INTEGER GSPH		! Dati tipo GAMMASPHERE
	PARAMETER (GSPH=4)
	INTEGER GSPN		! Dati tipo GASP - nuova DAQ (2001)
	PARAMETER (GSPN=5)
	INTEGER YALE		! Dati tipo YrastBall - Yale Univ.
	PARAMETER (YALE=6)
	INTEGER TNDB
	PARAMETER (TNDB=7)	! Format Tandem-Bucharest
	INTEGER PRISMA
	PARAMETER (PRISMA=8)	! Format PRISMA (spettrometro)
	INTEGER GSR
	PARAMETER (GSR=9)       ! Temporary, GSPH reduced data
	INTEGER GANIL
	PARAMETER (GANIL=10)    ! GANIL data (EXOGAM,DIAMANT,NWALL)
	
	INTEGER CSEG,QSEG,TSEG,ISEG,SISEG, PPACSEG, DANTESEG
	PARAMETER (CSEG=7, QSEG=4, TSEG=1, ISEG=1, SISEG=4, PPACSEG=10, DANTESEG=8)
	INTEGER CDET,QDET,TDET,SIDET, PPACDET
	PARAMETER (CDET=15, QDET=26, TDET=30, SIDET=40, PPACDET=1)
	INTEGER COFF,QOFF,TOFF,FOFF
	PARAMETER (COFF=0, QOFF=COFF+CDET*CSEG, TOFF=QOFF+QDET*QSEG, FOFF=TOFF+TDET*TSEG)

	INTEGER MAXNBAN		! MAX # banane per BANANA e PIN
	PARAMETER (MAXNBAN=32)
	INTEGER MAXRCL_ORD	! MAX # coefficienti di ricalibrazione
	PARAMETER (MAXRCL_ORD=6)
	INTEGER MAXRCL_REGS	! MAX # regioni di ricalibrazione
	PARAMETER (MAXRCL_REGS=6)

	INTEGER MAXTADJ		! MAX # di parametri in time_adjust
	PARAMETER (MAXTADJ=16)

	INTEGER MAXHASH		! MAX # liste hashgates
	PARAMETER (MAXHASH=50)

	INTEGER MAXPAIRS	! MAX # liste ind=f(id,id) definibili
	PARAMETER (MAXPAIRS=4)
	
	INTEGER MAXMATDIM	! massima dimensione della matrice
	PARAMETER (MAXMATDIM=4)

	INTEGER DEFSTEP2D	! suddivisione default 2D
	PARAMETER (DEFSTEP2D=128)
	INTEGER DEFSTEP3D	! suddivisione default 3D
	PARAMETER (DEFSTEP3D=64)
	INTEGER DEFSTEP4D	! suddivisione default 4D
	PARAMETER (DEFSTEP4D=32)

	INTEGER DEFSTEP2DS	! suddivisione default 2D_symm
	PARAMETER (DEFSTEP2DS=128)
	INTEGER DEFSTEP3DS	! suddivisione default 3D_symm
	PARAMETER (DEFSTEP3DS=64)
	INTEGER DEFSTEP4DS	! suddivisione default 4D_symm
	PARAMETER (DEFSTEP4DS=32)

	INTEGER DEFSTEP2DH	! suddivisione default 2D_hsymm
	PARAMETER (DEFSTEP2DH=32)
	INTEGER DEFSTEP3DH	! suddivisione default 3D_hsymm
	PARAMETER (DEFSTEP3DH=8)
	INTEGER DEFSTEP4DH	! suddivisione default 4D_hsymm
	PARAMETER (DEFSTEP4DH=8)

	INTEGER MAXBYTES	! Max. lunghezza records su nastro
	PARAMETER (MAXBYTES=32*1024)
	INTEGER MAXWORDS	! Max. lunghezza records su nastro
	PARAMETER (MAXWORDS=MAXBYTES/2)
	INTEGER MAXEVL		! MAX lunghezza evento (words)
	PARAMETER (MAXEVL=2047)

	INTEGER MTFLAGR		! Flag per sync. nastro input
	PARAMETER (MTFLAGR=1)
	INTEGER MTFLAGW		! Flag per sync. nastro output
	PARAMETER (MTFLAGW=2)

	INTEGER NFORMCOM	! # comandi di Formato
	PARAMETER (NFORMCOM=9)
	INTEGER NDECLCOM	! # comandi di Dichiarazione
	PARAMETER (NDECLCOM=9)
	INTEGER NANALCOM	! # comandi di Analisi
	PARAMETER (NANALCOM=199)

	INTEGER MAXCOMANDI	! MAX # di COMANDI
	PARAMETER (MAXCOMANDI=360)
# 3 "./gsort.def" 2 

	structure/detector/
	  union
	    map
	      integer id
	    endmap
	    map
	      integer ival(-1:maxpar-1)
	    endmap
	  endunion
	  real    xval( 0:maxpar-1)
	endstructure

	structure/hashdat/
	  INTEGER IND,PAR,RES			! su quale parametro
	  INTEGER WHICH				! gate number
	  logical*1 SAMEFORALL			! same gate for all detectors
	  logical*1 FROMFILE			! dati da file
	  INTEGER NGATES(0:MAXDET-1)		! quanti gates ha letto
	  CHARACTER*72 FILE			! File dei gates
	  REAL    DIST2(0:1)			! Distanza**2 normalizzata per gate sferici
	endstructure
	
	structure/pairsdat/
	  INTEGER INDMAX
	  CHARACTER*72 FILE			! nomi dei file con i dati
	  INTEGER PIND(0:MAXDET-1,0:MAXDET-1)
	endstructure

	structure/recall/
	  logical*1 always
	endstructure

	structure/fold/
	  INTEGER IND		! Tipo di parametro
	  INTEGER MIN		! Finestra sul fold
	  INTEGER MAX		! Finestra sul fold
	endstructure

	structure/gate/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  logical*1 INSIDE	! dentro/fuori
	  INTEGER LO		! Estremo inferiore
	  INTEGER HI		! Estremo superiore
	  INTEGER NGATES	! Quanti intervalli
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	endstructure

	structure/gatesdat/
	  logical*1 BAD(0:1)
	endstructure

	structure/filter/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PARS		! parametro sorgente
	  INTEGER PARD		! parametro destinazione
	  INTEGER RES		! Risoluzione filtro
	  character*80 file     ! no comment
	  integer iform         !
	  real F(0:MAXRES)        !
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	  logical*1 initialized   !
	endstructure

	structure/window/
	  INTEGER  IND		! Tipo di parametro
	  INTEGER  LO(0:MAXPAR-1)! Valore inferiore
	  INTEGER  HI(0:MAXPAR-1)! Valore superiore
	  INTEGER  FL		! Minfold
	  INTEGER  FH		! Minfold
	  logical*1  SAME		! Same as before window
	endstructure

	structure/banana/
	  INTEGER IND(2)	! Tipo di parametro x,y
	  INTEGER PAR(2)	! Parametro asse x,y
	  INTEGER RES(2)	! Risoluzione della banana
	  INTEGER NBAN		! Numero di banane
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 INSIDE	! In-Out
	  logical*1 SAME		! Same as before banana
	  logical*1 multiadc      ! N.M. - for ADC-dependent banana
	  logical*1 ignore(0:MAXDET-1) ! only for ADC-dependent banana
	endstructure

	structure/bananadat/
	  character*72 file
	  integer ban(2,0:1)
	endstructure

	structure/bananasdat/
	  character*72 file(MAXNBAN)
	  integer ban(2,0:1)
	endstructure

	structure/pairgate/
	   integer IND
	   integer LO
	   integer HI
	   integer FL
	   integer FH
	   integer PAR
	   integer RES
	   integer*2 PVAL(0:MAXDET-1,0:MAXDET-1)
	   integer*2 LIST(0:MAXDET-1)
	   character*72 filename
	   logical*1 INSIDE
	   logical*1 SAME
	endstructure

	structure/pin/
	  INTEGER IND(4)	! Tipo di parametro per banane
	  INTEGER PAR(4)	! Parametro asse x,y ; la massa e il tipo della particella
	  INTEGER RES(2)	! Risoluzione delle banane
	  INTEGER FIX		! Parametro fisso per il risultato
	  INTEGER FIXRES        ! Risoluzione dell parametro fisso
	  INTEGER NBAN		! quante banane
	  INTEGER F1(MAXNBAN)	! Numero di particelle della banana
	  INTEGER F2(MAXNBAN)	! Peso della banana in PIN
	  INTEGER F3(MAXNBAN)	! Massa della particella
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before PIN
	  logical*1 multiadc(MAXNBAN)          ! N.M. - for ADC-dependent banana
	  logical*1 ignore(MAXNBAN,0:MAXDET-1) ! only for ADC-dependent banana
	endstructure

	structure/hk/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER FIXH		! Parametro fisso per H
	  INTEGER FIXk		! Parametro fisso per k
	  REAL	  OFFS		! Offset per H
	  REAL	  GAIN		! Guadagno finale per H
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	endstructure

	structure/recal/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 RUN		! coefficenti run-dependent
	  logical*1 SAME	! Same as before recal
	  logical*1 ROUND       ! Round the result to integer value
	  logical*1 ZERO        ! Set negative result to zero
	  CHARACTER*73 FILE	! files dei coefficenti
	endstructure

	structure/recal_choose/
	  INTEGER IND(3)	! Tipo di parametro
	  INTEGER PAR(3)	! Quale parametro
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL    VAL           ! Valore di riferimento per fare la scelta
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  CHARACTER*73 FILE1	! files dei coefficenti
	  CHARACTER*73 FILE2	! files dei coefficenti
	  logical*1 RUN		! coefficenti run-dependent
	  logical*1 SAME	! Same as before recal
	  logical*1 ROUND       ! Round the result to integer value
	endstructure

	structure/calcoef/
	  INTEGER*4 ORD
	  REAL*4    COEF(MAXRCL_ORD)
	endstructure

	structure/mcalcoef/
	  integer nregs
	  INTEGER ORD(MAXRCL_REGS)
	  REAL    COEF(MAXRCL_ORD,MAXRCL_REGS)
	  real    limit(0:MAXRCL_REGS)
	endstructure


	structure/doppler/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET		! Quanti rivelatori
         real*8  fact1
         real*8  fact2
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL	  VC0		! Velocita' di rinculo
	  real    rtheta
	  real    rphi
	  REAL	  E0		! VC0 fino a E0
	  REAL	  E1		! Da VC0 a VC1 tra E0 e E1
	  REAL	  VC1		! VC1 sopra E1
	  REAL	  SLOPE		! Slope tra E0 e E1
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
         logical*1 RefChanged
	  logical*1 CONST		! Costante
	  logical*1 SAME		! Same as before recal
	  CHARACTER*73 FILE	! file con gli angoli
	endstructure

	structure/dopplerdat/
	  real costheta(0:1)
	endstructure
	
	structure/polar/
         real*8  fact1
         real*8  fact2
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET		! Quanti rivelatori
          INTEGER DTHETA        ! Angolo THETA del rivelatore, se viene dato come parametro
          INTEGER DPHI          ! Angolo PHI del rivelatore, se viene dato come parametro
	  INTEGER IND_POLAR
	  INTEGER PTHETA
	  INTEGER PPHI
         INTEGER IND_VEL
         INTEGER PVEL
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL	  VC0		! Velocita' di rinculo
	  REAL	  E0		! VC0 fino a E0
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
          logical*1 FIXED
	  logical*1 VarDetAngle	! Se angoli degli rivelatori sono parametri
	  logical*1 SAME		! Same as before recal
	  CHARACTER*73 FILE	! file con gli angoli
	endstructure
	
	structure/polardat/
	  real cdir(3,0:MAXDET-1)
	endstructure

	structure/meanvalstr/
	  INTEGER IND1(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR1(MAXTADJ)	! Quale parametro
	  INTEGER RES1(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY1	! Quanti definiti
	  REAL    POSITION	! Posizione finale
	  REAL	  FACTOR	! Rejection factor
	  INTEGER IND2	! FIX only
	  INTEGER PAR2	! Quale parametro fisso
	  INTEGER RES2	! Risoluzione
	endstructure

	structure/tadjust/
	  INTEGER IND1(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR1(MAXTADJ)	! Quale parametro
	  INTEGER RES1(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY1	! Quanti definiti
	  REAL    POSITION	! Posizione finale
	  REAL	  FACTOR	! Rejection factor
	  INTEGER IND2(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR2(MAXTADJ)	! Quale parametro
	  INTEGER RES2(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY2	! Quanti definiti
	endstructure

	structure/tref/
	   integer*8 N
# 285

	   real*16 SQSUM

	   INTEGER IND
	   INTEGER PAR
	   INTEGER RES
	   INTEGER REFNO
	   REAL POSITION
	endstructure

	structure/kine/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET_GE	! quanti germani
	  INTEGER NDET_SI	! quanti silici
	  INTEGER BIND(2)	! Tipo di parametro per banane
	  INTEGER BPAR(2)	! Parametro asse x,y
	  INTEGER BRES(2)	! Risoluzione della banana
	  INTEGER NBAN		! Numero di banane
	  INTEGER PINFIX	! Fixpar per PIN (se >=0)
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before recal
	  CHARACTER*72 KINEFILE	! file di descrizione
	  CHARACTER*72 AFILE_GE	! angoli germanio
	  CHARACTER*72 AFILE_SI	! angoli silici
	endstructure

	structure/kinedat/
	  REAL    ACMP,ECMP		! il nucleo composto
	  REAL    XMOMCM		! suo momento
	  REAL    AD(MAXNBAN)		! massa della particella
	  INTEGER MDET(MAXNBAN)		! numero di rivelatori attivi
	  INTEGER F1(MAXNBAN)		! Numero di particelle della banana per PIN
	  INTEGER F2(MAXNBAN)		! Peso della banana per PIN
	  real mom_si(0:MAXDET-1,MAXNBAN)
	  real cdir_ge(3,0:MAXDET-1)
	  real cdir_si(3,0:MAXDET-1)
	  record/bananasdat/kban
	endstructure

	structure/kinenew/
	  INTEGER IND(3)		! Tipo di parametro
	  INTEGER PAR(3)		! Parametro
	  INTEGER RES(3)		! Risoluzione del parametro
	  INTEGER NDET_GE	! quanti germani
	  INTEGER NDET_SI	! quanti silici
	  real ecal
	  real acmp
	  real ecmp
	  real atenuation
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before recal
	  CHARACTER*72 AFILE_GE	! angoli germanio
	  CHARACTER*72 AFILE_SI	! angoli silici
	  character*73 except_file
	  logical*1 except
	endstructure

	structure/kinenewdat/
	  REAL    ACMP,ECMP		! il nucleo composto
	  REAL    XMOMCM		! suo momento
	  real    ecal
	  real    afac
	  real cdir_ge(3,0:MAXDET-1)
	  real cdir_si(3,0:MAXDET-1)
	  integer nregs
	  integer reg(MAXRES,2)
	endstructure

	structure/sltocm/
	  integer IND(2)
	  integer PAR(2)
	  integer NDET_SI
	  real F(2)
	  real COS_TH(0:MAXDET-1)
	  CHARACTER*72 AFILE_SI	! angoli silici
	endstructure


	structure/add/
	  INTEGER IND(3)	! Tipo di parametro
	  INTEGER PAR(3)	! Parametro 1+2==>3
	  REAL	  FAC(3)	! fattori moltiplicativi
	  REAL    OFFSET	! offset da sommare al risultato
	  REAL    GAIN		! gain sul risultato (non sul'offset)
	  INTEGER ICHAN		! canale limite per COMBINE
	  INTEGER DELTA		! Sliding range
	  INTEGER SLIDE		! Sliding value
	  logical*1 CHECK		! check che il secondo parametro sia > limit
	  logical*1 MULT		! fattori moltiplicativi?
	endstructure

	structure/kill/
	  INTEGER IND			! Tipo di parametro
	  logical*1 RUN			! run dependent selective kill
	  CHARACTER*72 FILE		! files contenente i detbad
	  logical*1 DET(0:MAXDET-1)	! Quali rivelatori
	endstructure

	structure/killdat/
	  logical*1 remove(0:MAXDET-1,0:MAXDET-1)
	endstructure

	structure/select/
	  INTEGER IND		! Tipo di parametro
	  logical*1 DET(0:MAXDET-1)  ! Quali rivelatori
	endstructure

	structure/listev/
	  logical*1 TOFILE	! list events on file
	  logical*1 ISOPEN
	  INTEGER LUN
	  CHARACTER*72 FILE
	endstructure

	structure/reorder/
	  INTEGER IND		! Tipo di parametro
	endstructure

	structure/statistics/
	  INTEGER WHICH
	endstructure

	structure/statdat/
	  INTEGER FOLD(0:MAXDET,0:NDTYPE,0:1)
	endstructure

	structure/swap/
	  INTEGER IND(2)	! Tipo di parametro x,y
	  INTEGER PAR(2)	! Parametro asse x,y
	endstructure

	structure/mask/
	  INTEGER IND		! Tipo di parametro x
	  INTEGER PAR		! Parametro asse x
	  INTEGER MASK		! Maschera
	  logical*1 ONE		! solo un parametro o tutti?
	endstructure

	structure/useful/
	  INTEGER IND		! Tipo di parametro (deve essere 0)
	  INTEGER PAR1		! Parametro per RUN#
	  INTEGER PAR2		! Parametro per REC#
	  INTEGER PAR3		! Parametro per EVCOUNT#
	  INTEGER PAR4		! Parametro per EVNUMBER#
	endstructure

	structure/move/
	  INTEGER IND1			! da quale tipo
	  INTEGER IND2			! a  quale tipo
	  INTEGER PAR1			! da quale parametro
	  INTEGER PAR2			! a  quale parametro
	  INTEGER OFFSET                ! offset alla nuova numerazione
	  logical*1 CONDITION		! TRUE if GATE or copy detector
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
c	  logical*1 INSIDE	! dentro/fuori
	  INTEGER LO		! Estremo inferiore
	  INTEGER HI		! Estremo superiore
c	  INTEGER NGATES	! Quanti intervalli
c	  INTEGER FL		! Minfold
c	  INTEGER FH		! Maxfold
c	  logical*1 SAME		! Same as before gate
	  INTEGER HOWMANY		! Howmany to move
	  INTEGER WHICH(0:MAXDET-1)	! Quali rivelatori
	endstructure

	structure/splitmerge/
	  INTEGER NIND			! quanti tipi di rivelatore coinvolti
	  INTEGER IND(0:NDTYPE)		! Quali rivelatori
	  logical*1 REMOVE(0:NDTYPE)	!
	  INTEGER OFFSET(0:NDTYPE)	! Offset degli indici
	endstructure

	structure/newid/
	  INTEGER IND			! tipo di rivelatore
	  INTEGER LUT(0:MAXDET-1)	! mappa dei nuovi indici
	  logical*1 REORDER		! reordina dopo la mappatura
	  CHARACTER*72 FILE		! files dei nuovi id
	endstructure

	structure/addback/
	  INTEGER IND			! Tipo di parametro
	  INTEGER PAR			! Quale parametro
	  INTEGER RES			! Risoluzione del parametro
	  INTEGER NSEGS			! quanti segmenti (se composito)
	  logical*1 REJECT		! elimina le doppie non contigue
	  logical*1 PATTERN		! produce la hit-pattern sommando tutto
	  INTEGER PATPAR		! dove la registra
	  logical*1 TGATE			! verifica la relazione temporale?
	  INTEGER TPAR			! su quale parametro
	  INTEGER TVAL			! massima deviazione
	endstructure

        structure /pms_str/
	    real*8  sin_70
	    real*8  sin_110
	    real*8  cos_20
	    real*8  tan_20
	    integer ind
	    integer par_r
	    integer par_p
	    integer par_e
	    integer par_icp
	    integer ind_theta
	    integer par_theta
	    integer ind_path
	    integer par_path
	    integer ind_x
	    integer par_x
	    integer ind_ic
	    integer par_a
	    integer par_b
	    integer par_c
	    integer par_d
	    real    thr
	    real    qfact
	end structure
	
	structure /pms_dat/
	   real*8 R(40,0:100)
	end structure
	 
	structure /qvalue_str/
	   real*8  costhp
	   real*8  sinthp
	   real*8  amu
	   integer ind_q
	   integer par_q
	   integer ind_mass
	   integer par_mass
	   integer ind_theta
	   integer par_theta
	   integer ind_phi
	   integer par_phi
	   integer ind_beta
	   integer par_beta
	   integer AP
	   integer AT
	   integer ATOT
	   integer    low
	   integer    high
	   real    ep
	   real    thp
	   real    gain
	   real    offset
	end structure

       structure /prisma_angles_str/
          real*8  prisma_angle
          real*8  D
          real*8  sinalpha
          real*8  cosalpha
          real*8  costhp
          real*8  sinthp
	  real*8  cx(0:3)
	  real*8  cy(0:3)
          integer ind
          integer parx
          integer pary
          integer parz
          integer parq
          integer part
	  integer parp
	  integer pard
	  integer degx
	  integer degy
	  integer oldstyle
	  character*128 calfile
       end structure
	   
	structure/proje/
	  INTEGER LEN		! Lunghezza totale degli spettri (LW)
	  INTEGER MFRES 	! risoluzione max. di F
	  INTEGER*8 NINCR
	endstructure

	structure/projedat/
	  CHARACTER*16 NAME(0:MAXPAR-1,0:NDTYPE)
	  INTEGER      ADDR(0:MAXDET-1,0:MAXPAR-1,0:NDTYPE)
	  INTEGER      SPEC(0:1)
	endstructure

	structure/spectrum/
	  INTEGER IND			! Tipo di parametro
	  INTEGER PAR			! Quale parametri
	  INTEGER RES			! numero di canali dello spettro
	  INTEGER IHASH			! Quale tabella di hash (se >=0)
	  INTEGER HIND,HPAR		! riportati qui per comodita'
	  logical*1 HSAME			! IND e' quello su cui applicare HGATE
	  INTEGER NHASH			! Quante volte
	  logical*1 SPHERICAL		! gate di tipo elissoidale
	  INTEGER MINFOLD		! fold minimo (per hsort1d)
	  INTEGER*8 NINCR		! numero di incrementi
	  CHARACTER*72 FILE		! Filename dello spettro
	endstructure

	structure/specdat/
	  INTEGER SPEC(0:1)
	endstructure

	structure/matrix/

	  integer*8 CMTBASE

	  INTEGER NDIM			! Ordine della matrice
	  INTEGER IND(MAXMATDIM)	! Tipo di parametri per i due lati
	  INTEGER PAR(MAXMATDIM)	! Quale dei parametri
	  INTEGER RES(MAXMATDIM)	! Dimensioni lati della matrice
	  INTEGER STEP(MAXMATDIM)	! suddivisione lati della matrice
	  INTEGER MATMODE		! 0=normal 1=symmetric 2=halfsymmetric
	  INTEGER MSDIM			! numero di indici simmetrizzati
	  INTEGER DOFFSET		! per SORTxD_DIFF
	  INTEGER NIND
	  INTEGER IHASH			! Quale tabella di hash (se >=0)
	  INTEGER HIND,HPAR		! riportati qui per comodita'
	  logical*1 HSAME			! IND e' quello su cui applicare HGATE
	  INTEGER NHASH			! quante volte
	  logical*1 SPHERICAL		! gate di tipo elissoidale
	  INTEGER TYPE
	  INTEGER IND1,IND2,IND3,IND4
	  INTEGER PAR1,PAR2,PAR3,PAR4
	  logical*1 HHH1,HHH2,HHH3,HHH4
	  CHARACTER*72 FILE		! Filename matrice
	endstructure

	structure/iodef/
	  logical*1 datafile	! TRUE se da file o Virtuale
	  logical*1 virtuale
	  logical*1 closed
	  integer lun
	  integer flag
	  integer wlen
	  integer run
	  integer rec
	  integer maxrec
	  integer evcount	! internal count
	  integer evnumber	! from ACQ
	  integer vrun,vevents
CVMS	  integer*2 iosb(4)
	  integer iosb(4)
CVMS	  integer*2 mtch,hctm
	  integer mtch
	  character*72 name
	endstructure

	structure/oevdef/
	  logical*1 defined
	  logical*1 same				! Same FILE structure on output
	  logical*1 sameRUN			! Same RUN number on output
	  logical*1 ask				! Will ask every EOF
	  logical*1 reduce			! Output events in forma ridotta
	  logical*1 det(0:NDTYPE)			! Output this detector
	  logical*1 par(0:MAXPAR-1,0:NDTYPE)  	! Output this parameter
	  integer evlen,pointer
	  logical*1 done
	endstructure

	structure/commands/
	  union
	    map
	      record/recall/recall
	    endmap
	    map
	      record/fold/fold
	    endmap
	    map
	      record/gate/gate
	    endmap
	    map
	      record/filter/filter
	    endmap
	    map
	      record/window/win
	    endmap
	    map
	      record/pairgate/pgate
	    endmap
	    map
	      record/banana/ban
	    endmap
	    map
	      record/pin/pin
	    endmap
	    map
	      record/recal/rcl
	    endmap
	    map
	      record/kine/kine
	    endmap
	    map
	      record/doppler/doppl
	    endmap
	    map
	      record/tadjust/tadj
	    endmap
	    map
	      record/tref/tref
	    endmap
	    map
	      record/hk/hk
	    endmap
	    map
	      record/add/add
	    endmap
	    map
	      record/kill/kill
	    endmap
	    map
	      record/select/sel
	    endmap
	    map
	      record/listev/lev
	    endmap
	    map
	      record/reorder/reo
	    endmap
	    map
	      record/statistics/stat
	    endmap
	    map
	      record/swap/swap
	    endmap
	    map
	      record/mask/mask
	    endmap
	    map
	      record/useful/useful
	    endmap
	    map
	      record/move/move
	    endmap
	    map
	      record/newid/newid
	    endmap
	    map
	      record/splitmerge/splmrg
	    endmap
	    map
	      record/polar/dpolar
	    endmap
	    map
	      record/pms_str/pms
	    endmap
	    map
	      record/qvalue_str/qvalue
	    endmap
	    map
	      record /prisma_angles_str/ang
	    endmap
	    map
	      record/addback/abck
	    endmap
	    map
	      record/proje/pro
	    endmap
	    map
	      record/spectrum/spec
	    endmap
	    map
	      record/matrix/mat
	    endmap
	  endunion

	  integer*8 addr

	  integer size
	  integer whichcom
	endstructure

	structure /tndio/
	  integer nfiles
	  integer current
	  logical*1 presorted
	end structure
# 2 "./gsort.inc" 2 

	INTEGER	  LLUN
	INTEGER	  LLIST
	INTEGER   GS2LUN
	INTEGER   LGSDIR
	CHARACTER GSDIR*64
	CHARACTER LIST*256
	CHARACTER LINE*256
	CHARACTER COMSORT*40
	integer INFILES
	COMMON /VARIECOM/ LLUN,LLIST,GS2LUN,LGSDIR,GSDIR,LIST,LINE,COMSORT,INFILES

	logical*1 BREAK
	logical*1 KILLEV
	logical*1 AUTONUMBER	! File etichettati con RUN#
	logical*1 TAPEDISMOUNT	! Controllo fine nastro
	logical*1 LASTTAPE	! Controllo fine nastro
	logical*1 AUTOLOADER
	logical*1 STORED_EV
	logical*1 DEFINED_OUT,FINISHED_OUT
	COMMON/FLAGS/ BREAK,KILLEV,AUTONUMBER,TAPEDISMOUNT,LASTTAPE,AUTOLOADER,STORED_EV,
     1 DEFINED_OUT,FINISHED_OUT

	logical*1 SYNTAX
	INTEGER PHASE
	CHARACTER COMLINE*256
	INTEGER LCOMLINE
	INTEGER INPLU
	CHARACTER*128  FORM(NFORMCOM)
	CHARACTER*128  DECL(NDECLCOM)
	CHARACTER*128  ANAL(NANALCOM)
	CHARACTER*128 SYNT(NSYNT)
	COMMON /COMDEFS/ PHASE,COMLINE,LCOMLINE,INPLU,FORM,DECL,ANAL,SYNT,SYNTAX

	RECORD /COMMANDS/COM(MAXCOMANDI)
	INTEGER	NCOMANDI
	INTEGER ICMD
	INTEGER WHICHCOM
	INTEGER INIT_MODE,FINIT_MODE
	COMMON /COMMAND/ COM,NCOMANDI,ICMD,WHICHCOM,INIT_MODE,FINIT_MODE

	INTEGER   DATAFORMAT			! GASP/EUROBALL/8PI
	INTEGER   GASPMAP(0:NDTYPE)		! Mappatura per formato GASP
	INTEGER   EUROMAP(0:NDTYPE)		! Mappatura per formato euroball
	INTEGER   ISEED				! Seed per i numeri random
	INTEGER   NTIPI				! quanti tipi di rivelatori
	INTEGER   NDPAR(0:NDTYPE)		! # parametri
	INTEGER   MDPAR(0:NDTYPE)		! # parametri aggiunti
	INTEGER   TDPAR(0:NDTYPE)		! # parametri totali
	INTEGER   PARRES(0:MAXPAR-1,0:NDTYPE)  	! Risoluzione dei vari parametri

	INTEGER   NDETS(0:NDTYPE)		! # di rivelatori (se composito)
	INTEGER   NSEGS(0:NDTYPE)		! # segmenti (se composito)
	INTEGER   NCPAR(0:NDTYPE)		! # di parametri comuni (se composito)
	
	INTEGER   NITEMS(0:NDTYPE)		! # totale di rivelatori
	logical*1   EXISTS(0:NDTYPE)		! Esiste il tipo di rivelatore
	INTEGER   FOLDMIN(0:NDTYPE)		! Minimo fold da nastro
	CHARACTER*1 DNAME(0:NDTYPE)		! simboli per rivelatori
	COMMON /EVDEF/DATAFORMAT,GASPMAP,EUROMAP,ISEED,NTIPI,NDPAR,MDPAR,
     1TDPAR,PARRES,NDETS,NSEGS,NCPAR,NITEMS,FOLDMIN,EXISTS,DNAME

	INTEGER   CLASS,TAG			! Descrizione dell'evento
	INTEGER   NDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   DOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/DET(0:MAXDET-1)		! i dati
	COMMON /CEVENT/  CLASS,TAG,NDET,DOFF,DET

	INTEGER   sCLASS,sTAG			! Copia dell'evento per SAVE/RECALL
	INTEGER   sNDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   sDOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/sDET(0:MAXDET-1)	! i dati
	COMMON /SEVENT/  sCLASS,sTAG,sNDET,sDOFF,sDET

	INTEGER   mCLASS,mTAG			! Copia di lavoro (Move...)
	INTEGER   mNDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   mDOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/mDET(0:MAXDET-1)	! i dati
	COMMON /mEVENT/  mCLASS,mTAG,mNDET,mDOFF,mDET


	integer*8 HASHADDR(0:MAXHASH-1)

	INTEGER HASHGIVEN
	INTEGER HNTRUE,HSTRUE			! Quanti rivelatori in gate
	logical*1 HLTRUE(0:MAXDET-1)		! Quali rivelatori in gate
	REAL    HDIST1(0:MAXDET-1)		! per gestione gates sferici
	REAL    HDIST2(0:MAXDET-1)		! per gestione gates sferici
	COMMON /HASHDEF/ HASHADDR,HASHGIVEN,HNTRUE,HSTRUE,HLTRUE,HDIST1,HDIST2


	integer*8 PAIRSADDR(0:MAXPAIRS-1)

	INTEGER PAIRSGIVEN
	COMMON /PAIRSDEF/ PAIRSADDR,PAIRSGIVEN


	integer*8 STATADDR

	INTEGER STATSIZE
	INTEGER NSTATCOM			! quanti comandi di statistica
	COMMON /STATISTICS/ STATADDR,STATSIZE,NSTATCOM

	INTEGER NLOST,NCLASS
	INTEGER*8 NINCR(MAXMATDIM)
	INTEGER FLUSHED
	INTEGER NPRINT
	COMMON /STATUS/ NLOST,NCLASS,NINCR,FLUSHED,NPRINT

	integer*2 ievbuf(16*MAXWORDS)	! Buffer dati
	integer*2 ievheader(MAXWORDS)	! header record
	record/iodef/iio
	common/ievcom/ievheader,ievbuf,iio

	record/oevdef/oev
	integer*2 oevent(MAXEVL*4)	! evento da scrivere
	integer*2 oevbuf(MAXBYTES/2)	! Buffer scrittura eventi
	record/iodef/oio
	common/oevcom/oev,oevent,oevbuf,oio

	record /tndio/tnd
	common /tndcom/tnd
# 16 "gsort.F" 2 

	logical MOUNTSTATUS
	
	logical*1 stopsort
	integer filexist,fileremove

	CHARACTER LOGFILE*40		! Filename logbook

	REAL	TIM_START_T	/0.0/
	REAL	TIM_END_T	/0.0/

	logical*1 INP_YES
	INTEGER TAPE_READ
	integer lastprint
	
	real*4 r4_time
	external r4_time

	external GS_INT_RUN
# 37

	external init_frand

	common /STOP_CTRL/stopsort


	call xinpmode(1)
CVMS
*	OPEN(UNIT=6,CARRIAGECONTROL='FORTRAN',STATUS='UNKNOWN')

CVMS	CALL CPU_SECONDS(TIM_START_T)

CVMS	gsdir='GASP'
	call getenv('GSDIR',gsdir)
	lgsdir=lengthc(gsdir)
	if(lgsdir.le.0) stop 'Please set the environment variable GSDIR'

CVMS	gsdir=gsdir(1:lgsdir)//':'
	if(gsdir(lgsdir:lgsdir) .ne. '/')gsdir=gsdir(1:lgsdir)//'/'
	lgsdir=lengthc(gsdir)
# 59

	SEC_START_T=SECNDS(0.)

1	DATAFORMAT=NOFORMAT			! Default data type

	call init_frand()			! inizializza il random seed

	CALL GETINPUT			! legge il programma di analisi

	WRITE(LU1,*)
	CALL LISTPROG(LU1)		! e lo stampa + alcune verifiche

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! INIZIALIZZAZIONI !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	IRUNOLD=0		! Flag di primo nastro
	INFILES=0		! quanti files ha fatto
	INCASSETTE=0		! quanti nastri ha fatto
	STOPSORT=.FALSE.
	BREAK=.FALSE.
	iio.datafile=.FALSE.
	iio.wlen=16384
	iio.lun=-1
	iio.flag=MTFLAGR
	iio.mtch=-1
	iio.run=0
	iio.rec=0
	iio.maxrec=4000000
	iio.evcount=0
	iio.evnumber=0
	NFILES =100
	AUTONUMBER=DATAFORMAT.EQ.GASP
	TAPEDISMOUNT=.FALSE.
	LASTTAPE=.false.
	NCASSETTE=1
	NPRINT = 400

*	stopsort=.true.
*	dowhile(stopsort)
*	   call gs_test_stop(stopsort,0,0)	! Cancella tutti i files STOP_MATRIX.DAT
*	enddo
	stopsort = .false.
	if( filexist('STOP_MATRIX').eq.0 )ii=fileremove('STOP_MATRIX')
	data_proc = .FALSE.

	call signal(2,GS_INT_RUN,-1)
	call signal(15,GS_INT_RUN,-1)
# 109

			
	call lib$get_lun(GS2LUN)
	call INIT_RUN(0)
	if(break) goto 50

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!! CICLO SUI NASTRI !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

5	call gs_define_istream(nfiles,ncassette)
	if(break) goto 500
	incassette=1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!! CICLO SUI RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

10	ICONT=1
	iio.rec=0
	nlost=0
	nclass=0
	lastprint = 0

12	call gs_get_run_number(nseen)
	if(break) goto 50

	infiles=infiles+1
	IF(iio.run.NE.IRUNOLD) THEN
	   WRITE(LOGFILE,'(''LOG.'',I4.4)') MOD(iio.run,10000)
CVMS	   OPEN(UNIT=LU2,NAME=LOGFILE,STATUS='NEW',FORM='FORMATTED')
	   OPEN(UNIT=LU2,NAME=LOGFILE,STATUS='UNKNOWN',FORM='FORMATTED')
	   IRUNOLD=iio.run
	ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!! SCRITTE INIZIALI SU LOGFILE !!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	WRITE(LU2,'(/,1X,75(1H*),/)')
	call timestamp(lu2,' ')
	WRITE(LU2,'('' Sort of data from  RUN#'',I4.4)') iio.run
	WRITE(LU2,*) 'According to the following instructions'
	CALL LISTPROG(LU2)
	write(lu2,*)
	IF(AUTONUMBER) THEN
	  WRITE(LU2,*)
	  CALL gs_list_bytes(ievheader,256,LU2)
	endif

	call INIT_RUN(1)
	if(break) goto 50

	do ilu=lu1,lu2
	  WRITE(ilu,'(/,1X,75(1H*),/)')
	  call timestamp(ilu,'  SORT  starts now')
	enddo
	lfile=MAX(1,LENGTHC(iio.name))
	if(iio.datafile) then
	  WRITE(LU1,'(''  Sort of data from  RUN#'',I4.4,''  ('',i3,A,/)')iio.run,infiles,
     &      ' )  on file '//iio.name(1:lfile)
	else
	  WRITE(LU1,'(''  Sort of data from  RUN#'',I4.4,''  ('',i3,A,/)')iio.run,infiles,
     &	    ' )  on tape '//iio.name(1:lfile)
	endif
	WRITE(LU1,*)
	
	IF(STOPSORT) GOTO 40

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!! INIZIALIZZAZIONI PER QUESTO RUN  !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	BREAK=.FALSE.
	icont=0
	if(autonumber) icont=1
	iio.rec=1			! per abilitare il ping-pong

CVMS	CALL CPU_SECONDS(TIM_START)

# 192

	SEC_START=SECNDS(0.)
	SEC_LAST=SECNDS(0.)

	call gs_statusline(0.)		! inizializza le variabili della riga di stato


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!! CICLO SUI RECORDS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

20	IF(iio.rec.ge.iio.maxrec) then
	   break=.true.
	   goto 40
	endif

	CALL NEXTRECORD(iaddr,nseen,icont)
	IF(NSEEN.LT.0) goto 40

*	IF(MOD(iio.REC,NPRINT).EQ.0) THEN
	IF(iio.REC-LASTPRINT .eq. NPRINT) THEN
# 215

	  SECONDI=SECNDS(SEC_LAST)

	  call gs_statusline(secondi,nseen)
# 221

	  SEC_LAST=SECNDS(0.)

	  LASTPRINT = iio.REC
	ENDIF
	
	IF(MOD(iio.REC,NSTOP).EQ.0) THEN
	  if( filexist('STOP_MATRIX').eq.0 )then
	    ii=fileremove('STOP_MATRIX')
	    stopsort = .true.
*	  call gs_test_stop(stopsort,LU1,LU2)
	  endif
	ENDIF
	IF(STOPSORT) GOTO 40

	nwords=nseen/2
	if(iio.virtuale) then
	  call SORT_FAKE(ievbuf(iaddr),nwords,%val(stataddr))
	elseif(DATAFORMAT.EQ.GASP) then
# 242

	  call SORT_GASP(ievbuf(iaddr),nwords,%val(stataddr))

	elseif(DATAFORMAT.EQ.EURO) then

	  call gs_swap_bytes(ievbuf(iaddr),nwords)

	if(IsEventBlock(ievbuf(iaddr)) .eq. 1)then
	 jjj = GetEventStructure(ievbuf(iaddr),nwords)
	endif
	  call SORT_EURO(ievbuf(iaddr),nwords,%val(stataddr))

	elseif(DATAFORMAT.EQ.PRISMA) then
# 257

	  call SORT_PRISMA(ievbuf(iaddr),nwords,%val(stataddr))

	elseif(DATAFORMAT.EQ.PI8) then
	  call SORT_8PI(ievbuf(iaddr),nwords,%val(stataddr))
	  
	elseif(DATAFORMAT.EQ.GSPH) then
	  call SORT_GAMMASPHERE(ievbuf(iaddr),nwords,%val(stataddr))

      elseif(DATAFORMAT.EQ.YALE) then

	call gs_swap_bytes(ievbuf(iaddr),nwords)

	call SORT_YALE(ievbuf(iaddr),nwords,%val(stataddr))

	elseif(DATAFORMAT .EQ. GSPN) then

	  call gs_swap_bytes(ievbuf(iaddr),nwords)

**	  if(IsEventBlock(ievbuf(iaddr)) .eq. 1)then
**	   jjj = GetEventStructure(ievbuf(iaddr),nwords)
**	  endif
	  call SORT_GSPN(ievbuf(iaddr),nwords,%val(stataddr))

	elseif(DATAFORMAT .EQ. TNDB) then
# 284

	  call SORT_TANDEM(ievbuf(iaddr),nwords,%val(stataddr))

	elseif(DATAFORMAT .EQ. GSR) then

	  call gs_swap_bytes(ievbuf(iaddr),nwords)

	  call SORT_GSR(ievbuf(iaddr),nwords,%val(stataddr))

	elseif(DATAFORMAT .EQ. GANIL) then
	  call SORT_GANIL(ievbuf(iaddr),nwords,%val(stataddr))

	else
	  call gs_syntax('Unknown event format')
	endif
	if(break) goto 40

	GOTO 20

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!! FINITO UN RUN  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

40	CALL ANSI_BELL(LU1)
CVMS	CALL CPU_SECONDS(TIM_END)

# 312

	SEC_END=SECNDS(SEC_START)

	call FINIT_RUN(0)
	DO ilu=LU1,LU2
	  lfile=MAX(1,LENGTHC(iio.name))
	  if(iio.datafile) then
	    WRITE(ilu,'(/,I15,''   Records read from  file   '',A)') iio.REC,iio.name(1:lfile)
	  else
	    WRITE(ilu,'(/,I15,''   Records read from tape    '',A)') iio.REC,iio.name(1:lfile)
	  endif
	  WRITE(ilu,'(I,''   Analysed events'')') iio.evcount
	  if(nlost.ne.0) then
	    WRITE(ilu,'(I15,''   Bad event sequences       '',A)') nlost
	  endif
	  do ii=1,MAXMATDIM
	    WRITE(ilu,'(I15,I4,''D increments'')') nincr(ii),ii
	  end do
	  call timestamp(ilu,'  SORT  completed')
	enddo
	call FINIT_RUN(1)
	call showtypes
	if(stopsort) goto 500

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!! Vari test per funzionamentoo in ciclo !!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

50	if(autoloader .and. .not.iio.datafile) then
	  if(infiles.lt.nfiles) then
	    if(nseen.eq.-2) goto 10
	    if(break .and. .not.(nseen.eq.-3)) then
		write(6,*) 'Skipping the rest of the file'
		call TAPE_SKIPFIL(iio.mtch,1) 
		goto 10
	    endif
	  endif
	  incassette=incassette+1
	  if(incassette.gt.ncassette) goto 500
	  write(6,*) 'Dismounting Tape#',incassette-1
	  CALL TAPE_DISMOUNT(iio.name,iio.mtch,1)
	  write(6,*) 'Waiting for Tape#',incassette
	  do jj=1,5
	    write(6,*)
	    do ii=60,0,-1
		call lib$wait(1.)
# 360

		write(6,'(''\r'',i5,$)') ii

	        call flush(6)
	    enddo

	write(6,*)

	    CALL TAPE_MOUNTFOR(iio.name,iio.mtch,MOUNTSTATUS)
	    if(MOUNTSTATUS) then
		infiles=0
		goto 10
	    endif
	  enddo
	  write(6,*) 'Could not mount Tape#',incassette
	  goto 500
	endif

	if(autonumber .and. nseen.ne.-3) then
	  if(infiles.lt.nfiles) goto 10
	  if(lasttape) goto 500
	  CALL TAPE_CONTROL(iio.name,iio.mtch)
	  if(inp_yes('Want to continue with another tape') ) then
	     infiles=0
	     goto 5
	  endif
	  goto 500
	endif

60	if(.not.lasttape) then
	  
	  if( iio.datafile ) then
	    if(inp_yes('Want to continue with another FILE') ) then
*	    	infiles=0
	    	goto 5
	    endif
	  
	  else
	  	     
	    CALL TAPE_CONTROL(iio.name,iio.mtch)
	    if(inp_yes('Continue reading this tape with another RUN '))goto 10
	    if(inp_yes('Want to continue with another TAPE') ) then
	      infiles=0
	       goto 5
	    endif
	  endif
	  
	endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!! SALVA I DATI PRIMA DI CHIUDERE !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

500	IF(TAPEDISMOUNT) then 
	  CALL TAPE_DISMOUNT(iio.name,iio.mtch,1)
	else
	  CALL TAPE_DISMOUNT(iio.name,iio.mtch,9)
	endif
	call FINIT_RUN(2)

CVMS	CALL CPU_SECONDS(TIM_END_T)

# 424

	SEC_END_T=SECNDS(SEC_START_T)

	WRITE(LU1,'('' Total of '',F11.1,'' Ellapsed seconds'')')
     1	 SEC_END_T
	WRITE(LU1,'('' Total of '',F11.1,'' CPU_seconds used'')')
     1	 TIM_END_T-TIM_START_T

	CALL EXIT(0)

	END

	subroutine GETINPUT

! Legge e decodifica il file di setup che definisce il tipo di analisi
!
!  ATTENZIONE CHE VIENE ESEGUITO SOLO UN MINIMO DI VERIFICHE DI CONSISTENZA.
!  DUNQUE LA RESPONSABILITA' DELLA SENSATEZZA DEL PROGRAMMA E' DELL'UTENTE.

# 1 "./gsort.inc" 1 
# 1 "./gsort.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./gsort.def" 2 
# 1 "./gsort.par" 1 
	INTEGER LU1		! LU per messaggi
	PARAMETER (LU1=6)
	INTEGER LU2		! LU per Logbook
	PARAMETER (LU2=7)
	INTEGER INDENT		! incolonnamento listato programma
	PARAMETER (INDENT=20)
*	INTEGER NPRINT		! intervallo per stampa status su LU1
*	PARAMETER (NPRINT=400)
	INTEGER NPRINT2		! intervallo per stampa status su LU2
	PARAMETER (NPRINT2=2000)
	INTEGER NSTOP		! intervallo per check di mat_stop
	PARAMETER (NSTOP=1000)
	INTEGER NSYNT		! # righe per la sintassi
	PARAMETER (NSYNT=5)

	INTEGER MAXRES		! Risoluzione massima gestita
	PARAMETER (MAXRES=32768)
	INTEGER MINRES		! Risoluzione minima gestita
	PARAMETER (MINRES=16)
	INTEGER MAXDET		! MAX # di rivelatori
	PARAMETER (MAXDET=256)
	INTEGER MAXPAR		! MAX # di parametri/rivelatore
	PARAMETER (MAXPAR=16)
	INTEGER NDTYPE		! Numero di tipi di rivelatore
	PARAMETER (NDTYPE=16)
	REAL PSTEP
	PARAMETER (PSTEP=359./1024.)

	INTEGER NOFORMAT	! No format defined
	PARAMETER (NOFORMAT=-1)
	INTEGER GASP		! Dati tipo GASP
	PARAMETER (GASP=1)
	INTEGER EURO		! Dati tipo Euroball
	PARAMETER (EURO=2)
	INTEGER PI8		! Dati tipo 8Pi-Berkeley
	PARAMETER (PI8=3)
	INTEGER GSPH		! Dati tipo GAMMASPHERE
	PARAMETER (GSPH=4)
	INTEGER GSPN		! Dati tipo GASP - nuova DAQ (2001)
	PARAMETER (GSPN=5)
	INTEGER YALE		! Dati tipo YrastBall - Yale Univ.
	PARAMETER (YALE=6)
	INTEGER TNDB
	PARAMETER (TNDB=7)	! Format Tandem-Bucharest
	INTEGER PRISMA
	PARAMETER (PRISMA=8)	! Format PRISMA (spettrometro)
	INTEGER GSR
	PARAMETER (GSR=9)       ! Temporary, GSPH reduced data
	INTEGER GANIL
	PARAMETER (GANIL=10)    ! GANIL data (EXOGAM,DIAMANT,NWALL)
	
	INTEGER CSEG,QSEG,TSEG,ISEG,SISEG, PPACSEG, DANTESEG
	PARAMETER (CSEG=7, QSEG=4, TSEG=1, ISEG=1, SISEG=4, PPACSEG=10, DANTESEG=8)
	INTEGER CDET,QDET,TDET,SIDET, PPACDET
	PARAMETER (CDET=15, QDET=26, TDET=30, SIDET=40, PPACDET=1)
	INTEGER COFF,QOFF,TOFF,FOFF
	PARAMETER (COFF=0, QOFF=COFF+CDET*CSEG, TOFF=QOFF+QDET*QSEG, FOFF=TOFF+TDET*TSEG)

	INTEGER MAXNBAN		! MAX # banane per BANANA e PIN
	PARAMETER (MAXNBAN=32)
	INTEGER MAXRCL_ORD	! MAX # coefficienti di ricalibrazione
	PARAMETER (MAXRCL_ORD=6)
	INTEGER MAXRCL_REGS	! MAX # regioni di ricalibrazione
	PARAMETER (MAXRCL_REGS=6)

	INTEGER MAXTADJ		! MAX # di parametri in time_adjust
	PARAMETER (MAXTADJ=16)

	INTEGER MAXHASH		! MAX # liste hashgates
	PARAMETER (MAXHASH=50)

	INTEGER MAXPAIRS	! MAX # liste ind=f(id,id) definibili
	PARAMETER (MAXPAIRS=4)
	
	INTEGER MAXMATDIM	! massima dimensione della matrice
	PARAMETER (MAXMATDIM=4)

	INTEGER DEFSTEP2D	! suddivisione default 2D
	PARAMETER (DEFSTEP2D=128)
	INTEGER DEFSTEP3D	! suddivisione default 3D
	PARAMETER (DEFSTEP3D=64)
	INTEGER DEFSTEP4D	! suddivisione default 4D
	PARAMETER (DEFSTEP4D=32)

	INTEGER DEFSTEP2DS	! suddivisione default 2D_symm
	PARAMETER (DEFSTEP2DS=128)
	INTEGER DEFSTEP3DS	! suddivisione default 3D_symm
	PARAMETER (DEFSTEP3DS=64)
	INTEGER DEFSTEP4DS	! suddivisione default 4D_symm
	PARAMETER (DEFSTEP4DS=32)

	INTEGER DEFSTEP2DH	! suddivisione default 2D_hsymm
	PARAMETER (DEFSTEP2DH=32)
	INTEGER DEFSTEP3DH	! suddivisione default 3D_hsymm
	PARAMETER (DEFSTEP3DH=8)
	INTEGER DEFSTEP4DH	! suddivisione default 4D_hsymm
	PARAMETER (DEFSTEP4DH=8)

	INTEGER MAXBYTES	! Max. lunghezza records su nastro
	PARAMETER (MAXBYTES=32*1024)
	INTEGER MAXWORDS	! Max. lunghezza records su nastro
	PARAMETER (MAXWORDS=MAXBYTES/2)
	INTEGER MAXEVL		! MAX lunghezza evento (words)
	PARAMETER (MAXEVL=2047)

	INTEGER MTFLAGR		! Flag per sync. nastro input
	PARAMETER (MTFLAGR=1)
	INTEGER MTFLAGW		! Flag per sync. nastro output
	PARAMETER (MTFLAGW=2)

	INTEGER NFORMCOM	! # comandi di Formato
	PARAMETER (NFORMCOM=9)
	INTEGER NDECLCOM	! # comandi di Dichiarazione
	PARAMETER (NDECLCOM=9)
	INTEGER NANALCOM	! # comandi di Analisi
	PARAMETER (NANALCOM=199)

	INTEGER MAXCOMANDI	! MAX # di COMANDI
	PARAMETER (MAXCOMANDI=360)
# 3 "./gsort.def" 2 

	structure/detector/
	  union
	    map
	      integer id
	    endmap
	    map
	      integer ival(-1:maxpar-1)
	    endmap
	  endunion
	  real    xval( 0:maxpar-1)
	endstructure

	structure/hashdat/
	  INTEGER IND,PAR,RES			! su quale parametro
	  INTEGER WHICH				! gate number
	  logical*1 SAMEFORALL			! same gate for all detectors
	  logical*1 FROMFILE			! dati da file
	  INTEGER NGATES(0:MAXDET-1)		! quanti gates ha letto
	  CHARACTER*72 FILE			! File dei gates
	  REAL    DIST2(0:1)			! Distanza**2 normalizzata per gate sferici
	endstructure
	
	structure/pairsdat/
	  INTEGER INDMAX
	  CHARACTER*72 FILE			! nomi dei file con i dati
	  INTEGER PIND(0:MAXDET-1,0:MAXDET-1)
	endstructure

	structure/recall/
	  logical*1 always
	endstructure

	structure/fold/
	  INTEGER IND		! Tipo di parametro
	  INTEGER MIN		! Finestra sul fold
	  INTEGER MAX		! Finestra sul fold
	endstructure

	structure/gate/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  logical*1 INSIDE	! dentro/fuori
	  INTEGER LO		! Estremo inferiore
	  INTEGER HI		! Estremo superiore
	  INTEGER NGATES	! Quanti intervalli
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	endstructure

	structure/gatesdat/
	  logical*1 BAD(0:1)
	endstructure

	structure/filter/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PARS		! parametro sorgente
	  INTEGER PARD		! parametro destinazione
	  INTEGER RES		! Risoluzione filtro
	  character*80 file     ! no comment
	  integer iform         !
	  real F(0:MAXRES)        !
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	  logical*1 initialized   !
	endstructure

	structure/window/
	  INTEGER  IND		! Tipo di parametro
	  INTEGER  LO(0:MAXPAR-1)! Valore inferiore
	  INTEGER  HI(0:MAXPAR-1)! Valore superiore
	  INTEGER  FL		! Minfold
	  INTEGER  FH		! Minfold
	  logical*1  SAME		! Same as before window
	endstructure

	structure/banana/
	  INTEGER IND(2)	! Tipo di parametro x,y
	  INTEGER PAR(2)	! Parametro asse x,y
	  INTEGER RES(2)	! Risoluzione della banana
	  INTEGER NBAN		! Numero di banane
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 INSIDE	! In-Out
	  logical*1 SAME		! Same as before banana
	  logical*1 multiadc      ! N.M. - for ADC-dependent banana
	  logical*1 ignore(0:MAXDET-1) ! only for ADC-dependent banana
	endstructure

	structure/bananadat/
	  character*72 file
	  integer ban(2,0:1)
	endstructure

	structure/bananasdat/
	  character*72 file(MAXNBAN)
	  integer ban(2,0:1)
	endstructure

	structure/pairgate/
	   integer IND
	   integer LO
	   integer HI
	   integer FL
	   integer FH
	   integer PAR
	   integer RES
	   integer*2 PVAL(0:MAXDET-1,0:MAXDET-1)
	   integer*2 LIST(0:MAXDET-1)
	   character*72 filename
	   logical*1 INSIDE
	   logical*1 SAME
	endstructure

	structure/pin/
	  INTEGER IND(4)	! Tipo di parametro per banane
	  INTEGER PAR(4)	! Parametro asse x,y ; la massa e il tipo della particella
	  INTEGER RES(2)	! Risoluzione delle banane
	  INTEGER FIX		! Parametro fisso per il risultato
	  INTEGER FIXRES        ! Risoluzione dell parametro fisso
	  INTEGER NBAN		! quante banane
	  INTEGER F1(MAXNBAN)	! Numero di particelle della banana
	  INTEGER F2(MAXNBAN)	! Peso della banana in PIN
	  INTEGER F3(MAXNBAN)	! Massa della particella
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before PIN
	  logical*1 multiadc(MAXNBAN)          ! N.M. - for ADC-dependent banana
	  logical*1 ignore(MAXNBAN,0:MAXDET-1) ! only for ADC-dependent banana
	endstructure

	structure/hk/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER FIXH		! Parametro fisso per H
	  INTEGER FIXk		! Parametro fisso per k
	  REAL	  OFFS		! Offset per H
	  REAL	  GAIN		! Guadagno finale per H
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	endstructure

	structure/recal/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 RUN		! coefficenti run-dependent
	  logical*1 SAME	! Same as before recal
	  logical*1 ROUND       ! Round the result to integer value
	  logical*1 ZERO        ! Set negative result to zero
	  CHARACTER*73 FILE	! files dei coefficenti
	endstructure

	structure/recal_choose/
	  INTEGER IND(3)	! Tipo di parametro
	  INTEGER PAR(3)	! Quale parametro
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL    VAL           ! Valore di riferimento per fare la scelta
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  CHARACTER*73 FILE1	! files dei coefficenti
	  CHARACTER*73 FILE2	! files dei coefficenti
	  logical*1 RUN		! coefficenti run-dependent
	  logical*1 SAME	! Same as before recal
	  logical*1 ROUND       ! Round the result to integer value
	endstructure

	structure/calcoef/
	  INTEGER*4 ORD
	  REAL*4    COEF(MAXRCL_ORD)
	endstructure

	structure/mcalcoef/
	  integer nregs
	  INTEGER ORD(MAXRCL_REGS)
	  REAL    COEF(MAXRCL_ORD,MAXRCL_REGS)
	  real    limit(0:MAXRCL_REGS)
	endstructure


	structure/doppler/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET		! Quanti rivelatori
         real*8  fact1
         real*8  fact2
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL	  VC0		! Velocita' di rinculo
	  real    rtheta
	  real    rphi
	  REAL	  E0		! VC0 fino a E0
	  REAL	  E1		! Da VC0 a VC1 tra E0 e E1
	  REAL	  VC1		! VC1 sopra E1
	  REAL	  SLOPE		! Slope tra E0 e E1
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
         logical*1 RefChanged
	  logical*1 CONST		! Costante
	  logical*1 SAME		! Same as before recal
	  CHARACTER*73 FILE	! file con gli angoli
	endstructure

	structure/dopplerdat/
	  real costheta(0:1)
	endstructure
	
	structure/polar/
         real*8  fact1
         real*8  fact2
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET		! Quanti rivelatori
          INTEGER DTHETA        ! Angolo THETA del rivelatore, se viene dato come parametro
          INTEGER DPHI          ! Angolo PHI del rivelatore, se viene dato come parametro
	  INTEGER IND_POLAR
	  INTEGER PTHETA
	  INTEGER PPHI
         INTEGER IND_VEL
         INTEGER PVEL
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL	  VC0		! Velocita' di rinculo
	  REAL	  E0		! VC0 fino a E0
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
          logical*1 FIXED
	  logical*1 VarDetAngle	! Se angoli degli rivelatori sono parametri
	  logical*1 SAME		! Same as before recal
	  CHARACTER*73 FILE	! file con gli angoli
	endstructure
	
	structure/polardat/
	  real cdir(3,0:MAXDET-1)
	endstructure

	structure/meanvalstr/
	  INTEGER IND1(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR1(MAXTADJ)	! Quale parametro
	  INTEGER RES1(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY1	! Quanti definiti
	  REAL    POSITION	! Posizione finale
	  REAL	  FACTOR	! Rejection factor
	  INTEGER IND2	! FIX only
	  INTEGER PAR2	! Quale parametro fisso
	  INTEGER RES2	! Risoluzione
	endstructure

	structure/tadjust/
	  INTEGER IND1(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR1(MAXTADJ)	! Quale parametro
	  INTEGER RES1(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY1	! Quanti definiti
	  REAL    POSITION	! Posizione finale
	  REAL	  FACTOR	! Rejection factor
	  INTEGER IND2(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR2(MAXTADJ)	! Quale parametro
	  INTEGER RES2(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY2	! Quanti definiti
	endstructure

	structure/tref/
	   integer*8 N
# 285

	   real*16 SQSUM

	   INTEGER IND
	   INTEGER PAR
	   INTEGER RES
	   INTEGER REFNO
	   REAL POSITION
	endstructure

	structure/kine/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET_GE	! quanti germani
	  INTEGER NDET_SI	! quanti silici
	  INTEGER BIND(2)	! Tipo di parametro per banane
	  INTEGER BPAR(2)	! Parametro asse x,y
	  INTEGER BRES(2)	! Risoluzione della banana
	  INTEGER NBAN		! Numero di banane
	  INTEGER PINFIX	! Fixpar per PIN (se >=0)
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before recal
	  CHARACTER*72 KINEFILE	! file di descrizione
	  CHARACTER*72 AFILE_GE	! angoli germanio
	  CHARACTER*72 AFILE_SI	! angoli silici
	endstructure

	structure/kinedat/
	  REAL    ACMP,ECMP		! il nucleo composto
	  REAL    XMOMCM		! suo momento
	  REAL    AD(MAXNBAN)		! massa della particella
	  INTEGER MDET(MAXNBAN)		! numero di rivelatori attivi
	  INTEGER F1(MAXNBAN)		! Numero di particelle della banana per PIN
	  INTEGER F2(MAXNBAN)		! Peso della banana per PIN
	  real mom_si(0:MAXDET-1,MAXNBAN)
	  real cdir_ge(3,0:MAXDET-1)
	  real cdir_si(3,0:MAXDET-1)
	  record/bananasdat/kban
	endstructure

	structure/kinenew/
	  INTEGER IND(3)		! Tipo di parametro
	  INTEGER PAR(3)		! Parametro
	  INTEGER RES(3)		! Risoluzione del parametro
	  INTEGER NDET_GE	! quanti germani
	  INTEGER NDET_SI	! quanti silici
	  real ecal
	  real acmp
	  real ecmp
	  real atenuation
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before recal
	  CHARACTER*72 AFILE_GE	! angoli germanio
	  CHARACTER*72 AFILE_SI	! angoli silici
	  character*73 except_file
	  logical*1 except
	endstructure

	structure/kinenewdat/
	  REAL    ACMP,ECMP		! il nucleo composto
	  REAL    XMOMCM		! suo momento
	  real    ecal
	  real    afac
	  real cdir_ge(3,0:MAXDET-1)
	  real cdir_si(3,0:MAXDET-1)
	  integer nregs
	  integer reg(MAXRES,2)
	endstructure

	structure/sltocm/
	  integer IND(2)
	  integer PAR(2)
	  integer NDET_SI
	  real F(2)
	  real COS_TH(0:MAXDET-1)
	  CHARACTER*72 AFILE_SI	! angoli silici
	endstructure


	structure/add/
	  INTEGER IND(3)	! Tipo di parametro
	  INTEGER PAR(3)	! Parametro 1+2==>3
	  REAL	  FAC(3)	! fattori moltiplicativi
	  REAL    OFFSET	! offset da sommare al risultato
	  REAL    GAIN		! gain sul risultato (non sul'offset)
	  INTEGER ICHAN		! canale limite per COMBINE
	  INTEGER DELTA		! Sliding range
	  INTEGER SLIDE		! Sliding value
	  logical*1 CHECK		! check che il secondo parametro sia > limit
	  logical*1 MULT		! fattori moltiplicativi?
	endstructure

	structure/kill/
	  INTEGER IND			! Tipo di parametro
	  logical*1 RUN			! run dependent selective kill
	  CHARACTER*72 FILE		! files contenente i detbad
	  logical*1 DET(0:MAXDET-1)	! Quali rivelatori
	endstructure

	structure/killdat/
	  logical*1 remove(0:MAXDET-1,0:MAXDET-1)
	endstructure

	structure/select/
	  INTEGER IND		! Tipo di parametro
	  logical*1 DET(0:MAXDET-1)  ! Quali rivelatori
	endstructure

	structure/listev/
	  logical*1 TOFILE	! list events on file
	  logical*1 ISOPEN
	  INTEGER LUN
	  CHARACTER*72 FILE
	endstructure

	structure/reorder/
	  INTEGER IND		! Tipo di parametro
	endstructure

	structure/statistics/
	  INTEGER WHICH
	endstructure

	structure/statdat/
	  INTEGER FOLD(0:MAXDET,0:NDTYPE,0:1)
	endstructure

	structure/swap/
	  INTEGER IND(2)	! Tipo di parametro x,y
	  INTEGER PAR(2)	! Parametro asse x,y
	endstructure

	structure/mask/
	  INTEGER IND		! Tipo di parametro x
	  INTEGER PAR		! Parametro asse x
	  INTEGER MASK		! Maschera
	  logical*1 ONE		! solo un parametro o tutti?
	endstructure

	structure/useful/
	  INTEGER IND		! Tipo di parametro (deve essere 0)
	  INTEGER PAR1		! Parametro per RUN#
	  INTEGER PAR2		! Parametro per REC#
	  INTEGER PAR3		! Parametro per EVCOUNT#
	  INTEGER PAR4		! Parametro per EVNUMBER#
	endstructure

	structure/move/
	  INTEGER IND1			! da quale tipo
	  INTEGER IND2			! a  quale tipo
	  INTEGER PAR1			! da quale parametro
	  INTEGER PAR2			! a  quale parametro
	  INTEGER OFFSET                ! offset alla nuova numerazione
	  logical*1 CONDITION		! TRUE if GATE or copy detector
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
c	  logical*1 INSIDE	! dentro/fuori
	  INTEGER LO		! Estremo inferiore
	  INTEGER HI		! Estremo superiore
c	  INTEGER NGATES	! Quanti intervalli
c	  INTEGER FL		! Minfold
c	  INTEGER FH		! Maxfold
c	  logical*1 SAME		! Same as before gate
	  INTEGER HOWMANY		! Howmany to move
	  INTEGER WHICH(0:MAXDET-1)	! Quali rivelatori
	endstructure

	structure/splitmerge/
	  INTEGER NIND			! quanti tipi di rivelatore coinvolti
	  INTEGER IND(0:NDTYPE)		! Quali rivelatori
	  logical*1 REMOVE(0:NDTYPE)	!
	  INTEGER OFFSET(0:NDTYPE)	! Offset degli indici
	endstructure

	structure/newid/
	  INTEGER IND			! tipo di rivelatore
	  INTEGER LUT(0:MAXDET-1)	! mappa dei nuovi indici
	  logical*1 REORDER		! reordina dopo la mappatura
	  CHARACTER*72 FILE		! files dei nuovi id
	endstructure

	structure/addback/
	  INTEGER IND			! Tipo di parametro
	  INTEGER PAR			! Quale parametro
	  INTEGER RES			! Risoluzione del parametro
	  INTEGER NSEGS			! quanti segmenti (se composito)
	  logical*1 REJECT		! elimina le doppie non contigue
	  logical*1 PATTERN		! produce la hit-pattern sommando tutto
	  INTEGER PATPAR		! dove la registra
	  logical*1 TGATE			! verifica la relazione temporale?
	  INTEGER TPAR			! su quale parametro
	  INTEGER TVAL			! massima deviazione
	endstructure

        structure /pms_str/
	    real*8  sin_70
	    real*8  sin_110
	    real*8  cos_20
	    real*8  tan_20
	    integer ind
	    integer par_r
	    integer par_p
	    integer par_e
	    integer par_icp
	    integer ind_theta
	    integer par_theta
	    integer ind_path
	    integer par_path
	    integer ind_x
	    integer par_x
	    integer ind_ic
	    integer par_a
	    integer par_b
	    integer par_c
	    integer par_d
	    real    thr
	    real    qfact
	end structure
	
	structure /pms_dat/
	   real*8 R(40,0:100)
	end structure
	 
	structure /qvalue_str/
	   real*8  costhp
	   real*8  sinthp
	   real*8  amu
	   integer ind_q
	   integer par_q
	   integer ind_mass
	   integer par_mass
	   integer ind_theta
	   integer par_theta
	   integer ind_phi
	   integer par_phi
	   integer ind_beta
	   integer par_beta
	   integer AP
	   integer AT
	   integer ATOT
	   integer    low
	   integer    high
	   real    ep
	   real    thp
	   real    gain
	   real    offset
	end structure

       structure /prisma_angles_str/
          real*8  prisma_angle
          real*8  D
          real*8  sinalpha
          real*8  cosalpha
          real*8  costhp
          real*8  sinthp
	  real*8  cx(0:3)
	  real*8  cy(0:3)
          integer ind
          integer parx
          integer pary
          integer parz
          integer parq
          integer part
	  integer parp
	  integer pard
	  integer degx
	  integer degy
	  integer oldstyle
	  character*128 calfile
       end structure
	   
	structure/proje/
	  INTEGER LEN		! Lunghezza totale degli spettri (LW)
	  INTEGER MFRES 	! risoluzione max. di F
	  INTEGER*8 NINCR
	endstructure

	structure/projedat/
	  CHARACTER*16 NAME(0:MAXPAR-1,0:NDTYPE)
	  INTEGER      ADDR(0:MAXDET-1,0:MAXPAR-1,0:NDTYPE)
	  INTEGER      SPEC(0:1)
	endstructure

	structure/spectrum/
	  INTEGER IND			! Tipo di parametro
	  INTEGER PAR			! Quale parametri
	  INTEGER RES			! numero di canali dello spettro
	  INTEGER IHASH			! Quale tabella di hash (se >=0)
	  INTEGER HIND,HPAR		! riportati qui per comodita'
	  logical*1 HSAME			! IND e' quello su cui applicare HGATE
	  INTEGER NHASH			! Quante volte
	  logical*1 SPHERICAL		! gate di tipo elissoidale
	  INTEGER MINFOLD		! fold minimo (per hsort1d)
	  INTEGER*8 NINCR		! numero di incrementi
	  CHARACTER*72 FILE		! Filename dello spettro
	endstructure

	structure/specdat/
	  INTEGER SPEC(0:1)
	endstructure

	structure/matrix/

	  integer*8 CMTBASE

	  INTEGER NDIM			! Ordine della matrice
	  INTEGER IND(MAXMATDIM)	! Tipo di parametri per i due lati
	  INTEGER PAR(MAXMATDIM)	! Quale dei parametri
	  INTEGER RES(MAXMATDIM)	! Dimensioni lati della matrice
	  INTEGER STEP(MAXMATDIM)	! suddivisione lati della matrice
	  INTEGER MATMODE		! 0=normal 1=symmetric 2=halfsymmetric
	  INTEGER MSDIM			! numero di indici simmetrizzati
	  INTEGER DOFFSET		! per SORTxD_DIFF
	  INTEGER NIND
	  INTEGER IHASH			! Quale tabella di hash (se >=0)
	  INTEGER HIND,HPAR		! riportati qui per comodita'
	  logical*1 HSAME			! IND e' quello su cui applicare HGATE
	  INTEGER NHASH			! quante volte
	  logical*1 SPHERICAL		! gate di tipo elissoidale
	  INTEGER TYPE
	  INTEGER IND1,IND2,IND3,IND4
	  INTEGER PAR1,PAR2,PAR3,PAR4
	  logical*1 HHH1,HHH2,HHH3,HHH4
	  CHARACTER*72 FILE		! Filename matrice
	endstructure

	structure/iodef/
	  logical*1 datafile	! TRUE se da file o Virtuale
	  logical*1 virtuale
	  logical*1 closed
	  integer lun
	  integer flag
	  integer wlen
	  integer run
	  integer rec
	  integer maxrec
	  integer evcount	! internal count
	  integer evnumber	! from ACQ
	  integer vrun,vevents
CVMS	  integer*2 iosb(4)
	  integer iosb(4)
CVMS	  integer*2 mtch,hctm
	  integer mtch
	  character*72 name
	endstructure

	structure/oevdef/
	  logical*1 defined
	  logical*1 same				! Same FILE structure on output
	  logical*1 sameRUN			! Same RUN number on output
	  logical*1 ask				! Will ask every EOF
	  logical*1 reduce			! Output events in forma ridotta
	  logical*1 det(0:NDTYPE)			! Output this detector
	  logical*1 par(0:MAXPAR-1,0:NDTYPE)  	! Output this parameter
	  integer evlen,pointer
	  logical*1 done
	endstructure

	structure/commands/
	  union
	    map
	      record/recall/recall
	    endmap
	    map
	      record/fold/fold
	    endmap
	    map
	      record/gate/gate
	    endmap
	    map
	      record/filter/filter
	    endmap
	    map
	      record/window/win
	    endmap
	    map
	      record/pairgate/pgate
	    endmap
	    map
	      record/banana/ban
	    endmap
	    map
	      record/pin/pin
	    endmap
	    map
	      record/recal/rcl
	    endmap
	    map
	      record/kine/kine
	    endmap
	    map
	      record/doppler/doppl
	    endmap
	    map
	      record/tadjust/tadj
	    endmap
	    map
	      record/tref/tref
	    endmap
	    map
	      record/hk/hk
	    endmap
	    map
	      record/add/add
	    endmap
	    map
	      record/kill/kill
	    endmap
	    map
	      record/select/sel
	    endmap
	    map
	      record/listev/lev
	    endmap
	    map
	      record/reorder/reo
	    endmap
	    map
	      record/statistics/stat
	    endmap
	    map
	      record/swap/swap
	    endmap
	    map
	      record/mask/mask
	    endmap
	    map
	      record/useful/useful
	    endmap
	    map
	      record/move/move
	    endmap
	    map
	      record/newid/newid
	    endmap
	    map
	      record/splitmerge/splmrg
	    endmap
	    map
	      record/polar/dpolar
	    endmap
	    map
	      record/pms_str/pms
	    endmap
	    map
	      record/qvalue_str/qvalue
	    endmap
	    map
	      record /prisma_angles_str/ang
	    endmap
	    map
	      record/addback/abck
	    endmap
	    map
	      record/proje/pro
	    endmap
	    map
	      record/spectrum/spec
	    endmap
	    map
	      record/matrix/mat
	    endmap
	  endunion

	  integer*8 addr

	  integer size
	  integer whichcom
	endstructure

	structure /tndio/
	  integer nfiles
	  integer current
	  logical*1 presorted
	end structure
# 2 "./gsort.inc" 2 

	INTEGER	  LLUN
	INTEGER	  LLIST
	INTEGER   GS2LUN
	INTEGER   LGSDIR
	CHARACTER GSDIR*64
	CHARACTER LIST*256
	CHARACTER LINE*256
	CHARACTER COMSORT*40
	integer INFILES
	COMMON /VARIECOM/ LLUN,LLIST,GS2LUN,LGSDIR,GSDIR,LIST,LINE,COMSORT,INFILES

	logical*1 BREAK
	logical*1 KILLEV
	logical*1 AUTONUMBER	! File etichettati con RUN#
	logical*1 TAPEDISMOUNT	! Controllo fine nastro
	logical*1 LASTTAPE	! Controllo fine nastro
	logical*1 AUTOLOADER
	logical*1 STORED_EV
	logical*1 DEFINED_OUT,FINISHED_OUT
	COMMON/FLAGS/ BREAK,KILLEV,AUTONUMBER,TAPEDISMOUNT,LASTTAPE,AUTOLOADER,STORED_EV,
     1 DEFINED_OUT,FINISHED_OUT

	logical*1 SYNTAX
	INTEGER PHASE
	CHARACTER COMLINE*256
	INTEGER LCOMLINE
	INTEGER INPLU
	CHARACTER*128  FORM(NFORMCOM)
	CHARACTER*128  DECL(NDECLCOM)
	CHARACTER*128  ANAL(NANALCOM)
	CHARACTER*128 SYNT(NSYNT)
	COMMON /COMDEFS/ PHASE,COMLINE,LCOMLINE,INPLU,FORM,DECL,ANAL,SYNT,SYNTAX

	RECORD /COMMANDS/COM(MAXCOMANDI)
	INTEGER	NCOMANDI
	INTEGER ICMD
	INTEGER WHICHCOM
	INTEGER INIT_MODE,FINIT_MODE
	COMMON /COMMAND/ COM,NCOMANDI,ICMD,WHICHCOM,INIT_MODE,FINIT_MODE

	INTEGER   DATAFORMAT			! GASP/EUROBALL/8PI
	INTEGER   GASPMAP(0:NDTYPE)		! Mappatura per formato GASP
	INTEGER   EUROMAP(0:NDTYPE)		! Mappatura per formato euroball
	INTEGER   ISEED				! Seed per i numeri random
	INTEGER   NTIPI				! quanti tipi di rivelatori
	INTEGER   NDPAR(0:NDTYPE)		! # parametri
	INTEGER   MDPAR(0:NDTYPE)		! # parametri aggiunti
	INTEGER   TDPAR(0:NDTYPE)		! # parametri totali
	INTEGER   PARRES(0:MAXPAR-1,0:NDTYPE)  	! Risoluzione dei vari parametri

	INTEGER   NDETS(0:NDTYPE)		! # di rivelatori (se composito)
	INTEGER   NSEGS(0:NDTYPE)		! # segmenti (se composito)
	INTEGER   NCPAR(0:NDTYPE)		! # di parametri comuni (se composito)
	
	INTEGER   NITEMS(0:NDTYPE)		! # totale di rivelatori
	logical*1   EXISTS(0:NDTYPE)		! Esiste il tipo di rivelatore
	INTEGER   FOLDMIN(0:NDTYPE)		! Minimo fold da nastro
	CHARACTER*1 DNAME(0:NDTYPE)		! simboli per rivelatori
	COMMON /EVDEF/DATAFORMAT,GASPMAP,EUROMAP,ISEED,NTIPI,NDPAR,MDPAR,
     1TDPAR,PARRES,NDETS,NSEGS,NCPAR,NITEMS,FOLDMIN,EXISTS,DNAME

	INTEGER   CLASS,TAG			! Descrizione dell'evento
	INTEGER   NDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   DOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/DET(0:MAXDET-1)		! i dati
	COMMON /CEVENT/  CLASS,TAG,NDET,DOFF,DET

	INTEGER   sCLASS,sTAG			! Copia dell'evento per SAVE/RECALL
	INTEGER   sNDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   sDOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/sDET(0:MAXDET-1)	! i dati
	COMMON /SEVENT/  sCLASS,sTAG,sNDET,sDOFF,sDET

	INTEGER   mCLASS,mTAG			! Copia di lavoro (Move...)
	INTEGER   mNDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   mDOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/mDET(0:MAXDET-1)	! i dati
	COMMON /mEVENT/  mCLASS,mTAG,mNDET,mDOFF,mDET


	integer*8 HASHADDR(0:MAXHASH-1)

	INTEGER HASHGIVEN
	INTEGER HNTRUE,HSTRUE			! Quanti rivelatori in gate
	logical*1 HLTRUE(0:MAXDET-1)		! Quali rivelatori in gate
	REAL    HDIST1(0:MAXDET-1)		! per gestione gates sferici
	REAL    HDIST2(0:MAXDET-1)		! per gestione gates sferici
	COMMON /HASHDEF/ HASHADDR,HASHGIVEN,HNTRUE,HSTRUE,HLTRUE,HDIST1,HDIST2


	integer*8 PAIRSADDR(0:MAXPAIRS-1)

	INTEGER PAIRSGIVEN
	COMMON /PAIRSDEF/ PAIRSADDR,PAIRSGIVEN


	integer*8 STATADDR

	INTEGER STATSIZE
	INTEGER NSTATCOM			! quanti comandi di statistica
	COMMON /STATISTICS/ STATADDR,STATSIZE,NSTATCOM

	INTEGER NLOST,NCLASS
	INTEGER*8 NINCR(MAXMATDIM)
	INTEGER FLUSHED
	INTEGER NPRINT
	COMMON /STATUS/ NLOST,NCLASS,NINCR,FLUSHED,NPRINT

	integer*2 ievbuf(16*MAXWORDS)	! Buffer dati
	integer*2 ievheader(MAXWORDS)	! header record
	record/iodef/iio
	common/ievcom/ievheader,ievbuf,iio

	record/oevdef/oev
	integer*2 oevent(MAXEVL*4)	! evento da scrivere
	integer*2 oevbuf(MAXBYTES/2)	! Buffer scrittura eventi
	record/iodef/oio
	common/oevcom/oev,oevent,oevbuf,oio

	record /tndio/tnd
	common /tndcom/tnd
# 444 "gsort.F" 2 

	CHARACTER SETUPFILE*80

	DO II=1,NFORMCOM
	  FORM(II)=' '
	ENDDO

	DO II=1,NDECLCOM
	  DECL(II)=' '
	ENDDO

	DO II=1,NANALCOM
	  ANAL(II)=' '
	ENDDO

!!!!!!!!!!!!!!!!!!!!!  Format !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	FORM( 1)='FORMAT          Define format of data (GASP, GASP2, PRISMA, EUROBALL, GAMMASPHERE, YALE, 8PI, TANDEM default=GASP)'
*	FORM( 2)='EUROBALL        Define format of data EUROBALL style'
*	FORM( 3)='8PI             Define format of data 8Pi-Berkeley style'
*	FORM( 4)='GAMMASPHERE     Define format of data GAMMASPHERE style'
*	FORM( 5)='GASP2001        Define format of data GASP (new DAQ - 2001) style'
	FORM( 6)='HEADER          Define fixed parameters (header of the event)'
	FORM( 7)='DETECTOR        Define a detector type'
	FORM( 8)='CDETECTOR       Define a composite detector type'

!!!!!!!!!!!!!!!!!!!!!  Declarations !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	DECL( 1)='RAWFOLDMIN      Define minimum fold to accept event from tape'
	DECL( 2)='HGATEDEF        Define a multiple gate to be applied to a parameter during sort'
	DECL( 3)='PAIRDEF         Define values for function index(n1,n2)'

!!!!!!!!!!!!!!!!!!!!!  Analysis commands !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	ANAL( 1)='FOLD            Discard event if number of detectors outside limits'
	ANAL( 2)='GATE            Gate on a parameter'
	ANAL( 3)='GATES           Multiple Gates on a parameter'
	ANAL( 4)='WINDOW          Gates on all parameters of the defined type'
	ANAL( 5)='BANANA          2-dimensional gate'
	ANAL( 6)='BANANAS         Multiple 2-dimensional gates'
	ANAL( 7)='FILTER          Assign score acording to a filtering function'
	ANAL( 8)='XBAN            Exclusive 2-dimensional gate '

	ANAL(10)='RECAL_MULT      Recalibration of a parameter on different regions. Coefficients from file'
	ANAL(11)='GAIN            Change gain of a parameter'
	ANAL(12)='RECAL           Recalibration of a parameter. Coefficients from file'
	ANAL(13)='RECAL_LUT       Recalibration of a parameter from look-up table'
	ANAL(14)='TIME_REFERENCE  make time spectra with respect of one detector'
	ANAL(15)='TIMING          make time spectra taking first coincidence as reference'
	ANAL(16)='RECAL_DOPPLER   Doppler correction with recoil-velocity function of gamma-energy'
	ANAL(17)='RECAL_KINE      Kinematic reconstruction of G0 according to ISIS'
	ANAL(18)='TIME_ADJUST     Improve the timing by adjustment of the time reference'
	ANAL(19)='PIN             Particle Identification Number according to ISIS'
	ANAL(20)='HK              Total energy H and Fold k of a detector (e.g. BGO ball)'

	ANAL(21)='ADD             Add [with factors] 2 parameters into a 3rd one'
	ANAL(22)='KILL            Kill detectors from the event'
	ANAL(23)='SELECT          Select events with defined detectors'
	ANAL(24)='STORE_EVENT     Save a copy of the event in its present status'
	ANAL(25)='RECALL_EVENT    Recall the saved copy'
	ANAL(26)='WRITE_EVENT     Write events to Tape or Disk_file in [reduced] GASP format'
	ANAL(27)='LIST_EVENT      List events on Terminal'
	ANAL(28)='REORDER         Order the sequence of detectors of the event'
	ANAL(29)='STATISTICS      Calculate the statistics of detectors'

	ANAL(30)='SWAP            Swap two parameters'
	ANAL(31)='MASK            Binary mask of a parameters'
	ANAL(32)='MOVE            Move a list of detectors of one type into another type'
	ANAL(33)='MERGE           Merge detectors together'
	ANAL(34)='SPLIT           Split detectors from one type to a list of'
	ANAL(35)='NEWID           Change the id of detectors'
	ANAL(36)='EBKILL          Selective kill of Euroball detectors'
	ANAL(37)='COMBINE         Combines 2 parameters together'
	ANAL(38)='ADDBACK         Addback of composite Euroball detectors'
	ANAL(39)='COPY            Copy one detector or parameter into another'
	ANAL(40)='USEFUL          Put Run#, Record#, Event# and Count# in header parameters'
	ANAL(41)='THRESHOLDS      Eliminate out-of-range values for a parameter. Threshold values taken from file'

	ANAL(44)='MULT            Multiply 2 parameters into a 3rd one with gain'
	ANAL(45)='MEAN_VALUE      Put in a header parameter the mean value of the given detector parameter '
	ANAL(46)='DIVIDE          Divide 2 parameters into a 3rd one with gain ( P1/P2 -> P3 )'
	ANAL(47)='ESL_TO_ECM      Calculate the energy value in CM (nonrelativistic)'
	ANAL(48)='RECAL_CHOOSE    Recalibration of two parameters, choosing the best value for the first one'
	ANAL(49)='WMEAN_VALUE     Put in a header parameter the weighted mean value of the given detector parameter'
	ANAL(50)='RECAL_POLAR     Doppler correction with recoil-velocity and polar angles'
	ANAL(54)='ANGLES_PRISMA   Calculate polar angles and theta for particles detected with PRISMA spectrometer'
	ANAL(55)='TRACK_PRISMA    Track trajectories in PRISMA spectrometer'
	ANAL(56)='QVALUE_PRISMA   Calculate Q-value for binary reactions (PRISMA/CLARA setup)'
	ANAL(57)='BP_VELOCITY_PRISMA   Calculate velocity vector for the unobserved particle in binary reactions (PRISMA/CLARA setup)'
	ANAL(58)='XFP_PRISMA      Pre-process focal-plane X coordinate for PRISMA  spectrometer'
	
	ANAL(74)='BREAK           Stop event analysis, ignore the comands following it'
	ANAL(75)='PAIRGATE        Gate on parameter asociated with pairs of detectors'
	ANAL(76)='POWER           Raise one parameter to a given power'
	ANAL(77)='PAIRKILL        Eliminate combinations of detectors according to a pair list'

	ANAL(91)='USERSUB1'
	ANAL(92)='USERSUB2'
	ANAL(93)='USERSUB3'
	ANAL(94)='USERSUB4'
	ANAL(95)='USERSUB5'
	ANAL(96)='USERSUB6'
	ANAL(97)='USERSUB7'
	ANAL(98)='USERSUB8'
	ANAL(99)='USERSUB9'

	ANAL(101)='PROJECTIONS     Projections for all defined parameters and detectors'
	ANAL(102)='SORT1D         *1D sort of any parameter'
	ANAL(103)='SORT1D_M       *1-D multiple sort with HGATE'
	
	ANAL(111)='SORT2D         *2-D sort of any pair       of parameters'
	ANAL(112)='SORT3D         *3-D sort of any triplet    of parameters'
	ANAL(113)='SORT4D         *4-D sort of any quadruplet of parameters'

	ANAL(116)='SORT2D_AB      *2-D sort of one detector with different parameters on the 2 axis'

	ANAL(121)='SORT2D_SYMM    *Symmetrized 2-D sort'
	ANAL(122)='SORT3D_SYMM    *Symmetrized 3-D sort'
	ANAL(123)='SORT4D_SYMM    *Symmetrized 4-D sort'

	ANAL(125)='SORT3D_PAIR    *3-D sort of Pn-Pn-Pair_index'
	ANAL(127)='SORT3D_DIFF    *3-D sort of Pn-Pn-Difference'

	ANAL(131)='SORT2D_HSYMM   *Half-Symmetrized 2-D sort'
	ANAL(132)='SORT3D_HSYMM   *Half-Symmetrized 3-D sort'
	ANAL(133)='SORT4D_HSYMM   *Half-Symmetrized 4-D sort'

1	iin=inp_str('Name of the Setup_file (?? or ??command for info on commands)', setupfile)
	if(iin.lt.1) call exit(0)

	if(setupfile(1:2).eq.'??') then
	  lfile=lengthc(setupfile)
	  if(lfile.eq.2) then
	    write(6,*)
	    write(6,*)'         F O R M A T commands'
	    do ii=1,NFORMCOM
	       lcom=lengthc(form(ii))
*	       if(lcom.gt.0) write(6,'(1x,a<lcom>)') form(ii)(1:lcom)
	       if(lcom .gt. 0)then
	          if( lcom .le. 72 )then
		     write(6,1104) form(ii)(1:lcom)
	          else
	             ii1 = index(form(ii)(56:lcom), ' ')
		     if( ii1 .gt. 0 )then
		        ii1 = ii1 + 55
		     	write(6,1101) form(ii)(1:ii1)
		     	write(6,1102) form(ii)(ii1+1:lcom)
		     else
		        ii1 = 70
		     	write(6,1103) form(ii)(1:ii1)
		     	write(6,1102) form(ii)(ii1+1:lcom)
		     endif
	          endif
	       endif
	    enddo
	    write(6,*)
	    write(6,*)'         D E C L A R A T I O N commands'
	    do ii=1,NDECLCOM
	       lcom=lengthc(decl(ii))
*	       if(lcom.gt.0) write(6,'(1x,a<lcom>)') decl(ii)(1:lcom)
	       if(lcom .gt. 0)then
	          if( lcom .le. 72 )then
		     write(6,1104) decl(ii)(1:lcom)
	          else
	             ii1 = index(decl(ii)(56:lcom), ' ')
		     if( ii1 .gt. 0 )then
		        ii1 = ii1 + 55
		     	write(6,1101) decl(ii)(1:ii1)
		     	write(6,1102) decl(ii)(ii1+1:lcom)
		     else
		        ii1 = 70
		     	write(6,1103) decl(ii)(1:ii1)
		     	write(6,1102) decl(ii)(ii1+1:lcom)
		     endif
	          endif
	       endif
	    enddo
	    write(6,*)
	    write(6,*)'         A N A L Y S I S commands'
	    do ii=1,NANALCOM
	       lcom=lengthc(anal(ii))
*	     if(lcom.gt.0) write(6,'(1x,a<lcom>)') anal(ii)(1:lcom)
	       if(lcom .gt. 0)then
	          if( lcom .le. 72 )then
		     write(6,1104) anal(ii)(1:lcom)
	          else
	             ii1 = index(anal(ii)(56:lcom), ' ')
		     if( ii1 .gt. 0 )then
		        ii1 = ii1 + 55
		     	write(6,1101) anal(ii)(1:ii1)
		     	write(6,1102) anal(ii)(ii1+1:lcom)
		     else
		        ii1 = 70
		     	write(6,1103) anal(ii)(1:ii1)
		     	write(6,1102) anal(ii)(ii1+1:lcom)
		     endif
	          endif
	       endif
	    enddo
	    write(6,*) '* The HGATE  (if defined) can be applied to these commands'
	    write(6,*)
	    goto 1
	  endif
	  syntax=.true.
	  comline=setupfile(3:)
	  lcomline=lengthc(comline)
	  phase=1
	  goto 11
	endif
	
1101	format(1x,a<ii1>)       	  
1102	format(17x,a<lcom-ii1>) 	  
1103	format(1x,a<ii1>,'-')   	  
1104	format(1x,a<lcom>)      	  

	call filetype(setupfile,'setup')
	lsetupfile=lengthc(setupfile)

	call lib$get_lun(inplu)
	open(unit=inplu,file=setupfile,status='old',readonly,err=1000)

	dataformat=NOFORMAT
	ntipi=0
	do ii=0,NDTYPE
	  exists(ii)=.false.
	enddo
	hasgiven=0
	pairsgiven=0
	ncomandi=0
	nstatcom=0
	syntax=.false.
	phase=1			! first the format

10	call gs_readline
11	if(lcomline.lt.0) then
	  close(inplu)
	  call lib$free_lun(inplu)
	  return
	endif

	do ii=1,nsynt
	  SYNT(ii)=' '
	enddo

        call istr_ch2up(comline,comsort)
	goto(100,200,300) PHASE
	call exit(0)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!! FORMAT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

100	iic=istr_cmd(comsort,form,nformcom,whichcom)
	goto(101,102,103,104,105,106,107,108,109) whichcom
	PHASE=2					! not a Format command
	goto 200	

101	call gs0_format
	goto 800

102	goto 900

103	goto 900

104	goto 900

105	goto 900
	
106	call gs0_header
	goto 800

107	call gs0_detector
	goto 800

108	call gs0_cdetector
	goto 800

109	goto 900

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!! DECLARATIONS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

200	iic=istr_cmd(comsort,decl,ndeclcom,whichcom)
	if( DATAFORMAT .EQ. NOFORMAT .and.  .not.syntax ) then
		write(6,*)' %%%ERROR - No data format defined'
		write(6,*)'    ----->  Use FORMAT command to define data type'
		call exit(0)
	endif
	goto(201,202,203,204,205,206,207,208,209) whichcom
	PHASE=3					! not a Declaration
	goto 300	

201	call gs0_rawfoldmin
	goto 800

202	call gs0_hgatedef
	goto 800

203	call gs0_pairdef
	goto 800

204	goto 900
205	goto 900
206	goto 900
207	goto 900
208	goto 900
209	goto 900

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!! ANALYSIS COMMANDS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

300	if(ncomandi.ge.MAXCOMANDI) call gs_syntax1('Max. number of commands is',MAXCOMANDI)
	iic=istr_cmd(comsort,anal,nanalcom,whichcom)
	if(iic.lt.0) then
	  call ansi_bell(lu1)
	  write(6,*) 'Unknown command'
	  write(6,*)
	  if(syntax) goto 1
	  call exit(0)
	endif
	ncomandi=ncomandi+1
	icmd=ncomandi
	com(icmd).whichcom=whichcom
	goto(	    301,302,303,304,305,306,307,308,309,
     1	310,311,312,313,314,315,316,317,318,319,
     2	320,321,322,323,324,325,326,327,328,329,
     3	330,331,332,333,334,335,336,337,338,339,
     4	340,341,342,343,344,345,346,347,348,349,
     5	350,351,352,353,354,355,356,357,358,359,
     6	360,361,362,363,364,365,366,367,368,369,
     7	370,371,372,373,374,375,376,377,378,379,
     8	380,381,382,383,384,385,386,387,388,389,
     9	390,391,392,393,394,395,396,397,398,399,
     1	400,401,402,403,404,405,406,407,408,409,
     1	410,411,412,413,414,415,416,417,418,419,
     1	420,421,422,423,424,425,426,427,428,429,
     1	430,431,432,433,434,435,436,437,438,439,
     1	440,441,442,443,444,445,446,447,448,449) whichcom

	call ansi_bell(6)
	stop 'Unknown command in GETINPUT'

301	call gs0_fold(com(icmd))
	goto 800

302	call gs0_gate(com(icmd))
	goto 800

303	call gs0_gates(com(icmd))
	goto 800

304	call gs0_window(com(icmd))
	goto 800

305	call gs0_banana(com(icmd))
	goto 800

306	call gs0_bananas(com(icmd))
	goto 800

307	call gs0_filter(com(icmd))
	goto 800

308	call gs0_xban(com(icmd))
	goto 800

309	goto 900
310	call gs0_recal_mult(com(icmd))
	goto 800

311	call gs0_gain(com(icmd))
	goto 800

312	call gs0_recal(com(icmd))
	goto 800

313	call gs0_recal_lut(com(icmd))
	goto 800

314	call gs0_time_reference(com(icmd))
	goto 800

315	call gs0_timing(com(icmd))
	goto 800

316	call gs0_recal_doppler(com(icmd))
	goto 800

317	call gs0_recal_kinenew(com(icmd))
	goto 800

318	call gs0_time_adjust(com(icmd))
	goto 800

319	call gs0_pin(com(icmd))
	goto 800

320	call gs0_hk(com(icmd))
	goto 800

321	call gs0_add(com(icmd))
	goto 800

322	call gs0_kill(com(icmd))
	goto 800

323	call gs0_select(com(icmd))
	goto 800

324	call gs0_store_event(com(icmd))
	goto 800

325	call gs0_recall_event(com(icmd))
	goto 800

326	call gs0_write_event(com(icmd))
	goto 800

327	call gs0_list_event(com(icmd))
	goto 800

328	call gs0_reorder(com(icmd))
	goto 800

329	call gs0_statistics(com(icmd))
	goto 800

330	call gs0_swap(com(icmd))
	goto 800

331	call gs0_mask(com(icmd))
	goto 800

332	call gs0_move(com(icmd))
	goto 800

333	call gs0_merge(com(icmd))
	goto 800

334	call gs0_split(com(icmd))
	goto 800

335	call gs0_newid(com(icmd))
	goto 800

336	call gs0_ebkill(com(icmd))
	goto 800

337	call gs0_combine(com(icmd))
	goto 800

338	call gs0_addback(com(icmd))
	goto 800

339	call gs0_copy(com(icmd))
	goto 800

340	call gs0_useful(com(icmd))
	goto 800

341	call gs0_thresholds(com(icmd))
	goto 800
	
342	goto 900
343	goto 900
	
344	call gs0_mult(com(icmd))
	goto 800

345	call gs0_mean_value(com(icmd))
	goto 800

346	call gs0_div(com(icmd))
	goto 800
	
347	call gs0_SLtoCM(com(icmd))
	goto 800
	
348	call gs0_recal_choose(com(icmd))
	goto 800
	
349	call gs0_wmean_value(com(icmd))
	goto 800

350	call gs0_recal_polar(com(icmd))
	goto 800

351	goto 900
352	goto 900
353	goto 900

354	call gs0_angles_pms(com(icmd))
	goto 800

355	call gs0_track_pms(com(icmd))
	goto 800

356	call gs0_qvalue_pms(com(icmd))
	goto 800

357	call gs0_bpvelocity_pms(com(icmd))
	goto 800

358	call gs0_xfp_pms(com(icmd))
	goto 800
	
359	goto 900
360	goto 900
361	goto 900
362	goto 900
363	goto 900
364	goto 900
365	goto 900
366	goto 900
367	goto 900
368	goto 900
369	goto 900
370	goto 900
371	goto 900
372	goto 900
373	goto 900

374	call gs0_break
	goto 800
375	call gs0_pairgate(com(icmd))
	goto 800

376	call gs0_pow(com(icmd))
	goto 800

377	call gs0_pairkill(com(icmd))
	goto 800

378	goto 900
379	goto 900
380	goto 900
381	goto 900
382	goto 900
383	goto 900
384	goto 900
385	goto 900
386	goto 900
387	goto 900
388	goto 900
389	goto 900
390	goto 900

391	call usersub1(0)
	goto 800

392	call usersub2(0)
	goto 800

393	call usersub3(0)
	goto 800

394	call usersub4(0)
	goto 800

395	call usersub5(0)
	goto 800

396	call usersub6(0)
	goto 800

397	call usersub7(0)
	goto 800

398	call usersub8(0)
	goto 800

399	call usersub9(0)
	goto 800

400	goto 900

401	call gs0_projections(com(icmd))
	goto 800

402	call gs0_sort1d(com(icmd))
	goto 800

403	call gs0_sort1d_m(com(icmd))
	goto 800

404	goto 900
405	goto 900
406	goto 900
407	goto 900
408	goto 900
409	goto 900
410	goto 900
	
411	call gs0_sort2d(com(icmd))
	goto 800

412	call gs0_sort3d(com(icmd))
	goto 800

413	call gs0_sort4d(com(icmd))
	goto 800

414	goto 900
415	goto 900

416	call gs0_sort2d_ab(com(icmd))
	goto 800

417	goto 900
418	goto 900
419	goto 900
420	goto 900	

421	call gs0_sort2d_symm(com(icmd))
	goto 800

422	call gs0_sort3d_symm(com(icmd))
	goto 800

423	call gs0_sort4d_symm(com(icmd))
	goto 800

424	goto 900

425	call gs0_sort3d_pair(com(icmd))
	goto 800

426	goto 900

427	call gs0_sort3d_diff(com(icmd))
	goto 800

428	goto 900
429	goto 900
430	goto 900

431	call gs0_sort2d_hsymm(com(icmd))
	goto 800

432	call gs0_sort3d_hsymm(com(icmd))
	goto 800

433	call gs0_sort4d_hsymm(com(icmd))
	goto 800

434	goto 900
435	goto 900
436	goto 900
437	goto 900
438	goto 900
439	goto 900
440	goto 900
441	goto 900
442	goto 900
443	goto 900
444	goto 900
445	goto 900
446	goto 900
447	goto 900
448	goto 900
449	goto 900
			
	
800	if(syntax) goto 1
	iin=lengthc(comline)
	if(iin.ne.0) call gs_syntax('Too many parameters')
	goto 10

900	stop 'Unknown command'

1000	call ansi_bell(lu1)
	write(lu1,*) 'Error opening  SETUPFILE   '//SETUPFILE
	call ansi_bell(lu1)
	call exit(0)

	END

	subroutine LISTPROG(lun)

! Lista il programma di analisi definito nel file di comando

# 1 "./gsort.inc" 1 
# 1 "./gsort.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./gsort.def" 2 
# 1 "./gsort.par" 1 
	INTEGER LU1		! LU per messaggi
	PARAMETER (LU1=6)
	INTEGER LU2		! LU per Logbook
	PARAMETER (LU2=7)
	INTEGER INDENT		! incolonnamento listato programma
	PARAMETER (INDENT=20)
*	INTEGER NPRINT		! intervallo per stampa status su LU1
*	PARAMETER (NPRINT=400)
	INTEGER NPRINT2		! intervallo per stampa status su LU2
	PARAMETER (NPRINT2=2000)
	INTEGER NSTOP		! intervallo per check di mat_stop
	PARAMETER (NSTOP=1000)
	INTEGER NSYNT		! # righe per la sintassi
	PARAMETER (NSYNT=5)

	INTEGER MAXRES		! Risoluzione massima gestita
	PARAMETER (MAXRES=32768)
	INTEGER MINRES		! Risoluzione minima gestita
	PARAMETER (MINRES=16)
	INTEGER MAXDET		! MAX # di rivelatori
	PARAMETER (MAXDET=256)
	INTEGER MAXPAR		! MAX # di parametri/rivelatore
	PARAMETER (MAXPAR=16)
	INTEGER NDTYPE		! Numero di tipi di rivelatore
	PARAMETER (NDTYPE=16)
	REAL PSTEP
	PARAMETER (PSTEP=359./1024.)

	INTEGER NOFORMAT	! No format defined
	PARAMETER (NOFORMAT=-1)
	INTEGER GASP		! Dati tipo GASP
	PARAMETER (GASP=1)
	INTEGER EURO		! Dati tipo Euroball
	PARAMETER (EURO=2)
	INTEGER PI8		! Dati tipo 8Pi-Berkeley
	PARAMETER (PI8=3)
	INTEGER GSPH		! Dati tipo GAMMASPHERE
	PARAMETER (GSPH=4)
	INTEGER GSPN		! Dati tipo GASP - nuova DAQ (2001)
	PARAMETER (GSPN=5)
	INTEGER YALE		! Dati tipo YrastBall - Yale Univ.
	PARAMETER (YALE=6)
	INTEGER TNDB
	PARAMETER (TNDB=7)	! Format Tandem-Bucharest
	INTEGER PRISMA
	PARAMETER (PRISMA=8)	! Format PRISMA (spettrometro)
	INTEGER GSR
	PARAMETER (GSR=9)       ! Temporary, GSPH reduced data
	INTEGER GANIL
	PARAMETER (GANIL=10)    ! GANIL data (EXOGAM,DIAMANT,NWALL)
	
	INTEGER CSEG,QSEG,TSEG,ISEG,SISEG, PPACSEG, DANTESEG
	PARAMETER (CSEG=7, QSEG=4, TSEG=1, ISEG=1, SISEG=4, PPACSEG=10, DANTESEG=8)
	INTEGER CDET,QDET,TDET,SIDET, PPACDET
	PARAMETER (CDET=15, QDET=26, TDET=30, SIDET=40, PPACDET=1)
	INTEGER COFF,QOFF,TOFF,FOFF
	PARAMETER (COFF=0, QOFF=COFF+CDET*CSEG, TOFF=QOFF+QDET*QSEG, FOFF=TOFF+TDET*TSEG)

	INTEGER MAXNBAN		! MAX # banane per BANANA e PIN
	PARAMETER (MAXNBAN=32)
	INTEGER MAXRCL_ORD	! MAX # coefficienti di ricalibrazione
	PARAMETER (MAXRCL_ORD=6)
	INTEGER MAXRCL_REGS	! MAX # regioni di ricalibrazione
	PARAMETER (MAXRCL_REGS=6)

	INTEGER MAXTADJ		! MAX # di parametri in time_adjust
	PARAMETER (MAXTADJ=16)

	INTEGER MAXHASH		! MAX # liste hashgates
	PARAMETER (MAXHASH=50)

	INTEGER MAXPAIRS	! MAX # liste ind=f(id,id) definibili
	PARAMETER (MAXPAIRS=4)
	
	INTEGER MAXMATDIM	! massima dimensione della matrice
	PARAMETER (MAXMATDIM=4)

	INTEGER DEFSTEP2D	! suddivisione default 2D
	PARAMETER (DEFSTEP2D=128)
	INTEGER DEFSTEP3D	! suddivisione default 3D
	PARAMETER (DEFSTEP3D=64)
	INTEGER DEFSTEP4D	! suddivisione default 4D
	PARAMETER (DEFSTEP4D=32)

	INTEGER DEFSTEP2DS	! suddivisione default 2D_symm
	PARAMETER (DEFSTEP2DS=128)
	INTEGER DEFSTEP3DS	! suddivisione default 3D_symm
	PARAMETER (DEFSTEP3DS=64)
	INTEGER DEFSTEP4DS	! suddivisione default 4D_symm
	PARAMETER (DEFSTEP4DS=32)

	INTEGER DEFSTEP2DH	! suddivisione default 2D_hsymm
	PARAMETER (DEFSTEP2DH=32)
	INTEGER DEFSTEP3DH	! suddivisione default 3D_hsymm
	PARAMETER (DEFSTEP3DH=8)
	INTEGER DEFSTEP4DH	! suddivisione default 4D_hsymm
	PARAMETER (DEFSTEP4DH=8)

	INTEGER MAXBYTES	! Max. lunghezza records su nastro
	PARAMETER (MAXBYTES=32*1024)
	INTEGER MAXWORDS	! Max. lunghezza records su nastro
	PARAMETER (MAXWORDS=MAXBYTES/2)
	INTEGER MAXEVL		! MAX lunghezza evento (words)
	PARAMETER (MAXEVL=2047)

	INTEGER MTFLAGR		! Flag per sync. nastro input
	PARAMETER (MTFLAGR=1)
	INTEGER MTFLAGW		! Flag per sync. nastro output
	PARAMETER (MTFLAGW=2)

	INTEGER NFORMCOM	! # comandi di Formato
	PARAMETER (NFORMCOM=9)
	INTEGER NDECLCOM	! # comandi di Dichiarazione
	PARAMETER (NDECLCOM=9)
	INTEGER NANALCOM	! # comandi di Analisi
	PARAMETER (NANALCOM=199)

	INTEGER MAXCOMANDI	! MAX # di COMANDI
	PARAMETER (MAXCOMANDI=360)
# 3 "./gsort.def" 2 

	structure/detector/
	  union
	    map
	      integer id
	    endmap
	    map
	      integer ival(-1:maxpar-1)
	    endmap
	  endunion
	  real    xval( 0:maxpar-1)
	endstructure

	structure/hashdat/
	  INTEGER IND,PAR,RES			! su quale parametro
	  INTEGER WHICH				! gate number
	  logical*1 SAMEFORALL			! same gate for all detectors
	  logical*1 FROMFILE			! dati da file
	  INTEGER NGATES(0:MAXDET-1)		! quanti gates ha letto
	  CHARACTER*72 FILE			! File dei gates
	  REAL    DIST2(0:1)			! Distanza**2 normalizzata per gate sferici
	endstructure
	
	structure/pairsdat/
	  INTEGER INDMAX
	  CHARACTER*72 FILE			! nomi dei file con i dati
	  INTEGER PIND(0:MAXDET-1,0:MAXDET-1)
	endstructure

	structure/recall/
	  logical*1 always
	endstructure

	structure/fold/
	  INTEGER IND		! Tipo di parametro
	  INTEGER MIN		! Finestra sul fold
	  INTEGER MAX		! Finestra sul fold
	endstructure

	structure/gate/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  logical*1 INSIDE	! dentro/fuori
	  INTEGER LO		! Estremo inferiore
	  INTEGER HI		! Estremo superiore
	  INTEGER NGATES	! Quanti intervalli
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	endstructure

	structure/gatesdat/
	  logical*1 BAD(0:1)
	endstructure

	structure/filter/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PARS		! parametro sorgente
	  INTEGER PARD		! parametro destinazione
	  INTEGER RES		! Risoluzione filtro
	  character*80 file     ! no comment
	  integer iform         !
	  real F(0:MAXRES)        !
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	  logical*1 initialized   !
	endstructure

	structure/window/
	  INTEGER  IND		! Tipo di parametro
	  INTEGER  LO(0:MAXPAR-1)! Valore inferiore
	  INTEGER  HI(0:MAXPAR-1)! Valore superiore
	  INTEGER  FL		! Minfold
	  INTEGER  FH		! Minfold
	  logical*1  SAME		! Same as before window
	endstructure

	structure/banana/
	  INTEGER IND(2)	! Tipo di parametro x,y
	  INTEGER PAR(2)	! Parametro asse x,y
	  INTEGER RES(2)	! Risoluzione della banana
	  INTEGER NBAN		! Numero di banane
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 INSIDE	! In-Out
	  logical*1 SAME		! Same as before banana
	  logical*1 multiadc      ! N.M. - for ADC-dependent banana
	  logical*1 ignore(0:MAXDET-1) ! only for ADC-dependent banana
	endstructure

	structure/bananadat/
	  character*72 file
	  integer ban(2,0:1)
	endstructure

	structure/bananasdat/
	  character*72 file(MAXNBAN)
	  integer ban(2,0:1)
	endstructure

	structure/pairgate/
	   integer IND
	   integer LO
	   integer HI
	   integer FL
	   integer FH
	   integer PAR
	   integer RES
	   integer*2 PVAL(0:MAXDET-1,0:MAXDET-1)
	   integer*2 LIST(0:MAXDET-1)
	   character*72 filename
	   logical*1 INSIDE
	   logical*1 SAME
	endstructure

	structure/pin/
	  INTEGER IND(4)	! Tipo di parametro per banane
	  INTEGER PAR(4)	! Parametro asse x,y ; la massa e il tipo della particella
	  INTEGER RES(2)	! Risoluzione delle banane
	  INTEGER FIX		! Parametro fisso per il risultato
	  INTEGER FIXRES        ! Risoluzione dell parametro fisso
	  INTEGER NBAN		! quante banane
	  INTEGER F1(MAXNBAN)	! Numero di particelle della banana
	  INTEGER F2(MAXNBAN)	! Peso della banana in PIN
	  INTEGER F3(MAXNBAN)	! Massa della particella
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before PIN
	  logical*1 multiadc(MAXNBAN)          ! N.M. - for ADC-dependent banana
	  logical*1 ignore(MAXNBAN,0:MAXDET-1) ! only for ADC-dependent banana
	endstructure

	structure/hk/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER FIXH		! Parametro fisso per H
	  INTEGER FIXk		! Parametro fisso per k
	  REAL	  OFFS		! Offset per H
	  REAL	  GAIN		! Guadagno finale per H
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	endstructure

	structure/recal/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 RUN		! coefficenti run-dependent
	  logical*1 SAME	! Same as before recal
	  logical*1 ROUND       ! Round the result to integer value
	  logical*1 ZERO        ! Set negative result to zero
	  CHARACTER*73 FILE	! files dei coefficenti
	endstructure

	structure/recal_choose/
	  INTEGER IND(3)	! Tipo di parametro
	  INTEGER PAR(3)	! Quale parametro
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL    VAL           ! Valore di riferimento per fare la scelta
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  CHARACTER*73 FILE1	! files dei coefficenti
	  CHARACTER*73 FILE2	! files dei coefficenti
	  logical*1 RUN		! coefficenti run-dependent
	  logical*1 SAME	! Same as before recal
	  logical*1 ROUND       ! Round the result to integer value
	endstructure

	structure/calcoef/
	  INTEGER*4 ORD
	  REAL*4    COEF(MAXRCL_ORD)
	endstructure

	structure/mcalcoef/
	  integer nregs
	  INTEGER ORD(MAXRCL_REGS)
	  REAL    COEF(MAXRCL_ORD,MAXRCL_REGS)
	  real    limit(0:MAXRCL_REGS)
	endstructure


	structure/doppler/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET		! Quanti rivelatori
         real*8  fact1
         real*8  fact2
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL	  VC0		! Velocita' di rinculo
	  real    rtheta
	  real    rphi
	  REAL	  E0		! VC0 fino a E0
	  REAL	  E1		! Da VC0 a VC1 tra E0 e E1
	  REAL	  VC1		! VC1 sopra E1
	  REAL	  SLOPE		! Slope tra E0 e E1
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
         logical*1 RefChanged
	  logical*1 CONST		! Costante
	  logical*1 SAME		! Same as before recal
	  CHARACTER*73 FILE	! file con gli angoli
	endstructure

	structure/dopplerdat/
	  real costheta(0:1)
	endstructure
	
	structure/polar/
         real*8  fact1
         real*8  fact2
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET		! Quanti rivelatori
          INTEGER DTHETA        ! Angolo THETA del rivelatore, se viene dato come parametro
          INTEGER DPHI          ! Angolo PHI del rivelatore, se viene dato come parametro
	  INTEGER IND_POLAR
	  INTEGER PTHETA
	  INTEGER PPHI
         INTEGER IND_VEL
         INTEGER PVEL
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL	  VC0		! Velocita' di rinculo
	  REAL	  E0		! VC0 fino a E0
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
          logical*1 FIXED
	  logical*1 VarDetAngle	! Se angoli degli rivelatori sono parametri
	  logical*1 SAME		! Same as before recal
	  CHARACTER*73 FILE	! file con gli angoli
	endstructure
	
	structure/polardat/
	  real cdir(3,0:MAXDET-1)
	endstructure

	structure/meanvalstr/
	  INTEGER IND1(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR1(MAXTADJ)	! Quale parametro
	  INTEGER RES1(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY1	! Quanti definiti
	  REAL    POSITION	! Posizione finale
	  REAL	  FACTOR	! Rejection factor
	  INTEGER IND2	! FIX only
	  INTEGER PAR2	! Quale parametro fisso
	  INTEGER RES2	! Risoluzione
	endstructure

	structure/tadjust/
	  INTEGER IND1(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR1(MAXTADJ)	! Quale parametro
	  INTEGER RES1(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY1	! Quanti definiti
	  REAL    POSITION	! Posizione finale
	  REAL	  FACTOR	! Rejection factor
	  INTEGER IND2(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR2(MAXTADJ)	! Quale parametro
	  INTEGER RES2(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY2	! Quanti definiti
	endstructure

	structure/tref/
	   integer*8 N
# 285

	   real*16 SQSUM

	   INTEGER IND
	   INTEGER PAR
	   INTEGER RES
	   INTEGER REFNO
	   REAL POSITION
	endstructure

	structure/kine/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET_GE	! quanti germani
	  INTEGER NDET_SI	! quanti silici
	  INTEGER BIND(2)	! Tipo di parametro per banane
	  INTEGER BPAR(2)	! Parametro asse x,y
	  INTEGER BRES(2)	! Risoluzione della banana
	  INTEGER NBAN		! Numero di banane
	  INTEGER PINFIX	! Fixpar per PIN (se >=0)
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before recal
	  CHARACTER*72 KINEFILE	! file di descrizione
	  CHARACTER*72 AFILE_GE	! angoli germanio
	  CHARACTER*72 AFILE_SI	! angoli silici
	endstructure

	structure/kinedat/
	  REAL    ACMP,ECMP		! il nucleo composto
	  REAL    XMOMCM		! suo momento
	  REAL    AD(MAXNBAN)		! massa della particella
	  INTEGER MDET(MAXNBAN)		! numero di rivelatori attivi
	  INTEGER F1(MAXNBAN)		! Numero di particelle della banana per PIN
	  INTEGER F2(MAXNBAN)		! Peso della banana per PIN
	  real mom_si(0:MAXDET-1,MAXNBAN)
	  real cdir_ge(3,0:MAXDET-1)
	  real cdir_si(3,0:MAXDET-1)
	  record/bananasdat/kban
	endstructure

	structure/kinenew/
	  INTEGER IND(3)		! Tipo di parametro
	  INTEGER PAR(3)		! Parametro
	  INTEGER RES(3)		! Risoluzione del parametro
	  INTEGER NDET_GE	! quanti germani
	  INTEGER NDET_SI	! quanti silici
	  real ecal
	  real acmp
	  real ecmp
	  real atenuation
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before recal
	  CHARACTER*72 AFILE_GE	! angoli germanio
	  CHARACTER*72 AFILE_SI	! angoli silici
	  character*73 except_file
	  logical*1 except
	endstructure

	structure/kinenewdat/
	  REAL    ACMP,ECMP		! il nucleo composto
	  REAL    XMOMCM		! suo momento
	  real    ecal
	  real    afac
	  real cdir_ge(3,0:MAXDET-1)
	  real cdir_si(3,0:MAXDET-1)
	  integer nregs
	  integer reg(MAXRES,2)
	endstructure

	structure/sltocm/
	  integer IND(2)
	  integer PAR(2)
	  integer NDET_SI
	  real F(2)
	  real COS_TH(0:MAXDET-1)
	  CHARACTER*72 AFILE_SI	! angoli silici
	endstructure


	structure/add/
	  INTEGER IND(3)	! Tipo di parametro
	  INTEGER PAR(3)	! Parametro 1+2==>3
	  REAL	  FAC(3)	! fattori moltiplicativi
	  REAL    OFFSET	! offset da sommare al risultato
	  REAL    GAIN		! gain sul risultato (non sul'offset)
	  INTEGER ICHAN		! canale limite per COMBINE
	  INTEGER DELTA		! Sliding range
	  INTEGER SLIDE		! Sliding value
	  logical*1 CHECK		! check che il secondo parametro sia > limit
	  logical*1 MULT		! fattori moltiplicativi?
	endstructure

	structure/kill/
	  INTEGER IND			! Tipo di parametro
	  logical*1 RUN			! run dependent selective kill
	  CHARACTER*72 FILE		! files contenente i detbad
	  logical*1 DET(0:MAXDET-1)	! Quali rivelatori
	endstructure

	structure/killdat/
	  logical*1 remove(0:MAXDET-1,0:MAXDET-1)
	endstructure

	structure/select/
	  INTEGER IND		! Tipo di parametro
	  logical*1 DET(0:MAXDET-1)  ! Quali rivelatori
	endstructure

	structure/listev/
	  logical*1 TOFILE	! list events on file
	  logical*1 ISOPEN
	  INTEGER LUN
	  CHARACTER*72 FILE
	endstructure

	structure/reorder/
	  INTEGER IND		! Tipo di parametro
	endstructure

	structure/statistics/
	  INTEGER WHICH
	endstructure

	structure/statdat/
	  INTEGER FOLD(0:MAXDET,0:NDTYPE,0:1)
	endstructure

	structure/swap/
	  INTEGER IND(2)	! Tipo di parametro x,y
	  INTEGER PAR(2)	! Parametro asse x,y
	endstructure

	structure/mask/
	  INTEGER IND		! Tipo di parametro x
	  INTEGER PAR		! Parametro asse x
	  INTEGER MASK		! Maschera
	  logical*1 ONE		! solo un parametro o tutti?
	endstructure

	structure/useful/
	  INTEGER IND		! Tipo di parametro (deve essere 0)
	  INTEGER PAR1		! Parametro per RUN#
	  INTEGER PAR2		! Parametro per REC#
	  INTEGER PAR3		! Parametro per EVCOUNT#
	  INTEGER PAR4		! Parametro per EVNUMBER#
	endstructure

	structure/move/
	  INTEGER IND1			! da quale tipo
	  INTEGER IND2			! a  quale tipo
	  INTEGER PAR1			! da quale parametro
	  INTEGER PAR2			! a  quale parametro
	  INTEGER OFFSET                ! offset alla nuova numerazione
	  logical*1 CONDITION		! TRUE if GATE or copy detector
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
c	  logical*1 INSIDE	! dentro/fuori
	  INTEGER LO		! Estremo inferiore
	  INTEGER HI		! Estremo superiore
c	  INTEGER NGATES	! Quanti intervalli
c	  INTEGER FL		! Minfold
c	  INTEGER FH		! Maxfold
c	  logical*1 SAME		! Same as before gate
	  INTEGER HOWMANY		! Howmany to move
	  INTEGER WHICH(0:MAXDET-1)	! Quali rivelatori
	endstructure

	structure/splitmerge/
	  INTEGER NIND			! quanti tipi di rivelatore coinvolti
	  INTEGER IND(0:NDTYPE)		! Quali rivelatori
	  logical*1 REMOVE(0:NDTYPE)	!
	  INTEGER OFFSET(0:NDTYPE)	! Offset degli indici
	endstructure

	structure/newid/
	  INTEGER IND			! tipo di rivelatore
	  INTEGER LUT(0:MAXDET-1)	! mappa dei nuovi indici
	  logical*1 REORDER		! reordina dopo la mappatura
	  CHARACTER*72 FILE		! files dei nuovi id
	endstructure

	structure/addback/
	  INTEGER IND			! Tipo di parametro
	  INTEGER PAR			! Quale parametro
	  INTEGER RES			! Risoluzione del parametro
	  INTEGER NSEGS			! quanti segmenti (se composito)
	  logical*1 REJECT		! elimina le doppie non contigue
	  logical*1 PATTERN		! produce la hit-pattern sommando tutto
	  INTEGER PATPAR		! dove la registra
	  logical*1 TGATE			! verifica la relazione temporale?
	  INTEGER TPAR			! su quale parametro
	  INTEGER TVAL			! massima deviazione
	endstructure

        structure /pms_str/
	    real*8  sin_70
	    real*8  sin_110
	    real*8  cos_20
	    real*8  tan_20
	    integer ind
	    integer par_r
	    integer par_p
	    integer par_e
	    integer par_icp
	    integer ind_theta
	    integer par_theta
	    integer ind_path
	    integer par_path
	    integer ind_x
	    integer par_x
	    integer ind_ic
	    integer par_a
	    integer par_b
	    integer par_c
	    integer par_d
	    real    thr
	    real    qfact
	end structure
	
	structure /pms_dat/
	   real*8 R(40,0:100)
	end structure
	 
	structure /qvalue_str/
	   real*8  costhp
	   real*8  sinthp
	   real*8  amu
	   integer ind_q
	   integer par_q
	   integer ind_mass
	   integer par_mass
	   integer ind_theta
	   integer par_theta
	   integer ind_phi
	   integer par_phi
	   integer ind_beta
	   integer par_beta
	   integer AP
	   integer AT
	   integer ATOT
	   integer    low
	   integer    high
	   real    ep
	   real    thp
	   real    gain
	   real    offset
	end structure

       structure /prisma_angles_str/
          real*8  prisma_angle
          real*8  D
          real*8  sinalpha
          real*8  cosalpha
          real*8  costhp
          real*8  sinthp
	  real*8  cx(0:3)
	  real*8  cy(0:3)
          integer ind
          integer parx
          integer pary
          integer parz
          integer parq
          integer part
	  integer parp
	  integer pard
	  integer degx
	  integer degy
	  integer oldstyle
	  character*128 calfile
       end structure
	   
	structure/proje/
	  INTEGER LEN		! Lunghezza totale degli spettri (LW)
	  INTEGER MFRES 	! risoluzione max. di F
	  INTEGER*8 NINCR
	endstructure

	structure/projedat/
	  CHARACTER*16 NAME(0:MAXPAR-1,0:NDTYPE)
	  INTEGER      ADDR(0:MAXDET-1,0:MAXPAR-1,0:NDTYPE)
	  INTEGER      SPEC(0:1)
	endstructure

	structure/spectrum/
	  INTEGER IND			! Tipo di parametro
	  INTEGER PAR			! Quale parametri
	  INTEGER RES			! numero di canali dello spettro
	  INTEGER IHASH			! Quale tabella di hash (se >=0)
	  INTEGER HIND,HPAR		! riportati qui per comodita'
	  logical*1 HSAME			! IND e' quello su cui applicare HGATE
	  INTEGER NHASH			! Quante volte
	  logical*1 SPHERICAL		! gate di tipo elissoidale
	  INTEGER MINFOLD		! fold minimo (per hsort1d)
	  INTEGER*8 NINCR		! numero di incrementi
	  CHARACTER*72 FILE		! Filename dello spettro
	endstructure

	structure/specdat/
	  INTEGER SPEC(0:1)
	endstructure

	structure/matrix/

	  integer*8 CMTBASE

	  INTEGER NDIM			! Ordine della matrice
	  INTEGER IND(MAXMATDIM)	! Tipo di parametri per i due lati
	  INTEGER PAR(MAXMATDIM)	! Quale dei parametri
	  INTEGER RES(MAXMATDIM)	! Dimensioni lati della matrice
	  INTEGER STEP(MAXMATDIM)	! suddivisione lati della matrice
	  INTEGER MATMODE		! 0=normal 1=symmetric 2=halfsymmetric
	  INTEGER MSDIM			! numero di indici simmetrizzati
	  INTEGER DOFFSET		! per SORTxD_DIFF
	  INTEGER NIND
	  INTEGER IHASH			! Quale tabella di hash (se >=0)
	  INTEGER HIND,HPAR		! riportati qui per comodita'
	  logical*1 HSAME			! IND e' quello su cui applicare HGATE
	  INTEGER NHASH			! quante volte
	  logical*1 SPHERICAL		! gate di tipo elissoidale
	  INTEGER TYPE
	  INTEGER IND1,IND2,IND3,IND4
	  INTEGER PAR1,PAR2,PAR3,PAR4
	  logical*1 HHH1,HHH2,HHH3,HHH4
	  CHARACTER*72 FILE		! Filename matrice
	endstructure

	structure/iodef/
	  logical*1 datafile	! TRUE se da file o Virtuale
	  logical*1 virtuale
	  logical*1 closed
	  integer lun
	  integer flag
	  integer wlen
	  integer run
	  integer rec
	  integer maxrec
	  integer evcount	! internal count
	  integer evnumber	! from ACQ
	  integer vrun,vevents
CVMS	  integer*2 iosb(4)
	  integer iosb(4)
CVMS	  integer*2 mtch,hctm
	  integer mtch
	  character*72 name
	endstructure

	structure/oevdef/
	  logical*1 defined
	  logical*1 same				! Same FILE structure on output
	  logical*1 sameRUN			! Same RUN number on output
	  logical*1 ask				! Will ask every EOF
	  logical*1 reduce			! Output events in forma ridotta
	  logical*1 det(0:NDTYPE)			! Output this detector
	  logical*1 par(0:MAXPAR-1,0:NDTYPE)  	! Output this parameter
	  integer evlen,pointer
	  logical*1 done
	endstructure

	structure/commands/
	  union
	    map
	      record/recall/recall
	    endmap
	    map
	      record/fold/fold
	    endmap
	    map
	      record/gate/gate
	    endmap
	    map
	      record/filter/filter
	    endmap
	    map
	      record/window/win
	    endmap
	    map
	      record/pairgate/pgate
	    endmap
	    map
	      record/banana/ban
	    endmap
	    map
	      record/pin/pin
	    endmap
	    map
	      record/recal/rcl
	    endmap
	    map
	      record/kine/kine
	    endmap
	    map
	      record/doppler/doppl
	    endmap
	    map
	      record/tadjust/tadj
	    endmap
	    map
	      record/tref/tref
	    endmap
	    map
	      record/hk/hk
	    endmap
	    map
	      record/add/add
	    endmap
	    map
	      record/kill/kill
	    endmap
	    map
	      record/select/sel
	    endmap
	    map
	      record/listev/lev
	    endmap
	    map
	      record/reorder/reo
	    endmap
	    map
	      record/statistics/stat
	    endmap
	    map
	      record/swap/swap
	    endmap
	    map
	      record/mask/mask
	    endmap
	    map
	      record/useful/useful
	    endmap
	    map
	      record/move/move
	    endmap
	    map
	      record/newid/newid
	    endmap
	    map
	      record/splitmerge/splmrg
	    endmap
	    map
	      record/polar/dpolar
	    endmap
	    map
	      record/pms_str/pms
	    endmap
	    map
	      record/qvalue_str/qvalue
	    endmap
	    map
	      record /prisma_angles_str/ang
	    endmap
	    map
	      record/addback/abck
	    endmap
	    map
	      record/proje/pro
	    endmap
	    map
	      record/spectrum/spec
	    endmap
	    map
	      record/matrix/mat
	    endmap
	  endunion

	  integer*8 addr

	  integer size
	  integer whichcom
	endstructure

	structure /tndio/
	  integer nfiles
	  integer current
	  logical*1 presorted
	end structure
# 2 "./gsort.inc" 2 

	INTEGER	  LLUN
	INTEGER	  LLIST
	INTEGER   GS2LUN
	INTEGER   LGSDIR
	CHARACTER GSDIR*64
	CHARACTER LIST*256
	CHARACTER LINE*256
	CHARACTER COMSORT*40
	integer INFILES
	COMMON /VARIECOM/ LLUN,LLIST,GS2LUN,LGSDIR,GSDIR,LIST,LINE,COMSORT,INFILES

	logical*1 BREAK
	logical*1 KILLEV
	logical*1 AUTONUMBER	! File etichettati con RUN#
	logical*1 TAPEDISMOUNT	! Controllo fine nastro
	logical*1 LASTTAPE	! Controllo fine nastro
	logical*1 AUTOLOADER
	logical*1 STORED_EV
	logical*1 DEFINED_OUT,FINISHED_OUT
	COMMON/FLAGS/ BREAK,KILLEV,AUTONUMBER,TAPEDISMOUNT,LASTTAPE,AUTOLOADER,STORED_EV,
     1 DEFINED_OUT,FINISHED_OUT

	logical*1 SYNTAX
	INTEGER PHASE
	CHARACTER COMLINE*256
	INTEGER LCOMLINE
	INTEGER INPLU
	CHARACTER*128  FORM(NFORMCOM)
	CHARACTER*128  DECL(NDECLCOM)
	CHARACTER*128  ANAL(NANALCOM)
	CHARACTER*128 SYNT(NSYNT)
	COMMON /COMDEFS/ PHASE,COMLINE,LCOMLINE,INPLU,FORM,DECL,ANAL,SYNT,SYNTAX

	RECORD /COMMANDS/COM(MAXCOMANDI)
	INTEGER	NCOMANDI
	INTEGER ICMD
	INTEGER WHICHCOM
	INTEGER INIT_MODE,FINIT_MODE
	COMMON /COMMAND/ COM,NCOMANDI,ICMD,WHICHCOM,INIT_MODE,FINIT_MODE

	INTEGER   DATAFORMAT			! GASP/EUROBALL/8PI
	INTEGER   GASPMAP(0:NDTYPE)		! Mappatura per formato GASP
	INTEGER   EUROMAP(0:NDTYPE)		! Mappatura per formato euroball
	INTEGER   ISEED				! Seed per i numeri random
	INTEGER   NTIPI				! quanti tipi di rivelatori
	INTEGER   NDPAR(0:NDTYPE)		! # parametri
	INTEGER   MDPAR(0:NDTYPE)		! # parametri aggiunti
	INTEGER   TDPAR(0:NDTYPE)		! # parametri totali
	INTEGER   PARRES(0:MAXPAR-1,0:NDTYPE)  	! Risoluzione dei vari parametri

	INTEGER   NDETS(0:NDTYPE)		! # di rivelatori (se composito)
	INTEGER   NSEGS(0:NDTYPE)		! # segmenti (se composito)
	INTEGER   NCPAR(0:NDTYPE)		! # di parametri comuni (se composito)
	
	INTEGER   NITEMS(0:NDTYPE)		! # totale di rivelatori
	logical*1   EXISTS(0:NDTYPE)		! Esiste il tipo di rivelatore
	INTEGER   FOLDMIN(0:NDTYPE)		! Minimo fold da nastro
	CHARACTER*1 DNAME(0:NDTYPE)		! simboli per rivelatori
	COMMON /EVDEF/DATAFORMAT,GASPMAP,EUROMAP,ISEED,NTIPI,NDPAR,MDPAR,
     1TDPAR,PARRES,NDETS,NSEGS,NCPAR,NITEMS,FOLDMIN,EXISTS,DNAME

	INTEGER   CLASS,TAG			! Descrizione dell'evento
	INTEGER   NDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   DOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/DET(0:MAXDET-1)		! i dati
	COMMON /CEVENT/  CLASS,TAG,NDET,DOFF,DET

	INTEGER   sCLASS,sTAG			! Copia dell'evento per SAVE/RECALL
	INTEGER   sNDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   sDOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/sDET(0:MAXDET-1)	! i dati
	COMMON /SEVENT/  sCLASS,sTAG,sNDET,sDOFF,sDET

	INTEGER   mCLASS,mTAG			! Copia di lavoro (Move...)
	INTEGER   mNDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   mDOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/mDET(0:MAXDET-1)	! i dati
	COMMON /mEVENT/  mCLASS,mTAG,mNDET,mDOFF,mDET


	integer*8 HASHADDR(0:MAXHASH-1)

	INTEGER HASHGIVEN
	INTEGER HNTRUE,HSTRUE			! Quanti rivelatori in gate
	logical*1 HLTRUE(0:MAXDET-1)		! Quali rivelatori in gate
	REAL    HDIST1(0:MAXDET-1)		! per gestione gates sferici
	REAL    HDIST2(0:MAXDET-1)		! per gestione gates sferici
	COMMON /HASHDEF/ HASHADDR,HASHGIVEN,HNTRUE,HSTRUE,HLTRUE,HDIST1,HDIST2


	integer*8 PAIRSADDR(0:MAXPAIRS-1)

	INTEGER PAIRSGIVEN
	COMMON /PAIRSDEF/ PAIRSADDR,PAIRSGIVEN


	integer*8 STATADDR

	INTEGER STATSIZE
	INTEGER NSTATCOM			! quanti comandi di statistica
	COMMON /STATISTICS/ STATADDR,STATSIZE,NSTATCOM

	INTEGER NLOST,NCLASS
	INTEGER*8 NINCR(MAXMATDIM)
	INTEGER FLUSHED
	INTEGER NPRINT
	COMMON /STATUS/ NLOST,NCLASS,NINCR,FLUSHED,NPRINT

	integer*2 ievbuf(16*MAXWORDS)	! Buffer dati
	integer*2 ievheader(MAXWORDS)	! header record
	record/iodef/iio
	common/ievcom/ievheader,ievbuf,iio

	record/oevdef/oev
	integer*2 oevent(MAXEVL*4)	! evento da scrivere
	integer*2 oevbuf(MAXBYTES/2)	! Buffer scrittura eventi
	record/iodef/oio
	common/oevcom/oev,oevent,oevbuf,oio

	record /tndio/tnd
	common /tndcom/tnd
# 1128 "gsort.F" 2 

	llun=lun

	if(DATAFORMAT.eq.GASP) then
	  WRITE(lun,*)'      Event format (GASP style)'
	elseif(DATAFORMAT.eq.EURO) then
	  WRITE(lun,*)'      Event format (EUROBALL style)'
	elseif(DATAFORMAT.eq.PRISMA) then
	  WRITE(lun,*)'      Event format (PRISMA style)'
	elseif(DATAFORMAT.eq.PI8) then
	  WRITE(lun,*)'      Event format (8PI-Berkeley style)'
	  if(.NOT.EXISTS(0)) call gs_syntax('At least 3 Header parameters must be defined') 
	  if(NDPAR(0) .lt.3) call gs_syntax('At least 3 Header parameters must be defined') 
	  if(.NOT.EXISTS(1)) call gs_syntax('At least one detector type must be defined') 
	  if(NDPAR(1) .lt.2) call gs_syntax('At least 2 parameters must be defined in detector') 
	elseif(DATAFORMAT.eq.GSPH) then
	  WRITE(lun,*)'      Event format (GAMMASPHERE style)'
	  if(.NOT.EXISTS(1)) call gs_syntax('At least one detector type must be defined') 
	  if(NDPAR(1) .lt.2) call gs_syntax('At least 2 parameters must be defined in detector') 
	elseif(DATAFORMAT.eq.YALE) then
	  WRITE(lun,*)'      Event format (Yale/YRASTBALL style)'
	  if(.NOT.EXISTS(1)) call gs_syntax('At least one detector type must be defined') 
	  if(NDPAR(1) .lt.2) call gs_syntax('At least 2 parameters must be defined in detector') 
	elseif(DATAFORMAT.eq.GSPN) then
	  WRITE(lun,*)'      Event format (GASP new DAQ style)'
	elseif(DATAFORMAT.eq.TNDB) then
	  WRITE(lun,*)'      Event format (TANDEM-Bucharest style)'
	elseif(DATAFORMAT.eq.GSR) then
	  WRITE(lun,*)'      Event format (GAMMASPHERE reduced data)'
	elseif(DATAFORMAT.eq.GANIL) then
	  WRITE(lun,*)'      Event format (GANIL style)'
	else
	  call gs_syntax('Unknown event format')
	endif
	do jj=0,ntipi
	  if(jj.eq.0) then
	    if(.not.EXISTS(0)) goto 5
	    write(list,'(i3,''  HEADER    '',A)') jj,DNAME(0)
	    llist=lengthc(list)+7
	  elseif(NSEGS(jj).le.1) then
	    write(list,'(i3,''  DETECTOR  '',A,I4)') jj,DNAME(jj),NDETS(jj)
	    llist=lengthc(list)+3
	  else
	    write(list,'(i3,''  CDETECTOR '',A,I4,I3)') jj,DNAME(jj),NDETS(jj),NSEGS(jj)
	    llist=lengthc(list)
	  endif
	  llist=llist+1
	  if(ndpar(jj).gt.0) then
	    write(list(llist:),4201) NDPAR(jj),(PARRES(ii,jj),ii=0,NDPAR(jj)-1)
	    llist=lengthc(list)+2
	  else
	    llist=llist+2
	  endif
	  if(MDPAR(jj).gt.0) then
	    write(list(llist:),4202) MDPAR(jj),(PARRES(ii,jj),ii=NDPAR(jj),TDPAR(jj)-1)
	  endif
	  llist=lengthc(list)
	  write(LUN,*) list(1:llist)
5	enddo
4201	format(i3,<NDPAR(jj)>i6)
4202	format('PLUS',i4,<MDPAR(jj)>i6)
	call gs1_rawfoldmin
	
	do ii=0,hashgiven-1
	  call gs1_hgatedef(%val(hashaddr(ii)))
	enddo
	
	do ii=0,pairsgiven-1
	  call gs1_pairdef(ii,%val(pairsaddr(ii)))
	enddo
	
	write(LUN,*)
	WRITE(LUN,*)'        List of the analysis program'
	write(LUN,*)

	do icmd=1,ncomandi
	  whichcom=com(icmd).whichcom
	  lcom=index(anal(whichcom),' ')
	  list=' '
	  write(list,'(i3,2x,a)') icmd,anal(whichcom)(1:lcom)
	  llist=INDENT

	goto(	    301,302,303,304,305,306,307,308,309,
     1	310,311,312,313,314,315,316,317,318,319,
     2	320,321,322,323,324,325,326,327,328,329,
     3	330,331,332,333,334,335,336,337,338,339,
     4	340,341,342,343,344,345,346,347,348,349,
     5	350,351,352,353,354,355,356,357,358,359,
     6	360,361,362,363,364,365,366,367,368,369,
     7	370,371,372,373,374,375,376,377,378,379,
     8	380,381,382,383,384,385,386,387,388,389,
     9	390,391,392,393,394,395,396,397,398,399,
     1	400,401,402,403,404,405,406,407,408,409,
     1	410,411,412,413,414,415,416,417,418,419,
     1	420,421,422,423,424,425,426,427,428,429,
     1	430,431,432,433,434,435,436,437,438,439,
     1	440,441,442,443,444,445,446,447,448,449) whichcom

	  call ansi_bell(6)
	  stop 'Inconsistent command in LISTPROG'

301	  call gs1_fold(com(icmd),%val(com(icmd).addr))
	  goto 800

302	  call gs1_gate(com(icmd),%val(com(icmd).addr))
	  goto 800

303	  call gs1_gates(com(icmd),%val(com(icmd).addr))
	  goto 800

304	  call gs1_window(com(icmd),%val(com(icmd).addr))
	  goto 800

305	  call gs1_banana(com(icmd),%val(com(icmd).addr))
	  goto 800

306	  call gs1_bananas(com(icmd),%val(com(icmd).addr))
	  goto 800

307	  call gs1_filter(com(icmd),%val(com(icmd).addr))
	  goto 800

308	  call gs1_xban(com(icmd),%val(com(icmd).addr))
	  goto 800

309	  goto 900
310	  call gs1_recal_mult(com(icmd),%val(com(icmd).addr))
	  goto 800

311	  call gs1_gain(com(icmd),%val(com(icmd).addr))
	  goto 800

312	  call gs1_recal(com(icmd),%val(com(icmd).addr))
	  goto 800

313	  call gs1_recal_lut(com(icmd),%val(com(icmd).addr))
	  goto 800

314	  call gs1_time_reference(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
315	  call gs1_timing(com(icmd),%val(com(icmd).addr))
	  goto 800

316	  call gs1_recal_doppler(com(icmd),%val(com(icmd).addr))
	  goto 800

317	  call gs1_recal_kinenew(com(icmd),%val(com(icmd).addr))
	  goto 800

318	  call gs1_time_adjust(com(icmd),%val(com(icmd).addr))
	  goto 800

319	  call gs1_pin(com(icmd),%val(com(icmd).addr))
	  goto 800

320	  call gs1_hk(com(icmd),%val(com(icmd).addr))
	  goto 800

321	  call gs1_add(com(icmd),%val(com(icmd).addr))
	  goto 800

322	  call gs1_kill(com(icmd),%val(com(icmd).addr))
	  goto 800

323	  call gs1_select(com(icmd),%val(com(icmd).addr))
	  goto 800

324	  call gs1_store_event(com(icmd),%val(com(icmd).addr))
	  goto 800

325	  call gs1_recall_event(com(icmd),%val(com(icmd).addr))
	  goto 800

326	  call gs1_write_event(com(icmd),%val(com(icmd).addr))
	  goto 800

327	  call gs1_list_event(com(icmd),%val(com(icmd).addr))
	  goto 800

328	  call gs1_reorder(com(icmd),%val(com(icmd).addr))
	  goto 800

329	  call gs1_statistics(com(icmd),%val(com(icmd).addr))
	  goto 800

330	  call gs1_swap(com(icmd),%val(com(icmd).addr))
	  goto 800

331	  call gs1_mask(com(icmd),%val(com(icmd).addr))
	  goto 800

332	  call gs1_move(com(icmd),%val(com(icmd).addr))
	  goto 800

333	  call gs1_merge(com(icmd),%val(com(icmd).addr))
	  goto 800

334	  call gs1_split(com(icmd),%val(com(icmd).addr))
	  goto 800

335	  call gs1_newid(com(icmd),%val(com(icmd).addr))
	  goto 800

336	  call gs1_ebkill(com(icmd),%val(com(icmd).addr))
	  goto 800

337	  call gs1_combine(com(icmd),%val(com(icmd).addr))
	  goto 800

338	  call gs1_addback(com(icmd),%val(com(icmd).addr))
	  goto 800

339	  call gs1_copy(com(icmd),%val(com(icmd).addr))
	  goto 800

340	  call gs1_useful(com(icmd),%val(com(icmd).addr))
	  goto 800

341	  call gs1_thresholds(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
342	  goto 900
343	  goto 900

344	  call gs1_mult(com(icmd),%val(com(icmd).addr))
	  goto 800

345	  call gs1_mean_value(com(icmd),%val(com(icmd).addr))
	  goto 800

346	  call gs1_div(com(icmd),%val(com(icmd).addr))
	  goto 800

347	  call gs1_SLtoCM(com(icmd),%val(com(icmd).addr))
	  goto 800

348	  call gs1_recal_choose(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
349	  call gs1_wmean_value(com(icmd),%val(com(icmd).addr))
	  goto 800

350	  call gs1_recal_polar(com(icmd),%val(com(icmd).addr))
	  goto 800

351	  goto 900
352	  goto 900
353	  goto 900

354	  call gs1_angles_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

355	  call gs1_track_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

356	  call gs1_qvalue_pms(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
357	  call gs1_bpvelocity_pms(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
358	  call gs1_xfp_pms(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
359	  goto 900
360	  goto 900
361	  goto 900
362	  goto 900
363	  goto 900
364	  goto 900
365	  goto 900
366	  goto 900
367	  goto 900
368	  goto 900
369	  goto 900
370	  goto 900
371	  goto 900
372	  goto 900
373	  goto 900

374	  call gs1_break
	  goto 800
375	  call gs1_pairgate(com(icmd),%val(com(icmd).addr))
	  goto 800

376	  call gs1_pow(com(icmd),%val(com(icmd).addr))
	  goto 800

377	  call gs1_pairkill(com(icmd),%val(com(icmd).addr))
	  goto 800

378	  goto 900
379	  goto 900
380	  goto 900
381	  goto 900
382	  goto 900
383	  goto 900
384	  goto 900
385	  goto 900
386	  goto 900
387	  goto 900
388	  goto 900
389	  goto 900
390	  goto 900

391	  call usersub1(1)
	  goto 800

392	  call usersub2(1)
	  goto 800

393	  call usersub3(1)
	  goto 800

394	  call usersub4(1)
	  goto 800

395	  call usersub5(1)
	  goto 800

396	  call usersub6(1)
	  goto 800

397	  call usersub7(1)
	  goto 800

398	  call usersub8(1)
	  goto 800

399	  call usersub9(1)
	  goto 800

400	goto 900

401	call gs1_projections(com(icmd), %VAL(com(icmd).addr))
	goto 800

402	call gs1_sort1d(com(icmd), %VAL(com(icmd).addr))
	goto 800

403	call gs1_sort1d_m(com(icmd), %VAL(com(icmd).addr))
	goto 800

404	goto 900
405	goto 900
406	goto 900
407	goto 900
408	goto 900
409	goto 900
410	goto 900
	
411	call gs1_sort2d(com(icmd), %VAL(com(icmd).addr))
	goto 800

412	call gs1_sort3d(com(icmd), %VAL(com(icmd).addr))
	goto 800

413	call gs1_sort4d(com(icmd), %VAL(com(icmd).addr))
	goto 800

414	goto 900
415	goto 900

416	call gs1_sort2d_ab(com(icmd), %VAL(com(icmd).addr))
	goto 800

417	goto 900
418	goto 900
419	goto 900
420	goto 900	

421	call gs1_sort2d_symm(com(icmd), %VAL(com(icmd).addr))
	goto 800

422	call gs1_sort3d_symm(com(icmd), %VAL(com(icmd).addr))
	goto 800

423	call gs1_sort4d_symm(com(icmd), %VAL(com(icmd).addr))
	goto 800

424	goto 900

425	call gs1_sort3d_pair(com(icmd), %VAL(com(icmd).addr))
	goto 800

426	goto 900

427	call gs1_sort3d_diff(com(icmd), %VAL(com(icmd).addr))
	goto 800

428	goto 900
429	goto 900
430	goto 900

431	call gs1_sort2d_hsymm(com(icmd), %VAL(com(icmd).addr))
	goto 800

432	call gs1_sort3d_hsymm(com(icmd), %VAL(com(icmd).addr))
	goto 800

433	call gs1_sort4d_hsymm(com(icmd), %VAL(com(icmd).addr))
	goto 800

434	goto 900
435	goto 900
436	goto 900
437	goto 900
438	goto 900
439	goto 900
440	goto 900
441	goto 900
442	goto 900
443	goto 900
444	goto 900
445	goto 900
446	goto 900
447	goto 900
448	goto 900
449	goto 900
	  
800	  call gs_writeline(LUN,list,INDENT)

	ENDDO

	return

900	stop 'Unknown command'

	END

	subroutine INIT_RUN(MODE)

! chiamata all'inizio e prima di ogni RUN

# 1 "./gsort.inc" 1 
# 1 "./gsort.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./gsort.def" 2 
# 1 "./gsort.par" 1 
	INTEGER LU1		! LU per messaggi
	PARAMETER (LU1=6)
	INTEGER LU2		! LU per Logbook
	PARAMETER (LU2=7)
	INTEGER INDENT		! incolonnamento listato programma
	PARAMETER (INDENT=20)
*	INTEGER NPRINT		! intervallo per stampa status su LU1
*	PARAMETER (NPRINT=400)
	INTEGER NPRINT2		! intervallo per stampa status su LU2
	PARAMETER (NPRINT2=2000)
	INTEGER NSTOP		! intervallo per check di mat_stop
	PARAMETER (NSTOP=1000)
	INTEGER NSYNT		! # righe per la sintassi
	PARAMETER (NSYNT=5)

	INTEGER MAXRES		! Risoluzione massima gestita
	PARAMETER (MAXRES=32768)
	INTEGER MINRES		! Risoluzione minima gestita
	PARAMETER (MINRES=16)
	INTEGER MAXDET		! MAX # di rivelatori
	PARAMETER (MAXDET=256)
	INTEGER MAXPAR		! MAX # di parametri/rivelatore
	PARAMETER (MAXPAR=16)
	INTEGER NDTYPE		! Numero di tipi di rivelatore
	PARAMETER (NDTYPE=16)
	REAL PSTEP
	PARAMETER (PSTEP=359./1024.)

	INTEGER NOFORMAT	! No format defined
	PARAMETER (NOFORMAT=-1)
	INTEGER GASP		! Dati tipo GASP
	PARAMETER (GASP=1)
	INTEGER EURO		! Dati tipo Euroball
	PARAMETER (EURO=2)
	INTEGER PI8		! Dati tipo 8Pi-Berkeley
	PARAMETER (PI8=3)
	INTEGER GSPH		! Dati tipo GAMMASPHERE
	PARAMETER (GSPH=4)
	INTEGER GSPN		! Dati tipo GASP - nuova DAQ (2001)
	PARAMETER (GSPN=5)
	INTEGER YALE		! Dati tipo YrastBall - Yale Univ.
	PARAMETER (YALE=6)
	INTEGER TNDB
	PARAMETER (TNDB=7)	! Format Tandem-Bucharest
	INTEGER PRISMA
	PARAMETER (PRISMA=8)	! Format PRISMA (spettrometro)
	INTEGER GSR
	PARAMETER (GSR=9)       ! Temporary, GSPH reduced data
	INTEGER GANIL
	PARAMETER (GANIL=10)    ! GANIL data (EXOGAM,DIAMANT,NWALL)
	
	INTEGER CSEG,QSEG,TSEG,ISEG,SISEG, PPACSEG, DANTESEG
	PARAMETER (CSEG=7, QSEG=4, TSEG=1, ISEG=1, SISEG=4, PPACSEG=10, DANTESEG=8)
	INTEGER CDET,QDET,TDET,SIDET, PPACDET
	PARAMETER (CDET=15, QDET=26, TDET=30, SIDET=40, PPACDET=1)
	INTEGER COFF,QOFF,TOFF,FOFF
	PARAMETER (COFF=0, QOFF=COFF+CDET*CSEG, TOFF=QOFF+QDET*QSEG, FOFF=TOFF+TDET*TSEG)

	INTEGER MAXNBAN		! MAX # banane per BANANA e PIN
	PARAMETER (MAXNBAN=32)
	INTEGER MAXRCL_ORD	! MAX # coefficienti di ricalibrazione
	PARAMETER (MAXRCL_ORD=6)
	INTEGER MAXRCL_REGS	! MAX # regioni di ricalibrazione
	PARAMETER (MAXRCL_REGS=6)

	INTEGER MAXTADJ		! MAX # di parametri in time_adjust
	PARAMETER (MAXTADJ=16)

	INTEGER MAXHASH		! MAX # liste hashgates
	PARAMETER (MAXHASH=50)

	INTEGER MAXPAIRS	! MAX # liste ind=f(id,id) definibili
	PARAMETER (MAXPAIRS=4)
	
	INTEGER MAXMATDIM	! massima dimensione della matrice
	PARAMETER (MAXMATDIM=4)

	INTEGER DEFSTEP2D	! suddivisione default 2D
	PARAMETER (DEFSTEP2D=128)
	INTEGER DEFSTEP3D	! suddivisione default 3D
	PARAMETER (DEFSTEP3D=64)
	INTEGER DEFSTEP4D	! suddivisione default 4D
	PARAMETER (DEFSTEP4D=32)

	INTEGER DEFSTEP2DS	! suddivisione default 2D_symm
	PARAMETER (DEFSTEP2DS=128)
	INTEGER DEFSTEP3DS	! suddivisione default 3D_symm
	PARAMETER (DEFSTEP3DS=64)
	INTEGER DEFSTEP4DS	! suddivisione default 4D_symm
	PARAMETER (DEFSTEP4DS=32)

	INTEGER DEFSTEP2DH	! suddivisione default 2D_hsymm
	PARAMETER (DEFSTEP2DH=32)
	INTEGER DEFSTEP3DH	! suddivisione default 3D_hsymm
	PARAMETER (DEFSTEP3DH=8)
	INTEGER DEFSTEP4DH	! suddivisione default 4D_hsymm
	PARAMETER (DEFSTEP4DH=8)

	INTEGER MAXBYTES	! Max. lunghezza records su nastro
	PARAMETER (MAXBYTES=32*1024)
	INTEGER MAXWORDS	! Max. lunghezza records su nastro
	PARAMETER (MAXWORDS=MAXBYTES/2)
	INTEGER MAXEVL		! MAX lunghezza evento (words)
	PARAMETER (MAXEVL=2047)

	INTEGER MTFLAGR		! Flag per sync. nastro input
	PARAMETER (MTFLAGR=1)
	INTEGER MTFLAGW		! Flag per sync. nastro output
	PARAMETER (MTFLAGW=2)

	INTEGER NFORMCOM	! # comandi di Formato
	PARAMETER (NFORMCOM=9)
	INTEGER NDECLCOM	! # comandi di Dichiarazione
	PARAMETER (NDECLCOM=9)
	INTEGER NANALCOM	! # comandi di Analisi
	PARAMETER (NANALCOM=199)

	INTEGER MAXCOMANDI	! MAX # di COMANDI
	PARAMETER (MAXCOMANDI=360)
# 3 "./gsort.def" 2 

	structure/detector/
	  union
	    map
	      integer id
	    endmap
	    map
	      integer ival(-1:maxpar-1)
	    endmap
	  endunion
	  real    xval( 0:maxpar-1)
	endstructure

	structure/hashdat/
	  INTEGER IND,PAR,RES			! su quale parametro
	  INTEGER WHICH				! gate number
	  logical*1 SAMEFORALL			! same gate for all detectors
	  logical*1 FROMFILE			! dati da file
	  INTEGER NGATES(0:MAXDET-1)		! quanti gates ha letto
	  CHARACTER*72 FILE			! File dei gates
	  REAL    DIST2(0:1)			! Distanza**2 normalizzata per gate sferici
	endstructure
	
	structure/pairsdat/
	  INTEGER INDMAX
	  CHARACTER*72 FILE			! nomi dei file con i dati
	  INTEGER PIND(0:MAXDET-1,0:MAXDET-1)
	endstructure

	structure/recall/
	  logical*1 always
	endstructure

	structure/fold/
	  INTEGER IND		! Tipo di parametro
	  INTEGER MIN		! Finestra sul fold
	  INTEGER MAX		! Finestra sul fold
	endstructure

	structure/gate/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  logical*1 INSIDE	! dentro/fuori
	  INTEGER LO		! Estremo inferiore
	  INTEGER HI		! Estremo superiore
	  INTEGER NGATES	! Quanti intervalli
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	endstructure

	structure/gatesdat/
	  logical*1 BAD(0:1)
	endstructure

	structure/filter/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PARS		! parametro sorgente
	  INTEGER PARD		! parametro destinazione
	  INTEGER RES		! Risoluzione filtro
	  character*80 file     ! no comment
	  integer iform         !
	  real F(0:MAXRES)        !
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	  logical*1 initialized   !
	endstructure

	structure/window/
	  INTEGER  IND		! Tipo di parametro
	  INTEGER  LO(0:MAXPAR-1)! Valore inferiore
	  INTEGER  HI(0:MAXPAR-1)! Valore superiore
	  INTEGER  FL		! Minfold
	  INTEGER  FH		! Minfold
	  logical*1  SAME		! Same as before window
	endstructure

	structure/banana/
	  INTEGER IND(2)	! Tipo di parametro x,y
	  INTEGER PAR(2)	! Parametro asse x,y
	  INTEGER RES(2)	! Risoluzione della banana
	  INTEGER NBAN		! Numero di banane
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 INSIDE	! In-Out
	  logical*1 SAME		! Same as before banana
	  logical*1 multiadc      ! N.M. - for ADC-dependent banana
	  logical*1 ignore(0:MAXDET-1) ! only for ADC-dependent banana
	endstructure

	structure/bananadat/
	  character*72 file
	  integer ban(2,0:1)
	endstructure

	structure/bananasdat/
	  character*72 file(MAXNBAN)
	  integer ban(2,0:1)
	endstructure

	structure/pairgate/
	   integer IND
	   integer LO
	   integer HI
	   integer FL
	   integer FH
	   integer PAR
	   integer RES
	   integer*2 PVAL(0:MAXDET-1,0:MAXDET-1)
	   integer*2 LIST(0:MAXDET-1)
	   character*72 filename
	   logical*1 INSIDE
	   logical*1 SAME
	endstructure

	structure/pin/
	  INTEGER IND(4)	! Tipo di parametro per banane
	  INTEGER PAR(4)	! Parametro asse x,y ; la massa e il tipo della particella
	  INTEGER RES(2)	! Risoluzione delle banane
	  INTEGER FIX		! Parametro fisso per il risultato
	  INTEGER FIXRES        ! Risoluzione dell parametro fisso
	  INTEGER NBAN		! quante banane
	  INTEGER F1(MAXNBAN)	! Numero di particelle della banana
	  INTEGER F2(MAXNBAN)	! Peso della banana in PIN
	  INTEGER F3(MAXNBAN)	! Massa della particella
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before PIN
	  logical*1 multiadc(MAXNBAN)          ! N.M. - for ADC-dependent banana
	  logical*1 ignore(MAXNBAN,0:MAXDET-1) ! only for ADC-dependent banana
	endstructure

	structure/hk/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER FIXH		! Parametro fisso per H
	  INTEGER FIXk		! Parametro fisso per k
	  REAL	  OFFS		! Offset per H
	  REAL	  GAIN		! Guadagno finale per H
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	endstructure

	structure/recal/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 RUN		! coefficenti run-dependent
	  logical*1 SAME	! Same as before recal
	  logical*1 ROUND       ! Round the result to integer value
	  logical*1 ZERO        ! Set negative result to zero
	  CHARACTER*73 FILE	! files dei coefficenti
	endstructure

	structure/recal_choose/
	  INTEGER IND(3)	! Tipo di parametro
	  INTEGER PAR(3)	! Quale parametro
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL    VAL           ! Valore di riferimento per fare la scelta
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  CHARACTER*73 FILE1	! files dei coefficenti
	  CHARACTER*73 FILE2	! files dei coefficenti
	  logical*1 RUN		! coefficenti run-dependent
	  logical*1 SAME	! Same as before recal
	  logical*1 ROUND       ! Round the result to integer value
	endstructure

	structure/calcoef/
	  INTEGER*4 ORD
	  REAL*4    COEF(MAXRCL_ORD)
	endstructure

	structure/mcalcoef/
	  integer nregs
	  INTEGER ORD(MAXRCL_REGS)
	  REAL    COEF(MAXRCL_ORD,MAXRCL_REGS)
	  real    limit(0:MAXRCL_REGS)
	endstructure


	structure/doppler/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET		! Quanti rivelatori
         real*8  fact1
         real*8  fact2
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL	  VC0		! Velocita' di rinculo
	  real    rtheta
	  real    rphi
	  REAL	  E0		! VC0 fino a E0
	  REAL	  E1		! Da VC0 a VC1 tra E0 e E1
	  REAL	  VC1		! VC1 sopra E1
	  REAL	  SLOPE		! Slope tra E0 e E1
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
         logical*1 RefChanged
	  logical*1 CONST		! Costante
	  logical*1 SAME		! Same as before recal
	  CHARACTER*73 FILE	! file con gli angoli
	endstructure

	structure/dopplerdat/
	  real costheta(0:1)
	endstructure
	
	structure/polar/
         real*8  fact1
         real*8  fact2
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET		! Quanti rivelatori
          INTEGER DTHETA        ! Angolo THETA del rivelatore, se viene dato come parametro
          INTEGER DPHI          ! Angolo PHI del rivelatore, se viene dato come parametro
	  INTEGER IND_POLAR
	  INTEGER PTHETA
	  INTEGER PPHI
         INTEGER IND_VEL
         INTEGER PVEL
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL	  VC0		! Velocita' di rinculo
	  REAL	  E0		! VC0 fino a E0
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
          logical*1 FIXED
	  logical*1 VarDetAngle	! Se angoli degli rivelatori sono parametri
	  logical*1 SAME		! Same as before recal
	  CHARACTER*73 FILE	! file con gli angoli
	endstructure
	
	structure/polardat/
	  real cdir(3,0:MAXDET-1)
	endstructure

	structure/meanvalstr/
	  INTEGER IND1(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR1(MAXTADJ)	! Quale parametro
	  INTEGER RES1(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY1	! Quanti definiti
	  REAL    POSITION	! Posizione finale
	  REAL	  FACTOR	! Rejection factor
	  INTEGER IND2	! FIX only
	  INTEGER PAR2	! Quale parametro fisso
	  INTEGER RES2	! Risoluzione
	endstructure

	structure/tadjust/
	  INTEGER IND1(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR1(MAXTADJ)	! Quale parametro
	  INTEGER RES1(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY1	! Quanti definiti
	  REAL    POSITION	! Posizione finale
	  REAL	  FACTOR	! Rejection factor
	  INTEGER IND2(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR2(MAXTADJ)	! Quale parametro
	  INTEGER RES2(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY2	! Quanti definiti
	endstructure

	structure/tref/
	   integer*8 N
# 285

	   real*16 SQSUM

	   INTEGER IND
	   INTEGER PAR
	   INTEGER RES
	   INTEGER REFNO
	   REAL POSITION
	endstructure

	structure/kine/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET_GE	! quanti germani
	  INTEGER NDET_SI	! quanti silici
	  INTEGER BIND(2)	! Tipo di parametro per banane
	  INTEGER BPAR(2)	! Parametro asse x,y
	  INTEGER BRES(2)	! Risoluzione della banana
	  INTEGER NBAN		! Numero di banane
	  INTEGER PINFIX	! Fixpar per PIN (se >=0)
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before recal
	  CHARACTER*72 KINEFILE	! file di descrizione
	  CHARACTER*72 AFILE_GE	! angoli germanio
	  CHARACTER*72 AFILE_SI	! angoli silici
	endstructure

	structure/kinedat/
	  REAL    ACMP,ECMP		! il nucleo composto
	  REAL    XMOMCM		! suo momento
	  REAL    AD(MAXNBAN)		! massa della particella
	  INTEGER MDET(MAXNBAN)		! numero di rivelatori attivi
	  INTEGER F1(MAXNBAN)		! Numero di particelle della banana per PIN
	  INTEGER F2(MAXNBAN)		! Peso della banana per PIN
	  real mom_si(0:MAXDET-1,MAXNBAN)
	  real cdir_ge(3,0:MAXDET-1)
	  real cdir_si(3,0:MAXDET-1)
	  record/bananasdat/kban
	endstructure

	structure/kinenew/
	  INTEGER IND(3)		! Tipo di parametro
	  INTEGER PAR(3)		! Parametro
	  INTEGER RES(3)		! Risoluzione del parametro
	  INTEGER NDET_GE	! quanti germani
	  INTEGER NDET_SI	! quanti silici
	  real ecal
	  real acmp
	  real ecmp
	  real atenuation
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before recal
	  CHARACTER*72 AFILE_GE	! angoli germanio
	  CHARACTER*72 AFILE_SI	! angoli silici
	  character*73 except_file
	  logical*1 except
	endstructure

	structure/kinenewdat/
	  REAL    ACMP,ECMP		! il nucleo composto
	  REAL    XMOMCM		! suo momento
	  real    ecal
	  real    afac
	  real cdir_ge(3,0:MAXDET-1)
	  real cdir_si(3,0:MAXDET-1)
	  integer nregs
	  integer reg(MAXRES,2)
	endstructure

	structure/sltocm/
	  integer IND(2)
	  integer PAR(2)
	  integer NDET_SI
	  real F(2)
	  real COS_TH(0:MAXDET-1)
	  CHARACTER*72 AFILE_SI	! angoli silici
	endstructure


	structure/add/
	  INTEGER IND(3)	! Tipo di parametro
	  INTEGER PAR(3)	! Parametro 1+2==>3
	  REAL	  FAC(3)	! fattori moltiplicativi
	  REAL    OFFSET	! offset da sommare al risultato
	  REAL    GAIN		! gain sul risultato (non sul'offset)
	  INTEGER ICHAN		! canale limite per COMBINE
	  INTEGER DELTA		! Sliding range
	  INTEGER SLIDE		! Sliding value
	  logical*1 CHECK		! check che il secondo parametro sia > limit
	  logical*1 MULT		! fattori moltiplicativi?
	endstructure

	structure/kill/
	  INTEGER IND			! Tipo di parametro
	  logical*1 RUN			! run dependent selective kill
	  CHARACTER*72 FILE		! files contenente i detbad
	  logical*1 DET(0:MAXDET-1)	! Quali rivelatori
	endstructure

	structure/killdat/
	  logical*1 remove(0:MAXDET-1,0:MAXDET-1)
	endstructure

	structure/select/
	  INTEGER IND		! Tipo di parametro
	  logical*1 DET(0:MAXDET-1)  ! Quali rivelatori
	endstructure

	structure/listev/
	  logical*1 TOFILE	! list events on file
	  logical*1 ISOPEN
	  INTEGER LUN
	  CHARACTER*72 FILE
	endstructure

	structure/reorder/
	  INTEGER IND		! Tipo di parametro
	endstructure

	structure/statistics/
	  INTEGER WHICH
	endstructure

	structure/statdat/
	  INTEGER FOLD(0:MAXDET,0:NDTYPE,0:1)
	endstructure

	structure/swap/
	  INTEGER IND(2)	! Tipo di parametro x,y
	  INTEGER PAR(2)	! Parametro asse x,y
	endstructure

	structure/mask/
	  INTEGER IND		! Tipo di parametro x
	  INTEGER PAR		! Parametro asse x
	  INTEGER MASK		! Maschera
	  logical*1 ONE		! solo un parametro o tutti?
	endstructure

	structure/useful/
	  INTEGER IND		! Tipo di parametro (deve essere 0)
	  INTEGER PAR1		! Parametro per RUN#
	  INTEGER PAR2		! Parametro per REC#
	  INTEGER PAR3		! Parametro per EVCOUNT#
	  INTEGER PAR4		! Parametro per EVNUMBER#
	endstructure

	structure/move/
	  INTEGER IND1			! da quale tipo
	  INTEGER IND2			! a  quale tipo
	  INTEGER PAR1			! da quale parametro
	  INTEGER PAR2			! a  quale parametro
	  INTEGER OFFSET                ! offset alla nuova numerazione
	  logical*1 CONDITION		! TRUE if GATE or copy detector
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
c	  logical*1 INSIDE	! dentro/fuori
	  INTEGER LO		! Estremo inferiore
	  INTEGER HI		! Estremo superiore
c	  INTEGER NGATES	! Quanti intervalli
c	  INTEGER FL		! Minfold
c	  INTEGER FH		! Maxfold
c	  logical*1 SAME		! Same as before gate
	  INTEGER HOWMANY		! Howmany to move
	  INTEGER WHICH(0:MAXDET-1)	! Quali rivelatori
	endstructure

	structure/splitmerge/
	  INTEGER NIND			! quanti tipi di rivelatore coinvolti
	  INTEGER IND(0:NDTYPE)		! Quali rivelatori
	  logical*1 REMOVE(0:NDTYPE)	!
	  INTEGER OFFSET(0:NDTYPE)	! Offset degli indici
	endstructure

	structure/newid/
	  INTEGER IND			! tipo di rivelatore
	  INTEGER LUT(0:MAXDET-1)	! mappa dei nuovi indici
	  logical*1 REORDER		! reordina dopo la mappatura
	  CHARACTER*72 FILE		! files dei nuovi id
	endstructure

	structure/addback/
	  INTEGER IND			! Tipo di parametro
	  INTEGER PAR			! Quale parametro
	  INTEGER RES			! Risoluzione del parametro
	  INTEGER NSEGS			! quanti segmenti (se composito)
	  logical*1 REJECT		! elimina le doppie non contigue
	  logical*1 PATTERN		! produce la hit-pattern sommando tutto
	  INTEGER PATPAR		! dove la registra
	  logical*1 TGATE			! verifica la relazione temporale?
	  INTEGER TPAR			! su quale parametro
	  INTEGER TVAL			! massima deviazione
	endstructure

        structure /pms_str/
	    real*8  sin_70
	    real*8  sin_110
	    real*8  cos_20
	    real*8  tan_20
	    integer ind
	    integer par_r
	    integer par_p
	    integer par_e
	    integer par_icp
	    integer ind_theta
	    integer par_theta
	    integer ind_path
	    integer par_path
	    integer ind_x
	    integer par_x
	    integer ind_ic
	    integer par_a
	    integer par_b
	    integer par_c
	    integer par_d
	    real    thr
	    real    qfact
	end structure
	
	structure /pms_dat/
	   real*8 R(40,0:100)
	end structure
	 
	structure /qvalue_str/
	   real*8  costhp
	   real*8  sinthp
	   real*8  amu
	   integer ind_q
	   integer par_q
	   integer ind_mass
	   integer par_mass
	   integer ind_theta
	   integer par_theta
	   integer ind_phi
	   integer par_phi
	   integer ind_beta
	   integer par_beta
	   integer AP
	   integer AT
	   integer ATOT
	   integer    low
	   integer    high
	   real    ep
	   real    thp
	   real    gain
	   real    offset
	end structure

       structure /prisma_angles_str/
          real*8  prisma_angle
          real*8  D
          real*8  sinalpha
          real*8  cosalpha
          real*8  costhp
          real*8  sinthp
	  real*8  cx(0:3)
	  real*8  cy(0:3)
          integer ind
          integer parx
          integer pary
          integer parz
          integer parq
          integer part
	  integer parp
	  integer pard
	  integer degx
	  integer degy
	  integer oldstyle
	  character*128 calfile
       end structure
	   
	structure/proje/
	  INTEGER LEN		! Lunghezza totale degli spettri (LW)
	  INTEGER MFRES 	! risoluzione max. di F
	  INTEGER*8 NINCR
	endstructure

	structure/projedat/
	  CHARACTER*16 NAME(0:MAXPAR-1,0:NDTYPE)
	  INTEGER      ADDR(0:MAXDET-1,0:MAXPAR-1,0:NDTYPE)
	  INTEGER      SPEC(0:1)
	endstructure

	structure/spectrum/
	  INTEGER IND			! Tipo di parametro
	  INTEGER PAR			! Quale parametri
	  INTEGER RES			! numero di canali dello spettro
	  INTEGER IHASH			! Quale tabella di hash (se >=0)
	  INTEGER HIND,HPAR		! riportati qui per comodita'
	  logical*1 HSAME			! IND e' quello su cui applicare HGATE
	  INTEGER NHASH			! Quante volte
	  logical*1 SPHERICAL		! gate di tipo elissoidale
	  INTEGER MINFOLD		! fold minimo (per hsort1d)
	  INTEGER*8 NINCR		! numero di incrementi
	  CHARACTER*72 FILE		! Filename dello spettro
	endstructure

	structure/specdat/
	  INTEGER SPEC(0:1)
	endstructure

	structure/matrix/

	  integer*8 CMTBASE

	  INTEGER NDIM			! Ordine della matrice
	  INTEGER IND(MAXMATDIM)	! Tipo di parametri per i due lati
	  INTEGER PAR(MAXMATDIM)	! Quale dei parametri
	  INTEGER RES(MAXMATDIM)	! Dimensioni lati della matrice
	  INTEGER STEP(MAXMATDIM)	! suddivisione lati della matrice
	  INTEGER MATMODE		! 0=normal 1=symmetric 2=halfsymmetric
	  INTEGER MSDIM			! numero di indici simmetrizzati
	  INTEGER DOFFSET		! per SORTxD_DIFF
	  INTEGER NIND
	  INTEGER IHASH			! Quale tabella di hash (se >=0)
	  INTEGER HIND,HPAR		! riportati qui per comodita'
	  logical*1 HSAME			! IND e' quello su cui applicare HGATE
	  INTEGER NHASH			! quante volte
	  logical*1 SPHERICAL		! gate di tipo elissoidale
	  INTEGER TYPE
	  INTEGER IND1,IND2,IND3,IND4
	  INTEGER PAR1,PAR2,PAR3,PAR4
	  logical*1 HHH1,HHH2,HHH3,HHH4
	  CHARACTER*72 FILE		! Filename matrice
	endstructure

	structure/iodef/
	  logical*1 datafile	! TRUE se da file o Virtuale
	  logical*1 virtuale
	  logical*1 closed
	  integer lun
	  integer flag
	  integer wlen
	  integer run
	  integer rec
	  integer maxrec
	  integer evcount	! internal count
	  integer evnumber	! from ACQ
	  integer vrun,vevents
CVMS	  integer*2 iosb(4)
	  integer iosb(4)
CVMS	  integer*2 mtch,hctm
	  integer mtch
	  character*72 name
	endstructure

	structure/oevdef/
	  logical*1 defined
	  logical*1 same				! Same FILE structure on output
	  logical*1 sameRUN			! Same RUN number on output
	  logical*1 ask				! Will ask every EOF
	  logical*1 reduce			! Output events in forma ridotta
	  logical*1 det(0:NDTYPE)			! Output this detector
	  logical*1 par(0:MAXPAR-1,0:NDTYPE)  	! Output this parameter
	  integer evlen,pointer
	  logical*1 done
	endstructure

	structure/commands/
	  union
	    map
	      record/recall/recall
	    endmap
	    map
	      record/fold/fold
	    endmap
	    map
	      record/gate/gate
	    endmap
	    map
	      record/filter/filter
	    endmap
	    map
	      record/window/win
	    endmap
	    map
	      record/pairgate/pgate
	    endmap
	    map
	      record/banana/ban
	    endmap
	    map
	      record/pin/pin
	    endmap
	    map
	      record/recal/rcl
	    endmap
	    map
	      record/kine/kine
	    endmap
	    map
	      record/doppler/doppl
	    endmap
	    map
	      record/tadjust/tadj
	    endmap
	    map
	      record/tref/tref
	    endmap
	    map
	      record/hk/hk
	    endmap
	    map
	      record/add/add
	    endmap
	    map
	      record/kill/kill
	    endmap
	    map
	      record/select/sel
	    endmap
	    map
	      record/listev/lev
	    endmap
	    map
	      record/reorder/reo
	    endmap
	    map
	      record/statistics/stat
	    endmap
	    map
	      record/swap/swap
	    endmap
	    map
	      record/mask/mask
	    endmap
	    map
	      record/useful/useful
	    endmap
	    map
	      record/move/move
	    endmap
	    map
	      record/newid/newid
	    endmap
	    map
	      record/splitmerge/splmrg
	    endmap
	    map
	      record/polar/dpolar
	    endmap
	    map
	      record/pms_str/pms
	    endmap
	    map
	      record/qvalue_str/qvalue
	    endmap
	    map
	      record /prisma_angles_str/ang
	    endmap
	    map
	      record/addback/abck
	    endmap
	    map
	      record/proje/pro
	    endmap
	    map
	      record/spectrum/spec
	    endmap
	    map
	      record/matrix/mat
	    endmap
	  endunion

	  integer*8 addr

	  integer size
	  integer whichcom
	endstructure

	structure /tndio/
	  integer nfiles
	  integer current
	  logical*1 presorted
	end structure
# 2 "./gsort.inc" 2 

	INTEGER	  LLUN
	INTEGER	  LLIST
	INTEGER   GS2LUN
	INTEGER   LGSDIR
	CHARACTER GSDIR*64
	CHARACTER LIST*256
	CHARACTER LINE*256
	CHARACTER COMSORT*40
	integer INFILES
	COMMON /VARIECOM/ LLUN,LLIST,GS2LUN,LGSDIR,GSDIR,LIST,LINE,COMSORT,INFILES

	logical*1 BREAK
	logical*1 KILLEV
	logical*1 AUTONUMBER	! File etichettati con RUN#
	logical*1 TAPEDISMOUNT	! Controllo fine nastro
	logical*1 LASTTAPE	! Controllo fine nastro
	logical*1 AUTOLOADER
	logical*1 STORED_EV
	logical*1 DEFINED_OUT,FINISHED_OUT
	COMMON/FLAGS/ BREAK,KILLEV,AUTONUMBER,TAPEDISMOUNT,LASTTAPE,AUTOLOADER,STORED_EV,
     1 DEFINED_OUT,FINISHED_OUT

	logical*1 SYNTAX
	INTEGER PHASE
	CHARACTER COMLINE*256
	INTEGER LCOMLINE
	INTEGER INPLU
	CHARACTER*128  FORM(NFORMCOM)
	CHARACTER*128  DECL(NDECLCOM)
	CHARACTER*128  ANAL(NANALCOM)
	CHARACTER*128 SYNT(NSYNT)
	COMMON /COMDEFS/ PHASE,COMLINE,LCOMLINE,INPLU,FORM,DECL,ANAL,SYNT,SYNTAX

	RECORD /COMMANDS/COM(MAXCOMANDI)
	INTEGER	NCOMANDI
	INTEGER ICMD
	INTEGER WHICHCOM
	INTEGER INIT_MODE,FINIT_MODE
	COMMON /COMMAND/ COM,NCOMANDI,ICMD,WHICHCOM,INIT_MODE,FINIT_MODE

	INTEGER   DATAFORMAT			! GASP/EUROBALL/8PI
	INTEGER   GASPMAP(0:NDTYPE)		! Mappatura per formato GASP
	INTEGER   EUROMAP(0:NDTYPE)		! Mappatura per formato euroball
	INTEGER   ISEED				! Seed per i numeri random
	INTEGER   NTIPI				! quanti tipi di rivelatori
	INTEGER   NDPAR(0:NDTYPE)		! # parametri
	INTEGER   MDPAR(0:NDTYPE)		! # parametri aggiunti
	INTEGER   TDPAR(0:NDTYPE)		! # parametri totali
	INTEGER   PARRES(0:MAXPAR-1,0:NDTYPE)  	! Risoluzione dei vari parametri

	INTEGER   NDETS(0:NDTYPE)		! # di rivelatori (se composito)
	INTEGER   NSEGS(0:NDTYPE)		! # segmenti (se composito)
	INTEGER   NCPAR(0:NDTYPE)		! # di parametri comuni (se composito)
	
	INTEGER   NITEMS(0:NDTYPE)		! # totale di rivelatori
	logical*1   EXISTS(0:NDTYPE)		! Esiste il tipo di rivelatore
	INTEGER   FOLDMIN(0:NDTYPE)		! Minimo fold da nastro
	CHARACTER*1 DNAME(0:NDTYPE)		! simboli per rivelatori
	COMMON /EVDEF/DATAFORMAT,GASPMAP,EUROMAP,ISEED,NTIPI,NDPAR,MDPAR,
     1TDPAR,PARRES,NDETS,NSEGS,NCPAR,NITEMS,FOLDMIN,EXISTS,DNAME

	INTEGER   CLASS,TAG			! Descrizione dell'evento
	INTEGER   NDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   DOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/DET(0:MAXDET-1)		! i dati
	COMMON /CEVENT/  CLASS,TAG,NDET,DOFF,DET

	INTEGER   sCLASS,sTAG			! Copia dell'evento per SAVE/RECALL
	INTEGER   sNDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   sDOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/sDET(0:MAXDET-1)	! i dati
	COMMON /SEVENT/  sCLASS,sTAG,sNDET,sDOFF,sDET

	INTEGER   mCLASS,mTAG			! Copia di lavoro (Move...)
	INTEGER   mNDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   mDOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/mDET(0:MAXDET-1)	! i dati
	COMMON /mEVENT/  mCLASS,mTAG,mNDET,mDOFF,mDET


	integer*8 HASHADDR(0:MAXHASH-1)

	INTEGER HASHGIVEN
	INTEGER HNTRUE,HSTRUE			! Quanti rivelatori in gate
	logical*1 HLTRUE(0:MAXDET-1)		! Quali rivelatori in gate
	REAL    HDIST1(0:MAXDET-1)		! per gestione gates sferici
	REAL    HDIST2(0:MAXDET-1)		! per gestione gates sferici
	COMMON /HASHDEF/ HASHADDR,HASHGIVEN,HNTRUE,HSTRUE,HLTRUE,HDIST1,HDIST2


	integer*8 PAIRSADDR(0:MAXPAIRS-1)

	INTEGER PAIRSGIVEN
	COMMON /PAIRSDEF/ PAIRSADDR,PAIRSGIVEN


	integer*8 STATADDR

	INTEGER STATSIZE
	INTEGER NSTATCOM			! quanti comandi di statistica
	COMMON /STATISTICS/ STATADDR,STATSIZE,NSTATCOM

	INTEGER NLOST,NCLASS
	INTEGER*8 NINCR(MAXMATDIM)
	INTEGER FLUSHED
	INTEGER NPRINT
	COMMON /STATUS/ NLOST,NCLASS,NINCR,FLUSHED,NPRINT

	integer*2 ievbuf(16*MAXWORDS)	! Buffer dati
	integer*2 ievheader(MAXWORDS)	! header record
	record/iodef/iio
	common/ievcom/ievheader,ievbuf,iio

	record/oevdef/oev
	integer*2 oevent(MAXEVL*4)	! evento da scrivere
	integer*2 oevbuf(MAXBYTES/2)	! Buffer scrittura eventi
	record/iodef/oio
	common/oevcom/oev,oevent,oevbuf,oio

	record /tndio/tnd
	common /tndcom/tnd
# 1564 "gsort.F" 2 

	if(mode.eq.0) then		! the very beginning
	  init_mode=0
	  call gs_init_statistics
	elseif(mode.eq.1) then		! beginning a new run
	  init_mode=1
	else
	  return
	endif

	call lvect_erase(%val(stataddr),statsize)

	do ii=0,hashgiven-1
	  call gs2_hgatedef(%val(hashaddr(ii)))
	enddo
	
	do ii=0,pairsgiven-1
	  call gs2_pairdef(ii,%val(pairsaddr(ii)))
	enddo

	do icmd=1,ncomandi
	  whichcom=com(icmd).whichcom

	goto(	    301,302,303,304,305,306,307,308,309,
     1	310,311,312,313,314,315,316,317,318,319,
     2	320,321,322,323,324,325,326,327,328,329,
     3	330,331,332,333,334,335,336,337,338,339,
     4	340,341,342,343,344,345,346,347,348,349,
     5	350,351,352,353,354,355,356,357,358,359,
     6	360,361,362,363,364,365,366,367,368,369,
     7	370,371,372,373,374,375,376,377,378,379,
     8	380,381,382,383,384,385,386,387,388,389,
     9	390,391,392,393,394,395,396,397,398,399,
     1	400,401,402,403,404,405,406,407,408,409,
     1	410,411,412,413,414,415,416,417,418,419,
     1	420,421,422,423,424,425,426,427,428,429,
     1	430,431,432,433,434,435,436,437,438,439,
     1	440,441,442,443,444,445,446,447,448,449) whichcom

	  call ansi_bell(6)
	  stop 'Inconsistent command in INIT_RUN'

301	  call gs2_fold(com(icmd),%val(com(icmd).addr))
	  goto 800

302	  call gs2_gate(com(icmd),%val(com(icmd).addr))
	  goto 800

303	  call gs2_gates(com(icmd),%val(com(icmd).addr))
	  goto 800

304	  call gs2_window(com(icmd),%val(com(icmd).addr))
	  goto 800

305	  call gs2_banana(com(icmd),%val(com(icmd).addr))
	  goto 800

306	  call gs2_bananas(com(icmd),%val(com(icmd).addr))
	  goto 800

307	  call gs2_filter(com(icmd),%val(com(icmd).addr))
	  goto 800

308	  call gs2_xban(com(icmd),%val(com(icmd).addr))
	  goto 800

309	  goto 900
310	  call gs2_recal_mult(com(icmd),%val(com(icmd).addr))
	  goto 800

311	  call gs2_gain(com(icmd),%val(com(icmd).addr))
	  goto 800

312	  call gs2_recal(com(icmd),%val(com(icmd).addr))
	  goto 800

313	  call gs2_recal_lut(com(icmd),%val(com(icmd).addr))
	  goto 800

314	  call gs2_time_reference(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
315	  call gs2_timing(com(icmd),%val(com(icmd).addr))
	  goto 800

316	  call gs2_recal_doppler(com(icmd),%val(com(icmd).addr))
	  goto 800

317	  call gs2_recal_kinenew(com(icmd),%val(com(icmd).addr))
	  goto 800

318	  call gs2_time_adjust(com(icmd),%val(com(icmd).addr))
	  goto 800

319	  call gs2_pin(com(icmd),%val(com(icmd).addr))
	  goto 800

320	  call gs2_hk(com(icmd),%val(com(icmd).addr))
	  goto 800

321	  call gs2_add(com(icmd),%val(com(icmd).addr))
	  goto 800

322	  call gs2_kill(com(icmd),%val(com(icmd).addr))
	  goto 800

323	  call gs2_select(com(icmd),%val(com(icmd).addr))
	  goto 800

324	  call gs2_store_event(com(icmd),%val(com(icmd).addr))
	  goto 800

325	  call gs2_recall_event(com(icmd),%val(com(icmd).addr))
	  goto 800

326	  call gs2_write_event(com(icmd),%val(com(icmd).addr))
	  goto 800

327	  call gs2_list_event(com(icmd),%val(com(icmd).addr))
	  goto 800

328	  call gs2_reorder(com(icmd),%val(com(icmd).addr))
	  goto 800

329	  call gs2_statistics(com(icmd),%val(com(icmd).addr))
	  goto 800

330	  call gs2_swap(com(icmd),%val(com(icmd).addr))
	  goto 800

331	  call gs2_mask(com(icmd),%val(com(icmd).addr))
	  goto 800

332	  call gs2_move(com(icmd),%val(com(icmd).addr))
	  goto 800

333	  call gs2_merge(com(icmd),%val(com(icmd).addr))
	  goto 800

334	  call gs2_split(com(icmd),%val(com(icmd).addr))
	  goto 800

335	  call gs2_newid(com(icmd),%val(com(icmd).addr))
	  goto 800

336	  call gs2_ebkill(com(icmd),%val(com(icmd).addr))
	  goto 800

337	  call gs2_combine(com(icmd),%val(com(icmd).addr))
	  goto 800

338	  call gs2_addback(com(icmd),%val(com(icmd).addr))
	  goto 800

339	  call gs2_copy(com(icmd),%val(com(icmd).addr))
	  goto 800

340	  call gs2_useful(com(icmd),%val(com(icmd).addr))
	  goto 800

341	  call gs2_thresholds(com(icmd),%val(com(icmd).addr))
	  goto 800

342	  goto 900
343	  goto 900

344	  call gs2_mult(com(icmd),%val(com(icmd).addr))
	  goto 800

345	  call gs2_mean_value(com(icmd),%val(com(icmd).addr))
	  goto 800

346	  call gs2_div(com(icmd),%val(com(icmd).addr))
	  goto 800

347	  call gs2_SLtoCM(com(icmd),%val(com(icmd).addr))
	  goto 800

348	  call gs2_recal_choose(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
349	  call gs2_wmean_value(com(icmd),%val(com(icmd).addr))
	  goto 800

350	  call gs2_recal_polar(com(icmd),%val(com(icmd).addr))
	  goto 800

351	  goto 900
352	  goto 900
353	  goto 900

354	  call gs2_angles_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

355	  call gs2_track_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

356	  call gs2_qvalue_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

357	  call gs2_bpvelocity_pms(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
358	  call gs2_xfp_pms(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
359	  goto 900
360	  goto 900
361	  goto 900
362	  goto 900
363	  goto 900
364	  goto 900
365	  goto 900
366	  goto 900
367	  goto 900
368	  goto 900
369	  goto 900
370	  goto 900
371	  goto 900
372	  goto 900
373	  goto 900

374	  call gs2_break
	  goto 800
375	  call gs2_pairgate(com(icmd),%val(com(icmd).addr))
	  goto 800

376	  call gs2_pow(com(icmd),%val(com(icmd).addr))
	  goto 800

377	  call gs2_pairkill(com(icmd),%val(com(icmd).addr))
	  goto 800

378	  goto 900
379	  goto 900
380	  goto 900
381	  goto 900
382	  goto 900
383	  goto 900
384	  goto 900
385	  goto 900
386	  goto 900
387	  goto 900
388	  goto 900
389	  goto 900
390	  goto 900

391	  call usersub1(2)
	  goto 800

392	  call usersub2(2)
	  goto 800

393	  call usersub3(2)
	  goto 800

394	  call usersub4(2)
	  goto 800

395	  call usersub5(2)
	  goto 800

396	  call usersub6(2)
	  goto 800

397	  call usersub7(2)
	  goto 800

398	  call usersub8(2)
	  goto 800

399	  call usersub9(2)
	  goto 800

400	goto 900

401	call gs2_projections(com(icmd), %VAL(com(icmd).addr))
	goto 800

402	call gs2_sort1d(com(icmd), %VAL(com(icmd).addr))
	goto 800

403	call gs2_sort1d_m(com(icmd), %VAL(com(icmd).addr))
	goto 800

404	goto 900
405	goto 900
406	goto 900
407	goto 900
408	goto 900
409	goto 900
410	goto 900
	
411	call gs2_sort2d(com(icmd), %VAL(com(icmd).addr))
	goto 800

412	call gs2_sort3d(com(icmd), %VAL(com(icmd).addr))
	goto 800

413	call gs2_sort4d(com(icmd), %VAL(com(icmd).addr))
	goto 800

414	goto 900
415	goto 900

416	call gs2_sort2d_ab(com(icmd), %VAL(com(icmd).addr))
	goto 800

417	goto 900
418	goto 900
419	goto 900
420	goto 900	

421	call gs2_sort2d_symm(com(icmd), %VAL(com(icmd).addr))
	goto 800

422	call gs2_sort3d_symm(com(icmd), %VAL(com(icmd).addr))
	goto 800

423	call gs2_sort4d_symm(com(icmd), %VAL(com(icmd).addr))
	goto 800

424	goto 900

425	call gs2_sort3d_pair(com(icmd), %VAL(com(icmd).addr))
	goto 800

426	goto 900

427	call gs2_sort3d_diff(com(icmd), %VAL(com(icmd).addr))
	goto 800

428	goto 900
429	goto 900
430	goto 900

431	call gs2_sort2d_hsymm(com(icmd), %VAL(com(icmd).addr))
	goto 800

432	call gs2_sort3d_hsymm(com(icmd), %VAL(com(icmd).addr))
	goto 800

433	call gs2_sort4d_hsymm(com(icmd), %VAL(com(icmd).addr))
	goto 800

434	goto 900
435	goto 900
436	goto 900
437	goto 900
438	goto 900
439	goto 900
440	goto 900
441	goto 900
442	goto 900
443	goto 900
444	goto 900
445	goto 900
446	goto 900
447	goto 900
448	goto 900
449	goto 900

800	ENDDO

	return

900	stop 'Unknown command'

	END

	subroutine EVANA

! analisi dell'evento secondo quanto definito nel file di comando

# 1 "./gsort.inc" 1 
# 1 "./gsort.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./gsort.def" 2 
# 1 "./gsort.par" 1 
	INTEGER LU1		! LU per messaggi
	PARAMETER (LU1=6)
	INTEGER LU2		! LU per Logbook
	PARAMETER (LU2=7)
	INTEGER INDENT		! incolonnamento listato programma
	PARAMETER (INDENT=20)
*	INTEGER NPRINT		! intervallo per stampa status su LU1
*	PARAMETER (NPRINT=400)
	INTEGER NPRINT2		! intervallo per stampa status su LU2
	PARAMETER (NPRINT2=2000)
	INTEGER NSTOP		! intervallo per check di mat_stop
	PARAMETER (NSTOP=1000)
	INTEGER NSYNT		! # righe per la sintassi
	PARAMETER (NSYNT=5)

	INTEGER MAXRES		! Risoluzione massima gestita
	PARAMETER (MAXRES=32768)
	INTEGER MINRES		! Risoluzione minima gestita
	PARAMETER (MINRES=16)
	INTEGER MAXDET		! MAX # di rivelatori
	PARAMETER (MAXDET=256)
	INTEGER MAXPAR		! MAX # di parametri/rivelatore
	PARAMETER (MAXPAR=16)
	INTEGER NDTYPE		! Numero di tipi di rivelatore
	PARAMETER (NDTYPE=16)
	REAL PSTEP
	PARAMETER (PSTEP=359./1024.)

	INTEGER NOFORMAT	! No format defined
	PARAMETER (NOFORMAT=-1)
	INTEGER GASP		! Dati tipo GASP
	PARAMETER (GASP=1)
	INTEGER EURO		! Dati tipo Euroball
	PARAMETER (EURO=2)
	INTEGER PI8		! Dati tipo 8Pi-Berkeley
	PARAMETER (PI8=3)
	INTEGER GSPH		! Dati tipo GAMMASPHERE
	PARAMETER (GSPH=4)
	INTEGER GSPN		! Dati tipo GASP - nuova DAQ (2001)
	PARAMETER (GSPN=5)
	INTEGER YALE		! Dati tipo YrastBall - Yale Univ.
	PARAMETER (YALE=6)
	INTEGER TNDB
	PARAMETER (TNDB=7)	! Format Tandem-Bucharest
	INTEGER PRISMA
	PARAMETER (PRISMA=8)	! Format PRISMA (spettrometro)
	INTEGER GSR
	PARAMETER (GSR=9)       ! Temporary, GSPH reduced data
	INTEGER GANIL
	PARAMETER (GANIL=10)    ! GANIL data (EXOGAM,DIAMANT,NWALL)
	
	INTEGER CSEG,QSEG,TSEG,ISEG,SISEG, PPACSEG, DANTESEG
	PARAMETER (CSEG=7, QSEG=4, TSEG=1, ISEG=1, SISEG=4, PPACSEG=10, DANTESEG=8)
	INTEGER CDET,QDET,TDET,SIDET, PPACDET
	PARAMETER (CDET=15, QDET=26, TDET=30, SIDET=40, PPACDET=1)
	INTEGER COFF,QOFF,TOFF,FOFF
	PARAMETER (COFF=0, QOFF=COFF+CDET*CSEG, TOFF=QOFF+QDET*QSEG, FOFF=TOFF+TDET*TSEG)

	INTEGER MAXNBAN		! MAX # banane per BANANA e PIN
	PARAMETER (MAXNBAN=32)
	INTEGER MAXRCL_ORD	! MAX # coefficienti di ricalibrazione
	PARAMETER (MAXRCL_ORD=6)
	INTEGER MAXRCL_REGS	! MAX # regioni di ricalibrazione
	PARAMETER (MAXRCL_REGS=6)

	INTEGER MAXTADJ		! MAX # di parametri in time_adjust
	PARAMETER (MAXTADJ=16)

	INTEGER MAXHASH		! MAX # liste hashgates
	PARAMETER (MAXHASH=50)

	INTEGER MAXPAIRS	! MAX # liste ind=f(id,id) definibili
	PARAMETER (MAXPAIRS=4)
	
	INTEGER MAXMATDIM	! massima dimensione della matrice
	PARAMETER (MAXMATDIM=4)

	INTEGER DEFSTEP2D	! suddivisione default 2D
	PARAMETER (DEFSTEP2D=128)
	INTEGER DEFSTEP3D	! suddivisione default 3D
	PARAMETER (DEFSTEP3D=64)
	INTEGER DEFSTEP4D	! suddivisione default 4D
	PARAMETER (DEFSTEP4D=32)

	INTEGER DEFSTEP2DS	! suddivisione default 2D_symm
	PARAMETER (DEFSTEP2DS=128)
	INTEGER DEFSTEP3DS	! suddivisione default 3D_symm
	PARAMETER (DEFSTEP3DS=64)
	INTEGER DEFSTEP4DS	! suddivisione default 4D_symm
	PARAMETER (DEFSTEP4DS=32)

	INTEGER DEFSTEP2DH	! suddivisione default 2D_hsymm
	PARAMETER (DEFSTEP2DH=32)
	INTEGER DEFSTEP3DH	! suddivisione default 3D_hsymm
	PARAMETER (DEFSTEP3DH=8)
	INTEGER DEFSTEP4DH	! suddivisione default 4D_hsymm
	PARAMETER (DEFSTEP4DH=8)

	INTEGER MAXBYTES	! Max. lunghezza records su nastro
	PARAMETER (MAXBYTES=32*1024)
	INTEGER MAXWORDS	! Max. lunghezza records su nastro
	PARAMETER (MAXWORDS=MAXBYTES/2)
	INTEGER MAXEVL		! MAX lunghezza evento (words)
	PARAMETER (MAXEVL=2047)

	INTEGER MTFLAGR		! Flag per sync. nastro input
	PARAMETER (MTFLAGR=1)
	INTEGER MTFLAGW		! Flag per sync. nastro output
	PARAMETER (MTFLAGW=2)

	INTEGER NFORMCOM	! # comandi di Formato
	PARAMETER (NFORMCOM=9)
	INTEGER NDECLCOM	! # comandi di Dichiarazione
	PARAMETER (NDECLCOM=9)
	INTEGER NANALCOM	! # comandi di Analisi
	PARAMETER (NANALCOM=199)

	INTEGER MAXCOMANDI	! MAX # di COMANDI
	PARAMETER (MAXCOMANDI=360)
# 3 "./gsort.def" 2 

	structure/detector/
	  union
	    map
	      integer id
	    endmap
	    map
	      integer ival(-1:maxpar-1)
	    endmap
	  endunion
	  real    xval( 0:maxpar-1)
	endstructure

	structure/hashdat/
	  INTEGER IND,PAR,RES			! su quale parametro
	  INTEGER WHICH				! gate number
	  logical*1 SAMEFORALL			! same gate for all detectors
	  logical*1 FROMFILE			! dati da file
	  INTEGER NGATES(0:MAXDET-1)		! quanti gates ha letto
	  CHARACTER*72 FILE			! File dei gates
	  REAL    DIST2(0:1)			! Distanza**2 normalizzata per gate sferici
	endstructure
	
	structure/pairsdat/
	  INTEGER INDMAX
	  CHARACTER*72 FILE			! nomi dei file con i dati
	  INTEGER PIND(0:MAXDET-1,0:MAXDET-1)
	endstructure

	structure/recall/
	  logical*1 always
	endstructure

	structure/fold/
	  INTEGER IND		! Tipo di parametro
	  INTEGER MIN		! Finestra sul fold
	  INTEGER MAX		! Finestra sul fold
	endstructure

	structure/gate/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  logical*1 INSIDE	! dentro/fuori
	  INTEGER LO		! Estremo inferiore
	  INTEGER HI		! Estremo superiore
	  INTEGER NGATES	! Quanti intervalli
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	endstructure

	structure/gatesdat/
	  logical*1 BAD(0:1)
	endstructure

	structure/filter/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PARS		! parametro sorgente
	  INTEGER PARD		! parametro destinazione
	  INTEGER RES		! Risoluzione filtro
	  character*80 file     ! no comment
	  integer iform         !
	  real F(0:MAXRES)        !
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	  logical*1 initialized   !
	endstructure

	structure/window/
	  INTEGER  IND		! Tipo di parametro
	  INTEGER  LO(0:MAXPAR-1)! Valore inferiore
	  INTEGER  HI(0:MAXPAR-1)! Valore superiore
	  INTEGER  FL		! Minfold
	  INTEGER  FH		! Minfold
	  logical*1  SAME		! Same as before window
	endstructure

	structure/banana/
	  INTEGER IND(2)	! Tipo di parametro x,y
	  INTEGER PAR(2)	! Parametro asse x,y
	  INTEGER RES(2)	! Risoluzione della banana
	  INTEGER NBAN		! Numero di banane
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 INSIDE	! In-Out
	  logical*1 SAME		! Same as before banana
	  logical*1 multiadc      ! N.M. - for ADC-dependent banana
	  logical*1 ignore(0:MAXDET-1) ! only for ADC-dependent banana
	endstructure

	structure/bananadat/
	  character*72 file
	  integer ban(2,0:1)
	endstructure

	structure/bananasdat/
	  character*72 file(MAXNBAN)
	  integer ban(2,0:1)
	endstructure

	structure/pairgate/
	   integer IND
	   integer LO
	   integer HI
	   integer FL
	   integer FH
	   integer PAR
	   integer RES
	   integer*2 PVAL(0:MAXDET-1,0:MAXDET-1)
	   integer*2 LIST(0:MAXDET-1)
	   character*72 filename
	   logical*1 INSIDE
	   logical*1 SAME
	endstructure

	structure/pin/
	  INTEGER IND(4)	! Tipo di parametro per banane
	  INTEGER PAR(4)	! Parametro asse x,y ; la massa e il tipo della particella
	  INTEGER RES(2)	! Risoluzione delle banane
	  INTEGER FIX		! Parametro fisso per il risultato
	  INTEGER FIXRES        ! Risoluzione dell parametro fisso
	  INTEGER NBAN		! quante banane
	  INTEGER F1(MAXNBAN)	! Numero di particelle della banana
	  INTEGER F2(MAXNBAN)	! Peso della banana in PIN
	  INTEGER F3(MAXNBAN)	! Massa della particella
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before PIN
	  logical*1 multiadc(MAXNBAN)          ! N.M. - for ADC-dependent banana
	  logical*1 ignore(MAXNBAN,0:MAXDET-1) ! only for ADC-dependent banana
	endstructure

	structure/hk/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER FIXH		! Parametro fisso per H
	  INTEGER FIXk		! Parametro fisso per k
	  REAL	  OFFS		! Offset per H
	  REAL	  GAIN		! Guadagno finale per H
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	endstructure

	structure/recal/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 RUN		! coefficenti run-dependent
	  logical*1 SAME	! Same as before recal
	  logical*1 ROUND       ! Round the result to integer value
	  logical*1 ZERO        ! Set negative result to zero
	  CHARACTER*73 FILE	! files dei coefficenti
	endstructure

	structure/recal_choose/
	  INTEGER IND(3)	! Tipo di parametro
	  INTEGER PAR(3)	! Quale parametro
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL    VAL           ! Valore di riferimento per fare la scelta
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  CHARACTER*73 FILE1	! files dei coefficenti
	  CHARACTER*73 FILE2	! files dei coefficenti
	  logical*1 RUN		! coefficenti run-dependent
	  logical*1 SAME	! Same as before recal
	  logical*1 ROUND       ! Round the result to integer value
	endstructure

	structure/calcoef/
	  INTEGER*4 ORD
	  REAL*4    COEF(MAXRCL_ORD)
	endstructure

	structure/mcalcoef/
	  integer nregs
	  INTEGER ORD(MAXRCL_REGS)
	  REAL    COEF(MAXRCL_ORD,MAXRCL_REGS)
	  real    limit(0:MAXRCL_REGS)
	endstructure


	structure/doppler/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET		! Quanti rivelatori
         real*8  fact1
         real*8  fact2
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL	  VC0		! Velocita' di rinculo
	  real    rtheta
	  real    rphi
	  REAL	  E0		! VC0 fino a E0
	  REAL	  E1		! Da VC0 a VC1 tra E0 e E1
	  REAL	  VC1		! VC1 sopra E1
	  REAL	  SLOPE		! Slope tra E0 e E1
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
         logical*1 RefChanged
	  logical*1 CONST		! Costante
	  logical*1 SAME		! Same as before recal
	  CHARACTER*73 FILE	! file con gli angoli
	endstructure

	structure/dopplerdat/
	  real costheta(0:1)
	endstructure
	
	structure/polar/
         real*8  fact1
         real*8  fact2
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET		! Quanti rivelatori
          INTEGER DTHETA        ! Angolo THETA del rivelatore, se viene dato come parametro
          INTEGER DPHI          ! Angolo PHI del rivelatore, se viene dato come parametro
	  INTEGER IND_POLAR
	  INTEGER PTHETA
	  INTEGER PPHI
         INTEGER IND_VEL
         INTEGER PVEL
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL	  VC0		! Velocita' di rinculo
	  REAL	  E0		! VC0 fino a E0
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
          logical*1 FIXED
	  logical*1 VarDetAngle	! Se angoli degli rivelatori sono parametri
	  logical*1 SAME		! Same as before recal
	  CHARACTER*73 FILE	! file con gli angoli
	endstructure
	
	structure/polardat/
	  real cdir(3,0:MAXDET-1)
	endstructure

	structure/meanvalstr/
	  INTEGER IND1(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR1(MAXTADJ)	! Quale parametro
	  INTEGER RES1(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY1	! Quanti definiti
	  REAL    POSITION	! Posizione finale
	  REAL	  FACTOR	! Rejection factor
	  INTEGER IND2	! FIX only
	  INTEGER PAR2	! Quale parametro fisso
	  INTEGER RES2	! Risoluzione
	endstructure

	structure/tadjust/
	  INTEGER IND1(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR1(MAXTADJ)	! Quale parametro
	  INTEGER RES1(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY1	! Quanti definiti
	  REAL    POSITION	! Posizione finale
	  REAL	  FACTOR	! Rejection factor
	  INTEGER IND2(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR2(MAXTADJ)	! Quale parametro
	  INTEGER RES2(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY2	! Quanti definiti
	endstructure

	structure/tref/
	   integer*8 N
# 285

	   real*16 SQSUM

	   INTEGER IND
	   INTEGER PAR
	   INTEGER RES
	   INTEGER REFNO
	   REAL POSITION
	endstructure

	structure/kine/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET_GE	! quanti germani
	  INTEGER NDET_SI	! quanti silici
	  INTEGER BIND(2)	! Tipo di parametro per banane
	  INTEGER BPAR(2)	! Parametro asse x,y
	  INTEGER BRES(2)	! Risoluzione della banana
	  INTEGER NBAN		! Numero di banane
	  INTEGER PINFIX	! Fixpar per PIN (se >=0)
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before recal
	  CHARACTER*72 KINEFILE	! file di descrizione
	  CHARACTER*72 AFILE_GE	! angoli germanio
	  CHARACTER*72 AFILE_SI	! angoli silici
	endstructure

	structure/kinedat/
	  REAL    ACMP,ECMP		! il nucleo composto
	  REAL    XMOMCM		! suo momento
	  REAL    AD(MAXNBAN)		! massa della particella
	  INTEGER MDET(MAXNBAN)		! numero di rivelatori attivi
	  INTEGER F1(MAXNBAN)		! Numero di particelle della banana per PIN
	  INTEGER F2(MAXNBAN)		! Peso della banana per PIN
	  real mom_si(0:MAXDET-1,MAXNBAN)
	  real cdir_ge(3,0:MAXDET-1)
	  real cdir_si(3,0:MAXDET-1)
	  record/bananasdat/kban
	endstructure

	structure/kinenew/
	  INTEGER IND(3)		! Tipo di parametro
	  INTEGER PAR(3)		! Parametro
	  INTEGER RES(3)		! Risoluzione del parametro
	  INTEGER NDET_GE	! quanti germani
	  INTEGER NDET_SI	! quanti silici
	  real ecal
	  real acmp
	  real ecmp
	  real atenuation
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before recal
	  CHARACTER*72 AFILE_GE	! angoli germanio
	  CHARACTER*72 AFILE_SI	! angoli silici
	  character*73 except_file
	  logical*1 except
	endstructure

	structure/kinenewdat/
	  REAL    ACMP,ECMP		! il nucleo composto
	  REAL    XMOMCM		! suo momento
	  real    ecal
	  real    afac
	  real cdir_ge(3,0:MAXDET-1)
	  real cdir_si(3,0:MAXDET-1)
	  integer nregs
	  integer reg(MAXRES,2)
	endstructure

	structure/sltocm/
	  integer IND(2)
	  integer PAR(2)
	  integer NDET_SI
	  real F(2)
	  real COS_TH(0:MAXDET-1)
	  CHARACTER*72 AFILE_SI	! angoli silici
	endstructure


	structure/add/
	  INTEGER IND(3)	! Tipo di parametro
	  INTEGER PAR(3)	! Parametro 1+2==>3
	  REAL	  FAC(3)	! fattori moltiplicativi
	  REAL    OFFSET	! offset da sommare al risultato
	  REAL    GAIN		! gain sul risultato (non sul'offset)
	  INTEGER ICHAN		! canale limite per COMBINE
	  INTEGER DELTA		! Sliding range
	  INTEGER SLIDE		! Sliding value
	  logical*1 CHECK		! check che il secondo parametro sia > limit
	  logical*1 MULT		! fattori moltiplicativi?
	endstructure

	structure/kill/
	  INTEGER IND			! Tipo di parametro
	  logical*1 RUN			! run dependent selective kill
	  CHARACTER*72 FILE		! files contenente i detbad
	  logical*1 DET(0:MAXDET-1)	! Quali rivelatori
	endstructure

	structure/killdat/
	  logical*1 remove(0:MAXDET-1,0:MAXDET-1)
	endstructure

	structure/select/
	  INTEGER IND		! Tipo di parametro
	  logical*1 DET(0:MAXDET-1)  ! Quali rivelatori
	endstructure

	structure/listev/
	  logical*1 TOFILE	! list events on file
	  logical*1 ISOPEN
	  INTEGER LUN
	  CHARACTER*72 FILE
	endstructure

	structure/reorder/
	  INTEGER IND		! Tipo di parametro
	endstructure

	structure/statistics/
	  INTEGER WHICH
	endstructure

	structure/statdat/
	  INTEGER FOLD(0:MAXDET,0:NDTYPE,0:1)
	endstructure

	structure/swap/
	  INTEGER IND(2)	! Tipo di parametro x,y
	  INTEGER PAR(2)	! Parametro asse x,y
	endstructure

	structure/mask/
	  INTEGER IND		! Tipo di parametro x
	  INTEGER PAR		! Parametro asse x
	  INTEGER MASK		! Maschera
	  logical*1 ONE		! solo un parametro o tutti?
	endstructure

	structure/useful/
	  INTEGER IND		! Tipo di parametro (deve essere 0)
	  INTEGER PAR1		! Parametro per RUN#
	  INTEGER PAR2		! Parametro per REC#
	  INTEGER PAR3		! Parametro per EVCOUNT#
	  INTEGER PAR4		! Parametro per EVNUMBER#
	endstructure

	structure/move/
	  INTEGER IND1			! da quale tipo
	  INTEGER IND2			! a  quale tipo
	  INTEGER PAR1			! da quale parametro
	  INTEGER PAR2			! a  quale parametro
	  INTEGER OFFSET                ! offset alla nuova numerazione
	  logical*1 CONDITION		! TRUE if GATE or copy detector
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
c	  logical*1 INSIDE	! dentro/fuori
	  INTEGER LO		! Estremo inferiore
	  INTEGER HI		! Estremo superiore
c	  INTEGER NGATES	! Quanti intervalli
c	  INTEGER FL		! Minfold
c	  INTEGER FH		! Maxfold
c	  logical*1 SAME		! Same as before gate
	  INTEGER HOWMANY		! Howmany to move
	  INTEGER WHICH(0:MAXDET-1)	! Quali rivelatori
	endstructure

	structure/splitmerge/
	  INTEGER NIND			! quanti tipi di rivelatore coinvolti
	  INTEGER IND(0:NDTYPE)		! Quali rivelatori
	  logical*1 REMOVE(0:NDTYPE)	!
	  INTEGER OFFSET(0:NDTYPE)	! Offset degli indici
	endstructure

	structure/newid/
	  INTEGER IND			! tipo di rivelatore
	  INTEGER LUT(0:MAXDET-1)	! mappa dei nuovi indici
	  logical*1 REORDER		! reordina dopo la mappatura
	  CHARACTER*72 FILE		! files dei nuovi id
	endstructure

	structure/addback/
	  INTEGER IND			! Tipo di parametro
	  INTEGER PAR			! Quale parametro
	  INTEGER RES			! Risoluzione del parametro
	  INTEGER NSEGS			! quanti segmenti (se composito)
	  logical*1 REJECT		! elimina le doppie non contigue
	  logical*1 PATTERN		! produce la hit-pattern sommando tutto
	  INTEGER PATPAR		! dove la registra
	  logical*1 TGATE			! verifica la relazione temporale?
	  INTEGER TPAR			! su quale parametro
	  INTEGER TVAL			! massima deviazione
	endstructure

        structure /pms_str/
	    real*8  sin_70
	    real*8  sin_110
	    real*8  cos_20
	    real*8  tan_20
	    integer ind
	    integer par_r
	    integer par_p
	    integer par_e
	    integer par_icp
	    integer ind_theta
	    integer par_theta
	    integer ind_path
	    integer par_path
	    integer ind_x
	    integer par_x
	    integer ind_ic
	    integer par_a
	    integer par_b
	    integer par_c
	    integer par_d
	    real    thr
	    real    qfact
	end structure
	
	structure /pms_dat/
	   real*8 R(40,0:100)
	end structure
	 
	structure /qvalue_str/
	   real*8  costhp
	   real*8  sinthp
	   real*8  amu
	   integer ind_q
	   integer par_q
	   integer ind_mass
	   integer par_mass
	   integer ind_theta
	   integer par_theta
	   integer ind_phi
	   integer par_phi
	   integer ind_beta
	   integer par_beta
	   integer AP
	   integer AT
	   integer ATOT
	   integer    low
	   integer    high
	   real    ep
	   real    thp
	   real    gain
	   real    offset
	end structure

       structure /prisma_angles_str/
          real*8  prisma_angle
          real*8  D
          real*8  sinalpha
          real*8  cosalpha
          real*8  costhp
          real*8  sinthp
	  real*8  cx(0:3)
	  real*8  cy(0:3)
          integer ind
          integer parx
          integer pary
          integer parz
          integer parq
          integer part
	  integer parp
	  integer pard
	  integer degx
	  integer degy
	  integer oldstyle
	  character*128 calfile
       end structure
	   
	structure/proje/
	  INTEGER LEN		! Lunghezza totale degli spettri (LW)
	  INTEGER MFRES 	! risoluzione max. di F
	  INTEGER*8 NINCR
	endstructure

	structure/projedat/
	  CHARACTER*16 NAME(0:MAXPAR-1,0:NDTYPE)
	  INTEGER      ADDR(0:MAXDET-1,0:MAXPAR-1,0:NDTYPE)
	  INTEGER      SPEC(0:1)
	endstructure

	structure/spectrum/
	  INTEGER IND			! Tipo di parametro
	  INTEGER PAR			! Quale parametri
	  INTEGER RES			! numero di canali dello spettro
	  INTEGER IHASH			! Quale tabella di hash (se >=0)
	  INTEGER HIND,HPAR		! riportati qui per comodita'
	  logical*1 HSAME			! IND e' quello su cui applicare HGATE
	  INTEGER NHASH			! Quante volte
	  logical*1 SPHERICAL		! gate di tipo elissoidale
	  INTEGER MINFOLD		! fold minimo (per hsort1d)
	  INTEGER*8 NINCR		! numero di incrementi
	  CHARACTER*72 FILE		! Filename dello spettro
	endstructure

	structure/specdat/
	  INTEGER SPEC(0:1)
	endstructure

	structure/matrix/

	  integer*8 CMTBASE

	  INTEGER NDIM			! Ordine della matrice
	  INTEGER IND(MAXMATDIM)	! Tipo di parametri per i due lati
	  INTEGER PAR(MAXMATDIM)	! Quale dei parametri
	  INTEGER RES(MAXMATDIM)	! Dimensioni lati della matrice
	  INTEGER STEP(MAXMATDIM)	! suddivisione lati della matrice
	  INTEGER MATMODE		! 0=normal 1=symmetric 2=halfsymmetric
	  INTEGER MSDIM			! numero di indici simmetrizzati
	  INTEGER DOFFSET		! per SORTxD_DIFF
	  INTEGER NIND
	  INTEGER IHASH			! Quale tabella di hash (se >=0)
	  INTEGER HIND,HPAR		! riportati qui per comodita'
	  logical*1 HSAME			! IND e' quello su cui applicare HGATE
	  INTEGER NHASH			! quante volte
	  logical*1 SPHERICAL		! gate di tipo elissoidale
	  INTEGER TYPE
	  INTEGER IND1,IND2,IND3,IND4
	  INTEGER PAR1,PAR2,PAR3,PAR4
	  logical*1 HHH1,HHH2,HHH3,HHH4
	  CHARACTER*72 FILE		! Filename matrice
	endstructure

	structure/iodef/
	  logical*1 datafile	! TRUE se da file o Virtuale
	  logical*1 virtuale
	  logical*1 closed
	  integer lun
	  integer flag
	  integer wlen
	  integer run
	  integer rec
	  integer maxrec
	  integer evcount	! internal count
	  integer evnumber	! from ACQ
	  integer vrun,vevents
CVMS	  integer*2 iosb(4)
	  integer iosb(4)
CVMS	  integer*2 mtch,hctm
	  integer mtch
	  character*72 name
	endstructure

	structure/oevdef/
	  logical*1 defined
	  logical*1 same				! Same FILE structure on output
	  logical*1 sameRUN			! Same RUN number on output
	  logical*1 ask				! Will ask every EOF
	  logical*1 reduce			! Output events in forma ridotta
	  logical*1 det(0:NDTYPE)			! Output this detector
	  logical*1 par(0:MAXPAR-1,0:NDTYPE)  	! Output this parameter
	  integer evlen,pointer
	  logical*1 done
	endstructure

	structure/commands/
	  union
	    map
	      record/recall/recall
	    endmap
	    map
	      record/fold/fold
	    endmap
	    map
	      record/gate/gate
	    endmap
	    map
	      record/filter/filter
	    endmap
	    map
	      record/window/win
	    endmap
	    map
	      record/pairgate/pgate
	    endmap
	    map
	      record/banana/ban
	    endmap
	    map
	      record/pin/pin
	    endmap
	    map
	      record/recal/rcl
	    endmap
	    map
	      record/kine/kine
	    endmap
	    map
	      record/doppler/doppl
	    endmap
	    map
	      record/tadjust/tadj
	    endmap
	    map
	      record/tref/tref
	    endmap
	    map
	      record/hk/hk
	    endmap
	    map
	      record/add/add
	    endmap
	    map
	      record/kill/kill
	    endmap
	    map
	      record/select/sel
	    endmap
	    map
	      record/listev/lev
	    endmap
	    map
	      record/reorder/reo
	    endmap
	    map
	      record/statistics/stat
	    endmap
	    map
	      record/swap/swap
	    endmap
	    map
	      record/mask/mask
	    endmap
	    map
	      record/useful/useful
	    endmap
	    map
	      record/move/move
	    endmap
	    map
	      record/newid/newid
	    endmap
	    map
	      record/splitmerge/splmrg
	    endmap
	    map
	      record/polar/dpolar
	    endmap
	    map
	      record/pms_str/pms
	    endmap
	    map
	      record/qvalue_str/qvalue
	    endmap
	    map
	      record /prisma_angles_str/ang
	    endmap
	    map
	      record/addback/abck
	    endmap
	    map
	      record/proje/pro
	    endmap
	    map
	      record/spectrum/spec
	    endmap
	    map
	      record/matrix/mat
	    endmap
	  endunion

	  integer*8 addr

	  integer size
	  integer whichcom
	endstructure

	structure /tndio/
	  integer nfiles
	  integer current
	  logical*1 presorted
	end structure
# 2 "./gsort.inc" 2 

	INTEGER	  LLUN
	INTEGER	  LLIST
	INTEGER   GS2LUN
	INTEGER   LGSDIR
	CHARACTER GSDIR*64
	CHARACTER LIST*256
	CHARACTER LINE*256
	CHARACTER COMSORT*40
	integer INFILES
	COMMON /VARIECOM/ LLUN,LLIST,GS2LUN,LGSDIR,GSDIR,LIST,LINE,COMSORT,INFILES

	logical*1 BREAK
	logical*1 KILLEV
	logical*1 AUTONUMBER	! File etichettati con RUN#
	logical*1 TAPEDISMOUNT	! Controllo fine nastro
	logical*1 LASTTAPE	! Controllo fine nastro
	logical*1 AUTOLOADER
	logical*1 STORED_EV
	logical*1 DEFINED_OUT,FINISHED_OUT
	COMMON/FLAGS/ BREAK,KILLEV,AUTONUMBER,TAPEDISMOUNT,LASTTAPE,AUTOLOADER,STORED_EV,
     1 DEFINED_OUT,FINISHED_OUT

	logical*1 SYNTAX
	INTEGER PHASE
	CHARACTER COMLINE*256
	INTEGER LCOMLINE
	INTEGER INPLU
	CHARACTER*128  FORM(NFORMCOM)
	CHARACTER*128  DECL(NDECLCOM)
	CHARACTER*128  ANAL(NANALCOM)
	CHARACTER*128 SYNT(NSYNT)
	COMMON /COMDEFS/ PHASE,COMLINE,LCOMLINE,INPLU,FORM,DECL,ANAL,SYNT,SYNTAX

	RECORD /COMMANDS/COM(MAXCOMANDI)
	INTEGER	NCOMANDI
	INTEGER ICMD
	INTEGER WHICHCOM
	INTEGER INIT_MODE,FINIT_MODE
	COMMON /COMMAND/ COM,NCOMANDI,ICMD,WHICHCOM,INIT_MODE,FINIT_MODE

	INTEGER   DATAFORMAT			! GASP/EUROBALL/8PI
	INTEGER   GASPMAP(0:NDTYPE)		! Mappatura per formato GASP
	INTEGER   EUROMAP(0:NDTYPE)		! Mappatura per formato euroball
	INTEGER   ISEED				! Seed per i numeri random
	INTEGER   NTIPI				! quanti tipi di rivelatori
	INTEGER   NDPAR(0:NDTYPE)		! # parametri
	INTEGER   MDPAR(0:NDTYPE)		! # parametri aggiunti
	INTEGER   TDPAR(0:NDTYPE)		! # parametri totali
	INTEGER   PARRES(0:MAXPAR-1,0:NDTYPE)  	! Risoluzione dei vari parametri

	INTEGER   NDETS(0:NDTYPE)		! # di rivelatori (se composito)
	INTEGER   NSEGS(0:NDTYPE)		! # segmenti (se composito)
	INTEGER   NCPAR(0:NDTYPE)		! # di parametri comuni (se composito)
	
	INTEGER   NITEMS(0:NDTYPE)		! # totale di rivelatori
	logical*1   EXISTS(0:NDTYPE)		! Esiste il tipo di rivelatore
	INTEGER   FOLDMIN(0:NDTYPE)		! Minimo fold da nastro
	CHARACTER*1 DNAME(0:NDTYPE)		! simboli per rivelatori
	COMMON /EVDEF/DATAFORMAT,GASPMAP,EUROMAP,ISEED,NTIPI,NDPAR,MDPAR,
     1TDPAR,PARRES,NDETS,NSEGS,NCPAR,NITEMS,FOLDMIN,EXISTS,DNAME

	INTEGER   CLASS,TAG			! Descrizione dell'evento
	INTEGER   NDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   DOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/DET(0:MAXDET-1)		! i dati
	COMMON /CEVENT/  CLASS,TAG,NDET,DOFF,DET

	INTEGER   sCLASS,sTAG			! Copia dell'evento per SAVE/RECALL
	INTEGER   sNDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   sDOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/sDET(0:MAXDET-1)	! i dati
	COMMON /SEVENT/  sCLASS,sTAG,sNDET,sDOFF,sDET

	INTEGER   mCLASS,mTAG			! Copia di lavoro (Move...)
	INTEGER   mNDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   mDOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/mDET(0:MAXDET-1)	! i dati
	COMMON /mEVENT/  mCLASS,mTAG,mNDET,mDOFF,mDET


	integer*8 HASHADDR(0:MAXHASH-1)

	INTEGER HASHGIVEN
	INTEGER HNTRUE,HSTRUE			! Quanti rivelatori in gate
	logical*1 HLTRUE(0:MAXDET-1)		! Quali rivelatori in gate
	REAL    HDIST1(0:MAXDET-1)		! per gestione gates sferici
	REAL    HDIST2(0:MAXDET-1)		! per gestione gates sferici
	COMMON /HASHDEF/ HASHADDR,HASHGIVEN,HNTRUE,HSTRUE,HLTRUE,HDIST1,HDIST2


	integer*8 PAIRSADDR(0:MAXPAIRS-1)

	INTEGER PAIRSGIVEN
	COMMON /PAIRSDEF/ PAIRSADDR,PAIRSGIVEN


	integer*8 STATADDR

	INTEGER STATSIZE
	INTEGER NSTATCOM			! quanti comandi di statistica
	COMMON /STATISTICS/ STATADDR,STATSIZE,NSTATCOM

	INTEGER NLOST,NCLASS
	INTEGER*8 NINCR(MAXMATDIM)
	INTEGER FLUSHED
	INTEGER NPRINT
	COMMON /STATUS/ NLOST,NCLASS,NINCR,FLUSHED,NPRINT

	integer*2 ievbuf(16*MAXWORDS)	! Buffer dati
	integer*2 ievheader(MAXWORDS)	! header record
	record/iodef/iio
	common/ievcom/ievheader,ievbuf,iio

	record/oevdef/oev
	integer*2 oevent(MAXEVL*4)	! evento da scrivere
	integer*2 oevbuf(MAXBYTES/2)	! Buffer scrittura eventi
	record/iodef/oio
	common/oevcom/oev,oevent,oevbuf,oio

	record /tndio/tnd
	common /tndcom/tnd
# 1939 "gsort.F" 2 

CVMS	INTEGER LABELNN(MAXCOMANDI+1)
	integer*8 LABELNN(MAXCOMANDI+1),label_325
CVMS	INTEGER LABEL
	integer*8 LABEL
	EQUIVALENCE (LABEL,LABELNN)
	logical*1 INITIALIZE /.TRUE./

	if(initialize) then
	 DO ICMD=1,NCOMANDI
	   whichcom=com(icmd).whichcom

	  goto(	      1,  2,  3,  4,  5,  6,  7,  8,  9,
     1	 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
     2	 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
     3	 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
     4	 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
     5	 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
     6	 60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
     7	 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
     8	 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
     9	 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
     1	 100,101,102,103,104,105,106,107,108,109,
     1	 110,111,112,113,114,115,116,117,118,119,
     1	 120,121,122,123,124,125,126,127,128,129,
     1	 130,131,132,133,134,135,136,137,138,139,
     1	 140,141,142,143,144,145,146,147,148,149) whichcom

	   call ansi_bell(6)
	   stop  'Inconsistent initialization in EVANA'

1	   assign 301 to label
	   goto 888
2	   assign 302 to label
	   goto 888
3	   assign 303 to label
	   goto 888
4	   assign 304 to label
	   goto 888
5	   assign 305 to label
	   goto 888
6	   assign 306 to label
	   goto 888
7	   assign 307 to label
	   goto 888
8	   assign 308 to label
	   goto 888
9	   assign 309 to label
	   goto 888
10	   assign 310 to label
	   goto 888
11	   assign 311 to label
	   goto 888
12	   assign 312 to label
	   goto 888
13	   assign 313 to label
	   goto 888
14	   assign 314 to label
	   goto 888
15	   assign 315 to label
	   goto 888
16	   assign 316 to label
	   goto 888
17	   assign 317 to label
	   goto 888
18	   assign 318 to label
	   goto 888
19	   assign 319 to label
	   goto 888
20	   assign 320 to label
	   goto 888
21	   assign 321 to label
	   goto 888
22	   assign 322 to label
	   goto 888
23	   assign 323 to label
	   goto 888
24	   assign 324 to label
	   goto 888
25	   assign 325 to label
	   label_325=labelnn(1)
	   goto 888
26	   assign 326 to label
	   goto 888
27	   assign 327 to label
	   goto 888
28	   assign 328 to label
	   goto 888
29	   assign 329 to label
	   goto 888
30	   assign 330 to label
	   goto 888
31	   assign 331 to label
	   goto 888
32	   assign 332 to label
	   goto 888
33	   assign 333 to label
	   goto 888
34	   assign 334 to label
	   goto 888
35	   assign 335 to label
	   goto 888
36	   assign 336 to label
	   goto 888
37	   assign 337 to label
	   goto 888
38	   assign 338 to label
	   goto 888
39	   assign 339 to label
	   goto 888
40	   assign 340 to label
	   goto 888
41	   assign 341 to label
	   goto 888
42	   assign 342 to label
	   goto 888
43	   assign 343 to label
	   goto 888
44	   assign 344 to label
	   goto 888
45	   assign 345 to label
	   goto 888
46	   assign 346 to label
	   goto 888
47	   assign 347 to label
	   goto 888
48	   assign 348 to label
	   goto 888
49	   assign 349 to label
	   goto 888
50	   assign 350 to label
	   goto 888
51	   assign 351 to label
	   goto 888
52	   assign 352 to label
	   goto 888
53	   assign 353 to label
	   goto 888
54	   assign 354 to label
	   goto 888
55	   assign 355 to label
	   goto 888
56	   assign 356 to label
	   goto 888
57	   assign 357 to label
	   goto 888
58	   assign 358 to label
	   goto 888
59	   assign 359 to label
	   goto 888
60	   assign 360 to label
	   goto 888
61	   assign 361 to label
	   goto 888
62	   assign 362 to label
	   goto 888
63	   assign 363 to label
	   goto 888
64	   assign 364 to label
	   goto 888
65	   assign 365 to label
	   goto 888
66	   assign 366 to label
	   goto 888
67	   assign 367 to label
	   goto 888
68	   assign 368 to label
	   goto 888
69	   assign 369 to label
	   goto 888
70	   assign 370 to label
	   goto 888
71	   assign 371 to label
	   goto 888
72	   assign 372 to label
	   goto 888
73	   assign 373 to label
	   goto 888
74	   assign 374 to label
	   goto 888
75	   assign 375 to label
	   goto 888
76	   assign 376 to label
	   goto 888
77	   assign 377 to label
	   goto 888
78	   assign 378 to label
	   goto 888
79	   assign 379 to label
	   goto 888
80	   assign 380 to label
	   goto 888
81	   assign 381 to label
	   goto 888
82	   assign 382 to label
	   goto 888
83	   assign 383 to label
	   goto 888
84	   assign 384 to label
	   goto 888
85	   assign 385 to label
	   goto 888
86	   assign 386 to label
	   goto 888
87	   assign 387 to label
	   goto 888
88	   assign 388 to label
	   goto 888
89	   assign 389 to label
	   goto 888
90	   assign 390 to label
	   goto 888
91	   assign 391 to label
	   goto 888
92	   assign 392 to label
	   goto 888
93	   assign 393 to label
	   goto 888
94	   assign 394 to label
	   goto 888
95	   assign 395 to label
	   goto 888
96	   assign 396 to label
	   goto 888
97	   assign 397 to label
	   goto 888
98	   assign 398 to label
	   goto 888
99	   assign 399 to label
	   goto 888
100	   assign 400 to label
	   goto 888
101	   assign 401 to label
	   goto 888
102	   assign 402 to label
	   goto 888
103	   assign 403 to label
	   goto 888
104	   assign 404 to label
	   goto 888
105	   assign 405 to label
	   goto 888
106	   assign 406 to label
	   goto 888
107	   assign 407 to label
	   goto 888
108	   assign 408 to label
	   goto 888
109	   assign 409 to label
	   goto 888
110	   assign 410 to label
	   goto 888
111	   assign 411 to label
	   goto 888
112	   assign 412 to label
	   goto 888
113	   assign 413 to label
	   goto 888
114	   assign 414 to label
	   goto 888
115	   assign 415 to label
	   goto 888
116	   assign 416 to label
	   goto 888
117	   assign 417 to label
	   goto 888
118	   assign 418 to label
	   goto 888
119	   assign 419 to label
	   goto 888
120	   assign 420 to label
	   goto 888
121	   assign 421 to label
	   goto 888
122	   assign 422 to label
	   goto 888
123	   assign 423 to label
	   goto 888
124	   assign 424 to label
	   goto 888
125	   assign 425 to label
	   goto 888
126	   assign 426 to label
	   goto 888
127	   assign 427 to label
	   goto 888
128	   assign 428 to label
	   goto 888
129	   assign 429 to label
	   goto 888
130	   assign 430 to label
	   goto 888
131	   assign 431 to label
	   goto 888
132	   assign 432 to label
	   goto 888
133	   assign 433 to label
	   goto 888
134	   assign 434 to label
	   goto 888
135	   assign 435 to label
	   goto 888
136	   assign 436 to label
	   goto 888
137	   assign 437 to label
	   goto 888
138	   assign 438 to label
	   goto 888
139	   assign 439 to label
	   goto 888
140	   assign 440 to label
	   goto 888
141	   assign 441 to label
	   goto 888
142	   assign 442 to label
	   goto 888
143	   assign 443 to label
	   goto 888
144	   assign 444 to label
	   goto 888
145	   assign 445 to label
	   goto 888
146	   assign 446 to label
	   goto 888
147	   assign 447 to label
	   goto 888
148	   assign 448 to label
	   goto 888
149	   assign 449 to label
	   goto 888

888	   do jj=ncomandi+1,2,-1
	    labelnn(jj)=labelnn(jj-1)
	  enddo
	 ENDDO
	 initialize=.false.
	endif

	NSTA=0
	KILLEV=.FALSE.
	STORED_EV=.FALSE.
	oev.done=.false.

	icmd = 1

	DO while ( ICMD .le. NCOMANDI)
	   whichcom=com(icmd).whichcom
	   LABELNN(1)=LABELNN(NCOMANDI+2-ICMD)
	   GOTO LABEL

301	  call gs3_fold(com(icmd),%val(com(icmd).addr))
	  goto 800

302	  call gs3_gate(com(icmd),%val(com(icmd).addr))
	  goto 800

303	  call gs3_gates(com(icmd),%val(com(icmd).addr))
	  goto 800

304	  call gs3_window(com(icmd),%val(com(icmd).addr))
	  goto 800

305	  call gs3_banana(com(icmd),%val(com(icmd).addr))
	  goto 800

306	  call gs3_bananas(com(icmd),%val(com(icmd).addr))
	  goto 800

307	  call gs3_filter(com(icmd),%val(com(icmd).addr))
	  goto 800

308	  call gs3_xban(com(icmd),%val(com(icmd).addr))
	  goto 800

309	  goto 900
310	  call gs3_recal_mult(com(icmd),%val(com(icmd).addr))
	  goto 800

311	  call gs3_gain(com(icmd),%val(com(icmd).addr))
	  goto 800

312	  call gs3_recal(com(icmd),%val(com(icmd).addr))
	  goto 800

313	  call gs3_recal_lut(com(icmd),%val(com(icmd).addr))
	  goto 800

314	  call gs3_time_reference(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
315	  call gs3_timing(com(icmd),%val(com(icmd).addr))
	  goto 800

316	  call gs3_recal_doppler(com(icmd),%val(com(icmd).addr))
	  goto 800

317	  call gs3_recal_kinenew(com(icmd),%val(com(icmd).addr))
	  goto 800

318	  call gs3_time_adjust(com(icmd),%val(com(icmd).addr))
	  goto 800

319	  call gs3_pin(com(icmd),%val(com(icmd).addr))
	  goto 800

320	  call gs3_hk(com(icmd),%val(com(icmd).addr))
	  goto 800

321	  call gs3_add(com(icmd),%val(com(icmd).addr))
	  goto 800

322	  call gs3_kill(com(icmd),%val(com(icmd).addr))
	  goto 800

323	  call gs3_select(com(icmd),%val(com(icmd).addr))
	  goto 800

324	  call gs3_store_event(com(icmd),%val(com(icmd).addr))
	  goto 800

325	  call gs3_recall_event(com(icmd),%val(com(icmd).addr))
	  goto 800

326	  call gs3_write_event(com(icmd),%val(com(icmd).addr))
	  goto 800

327	  call gs3_list_event(com(icmd),%val(com(icmd).addr))
	  goto 800

328	  call gs3_reorder(com(icmd),%val(com(icmd).addr))
	  goto 800

329	  call gs3_statistics(com(icmd),%val(com(icmd).addr))
	  goto 800

330	  call gs3_swap(com(icmd),%val(com(icmd).addr))
	  goto 800

331	  call gs3_mask(com(icmd),%val(com(icmd).addr))
	  goto 800

332	  call gs3_move(com(icmd),%val(com(icmd).addr))
	  goto 800

333	  call gs3_merge(com(icmd),%val(com(icmd).addr))
	  goto 800

334	  call gs3_split(com(icmd),%val(com(icmd).addr))
	  goto 800

335	  call gs3_newid(com(icmd),%val(com(icmd).addr))
	  goto 800

336	  call gs3_ebkill(com(icmd),%val(com(icmd).addr))
	  goto 800

337	  call gs3_combine(com(icmd),%val(com(icmd).addr))
	  goto 800

338	  call gs3_addback(com(icmd),%val(com(icmd).addr))
	  goto 800

339	  call gs3_copy(com(icmd),%val(com(icmd).addr))
	  goto 800

340	  call gs3_useful(com(icmd),%val(com(icmd).addr))
	  goto 800

341	  call gs3_thresholds(com(icmd),%val(com(icmd).addr))
	  goto 800

342	  goto 900
343	  goto 900

344	  call gs3_mult(com(icmd),%val(com(icmd).addr))
	  goto 800

345	  call gs3_mean_value(com(icmd),%val(com(icmd).addr))
	  goto 800

346	  call gs3_div(com(icmd),%val(com(icmd).addr))
	  goto 800

347	  call gs3_SLtoCM(com(icmd),%val(com(icmd).addr))
	  goto 800

348	  call gs3_recal_choose(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
349	  call gs3_wmean_value(com(icmd),%val(com(icmd).addr))
	  goto 800

350	  call gs3_recal_polar(com(icmd),%val(com(icmd).addr))
	  goto 800

351	  goto 900
352	  goto 900
353	  goto 900

354	  call gs3_angles_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

355	  call gs3_track_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

356	  call gs3_qvalue_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

357	  call gs3_bpvelocity_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

358	  call gs3_xfp_pms(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
359	  goto 900
360	  goto 900
361	  goto 900
362	  goto 900
363	  goto 900
364	  goto 900
365	  goto 900
366	  goto 900
367	  goto 900
368	  goto 900
369	  goto 900
370	  goto 900
371	  goto 900
372	  goto 900
373	  goto 900

374	  call gs3_break
	  goto 800
375	  call gs3_pairgate(com(icmd),%val(com(icmd).addr))
	  goto 800

376	  call gs3_pow(com(icmd),%val(com(icmd).addr))
	  goto 800

377	  call gs3_pairkill(com(icmd),%val(com(icmd).addr))
	  goto 800

378	  goto 900
379	  goto 900
380	  goto 900
381	  goto 900
382	  goto 900
383	  goto 900
384	  goto 900
385	  goto 900
386	  goto 900
387	  goto 900
388	  goto 900
389	  goto 900
390	  goto 900

391	  call usersub1(3)
	  goto 800

392	  call usersub2(3)
	  goto 800

393	  call usersub3(3)
	  goto 800

394	  call usersub4(3)
	  goto 800

395	  call usersub5(3)
	  goto 800

396	  call usersub6(3)
	  goto 800

397	  call usersub7(3)
	  goto 800

398	  call usersub8(3)
	  goto 800

399	  call usersub9(3)
	  goto 800

400	goto 900

401	call gs3_projections(com(icmd), %VAL(com(icmd).addr))
	goto 800

402	call gs3_sort1d(com(icmd), %VAL(com(icmd).addr))
	goto 800

403	call gs3_sort1d_m(com(icmd), %VAL(com(icmd).addr))
	goto 800

404	goto 900
405	goto 900
406	goto 900
407	goto 900
408	goto 900
409	goto 900
410	goto 900
	
411	call gs3_sort2d(com(icmd), %VAL(com(icmd).addr))
	goto 800

412	call gs3_sort3d(com(icmd), %VAL(com(icmd).addr))
	goto 800

413	call gs3_sort4d(com(icmd), %VAL(com(icmd).addr))
	goto 800

414	goto 900
415	goto 900

416	call gs3_sort2d_ab(com(icmd), %VAL(com(icmd).addr))
	goto 800

417	goto 900
418	goto 900
419	goto 900
420	goto 900	

421	call gs3_sort2d_symm(com(icmd), %VAL(com(icmd).addr))
	goto 800

422	call gs3_sort3d_symm(com(icmd), %VAL(com(icmd).addr))
	goto 800

423	call gs3_sort4d_symm(com(icmd), %VAL(com(icmd).addr))
	goto 800

424	goto 900

425	call gs3_sort3d_pair(com(icmd), %VAL(com(icmd).addr))
	goto 800

426	goto 900

427	call gs3_sort3d_diff(com(icmd), %VAL(com(icmd).addr))
	goto 800

428	goto 900
429	goto 900
430	goto 900

431	call gs3_sort2d_hsymm(com(icmd), %VAL(com(icmd).addr))
	goto 800

432	call gs3_sort3d_hsymm(com(icmd), %VAL(com(icmd).addr))
	goto 800

433	call gs3_sort4d_hsymm(com(icmd), %VAL(com(icmd).addr))
	goto 800

434	goto 900
435	goto 900
436	goto 900
437	goto 900
438	goto 900
439	goto 900
440	goto 900
441	goto 900
442	goto 900
443	goto 900
444	goto 900
445	goto 900
446	goto 900
447	goto 900
448	goto 900
449	goto 900

800	  if(killev)then
	     if(.NOT.STORED_EV) return
	     do jcmd=icmd+1,ncomandi
	       whichcom=com(jcmd).whichcom
	       LABELNN(1)=LABELNN(NCOMANDI+2-JCMD)
	       if(whichcom .eq. 25)then
*	          killev=.false.
	          icmd=jcmd
		  goto LABEL
	       endif
	     enddo
	     return
	  endif
	  icmd = icmd + 1

	ENDDO

	return

900	Stop 'Unknown command'

	END

	subroutine FINIT_RUN(mode)

! chiamata dopo ogni RUN e alla fine

# 1 "./gsort.inc" 1 
# 1 "./gsort.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./gsort.def" 2 
# 1 "./gsort.par" 1 
	INTEGER LU1		! LU per messaggi
	PARAMETER (LU1=6)
	INTEGER LU2		! LU per Logbook
	PARAMETER (LU2=7)
	INTEGER INDENT		! incolonnamento listato programma
	PARAMETER (INDENT=20)
*	INTEGER NPRINT		! intervallo per stampa status su LU1
*	PARAMETER (NPRINT=400)
	INTEGER NPRINT2		! intervallo per stampa status su LU2
	PARAMETER (NPRINT2=2000)
	INTEGER NSTOP		! intervallo per check di mat_stop
	PARAMETER (NSTOP=1000)
	INTEGER NSYNT		! # righe per la sintassi
	PARAMETER (NSYNT=5)

	INTEGER MAXRES		! Risoluzione massima gestita
	PARAMETER (MAXRES=32768)
	INTEGER MINRES		! Risoluzione minima gestita
	PARAMETER (MINRES=16)
	INTEGER MAXDET		! MAX # di rivelatori
	PARAMETER (MAXDET=256)
	INTEGER MAXPAR		! MAX # di parametri/rivelatore
	PARAMETER (MAXPAR=16)
	INTEGER NDTYPE		! Numero di tipi di rivelatore
	PARAMETER (NDTYPE=16)
	REAL PSTEP
	PARAMETER (PSTEP=359./1024.)

	INTEGER NOFORMAT	! No format defined
	PARAMETER (NOFORMAT=-1)
	INTEGER GASP		! Dati tipo GASP
	PARAMETER (GASP=1)
	INTEGER EURO		! Dati tipo Euroball
	PARAMETER (EURO=2)
	INTEGER PI8		! Dati tipo 8Pi-Berkeley
	PARAMETER (PI8=3)
	INTEGER GSPH		! Dati tipo GAMMASPHERE
	PARAMETER (GSPH=4)
	INTEGER GSPN		! Dati tipo GASP - nuova DAQ (2001)
	PARAMETER (GSPN=5)
	INTEGER YALE		! Dati tipo YrastBall - Yale Univ.
	PARAMETER (YALE=6)
	INTEGER TNDB
	PARAMETER (TNDB=7)	! Format Tandem-Bucharest
	INTEGER PRISMA
	PARAMETER (PRISMA=8)	! Format PRISMA (spettrometro)
	INTEGER GSR
	PARAMETER (GSR=9)       ! Temporary, GSPH reduced data
	INTEGER GANIL
	PARAMETER (GANIL=10)    ! GANIL data (EXOGAM,DIAMANT,NWALL)
	
	INTEGER CSEG,QSEG,TSEG,ISEG,SISEG, PPACSEG, DANTESEG
	PARAMETER (CSEG=7, QSEG=4, TSEG=1, ISEG=1, SISEG=4, PPACSEG=10, DANTESEG=8)
	INTEGER CDET,QDET,TDET,SIDET, PPACDET
	PARAMETER (CDET=15, QDET=26, TDET=30, SIDET=40, PPACDET=1)
	INTEGER COFF,QOFF,TOFF,FOFF
	PARAMETER (COFF=0, QOFF=COFF+CDET*CSEG, TOFF=QOFF+QDET*QSEG, FOFF=TOFF+TDET*TSEG)

	INTEGER MAXNBAN		! MAX # banane per BANANA e PIN
	PARAMETER (MAXNBAN=32)
	INTEGER MAXRCL_ORD	! MAX # coefficienti di ricalibrazione
	PARAMETER (MAXRCL_ORD=6)
	INTEGER MAXRCL_REGS	! MAX # regioni di ricalibrazione
	PARAMETER (MAXRCL_REGS=6)

	INTEGER MAXTADJ		! MAX # di parametri in time_adjust
	PARAMETER (MAXTADJ=16)

	INTEGER MAXHASH		! MAX # liste hashgates
	PARAMETER (MAXHASH=50)

	INTEGER MAXPAIRS	! MAX # liste ind=f(id,id) definibili
	PARAMETER (MAXPAIRS=4)
	
	INTEGER MAXMATDIM	! massima dimensione della matrice
	PARAMETER (MAXMATDIM=4)

	INTEGER DEFSTEP2D	! suddivisione default 2D
	PARAMETER (DEFSTEP2D=128)
	INTEGER DEFSTEP3D	! suddivisione default 3D
	PARAMETER (DEFSTEP3D=64)
	INTEGER DEFSTEP4D	! suddivisione default 4D
	PARAMETER (DEFSTEP4D=32)

	INTEGER DEFSTEP2DS	! suddivisione default 2D_symm
	PARAMETER (DEFSTEP2DS=128)
	INTEGER DEFSTEP3DS	! suddivisione default 3D_symm
	PARAMETER (DEFSTEP3DS=64)
	INTEGER DEFSTEP4DS	! suddivisione default 4D_symm
	PARAMETER (DEFSTEP4DS=32)

	INTEGER DEFSTEP2DH	! suddivisione default 2D_hsymm
	PARAMETER (DEFSTEP2DH=32)
	INTEGER DEFSTEP3DH	! suddivisione default 3D_hsymm
	PARAMETER (DEFSTEP3DH=8)
	INTEGER DEFSTEP4DH	! suddivisione default 4D_hsymm
	PARAMETER (DEFSTEP4DH=8)

	INTEGER MAXBYTES	! Max. lunghezza records su nastro
	PARAMETER (MAXBYTES=32*1024)
	INTEGER MAXWORDS	! Max. lunghezza records su nastro
	PARAMETER (MAXWORDS=MAXBYTES/2)
	INTEGER MAXEVL		! MAX lunghezza evento (words)
	PARAMETER (MAXEVL=2047)

	INTEGER MTFLAGR		! Flag per sync. nastro input
	PARAMETER (MTFLAGR=1)
	INTEGER MTFLAGW		! Flag per sync. nastro output
	PARAMETER (MTFLAGW=2)

	INTEGER NFORMCOM	! # comandi di Formato
	PARAMETER (NFORMCOM=9)
	INTEGER NDECLCOM	! # comandi di Dichiarazione
	PARAMETER (NDECLCOM=9)
	INTEGER NANALCOM	! # comandi di Analisi
	PARAMETER (NANALCOM=199)

	INTEGER MAXCOMANDI	! MAX # di COMANDI
	PARAMETER (MAXCOMANDI=360)
# 3 "./gsort.def" 2 

	structure/detector/
	  union
	    map
	      integer id
	    endmap
	    map
	      integer ival(-1:maxpar-1)
	    endmap
	  endunion
	  real    xval( 0:maxpar-1)
	endstructure

	structure/hashdat/
	  INTEGER IND,PAR,RES			! su quale parametro
	  INTEGER WHICH				! gate number
	  logical*1 SAMEFORALL			! same gate for all detectors
	  logical*1 FROMFILE			! dati da file
	  INTEGER NGATES(0:MAXDET-1)		! quanti gates ha letto
	  CHARACTER*72 FILE			! File dei gates
	  REAL    DIST2(0:1)			! Distanza**2 normalizzata per gate sferici
	endstructure
	
	structure/pairsdat/
	  INTEGER INDMAX
	  CHARACTER*72 FILE			! nomi dei file con i dati
	  INTEGER PIND(0:MAXDET-1,0:MAXDET-1)
	endstructure

	structure/recall/
	  logical*1 always
	endstructure

	structure/fold/
	  INTEGER IND		! Tipo di parametro
	  INTEGER MIN		! Finestra sul fold
	  INTEGER MAX		! Finestra sul fold
	endstructure

	structure/gate/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  logical*1 INSIDE	! dentro/fuori
	  INTEGER LO		! Estremo inferiore
	  INTEGER HI		! Estremo superiore
	  INTEGER NGATES	! Quanti intervalli
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	endstructure

	structure/gatesdat/
	  logical*1 BAD(0:1)
	endstructure

	structure/filter/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PARS		! parametro sorgente
	  INTEGER PARD		! parametro destinazione
	  INTEGER RES		! Risoluzione filtro
	  character*80 file     ! no comment
	  integer iform         !
	  real F(0:MAXRES)        !
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	  logical*1 initialized   !
	endstructure

	structure/window/
	  INTEGER  IND		! Tipo di parametro
	  INTEGER  LO(0:MAXPAR-1)! Valore inferiore
	  INTEGER  HI(0:MAXPAR-1)! Valore superiore
	  INTEGER  FL		! Minfold
	  INTEGER  FH		! Minfold
	  logical*1  SAME		! Same as before window
	endstructure

	structure/banana/
	  INTEGER IND(2)	! Tipo di parametro x,y
	  INTEGER PAR(2)	! Parametro asse x,y
	  INTEGER RES(2)	! Risoluzione della banana
	  INTEGER NBAN		! Numero di banane
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 INSIDE	! In-Out
	  logical*1 SAME		! Same as before banana
	  logical*1 multiadc      ! N.M. - for ADC-dependent banana
	  logical*1 ignore(0:MAXDET-1) ! only for ADC-dependent banana
	endstructure

	structure/bananadat/
	  character*72 file
	  integer ban(2,0:1)
	endstructure

	structure/bananasdat/
	  character*72 file(MAXNBAN)
	  integer ban(2,0:1)
	endstructure

	structure/pairgate/
	   integer IND
	   integer LO
	   integer HI
	   integer FL
	   integer FH
	   integer PAR
	   integer RES
	   integer*2 PVAL(0:MAXDET-1,0:MAXDET-1)
	   integer*2 LIST(0:MAXDET-1)
	   character*72 filename
	   logical*1 INSIDE
	   logical*1 SAME
	endstructure

	structure/pin/
	  INTEGER IND(4)	! Tipo di parametro per banane
	  INTEGER PAR(4)	! Parametro asse x,y ; la massa e il tipo della particella
	  INTEGER RES(2)	! Risoluzione delle banane
	  INTEGER FIX		! Parametro fisso per il risultato
	  INTEGER FIXRES        ! Risoluzione dell parametro fisso
	  INTEGER NBAN		! quante banane
	  INTEGER F1(MAXNBAN)	! Numero di particelle della banana
	  INTEGER F2(MAXNBAN)	! Peso della banana in PIN
	  INTEGER F3(MAXNBAN)	! Massa della particella
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before PIN
	  logical*1 multiadc(MAXNBAN)          ! N.M. - for ADC-dependent banana
	  logical*1 ignore(MAXNBAN,0:MAXDET-1) ! only for ADC-dependent banana
	endstructure

	structure/hk/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER FIXH		! Parametro fisso per H
	  INTEGER FIXk		! Parametro fisso per k
	  REAL	  OFFS		! Offset per H
	  REAL	  GAIN		! Guadagno finale per H
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	endstructure

	structure/recal/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 RUN		! coefficenti run-dependent
	  logical*1 SAME	! Same as before recal
	  logical*1 ROUND       ! Round the result to integer value
	  logical*1 ZERO        ! Set negative result to zero
	  CHARACTER*73 FILE	! files dei coefficenti
	endstructure

	structure/recal_choose/
	  INTEGER IND(3)	! Tipo di parametro
	  INTEGER PAR(3)	! Quale parametro
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL    VAL           ! Valore di riferimento per fare la scelta
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  CHARACTER*73 FILE1	! files dei coefficenti
	  CHARACTER*73 FILE2	! files dei coefficenti
	  logical*1 RUN		! coefficenti run-dependent
	  logical*1 SAME	! Same as before recal
	  logical*1 ROUND       ! Round the result to integer value
	endstructure

	structure/calcoef/
	  INTEGER*4 ORD
	  REAL*4    COEF(MAXRCL_ORD)
	endstructure

	structure/mcalcoef/
	  integer nregs
	  INTEGER ORD(MAXRCL_REGS)
	  REAL    COEF(MAXRCL_ORD,MAXRCL_REGS)
	  real    limit(0:MAXRCL_REGS)
	endstructure


	structure/doppler/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET		! Quanti rivelatori
         real*8  fact1
         real*8  fact2
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL	  VC0		! Velocita' di rinculo
	  real    rtheta
	  real    rphi
	  REAL	  E0		! VC0 fino a E0
	  REAL	  E1		! Da VC0 a VC1 tra E0 e E1
	  REAL	  VC1		! VC1 sopra E1
	  REAL	  SLOPE		! Slope tra E0 e E1
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
         logical*1 RefChanged
	  logical*1 CONST		! Costante
	  logical*1 SAME		! Same as before recal
	  CHARACTER*73 FILE	! file con gli angoli
	endstructure

	structure/dopplerdat/
	  real costheta(0:1)
	endstructure
	
	structure/polar/
         real*8  fact1
         real*8  fact2
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET		! Quanti rivelatori
          INTEGER DTHETA        ! Angolo THETA del rivelatore, se viene dato come parametro
          INTEGER DPHI          ! Angolo PHI del rivelatore, se viene dato come parametro
	  INTEGER IND_POLAR
	  INTEGER PTHETA
	  INTEGER PPHI
         INTEGER IND_VEL
         INTEGER PVEL
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  REAL	  VC0		! Velocita' di rinculo
	  REAL	  E0		! VC0 fino a E0
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
          logical*1 FIXED
	  logical*1 VarDetAngle	! Se angoli degli rivelatori sono parametri
	  logical*1 SAME		! Same as before recal
	  CHARACTER*73 FILE	! file con gli angoli
	endstructure
	
	structure/polardat/
	  real cdir(3,0:MAXDET-1)
	endstructure

	structure/meanvalstr/
	  INTEGER IND1(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR1(MAXTADJ)	! Quale parametro
	  INTEGER RES1(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY1	! Quanti definiti
	  REAL    POSITION	! Posizione finale
	  REAL	  FACTOR	! Rejection factor
	  INTEGER IND2	! FIX only
	  INTEGER PAR2	! Quale parametro fisso
	  INTEGER RES2	! Risoluzione
	endstructure

	structure/tadjust/
	  INTEGER IND1(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR1(MAXTADJ)	! Quale parametro
	  INTEGER RES1(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY1	! Quanti definiti
	  REAL    POSITION	! Posizione finale
	  REAL	  FACTOR	! Rejection factor
	  INTEGER IND2(MAXTADJ)	! Tipo di parametro
	  INTEGER PAR2(MAXTADJ)	! Quale parametro
	  INTEGER RES2(MAXTADJ)	! Risoluzione
	  INTEGER HOWMANY2	! Quanti definiti
	endstructure

	structure/tref/
	   integer*8 N
# 285

	   real*16 SQSUM

	   INTEGER IND
	   INTEGER PAR
	   INTEGER RES
	   INTEGER REFNO
	   REAL POSITION
	endstructure

	structure/kine/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Parametro
	  INTEGER RES		! Risoluzione del parametro
	  INTEGER NDET_GE	! quanti germani
	  INTEGER NDET_SI	! quanti silici
	  INTEGER BIND(2)	! Tipo di parametro per banane
	  INTEGER BPAR(2)	! Parametro asse x,y
	  INTEGER BRES(2)	! Risoluzione della banana
	  INTEGER NBAN		! Numero di banane
	  INTEGER PINFIX	! Fixpar per PIN (se >=0)
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before recal
	  CHARACTER*72 KINEFILE	! file di descrizione
	  CHARACTER*72 AFILE_GE	! angoli germanio
	  CHARACTER*72 AFILE_SI	! angoli silici
	endstructure

	structure/kinedat/
	  REAL    ACMP,ECMP		! il nucleo composto
	  REAL    XMOMCM		! suo momento
	  REAL    AD(MAXNBAN)		! massa della particella
	  INTEGER MDET(MAXNBAN)		! numero di rivelatori attivi
	  INTEGER F1(MAXNBAN)		! Numero di particelle della banana per PIN
	  INTEGER F2(MAXNBAN)		! Peso della banana per PIN
	  real mom_si(0:MAXDET-1,MAXNBAN)
	  real cdir_ge(3,0:MAXDET-1)
	  real cdir_si(3,0:MAXDET-1)
	  record/bananasdat/kban
	endstructure

	structure/kinenew/
	  INTEGER IND(3)		! Tipo di parametro
	  INTEGER PAR(3)		! Parametro
	  INTEGER RES(3)		! Risoluzione del parametro
	  INTEGER NDET_GE	! quanti germani
	  INTEGER NDET_SI	! quanti silici
	  real ecal
	  real acmp
	  real ecmp
	  real atenuation
	  REAL	  OFFS		! Offset
	  REAL	  GAIN		! Guadagno finale
	  INTEGER WLO		! Finestra sul finale
	  INTEGER WHI		! Finestra sul finale
	  INTEGER FL		! Minfold
	  INTEGER FH		! Maxfold
	  logical*1 SAME		! Same as before recal
	  CHARACTER*72 AFILE_GE	! angoli germanio
	  CHARACTER*72 AFILE_SI	! angoli silici
	  character*73 except_file
	  logical*1 except
	endstructure

	structure/kinenewdat/
	  REAL    ACMP,ECMP		! il nucleo composto
	  REAL    XMOMCM		! suo momento
	  real    ecal
	  real    afac
	  real cdir_ge(3,0:MAXDET-1)
	  real cdir_si(3,0:MAXDET-1)
	  integer nregs
	  integer reg(MAXRES,2)
	endstructure

	structure/sltocm/
	  integer IND(2)
	  integer PAR(2)
	  integer NDET_SI
	  real F(2)
	  real COS_TH(0:MAXDET-1)
	  CHARACTER*72 AFILE_SI	! angoli silici
	endstructure


	structure/add/
	  INTEGER IND(3)	! Tipo di parametro
	  INTEGER PAR(3)	! Parametro 1+2==>3
	  REAL	  FAC(3)	! fattori moltiplicativi
	  REAL    OFFSET	! offset da sommare al risultato
	  REAL    GAIN		! gain sul risultato (non sul'offset)
	  INTEGER ICHAN		! canale limite per COMBINE
	  INTEGER DELTA		! Sliding range
	  INTEGER SLIDE		! Sliding value
	  logical*1 CHECK		! check che il secondo parametro sia > limit
	  logical*1 MULT		! fattori moltiplicativi?
	endstructure

	structure/kill/
	  INTEGER IND			! Tipo di parametro
	  logical*1 RUN			! run dependent selective kill
	  CHARACTER*72 FILE		! files contenente i detbad
	  logical*1 DET(0:MAXDET-1)	! Quali rivelatori
	endstructure

	structure/killdat/
	  logical*1 remove(0:MAXDET-1,0:MAXDET-1)
	endstructure

	structure/select/
	  INTEGER IND		! Tipo di parametro
	  logical*1 DET(0:MAXDET-1)  ! Quali rivelatori
	endstructure

	structure/listev/
	  logical*1 TOFILE	! list events on file
	  logical*1 ISOPEN
	  INTEGER LUN
	  CHARACTER*72 FILE
	endstructure

	structure/reorder/
	  INTEGER IND		! Tipo di parametro
	endstructure

	structure/statistics/
	  INTEGER WHICH
	endstructure

	structure/statdat/
	  INTEGER FOLD(0:MAXDET,0:NDTYPE,0:1)
	endstructure

	structure/swap/
	  INTEGER IND(2)	! Tipo di parametro x,y
	  INTEGER PAR(2)	! Parametro asse x,y
	endstructure

	structure/mask/
	  INTEGER IND		! Tipo di parametro x
	  INTEGER PAR		! Parametro asse x
	  INTEGER MASK		! Maschera
	  logical*1 ONE		! solo un parametro o tutti?
	endstructure

	structure/useful/
	  INTEGER IND		! Tipo di parametro (deve essere 0)
	  INTEGER PAR1		! Parametro per RUN#
	  INTEGER PAR2		! Parametro per REC#
	  INTEGER PAR3		! Parametro per EVCOUNT#
	  INTEGER PAR4		! Parametro per EVNUMBER#
	endstructure

	structure/move/
	  INTEGER IND1			! da quale tipo
	  INTEGER IND2			! a  quale tipo
	  INTEGER PAR1			! da quale parametro
	  INTEGER PAR2			! a  quale parametro
	  INTEGER OFFSET                ! offset alla nuova numerazione
	  logical*1 CONDITION		! TRUE if GATE or copy detector
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
c	  logical*1 INSIDE	! dentro/fuori
	  INTEGER LO		! Estremo inferiore
	  INTEGER HI		! Estremo superiore
c	  INTEGER NGATES	! Quanti intervalli
c	  INTEGER FL		! Minfold
c	  INTEGER FH		! Maxfold
c	  logical*1 SAME		! Same as before gate
	  INTEGER HOWMANY		! Howmany to move
	  INTEGER WHICH(0:MAXDET-1)	! Quali rivelatori
	endstructure

	structure/splitmerge/
	  INTEGER NIND			! quanti tipi di rivelatore coinvolti
	  INTEGER IND(0:NDTYPE)		! Quali rivelatori
	  logical*1 REMOVE(0:NDTYPE)	!
	  INTEGER OFFSET(0:NDTYPE)	! Offset degli indici
	endstructure

	structure/newid/
	  INTEGER IND			! tipo di rivelatore
	  INTEGER LUT(0:MAXDET-1)	! mappa dei nuovi indici
	  logical*1 REORDER		! reordina dopo la mappatura
	  CHARACTER*72 FILE		! files dei nuovi id
	endstructure

	structure/addback/
	  INTEGER IND			! Tipo di parametro
	  INTEGER PAR			! Quale parametro
	  INTEGER RES			! Risoluzione del parametro
	  INTEGER NSEGS			! quanti segmenti (se composito)
	  logical*1 REJECT		! elimina le doppie non contigue
	  logical*1 PATTERN		! produce la hit-pattern sommando tutto
	  INTEGER PATPAR		! dove la registra
	  logical*1 TGATE			! verifica la relazione temporale?
	  INTEGER TPAR			! su quale parametro
	  INTEGER TVAL			! massima deviazione
	endstructure

        structure /pms_str/
	    real*8  sin_70
	    real*8  sin_110
	    real*8  cos_20
	    real*8  tan_20
	    integer ind
	    integer par_r
	    integer par_p
	    integer par_e
	    integer par_icp
	    integer ind_theta
	    integer par_theta
	    integer ind_path
	    integer par_path
	    integer ind_x
	    integer par_x
	    integer ind_ic
	    integer par_a
	    integer par_b
	    integer par_c
	    integer par_d
	    real    thr
	    real    qfact
	end structure
	
	structure /pms_dat/
	   real*8 R(40,0:100)
	end structure
	 
	structure /qvalue_str/
	   real*8  costhp
	   real*8  sinthp
	   real*8  amu
	   integer ind_q
	   integer par_q
	   integer ind_mass
	   integer par_mass
	   integer ind_theta
	   integer par_theta
	   integer ind_phi
	   integer par_phi
	   integer ind_beta
	   integer par_beta
	   integer AP
	   integer AT
	   integer ATOT
	   integer    low
	   integer    high
	   real    ep
	   real    thp
	   real    gain
	   real    offset
	end structure

       structure /prisma_angles_str/
          real*8  prisma_angle
          real*8  D
          real*8  sinalpha
          real*8  cosalpha
          real*8  costhp
          real*8  sinthp
	  real*8  cx(0:3)
	  real*8  cy(0:3)
          integer ind
          integer parx
          integer pary
          integer parz
          integer parq
          integer part
	  integer parp
	  integer pard
	  integer degx
	  integer degy
	  integer oldstyle
	  character*128 calfile
       end structure
	   
	structure/proje/
	  INTEGER LEN		! Lunghezza totale degli spettri (LW)
	  INTEGER MFRES 	! risoluzione max. di F
	  INTEGER*8 NINCR
	endstructure

	structure/projedat/
	  CHARACTER*16 NAME(0:MAXPAR-1,0:NDTYPE)
	  INTEGER      ADDR(0:MAXDET-1,0:MAXPAR-1,0:NDTYPE)
	  INTEGER      SPEC(0:1)
	endstructure

	structure/spectrum/
	  INTEGER IND			! Tipo di parametro
	  INTEGER PAR			! Quale parametri
	  INTEGER RES			! numero di canali dello spettro
	  INTEGER IHASH			! Quale tabella di hash (se >=0)
	  INTEGER HIND,HPAR		! riportati qui per comodita'
	  logical*1 HSAME			! IND e' quello su cui applicare HGATE
	  INTEGER NHASH			! Quante volte
	  logical*1 SPHERICAL		! gate di tipo elissoidale
	  INTEGER MINFOLD		! fold minimo (per hsort1d)
	  INTEGER*8 NINCR		! numero di incrementi
	  CHARACTER*72 FILE		! Filename dello spettro
	endstructure

	structure/specdat/
	  INTEGER SPEC(0:1)
	endstructure

	structure/matrix/

	  integer*8 CMTBASE

	  INTEGER NDIM			! Ordine della matrice
	  INTEGER IND(MAXMATDIM)	! Tipo di parametri per i due lati
	  INTEGER PAR(MAXMATDIM)	! Quale dei parametri
	  INTEGER RES(MAXMATDIM)	! Dimensioni lati della matrice
	  INTEGER STEP(MAXMATDIM)	! suddivisione lati della matrice
	  INTEGER MATMODE		! 0=normal 1=symmetric 2=halfsymmetric
	  INTEGER MSDIM			! numero di indici simmetrizzati
	  INTEGER DOFFSET		! per SORTxD_DIFF
	  INTEGER NIND
	  INTEGER IHASH			! Quale tabella di hash (se >=0)
	  INTEGER HIND,HPAR		! riportati qui per comodita'
	  logical*1 HSAME			! IND e' quello su cui applicare HGATE
	  INTEGER NHASH			! quante volte
	  logical*1 SPHERICAL		! gate di tipo elissoidale
	  INTEGER TYPE
	  INTEGER IND1,IND2,IND3,IND4
	  INTEGER PAR1,PAR2,PAR3,PAR4
	  logical*1 HHH1,HHH2,HHH3,HHH4
	  CHARACTER*72 FILE		! Filename matrice
	endstructure

	structure/iodef/
	  logical*1 datafile	! TRUE se da file o Virtuale
	  logical*1 virtuale
	  logical*1 closed
	  integer lun
	  integer flag
	  integer wlen
	  integer run
	  integer rec
	  integer maxrec
	  integer evcount	! internal count
	  integer evnumber	! from ACQ
	  integer vrun,vevents
CVMS	  integer*2 iosb(4)
	  integer iosb(4)
CVMS	  integer*2 mtch,hctm
	  integer mtch
	  character*72 name
	endstructure

	structure/oevdef/
	  logical*1 defined
	  logical*1 same				! Same FILE structure on output
	  logical*1 sameRUN			! Same RUN number on output
	  logical*1 ask				! Will ask every EOF
	  logical*1 reduce			! Output events in forma ridotta
	  logical*1 det(0:NDTYPE)			! Output this detector
	  logical*1 par(0:MAXPAR-1,0:NDTYPE)  	! Output this parameter
	  integer evlen,pointer
	  logical*1 done
	endstructure

	structure/commands/
	  union
	    map
	      record/recall/recall
	    endmap
	    map
	      record/fold/fold
	    endmap
	    map
	      record/gate/gate
	    endmap
	    map
	      record/filter/filter
	    endmap
	    map
	      record/window/win
	    endmap
	    map
	      record/pairgate/pgate
	    endmap
	    map
	      record/banana/ban
	    endmap
	    map
	      record/pin/pin
	    endmap
	    map
	      record/recal/rcl
	    endmap
	    map
	      record/kine/kine
	    endmap
	    map
	      record/doppler/doppl
	    endmap
	    map
	      record/tadjust/tadj
	    endmap
	    map
	      record/tref/tref
	    endmap
	    map
	      record/hk/hk
	    endmap
	    map
	      record/add/add
	    endmap
	    map
	      record/kill/kill
	    endmap
	    map
	      record/select/sel
	    endmap
	    map
	      record/listev/lev
	    endmap
	    map
	      record/reorder/reo
	    endmap
	    map
	      record/statistics/stat
	    endmap
	    map
	      record/swap/swap
	    endmap
	    map
	      record/mask/mask
	    endmap
	    map
	      record/useful/useful
	    endmap
	    map
	      record/move/move
	    endmap
	    map
	      record/newid/newid
	    endmap
	    map
	      record/splitmerge/splmrg
	    endmap
	    map
	      record/polar/dpolar
	    endmap
	    map
	      record/pms_str/pms
	    endmap
	    map
	      record/qvalue_str/qvalue
	    endmap
	    map
	      record /prisma_angles_str/ang
	    endmap
	    map
	      record/addback/abck
	    endmap
	    map
	      record/proje/pro
	    endmap
	    map
	      record/spectrum/spec
	    endmap
	    map
	      record/matrix/mat
	    endmap
	  endunion

	  integer*8 addr

	  integer size
	  integer whichcom
	endstructure

	structure /tndio/
	  integer nfiles
	  integer current
	  logical*1 presorted
	end structure
# 2 "./gsort.inc" 2 

	INTEGER	  LLUN
	INTEGER	  LLIST
	INTEGER   GS2LUN
	INTEGER   LGSDIR
	CHARACTER GSDIR*64
	CHARACTER LIST*256
	CHARACTER LINE*256
	CHARACTER COMSORT*40
	integer INFILES
	COMMON /VARIECOM/ LLUN,LLIST,GS2LUN,LGSDIR,GSDIR,LIST,LINE,COMSORT,INFILES

	logical*1 BREAK
	logical*1 KILLEV
	logical*1 AUTONUMBER	! File etichettati con RUN#
	logical*1 TAPEDISMOUNT	! Controllo fine nastro
	logical*1 LASTTAPE	! Controllo fine nastro
	logical*1 AUTOLOADER
	logical*1 STORED_EV
	logical*1 DEFINED_OUT,FINISHED_OUT
	COMMON/FLAGS/ BREAK,KILLEV,AUTONUMBER,TAPEDISMOUNT,LASTTAPE,AUTOLOADER,STORED_EV,
     1 DEFINED_OUT,FINISHED_OUT

	logical*1 SYNTAX
	INTEGER PHASE
	CHARACTER COMLINE*256
	INTEGER LCOMLINE
	INTEGER INPLU
	CHARACTER*128  FORM(NFORMCOM)
	CHARACTER*128  DECL(NDECLCOM)
	CHARACTER*128  ANAL(NANALCOM)
	CHARACTER*128 SYNT(NSYNT)
	COMMON /COMDEFS/ PHASE,COMLINE,LCOMLINE,INPLU,FORM,DECL,ANAL,SYNT,SYNTAX

	RECORD /COMMANDS/COM(MAXCOMANDI)
	INTEGER	NCOMANDI
	INTEGER ICMD
	INTEGER WHICHCOM
	INTEGER INIT_MODE,FINIT_MODE
	COMMON /COMMAND/ COM,NCOMANDI,ICMD,WHICHCOM,INIT_MODE,FINIT_MODE

	INTEGER   DATAFORMAT			! GASP/EUROBALL/8PI
	INTEGER   GASPMAP(0:NDTYPE)		! Mappatura per formato GASP
	INTEGER   EUROMAP(0:NDTYPE)		! Mappatura per formato euroball
	INTEGER   ISEED				! Seed per i numeri random
	INTEGER   NTIPI				! quanti tipi di rivelatori
	INTEGER   NDPAR(0:NDTYPE)		! # parametri
	INTEGER   MDPAR(0:NDTYPE)		! # parametri aggiunti
	INTEGER   TDPAR(0:NDTYPE)		! # parametri totali
	INTEGER   PARRES(0:MAXPAR-1,0:NDTYPE)  	! Risoluzione dei vari parametri

	INTEGER   NDETS(0:NDTYPE)		! # di rivelatori (se composito)
	INTEGER   NSEGS(0:NDTYPE)		! # segmenti (se composito)
	INTEGER   NCPAR(0:NDTYPE)		! # di parametri comuni (se composito)
	
	INTEGER   NITEMS(0:NDTYPE)		! # totale di rivelatori
	logical*1   EXISTS(0:NDTYPE)		! Esiste il tipo di rivelatore
	INTEGER   FOLDMIN(0:NDTYPE)		! Minimo fold da nastro
	CHARACTER*1 DNAME(0:NDTYPE)		! simboli per rivelatori
	COMMON /EVDEF/DATAFORMAT,GASPMAP,EUROMAP,ISEED,NTIPI,NDPAR,MDPAR,
     1TDPAR,PARRES,NDETS,NSEGS,NCPAR,NITEMS,FOLDMIN,EXISTS,DNAME

	INTEGER   CLASS,TAG			! Descrizione dell'evento
	INTEGER   NDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   DOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/DET(0:MAXDET-1)		! i dati
	COMMON /CEVENT/  CLASS,TAG,NDET,DOFF,DET

	INTEGER   sCLASS,sTAG			! Copia dell'evento per SAVE/RECALL
	INTEGER   sNDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   sDOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/sDET(0:MAXDET-1)	! i dati
	COMMON /SEVENT/  sCLASS,sTAG,sNDET,sDOFF,sDET

	INTEGER   mCLASS,mTAG			! Copia di lavoro (Move...)
	INTEGER   mNDET(0:NDTYPE)		! # rivelatori nell' evento
	INTEGER   mDOFF(0:NDTYPE)		! offset in LVAL dei vari tipi
	RECORD/DETECTOR/mDET(0:MAXDET-1)	! i dati
	COMMON /mEVENT/  mCLASS,mTAG,mNDET,mDOFF,mDET


	integer*8 HASHADDR(0:MAXHASH-1)

	INTEGER HASHGIVEN
	INTEGER HNTRUE,HSTRUE			! Quanti rivelatori in gate
	logical*1 HLTRUE(0:MAXDET-1)		! Quali rivelatori in gate
	REAL    HDIST1(0:MAXDET-1)		! per gestione gates sferici
	REAL    HDIST2(0:MAXDET-1)		! per gestione gates sferici
	COMMON /HASHDEF/ HASHADDR,HASHGIVEN,HNTRUE,HSTRUE,HLTRUE,HDIST1,HDIST2


	integer*8 PAIRSADDR(0:MAXPAIRS-1)

	INTEGER PAIRSGIVEN
	COMMON /PAIRSDEF/ PAIRSADDR,PAIRSGIVEN


	integer*8 STATADDR

	INTEGER STATSIZE
	INTEGER NSTATCOM			! quanti comandi di statistica
	COMMON /STATISTICS/ STATADDR,STATSIZE,NSTATCOM

	INTEGER NLOST,NCLASS
	INTEGER*8 NINCR(MAXMATDIM)
	INTEGER FLUSHED
	INTEGER NPRINT
	COMMON /STATUS/ NLOST,NCLASS,NINCR,FLUSHED,NPRINT

	integer*2 ievbuf(16*MAXWORDS)	! Buffer dati
	integer*2 ievheader(MAXWORDS)	! header record
	record/iodef/iio
	common/ievcom/ievheader,ievbuf,iio

	record/oevdef/oev
	integer*2 oevent(MAXEVL*4)	! evento da scrivere
	integer*2 oevbuf(MAXBYTES/2)	! Buffer scrittura eventi
	record/iodef/oio
	common/oevcom/oev,oevent,oevbuf,oio

	record /tndio/tnd
	common /tndcom/tnd
# 2637 "gsort.F" 2 

	do ii=1,MAXMATDIM
	  nincr(ii)=0
	enddo
	flushed=0
	if(mode.eq.0) then		! only show_status
	  finit_mode=0
	elseif(mode.eq.1) then		! end of a run
	  finit_mode=1
	  call gs_print_statistics(LU1,%val(stataddr))
	  call gs_print_statistics(LU2,%val(stataddr))
	elseif(mode.eq.2) then		! the very end
	  finit_mode=2
	else
	  return
	endif
	FINISHED_OUT=.false.

	do icmd=1,ncomandi
	  whichcom=com(icmd).whichcom

	goto(	    301,302,303,304,305,306,307,308,309,
     1	310,311,312,313,314,315,316,317,318,319,
     2	320,321,322,323,324,325,326,327,328,329,
     3	330,331,332,333,334,335,336,337,338,339,
     4	340,341,342,343,344,345,346,347,348,349,
     5	350,351,352,353,354,355,356,357,358,359,
     6	360,361,362,363,364,365,366,367,368,369,
     7	370,371,372,373,374,375,376,377,378,379,
     8	380,381,382,383,384,385,386,387,388,389,
     9	390,391,392,393,394,395,396,397,398,399,
     1	400,401,402,403,404,405,406,407,408,409,
     1	410,411,412,413,414,415,416,417,418,419,
     1	420,421,422,423,424,425,426,427,428,429,
     1	430,431,432,433,434,435,436,437,438,439,
     1	440,441,442,443,444,445,446,447,448,449) whichcom

	  call ansi_bell(6)
	  stop 'Inconsistent command in FINIT_RUN'

301	  call gs4_fold(com(icmd),%val(com(icmd).addr))
	  goto 800

302	  call gs4_gate(com(icmd),%val(com(icmd).addr))
	  goto 800

303	  call gs4_gates(com(icmd),%val(com(icmd).addr))
	  goto 800

304	  call gs4_window(com(icmd),%val(com(icmd).addr))
	  goto 800

305	  call gs4_banana(com(icmd),%val(com(icmd).addr))
	  goto 800

306	  call gs4_bananas(com(icmd),%val(com(icmd).addr))
	  goto 800

307	  call gs4_filter(com(icmd),%val(com(icmd).addr))
	  goto 800

308	  call gs4_xban(com(icmd),%val(com(icmd).addr))
	  goto 800

309	  goto 900
310	  call gs4_recal_mult(com(icmd),%val(com(icmd).addr))
	  goto 800

311	  call gs4_gain(com(icmd),%val(com(icmd).addr))
	  goto 800

312	  call gs4_recal(com(icmd),%val(com(icmd).addr))
	  goto 800

313	  call gs4_recal_lut(com(icmd),%val(com(icmd).addr))
	  goto 800

314	  call gs4_time_reference(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
315	  call gs4_timing(com(icmd),%val(com(icmd).addr))
	  goto 800

316	  call gs4_recal_doppler(com(icmd),%val(com(icmd).addr))
	  goto 800

317	  call gs4_recal_kinenew(com(icmd),%val(com(icmd).addr))
	  goto 800

318	  call gs4_time_adjust(com(icmd),%val(com(icmd).addr))
	  goto 800

319	  call gs4_pin(com(icmd),%val(com(icmd).addr))
	  goto 800

320	  call gs4_hk(com(icmd),%val(com(icmd).addr))
	  goto 800

321	  call gs4_add(com(icmd),%val(com(icmd).addr))
	  goto 800

322	  call gs4_kill(com(icmd),%val(com(icmd).addr))
	  goto 800

323	  call gs4_select(com(icmd),%val(com(icmd).addr))
	  goto 800

324	  call gs4_store_event(com(icmd),%val(com(icmd).addr))
	  goto 800

325	  call gs4_recall_event(com(icmd),%val(com(icmd).addr))
	  goto 800

326	  call gs4_write_event(com(icmd),%val(com(icmd).addr))
	  goto 800

327	  call gs4_list_event(com(icmd),%val(com(icmd).addr))
	  goto 800

328	  call gs4_reorder(com(icmd),%val(com(icmd).addr))
	  goto 800

329	  call gs4_statistics(com(icmd),%val(com(icmd).addr))
	  goto 800

330	  call gs4_swap(com(icmd),%val(com(icmd).addr))
	  goto 800

331	  call gs4_mask(com(icmd),%val(com(icmd).addr))
	  goto 800

332	  call gs4_move(com(icmd),%val(com(icmd).addr))
	  goto 800

333	  call gs4_merge(com(icmd),%val(com(icmd).addr))
	  goto 800

334	  call gs4_split(com(icmd),%val(com(icmd).addr))
	  goto 800

335	  call gs4_newid(com(icmd),%val(com(icmd).addr))
	  goto 800

336	  call gs4_ebkill(com(icmd),%val(com(icmd).addr))
	  goto 800

337	  call gs4_combine(com(icmd),%val(com(icmd).addr))
	  goto 800

338	  call gs4_addback(com(icmd),%val(com(icmd).addr))
	  goto 800

339	  call gs4_copy(com(icmd),%val(com(icmd).addr))
	  goto 800

340	  call gs4_useful(com(icmd),%val(com(icmd).addr))
	  goto 800

341	  call gs4_thresholds(com(icmd),%val(com(icmd).addr))
	  goto 800

342	  goto 900
343	  goto 900

344	  call gs4_mult(com(icmd),%val(com(icmd).addr))
	  goto 800

345	  call gs4_mean_value(com(icmd),%val(com(icmd).addr))
	  goto 800

346	  call gs4_div(com(icmd),%val(com(icmd).addr))
	  goto 800

347	  call gs4_SLtoCM(com(icmd),%val(com(icmd).addr))
	  goto 800

348	  call gs4_recal_choose(com(icmd),%val(com(icmd).addr))
	  goto 800
	  
349	  call gs4_wmean_value(com(icmd),%val(com(icmd).addr))
	  goto 800

350	  call gs4_recal_polar(com(icmd),%val(com(icmd).addr))
	  goto 800

351	  goto 900
352	  goto 900
353	  goto 900

354	  call gs4_angles_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

355	  call gs4_track_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

356	  call gs4_qvalue_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

357	  call gs4_bpvelocity_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

358	  call gs4_xfp_pms(com(icmd),%val(com(icmd).addr))
	  goto 800

359	  goto 900
360	  goto 900
361	  goto 900
362	  goto 900
363	  goto 900
364	  goto 900
365	  goto 900
366	  goto 900
367	  goto 900
368	  goto 900
369	  goto 900
370	  goto 900
371	  goto 900
372	  goto 900
373	  goto 900

374	  call gs4_break
	  goto 800
375	  call gs4_pairgate(com(icmd),%val(com(icmd).addr))
	  goto 800

376	  call gs4_pow(com(icmd),%val(com(icmd).addr))
	  goto 800

377	  call gs4_pairkill(com(icmd),%val(com(icmd).addr))
	  goto 800

378	  goto 900
379	  goto 900
380	  goto 900
381	  goto 900
382	  goto 900
383	  goto 900
384	  goto 900
385	  goto 900
386	  goto 900
387	  goto 900
388	  goto 900
389	  goto 900
390	  goto 900

391	  call usersub1(4)
	  goto 800

392	  call usersub2(4)
	  goto 800

393	  call usersub3(4)
	  goto 800

394	  call usersub4(4)
	  goto 800

395	  call usersub5(4)
	  goto 800

396	  call usersub6(4)
	  goto 800

397	  call usersub7(4)
	  goto 800

398	  call usersub8(4)
	  goto 800

399	  call usersub9(4)
	  goto 800

400	goto 900

401	call gs4_projections(com(icmd), %VAL(com(icmd).addr))
	goto 800

402	call gs4_sort1d(com(icmd), %VAL(com(icmd).addr))
	goto 800

403	call gs4_sort1d_m(com(icmd), %VAL(com(icmd).addr))
	goto 800

404	goto 900
405	goto 900
406	goto 900
407	goto 900
408	goto 900
409	goto 900
410	goto 900
	
411	call gs4_sort2d(com(icmd), %VAL(com(icmd).addr))
	goto 800

412	call gs4_sort3d(com(icmd), %VAL(com(icmd).addr))
	goto 800

413	call gs4_sort4d(com(icmd), %VAL(com(icmd).addr))
	goto 800

414	goto 900
415	goto 900

416	call gs4_sort2d_ab(com(icmd), %VAL(com(icmd).addr))
	goto 800

417	goto 900
418	goto 900
419	goto 900
420	goto 900	

421	call gs4_sort2d_symm(com(icmd), %VAL(com(icmd).addr))
	goto 800

422	call gs4_sort3d_symm(com(icmd), %VAL(com(icmd).addr))
	goto 800

423	call gs4_sort4d_symm(com(icmd), %VAL(com(icmd).addr))
	goto 800

424	goto 900

425	call gs4_sort3d_pair(com(icmd), %VAL(com(icmd).addr))
	goto 800

426	goto 900

427	call gs4_sort3d_diff(com(icmd), %VAL(com(icmd).addr))
	goto 800

428	goto 900
429	goto 900
430	goto 900

431	call gs4_sort2d_hsymm(com(icmd), %VAL(com(icmd).addr))
	goto 800

432	call gs4_sort3d_hsymm(com(icmd), %VAL(com(icmd).addr))
	goto 800

433	call gs4_sort4d_hsymm(com(icmd), %VAL(com(icmd).addr))
	goto 800

434	goto 900
435	goto 900
436	goto 900
437	goto 900
438	goto 900
439	goto 900
440	goto 900
441	goto 900
442	goto 900
443	goto 900
444	goto 900
445	goto 900
446	goto 900
447	goto 900
448	goto 900
449	goto 900

800	ENDDO

	return

900	stop 'Unknown command'

	END
	
