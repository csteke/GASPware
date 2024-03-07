# 1 "gsortuser2.F"
       subroutine usersub2(flag)

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
# 4 "gsortuser2.F" 2 

       integer flag
       
       integer ind, par1, par2, par3, par4, par5
       save  par1, par2, par3, par4, par5
       
       integer ind1, ind2, ind3, ind4, ind5, res
       save ind1, ind2, ind3, ind4, ind5
       
       character*256 OutFile
       save OutFile

       real*8  ux, uy, uz, theta, phi, energy


       if(flag.eq.0) then		       ! from GETINPUT
        synt(1)='USERSUB2 Dt Dp Rv Pa Pz output_file'

        call gs_getindpar(ind1,par1,res)
	 if( ind1 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind2,par2,res)
	 if( ind2 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind3,par3,res)
	 if( ind3 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind4,par4,res)
	 if( ind4 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind5,par5,res)
	 if( ind5 .lt. 0 ) call gs_syntax('Detector parameter expected here')

	 ind = ind1

        if( ind1 .ne. ind2 )then
	   call gs_syntax('Theta and Phi parameters must be in the same detector')
	endif
	
	if( ind4 .ne. ind5 )then
	  call gs_syntax('A and Z parameters must be in the same detector')
	endif

	if( par1 .eq. par2 ) call gs_syntax('Theta/Phi parameters must be different') 
	if( par4 .eq. par5 ) call gs_syntax('A/Z parameters must be different') 

        call istr_ch(comline,OutFile)
	if(lengthc(OutFile).LT.1) call gs_syntax('Name for the output file expected here')
	open( 201, file=OutFile, status='unknown', ERR = 1)
	write(201,'(''FORMAT 1 4'',/,''EMITTED 1 1'')')
	return
1	call gs_syntax('Cannot create/overwrite the output file') 	

       elseif(flag.eq.1) then		       ! from LISTPROG
	 call gs_putindpar( ind1, par1 )
	 llist = llist+3
	 call gs_putindpar( ind2, par2 )
	 llist = llist+3
	 call gs_putindpar( ind3, par3 )
	 llist = llist+3
	 call gs_putindpar( ind4, par4 )
	 llist = llist+3
	 call gs_putindpar( ind5, par5 )
        llist=lengthc(list)+2
	ii1 = lengthc(OutFile)
        write(list(llist:),'(a)')OutFile(1:ii1)

       elseif(flag.eq.2) then		       ! from INIT_RUN
         return

       elseif(flag.eq.3) then		       ! from EVANA
         if( ndet( ind1 ) .ne. 1 )return
         if( ndet( ind3 ) .ne. 1 )return
         if( ndet( ind4 ) .ne. 1 )return
	 
	 write(201,'(''$'')')
	 
	 ii1 = doff(ind1)
	 ii2 = doff(ind3)
	 ii3 = doff(ind4)
	 
	 theta = det(ii1).xval(par1)/10.0D0
	 phi   = det(ii1).xval(par2)/10.0D0
	 
	 ux = dsind(theta)*dcosd(phi)
	 uy = dsind(theta)*dsind(phi)
	 uz = dcosd(theta)
	 
	 Energy = det(ii2).xval(par3)/100.000D0
	 Energy = energy*energy*465.750D0*det(ii3).ival(par4)
	 
	 write(201,'(3i4,f9.3,1x,3(1x,f10.7))')-101,det(ii3).ival(par5),det(ii3).ival(par4),Energy,ux,uy,uz
	 write(201,'(''1 egamma_'',i3.3,''_'',i3.3,'' 0. 0.'')')det(ii3).ival(par5),det(ii3).ival(par4)
	      
       elseif(flag.eq.4) then		       ! from FINIT_RUN
	 call flush(201)
       
       endif

       return

       end



* previous USERSUB2
*
*	subroutine usersub2(flag)
*
*#include "gsort.inc"
*
*	integer flag
*
*	PARAMETER (IGG=5)
*	PARAMETER (IPP=7)
*
*
*	if(flag.eq.0) then			! from GETINPUT
*c	  call gs_syntax('Please replace this Subroutine with your own version')
*	elseif(flag.eq.1) then			! from LISTPROG
*	  write(list(llist:),'(A)') 'Time difference with respect to '//dname(IPP)
*	elseif(flag.eq.2) then			! from INIT_RUN
*	elseif(flag.eq.3) then			! from EVANA
*	  if(ndet(ipp).ne.1) return
*	  tppac=det(doff(IPP)).xval(0)
*	  do ii=doff(IGG),doff(IGG)+ndet(IGG)-1
*	    tger=det(ii).xval(2)-tppac
*	    det(ii).xval(2)=tger
*	    det(ii).ival(2)=tger
*	  enddo
*	elseif(flag.eq.4) then			! from FINIT_RUN
*	endif
*
*	return
*
*	end
