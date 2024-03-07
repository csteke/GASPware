# 1 "gsortuser5.F"
	subroutine usersub5(flag)

	integer flag

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
# 6 "gsortuser5.F" 2 
# 1 "./polar.inc" 1 
	structure/addbpol/
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
	  INTEGER POLAR(0:MAXDET-1)     ! Polarization
	endstructure
# 7 "gsortuser5.F" 2 
	record/addbpol/abpl
	
	common/addbpol/abpl
	record/matrix/mat

	    if(flag.eq.0) then			! from GETINPUT
		call gs0_sortpol(mat)
	elseif(flag.eq.1) then			! from LISTPROG
		call gs1_sortpol(mat)
	elseif(flag.eq.2) then			! from INIT_RUN
		call gs2_sortpol(mat)
	elseif(flag.eq.3) then			! from EVANA
		call gs3_sortpol(mat)
	elseif(flag.eq.4) then			! from FINIT_RUN
		call gs4_sortpol(mat)
	endif

	return

	end
	
	subroutine gs0_sortpol(mat)

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
# 31 "gsortuser5.F" 2 
	record/matrix/mat
	
	synt(1)='usersub5  g0 g0 Pz  matrixname  [RES Rx Ry Rz] [STEP Sx Sy Sz] [Hn [#times]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=3
CCCC	call gs_getmatmode(mat.matmode,mat.msdim)

	do nn=1,mat.ndim
	  call gs_getindpar(mat.ind(nn),mat.par(nn),mat.res(nn))
	  mat.step(nn)=max(1,min(mat.res(nn),DEFSTEP3D))
	end do

c	mat.res(mat.ndim)=mat.res(mat.ndim)*2		! Last param*2 pol.
	
	call gs_checkorder(mat.ind,mat.par,mat.ndim,mat.nind)

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res,mat.step,mat.ndim)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	if(mat.ihash.GE.0) then
	  mat.hsame=.FALSE.
	  do ii=1,mat.ndim
	    if(mat.ind(ii).EQ.mat.hind) mat.hsame=.TRUE.
	  end do
	endif
	  mat.hsame=.FALSE. !  fix mat.hsame for polar

	mind1=mat.ind(1)
	mind2=mat.ind(2)
	mind3=mat.ind(3)
	mat.type=-1

	if(mat.nind .EQ. 1) then
	  call gs_countvals(mat.par(1),3,nvals)
	  if(nvals.eq.1) then
	    mat.type=1					! AaAaAa
	  elseif(nvals.eq.3) then
	    mat.type=2					! AaAbAc
	  endif

	elseif(mat.nind.EQ.2) then
	  if(mind1.EQ.mind2) then
	    call gs_countvals(mat.par(1),2,nvals)
	    if(nvals.eq.1) then
	      mat.type=3				! AaAaBx
	    elseif(nvals.eq.2) then
	      mat.type=4				! AaAbBx
	    endif
	  elseif(mind2.EQ.mind3) then
	    call gs_countvals(mat.par(2),2,nvals)
	    if(nvals.eq.1) then
	      mat.type=5				! AxBaBa
	    elseif(nvals.eq.2) then
	      mat.type=6				! AxBaBb
	    endif
	  endif

	elseif(mat.nind.EQ.3) then
	  mat.type=7					! AxByCz
	endif
	mat.type=3      ! AaAaBx Fix mat.type for pol.

	if(mat.type.le.0) call gs_syntax('Do not know how to treat this combination of parameters')

	mat.ind1=mat.ind(1)
	mat.ind2=mat.ind(2)
	mat.ind3=mat.ind(3)

	mat.par1=mat.par(1)
	mat.par2=mat.par(2)
	mat.par3=mat.par(3)

	mat.hhh1=mat.ind1 .EQ. mat.hind
	mat.hhh2=mat.ind2 .EQ. mat.hind
	mat.hhh3=mat.ind3 .EQ. mat.hind
	
	mat.matmode=0

	return

	end


	subroutine gs1_sortpol(mat)

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
# 126 "gsortuser5.F" 2 
	record/matrix/mat

	matndim=mat.ndim
	do nn=1,matndim
	  call gs_putindpar(mat.ind(nn),mat.par(nn))
	  llist=llist+2
	end do

	lfile=max(1,lengthc(mat.file))
	write(list(llist:),'(1x,a)') mat.file(1:lfile)

	llist=lengthc(list)+3
	call gs_putres(mat.res,matndim)

	llist=llist+3
	call gs_putstep(mat.step,matndim)

	llist=llist+3
	call gs_puthash(mat.nhash,mat.ihash)

CCCC	llist=llist+3
CCCC	write(list(llist:),'(''mm'',2i4)') mat.matmode,mat.msdim

	return

	end

	subroutine gs2_sortpol(mat)

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
# 156 "gsortuser5.F" 2 
	record/matrix/mat

	if(init_mode.EQ.0) then
	  call gs_matopen(mat)
	endif

	return

	end


	subroutine gs3_sortpol(mat)

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
# 170 "gsortuser5.F" 2 
# 1 "./polar.inc" 1 
	structure/addbpol/
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
	  INTEGER POLAR(0:MAXDET-1)     ! Polarization
	endstructure
# 171 "gsortuser5.F" 2 
	record/addbpol/abpl
	
	common/addbpol/abpl
	
	record/matrix/mat

	integer incr(3)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue.LT.nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	goto(10,20,30,40,50,60,70) mat.type

10	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l-2					! AaAaAa
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l-1
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j2+1,j1l
	      incr(3)=det(j3).ival(mat.par1)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    end do
	  end do
	end do
	return

20	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l					! AaAbAc
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  incr(3)=det(j1).ival(mat.par3)
	  call cmt_3dincr(%val(mat.cmtbase),incr)
	end do
	return

30	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AaAaBx
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1f,j1l
	    if (j1.ne.j2) then
	     incr(2)=det(j2).ival(mat.par1)
	     do j3=j3f,j3l
	      if (abpl.polar(det(j2).id).eq.1) then
	       incr(3)=det(j3).ival(mat.par3)
	       if (incr(3).lt.mat.res(3)/2)call cmt_3dincr(%val(mat.cmtbase),incr)
	      else if (abpl.polar(det(j2).id).eq.2) then
	       incr(3)=det(j3).ival(mat.par3)+mat.res(3)/2
	       call cmt_3dincr(%val(mat.cmtbase),incr)
	      end if
	     end do
	    end if
	  end do
	end do
	return

40	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AaAbBx
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  do j3=j3f,j3l
	    incr(3)=det(j3).ival(mat.par3)
	    call cmt_3dincr(%val(mat.cmtbase),incr)
	  end do
	end do
	return

50	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBaBa
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l-1
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j2+1,j2l
	      incr(3)=det(j3).ival(mat.par2)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    end do
	  end do
	end do
	return

60	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBaBb
	  incr(1)=det(j1).ival(mat.par1)
	    do j2=j2f,j2l
	      incr(2)=det(j2).ival(mat.par2)
	      incr(3)=det(j2).ival(mat.par3)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    end do
	end do
	return

70	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AxByCz
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    incr(2)=det(j2).ival(mat.par2)
	      do j3=j3f,j3l
	        incr(3)=det(j3).ival(mat.par3)
	        call cmt_3dincr(%val(mat.cmtbase),incr)
	      end do
	  end do
	end do
	return

1000	goto(1010,1020,1030,1040,1050,1060,1070) mat.type

1010	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l-2					! AaAaAa
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1011
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1f-1
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1012
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j2+1,j1l
	        nn3=nn2
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1013
	      incr(3)=det(j3).ival(mat.par1)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
1013	    end do
1012	  end do
1011	end do
	return

1020	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l					! AaAbAc
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1021
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  incr(3)=det(j1).ival(mat.par3)
	  call cmt_3dincr(%val(mat.cmtbase),incr)
1021	end do
	return

1030	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l-1					! AaAaBx
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1031
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	    if(mat.hhh1) then
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1032
	    endif
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j3f,j3l
	      if(mat.hhh3) then
	        nn3=hntrue
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1033
	      endif
	      incr(3)=det(j3).ival(mat.par3)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
1033	    end do
1032	  end do
1031	end do
	return

1040	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AaAbBx
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1041
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  do j3=j3f,j3l
	    if(mat.hhh3) then
	      nn3=hntrue
	      if(hltrue(j3)) nn3=nn3-1
	      if(nn3.LT.nhash) goto 1043
	    endif
	    incr(3)=det(j3).ival(mat.par3)
	    call cmt_3dincr(%val(mat.cmtbase),incr)
1043	  end do
1041	end do
	return

1050	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBaBa
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1051
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l-1
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1052
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j2+1,j2l
	      if(mat.hhh2) then
	        nn3=nn2
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1053
	      endif
	      incr(3)=det(j3).ival(mat.par2)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
1053	    end do
1052	  end do
1051	end do
	return

1060	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBaBb
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1061
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1062
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    incr(3)=det(j2).ival(mat.par3)
	    call cmt_3dincr(%val(mat.cmtbase),incr)
1062	  end do
1061	end do
	return

1070	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AxByCz
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1071
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1072
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j3f,j3l
	      if(mat.hhh3) then
	        nn3=hntrue
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1073
	      endif
	      incr(3)=det(j3).ival(mat.par3)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
1073	     end do
1072	  end do
1071	end do

8000	return

	end





	subroutine gs4_sortpol(mat)

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
# 488 "gsortuser5.F" 2 
	record/matrix/mat

CVMS	integer   n1,n2
	integer*8 n1,n2

	call cmt_infoincr(%val(mat.cmtbase),n1,n2)
	nincr(mat.ndim)=nincr(mat.ndim) + n1
	flushed=flushed + n2
	  
	if(finit_mode.EQ.2) then
	  call gs_matclose(mat)
	endif

	return

	end

