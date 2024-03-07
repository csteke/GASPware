#include "../libr/types.def"
#define GO_UP_ONE_LINE char(27)//char(91)//char(65)

        integer*4 ika,ikl,nmin,nmax,npeaks,ifunct

	CHARACTER FNAMEI*60,FNAMEO*60,FRMI*1,FRMO*1
	integer*4 ifrmi,nkap
	COMMON /TRA_FILE1/ FNAMEI,FRMI,FNAMEO,FRMO
	COMMON /TRA_FILE2/ LFNAMEI,IFRMI,KLI,LFNAMEO,IFRMO,KLO

	COMMON /TRA_DATI/ SPEK(0:64*1024-1),ERR2(0:64*1024-1),NKAP,IKA,IKL

	COMMON /TRA_MARK/ MARKLE,MARKRI,RMARKLE,RMARKRI

	COMMON /TRA_DIS1/ LUNOUT,ITYP,NMIN,NMAX,YMIN,YMAX,IFUNCT
	COMMON /TRA_DIS2/ XMINF,XMAXF,YMINF,YMAXF
	COMMON /TRA_DIS3/ TWx0,TWx1,TWy0,TWy1
	COMMON /TRA_DIS4/ ticx,ticy,ticl,chrx,chry
	Character Labelx*60
	logical*1 islaser,xterm,tekmode
	integer*2 tekcolor
	logical*2 tekcolinc,emptyframe
	
	COMMON /TRA_DIS5/labelx,lxcal,textsize,textang,tekcolor,tekcolinc,emptyframe,islaser,xterm,tekmode

	COMMON /TRA_SCAL/ DWIN(4),TWIN(4),XSCA,YSCA

	integer*4 m$k
	parameter (m$k=6)
	logical*1 sqrterm
	real*4 ecal(m$k+1),wcal(m$k+1),aener1,aener2
	COMMON /TRA_CALI/ ECAL,IECAL,WCAL,IWCAL,WFIND,aener1,aener2,sqrterm

	logical*1 CYCLE,BREAK
	CHARACTER SPECLIST*60
	COMMON /TRA_AUTO/ LISTLUN,SPECLIST,CYCLE,BREAK

	integer lplotcom
	character*80 plotcom
	common /tra_plot/ lplotcom, plotcom

	COMMON /TRA_BACK/ IFBGD,XMBGD(12),NMBGD,BG0,BG1,N0BGD,E2BG0,E2BG1,EBG01
	COMMON /TRA_INTE/ IFINT,XMINT(40),NMINT,AREA,POSI,FWHM,DAREA,DPOSI,DFWHM
	
	real*8 GFPAR(60)
	COMMON /TRA_GAUS/ IFGAU,XMREG(2),NMREG,XMGAU(29),NMGAU,GFPAR,N0GAU

	PARAMETER (M$PEAKS=1024)	! max # picchi in peak_buffer
	PARAMETER (N$P=50)		! max # picchi in TRACKfit
	PARAMETER (N$T=1000)		! Max # tapes      "
	PARAMETER (M$CH=16384*4)	! max # canali spettri TRACKfit
	PARAMETER (M$P=1000)		! max # parametri in   TRACKfit
	COMMON /TRA_PEAK/ PEAKS(M$PEAKS),NPEAKS
	COMMON /TRA_EREC/ RENE(N$P),DRENE(N$P),NRENE,IREORD,IRWORD,IRESAV,IRWSAV
	COMMON /TRA_TOTR/ POSTOT(N$T,N$P),DPOSTOT(N$T,N$P),AREATOT(N$T,N$P),DAREATOT(N$T,N$P),CHIQFTOT(N$T)

	integer*4 ndata,ixfit(0:M$CH-1),ndata1,ndata2
	real*4 xfit(0:M$CH-1),dxfit(0:M$CH-1),yfit(0:M$CH-1),dyfit(0:M$CH-1)
	common /fit_data/ ndata,ixfit,xfit,dxfit,yfit,dyfit,ndata1,ndata2

	logical*1 VarWidth
	real fitpar(M$P),sfitpar(M$P)

	common /fit_pars/ nfitpar,nfreepar,fitpar,ifreepar(M$P),sfitpar,VarWidth
	common /fit_trreg/FWFACT,itrr1(N$P),itrr2(N$P),itrreg(N$P),ntrreg,ipack1(N$P),ipack2(N$P),ireg,irfit(0:M$CH-1)
	real trchichi(N$P)
	real trpar(M$P)
	common /fit_trpar/Ntrpar,trpar,IWFREE(M$P),trchichi
	common /fit_trind/iestart,jestart,netrpar,iwstart,jwstart,nwtrpar,i_w_e(M$P),i_e_w(M$P)

	PARAMETER (MAXRES=4*8192)		! max res per matrici
	PARAMETER (M$GATE=1000)		! max # coppie di gates
	INTEGER NGATES,NGNEXT
	REAL EGATE(2,M$GATE),CGATE(2,M$GATE)
	INTEGER IGATE(2,M$GATE), gclass(M$GATE)
	COMMON /tra_gate/ ngates,ngnext,egate,cgate,igate,gclass

	INTEGER NWFON
	REAL WFONX(0:MAXRES),WFONY(0:MAXRES)
	COMMON /tra_sfon/ nwfon,wfonx,wfony

	MACHINE_POINTERSIZE   CMT

	INTEGER   LCMATFILE
	CHARACTER CMATFILE*60
	COMMON /CM_M1/cmt,lcmatfile,cmatfile

	logical*1 CMATRIX,TRANSPOSED,SPHERICAL
	logical*1 ISPROJE,ISCUT,KEEPGATES
	INTEGER MDIM,MMODE,MRES(4),MSTEP(4)
	INTEGER GSIDE,CSIDE,CRES
	INTEGER IKAW,IKLW
	INTEGER MPROJ(0:MAXRES-1,4)
	COMMON /CM_M2/cmatrix,transposed,isproje,iscut,mdim,mmode,mres,mstep,gside,cside,cres,
	1	      ikaw,iklw,mproj,spherical,keepgates

	REAL*8	SBACKTOT(4),RBACKTOT(4),RBACKTOT12
	REAL	CORRBACK(4),CORREFFI(4)
	logical*1 BACKSUB,BACKCOM,BACKPRO,EFFICOR
	REAL	SBACK(0:MAXRES-1,4),RBACK(0:MAXRES-1,4),REFFI(0:MAXRES-1,4)
	REAL	CUT(0:MAXRES-1),BACK(0:MAXRES-1)
	COMMON /TRA_SUB/sbacktot,rbacktot,rbacktot12,corrback,correffi,backsub,backcom,backpro,efficor,sback,
	1	rback,reffi,cut,back,spec_divfac
*** N.M
	structure /AUTOBGTYPE/
	  real p(0:maxres-1,4)
	  real bp(0:maxres-1,4)
	  real sp(0:maxres-1)
	  real bg(0:maxres-1)
	  real dbg(0:maxres-1)
	  real wgh(0:maxres-1)
	  real bwgh(0:maxres-1)
	  logical*1 mark(0:maxres-1)
	  real*8 tot
	  real*8 gsc
	  real*8 gbc
	  real*8 rr
	  logical*1 yes
	  logical*1 useproj
	  integer m
	  integer itmax
	  real fstep
	  character ch

	  MACHINE_POINTERSIZE cmt

	  character*60 projfile
	  integer lprojfile
	end structure
	record /AUTOBGTYPE/autobg
	common /AUTOBGCOMM/autobg
        external DFZERO
********	

	logical*1 inp_yes, inp_not
	external inp_yes, inp_not
	integer inp_ch
	external inp_ch
	real*4 cpol, cpolinv
	external cpol,cpolinv
	logical*1 cmt_open,cmt_close,cmt_info,cmt_readspec,cmt_getproje
	external cmt_open,cmt_close,cmt_info,cmt_readspec,cmt_getproje

	logical*1 eff_define
	external eff_define
	real effvalue
	external effvalue
			
