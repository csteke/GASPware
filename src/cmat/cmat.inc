#include "../libr/types.def"

	PARAMETER (MAXRES=8*8192)		! Maximum Matrix_Dimension
	PARAMETER (MAXSTEP2=256)
	PARAMETER (MAXNMAT=5)
	PARAMETER (MAXMATDIM=4)
	PARAMETER (MAXSEGSIZE=8*1024*1024)
	PARAMETER (MAXGATES=1000)

	logical*1 error
	common /cmat0/ error

	CHARACTER*70 CMATFILE(MAXNMAT)
	common /cmat1/ cmatfile


	MACHINE_POINTERSIZE cmt(MAXNMAT)

	integer ndim(MAXNMAT)
	integer res(MAXMATDIM,MAXNMAT)
	integer step(MAXMATDIM,MAXNMAT)
	integer mblr(MAXMATDIM,MAXNMAT)
	integer nseg(MAXNMAT)
	integer segsize(MAXNMAT)
	integer matmode(MAXNMAT)
	logical*1 opened(MAXNMAT),readonly(MAXNMAT)
	common /cmat2/ cmt,ndim,res,step,mblr,nseg,matmode,opened,readonly

	INTEGER bufl(0:MAXSEGSIZE-1,MAXNMAT)
	common /cmat3/ bufl

	INTEGER PROJE(0:MAXRES-1,MAXNMAT)
	common /cmat4/ PROJE
	
	logical*1 inp_yes,inp_not
	external inp_yes,inp_not

*** N.M
	structure /AUTOBGTYPE/
	  real*8 tot
	  real*8 gsc
	  real*8 gbc
	  real*8 rr
	  real p(0:maxres-1)
	  real bp(0:maxres-1)
	  real sp(0:maxres-1)
	  real bg(0:maxres-1)
	  real dbg(0:maxres-1)
	  real wgh(0:maxres-1)
	  real bwgh(0:maxres-1)
	  logical*1 mark(0:maxres-1)
	  integer res
	  logical*1 yes
	  logical*1 useproj
	  logical*1 changed
	  logical*1 initialised
	  integer m
	  integer itmax
	  real fstep
	  character ch

	  MACHINE_POINTERSIZE cmt

	  character*60 specname
	  character*256 CmatName
	end structure
	record /AUTOBGTYPE/autobg
	common /AUTOBGCOMM/autobg
