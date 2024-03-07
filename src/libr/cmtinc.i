# 1 "cmtinc.F"

	logical*1 	function cmt_incr(cmt,incr)

# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 5 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer incr(MAXMATDIM)

	goto (1,2,3,4,5,6,7,8,9) cmt.incrmode
	goto 100

1	call cmt_2dincr(cmt,incr)
	goto 90
2	call cmt_3dincr(cmt,incr)
	goto 90
3	call cmt_4dincr(cmt,incr)
	goto 90
4	call cmt_s2dincr(cmt,incr)
	goto 90
5	call cmt_s3dincr(cmt,incr)
	goto 90
6	call cmt_s4dincr(cmt,incr)
	goto 90
7	call cmt_hs2dincr(cmt,incr)
	goto 90
8	call cmt_hs3dincr(cmt,incr)
	goto 90
9	call cmt_hs4dincr(cmt,incr)
	goto 90

90	cmt_incr=.true.
	return

100	cmt_incr=.false.
	return

	end

	subroutine cmt_2dincr(cmt,incr)

# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 42 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer incr(MAXMATDIM)

	integer iene1,iene2
	integer kn1,kn2
	integer ki1,ki2
	integer nblocco,ivalblo

	integer*8 iaddr

	integer nvals
	integer pval

	logical*1  cmt_flush,mlm_insert,mlm_flush
	external cmt_flush,mlm_insert,mlm_flush

	iene1=incr(1)
	if(iene1.lt.0 .or. iene1.ge. cmt.res(1)) return
	iene2=incr(2)
	if(iene2.lt.0 .or. iene2.ge. cmt.res(2)) return

	kn1 = iene1/    CMT.STEP(1)
	ki1 = iene1-kn1*CMT.STEP(1) 

	kn2 = iene2/    CMT.STEP(2)
	ki2 = iene2-kn2*CMT.STEP(2) 

	nblocco = kn1 + CMT.NDIV(1) * kn2
	ivalblo = ki1 + CMT.STEP(1) * ki2

	pval=iene1 + cmt.poff(1)
	cmt.proje(pval)=cmt.proje(pval)+1
	pval=iene2 + cmt.poff(2)
	cmt.proje(pval)=cmt.proje(pval)+1

	cmt.nincrement=cmt.nincrement+1
	if(.not.mlm_insert(%val(cmt.mlmbase),nblocco,ivalblo)) then
	  if(mlm_flush(%val(cmt.mlmbase),nblocco,iaddr,nvals)) then
	    if(nvals.gt.0) then
	      if(.not.cmt_flush(cmt,%val(iaddr),nblocco,nvals)) then
	        stop 'CMT_2DINCR: Illegal value returned from CMT_FLUSH'
	      endif
	    endif
	  else
	    stop 'CMT_2DINCR: Illegal value returned from MLM_FLUSH'
	  endif
	endif

	RETURN

	end

	subroutine cmt_3dincr(cmt,incr)

# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 98 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer incr(MAXMATDIM)

	integer iene1,iene2,iene3
	integer kn1,kn2,kn3
	integer ki1,ki2,ki3
	integer nblocco,ivalblo

	integer*8 iaddr

	integer nvals
	integer pval

	logical*1  cmt_flush,mlm_insert,mlm_flush
	external cmt_flush,mlm_insert,mlm_flush

	iene1=incr(1)
	if(iene1.lt.0 .or. iene1.ge. cmt.res(1)) return
	iene2=incr(2)
	if(iene2.lt.0 .or. iene2.ge. cmt.res(2)) return
	iene3=incr(3)
	if(iene3.lt.0 .or. iene3.ge. cmt.res(3)) return

	kn1 = iene1/    CMT.STEP(1)
	ki1 = iene1-kn1*CMT.STEP(1)

	kn2 = iene2/    CMT.STEP(2)
	ki2 = iene2-kn2*CMT.STEP(2)

	kn3 = iene3/    CMT.STEP(3)
	ki3 = iene3-kn3*CMT.STEP(3)

	nblocco = kn1 + CMT.NDIV(1) * ( kn2 + CMT.NDIV(2) * kn3 )
	ivalblo = ki1 + CMT.STEP(1) * ( ki2 + CMT.STEP(2) * ki3 )

	pval=iene1 + cmt.poff(1)
	cmt.proje(pval)=cmt.proje(pval)+1
	pval=iene2 + cmt.poff(2)
	cmt.proje(pval)=cmt.proje(pval)+1
	pval=iene3 + cmt.poff(3)
	cmt.proje(pval)=cmt.proje(pval)+1

	cmt.nincrement=cmt.nincrement+1
	if(.not.mlm_insert(%val(cmt.mlmbase),nblocco,ivalblo)) then
	  if(mlm_flush(%val(cmt.mlmbase),nblocco,iaddr,nvals)) then
	    if(nvals.gt.0) then
	      if(.not.cmt_flush(cmt,%val(iaddr),nblocco,nvals)) then
	        stop 'CMT_3DINCR: Illegal value returned from CMT_FLUSH'
	      endif
	    endif
	  else
	    stop 'CMT_3DINCR: Illegal value returned from MLM_FLUSH'
	  endif
	endif

	RETURN

	end

	subroutine cmt_4dincr(cmt,incr)

# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 161 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer incr(MAXMATDIM)

	integer iene1,iene2,iene3,iene4
	integer kn1,kn2,kn3,kn4
	integer ki1,ki2,ki3,ki4
	integer nblocco,ivalblo

	integer*8 iaddr

	integer nvals
	integer pval

	logical*1  cmt_flush,mlm_insert,mlm_flush
	external cmt_flush,mlm_insert,mlm_flush

	iene1=incr(1)
	if(iene1.lt.0 .or. iene1.ge. cmt.res(1)) return
	iene2=incr(2)
	if(iene2.lt.0 .or. iene2.ge. cmt.res(2)) return
	iene3=incr(3)
	if(iene3.lt.0 .or. iene3.ge. cmt.res(3)) return
	iene4=incr(4)
	if(iene4.lt.0 .or. iene4.ge. cmt.res(4)) return

	kn1 = iene1/    CMT.STEP(1)
	ki1 = iene1-kn1*CMT.STEP(1)

	kn2 = iene2/    CMT.STEP(2)
	ki2 = iene2-kn2*CMT.STEP(2)

	kn3 = iene3/    CMT.STEP(3)
	ki3 = iene3-kn3*CMT.STEP(3)

	kn4 = iene4/    CMT.STEP(4)
	ki4 = iene4-kn4*CMT.STEP(4)

	nblocco = kn1 + CMT.NDIV(1) * ( kn2 + CMT.NDIV(2) * ( kn3 + CMT.NDIV(3) * kn4 ) )
	ivalblo = ki1 + CMT.STEP(1) * ( ki2 + CMT.STEP(2) * ( ki3 + CMT.STEP(3) * ki4 ) )

	pval=iene1 + cmt.poff(1)
	cmt.proje(pval)=cmt.proje(pval)+1
	pval=iene2 + cmt.poff(2)
	cmt.proje(pval)=cmt.proje(pval)+1
	pval=iene3 + cmt.poff(3)
	cmt.proje(pval)=cmt.proje(pval)+1
	pval=iene4 + cmt.poff(4)
	cmt.proje(pval)=cmt.proje(pval)+1

	cmt.nincrement=cmt.nincrement+1
	if(.not.mlm_insert(%val(cmt.mlmbase),nblocco,ivalblo)) then
	  if(mlm_flush(%val(cmt.mlmbase),nblocco,iaddr,nvals)) then
	    if(nvals.gt.0) then
	      if(.not.cmt_flush(cmt,%val(iaddr),nblocco,nvals)) then
	        stop 'CMT_4DINCR: Illegal value returned from CMT_FLUSH'
	      endif
	    endif
	  else
	    stop 'CMT_4DINCR: Illegal value returned from MLM_FLUSH'
	  endif
	endif

	RETURN

	end

	subroutine cmt_s2dincr(cmt,incr)

# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 231 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer incr(MAXMATDIM)

	integer iene1,iene2
	integer kn1,kn2
	integer ki1,ki2
	integer nblocco,ivalblo

	integer*8 iaddr

	integer nvals

	logical*1  cmt_flush,mlm_insert,mlm_flush
	external cmt_flush,mlm_insert,mlm_flush

	iene1=min(incr(1),incr(2))
	iene2=max(incr(1),incr(2))

	kn1 = iene1/    CMT.STEP(1)
	ki1 = iene1-kn1*CMT.STEP(1) 

	kn2 = iene2/    CMT.STEP(1)
	ki2 = iene2-kn2*CMT.STEP(1) 

	nblocco = kn1 + (kn2*(kn2+1))/2
	ivalblo=  ki1 +   CMT.STEP(1)*ki2

	cmt.proje(iene1)=cmt.proje(iene1)+1
	cmt.proje(iene2)=cmt.proje(iene2)+1

	cmt.nincrement=cmt.nincrement+1
	if(.not.mlm_insert(%val(cmt.mlmbase),nblocco,ivalblo)) then
	  if(mlm_flush(%val(cmt.mlmbase),nblocco,iaddr,nvals)) then
	    if(nvals.gt.0) then
	      if(.not.cmt_flush(cmt,%val(iaddr),nblocco,nvals)) then
	        stop 'CMT_S2DINCR: Illegal value returned from CMT_FLUSH'
	      endif
	    endif
	  else
	    stop 'CMT_S2DINCR: Illegal value returned from MLM_FLUSH'
	  endif
	endif

	RETURN

	end

	subroutine cmt_s3dincr(cmt,incr)

# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 282 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer incr(MAXMATDIM)

	integer iene1,iene2,iene3,ii
	integer kn1,kn2,kn3
	integer ki1,ki2,ki3
	integer nblocco,ivalblo

	integer*8 iaddr

	integer nvals

	logical*1  cmt_flush,mlm_insert,mlm_flush
	external cmt_flush,mlm_insert,mlm_flush

	ii=incr(1)+incr(2)+incr(3)
	iene1=min(incr(1),incr(2),incr(3))
	iene3=max(incr(1),incr(2),incr(3))
	iene2=ii-iene1-iene3		

	kn1 = iene1/    CMT.STEP(1)
	ki1 = iene1-kn1*CMT.STEP(1) 

	kn2 = iene2/    CMT.STEP(1)
	ki2 = iene2-kn2*CMT.STEP(1) 

	kn3 = iene3/    CMT.STEP(1)
	ki3 = iene3-kn3*CMT.STEP(1) 

	nblocco = kn1 + (kn2*(kn2+1))/2 + (kn3*(kn3+1)*(kn3+2))/6
	ivalblo = ki1 + CMT.STEP(1)*(ki2 + CMT.STEP(1)*ki3 )

	cmt.proje(iene1)=cmt.proje(iene1)+1
	cmt.proje(iene2)=cmt.proje(iene2)+1
	cmt.proje(iene3)=cmt.proje(iene3)+1

	cmt.nincrement=cmt.nincrement+1
	if(.not.mlm_insert(%val(cmt.mlmbase),nblocco,ivalblo)) then
	  if(mlm_flush(%val(cmt.mlmbase),nblocco,iaddr,nvals)) then
	    if(nvals.gt.0) then
	      if(.not.cmt_flush(cmt,%val(iaddr),nblocco,nvals)) then
	        stop 'CMT_S3DINCR: Illegal value returned from CMT_FLUSH'
	      endif
	    endif
	  else
	    stop 'CMT_S3DINCR: Illegal value returned from MLM_FLUSH'
	  endif
	endif

	return

	end

	subroutine cmt_s4dincr(cmt,incr)

# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 339 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer incr(MAXMATDIM)

	integer iene1,iene2,iene3,iene4,ii
	integer kn1,kn2,kn3,kn4
	integer ki1,ki2,ki3,ki4
	integer nblocco,ivalblo

	integer*8 iaddr

	integer nvals

	logical*1  cmt_flush,mlm_insert,mlm_flush
	external cmt_flush,mlm_insert,mlm_flush

	ii=incr(1)+incr(2)+incr(3)
	iene1=min(incr(1),incr(2),incr(3))
	iene3=max(incr(1),incr(2),incr(3))
	iene2=ii-iene1-iene3
	iene4=incr(4)
	if(iene4.lt.iene3) then
	  ii=iene4
	  iene4=iene3
	  iene3=ii
	  if(iene3.lt.iene2) then
	    ii=iene3
	    iene3=iene2
	    iene2=ii
	    if(iene2.lt.iene1) then
	      ii=iene2
	      iene2=iene1
	      iene1=ii
	    endif
	  endif
	endif

	kn1 = iene1/    CMT.STEP(1)
	ki1 = iene1-kn1*CMT.STEP(1) 

	kn2 = iene2/    CMT.STEP(1)
	ki2 = iene2-kn2*CMT.STEP(1) 

	kn3 = iene3/    CMT.STEP(1)
	ki3 = iene3-kn3*CMT.STEP(1) 

	kn4 = iene4/    CMT.STEP(1)
	ki4 = iene4-kn4*CMT.STEP(1) 

	nblocco = kn1 + (kn2*(kn2+1))/2 + (kn3*(kn3+1)*(kn3+2))/6 + (kn4*(kn4+1)*(kn4+2)*(kn4+3))/24
	ivalblo = ki1 + CMT.STEP(1)*(ki2 + CMT.STEP(1)*(ki3  + CMT.STEP(1)*ki4) )

	cmt.proje(iene1)=cmt.proje(iene1)+1
	cmt.proje(iene2)=cmt.proje(iene2)+1
	cmt.proje(iene3)=cmt.proje(iene3)+1
	cmt.proje(iene4)=cmt.proje(iene4)+1

	cmt.nincrement=cmt.nincrement+1
	if(.not.mlm_insert(%val(cmt.mlmbase),nblocco,ivalblo)) then
	  if(mlm_flush(%val(cmt.mlmbase),nblocco,iaddr,nvals)) then
	    if(nvals.gt.0) then
	      if(.not.cmt_flush(cmt,%val(iaddr),nblocco,nvals)) then
	        stop 'CMT_S4DINCR: Illegal value returned from CMT_FLUSH'
	      endif
	    endif
	  else
	    stop 'CMT_S4DINCR: Illegal value returned from MLM_FLUSH'
	  endif
	endif

	return

	end

	subroutine cmt_hs2dincr(cmt,incr)

# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 416 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer incr(MAXMATDIM)

	integer nn,jj,iene1,iene2
	integer kn2
	integer ki2
	integer nblocco,ivalblo

	integer*8 iaddr

	integer nvals

	logical*1  cmt_flush,mlm_insert,mlm_flush
	external cmt_flush,mlm_insert,mlm_flush

	iene1=incr(1)
	iene2=incr(2)
	nn=1

10	kn2 = iene2/    CMT.STEP(2)
	ki2 = iene2-kn2*CMT.STEP(2) 

	nblocco = kn2
	ivalblo=  iene1 +  CMT.RES(1) * ki2

	cmt.proje(iene1)=cmt.proje(iene1)+1

	cmt.nincrement=cmt.nincrement+1
	if(.not.mlm_insert(%val(cmt.mlmbase),nblocco,ivalblo)) then
	  if(mlm_flush(%val(cmt.mlmbase),nblocco,iaddr,nvals)) then
	    if(nvals.gt.0) then
	      if(.not.cmt_flush(cmt,%val(iaddr),nblocco,nvals)) then
	        stop 'CMT_HS2DINCR: Illegal value returned from CMT_FLUSH'
	      endif
	    endif
	  else
	    stop 'CMT_HS2DINCR: Illegal value returned from MLM_FLUSH'
	  endif
	endif

	if(nn.eq.2) return
	nn=nn+1
	jj=iene1
	iene1=iene2
	iene2=jj
	goto 10

	end

	subroutine cmt_hs3dincr(cmt,incr)

# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 469 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer incr(MAXMATDIM)

	integer jene1,jene2,jene3
	integer nn,jj,iene1,iene2,iene3
	integer kn2,kn3
	integer ki2,ki3
	integer nblocco,ivalblo

	integer*8 iaddr

	integer nvals

	logical*1  cmt_flush,mlm_insert,mlm_flush
	external cmt_flush,mlm_insert,mlm_flush

	jene1=incr(1)
	jene2=incr(2)
	jene3=incr(3)
	nn=1

10	iene1=jene1
	iene2=min(jene2,jene3)
	iene3=max(jene2,jene3)

	kn2 = iene2/    CMT.STEP(2)
	ki2 = iene2-kn2*CMT.STEP(2) 

	kn3 = iene3/    CMT.STEP(2)
	ki3 = iene3-kn3*CMT.STEP(2) 

	nblocco = kn2 + (kn3*(kn3+1))/2
	ivalblo = iene1+ CMT.RES(1) * (ki2 + CMT.STEP(2)*ki3)

	cmt.proje(iene1)=cmt.proje(iene1)+1

	cmt.nincrement=cmt.nincrement+1
	if(.not.mlm_insert(%val(cmt.mlmbase),nblocco,ivalblo)) then
	  if(mlm_flush(%val(cmt.mlmbase),nblocco,iaddr,nvals)) then
	    if(nvals.gt.0) then
	      if(.not.cmt_flush(cmt,%val(iaddr),nblocco,nvals)) then
	        stop 'CMT_HS3DINCR: Illegal value returned from CMT_FLUSH'
	      endif
	    endif
	  else
	    stop 'CMT_HS3DINCR: Illegal value returned from MLM_FLUSH'
	  endif
	endif

	if(nn.eq.3) return
	nn=nn+1
	jj=jene1
	jene1=jene2
	jene2=jene3
	jene3=jj
	goto 10

	end

	subroutine cmt_hs4dincr(cmt,incr)

# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 532 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer incr(MAXMATDIM)

	integer jene1,jene2,jene3,jene4
	integer nn,jj,iene1,iene2,iene3,iene4,ii
	integer kn2,kn3,kn4
	integer ki2,ki3,ki4
	integer nblocco,ivalblo

	integer*8 iaddr

	integer nvals

	logical*1  cmt_flush,mlm_insert,mlm_flush
	external cmt_flush,mlm_insert,mlm_flush

	jene1=incr(1)
	jene2=incr(2)
	jene3=incr(3)
	jene4=incr(4)
	nn=1

10	iene1=jene1
	ii=jene2+jene3+jene4
	iene2=min(jene2,jene3,jene4)
	iene4=min(jene2,jene3,jene4)
	iene3=ii-iene2-iene4

	kn2 = iene2/    CMT.STEP(2)
	ki2 = iene2-kn2*CMT.STEP(2) 

	kn3 = iene3/    CMT.STEP(2)
	ki3 = iene3-kn3*CMT.STEP(2) 

	kn4 = iene4/    CMT.STEP(2)
	ki4 = iene4-kn4*CMT.STEP(2) 

	nblocco = kn2 + (kn3*(kn3+1))/2 + (kn4*(kn4+1)*(kn4+2))/6
	ivalblo = iene1 + CMT.RES(1) * ( ki2 + CMT.STEP(2)*(ki3 + CMT.STEP(2)*ki4) )

	cmt.proje(iene1)=cmt.proje(iene1)+1

	cmt.nincrement=cmt.nincrement+1
	if(.not.mlm_insert(%val(cmt.mlmbase),nblocco,ivalblo)) then
	  if(mlm_flush(%val(cmt.mlmbase),nblocco,iaddr,nvals)) then
	    if(nvals.gt.0) then
	      if(.not.cmt_flush(cmt,%val(iaddr),nblocco,nvals)) then
	        stop 'CMT_HS4DINCR: Illegal value returned from CMT_FLUSH'
	      endif
	    endif
	  else
	    stop 'CMT_HS4DINCR: Illegal value returned from MLM_FLUSH'
	  endif
	endif

	if(nn.eq.4) return
	nn=nn+1
	jj=jene1
	jene1=jene2
	jene2=jene3
	jene3=jene4
	jene4=jj
	goto 10

	end

	logical*1  function cmt_flush(cmt,mmlist,iseg,nentr)

# 1 "./cmtlib.inc" 1 
# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 2 "./cmtlib.inc" 2 

	integer*8 dbufbase,cbufbase,tbufbase

	integer dbuflen
	integer cbuflen
	integer tbuflen
	common/cmtcom/dbufbase,cbufbase,tbufbase,dbuflen,cbuflen,tbuflen
# 602 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer mmlist(1)
	integer iseg,nentr

	integer dbuf(0:1)
	pointer (dbufpntr,dbuf)
	
	integer nrec,ii,jj

	logical*1  getmem,freemem,cmt_readsegment,cmt_writesegment
	external getmem,freemem,cmt_readsegment,cmt_writesegment

	if(nentr.le.0) goto 90
	if(cmt.readonly .or. iseg.lt.0 .or. iseg.ge.cmt.nseg) goto 100

	dbufpntr=dbufbase

	if(.not.cmt_readsegment(cmt,iseg,%val(dbufbase),nrec)) goto 100

*	do ii=1,nentr
*	  jj=mmlist(ii)
*	  if(jj.ge.0) then
*	    dbuf(jj)=dbuf(jj)+1
*	  else
*	    jj=-(jj+1)
*	    dbuf(jj)=dbuf(jj)-1
*	  endif
*	enddo

	do ii=1,nentr
	    dbuf(mmlist(ii))=dbuf(mmlist(ii))+1
	enddo


	if(.not.cmt_writesegment(cmt,iseg,%val(dbufbase),nrec)) goto 100

	cmt.nflush=cmt.nflush+1

90	cmt_flush=.true.
	return

100	cmt_flush=.false.
	return

	end

	logical*1  function cmt_incr_init(cmt)

c#include "cmtlib.def"
# 1 "./cmtlib.inc" 1 
# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 2 "./cmtlib.inc" 2 

	integer*8 dbufbase,cbufbase,tbufbase

	integer dbuflen
	integer cbuflen
	integer tbuflen
	common/cmtcom/dbufbase,cbufbase,tbufbase,dbuflen,cbuflen,tbuflen
# 653 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer ii,n1,n2,n3,nbyt
	integer megabytes /32/

	integer inp_i1
	logical*1  mlm_init,mlm_info,cmt_getproje
	external inp_i1,mlm_init,mlm_info,cmt_getproje

	logical*1  getmem,freemem,cmt_readsegment,cmt_writesegment
	external getmem,freemem,cmt_readsegment,cmt_writesegment

4	if    (cmt.segsize.le.2**8) then
	  nbyt=1
	elseif(cmt.segsize.le.2**16) then
	  nbyt=2
	elseif(cmt.segsize.le.2**24) then
	  nbyt=3
	else
	  nbyt=4
	endif
	
	if(cmt.segsize .gt. dbuflen) then
	  if(dbuflen.gt.0) then
	    if(.not.freemem(4*dbuflen,dbufbase)) then
	      write(6,*) 'Error releasing  memory in CMT_FLUSH'
	      call exit(0)
	    endif
	  endif
	  dbuflen=cmt.segsize
	  if(.not.getmem(4*dbuflen,dbufbase)) then
	    write(6,*) 'Error allocating memory in CMT_FLUSH'
	    call exit(0)
	  endif
	endif

	if( cmt.ndim .gt. 2 ) then
		if( megabytes .lt. 96 ) megabytes = 96
	else
		if( megabytes .gt. 32 ) megabytes = 32	
	endif
	
	ii=inp_i1('Please give the amount of memory for this sort (MegaBytes) ',megabytes)
	if(ii.lt.0) call exit
	if(megabytes.le.0) stop 'Invalid memory size'

	if(.not.mlm_init(cmt.mlmbase,cmt.nseg,nbyt,megabytes,n1)) then
	  write(6,*) 'CMT_INCR_INIT: Bad status from MLM_INIT'
	  write(6,*) 'Called with',cmt.nseg,nbyt,megabytes,n1
	  write(6,*) 'Please change the size of memory'
	  goto 4	! riprovaci
	endif
	if(.not.mlm_info(%val(cmt.mlmbase),-1,n1,n2)) then
	  write(6,*) '# entries per segment from ',n1,'   to',n2
	endif

	cmt.incrmode=(MAXMATDIM-1)*cmt.matmode+cmt.ndim-1
	cmt.nflush=0

	if(cmt.matmode.eq.0) then		! legge le proiezioni
	  do ii=1,cmt.ndim
	    if(.not.cmt_getproje(cmt,ii,cmt.proje(cmt.poff(ii)))) goto 100
	  enddo
	else
	  if(.not.cmt_getproje(cmt,1,cmt.proje(cmt.poff(1)))) goto 100
	endif

	cmt_incr_init=.true.
	return

100	cmt_incr_init=.false.
	return

	end

	logical*1  function cmt_incr_finit(cmt)

# 1 "./cmtlib.def" 1 
# 1 "./../libr/types.def" 1 
# 6





# 28

# 2 "./cmtlib.def" 2 
# 1 "./cmtlib.par" 1 

	IMPLICIT NONE

	INTEGER  CMTVERSION
	INTEGER  MAXCMODE
	INTEGER  MAXNBITS
	INTEGER  DRECBYTE
	INTEGER  DRECWORD
	INTEGER  DRECLONG
	INTEGER  DRECBITS
	INTEGER  MAXMATDIM
	INTEGER  MAXMATRES
	INTEGER  MAXSEGSIZE
	INTEGER  MAXPARCAL
	INTEGER  MINFLEN
	INTEGER  MINFLEN2D
	INTEGER  PROJESEG
	INTEGER  TBUFSIZE
	INTEGER  MEMCSIZE
	

	PARAMETER (CMTVERSION=5)			! version of library
	PARAMETER (MAXCMODE=255)			! max value of compression mode
	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (DRECBYTE=512)			! # of bytes per disk record
	PARAMETER (DRECWORD=DRECBYTE/2)		! # of words
	PARAMETER (DRECLONG=DRECBYTE/4)		! # of longwords
	PARAMETER (DRECBITS=DRECBYTE*8)		! # of bits
	PARAMETER (MAXMATDIM=4)			! Max matrix dimension
	PARAMETER (MAXMATRES=8*1024)		! Max # length of matrix axis
	PARAMETER (MAXSEGSIZE=1024*1024)		! Max # of channels per segment
	PARAMETER (MAXPARCAL=10)			! Max number of calib.params
	PARAMETER (MINFLEN=3000)
	PARAMETER (MINFLEN2D=900)
	PARAMETER (PROJESEG=1)
	PARAMETER (TBUFSIZE=8)			! amount of memory for cmt_readspec
	PARAMETER (MEMCSIZE=8)			! amount of memory for memclib
# 3 "./cmtlib.def" 2 


	structure/cmtdef/
	  INTEGER NDIM			! Matrix dimension
	  INTEGER RES(MAXMATDIM)	! Resolution of matrix axis
	  INTEGER STEP(MAXMATDIM)	! size of step of matrix axis
	  INTEGER NDIV(MAXMATDIM)	! # segments of matrix axis
	  INTEGER POFF(MAXMATDIM)	! offset to projection in PROJE
	  INTEGER NSEG		! # segments of matrix
	  INTEGER NEXTRA	! # extra segments
	  INTEGER SEGSIZE	! size of data segments
	  INTEGER CMODE		! from compression of last read/written record
	  INTEGER CMINVAL	!         "

	  integer*8 OLDCMODE	! array of cmode for previous versions
	  integer*8 OLDCMINVAL	! array of cminval    "

	  INTEGER OLDCLEN	! their length in LW
	  logical*1 READONLY	! Matrix cannot be modified
	  INTEGER VERSION	! Version of CMT software
	  INTEGER CMTID		! a unique identifier of this structure
	  INTEGER MATMODE	! type of increment
	  INTEGER INCRMODE	! type of increment
	  INTEGER*8 NINCREMENT	! # increments
	  INTEGER NFLUSH	! # calls to cmt_flush
	  INTEGER CMTLEN	! sizeof this structure in LW

	  integer*8 IVFBASE	! structure for IVFLIB
	  integer*8 MLMBASE	! structure for MLMLIB
	  integer*8 MEMCBASE	! structure for MEMCLIB

	  INTEGER PROJE(0:1)	! space for projections
	endstructure

# 731 "cmtinc.F" 2 
	record/cmtdef/cmt

	integer*8 iaddr

	integer iseg,nvals

	integer ii

	logical*1  cmt_flush,mlm_flush,cmt_putproje
	external cmt_flush,mlm_flush,cmt_putproje

	write(6,*)
	do iseg=0,cmt.nseg-1
	  if(mlm_flush(%val(cmt.mlmbase),iseg,iaddr,nvals)) then
	    if(nvals.gt.0) then
	      if(.not.cmt_flush(cmt,%val(iaddr),iseg,nvals)) then
	        stop 'CMT_INCR_FINIT: Illegal value returned from CMT_FLUSH'
	      endif
# 751

	      write(6,'(a3,i8)')char(27)//char(91)//char(65), iseg

	    endif
	  else
	    stop 'CMT_INCR_FINIT: Illegal value returned from MLM_FLUSH'
	  endif
	enddo

	write(6,*)
        call flush(6)


	if(cmt.matmode.eq.0) then		! scrive le proiezioni
	  do ii=1,cmt.ndim
	    if(.not.cmt_putproje(cmt,ii,cmt.proje(cmt.poff(ii)))) goto 100
	  enddo
	else
	  if(.not.cmt_putproje(cmt,1,cmt.proje(cmt.poff(1)))) goto 100
	endif

	cmt_incr_finit=.true.
	return

100	cmt_incr_finit=.false.
	return

	end
