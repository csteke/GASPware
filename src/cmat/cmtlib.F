*
*	Gestione delle matrici compresse
*	I files indicizzati vengono  gestiti attraverso la IVFLIB
C
D
D	program cmt_test
D
D	character*60 filename
D	logical*1 old,symmetric
DCVMS	integer   handle
D	integer*8 handle
D	integer res(4),step(4),ndim,mode
D	integer lres(4),lstep(4),lndim
D	integer seed	/123456789/
D	integer dati(1024*1024)
D	logical*1 cmt_new,cmt_open,cmt_close,cmt_readsegment,cmt_writesegment
D
DCVMS
D       open(unit=6,carriagecontrol='fortran',status='unknown')
D
D	call inp_ch('Filename',filename)
D
D	old=cmt_open(filename,mode,handle)
D	if(old) then
D	  call cmt_info(%val(handle),-1,ndim,n2)
D	  if(n2.eq.1) then
D	    write(6,'(A,i1,a)') ' This is a ',ndim,'D Matrix (symmetrized)'
D	    symmetric=.true.
D	  else
D	    write(6,'(A,i1,a)') ' This is a ',ndim,'D Matrix'
D	    symmetric=.false.
D	  endif
D	  call cmt_info(%val(handle),-2,res,step)
D	  do ii=1,ndim
D	    write(6,*) ii,'  Res',res(ii),'   Step',step(ii)
D	  enddo
D	  call cmt_info(%val(handle),-3,nseg,isize)
D	  write(6,*) 'Nseg',nseg,'  Size',isize
D	  write(6,*)
D
D	  do ii=0,nseg-1
D	    do jj=1,isize
D	      dati(jj)=0
D	    enddo
D	    if(.not.cmt_readsegment(%val(handle),ii,dati,nget)) then
D	      write(6,*)'isize,nget,seg',isize,nget,ii
D	      call exit
D	    endif
D	    iarea=0
D	    nonzero=0
D	    do jj=3,isize
D	      if(dati(jj).ne.0) then
D	        nonzero=nonzero+1
D	        iarea=iarea+dati(jj)
D	      endif
D	    enddo
D	    call cmt_info(%val(handle),ii,nrec,irec)
D	    call cmt_info(%val(handle),-4,mode,minval)
D	    write(6,'(1h ,11i7)') ii,isize,nonzero,dati(1),iarea,dati(2),nrec,mode,minval
D	    if(nonzero.ne.dati(1) .or. iarea.ne.dati(2)) call exit
D	  enddo
D	else
D	  write(6,*) 'Error opening  ',filename(1:max(lengthc(filename),1))
D	  call inp_i1('# dimensions',ndim)
D	  call inp_ask('Symmetrized matrix',symmetric)
D	  if(symmetric) then
D	    call inp_i2('Res, Step',Res(1),Step(1))
D	    do ii=2,ndim
D	     res(ii)=res(1)
D	     step(ii)=step(1)
D	    enddo
D	    mode=1
D	  else
D	    call inp_ia('Res',res,ndim)
D	    call inp_ia('Step',step,ndim)
D	    mode=0
D	  endif
D	  if(.not.cmt_new(filename,ndim,res,step,mode)) then
D	    write(6,*) 'Error creating file'
D	    call exit
D	  endif
D	  mode=1
D	  if(.not.cmt_open(filename,mode,handle)) then
D	    write(6,*) 'Error Re-opening file'
D	    call exit
D	  endif
D	  call cmt_info(%val(handle),-1,lndim,n2)
D	  if(n2.eq.1) then
D	      write(6,'(A,i1,a)') ' This is a ',lndim,'D Matrix (symmetrized)'
D	      if(.not.symmetric) call exit
D	  else
D	      write(6,'(A,i1,a)') ' This is a ',lndim,'D Matrix'
D	      if(symmetric) call exit
D	  endif
D	  call cmt_info(%val(handle),-2,lres,lstep)
D	  do ii=1,lndim
D	    write(6,*) ii,'  Res',lres(ii),'   Step',lstep(ii)
D	    if(lres(ii).ne.res(ii) .or. lstep(ii).ne.step(ii)) call exit
D	  enddo
D	  call cmt_info(%val(handle),-3,nseg,isize)
D	  write(6,*) 'Nseg',nseg,'  Size',isize
D	  if(lndim.ne.ndim) call exit
D
D	  write(6,*)
D	  do ii=1,2*nseg
D	    iseg=ran(seed)*nseg
D	    do jj=1,isize
D	      dati(jj)=0
D	    enddo
D	    ndati=ran(seed)*isize
D	    do jj=1,ndati
D	      ll=1+ran(seed)*isize
D	      lval=ran(seed)*4
D	      dati(ll)=dati(ll)+lval
D	    enddo
D	    iarea=0
D	    nonzero=0
D	    dati(1)=0
D	    dati(2)=0
D	    do jj=1,isize
D	      if(dati(jj).ne.0) then
D	        nonzero=nonzero+1
D	        iarea=iarea+dati(jj)
D	      endif
D	    enddo
D	    dati(1)=nonzero
D	    dati(2)=iarea
D	    if(.not.cmt_writesegment(%val(handle),iseg,dati,nput)) then
D	      write(6,*)'nput=',nput,'   at seg#',iseg
D	      call exit
D	    endif
D	    write(6,'(1h+,5i10)') ii,iseg,nonzero,iarea,nput
D	  enddo
D	endif
D	write(6,*)
D	ii=cmt_close(%val(handle))
D
D	call exit
D
D	end

#include "../libr/types.def"

	logical*1 function cmt_new(filemat,ndim,res,step,mode)

#include "cmtlib.par"

	character filemat*(*)
	integer ndim
	integer res(ndim),step(ndim)
	integer mode			! 0=normal, 1=symmetrized, 2=half_symmetryzed

	integer lres(MAXMATDIM),lstep(MAXMATDIM),lndiv(MAXMATDIM)
	integer lfn,ii,segsize,nnn,nnn_o,nnn_u,nextra,nsegtot
	integer ip,nseg
	integer head(DRECLONG)

#if defined( _GW_BIG_ENDIAN )
	integer head_swp(DRECLONG)
#endif

	MACHINE_POINTERSIZE handle

	character*5 fext

	integer  lengthc
	external lengthc

	logical*1  ivf_new,ivf_open,ivf_write,ivf_close
	external ivf_new,ivf_open,ivf_write,ivf_close

	if(ndim.lt.2 .or. ndim.gt.MAXMATDIM) goto 100
	if(mode.lt.0 .or. mode.gt.2) goto 100

	lfn=max(1,lengthc(filemat))
	if(index(filemat(1:lfn),'.'). gt.0) then
	  fext=' '
	else
	  fext='.cmat'
	endif
	write(6,*) 'Creating compressed matrix  '//filemat(1:lfn)//fext

	if(mode.eq.0) then
	  do ii=1,ndim
	   lres(ii)=abs(res(ii))
	   lstep(ii)=abs(step(ii))
	   lstep(ii)=min(max(1,lstep(ii)),lres(ii))
	   lndiv(ii)=(lres(ii)+lstep(ii)-1)/lstep(ii)
	  enddo
	elseif(mode.eq.1) then
	  lres(1)=abs(res(1))
	  lstep(1)=abs(step(1))
	  lstep(1)=min(max(1,lstep(1)),lstep(1))
	  lndiv(1)=(lres(1)+lstep(1)-1)/lstep(1)
	  do ii=2,ndim
	    lres(ii)=lres(1)
	    lstep(ii)=lstep(1)
	    lndiv(ii)=lndiv(1)
	  enddo
	elseif(mode.eq.2) then
	  lres(1)=abs(res(1))
	  lstep(1)=lres(1)
	  lndiv(1)=1
	  lres(2)=lres(1)
	  lstep(2)=abs(step(2))
	  lstep(2)=min(max(1,lstep(2)),lstep(2))
	  lndiv(2)=(lres(2)+lstep(2)-1)/lstep(2)
	  do ii=3,ndim
	    lres(ii)=lres(2)
	    lstep(ii)=lstep(2)
	    lndiv(ii)=lndiv(2)
	  enddo
	endif

	do ii=1,ndim
	  if(mod(lres(ii),lstep(ii)).ne.0) then
	    write(6,*) 'CMT_NEW: Resolution must be an integer multiple of Step',lres(ii),lstep(ii)
	    goto 100
	  endif
	enddo

	segsize=1
	do ii=1,ndim
	  segsize=segsize*lstep(ii)
	enddo

       if(segsize.gt.MAXSEGSIZE) then
	  write(6,*) 'CMT_NEW: Segment size is too large',segsize,MAXSEGSIZE
	  goto 100
       endif

	if(mode.eq.0) then
	  nseg= 1
	  do ii=1,ndim
	    nseg=nseg*lndiv(ii)
	  enddo
	elseif(mode.eq.1) then
	  nnn_o=lndiv(1)
	  nnn_u=1
	  do ii=2,ndim
	    nnn_o=nnn_o*(lndiv(1)+ii-1)
	    nnn_u=nnn_u*ii
	  enddo
	  nseg=nnn_o/nnn_u
	elseif(mode.eq.2) then
	  nnn_o = lndiv(2)
	  nnn_u = 1
	  do ii=3,ndim
	    nnn_o=nnn_o*(lndiv(2)+ii-2)
	    nnn_u=nnn_u*(ii-1)
	  enddo
	  nseg = nnn_o/nnn_u
	endif

c	if(nseg.gt.MAXNSEG) then
c	   write(6,*) 'CMT_NEW: Number of segments is too large',nseg,MAXNSEG
c	   goto 100
c	endif

	if(mode.eq.0) then
	  nnn=ndim
	else
	  nnn=1
	endif
	nextra=1		! header record (1 block)
	nextra=nextra+1		! record for calibrations (1 block)
	nextra=nextra+nnn	! records for projections
	nextra=nextra+nnn	! records for backgrounds
	nextra=nextra+nnn	! records for efficiencies

	nsegtot=nseg+nextra
	if(ndim.eq.2) then
	  nnn=MINFLEN2D
	else
	  nnn=MINFLEN
	endif

	if(.not. ivf_new(filemat(1:lfn)//fext,nsegtot,nnn)) goto 100
	if(.not.ivf_open(filemat(1:lfn)//fext,1,handle)) goto 100

	do ii=1,128
	   head(ii)=0
	enddo
	head(128)=CMTVERSION
	head(127)=nsegtot
	head(126)=nextra
	head(125)=nseg
	head(124)=segsize
	head(119)=MAXCMODE		! Max # of bits/channel
	head(118)=MAXPARCAL		! # parametri di calibrazione

	head(1)=ndim			! # di dimensioni
	head(2)=mode			! 0=normal 1=symm 2=hsymm
	head(3)=DRECBITS		! # bits per disk record
	ip=4
	do ii=1,ndim
	  nnn=lres(ii)
	  head(ip)=nnn	! Risoluzione INDICE ii
	  ip=ip+1
	  head(ip)=lstep(ii)	! suddivisione lato ii
	  ip=ip+1
	  head(ip)=lndiv(ii)	! #blocchi sul lato ii
	  ip=ip+1
	enddo

#if defined( _GW_BIG_ENDIAN )
	do ii=1,DRECLONG
	  head_swp(ii)=head(ii)
	  call swap_bytes(head_swp(ii),4)
	enddo
	if(.not.ivf_write(%val(handle),head_swp(1),0,1)) goto 100
#else

	if(.not.ivf_write(%val(handle),head(1),0,1)) goto 100
#endif
	if(.not.ivf_close(%val(handle))) goto 100

	cmt_new=.true.
	return

100	cmt_new=.false.
	return

	end

	logical*1 function cmt_open(filemat,iomode,cmthandle)

#include "cmtlib.par"

	character filemat*(*)
	integer iomode			! 0=readonly 1=read/write

	MACHINE_POINTERSIZE cmthandle
	MACHINE_POINTERSIZE ivfhandle

	integer head(DRECLONG)
#if defined( _GW_BIG_ENDIAN )
	integer head_swp(DRECLONG)
#endif
	integer lfn,segsize,nn,ii,nrec,ndim,nseg,ivfnseg,cmtlen,matmode
CVMS	integer   ivfhandle
	integer nsegtot,nextra,ip,projelen,ivfversion
	character*5 fext

	integer  lengthc
	external lengthc

	logical	 ivf_open,ivf_read,ivf_close,ivf_info,getmem,freemem
	external ivf_open,ivf_read,ivf_close,ivf_info,getmem,freemem

	logical*1  ivf__get_old_head
	external ivf__get_old_head

	logical*1  cmt__open
	external cmt__open

	lfn=max(1,lengthc(filemat))
	if(index(filemat(1:lfn),'.').gt.0) then
	  fext=' '
	else
	  fext='.cmat'
	endif
	write(6,*) 'Opening    '//filemat(1:lfn)//fext

	if(.not.ivf_open(filemat(1:lfn)//fext,iomode,ivfhandle)) goto 100

	if(.not.ivf_info(%val(ivfhandle),-1,ivfnseg,ivfversion)) goto 99

	if(ivfversion.lt.5) then
#if defined( _GW_BIG_ENDIAN )
	  if(.not.ivf__get_old_head(%val(ivfhandle),head_swp)) goto 99
	  do ii=1,DRECLONG
	    head(ii)=head_swp(ii)
	    call swap_bytes(head(ii),4)
	  enddo
#else
	  if(.not.ivf__get_old_head(%val(ivfhandle),head)) goto 99
#endif
c	  segsize = head(2)	! canali per segmento
	  nseg=head(125)
	  nextra=0
	  nsegtot=nseg
	  if(head(7).lt.0) then
	    matmode=1
	  else
	    matmode=0
	  endif
	else
#if defined( _GW_BIG_ENDIAN )
	  if(.not.ivf_read(%val(ivfhandle),head_swp,0,nrec)) goto 99
	  do ii=1,DRECLONG
	    head(ii)=head_swp(ii)
	    call swap_bytes(head(ii),4)
	  enddo
#else
	  if(.not.ivf_read(%val(ivfhandle),head,0,nrec)) goto 99
#endif
c	  segsize = head(124)	! canali per segmento
	  nseg=head(125)
	  nextra=head(126)
	  nsegtot=head(127)
	  matmode=head(2)
	endif

	if(nseg.le.0) goto 99
	if(nsegtot .ne. ivfnseg) goto 99

	ndim  = head(1)
	if(ndim.lt.2 .or. ndim.gt.MAXMATDIM) then
	   write(6,*) 'CMT_OPEN: Illegal number of dimensions : ',ndim,MAXMATDIM
	  goto 99
	endif

c	if(segsize.lt.1 .OR. segsize.gt.MAXSEGSIZE) then
c	   write(6,*) 'CMT_OPEN: Illegal segment size',segsize,MAXSEGSIZE
c	   goto 99
c	endif

	if(matmode.lt.0 .or. matmode.gt.2) then
	  write(6,*) 'CMT_OPEN: Illegal matrix organisation',matmode
	  goto 99
	endif

	if(matmode.eq.0) then
	  projelen=0
	  ip=4
	  do ii=1,ndim
	    projelen=projelen+head(ip)
	    ip=ip+3
	  enddo
	elseif(matmode.ge.1) then
	  projelen=head(4)
	endif
	cmtlen= 64 + projelen
	if(.not.getmem(4*cmtlen,cmthandle)) then
	  write(6,*) 'CMT_OPEN: Error allocating memory',4*cmtlen
	  goto 99
	endif
	call lvect_erase(%val(cmthandle),cmtlen)

	if(.not.cmt__open(%val(cmthandle),%val(ivfhandle),head,cmtlen,matmode,iomode)) goto 99

D	call cmt_show(%val(cmthandle),6)

	cmt_open=.true.
	return

 99	if(.not.ivf_close(%val(ivfhandle))) then
	  continue
	endif
	if(cmtlen.gt.0) then
	  if(.not.freemem(4*cmtlen,cmthandle)) then
	    write(6,*) 'CMT_OPEN: Error releasing memory',4*cmtlen,cmthandle
	  endif
	endif
100	cmt_open=.false.
110	return

	end

	logical*1 function cmt__open(cmt,ivf,head,cmtlen,matmode,iomode)

#include "cmtlib.def"
	record/cmtdef/cmt

	integer ivf,head(DRECLONG)
	integer cmtlen,matmode,iomode		! 0=readonly, 1=read/write
	integer ii,ip,nn,nseg

	integer cmtid	/0/

	logical*1  ivf_info,getmem,ivf__get_old_descr
	external ivf_info,getmem,ivf__get_old_descr

	cmtid=cmtid+1

	if(.not.ivf_info(ivf,-1,nseg,nn)) goto 100

	cmt.version=head(128)
	cmt.ndim  = head(1)
	cmt.nseg=head(125)
	cmt.matmode=matmode
	if(cmt.version.lt.5) then
	  cmt.segsize = head(2)
	  cmt.nextra  = 0
	else
	  cmt.segsize = head(124)
	  cmt.nextra  = head(126)
	endif
	cmt.readonly=iomode.eq.0
	cmt.cmtid=cmtid
	cmt.nflush=0
	cmt.ivfbase=%loc(ivf)
	cmt.cmtlen=cmtlen

	ip=4
	do ii=1,cmt.ndim
	  cmt.res(ii)  =abs(head(ip))	! Risoluzione INDICE ii
	  ip=ip+1
	  cmt.step(ii) =abs(head(ip))	! suddivisione lato ii
	  ip=ip+1
	  cmt.ndiv(ii) =abs(head(ip))	! # di blocchi sul lato ii
	  ip=ip+1
	enddo
	if(cmt.version.lt.5 .and. cmt.matmode.gt.0) then
	  do ii=2,cmt.ndim
	    cmt.res(ii)=cmt.res(1)
	  enddo
	endif	

	cmt.incrmode=-1
	nn=0
	do ii=1,cmt.ndim
	  cmt.poff(ii)=nn
	  nn=nn+cmt.res(ii)
	enddo

	if(cmt.version .lt. 5) then
	  cmt.oldclen=((cmt.nseg+DRECLONG-1)/DRECLONG)*DRECLONG
	  if(.not.getmem(4*cmt.oldclen,cmt.oldcmode)) goto 100
	  call lvect_erase(%val(cmt.oldcmode),cmt.oldclen)
	  if(.not.getmem(4*cmt.oldclen,cmt.oldcminval)) goto 100
	  call lvect_erase(%val(cmt.oldcminval),cmt.oldclen)
	  if(.not.ivf__get_old_descr(ivf,%val(cmt.oldcmode),3)) goto 100
	  if(.not.ivf__get_old_descr(ivf,%val(cmt.oldcminval),4)) goto 100
	endif

	cmt__open=.true.
	return

100	cmt__open=.false.
	return

	end

	subroutine cmt_show(cmt,lun)

#include "cmtlib.def"
	record/cmtdef/cmt
	integer lun

	integer ii,last(MAXMATDIM)
	integer*8 ll
	integer*8 iad_nldim0,iad_sldim0
	
	character*2 nm_fmt

	if(lun.le.0) return
	if(cmt.ndim .lt.2 .or. cmt.ndim .gt. MAXMATDIM) return

	do ii=1,cmt.ndim
	  last(ii)=cmt.res(ii)-1
	end do
	if(cmt.matmode.eq.0) then
	   write(lun,'('' This is a '',i1,''D matrix'')')cmt.ndim
	   ll=iad_nldim0(last,cmt.res,cmt.ndim)+1
	elseif(cmt.matmode.eq.1) then
	   ll=iad_sldim0(last,cmt.ndim)+1
	   write(lun,'('' This is a '',i1,''D matrix (symmetrized)'')')cmt.ndim
	elseif(cmt.matmode.eq.2) then
	   ll=(iad_sldim0(last(2),cmt.ndim-1)+1)*cmt.res(1)
	   write(lun,'('' This is a '',i1,''D matrix (half_symmetrized)'')')cmt.ndim
	endif
	
	write(nm_fmt,'(i2)')cmt.ndim
	WRITE(lun,'('' Dimension = ('','//nm_fmt//'I6,'' ) =>'',i16)')(cmt.RES(ii),ii=1,cmt.ndim),ll
	WRITE(lun,'('' Blocksize = ('','//nm_fmt//'I6,'' ) =>'',i16)')(cmt.STEP(ii),ii=1,cmt.ndim),cmt.segsize
	write(lun,'('' #Segments =''I7)') cmt.nseg

	return

	end

	logical	function cmt_close(cmt,filemat)

#include "cmtlib.def"
	record/cmtdef/cmt
	character*(*) filemat

	integer ii,lfn
	character*5 fext

	integer  lengthc
	external lengthc

	logical*1  ivf_close,freemem
	external ivf_close,freemem

	lfn=lengthc(filemat)
	if(lfn.gt.0) then
	  if(index(filemat(1:lfn),'.').gt.0) then
	    fext=' '
	  else
	    fext='.cmat'
	  endif
	  write(6,*) 'Closing   '//filemat(1:lfn)//fext
	endif

	if(.not.ivf_close(%val(cmt.ivfbase))) goto 100

	if(cmt.oldclen.gt.0) then
	  if(.not.freemem(4*cmt.oldclen,cmt.oldcmode)) then
	    write(6,*) 'CMT_CLOSE: Error releasing  memory',4*cmt.oldclen,cmt.oldcmode
	  endif
	  if(.not.freemem(4*cmt.oldclen,cmt.oldcminval)) then
	    write(6,*) 'CMT_CLOSE: Error releasing  memory',4*cmt.oldclen,cmt.oldcminval
	  endif
	endif
	if(.not.freemem(4*cmt.cmtlen,%loc(cmt))) then
	  write(6,*) 'CMT_CLOSE: Error releasing  memory',4*cmt.cmtlen,%loc(cmt)
	  goto 100
	endif

	cmt_close=.true.
	return

100	cmt_close=.false.
	return

	end

	logical	function cmt_info(cmt,iseg,n1,n2)

#include "cmtlib.def"
	record/cmtdef/cmt

	integer iseg,n1(MAXMATDIM),n2(MAXMATDIM)

	integer ii

	logical*1  ivf_info
	external ivf_info

	cmt_info=.true.
	if(iseg.ge.0 .and. iseg.lt.cmt.nseg) then
	  cmt_info=ivf_info(%val(cmt.ivfbase),iseg+cmt.nextra,n1,n2)
	elseif(iseg.eq.-1) then
	  n1(1)=cmt.ndim
	  n2(1)=cmt.matmode
	elseif(iseg.eq.-2) then
	  do ii=1,cmt.ndim
	    n1(ii)=cmt.res(ii)
	    n2(ii)=cmt.step(ii)
	  enddo
	elseif(iseg.eq.-3) then
	  n1(1)=cmt.nseg
	  n2(1)=cmt.segsize
	elseif(iseg.eq.-4) then
	  n1(1)=cmt.cmode
	  n2(1)=cmt.cminval
	else
	  cmt_info=.false.
	endif

	return

	end

	logical	function cmt_infoincr(cmt,n1,n2)

#include "cmtlib.def"
	record/cmtdef/cmt

CVMS	integer   n1,n2
	integer*8 n1,n2

	n1=cmt.nincrement
	n2=cmt.nflush
	cmt_infoincr=.true.

	return

	end

	logical*1	function cmt__write(cmt,iseg,dat,nchan,nrec)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer dat(0:1)
	integer nchan,iseg,nrec

	integer mode,nbytes,minval

	logical*1  ivf_write,getmem,freemem
	external ivf_write,getmem,freemem

	if(nchan+2 .gt. cbuflen) then
	  if(cbuflen.gt.0) then
	    if(.not.freemem(4*cbuflen,cbufbase)) then
	      write(6,*) 'CMT__WRITE: Error releasing  memory',4*cbuflen,cbufbase
	      call exit
	    endif
	  endif
	  cbuflen=nchan+2
	  if(.not.getmem(4*cbuflen,cbufbase)) then
	    write(6,*) 'CMT__WRITE: Error allocating memory',4*cbuflen
	    call exit
	  endif
	endif

cccccccccc compressione

	call comp_compress(dat,nchan,%val(cbufbase+8),nbytes,mode,minval)
#if defined( _GW_BIG_ENDIAN )
	cmt.cmode=mode
	cmt.cminval=minval
	call swap_bytes(mode,4)
	call swap_bytes(minval,4)
	call put2LW(%val(cbufbase),mode,minval)
	call swap_bytes(mode,4)
	call swap_bytes(minval,4)
#else
	cmt.cmode=mode
	cmt.cminval=minval
	call put2LW(%val(cbufbase),mode,minval)
#endif
	if(mode.eq.0) then
	  if(minval.ne.0) then
	    nrec=1
	  else
	    nrec=0
	  endif
	else
	  nrec=(((nbytes+3)/4) + 2 + DRECLONG-1)/DRECLONG
	endif
	if(.not.ivf_write(%val(cmt.ivfbase),%val(cbufbase),iseg,nrec)) goto 100

	cmt__write=.true.
	return

100	cmt__write=.false.
	return

	end

	logical*1	function cmt__read(cmt,iseg,dat,nchan,nrec)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer dat(0:1)
	integer nchan,iseg,nrec

	integer nn,nbytes,lnchan

	integer oldcmode(0:1),oldcminval(0:1),bufbase(0:1)
	pointer (oldcmodepntr,oldcmode),(oldcminvalpntr,oldcminval),(bufpntr,bufbase)

	logical*1  ivf_read,getmem,freemem
	external ivf_read,getmem,freemem

	lnchan=max(256,nchan)    !?????????
	if(lnchan+2 .gt. cbuflen) then
	  if(cbuflen.gt.0) then
	    if(.not.freemem(4*cbuflen,cbufbase)) then
	      write(6,*) 'CMT__READ: Error releasing  memory',4*cbuflen,cbufbase
	      call exit
	    endif
	  endif
	  cbuflen=lnchan+2
	  if(.not.getmem(4*cbuflen,cbufbase)) then
	    write(6,*) 'CMT__READ: Error allocating memory',4*cbuflen
	    call exit
	  endif
	endif


	if(.not.ivf_read(%val(cmt.ivfbase),%val(cbufbase),iseg,nrec)) goto 100

cccccccccc decompressione

c	if(cmt.version.lt.5) then
c	  oldcmodepntr=cmt.oldcmode
c	  cmt.cmode  = oldcmode(iseg)
c*#ifdef sun
c*	call swap_bytes(cmt.cmode,4)
c*#endif
c	  oldcminvalpntr=cmt.oldcminval
c	  cmt.cminval= oldcminval(iseg)
c*#ifdef sun
c*	call swap_bytes(cmt.cminval,4)
c*#endif
c	  nn=0
c	elseif(nrec.eq.0) then
c	  cmt.cmode=0
c	  cmt.cminval=0
c	  nn=8
c	else
c	  bufpntr=cbufbase
c	  cmt.cmode  = bufbase(0)
c	  cmt.cminval= bufbase(1)
c#ifdef sun
c	  call swap_bytes(cmt.cmode,4)
c	  call swap_bytes(cmt.cminval,4)
c#endif
c	  nn=8
c	endif

	if(cmt.version.eq.5)then
	   if( nrec.gt.0 )then
	     bufpntr=cbufbase
	     cmt.cmode  = bufbase(0)
	     cmt.cminval= bufbase(1)
#if defined( _GW_BIG_ENDIAN )
	     call swap_bytes(cmt.cmode,4)
	     call swap_bytes(cmt.cminval,4)
#endif
	     nn=8
	   elseif( nrec.eq.0 )then
	     cmt.cmode=0
	     cmt.cminval=0
	     nn=8
	   else
	     goto 100
	   endif

	 elseif(cmt.version.lt.5)then
	   oldcmodepntr=cmt.oldcmode
	   cmt.cmode  = oldcmode(iseg)
	   oldcminvalpntr=cmt.oldcminval
	   cmt.cminval= oldcminval(iseg)
	   nn=0

	 else
	   goto 100
	 endif

	call comp_decompress(dat,nchan,%val(cbufbase+nn),nbytes,cmt.cmode,cmt.cminval)

	cmt__read=.true.
	return

100	cmt__read=.false.
	return

	end

	logical	function cmt_writesegment(cmt,iseg,dat,nrec)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer dat(0:1)
	integer iseg,nrec

	logical*1 cmt__write

	if(cmt.readonly) goto 100
	if(iseg.lt.0 .or. iseg.ge.cmt.NSEG) goto 100

	cmt_writesegment=cmt__write(cmt,iseg+cmt.nextra,dat,cmt.segsize,nrec)
	return

100	cmt_writesegment=.false.
	return

	end

	logical	function cmt_readsegment(cmt,iseg,dat,nrec)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer dat(0:1)
	integer iseg,nrec

	logical*1 cmt__read

	if(iseg.lt.0 .or. iseg.ge.cmt.NSEG) goto 100


	cmt_readsegment=cmt__read(cmt,iseg+cmt.nextra,dat,cmt.segsize,nrec)
	return

100	cmt_readsegment=.false.
	return

	end

	logical*1 function cmt_getproje(cmt,lato,spectrum)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer lato,spectrum(0:1)

	integer llato,nrec,frec,ii

	logical*1  cmt__read,ivf_info,ivf__get_old_rec
	external cmt__read,ivf_info,ivf__get_old_rec

	if(lato.lt.0 .or. lato.gt.cmt.ndim) goto 100

	if(cmt.matmode.eq.0) then
	  llato=lato
	else
	  llato=1
	endif

	if(cmt.version.lt.5) then
	  if(.not.ivf_info(%val(cmt.ivfbase),-5,nrec,frec)) goto 100
	  nrec=0
	  frec=frec+1		! for calibrations
	  do ii=1,llato
	    frec=frec+nrec
	    nrec=(cmt.res(ii)+DRECLONG-1)/DRECLONG
	  enddo
	  cmt_getproje=ivf__get_old_rec(%val(cmt.ivfbase),spectrum,nrec,frec)
#if defined( _GW_BIG_ENDIAN )
	  do ii=0,cmt.res(llato)-1
	    call swap_bytes(spectrum(ii),4)
	  enddo
#endif
	else
	  cmt_getproje=cmt__read(cmt,llato+PROJESEG,spectrum,cmt.res(llato),nrec)
	endif
	return

100	cmt_getproje=.false.
	return

	end

	logical*1 function cmt_putproje(cmt,lato,spectrum)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer lato,spectrum(0:1)

	integer llato,nrec

	logical*1 cmt__write
	integer ii

	if(cmt.readonly) goto 100
	if(lato.lt.0 .or. lato.gt.cmt.ndim) goto 100

	if(cmt.matmode.eq.0) then
	  llato=lato
	else
	  llato=1
	endif

!#ifdef sun
!	do ii=0,cmt.res(llato)-1
!	  call swap_bytes(spectrum(ii),4)
!	enddo
!#endif
	cmt_putproje=cmt__write(cmt,llato+PROJESEG,spectrum,cmt.res(llato),nrec)
!#ifdef sun
!	do ii=0,cmt.res(llato)-1
!	  call swap_bytes(spectrum(ii),4)
!	enddo
!#endif

	return

100	cmt_putproje=.false.
	return

	end
