	logical*1 function cmt_readspec(cmt,gate,cut)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer gate(1:*),cut(0:*)

	integer ii,nn,specmode,cside

	logical*1 cmt__initbuffers
	logical*1 cmt__2dspec,cmt__2sspec,cmt__2hspec
	logical*1 cmt__3dspec,cmt__3sspec,cmt__3hspec
	logical*1 cmt__4dspec,cmt__4sspec,cmt__4hspec

	if(.not.cmt__initbuffers(cmt)) goto 100

	nn=0
	do ii=1,cmt.ndim
	  if(gate(ii).lt.0) then
	    nn=nn+1
	    cside=ii
	  else
	    if(gate(ii).lt.0 .OR. gate(ii).ge.cmt.res(ii) ) goto 100
	  endif
	end do
	if(nn.ne.1) goto 100
	if(cmt.matmode.gt.0 .AND. cside.ne.1) goto 100

	do ii=0,cmt.res(cside)
	  cut(ii)=0
	end do

	specmode=(MAXMATDIM-1)*cmt.matmode+cmt.ndim-1
	cmt_readspec=.false.

	goto(1,2,3,4,5,6,7,8,9) specmode
	goto 100

1	continue
	cmt_readspec=cmt__2dspec(cmt,gate,cut)
	return

2	continue
	cmt_readspec=cmt__3dspec(cmt,gate,cut)
	return

3	continue
	cmt_readspec=cmt__4dspec(cmt,gate,cut)
	return

4	continue
	cmt_readspec=cmt__2sspec(cmt,gate,cut)
	return

5	continue
	cmt_readspec=cmt__3sspec(cmt,gate,cut)
	return

6	continue
	cmt_readspec=cmt__4sspec(cmt,gate,cut)
	return

7	continue
c	cmt_readspec=cmt__2hspec(cmt,gate,cut)
	return

8	continue
c	cmt_readspec=cmt__3hspec(cmt,gate,cut)
	goto 100

9	continue
c	cmt_readspec=cmt__4hspec(cmt,gate,cut)
	goto 100

100	cmt_readspec=.false.
	return

	end

cpgi$r opt=1
	logical*1 function cmt_readspecs(cmt,gates,ngates,cut)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer gates(1:*),ngates,cut(0:*)

	integer ii,jj,ll,mm,nn,specmode,cside

	logical*1 cmt__initbuffers
	logical*1 cmt__2dcatch,cmt__2scatch,cmt__2hcatch
	logical*1 cmt__3dcatch,cmt__3scatch,cmt__3hcatch
	logical*1 cmt__4dcatch,cmt__4scatch,cmt__4hcatch

	if(ngates.lt.1) goto 100
	if(.not.cmt__initbuffers(cmt)) goto 100

	ll=1
	cside=0	
	do jj=1,ngates
	  nn=0
	  do ii=1,cmt.ndim
	    if(gates(ll).lt.0) then
	      nn=nn+1
	      if(jj.eq.1) then
	        cside=ii
	      else
	        if(ii.ne.cside) goto 100
	      endif
	    else
	      if(gates(ll).lt.0 .OR. gates(ll).ge.cmt.res(ii) ) goto 100
	    endif
	    ll=ll+1
	  end do
	  if(nn.ne.1) goto 100
	end do
	if(cmt.matmode.gt.0 .AND. cside.ne.1) goto 100

	do ii=0,cmt.res(cside)
	  cut(ii)=0
	end do

	specmode=(MAXMATDIM-1)*cmt.matmode+cmt.ndim-1
	cmt_readspecs=.false.

	goto(1,2,3,4,5,6,7,8,9) specmode
	goto 100

1	continue
	cmt_readspecs=cmt__2dcatch(cmt,gates,ngates,cut,%val(dbufbase),%val(tbufbase))
	return

2	continue
	cmt_readspecs=cmt__3dcatch(cmt,gates,ngates,cut,%val(dbufbase),%val(tbufbase))
	return

3	continue
	cmt_readspecs=cmt__4dcatch(cmt,gates,ngates,cut,%val(dbufbase),%val(tbufbase))
	return

4	continue
	cmt_readspecs=cmt__2scatch(cmt,gates,ngates,cut,%val(dbufbase),%val(tbufbase))
	return

5	continue
	cmt_readspecs=cmt__3scatch(cmt,gates,ngates,cut,%val(dbufbase),%val(tbufbase))
	return

6	continue
	cmt_readspecs=cmt__4scatch(cmt,gates,ngates,cut,%val(dbufbase),%val(tbufbase))
	return

7	continue
*	cmt_readspecs=cmt__2hcatch(cmt,gates,ngates,%val(dbufbase))
	return

8	continue
*	cmt_readspecs=cmt__3hcatch(cmt,gates,ngates,%val(dbufbase))
	return

9	continue
*	cmt_readspecs=cmt__4hcatch(cmt,gates,ngates,%val(dbufbase))
	return

100	cmt_readspecs=.false.
	return

	end

	logical*1 function cmt__initbuffers(cmt)

#include "cmtlib.inc"
	record/cmtdef/cmt

	logical*1 initmemclib /.TRUE./
	integer oldcmtid /-1/

	integer iok

	logical*1 getmem,freemem
	integer memc_init
	logical*1 cmt_readspec_reset

	if(tbuflen.le.0 .AND. cmt.matmode.lt.2) then	! allocate memory for TBUF
	  tbuflen=(TBUFSIZE*1024*1024)/4
	  if(.not.getmem(4*tbuflen,tbufbase)) then
	    write(6,*) 'CMT__INITBUFFERS: Error allocating memory',4*tbuflen
	    call exit
	  endif
	endif

	if(initmemclib) then
	  iok=memc_init(MEMCSIZE)
	  initmemclib=.FALSE.
	endif

	if(cmt.cmtid.ne.oldcmtid .or. cmt.ndim .lt.2 .or. cmt.ndim.gt.MAXMATDIM) then
	  iok=memc_init(MEMCSIZE)
	  if(cmt.ndim .lt.2 .or. cmt.ndim.gt.MAXMATDIM) then
	    cmt__initbuffers=.false.
	    oldcmtid=-1
	    return
	  endif
	endif

	oldcmtid=cmt.cmtid

	if(cmt.segsize .gt. dbuflen) then		! verify consistency of DBUF in /cmtcom/
	  if(dbuflen.gt.0) then
	    if(.not.freemem(4*dbuflen,dbufbase)) then
	      write(6,*) 'CMT__INITBUFFERS: Error releasing  memory',4*dbuflen,dbufbase
	      call exit
	    endif
	  endif
	  dbuflen=cmt.segsize
	  if(.not.getmem(4*dbuflen,dbufbase)) then
	    write(6,*) 'CMT__INITBUFFERS: Error allocating memory',4*dbuflen
	    call exit
	  endif
	endif

	cmt__initbuffers=.true.
	return

	entry cmt_readspec_reset

	cmt_readspec_reset= .false.

	if(initmemclib) return

	iok=memc_init(MEMCSIZE)
	initmemclib=.FALSE.

	return

	end

	logical*1 function cmt__2dspec(cmt,gate,cut)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer gate(2)
	integer cut(*)

	integer nngg
	PARAMETER (NNGG=128)
	integer gates(2,NNGG)

	integer cside,cres,cstep
	integer gside,gres,gstep,gval
	integer ll1,ll1a,ll1b
	integer ii,jj,ll,nn,id
	integer ngg,npp,ntt

	logical*1 cmt__2dcatch
	integer iad_ndim0
	integer memc_get

	if(gate(1).lt.0) then
	  cside=1
	  gside=2
	else
	  cside=2
	  gside=1
	endif

	gate(cside)=0
	id=iad_ndim0(gate,cmt.res,2)
	gate(cside)=-1
	jj=memc_get(cut,id)
	if(jj.ge.0) then		! already present
	  cmt__2dspec=.true.
	  return
	endif

CCCCCCCCCC   need to read spectra

	cres = cmt.res(cside)
	cstep= cmt.step(cside)
	gres = cmt.res(gside)
	gstep= cmt.step(gside)
	gval =gate(gside)

	ll1b=min( gval + NNGG , (gval/gstep)*gstep + gstep-1 )
	ll1a=max( ll1b - NNGG , (gval/gstep)*gstep           )

	ngg=0
	do ll1=ll1a,ll1b
	  ngg=ngg+1
	  gates(gside,ngg)=ll1
	  gates(cside,ngg)=-1
	end do

	if(.not.cmt__2dcatch(cmt,gates,ngg,cut,%val(dbufbase),%val(tbufbase))) goto 100

	jj=memc_get(cut,id)
	if(jj.ge.0) then
	  cmt__2dspec=.true.
	  return
	endif

100	cmt__2dspec=.false.
	return

	end

	logical*1 function cmt__2dcatch(cmt,gates,ngates,cut,dati,tbuf)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer ngates
	integer gates(2,*)
	integer cut(0:*)
	integer dati(0:*)
	integer tbuf(0:*)

	integer cside,cres,cstep
	integer gside,gres,gstep
	integer id,iaddr,ival,idl
	integer seg(2),lstep(2),ind(2)
	integer nspecmax,nn1,nn2
	integer ii,jj,ll,mm,nn,it,ndrec,kkk
	integer ngg,npp,ntt
	logical*1 catchit

	logical*1 cmt_readsegment
	integer iad_ndim0
	integer memc_put,memc_get

	if(gates(1,1).lt.0) then
	  cside=1
	  gside=2
	else
	  cside=2
	  gside=1
	endif

	cres = cmt.res(cside)
	cstep= cmt.step(cside)
	gstep= cmt.step(gside)
	nspecmax=tbuflen/cres

	ngg=0
	do ii=1,ngates
	  gates(cside,ii)=0
	  id=iad_ndim0(gates,cmt.res,2)
	  jj=memc_get(tbuf,id)
	  if(jj.ge.0) then		! already present
	    do jj=0,cres-1
	      cut(jj)=cut(jj)+tbuf(jj)
	    end do
	    gates(cside,ii)=-1
	  else
	    ngg=ngg+1
	    seg(1)=gates(1,ii)/cmt.step(1)
	    seg(2)=gates(2,ii)/cmt.step(2)
	    ll=iad_ndim0(seg,cmt.ndiv,2)
	    gates(cside,ii)=ll
	  endif
	end do
	if(ngg.eq.0) goto 50

	call cmt__ordlk(gates,cside,2,ngates)

	lstep(1)=1
	lstep(2)=cmt.step(1)

	mm=ngates-ngg+1
	do while(mm.le.ngates)
	  seg(1)=gates(1,mm)/cmt.step(1)
	  seg(2)=gates(2,mm)/cmt.step(2)
	  seg(cside)=0

	  nn1=mm
	  nn2=mm
	  ll=gates(cside,mm)
	  do while(nn2.lt.ngates .AND. gates(cside,nn2+1).eq.ll)
	    nn2=nn2+1
	  end do

	  do ii=0,cmt.ndiv(cside)-1
	    it=iad_ndim0(seg,cmt.ndiv,2)
	    if(.not.cmt_readsegment(cmt,it,dati,ndrec)) goto 100
	    do nn=nn1,nn2
	      catchit= (nn-nn1) .LT. nspecmax
	      ll=(nn-nn1)*cres
	      ind(1)=gates(1,nn)
	      ind(2)=gates(2,nn)
	      ind(cside)=seg(cside)*cstep
	      iaddr=mod(ind(1),cmt.step(1))+mod(ind(2),cmt.step(2))*lstep(2)
	      do jj=ind(cside),ind(cside)+cstep-1
	        ival=dati(iaddr)
	        cut(jj)=cut(jj)+ival
	        if(catchit) tbuf(jj+ll)=ival
	        iaddr=iaddr+lstep(cside)
	      end do
	    end do
	    seg(cside)=seg(cside)+1
	  end do

	  do  nn=nn1,nn2
	    if( (nn-nn1) .LT. nspecmax ) then
	      ll=(nn-nn1)*cres
	      gates(cside,nn)=0
	      id=iad_ndim0(gates(1,nn),cmt.res,2)

c	      write(6,*) nn,id
c	      do ii=0,cres-1,8
c	        write(6,'(i5,8i12)') ii,(tbuf(kkk),kkk=ii,ii+7)
c	      enddo

	      jj=memc_put(tbuf(ll),cres,id)
	    endif
	    gates(cside,nn)=-1
	  end do
	  mm=nn2+1
	end do

50	cmt__2dcatch=.true.
	return

100	cmt__2dcatch=.false.
	return

	end

	logical*1 function cmt__3dspec(cmt,gate,cut)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer gate(3)
	integer cut(*)

	integer nngg
	PARAMETER (NNGG=12)
	integer gates(2,NNGG*NNGG)

	integer cside,cres,cstep
	integer gside(2),gres(2),gstep(2),gval(2)
	integer seg(3),lstep(3),ind(3)
	integer id
	integer ii,jj,ll,nn
	integer ll1,ll1a,ll1b,ll2,ll2a,ll2b
	integer ngg,npp,ntt

	logical*1 cmt__3dcatch
	integer iad_ndim0
	integer memc_get

	ll=1
	do ii=1,3
	  if(gate(ii).ge.0) then
	    gside(ll)=ii
	    ll=ll+1
	  else
	    cside=ii
	  endif
	end do
	
	gate(cside)=0
	id=iad_ndim0(gate,cmt.res,3)
	gate(cside)=-1
	jj=memc_get(cut,id)
	if(jj.ge.0) then
	  cmt__3dspec=.true.
	  return
	endif

CCCCCCCCCC   need to read spectra

	cres =cmt.res(cside)
	cstep=cmt.step(cside)
	do ii=1,2
	  gres(ii) =cmt.res(gside(ii))
	  gstep(ii)=cmt.step(gside(ii))
	  gval(ii) =gate(gside(ii))
	enddo

	ll1b=min( gval(1) + NNGG , (gval(1)/gstep(1))*gstep(1) + gstep(1)-1 )
	ll1a=max(   ll1b  - NNGG , (gval(1)/gstep(1))*gstep(1)              )

	ll2b=min( gval(2) + NNGG , (gval(2)/gstep(2))*gstep(2) + gstep(2)-1 )
	ll2a=max(   ll2b  - NNGG , (gval(2)/gstep(2))*gstep(2)              )

	ngg=0
	do ll2=ll2a,ll2b
	do ll1=ll1a,ll1b
	  ngg=ngg+1
	  gates(cside,ngg)=0
	  gates(gside(1),ngg)=ll1
	  gates(gside(2),ngg)=ll2
	  ll=iad_ndim0(gates(1,ngg),cmt.res,3)
	  gates(cside,ngg)=-1
	end do
	end do

	if(.not.cmt__3dcatch(cmt,gates,ngg,cut,%val(dbufbase),%val(tbufbase))) goto 100

	jj=memc_get(cut,id)
	if(jj.ge.0) then
	  cmt__3dspec=.true.
	  return
	endif

100	cmt__3dspec=.false.
	return

	end

	logical*1 function cmt__3dcatch(cmt,gates,ngates,cut,dati,tbuf)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer ngates
	integer gates(3,*)
	integer cut(0:*)
	integer dati(0:*)
	integer tbuf(0:*)

	integer cside,cres,cstep
	integer id,iaddr,ival,idl
	integer seg(3),lstep(3),ind(3)
	integer nspecmax,nn1,nn2
	integer ii,jj,ll,mm,nn,it,ndrec
	integer ngg,npp,ntt
	logical*1 catchit

	logical*1 cmt_readsegment
	integer iad_ndim0
	integer memc_put,memc_get

	if(gates(1,1).lt.0) then
	  cside=1
	elseif(gates(2,1).lt.0) then
	  cside=2
	else
	  cside=3
	endif

	cres = cmt.res(cside)
	cstep= cmt.step(cside)
	nspecmax=tbuflen/cres

	ngg=0
	do ii=1,ngates
	  gates(cside,ii)=0
	  id=iad_ndim0(gates,cmt.res,3)
	  jj=memc_get(tbuf,id)
	  if(jj.ge.0) then		! already present
	    do jj=0,cres-1
	      cut(jj)=cut(jj)+tbuf(jj)
	    end do
	    gates(cside,ii)=-1
	  else
	    ngg=ngg+1
	    seg(1)=gates(1,ii)/cmt.step(1)
	    seg(2)=gates(2,ii)/cmt.step(2)
	    seg(3)=gates(3,ii)/cmt.step(3)
	    ll=iad_ndim0(seg,cmt.ndiv,3)
	    gates(cside,ii)=ll
	  endif
	end do
	if(ngg.eq.0) goto 50

	call cmt__ordlk(gates,cside,3,ngates)

	lstep(1)=1
	lstep(2)=cmt.step(1)
	lstep(3)=cmt.step(2)*lstep(2)

	mm=ngates-ngg+1
	do while(mm.le.ngates)
	  seg(1)=gates(1,mm)/cmt.step(1)
	  seg(2)=gates(2,mm)/cmt.step(2)
	  seg(3)=gates(3,mm)/cmt.step(3)
	  seg(cside)=0

	  nn1=mm
	  nn2=mm
	  ll=gates(cside,mm)
	  do while(nn2.lt.ngates .AND. gates(cside,nn2+1).eq.ll)
	    nn2=nn2+1
	  end do

	  do ii=0,cmt.ndiv(cside)-1
	    it=iad_ndim0(seg,cmt.ndiv,3)
	    if(.not.cmt_readsegment(cmt,it,dati,ndrec)) goto 100
	    do nn=nn1,nn2
	      catchit= (nn-nn1) .LT. nspecmax
	      ll=(nn-nn1)*cres
	      ind(1)=gates(1,nn)
	      ind(2)=gates(2,nn)
	      ind(3)=gates(3,nn)
	      ind(cside)=seg(cside)*cstep
	      iaddr=mod(ind(1),cmt.step(1))+mod(ind(2),cmt.step(2))*lstep(2)+
	1                                   mod(ind(3),cmt.step(3))*lstep(3)
	      do jj=ind(cside),ind(cside)+cstep-1
	        ival=dati(iaddr)
	        cut(jj)=cut(jj)+ival
	        if(catchit) tbuf(jj+ll)=ival
	        iaddr=iaddr+lstep(cside)
	      end do
	    end do
	    seg(cside)=seg(cside)+1
	  end do

	  do  nn=nn1,nn2
	    if( (nn-nn1) .LT. nspecmax ) then
	      ll=(nn-nn1)*cres
	      gates(cside,nn)=0
	      id=iad_ndim0(gates(1,nn),cmt.res,3)
	      jj=memc_put(tbuf(ll),cres,id)
	    endif
	    gates(cside,nn)=-1
	  end do
	  mm=nn2+1
	end do

50	cmt__3dcatch=.true.
	return

100	cmt__3dcatch=.false.
	return

	end

	logical*1 function cmt__4dspec(cmt,gate,cut)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer gate(4)
	integer cut(*)

	integer nngg
	PARAMETER (NNGG=5)
	integer gates(3,NNGG*NNGG*NNGG)

	integer cside,cres,cstep
	integer gside(3),gres(3),gstep(3),gval(3)
	integer id
	integer ii,jj,ll,nn
	integer ll1,ll1a,ll1b,ll2,ll2a,ll2b,ll3,ll3a,ll3b
	integer ngg,npp,ntt

	logical*1 cmt__4dcatch
	integer iad_ndim0
	integer memc_get

	ll=1
	do ii=1,4
	  if(gate(ii).ge.0) then
	    gside(ll)=ii
	    ll=ll+1
	  else
	    cside=ii
	  endif
	end do
	
	gate(cside)=0
	id=iad_ndim0(gate,cmt.res,4)
	gate(cside)=-1
	jj=memc_get(cut,id)
	if(jj.ge.0) then
	  cmt__4dspec=.true.
	  return
	endif

CCCCCCCCCC   need to read spectra

	cres =cmt.res(cside)
	cstep=cmt.step(cside)
	do ii=1,3
	  gres(ii) =cmt.res(gside(ii))
	  gstep(ii)=cmt.step(gside(ii))
	  gval(ii) =gate(gside(ii))
	enddo

	ll1b=min( gval(1) + NNGG , (gval(1)/gstep(1))*gstep(1) + gstep(1)-1 )
	ll1a=max(   ll1b  - NNGG , (gval(1)/gstep(1))*gstep(1)              )

	ll2b=min( gval(2) + NNGG , (gval(2)/gstep(2))*gstep(2) + gstep(2)-1 )
	ll2a=max(   ll2b  - NNGG , (gval(2)/gstep(2))*gstep(2)              )

	ll3b=min( gval(3) + NNGG , (gval(3)/gstep(3))*gstep(3) + gstep(3)-1 )
	ll3a=max(   ll3b  - NNGG , (gval(3)/gstep(3))*gstep(3)              )

	ngg=0
	do ll3=ll3a,ll3b
	do ll2=ll2a,ll2b
	do ll1=ll1a,ll1b
	  ngg=ngg+1
	  gates(cside,ngg)=0
	  gates(gside(1),ngg)=ll1
	  gates(gside(2),ngg)=ll2
	  gates(gside(3),ngg)=ll3
	  ll=iad_ndim0(gates(1,ngg),cmt.res,4)
	  gates(cside,ngg)=-1
	end do
	end do
	end do

	if(.not.cmt__4dcatch(cmt,gates,ngg,cut,%val(dbufbase),%val(tbufbase))) goto 100

	jj=memc_get(cut,id)
	if(jj.ge.0) then
	  cmt__4dspec=.true.
	  return
	endif

100	cmt__4dspec=.false.
	return

	end

	logical*1 function cmt__4dcatch(cmt,gates,ngates,cut,dati,tbuf)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer ngates
	integer gates(4,*)
	integer cut(0:*)
	integer dati(0:*)
	integer tbuf(0:*)

	integer cside,cres,cstep
	integer id,iaddr,ival,idl
	integer seg(4),lstep(4),ind(4)
	integer nspecmax,nn1,nn2
	integer ii,jj,ll,mm,nn,it,ndrec
	integer ngg,npp,ntt
	logical*1 catchit

	logical*1 cmt_readsegment
	integer iad_ndim0
	integer memc_put,memc_get

	if(gates(1,1).lt.0) then
	  cside=1
	elseif(gates(2,1).lt.0) then
	  cside=2
	elseif(gates(3,1).lt.0) then
	  cside=3
	else
	  cside=4
	endif

	cres = cmt.res(cside)
	cstep= cmt.step(cside)
	nspecmax=tbuflen/cres

	ngg=0
	do ii=1,ngates
	  gates(cside,ii)=0
	  id=iad_ndim0(gates,cmt.res,4)
	  jj=memc_get(tbuf,id)
	  if(jj.ge.0) then		! already present
	    do jj=0,cres-1
	      cut(jj)=cut(jj)+tbuf(jj)
	    end do
	    gates(cside,ii)=-1
	  else
	    ngg=ngg+1
	    seg(1)=gates(1,ii)/cmt.step(1)
	    seg(2)=gates(2,ii)/cmt.step(2)
	    seg(3)=gates(3,ii)/cmt.step(3)
	    seg(4)=gates(4,ii)/cmt.step(4)
	    ll=iad_ndim0(seg,cmt.ndiv,3)
	    gates(cside,ii)=ll
	  endif
	end do
	if(ngg.eq.0) goto 50

	call cmt__ordlk(gates,cside,4,ngates)

	lstep(1)=1
	lstep(2)=cmt.step(1)
	lstep(3)=cmt.step(2)*lstep(2)
	lstep(4)=cmt.step(3)*lstep(3)

	mm=ngates-ngg+1
	do while(mm.le.ngates)
	  seg(1)=gates(1,mm)/cmt.step(1)
	  seg(2)=gates(2,mm)/cmt.step(2)
	  seg(3)=gates(3,mm)/cmt.step(3)
	  seg(4)=gates(4,mm)/cmt.step(4)
	  seg(cside)=0

	  nn1=mm
	  nn2=mm
	  ll=gates(cside,mm)
	  do while(nn2.lt.ngates .AND. gates(cside,nn2+1).eq.ll)
	    nn2=nn2+1
	  end do

	  do ii=0,cmt.ndiv(cside)-1
	    it=iad_ndim0(seg,cmt.ndiv,4)
	    if(.not.cmt_readsegment(cmt,it,dati,ndrec)) goto 100
	    do nn=nn1,nn2
	      catchit= (nn-nn1) .LT. nspecmax
	      ll=(nn-nn1)*cres
	      ind(1)=gates(1,nn)
	      ind(2)=gates(2,nn)
	      ind(3)=gates(3,nn)
	      ind(4)=gates(4,nn)
	      ind(cside)=seg(cside)*cstep
	      iaddr=mod(ind(1),cmt.step(1))+mod(ind(2),cmt.step(2))*lstep(2)+
	1                                   mod(ind(3),cmt.step(3))*lstep(3)+
	1                                   mod(ind(4),cmt.step(4))*lstep(4)
	      do jj=ind(cside),ind(cside)+cstep-1
	        ival=dati(iaddr)
	        cut(jj)=cut(jj)+ival
	        if(catchit) tbuf(jj+ll)=ival
	        iaddr=iaddr+lstep(cside)
	      end do
	    end do
	    seg(cside)=seg(cside)+1
	  end do

	  do  nn=nn1,nn2
	    if( (nn-nn1) .LT. nspecmax ) then
	      ll=(nn-nn1)*cres
	      gates(cside,nn)=0
	      id=iad_ndim0(gates(1,nn),cmt.res,4)
	      jj=memc_put(tbuf(ll),cres,id)
	    endif
	    gates(cside,nn)=-1
	  end do
	  mm=nn2+1
	end do

50	cmt__4dcatch=.true.
	return

100	cmt__4dcatch=.false.
	return

	end

	logical*1 function cmt__2sspec(cmt,gate,cut)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer gate(2)
	integer cut(*)

	integer nngg
	PARAMETER (NNGG=128)
	integer gates(2,NNGG)

	integer res,step
	integer id,gval
	integer ll1,ll1a,ll1b
	integer ii,jj,ll,mm,nn
	integer ngg,npp,ntt

	logical*1 cmt__2scatch
	integer iad_sdim0
	integer memc_get

	gval=gate(2)
	id=gval
	jj=memc_get(cut,id)
	if(jj.ge.0) then		! already present
	  cmt__2sspec=.true.
	  return
	endif

CCCCCCCCCC   need to read spectra

	res=cmt.res(1)
	step=cmt.step(1)

	ll1b=min( gval + NNGG , (gval/step)*step + step-1 )
	ll1a=max( ll1b - NNGG , (gval/step)*step          )

	ngg=0
	do ll1=ll1a,ll1b
	  ngg=ngg+1
	  gates(1,ngg)=-1
	  gates(2,ngg)=ll1
	end do

	if(.not.cmt__2scatch(cmt,gates,ngg,cut,%val(dbufbase),%val(tbufbase))) goto 100

	jj=memc_get(cut,id)
	if(jj.ge.0) then
	  cmt__2sspec=.true.
	  return
	endif

100	cmt__2sspec=.false.
	return

	end

	logical*1 function cmt__2scatch(cmt,gates,ngates,cut,dati,tbuf)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer ngates
	integer gates(2,*)
	integer cut(0:*)
	integer dati(0:*)
	integer tbuf(0:*)

	integer res,step
	integer iaddr,it,ival
	integer cside,iside
	integer ii,jj,kk,ll,mm,nn,ndrec
	integer nspecmax,nn1,nn2,ngg,id
	integer seg(3),lstep(3),ind(3)
	logical*1 catchit

	logical*1 cmt_readsegment
	integer iad_sdim0
	integer memc_put,memc_get

	res=cmt.res(1)
	step=cmt.step(1)
	nspecmax=tbuflen/res

	ngg=0
	do ii=1,ngates
	  id=gates(2,ii)
	  jj=memc_get(tbuf,id)
	  if(jj.ge.0) then		! already present
	    do jj=0,res-1
	      cut(jj)=cut(jj)+tbuf(jj)
	    end do
	    gates(1,ii)=-1
	  else
	    ngg=ngg+1
	    gates(1,ii)=gates(2,ii)/step
	  endif
	end do
	if(ngg.eq.0) goto 50

	call cmt__ordlk(gates,1,2,ngates)

	lstep(1)=1
	lstep(2)=step
	ind(3)=res+step

	mm=ngates-ngg+1
	do while(mm.le.ngates)
	  seg(1)=0
	  seg(2)=gates(2,mm)/step
	  seg(3)=res+step

	  nn1=mm
	  nn2=mm
	  ll=gates(1,mm)
	  do while(nn2.lt.ngates .AND. gates(1,nn2+1).eq.ll)
	    nn2=nn2+1
	  end do

	  cside=1
	  do ii=0,cmt.ndiv(1)-1
	    it=iad_sdim0(seg,2)
	    if(.not.cmt_readsegment(cmt,it,dati,ndrec)) goto 100

	    do nn=nn1,nn2
	      catchit= (nn-nn1) .LT. nspecmax
	      if(cside.eq.1) then
	        ind(1)=seg(1)*step
	        ind(2)=gates(2,nn)
	      else
	        ind(1)=gates(2,nn)
	        ind(2)=seg(2)*step
	      endif
	      iaddr=mod(ind(1),step)+mod(ind(2),step)*lstep(2)
	      ll=(nn-nn1)*res
	      if(seg(cside).lt.seg(cside+1)) then
	        do jj=ind(cside),ind(cside)+step-1
	          ival=dati(iaddr)
	          cut(jj)=cut(jj)+ival
	          if(catchit) tbuf(jj+ll)=ival
	          iaddr=iaddr+lstep(cside)
	        end do
	      else
	        iside=cside
	        jj=0
	        do while(jj.lt.step)
	          ival=dati(iaddr)
	          if(ind(iside).lt.ind(iside+1))then
	            cut(ind(iside))=cut(ind(iside))+ival
	            if(catchit) tbuf(ll+ind(iside))=ival
	          else
		    ival=ival*2
	            cut(ind(iside))=cut(ind(iside))+ival
	            if(catchit) tbuf(ll+ind(iside))=ival
	            iside=iside+1
	          endif
	          ind(iside)=ind(iside)+1
	          iaddr=iaddr+lstep(iside)
	          jj=jj+1
	        end do
	      endif
	    end do
	    if(cside.eq.1 .AND. seg(1).eq.seg(2)) cside=2
	    seg(cside)=seg(cside)+1
	  end do

	  do nn=nn1,nn2
	    if( (nn-nn1) .LT. nspecmax ) then
	      ll=(nn-nn1)*res
	      id=gates(2,nn)
	      jj=memc_put(tbuf(ll),res,id)
	    endif
	    gates(1,nn)=-1
	  end do
	  mm=nn2+1
	end do

50	cmt__2scatch=.true.
	return

100	cmt__2scatch=.false.
	return

	end

	logical*1 function cmt__3sspec(cmt,gate,cut)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer gate(3)
	integer cut(*)

	integer nngg
	PARAMETER (NNGG=12)
	integer gates(3,NNGG*NNGG)

	integer res,step
	integer id,gval(2)
	integer ll1,ll1a,ll1b,ll2,ll2a,ll2b
	integer ii,jj,ll,mm,nn
	integer ngg,npp,ntt

	logical*1 cmt__3scatch
	integer iad_sdim0
	integer memc_get

	gval(1)=gate(2)
	gval(2)=gate(3)
	call ordl(gval,2)

	id=iad_sdim0(gval,2)
	jj=memc_get(cut,id)
	if(jj.ge.0) then
	  cmt__3sspec=.true.
	  return
	endif

CCCCCCCCCC   need to read spectra

	res=cmt.res(1)
	step=cmt.step(1)

	ll1b=min( gval(1)+ NNGG , (gval(1)/step)*step + step-1 )
	ll1a=max(  ll1b  - NNGG , (gval(1)/step)*step          )

	ll2b=min( gval(2)+ NNGG , (gval(2)/step)*step + step-1 )
	ll2a=max(  ll2b  - NNGG , (gval(2)/step)*step          )

	ngg=0
	do ll2=ll2a,ll2b
	do ll1=ll1a,ll1b
	  if(ll1.le.ll2) then
	    ngg=ngg+1
	    gates(1,ngg)=-1
	    gates(2,ngg)=ll1
	    gates(3,ngg)=ll2
	  endif
	end do
	end do

	if(.not.cmt__3scatch(cmt,gates,ngg,cut,%val(dbufbase),%val(tbufbase))) goto 100

	jj=memc_get(cut,id)
	if(jj.ge.0) then
	  cmt__3sspec=.true.
	  return
	endif

100	cmt__3sspec=.false.
	return

	end

	logical*1 function cmt__3scatch(cmt,gates,ngates,cut,dati,tbuf)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer ngates
	integer gates(3,*)
	integer cut(0:*)
	integer dati(0:*)
	integer tbuf(0:*)

	integer res,step
	integer iaddr,it,ival
	integer cside,iside
	integer ii,jj,kk,ll,mm,nn,ndrec
	integer nspecmax,nn1,nn2,ngg,id
	integer seg(4),lstep(4),ind(4)
	integer peso
	integer mpeso(2) /2,3/
	logical*1 catchit

	logical*1 cmt_readsegment
	integer iad_sdim0,iad_sfactor
	integer memc_put,memc_get

	res=cmt.res(1)
	step=cmt.step(1)
	nspecmax=tbuflen/res

	ngg=0
	do ii=1,ngates
	  call ordl(gates(2,ii),2)
	  id=iad_sdim0(gates(2,ii),2)
	  jj=memc_get(tbuf,id)
	  if(jj.ge.0) then		! already present
	    do jj=0,res-1
	      cut(jj)=cut(jj)+tbuf(jj)
	    end do
	    gates(1,ii)=-1
	  else
	    ngg=ngg+1	  
	    seg(1)=gates(2,ii)/step
	    seg(2)=gates(3,ii)/step
	    gates(1,ii)=iad_sdim0(seg,2)
	  endif
	end do
	if(ngg.eq.0) goto 50

	call cmt__ordlk(gates,1,3,ngates)

	lstep(1)=1
	lstep(2)=step
	lstep(3)=step*step
	ind(4)=res+step

	mm=ngates-ngg+1
	do while(mm.le.ngates)
	  seg(1)=0
	  seg(2)=gates(2,mm)/step
	  seg(3)=gates(3,mm)/step
	  seg(4)=res+step

	  nn1=mm
	  nn2=mm
	  ll=gates(1,mm)
	  do while(nn2.lt.ngates .AND. gates(1,nn2+1).eq.ll)
	    nn2=nn2+1
	  end do

	  cside=1
	  do ii=0,cmt.ndiv(1)-1
	    it=iad_sdim0(seg,3)
	    if(.not.cmt_readsegment(cmt,it,dati,ndrec)) goto 100

	    do nn=nn1,nn2
	      catchit= (nn-nn1) .LT. nspecmax
	      if(cside.eq.1) then
	        ind(1)=seg(1)*step
	        ind(2)=gates(2,nn)
	        ind(3)=gates(3,nn)
	      elseif(cside.eq.2) then
	        ind(1)=gates(2,nn)
	        ind(2)=seg(2)*step
	        ind(3)=gates(3,nn)
	      else
	        ind(1)=gates(2,nn)
	        ind(2)=gates(3,nn)
	        ind(3)=seg(3)*step
	      endif
	      iaddr=mod(ind(1),step)+mod(ind(2),step)*lstep(2)+
	1                            mod(ind(3),step)*lstep(3)
	      ll=(nn-nn1)*res
	      peso=iad_sfactor(gates(2,nn),2)
	      if(seg(cside).lt.seg(cside+1)) then
	        do jj=ind(cside),ind(cside)+step-1
	          ival=dati(iaddr)*peso
	          cut(jj)=cut(jj)+ival
	          if(catchit) tbuf(jj+ll)=ival
	          ind(cside)=ind(cside)+1
	          iaddr=iaddr+lstep(cside)
	        end do
	      else
	        iside=cside
	        jj=0
	        do while(jj.lt.step)
	          ival=dati(iaddr)*peso
	          if(ind(iside).lt.ind(iside+1))then
	            cut(ind(iside))=cut(ind(iside))+ival
	            if(catchit) tbuf(ll+ind(iside))=ival
	          else
	            ival=ival*mpeso(peso)
	            cut(ind(iside))=cut(ind(iside))+ival
	            if(catchit) tbuf(ll+ind(iside))=ival
	            if(ind(iside+1).lt.ind(iside+2))then
	              iside=iside+1
	            else
	              iside=iside+2
	            endif
	          endif
	          ind(iside)=ind(iside)+1
	          iaddr=iaddr+lstep(iside)
	          jj=jj+1
	        end do
	      endif
	    end do
	    if(cside.eq.1 .AND. seg(1).eq.seg(2)) cside=2
	    if(cside.eq.2 .AND. seg(2).eq.seg(3)) cside=3
	    seg(cside)=seg(cside)+1
	  end do

	  do nn=nn1,nn2
	    if( (nn-nn1) .LT. nspecmax ) then
	      ll=(nn-nn1)*res
	      id=iad_sdim0(gates(2,nn),2)
	      jj=memc_put(tbuf(ll),res,id)
	    endif
	    gates(1,nn)=-1
	  end do
	  mm=nn2+1
	end do

50	cmt__3scatch=.true.
	return

100	cmt__3scatch=.false.
	return

	end

	logical*1 function cmt__4sspec(cmt,gate,cut)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer gate(4)
	integer cut(*)

	integer nngg
	PARAMETER (NNGG=5)
	integer gates(4,NNGG*NNGG*NNGG)

	integer res,step
	integer id,gval(3)
	integer ll1,ll1a,ll1b,ll2,ll2a,ll2b,ll3,ll3a,ll3b
	integer ii,jj,ll,mm,nn
	integer ngg,npp,ntt

	logical*1 cmt__4scatch
	integer iad_sdim0
	integer memc_get

	gval(1)=gate(2)
	gval(2)=gate(3)
	gval(3)=gate(4)
	call ordl(gval,3)

	id=iad_sdim0(gval,3)
	jj=memc_get(cut,id)
	if(jj.ge.0) then
	  cmt__4sspec=.true.
	  return
	endif

CCCCCCCCCC   need to read spectra

	res=cmt.res(1)
	step=cmt.step(1)

	ll1b=min( gval(1)+ NNGG , (gval(1)/step)*step + step-1 )
	ll1a=max(  ll1b  - NNGG , (gval(1)/step)*step          )

	ll2b=min( gval(2)+ NNGG , (gval(2)/step)*step + step-1 )
	ll2a=max(  ll2b  - NNGG , (gval(2)/step)*step          )

	ll3b=min( gval(3)+ NNGG , (gval(3)/step)*step + step-1 )
	ll3a=max(  ll3b  - NNGG , (gval(3)/step)*step          )

	ngg=0
	do ll3=ll3a,ll3b
	do ll2=ll2a,ll2b
	do ll1=ll1a,ll1b
	  if(ll1.le.ll2 .AND. ll2.le.ll3) then
	    ngg=ngg+1
	    gates(1,ngg)=-1
	    gates(2,ngg)=ll1
	    gates(3,ngg)=ll2
	    gates(4,ngg)=ll3
	  endif
	end do
	end do
	end do

	if(.not.cmt__4scatch(cmt,gates,ngg,cut,%val(dbufbase),%val(tbufbase))) goto 100

	jj=memc_get(cut,id)
	if(jj.ge.0) then
	  cmt__4sspec=.true.
	  return
	endif

100	cmt__4sspec=.false.
	return

	end

	logical*1 function cmt__4scatch(cmt,gates,ngates,cut,dati,tbuf)

#include "cmtlib.inc"
	record/cmtdef/cmt

	integer ngates
	integer gates(4,*)
	integer cut(0:*)
	integer dati(0:*)
	integer tbuf(0:*)

	integer res,step
	integer iaddr,it,ival
	integer cside,iside
	integer ii,jj,kk,ll,mm,nn,ndrec
	integer nspecmax,nn1,nn2,ngg,id
	integer seg(5),lstep(5),ind(5)
	integer peso,ipeso
	integer mpeso(6) /2,3,2,1,1,4/
	logical*1 catchit

	logical*1 cmt_readsegment
	integer iad_sdim0,iad_sfactor
	integer memc_put,memc_get

	res=cmt.res(1)
	step=cmt.step(1)
	nspecmax=tbuflen/res

	ngg=0
	do ii=1,ngates
	  call ordl(gates(2,ii),3)
	  id=iad_sdim0(gates(2,ii),3)
	  jj=memc_get(tbuf,id)
	  if(jj.ge.0) then		! already present
	    do jj=0,res-1
	      cut(jj)=cut(jj)+tbuf(jj)
	    end do
	    gates(1,ii)=-1
	  else
	    ngg=ngg+1	  
	    seg(1)=gates(2,ii)/step
	    seg(2)=gates(3,ii)/step
	    seg(3)=gates(4,ii)/step
	    gates(1,ii)=iad_sdim0(seg,3)
	  endif
	end do
	if(ngg.eq.0) goto 50

	call cmt__ordlk(gates,1,4,ngates)

	lstep(1)=1
	lstep(2)=step
	lstep(3)=step*step
	lstep(4)=step*step*step
	ind(5)=res+step

	mm=ngates-ngg+1
	do while(mm.le.ngates)
	  seg(1)=0
	  seg(2)=gates(2,mm)/step
	  seg(3)=gates(3,mm)/step
	  seg(4)=gates(4,mm)/step
	  seg(5)=res+step

	  nn1=mm
	  nn2=mm
	  ll=gates(1,mm)
	  do while(nn2.lt.ngates .AND. gates(1,nn2+1).eq.ll)
	    nn2=nn2+1
	  end do

	  cside=1
	  do ii=0,cmt.ndiv(1)-1
	    it=iad_sdim0(seg,4)
	    if(.not.cmt_readsegment(cmt,it,dati,ndrec)) goto 100
	    do nn=nn1,nn2
	      catchit= (nn-nn1) .LT. nspecmax
	      if(cside.eq.1) then
	        ind(1)=seg(1)*step
	        ind(2)=gates(2,nn)
	        ind(3)=gates(3,nn)
	        ind(4)=gates(4,nn)
	      elseif(cside.eq.2) then
	        ind(1)=gates(2,nn)
	        ind(2)=seg(2)*step
	        ind(3)=gates(3,nn)
	        ind(4)=gates(4,nn)
	      elseif(cside.eq.3) then
	        ind(1)=gates(2,nn)
	        ind(2)=gates(3,nn)
	        ind(3)=seg(3)*step
	        ind(4)=gates(4,nn)
	      else
	        ind(1)=gates(2,nn)
	        ind(2)=gates(3,nn)
	        ind(3)=gates(4,nn)
	        ind(4)=seg(4)*step
	      endif
	      iaddr=mod(ind(1),step)+mod(ind(2),step)*lstep(2)+
	1                            mod(ind(3),step)*lstep(3)+
	1                            mod(ind(4),step)*lstep(4)
	      ll=(nn-nn1)*res
	      peso=iad_sfactor(gates(2,nn),3)
	      if(seg(cside).lt.seg(cside+1)) then
	        do jj=ind(cside),ind(cside)+step-1
	          ival=dati(iaddr)*peso
	          cut(jj)=cut(jj)+ival
	          if(catchit) tbuf(jj+ll)=ival
	          ind(cside)=ind(cside)+1
	          iaddr=iaddr+lstep(cside)
	        end do
	      else
	        iside=cside
	        jj=0
	        do while(jj.lt.step)
	          ival=dati(iaddr)*peso
	          if(ind(iside).lt.ind(iside+1))then
	            cut(ind(iside))=cut(ind(iside))+ival
	            if(catchit) tbuf(ll+ind(iside))=ival
	          else
	  	    ipeso=peso
	            if(peso.eq.2 .and. ind(2).ne.ind(3)) ipeso=3
		    ival=ival*mpeso(ipeso)
	            cut(ind(iside))=cut(ind(iside))+ival
	            if(catchit) tbuf(ll+ind(iside))=ival
	            if(ind(iside+1).lt.ind(iside+2))then
	              iside=iside+1
	            elseif(ind(iside+2).lt.ind(iside+3))then
	              iside=iside+2
	            else
	              iside=iside+3
	            endif
	          endif
	          ind(iside)=ind(iside)+1
	          iaddr=iaddr+lstep(iside)
	          jj=jj+1
	        end do
	      endif
	    end do
	    if(cside.eq.1 .AND. seg(1).eq.seg(2)) cside=2
	    if(cside.eq.2 .AND. seg(2).eq.seg(3)) cside=3
	    if(cside.eq.3 .AND. seg(3).eq.seg(4)) cside=4
	    seg(cside)=seg(cside)+1
	  end do

	  do nn=nn1,nn2
	    if( (nn-nn1) .LT. nspecmax ) then
	      ll=(nn-nn1)*res
	      id=iad_sdim0(gates(2,nn),3)
	      jj=memc_put(tbuf(ll),res,id)
	    endif
	    gates(1,nn)=-1
	  end do
	  mm=nn2+1
	end do

50	cmt__4scatch=.true.
	return

100	cmt__4scatch=.false.
	return

	end

	subroutine cmt__ordlk(ia,ik,ord,nn)

c	ordina ia(ord,nn) in modo crescente usando ia(ik,ii) come key
c	usa straight insert method

	integer maxord
	PARAMETER (MAXORD=10)

	integer*4 ik,ord,nn
	integer*4 ia(ord,nn)
	integer*4 ix(MAXORD)
	integer key,ii,jj,kk,ll,lm,lr

	if(nn.lt.2) return
	if(ord.gt.MAXORD) stop 'CMT__ORDLK: Order is too high'

c	do ii=1,nn
c	  write(7,'(i5,i10,6i6)') ii,(ia(jj,ii),jj=1,ord)
c	end do
	do ii=1,nn
	  do jj=1,ord
	    ix(jj)=ia(jj,ii)
	  end do
	  key=ix(ik)
	  ll=1				! cerca il punto di inserzione
	  lr=ii-1
	  do while (ll.le.lr)
	    lm=(ll+lr)/2
	    if(key.ge.ia(ik,lm)) then
	      ll=lm+1
	    else
	      lr=lm-1
	    endif
	  end do
	  do kk=ii,ll+1,-1		! shift
	    do jj=1,ord
	      ia(jj,kk)=ia(jj,kk-1)
	    end do
	  end do
	  do jj=1,ord			! insert
	    ia(jj,ll)=ix(jj)
	  end do
	end do
c	do ii=1,nn
c	  write(8,'(i5,i10,6i6)') ii,(ia(jj,ii),jj=1,ord)
c	end do

	return

	end
