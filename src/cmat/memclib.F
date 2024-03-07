D	PROGRAM memctest
D
D	PARAMETER MAXRES=64*1024		! Maximum spec_Dimension
D
D	CHARACTER SPECFILE*40
D
D	INTEGER DATI(0:MAXRES-1)
D	INTEGER DATN(0:MAXRES-1)
D
D	megabytes=1
D	call inp_i1('memsize (Mbytes)',megabytes)
D	ii=memc_init(megabytes)
D	write(6,*) ii,'  mem_init'
D
D	ires=2048
D	ifrm=4
D
D10	call readdatl(1,specfile,dati,ires,ifrm,kv)
D	if(kv.le.0) goto 10
D	ifrm=mod(kv,100)
D	ires=kv/100
D	isize=ires
D
D	call inp_i1('#save',nsave)
D	lsize=isize
D
D	lstart=0
D	do ii=0,nsave-1
D	  jj=memc_put(dati(lstart),lsize,ii)		
D	  if(jj.lt.0) then		! esiste gia
D	    jj=memc_remove(ii)
D	    if(jj.lt.0) stop 'jj1'
D	    jj=memc_put(dati(lstart),lsize,ii)		
D	    if(jj.lt.0) stop 'jj2'
D	  endif
D	enddo
D
D	call memc_check
D	call inp_i1('id to recall',id)
D	do ii=0,isize-1
D	  datn(ii)=0
D	enddo
D	jj=memc_get(datn,id)		
D	if(jj.lt.0) then
D	  write(6,*) id,jj
D	  goto 10
D	endif
D
D	do ii=0,isize-1
D	  if(datn(ii).ne.dati(ii)) write(6,*)ii,dati(ii),datn(ii)
D	enddo
D	goto 10
D
D	END

	integer function memc_init(megabytes)

c	allocates space for internal buffers
c	if megabytes<=0 deallocates 
c
c	return values
c
c	>0	alloc ok memc_init=nbytes
c	 0	dealloc ok
c	-1	was not allocated
c	-2	was already allocated : memory resetted
c	-3	  alloc failed
c	-4	dealloc failed

#include "memclib.inc"

	integer megabytes

	logical*1 getmem,freemem
	external getmem,freemem

	if(megabytes.gt.0) then		! want to allocate
	  if(memc_size.gt.0) then
	    call memc__reset
	    memc_init=-2		! was already initialized: resetted
	    return
	  endif
	  memc_size=megabytes*1024*1024
	  if(.not.getmem(memc_size,memc_base))then
	    memc_init=-3		! alloc failed
	    return
	  endif
	  call memc__reset
	  memc_init=memc_size
	  memc_size=memc_size/4
	else				! want to deallocate
	  if(memc_size.gt.0) then
	    IF(.NOT.freemem(memc_size*4,memc_base)) then
	      memc_init=-4		! dealloc failed
	    else
	      call memc__reset
	      memc_init=0		! dealloc ok
	    endif
	  else
	    memc_init=-1		! was not allocated
	  endif
	endif
	
	return

	end

	integer function memc_find(id)

c	find the position of item id
c
c	return values
c
c	 >=0	position
c	-1	was not allocated
c	-2	not found

#include "memclib.inc"

	integer id
	integer ii,nn

	if(memc_size.le.0) then
	  memc_find=-1				! non inizializzata
	  return
	endif

	if(memc_nseg.gt.0) then
	  nn=memc_first
	  do ii=0,memc_nseg-1
	    if(mmc(nn).id .EQ. id) then
	      memc_find=nn
	      return
	    else
	      nn=mmc(nn).next
	    endif
	  enddo
	endif
	memc_find=-2

	return

	end

	integer function memc_put(data,isize,id)

c	insert (compressed) data identified by id
c
c	return values
c
c	 1	ok
c	-1	was not allocated
c	-2	id is already present
c	-3	insufficent space
c	-4	could not find space

#include "memclib.inc"

	integer data,isize,id
	integer memc_find
	integer nn,nbytes,nlw

	MACHINE_POINTERSIZE istart

	if(memc_size.le.0) then
	  memc_put=-1				! non inizializzata
	  return
	endif

	nn=memc_find(id)
	if(nn.ge.0) then
	  memc_put=-2				! esiste gia'
	  return
	endif

	if(isize.eq.0) then
	  nbytes=0
	else
	  call comp_compress(data,isize,memc_buf(2),nbytes,memc_buf(0),memc_buf(1))
	endif

	nlw= 2 + (nbytes+3)/4
	if(nlw.gt.memc_size) then
	  memc_put=-3
	  return
	endif

	if(memc_nseg.ge.MAXNSEG-1) then
	  call memc__kill_oldest	! there must be at least one free entry
	endif

	call memc__findspace(nlw,nn)

	istart=memc_base+4*mmc(nn).start
	call memc__copy(memc_buf,%val(istart),nlw)
	mmc(nn).size=isize
	mmc(nn).id=id
	memc_times=memc_times+1
	mmc(nn).times=memc_times
	memc_put=1

	return

	end


	integer function memc_get(data,id)

c	get (decompressed) data identified by id
c
c	return values
c
c	 1	ok
c	-1	was not allocated
c	-2	id is not present

#include "memclib.inc"

	integer data,id
	integer memc_find
	integer nn,nbytes

	MACHINE_POINTERSIZE istart

	if(memc_size.le.0) then
	  memc_get=-1				! non inizializzata
	  return
	endif

	nn=memc_find(id)
	if(nn.lt.0) then
	  memc_get=-2				! id non presente
	  return
	endif

	istart=memc_base+4*mmc(nn).start
	call comp_decompress(data,mmc(nn).size,%val(istart+8),nbytes,%val(istart),%val(istart+4))
	memc_times=memc_times+1
	mmc(nn).times=memc_times
	memc_get=1

	return

	end

	subroutine memc_check

#include "memclib.inc"

	integer ii,nn

	if(memc_nseg.le.0) then
	  write(6,*) 'que empty'
	  return
	endif

	write(6,*) memc_nseg,' entries',memc_used,' space used'
	write(6,*) memc_first,' first ',memc_last,' last'

	nn=memc_first
	do ii=0,memc_nseg-1
	  write(6,'(i7,9i8)')
	1 nn,
	1 mmc(nn).prev,
	1 mmc(nn).next,
	1 mmc(nn).start,
	1 mmc(nn).end,
	1 mmc(nn).end-mmc(nn).start+1,
	1 mmc(nn).id,
	1 mmc(nn).times,
	1 memc_mmc(nn)
	  nn=mmc(nn).next
	enddo

	return

	end

	integer function memc_remove(id)

c	remove id from buffers
c
c	return values
c
c	 1	ok
c	-1	was not allocated
c	-2	id is not present

#include "memclib.inc"

	integer id
	integer memc_find
	integer nn

	if(memc_size.le.0) then
	  memc_remove=-1				! non inizializzata
	  return
	endif

	nn=memc_find(id)
	if(nn.lt.0) then
	  memc_remove=-2
	  return
	endif

	call memc__remque(nn)
	memc_remove=1

	return

	end

	subroutine memc__reset

#include "memclib.inc"

	integer ii

	memc_nseg=0
	memc_used=0
	memc_times=0
	memc_first=-1
	memc_last=-1
	do ii=0,MAXNSEG-1
	  memc_mmc(ii)=-1
	enddo

	return

	end

	subroutine memc__remque(nn)

#include "memclib.inc"

	integer nn
	integer nnp,nnx,ispace

	if(memc_nseg.gt.1) then
	  nnp=mmc(nn).prev
	  nnx=mmc(nn).next
	  if(nnp.ge.0) then
	    mmc(nnp).next=nnx
	  else
	    memc_first=nnx
	  endif
	  if(nnx.ge.0) then
	    mmc(nnx).prev=nnp
	  else
	    memc_last=nnp
	  endif
	  ispace=mmc(nn).end - mmc(nn).start + 1
	  memc_used=memc_used-ispace
	  memc_nseg=memc_nseg-1
	  memc_mmc(nn)=-1
	else
	  call memc__reset
	endif

	return

	end

	subroutine memc__insque(after,nn)

#include "memclib.inc"

	integer after,nn
	integer before

	if(memc_nseg.lt.1) then
	  nn=0
	  memc_first=0
	  memc_last=0
	  mmc(nn).next=-1
	  mmc(nn).prev=-1
	  memc_mmc(nn)=0
	  memc_nseg=1
	  return
	elseif(memc_nseg.lt.MAXNSEG-1) then
	  do nn=0,MAXNSEG-1
	    if(memc_mmc(nn).lt.0) then
	      if(after.eq.-1) then
		before=memc_first
		mmc(nn).prev=-1
		mmc(nn).next=before
		mmc(before).prev=nn
		memc_first=nn
	      elseif(after.eq.memc_last) then
		mmc(nn).prev=after
		mmc(nn).next=-1
		mmc(after).next=nn
		memc_last=nn
	      else
		before=mmc(after).next
		mmc(nn).prev=after
		mmc(nn).next=before
		mmc(after).next=nn
		mmc(before).prev=nn
	      endif
	      memc_mmc(nn)=nn  
	      memc_nseg=memc_nseg+1
	      return
	    endif
	  enddo
	endif

	stop 'Space full in memc__insque'

	end

	subroutine memc__kill_oldest

#include "memclib.inc"

	integer ii,nn,kk,ntimes

	if(memc_nseg.le.1) then
	   call memc__reset
	   return
	endif

	nn=memc_first
	kk=nn
	ntimes=memc_times+1
	do ii=0,memc_nseg-1
	  if(mmc(nn).times .LT. ntimes) then
	    kk=nn
	    ntimes=mmc(nn).times
	  endif
	  nn=mmc(nn).next
	enddo

	call memc__remque(kk)

	return

	end

	subroutine memc__findspace(nlw,nn)

#include "memclib.inc"

	integer nlw,nn
	integer ii,after,space,minfree,start

	if(memc_nseg.lt.1) then
	  call memc__insque(-1,nn)
	  mmc(nn).start=0
	  mmc(nn).end=nlw-1
	  memc_used=nlw
	  return
	endif

	if(memc_size-memc_used .GE. nlw) then
	  if(mmc(memc_first).start .ge. nlw) then
	    call memc__insque(-1,nn)
	    mmc(nn).start=0
	    mmc(nn).end=nlw-1
	    memc_used=memc_used+nlw
	    return
	  endif
	  after=memc_first
	  do ii=1,memc_nseg-1
	    space=mmc(mmc(after).next).start - mmc(after).end -1
	    if(space .GE. nlw) then
	      start=mmc(after).end+1
	      call memc__insque(after,nn)
	      mmc(nn).start=start
	      mmc(nn).end=start+nlw-1
	      memc_used=memc_used+nlw
	      return
	    endif
	    after=mmc(after).next
	  enddo
	  if( memc_size - mmc(memc_last).end .gt. nlw) then
	    start=mmc(memc_last).end+1
	    call memc__insque(memc_last,nn)
	    mmc(nn).start=start
	    mmc(nn).end=start+nlw-1
	    memc_used=memc_used+nlw
	    return
	  endif
	endif

	minfree=MINPERCENT*(memc_size/100.)
	minfree=max(nlw,minfree)
	call memc__repack(minfree)

	start=mmc(memc_last).end+1
	call memc__insque(memc_last,nn)
	mmc(nn).start=start
	mmc(nn).end=start+nlw-1
	memc_used=memc_used+nlw
	return

	end

	subroutine memc__repack(minfree)

#include "memclib.inc"

	integer minfree
	integer ii,nn,ipos,nlw
*	integer memc__copy

	if(minfree.ge.memc_size) then
	  call memc__reset
	  return
	endif

	dowhile ( (memc_size-memc_used) .LT. minfree )
	   call memc__kill_oldest
	enddo

	ipos=0
	nn=memc_first
	do ii=0,memc_nseg-1
	  if(mmc(nn).start .EQ. ipos ) then
	    ipos=mmc(nn).end +1
	  else
	    nlw=mmc(nn).end - mmc(nn).start+1
	    call memc__copy(%val(memc_base+4*mmc(nn).start),
	1                   %val(memc_base+4*ipos),nlw)
	    mmc(nn).start=ipos
	    mmc(nn).end=ipos+nlw-1
	    ipos=ipos+nlw
	  endif
	  nn=mmc(nn).next
	enddo

	return

	end

	subroutine memc__copy(buf1,buf2,nlw)

	IMPLICIT NONE

	integer buf1(*),buf2(*),l1,l2,nlw
	integer ii

	do ii=1,nlw
	  buf2(ii)=buf1(ii)
	enddo

	return

	end
