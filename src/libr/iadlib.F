d	program iad_test
d
d	integer iv(10),id(10)
d
d10	call inp_i2('dim,count',ndim,nincr)
d	call inp_ia('res(1...dim)',id,ndim)
d
d	write(6,*)
d	write(6,*) 'Nincr0'
d	do ii=1,ndim
d	  iv(ii)=0
d	enddo
d	do ii=1,nincr
d	  write(6,'(9i8)') ii-1,iad_ndim0(iv,id,ndim),(iv(jj),jj=1,ndim),iad_sfactor(iv,ndim)
d	  call iad_nincr0(iv,id,ndim)
d	enddo
d	write(6,*) iad_nldim0(iv,id,ndim)
d
d	write(6,*)
d	write(6,*) 'Nincr1'
d	do ii=1,ndim
d	  iv(ii)=1
d	enddo
d	do ii=1,nincr
d	  write(6,'(9i8)') ii,iad_ndim1(iv,id,ndim),(iv(jj),jj=1,ndim),iad_sfactor(iv,ndim)
d	  call iad_nincr1(iv,id,ndim)
d	enddo
d
d	write(6,*)
d	write(6,*) 'Sincr0'
d	do ii=1,ndim
d	  iv(ii)=0
d	enddo
d	do ii=1,nincr
d	  write(6,'(9i8)') ii-1,iad_sdim0(iv,ndim),(iv(jj),jj=1,ndim),iad_sfactor(iv,ndim)
d	  call iad_sincr0(iv,ndim)
d	enddo
d	write(6,*) iad_sldim0(iv,ndim)
d
d	write(6,*)
d	write(6,*) 'Sincr1'
d	do ii=1,ndim
d	  iv(ii)=1
d	enddo
d	do ii=1,nincr
d	  write(6,'(9i8)') ii,iad_sdim1(iv,ndim),(iv(jj),jj=1,ndim),iad_sfactor(iv,ndim)
d	  call iad_sincr1(iv,ndim)
d	enddo
d
d	goto 10
d
d	end

	integer function iad_ndim0(vect,res,dim)

	integer vect(*),res(*),dim
	integer addr

	addr=vect(dim)
	do ii=dim-1,1,-1
	  addr=vect(ii)+res(ii)*addr
	enddo
	iad_ndim0=addr

	return

	end

	integer*8 function iad_nldim0(vect,res,dim)

	integer vect(*),res(*),dim
	integer*8 addr

	addr=vect(dim)
	do ii=dim-1,1,-1
	  addr=vect(ii)+res(ii)*addr
	enddo
	iad_nldim0=addr

	return

	end

	integer function iad_ndim1(vect,res,dim)

	integer vect(*),res(*),dim
	integer addr

	addr=vect(dim)-1
	do ii=dim-1,1,-1
	  addr=(vect(ii)-1)+res(ii)*addr
	enddo
	iad_ndim1=addr+1

	return

	end

	integer function iad_sdim0(vect,dim)

	integer vect(*),dim
	integer ii,kk,addr

	addr=vect(1)
	do ii=2,dim
	  k1=vect(ii)+ii-1
	  kk=k1	  
	  do jj=2,ii
	    kk=(kk*(k1-jj+1))/jj
	  enddo
	  addr=addr+kk
	enddo
	iad_sdim0=addr

	return

	end

	integer*8 function iad_sldim0(vect,dim)

	integer vect(*),dim
	integer ii,jj
	integer*8 addr,kk

	addr=vect(1)
	do ii=2,dim
	  k1=vect(ii)+ii-1
	  kk=k1	  
	  do jj=2,ii
	    kk=(kk*(k1-jj+1))/jj
	  enddo
	  addr=addr+kk
	enddo
	iad_sldim0=addr

	return

	end

	integer function iad_sdim1(vect,dim)

	integer vect(*),dim
	integer ii,kk,addr

	addr=vect(1)
	do ii=2,dim
	  k1=vect(ii)+ii-2
	  kk=k1
	  do jj=2,ii
	    kk=(kk*(k1-jj+1))/jj
	  enddo
	  addr=addr+kk
	enddo
	iad_sdim1=addr

	return

	end

	integer function iad_sfactor(vect,dim)

	integer vect(*),dim
	integer ii,count,value

	count=1
	value=1
	do ii=2,dim
	  if(vect(ii).eq.vect(ii-1)) then
	    count=count+1
	    value=value*count
	  else
	    count=1
	  endif
	enddo
	iad_sfactor=value

	return

	end

	subroutine iad_nincr0(vect,res,dim)

	integer vect(*),res(*),dim
	integer ii

	do ii=1,dim	
	  vect(ii)=vect(ii)+1
	  if(vect(ii).ge.res(ii)) then
	    vect(ii)=0
	  else
	    return
	  endif
	enddo

	return

	end

	subroutine iad_nincr1(vect,res,dim)

	integer vect(*),res(*),dim
	integer ii

	do ii=1,dim	
	  vect(ii)=vect(ii)+1
	  if(vect(ii).gt.res(ii)) then
	    vect(ii)=1
	  else
	    return
	  endif
	enddo

	return

	end

	subroutine iad_sincr0(vect,dim)

	integer vect(*),dim
	integer ii

	vect(1)=vect(1)+1
	do ii=2,dim	
	  if(vect(ii-1).gt.vect(ii)) then
	    vect(ii-1)=0
	    vect(ii)=vect(ii)+1
	  else
	    return
	  endif
	enddo
	return

	end

	subroutine iad_sincr1(vect,dim)

	integer vect(*),dim
	integer ii

	vect(1)=vect(1)+1
	do ii=2,dim	
	  if(vect(ii-1).gt.vect(ii)) then
	    vect(ii-1)=1
	    vect(ii)=vect(ii)+1
	  else
	    return
	  endif
	enddo
	return

	end
