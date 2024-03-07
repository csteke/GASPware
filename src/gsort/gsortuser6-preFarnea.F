	subroutine usersub6(flag)

	integer flag

#include "gsort.inc"
#include "polar.inc"
	record/addbpol/abpl
	
	common/addbpol/abpl
	record/matrix/mat

	    if(flag.eq.0) then			! from GETINPUT
		call gs0_spol2d(mat)
	elseif(flag.eq.1) then			! from LISTPROG
		call gs1_spol2d(mat)
	elseif(flag.eq.2) then			! from INIT_RUN
		call gs2_spol2d(mat)
	elseif(flag.eq.3) then			! from EVANA
		call gs3_spol2d(mat)
	elseif(flag.eq.4) then			! from FINIT_RUN
		call gs4_spol2d(mat)
	endif

	return

	end
	subroutine gs0_spol2d(mat)

#include "gsort.inc"
	record/matrix/mat
	
	synt(1)='usersub6  gi g1  matrixname [RES Rx Ry] [STEP Sx Sy] [Hn [#times]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=2
CCCC	call gs_getmatmode(mat.matmode,mat.msdim)

	do nn=1,mat.ndim
	  call gs_getindpar(mat.ind(nn),mat.par(nn),mat.res(nn))
	  mat.step(nn)=min(mat.res(nn),DEFSTEP2D)
	end do
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

	mat.type=-1

	if(mat.nind.EQ.1) then
	  call gs_countvals(mat.par(1),2,nvals)
	  if(nvals.eq.1) then
	    mat.type=1				! AaAa
	  elseif(nvals.eq.2) then
	    mat.type=2				! AaAb
	  endif

	elseif(mat.nind.EQ.2) then
	  mat.type=3				! AxBy
	endif

	mat.hsame=.FALSE.! AaBx Fix mat.hsame for pol. 
	mat.type=3      ! AaBx Fix mat.type for pol.

	if(mat.type.le.0) call gs_syntax('Do not know how to treat this combination of parameters')

	mat.ind1=mat.ind(1)
	mat.ind2=mat.ind(2)

	mat.par1=mat.par(1)
	mat.par2=mat.par(2)

	mat.hhh1=mat.ind1 .EQ. mat.hind
	mat.hhh2=mat.ind2 .EQ. mat.hind

	mat.matmode=0

	return

	end


	subroutine gs1_spol2d(mat)

#include "gsort.inc"
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

	subroutine gs2_spol2d(mat)

#include "gsort.inc"
	record/matrix/mat

	if(init_mode.EQ.0) then
	  call gs_matopen(mat)
	endif

	return

	end

	subroutine gs3_spol2d(mat)

#include "gsort.inc"
#include "polar.inc"
	record/addbpol/abpl
	
	common/addbpol/abpl
	
	record/matrix/mat

	integer incr(2)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue.LT.nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	goto (10,20,30) mat.type

10	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l-1					! AaAa
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	    incr(2)=det(j2).ival(mat.par1)
	    call cmt_2dincr(%val(mat.cmtbase),incr)
	  end do
	end do
	return

20	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l					! AaAb
	  incr(1)=det(j1).ival(mat.par1)
	    incr(2)=det(j1).ival(mat.par2)
	    call cmt_2dincr(%val(mat.cmtbase),incr)
	end do
	return

30	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
c	do j1=j1f,j1l					! AxBy
	  do j2=j2f,j2l
	    incr(1)=abpl.polar(det(j2).id)*1000+det(j2).id
	    incr(2)=det(j2).ival(mat.par2)
c	    write(6,*)' Incr : ',incr
    	    call cmt_2dincr(%val(mat.cmtbase),incr)
	  end do
c	end do
	return

1000	goto(1010,1020,1030) mat.type

1010	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l-1					! AaAa
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1011
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1012
	    incr(2)=det(j2).ival(mat.par1)
	    call cmt_2dincr(%val(mat.cmtbase),incr)
1012	  end do
1011	end do
	return

1020	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l					! AaAb
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1021
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  call cmt_2dincr(%val(mat.cmtbase),incr)
1021	end do
	return

1030	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBy
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1031
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1031
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    call cmt_2dincr(%val(mat.cmtbase),incr)
1032	  end do
1031	end do

8000	return

	end

	subroutine gs4_spol2d(mat)

#include "gsort.inc"
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



