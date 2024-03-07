	subroutine usersub5(flag)

	integer flag

#include "gsort.inc"
#include "polar.inc"
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

#include "gsort.inc"
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

	subroutine gs2_sortpol(mat)

#include "gsort.inc"
	record/matrix/mat

	if(init_mode.EQ.0) then
	  call gs_matopen(mat)
	endif

	return

	end


	subroutine gs3_sortpol(mat)

#include "gsort.inc"
#include "polar.inc"
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

