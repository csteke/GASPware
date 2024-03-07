	subroutine usersub7(flag)

	integer flag

#include "gsort.inc"

	structure /diffgate/
	    integer*4 ind
	    integer*4 par
	    integer*4 res
	    real*4    delow
	    real*4    dehigh
	    integer*4 mm
	    real*4    elow
	    real*4    ehigh
	    integer*4 fl
	    integer*4 fh
	 end structure
	record/diffgate/gate
	save gate

	
	    if(flag.eq.0) then			! from GETINPUT
	       call gs0_diffgate(gate)
	elseif(flag.eq.1) then			! from LISTPROG
	       call gs1_diffgate(gate)
	elseif(flag.eq.2) then			! from INIT_RUN
	       call gs2_diffgate(gate)
	elseif(flag.eq.3) then			! from EVANA
	       call gs3_diffgate(gate)
	elseif(flag.eq.4) then			! from FINIT_RUN
	       call gs4_diffgate(gate)
	endif

	return

	end


	subroutine gs0_diffgate(gate)

#include "gsort.inc"
	structure /diffgate/
	    integer*4 ind
	    integer*4 par
	    integer*4 res
	    real*4    delow
	    real*4    dehigh
	    integer*4 mm
	    real*4    elow
	    real*4    ehigh
	    integer*4 fl
	    integer*4 fh
	 end structure
	record/diffgate/gate
	logical*1 same

	logical*1 sign

	synt(1)='usersub7 Pn  dELow dEHigh MM ELow EHigh  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(gate.ind,gate.par,gate.res)
	call gs_get2ilim(jj1,jj2,0,gate.res-1)
	gate.delow=jj1
	gate.dehigh=jj2
	nn=istr_i1(comline,gate.mm)
	if(nn.ne.1) call gs_syntax('Wrong maximum multiple value')
	call gs_get2ilim(jj1,jj2,0,gate.res-1)
	gate.elow=jj1
	gate.ehigh=jj2
	call gs_getfoldgate(gate.ind,gate.fl,gate.fh,same)
	if(gate.fl .lt. 2)gate.fl=2

	return

	end

	subroutine gs1_diffgate(gate)

#include "gsort.inc"
	structure /diffgate/
	    integer*4 ind
	    integer*4 par
	    integer*4 res
	    real*4    delow
	    real*4    dehigh
	    integer*4 mm
	    real*4    elow
	    real*4    ehigh
	    integer*4 fl
	    integer*4 fh
	 end structure
	record/diffgate/gate
	logical*1 same

	call gs_putindpar(gate.ind,gate.par)

	write(list(llist+1:),'(2f10.2,i5,2f10.2)') gate.delow,gate.dehigh,gate.mm,gate.elow,gate.ehigh

	llist=lengthc(list)+2
	call gs_putfoldgate(gate.ind,gate.fl,gate.fh,same)

	return

	end

	subroutine gs2_diffgate

	return

	end

	subroutine gs3_diffgate(gate)

#include "gsort.inc"
	structure /diffgate/
	    integer*4 ind
	    integer*4 par
	    integer*4 res
	    real*4    delow
	    real*4    dehigh
	    integer*4 mm
	    real*4    elow
	    real*4    ehigh
	    integer*4 fl
	    integer*4 fh
	 end structure
	record/diffgate/gate

	ji=gate.ind
	jp=gate.par
	rlo=gate.elow
	rhi=gate.ehigh
	nfound=0
	nelim =0
	if( ndet(ji).lt.gate.fl .or. ndet(ji).gt.gate.fh )then
	  killev = .true.
	  return
	endif
	
	do jx=doff(ji),doff(ji)+ndet(ji)-2
	    if(det(jx).xval(jp).ge.rlo .and. det(jx).xval(jp).le.rhi) then
	    do jy=jx+1,doff(ji)+ndet(ji)-1
	       if(det(jy).xval(jp).ge.rlo .and. det(jy).xval(jp).le.rhi)then
	         rdiff = abs(det(jy).xval(jp)-det(jx).xval(jp))
		 do kk=1,gate.mm
		   rr=rdiff/kk
		   if( rr.ge.gate.delow .and. rr.le.gate.dehigh)then
		     nfound = nfound+1
	             nelim=nelim+2
	             det(jx).id=-1
		     det(jy).id=-1
		     goto 10
		   endif
		 enddo
	       endif
	     enddo
	    endif
	 end do

10	if(nfound .eq. 0) goto 9000
	if( ndet(ji)-nelim.lt.gate.fl .or. ndet(ji)-nelim.gt.gate.fh )goto 9000
	if(nelim.GT.0) call gs_checkdetector(ji)
	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_diffgate

	return

	end
