
      subroutine usersub6(flag)

      integer flag

#include "gsort.inc"

      integer swappar
      common/swappar/swappar


      if(flag.eq.0) then      ! from GETINPUT
        call gs0_swap_params(swappar)
      elseif(flag.eq.1) then      ! from LISTPROG
        call gs1_swap_params(swappar)
      elseif(flag.eq.2) then      ! from INIT_RUN
        call gs2_swap_params
      elseif(flag.eq.3) then      ! from EVANA
        call gs3_swap_params(swappar)
      elseif(flag.eq.4) then      ! from FINIT_RUN
        call gs4_swap_params
      endif

      return

      end


**********************************************************
**********************************************************
      subroutine gs0_swap_params(swappar)

#include "gsort.inc"

      integer swappar

      synt(1)='USERSUB6 P'

      if(syntax) then
        call gs_syntax(' ')
        return
      endif

      call gs_getind(swappar)
      if(swappar.ind.le.0) call gs_syntax('Meaningless for header ')

      return

      end


      subroutine gs1_swap_params(swappar)

#include "gsort.inc"

      integer swappar

      call gs_putind(swappar)

      return
      end


      subroutine gs2_swap_params

      return

      end


      subroutine gs3_swap_params(swappar)

#include "gsort.inc"

      integer swappar

      
      do js=doff(swappar),doff(swappar)+ndet(swappar)-1
        do ji =js+1,doff(swappar)+ndet(swappar)-1
       
         if( (det(js).id .eq. 51) .and. (det(ji).id .eq. 53) )then
	 	rrr = det(js).xval(1)
		det(js).xval(1) = det(ji).xval(1)
		det(ji).xval(1) = rrr
		det(js).ival(1) = det(js).xval(1)
		det(ji).ival(1) = det(ji).xval(1)

	 	rrr = det(js).xval(3)
		det(js).xval(3) = det(ji).xval(3)
		det(ji).xval(3) = rrr
		det(js).ival(3) = det(js).xval(3)
		det(ji).ival(3) = det(ji).xval(3)
		
	  endif

         if( (det(js).id .eq. 52) .and. (det(ji).id .eq. 54) )then
	 	rrr = det(js).xval(0)
		det(js).xval(0) = det(ji).xval(0)
		det(ji).xval(0) = rrr
		det(js).ival(0) = det(js).xval(0)
		det(ji).ival(0) = det(ji).xval(0)

	 	rrr = det(js).xval(2)
		det(js).xval(2) = det(ji).xval(2)
		det(ji).xval(2) = rrr
		det(js).ival(2) = det(js).xval(2)
		det(ji).ival(2) = det(ji).xval(2)
		
	  endif
      

      end do

      return

      end

      subroutine gs4_swap_params

      return

      end
