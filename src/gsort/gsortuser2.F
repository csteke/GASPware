       subroutine usersub2(flag)

#include "gsort.inc"

       integer flag
       
       integer ind, par1, par2, par3, par4, par5
       save  par1, par2, par3, par4, par5
       
       integer ind1, ind2, ind3, ind4, ind5, res
       save ind1, ind2, ind3, ind4, ind5
       
       character*256 OutFile
       save OutFile

       real*8  ux, uy, uz, theta, phi, energy


       if(flag.eq.0) then		       ! from GETINPUT
        synt(1)='USERSUB2 Dt Dp Rv Pa Pz output_file'

        call gs_getindpar(ind1,par1,res)
	 if( ind1 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind2,par2,res)
	 if( ind2 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind3,par3,res)
	 if( ind3 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind4,par4,res)
	 if( ind4 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind5,par5,res)
	 if( ind5 .lt. 0 ) call gs_syntax('Detector parameter expected here')

	 ind = ind1

        if( ind1 .ne. ind2 )then
	   call gs_syntax('Theta and Phi parameters must be in the same detector')
	endif
	
	if( ind4 .ne. ind5 )then
	  call gs_syntax('A and Z parameters must be in the same detector')
	endif

	if( par1 .eq. par2 ) call gs_syntax('Theta/Phi parameters must be different') 
	if( par4 .eq. par5 ) call gs_syntax('A/Z parameters must be different') 

        call istr_ch(comline,OutFile)
	if(lengthc(OutFile).LT.1) call gs_syntax('Name for the output file expected here')
	open( 201, file=OutFile, status='unknown', ERR = 1)
	write(201,'(''FORMAT 1 4'',/,''EMITTED 1 1'')')
	return
1	call gs_syntax('Cannot create/overwrite the output file') 	

       elseif(flag.eq.1) then		       ! from LISTPROG
	 call gs_putindpar( ind1, par1 )
	 llist = llist+3
	 call gs_putindpar( ind2, par2 )
	 llist = llist+3
	 call gs_putindpar( ind3, par3 )
	 llist = llist+3
	 call gs_putindpar( ind4, par4 )
 	 llist = llist+3
	 call gs_putindpar( ind5, par5 )
        llist=lengthc(list)+2
	ii1 = lengthc(OutFile)
        write(list(llist:),'(a)')OutFile(1:ii1)

       elseif(flag.eq.2) then		       ! from INIT_RUN
         return

       elseif(flag.eq.3) then		       ! from EVANA
         if( ndet( ind1 ) .ne. 1 )return
         if( ndet( ind3 ) .ne. 1 )return
         if( ndet( ind4 ) .ne. 1 )return
	 
	 write(201,'(''$'')')
	 
	 ii1 = doff(ind1)
	 ii2 = doff(ind3)
	 ii3 = doff(ind4)
	 
	 theta = det(ii1).xval(par1)/10.0D0
	 phi   = det(ii1).xval(par2)/10.0D0
	 
	 ux = dsind(theta)*dcosd(phi)
	 uy = dsind(theta)*dsind(phi)
	 uz = dcosd(theta)
	 
	 Energy = det(ii2).xval(par3)/100.000D0
	 Energy = energy*energy*465.750D0*det(ii3).ival(par4)
	 
	 write(201,'(3i4,f9.3,1x,3(1x,f10.7))')-101,det(ii3).ival(par5),det(ii3).ival(par4),Energy,ux,uy,uz
	 write(201,'(''1 egamma_'',i3.3,''_'',i3.3,'' 0. 0.'')')det(ii3).ival(par5),det(ii3).ival(par4)
	      
       elseif(flag.eq.4) then		       ! from FINIT_RUN
	 call flush(201)
       
       endif

       return

       end



* previous USERSUB2
*
*	subroutine usersub2(flag)
*
*#include "gsort.inc"
*
*	integer flag
*
*	PARAMETER (IGG=5)
*	PARAMETER (IPP=7)
*
*
*	if(flag.eq.0) then			! from GETINPUT
*c	  call gs_syntax('Please replace this Subroutine with your own version')
*	elseif(flag.eq.1) then			! from LISTPROG
*	  write(list(llist:),'(A)') 'Time difference with respect to '//dname(IPP)
*	elseif(flag.eq.2) then			! from INIT_RUN
*	elseif(flag.eq.3) then			! from EVANA
*	  if(ndet(ipp).ne.1) return
*	  tppac=det(doff(IPP)).xval(0)
*	  do ii=doff(IGG),doff(IGG)+ndet(IGG)-1
*	    tger=det(ii).xval(2)-tppac
*	    det(ii).xval(2)=tger
*	    det(ii).ival(2)=tger
*	  enddo
*	elseif(flag.eq.4) then			! from FINIT_RUN
*	endif
*
*	return
*
*	end
