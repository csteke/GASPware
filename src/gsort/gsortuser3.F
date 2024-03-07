	subroutine usersub3(flag)

	integer flag


#include "gsort.inc"
	 
	 record /pms_str/pms
	 save pms

	if(flag.eq.0) then			! from GETINPUT
*	  call gs_syntax('Please replace this Subroutine with your own version')
          call gs0_track_pms_u3(pms)
	elseif(flag.eq.1) then			! from LISTPROG
          call gs1_track_pms_u3(pms)
	
	elseif(flag.eq.2) then			! from INIT_RUN
          call gs2_track_pms_u3(pms)
	
	elseif(flag.eq.3) then      ! from EVANA
          call gs3_track_pms_u3(pms)
	
	elseif(flag.eq.4) then			! from FINIT_RUN
          call gs4_track_pms_u3(pms)
	
	endif

	return

	end

	
	subroutine gs0_track_pms_u3( pms )
	

#include "gsort.inc"
	record /pms_str/pms
	
	integer ind_r, ind_d, ind_e ii

	
	
	synt(1)='USERSUB3  Fn ( TOF-A/Q) Fm (path length) Fe (IC energy) Fp(mean path in IC) (all in output) [QFACT]'
	synt(2)='          Ah (entrance angle in horizontal plane -> 2000 + Theta[deg]*100)   Ap (target-MCP path[mm])'
	synt(3)='          Px (PPAC X [mm])'
	synt(4)='          Ia Ib Ic Id [low_thr] (individual energies in the sections of the IC and lower threshold)'
	

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif
	
	call gs_getindpar(ind_r, pms.par_r,ii)
	if( ind_r .ne. 0) call gs_syntax('Fixed parameter expected here')
	
	call gs_getindpar(ind_d, pms.par_p,ii)
	if( ind_d .ne. 0) call gs_syntax('Fixed parameter expected here')
	pms.ind = ind_r
	
	if( pms.par_r .eq. pms.par_d ) call gs_syntax('All parameters must be different')
	
	call gs_getindpar(ind_e, pms.par_e,ii)
	if( ind_e .ne. 0) call gs_syntax('Fixed parameter expected here')
	
	if( pms.par_r .eq. pms.par_e ) call gs_syntax('All parameters must be different')
	if( pms.par_d .eq. pms.par_e ) call gs_syntax('All parameters must be different')

	ind_e = -10
	call gs_getindpar(ind_e, pms.par_icp,ii)
	if( ind_e .ne. 0) call gs_syntax('Fixed parameter expected here')
	
	if( pms.par_r .eq. pms.par_icp ) call gs_syntax('All parameters must be different')
	if( pms.par_d .eq. pms.par_icp ) call gs_syntax('All parameters must be different')
	if( pms.par_e .eq. pms.par_icp ) call gs_syntax('All parameters must be different')
	
	pms.qfact = 0.9430E0
	if( istr_r1(comline,pms.qfact) .ne. 1 ) pms.qfact = 0.9430E0
	
	call gs_readline
	if(lcomline.LT.1) call gs_syntax('Incomplete command, THETA parameter expected')
	
	call gs_getindpar(pms.ind_theta, pms.par_theta, ii)
	call gs_getindpar(pms.ind_path, pms.par_path, ii)
	if( pms.ind_path .eq. pms.ind_theta )then
	   if(pms.par_path .eq. pms.par_theta)call gs_syntax('All parameters must be different')
	endif

	call gs_readline
	if(lcomline.LT.1) call gs_syntax('Incomplete command, X parameter expected')
	
	call gs_getindpar(pms.ind_x, pms.par_x, ii)

	call gs_readline
	if(lcomline.LT.1) call gs_syntax('Incomplete command')
	
	call gs_getindpar(pms.ind_ic, pms.par_a, ii)
	if( pms.ind_ic .lt. 1)call gs_syntax('Meaningless for header parameters')
	
	call gs_getindpar(ind_r, pms.par_b, ii)
	if(ind_r .ne. pms.ind_ic)call gs_syntax('Must be the same detector')
	if(pms.par_a .eq. pms.par_b)call gs_syntax('All IC parameters must be different')
	
	call gs_getindpar(ind_r, pms.par_c, ii)
	if(ind_r .ne. pms.ind_ic)call gs_syntax('Must be the same detector')
	if(pms.par_a .eq. pms.par_c)call gs_syntax('All IC parameters must be different')
	if(pms.par_b .eq. pms.par_c)call gs_syntax('All IC parameters must be different')

	call gs_getindpar(ind_r, pms.par_d, ii)
	if(ind_r .ne. pms.ind_ic)call gs_syntax('Must be the same detector')
	if(pms.par_a .eq. pms.par_d)call gs_syntax('All IC parameters must be different')
	if(pms.par_b .eq. pms.par_d)call gs_syntax('All IC parameters must be different')
	if(pms.par_c .eq. pms.par_d)call gs_syntax('All IC parameters must be different')
	
	if( istr_r1(comline,pms.thr) .ne. 1 ) pms.thr = 0.0D0
	
	return
	end
	
	subroutine gs1_track_pms_u3( pms )
	

#include "gsort.inc"
	record /pms_str/pms

	call gs_putindpar(pms.ind, pms.par_r)
	llist = llist+3
	call gs_putindpar(pms.ind, pms.par_p)
	llist = llist+3
	call gs_putindpar(pms.ind, pms.par_e)
	llist = llist+3
	call gs_putindpar(pms.ind, pms.par_icp)
	llist = llist+3
	write(list(llist+1:),'(F8.5)') pms.qfact
	call gs_writeline(LLUN,list,INDENT)
	
	llist = INDENT
	call gs_putindpar(pms.ind_theta, pms.par_theta)
	llist = llist+3
	call gs_putindpar(pms.ind_path, pms.par_path)
	call gs_writeline(LLUN,list,INDENT)

	llist = INDENT
	call gs_putindpar(pms.ind_x, pms.par_x)
	call gs_writeline(LLUN,list,INDENT)

	llist = INDENT
	call gs_putindpar(pms.ind_ic, pms.par_a)
	llist = llist+3
	call gs_putindpar(pms.ind_ic, pms.par_b)
	llist = llist+3
	call gs_putindpar(pms.ind_ic, pms.par_c)
	llist = llist+3
	call gs_putindpar(pms.ind_ic, pms.par_d)
	llist = llist+3
	write(list(llist+1:),'(F7.2)') pms.thr
	call gs_writeline(LLUN,list,INDENT)
		
	return
	end

	subroutine gs2_track_pms_u3( pms )
	

#include "gsort.inc"
	record /pms_str/pms
	
	return
	end
	

	subroutine gs3_track_pms_u3( pms )
	

#include "gsort.inc"
	record /pms_str/pms
	
	real*8 d1, d2, xx, zz, tmp, theta, gamma, x1, x2, xtmp, ztmp, etmp, ethr, alpha!, rfact
	real*8 tau, theta0, qk, r0,p1xx, p1tmp, d01, p1tq, tmp_a, dq, dtq, dqd, dtd, qfact
	real*8 theta_quad
	integer lastid(4), firstid(4)
	
	if( ndet( pms.ind_theta ) .ne. 1 )then
	   killev = .true.
	   return
	endif
	
	if( ndet( pms.ind_path ) .ne. 1 )then
	   killev = .true.
	   return
	endif
	
	if( ndet( pms.ind_x ) .ne. 1 )then
	   killev = .true.
	   return
	endif

	if( ndet( pms.ind_ic ) .lt. 1 )then
	   killev = .true.
	   return
	endif
	
	
	iter = 1
	r0 = 1200.0D0
		
*	theta = det(doff(pms.ind_theta)).xval(pms.par_theta)
*	theta = (theta-2000.0D0)/100.0D0!*1.10D0
	
	xx = 0.0D0
	zz = 0.0D0
	tmp = 0.0D0
	etmp = 0.0D0
	ethr = pms.thr
	
	do ii = 1,4
	  lastid(ii) = -1
	  firstid(ii)= -1
	enddo
	
	do ii =doff(pms.ind_ic), doff(pms.ind_ic)+ndet(pms.ind_ic)-1
	   xtmp = det(ii).id*100.0D0 + 50.0D0
	   
	   ztmp = 845.0D0
	   if(det(ii).xval(pms.par_a) .gt. ethr)then
	   	if( lastid(1) .ge. 0 )then
		  if( abs(lastid(1) - det(ii).id) .gt. 1 )then
		    killev = .true.
		    return
		  endif
		endif
		xx = xx + det(ii).xval(pms.par_a)*xtmp
	   	zz = zz + det(ii).xval(pms.par_a)*ztmp

	   	tmp = tmp + det(ii).xval(pms.par_a)
		lastid(1) = det(ii).id
		if(firstid(1) .lt. 0)firstid(1)= det(ii).id
*		if(det(ii).xval(pms.par_a) .gt. ethr) etmp = etmp+det(ii).xval(pms.par_a)
	   endif
	   
	   ztmp = 1095.0D0
	   if(det(ii).xval(pms.par_b) .gt. ethr)then
	   	if( lastid(2) .ge. 0 )then
		  if( abs(lastid(2) - det(ii).id) .gt. 1 )then
		    killev = .true.
		    return
		  endif
		endif
		xx = xx + det(ii).xval(pms.par_b)*xtmp
	   	zz = zz + det(ii).xval(pms.par_b)*ztmp

	   	tmp = tmp + det(ii).xval(pms.par_b)
		lastid(2) = det(ii).id
		if(firstid(2) .lt. 0)firstid(2)= det(ii).id
*		if(det(ii).xval(pms.par_b) .gt. ethr) etmp = etmp+det(ii).xval(pms.par_b)
	   endif
	   
	   ztmp = 1345.0D0
	   if(det(ii).xval(pms.par_c) .gt. ethr)then
	   	if( lastid(3) .ge. 0 )then
		  if( abs(lastid(3) - det(ii).id) .gt. 1 )then
		    killev = .true.
		    return
		  endif
		endif
		xx = xx + det(ii).xval(pms.par_c)*xtmp
	   	zz = zz + det(ii).xval(pms.par_c)*ztmp

	   	tmp = tmp + det(ii).xval(pms.par_c)
		lastid(3) = det(ii).id
		if(firstid(3) .lt. 0)firstid(3)= det(ii).id
*		if(det(ii).xval(pms.par_c) .gt. ethr) etmp = etmp+det(ii).xval(pms.par_c)
	   endif
	   
	   ztmp = 1595.0D0
	   if(det(ii).xval(pms.par_d) .gt. ethr)then
	   	if( lastid(4) .ge. 0 )then
		  if( abs(lastid(4) - det(ii).id) .gt. 1 )then
		    killev = .true.
		    return
		  endif
		endif
		xx = xx + det(ii).xval(pms.par_d)*xtmp
	   	zz = zz + det(ii).xval(pms.par_d)*ztmp

	   	tmp = tmp + det(ii).xval(pms.par_d)
		lastid(4) = det(ii).id
		if(firstid(4) .lt. 0)firstid(4)= det(ii).id
*		if(det(ii).xval(pms.par_d) .gt. ethr) etmp = etmp+det(ii).xval(pms.par_d)
	   endif
	   
	enddo
	if( tmp .lt. 1.0D-3 )then
	  killev = .true.
	  return
	else
	  etmp = tmp
	endif
	
	if( firstid(1) .lt. 0 )then
	  killev = .true.
	  return
	elseif( lastid(1) - firstid(1) .gt. 1)then
	  killev = .true.
	  return
	endif
	
	if( firstid(2) .gt. 0)then
	if( abs(firstid(2)-firstid(1)) .gt. 1)then
	   killev = .true.
	   return
	endif
	if( abs(lastid(2)-lastid(1)) .gt. 1)then
	   killev = .true.
	   return
	endif
	if( lastid(2)-firstid(2) .gt. 1)then
	   killev = .true.
	   return
	endif
	endif
	
	if( firstid(3) .gt. 0)then
	if( abs(firstid(3)-firstid(2)) .gt. 1)then
	   killev = .true.
	   return
	endif
	if( abs(lastid(3)-lastid(2)) .gt. 1)then
	   killev = .true.
	   return
	endif
	if( lastid(3)-firstid(3) .gt. 1)then
	   killev = .true.
	   return
	endif
	endif
	
	if( firstid(4) .gt. 0)then
	if( abs(firstid(4)-firstid(3)) .gt. 1)then
	   killev = .true.
	   return
	endif
	if( abs(lastid(4)-lastid(3)) .gt. 1)then
	   killev = .true.
	   return
	endif
	if( lastid(4)-firstid(4) .gt. 1)then
	   killev = .true.
	   return
	endif
	endif
	
	xx = xx/tmp
	zz = zz/tmp
	tmp = det(doff(pms.ind_x)).xval(pms.par_x)
	gamma =  xx - tmp
	gamma = 90.0D0+datand(gamma/zz)
	ztmp = (zz-720.0D0)/dcosd(gamma-90.0D0)

*	d2 = 3235.0D0/dcosd(gamma-90.0D0)
	tmp = 1035.0D0 - tmp
	zz = 3235.0D0 + tmp*0.08748866352592401D0
	d2 = zz*0.9961946980917455D0/dsind(175.0D0-gamma)
* 	r2 = 261.5113870D0 + tmp*1.003819837543347 + zz*dsind(gamma-90.0D0)/dsind(175.0D0-gamma)
 	r2 = 261.5113870D0 + tmp*1.003819837543347 + zz*dsind(gamma-90.0D0)/dsind(175.0D0-gamma)
	gamma = gamma + 5.0D0
 	rfact = r2-798.555D0

#define SINH( x ) ( (dexp(x)-dexp(-x))/2.0D0 )
#define COSH( x ) ( (dexp(x)+dexp(-x))/2.0D0 )
	
	dtd = 1600.0D0
	dq  = 480.0D0
	dtq = 750.0D0 - dq/2.0D0
	dqd = dtd - dtq - dq
	
	theta0 = det(doff(pms.ind_theta)).xval(pms.par_theta)
	theta0 = (theta0-2000.0D0)/100.0D0

	if( theta0 .ge. 0.0D0 )then
	    d1 = dtd*dsind(110.0D0)/dsind(70.0D0-theta0) 
	    r1 = 1123.9350D0 - dtd*dsind(theta0)/dsind(70.0D0-theta0)
	else
	    d1 = dtd*dsind(70.0D0)/dsind(110.0D0+theta0) 
	    r1 = 1123.9350D0 - dtd*dsind(theta0)/dsind(110.0D0+theta0)
	endif
	
	d1 = d1-det(doff(pms.ind_path)).xval(pms.par_path)
	
	alpha = gamma - theta0 - 35.0D0
	
	r0 = dsqrt( (r1*r1 + r2*r2 - 0.5176380902050415D0*r1*r2)/2.0D0/(1.0D0-dcosd(alpha))) 
	
*	qfact = 0.9200000D0 ! OK for 82Se
*	qfact = 0.943000000D0 ! OK for 64Ni
*	qfact = 0.79000000D0 ! OK for 54Fe
*	theta_quad = 0.000000D0  !   ---
	qfact = pms.qfact
		
	iter = 1
	r0 = 1200.0D0
	
	p1tq = dtand(theta0)
	p1tmp = dtq*dabs( p1tq )
	qk = qfact*dq/dsqrt(200.0D0*r0)
	d01 = dtq/dcosd(theta0)
	
	tmp_a = dtand(20.0D0)
*        if( theta0 .gt. 0.0D0 ) qk = qk*theta_quad
	
*10	theta = (1.0D0 + qk)*theta0 
*	p1tq = dcosd(theta0)/dcosd(theta)
*
*	if( dabs(theta-theta0) .le. .0100D0 )then
*	   p1xx = p1tmp + 500.0D0*dabs( dsind(theta0) )/dcosd(theta)
*	else
*	   p1xx = p1tmp + 57.295779513082D0*500.0D0/qk/dabs(theta0)*log(p1tq)
*	endif

10	theta = qk*p1tq*dtq/dq*SINH( qk ) + p1tq*COSH( qk )
	theta = datand( theta )

	if( dabs(qk) .le. .0100D0 )then
		p1xx = p1tmp*COSH( qk ) + dabs(p1tq)*dq
	else
		p1xx = p1tmp*COSH( qk ) + dabs(p1tq)/qk*dq*SINH( qk )
	endif
	
	if( theta .ge. 0.0D0 )then
	    d1 = d01 + (dqd+p1xx*tmp_a)*dsind(110.0D0)/dsind(70.0D0-theta)
	    r1 = 1123.9350D0 - p1xx/dcosd(20.0D0) - (dqd+p1xx*tmp_a)*dsind(theta)/dsind(70.0D0-theta)
	else
	    d1 = d01 + (dqd-p1xx*tmp_a)*dsind(70.0D0)/dsind(110.0D0+theta)
	    r1 = 1123.9350D0 + p1xx/dcosd(20.0D0) - (dqd-p1xx*tmp_a)*dsind(theta)/dsind(110.0D0+theta)
	endif
       d1 = d1 + (dsqrt(dq*dq + (p1xx-p1tmp)**2) + p1xx-(dtq+dq)*dabs(p1tq) + dq/dcosd(theta0))/2.0D0

	d1 = d1-det(doff(pms.ind_path)).xval(pms.par_path)
	
	alpha = gamma - 35.0D0 - theta !*(1.00D0+theta_quad) 
*	alpha = gamma - theta - 35.0D0
	
	tmp = dsqrt( (r1*r1 + r2*r2 - 0.5176380902050415D0*r1*r2)/2.0D0/(1.0D0-dcosd(alpha)))
*       write(*,'(7F12.3)')theta0, theta, alpha, tmp, d1, r1 ,p1xx
	
	if( iter .lt. 15)then
	   tau = r0/tmp
	   iter = iter+1
	   qk = qk*dsqrt(tau)
	   r0 = tmp
*	   r0 = tmp*(1.0D0+tmp/r0)/2.0D0
	   if( abs(tau-1.0D0) .gt. 5.0000D-4 )goto 10
 	endif
*        write(*,*) qk/dq*dsqrt(200.0D0*1200.0D0), tau
	rfact = rfact + r1-1123.935D0
!   	rfact = (1.0D0 + rfact*5.00D-9*rfact)
	rfact = 1.0D0 + dexp((abs(rfact)-1000.0D0)/77.0D0)	
! 	tmp = tmp/rfact

	xx = det(doff(pms.ind_theta)).xval(pms.par_theta)
	zz =  9031.7 -13.502*xx +0.010306*xx*xx -3.5314D-06*xx*xx*xx +4.4847D-10*xx*xx*xx*xx
	xx = -9.9674 +0.02149*xx -1.6428D-05*xx*xx +5.5329D-09*xx*xx*xx -6.8563D-13*xx*xx*xx*xx
	det(pms.ind).xval(pms.par_r) = zz+xx*det(doff(pms.ind_x)).xval(pms.par_x) -2000.0D0
	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)

	det(pms.ind).xval(pms.par_p) = d1 + d2 + tmp*alpha* 0.0174532925199433 
	det(pms.ind).ival(pms.par_p) = det(pms.ind).xval(pms.par_p)

	det(pms.ind).xval(pms.par_e) = etmp 
	det(pms.ind).ival(pms.par_e) = det(pms.ind).xval(pms.par_e)
	
	det(pms.ind).xval(pms.par_icp) = ztmp*5.0D0 
	det(pms.ind).ival(pms.par_icp) = det(pms.ind).xval(pms.par_icp)
	
	det(doff(pms.ind_x)).xval(pms.par_x) = tmp/rfact 
	det(doff(pms.ind_x)).ival(pms.par_x) = det(doff(pms.ind_x)).xval(pms.par_x)

*	det(pms.ind).xval(pms.par_r) = 10.0D0*alpha
*	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)
		
	return	
	end
	
	subroutine gs4_track_pms_u3( pms )
	

#include "gsort.inc"
	record /pms_str/pms
	
	return
	end
