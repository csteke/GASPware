	subroutine usersub1(flag)

	integer flag

#include "gsort.inc"
#include "polar.inc"
	record/addbpol/abpl
	
	common/addbpol/abpl
	
	    if(flag.eq.0) then			! from GETINPUT
		call gs0_addbpol1(abpl)
	elseif(flag.eq.1) then			! from LISTPROG
		call gs1_addbpol1(abpl)
	elseif(flag.eq.2) then			! from INIT_RUN
		call gs2_addbpol1
	elseif(flag.eq.3) then			! from EVANA
		call gs3_addbpol1(abpl)
	elseif(flag.eq.4) then			! from FINIT_RUN
		call gs4_addbpol1
	endif

	return

	end

	subroutine gs0_addbpol1(abpl)

#include "gsort.inc"
#include "polar.inc"
	record/addbpol/abpl

	synt(1)='ADDB_POL Dn [Pattern Dm | Keep2 | Tgate Dm tval]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(abpl.ind,abpl.par,ires)
	if(abpl.ind .EQ. 0) call gs_syntax('Not valid for header parapeters')
	if(NSEGS(abpl.ind) .le. 1) then  	! not defined as a composite
	  abpl.nsegs=-1				! assumes EB packed C(15,7),Q(26,4),T(rest,1)
	elseif( (NSEGS(abpl.ind) .EQ. QSEG) ) then
c	elseif( (NSEGS(abpl.ind) .EQ. QSEG) .OR.
c	1       (NSEGS(abpl.ind) .EQ. CSEG) ) then
	  abpl.nsegs=NSEGS(abpl.ind)
	else
	  call gs_syntax1('Addbpol not yet inplemented for #Segments=',NSEGS(abpl.ind))
	endif

	abpl.pattern=.FALSE.
	abpl.reject =.TRUE.
	abpl.tgate  =.FALSE.
	if(lengthn(comline).LT.0) then
	  call istr_ch2up(comline,comsort)
	  if(comsort(1:1).EQ.'P') then
	    call gs_getindpar(ind,abpl.patpar,ires)
	    if(ind.NE.abpl.ind) call gs_syntax('Must be in the same detector')
	    if(abpl.patpar.LT.0 .OR.
	1      abpl.patpar.GE.TDPAR(abpl.ind) .OR.
	2      abpl.patpar.EQ.abpl.par) call gs_syntax('Illegal parameter value')
	    abpl.pattern=.TRUE.
	  elseif(comsort(1:1).EQ.'K') then
	    abpl.reject=.FALSE.
	  elseif(comsort(1:1).EQ.'T') then
	    call gs_getindpar(ind,abpl.tpar,ires)
	    if(ind.NE.abpl.ind) call gs_syntax('Must be in the same detector')
	    if(abpl.tpar.LT.0 .OR.
	1      abpl.tpar.GE.TDPAR(abpl.ind) .OR.
	2      abpl.tpar.EQ.abpl.par) call gs_syntax('Illegal parameter value')
	    nn=istr_i1(comline,abpl.tval)
	    if(nn.ne.1) call gs_syntax('Valid range for time difference expected')
	    abpl.tgate=.TRUE.
	  else
	    call gs_syntax('Pattern Dm  or  Keep2  or  Tgate Dm tval  expected')
	  endif
	endif
	  
	return

	end

	subroutine gs1_addbpol1(abpl)

#include "gsort.inc"
#include "polar.inc"
	record/addbpol/abpl

	call gs_putindpar(abpl.ind,abpl.par)

	if(abpl.pattern) then
	  llist=lengthc(list)+4
	  write(list(llist:),'(A)') 'Pattern'
	  llist=lengthc(list)+4
	  call gs_putindpar(abpl.ind,abpl.patpar)
	endif

	if(.NOT.abpl.reject) then
	  llist=lengthc(list)+4
	  write(list(llist:),'(A)') 'Keep2'
	endif

	if(abpl.tgate) then
	  llist=lengthc(list)+4
	  write(list(llist:),'(A)') 'Tgate'
	  llist=lengthc(list)+4
	  call gs_putindpar(abpl.ind,abpl.tpar)
	  llist=lengthc(list)+1
	  write(list(llist:),'(i6)') abpl.tval
	endif

	llist=lengthc(list)+5
	if(abpl.nsegs .EQ.4) then
	  write(list(llist:),'(A)') '(Clover)'
	elseif(abpl.nsegs .EQ.7) then
	  write(list(llist:),'(A)') '(Cluster)'
	elseif(abpl.nsegs .EQ.-1) then
	  write(list(llist:),'(A)') '(Cluster and Clover)'
	endif

	return

	end

	subroutine gs2_addbpol1

	return

	end

	subroutine gs3_addbpol1(abpl)

#include "gsort.inc"
#include "polar.inc"
	record/addbpol/abpl
	
	integer pattern(0:31)
	data pattern/	'00000001'X,'00000002'X,'00000004'X,'00000008'X,'00000010'X,'00000020'X,'00000040'X,'00000080'X,
	1		'00000100'X,'00000200'X,'00000400'X,'00000800'X,'00001000'X,'00002000'X,'00004000'X,'00008000'X,
	2		'00010000'X,'00020000'X,'00040000'X,'00080000'X,'00100000'X,'00200000'X,'00400000'X,'00800000'X,
	3		'01000000'X,'02000000'X,'04000000'X,'08000000'X,'10000000'X,'20000000'X,'40000000'X,'80000000'X/

	logical*1 Cadd(0:6,0:6) / .FALSE.,  .TRUE., .FALSE., .FALSE., .FALSE.,  .TRUE.,  .TRUE.,
	1                        .TRUE., .FALSE.,  .TRUE., .FALSE., .FALSE., .FALSE.,  .TRUE.,
	2                       .FALSE.,  .TRUE., .FALSE.,  .TRUE., .FALSE., .FALSE.,  .TRUE.,
	3                       .FALSE., .FALSE.,  .TRUE., .FALSE.,  .TRUE., .FALSE.,  .TRUE.,
	4                       .FALSE., .FALSE.,  .TRUE.,  .TRUE., .FALSE.,  .TRUE.,  .TRUE.,
	5                        .TRUE., .FALSE., .FALSE., .FALSE.,  .TRUE., .FALSE.,  .TRUE.,
	6                        .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE., .TRUE.,  .FALSE./


	logical*1 Qadd(0:3,0:3) /  .FALSE., .TRUE.,  .TRUE., .TRUE.,
	1                        .TRUE.,  .FALSE., .TRUE., .TRUE.,
	2                        .TRUE.,  .TRUE., .FALSE., .TRUE.,
	3                        .TRUE., .TRUE.,  .TRUE., .FALSE./

	integer QpolD(0:3,0:3) /  0, 0, 1, 2,       ! Configurazione Strasburgo
	1 		          0, 0, 2, 1,
	2		          1, 2, 0, 0,
	3		          2, 1, 0, 0/ 

	integer QpolC(0:3,0:3) /  0, 1, 0, 2,       ! Configurazione Legnaro
	1 		          1, 0, 2, 0,
	2		          0, 2, 0, 1,
	3		          2, 0, 1, 0/ 
	integer Qpol(0:3,0:3)  /  0, 1, 0, 2,
	1 		          1, 0, 2, 0,
	2		          0, 2, 0, 1,
	3		          2, 0, 1, 0/ 

	
	integer Cpol(0:6,0:6) / 0, 0, 0, 0, 0, 0, 0,
	1		     0, 0, 0, 0, 0, 0, 0,
	2	    	     0, 0, 0, 0, 0, 0, 0,
	3		     0, 0, 0, 0, 0, 0, 0,
	4		     0, 0, 0, 0, 0, 0, 0,
	5	             0, 0, 0, 0, 0, 0, 0,
	6		     0, 0, 0, 0, 0, 0, 0/
				
	integer nii(0:6),iss(0:6)

	logical*1 ripeti,dopat,reject,tgate,valid,possible1,possible2

	ji=abpl.ind
	if(ndet(ji).LT.1) return
	
	if(ndet(ji).EQ.1 .AND. .NOT.abpl.pattern) return

	do ii=0,MAXDET-1
	 abpl.polar(ii)=0
	end do
	ripeti=.TRUE.				! riordina i rivelatori
	do while(ripeti)
	  ripeti=.FALSE.
	  do ii=doff(ji),doff(ji)+ndet(ji)-2
	    if(det(ii).id .GT. det(ii+1).id) then
	      ripeti=.TRUE.
	      ltmp         = det(ii  ).id
	      det(ii  ).id = det(ii+1).id
	      det(ii+1).id = ltmp
	      do jj=0,TDPAR(ii)-1
	        ltmp               = det(ii  ).ival(jj)
	        det(ii  ).ival(jj) = det(ii+1).ival(jj)
	        det(ii+1).ival(jj) = ltmp
	        xtmp               = det(ii  ).xval(jj)
	        det(ii  ).xval(jj) = det(ii+1).xval(jj)
	        det(ii+1).xval(jj) = xtmp
	      end do
	    endif
	  end do
	end do

	jp=abpl.par
	reject=abpl.reject
	dopat=abpl.pattern
	jpatpar=abpl.patpar
	tgate=abpl.tgate
	jtpar=abpl.tpar
	jtval=abpl.tval
	ii1=doff(ji)
	ii2=doff(ji)+ndet(ji)-1
	nelim=0

	ioff=0
	assign 1000 to iret
	if(abpl.nsegs .EQ. 4) goto 400		! Clover  da soli
	if(abpl.nsegs .EQ. 7) goto 700		! Cluster da soli

	if(det(ii1).id .LT. QOFF) then		! impaccato e ci sono dei cluster
	  ioff=0
	  do ii=ii1,ii2
	    if(det(ii).id .GE. QOFF) then
	      ii2=ii-1
	      assign 40 to iret
	      goto 700
	    endif
          end do
	  assign 1000 to iret		! solo cluster
	  goto 700
	endif

40	if(det(ii1).id .LT. TOFF) then		! Impaccato e ci sono dei clover
	  ioff=QOFF
	  ii2=doff(ji)+ndet(ji)-1
	  do ii=ii1,ii2
	    if(det(ii).id .GE. TOFF) then
	      ii2=ii-1
	      assign 1000 to iret
	      goto 400
	    endif
          end do
	  assign 1000 to iret		! (restano) solo clover
	  goto 400
	endif

	goto 1000				! Solo tapered


400	ni1=ii1				!!! Clover
	ni2=ii1
	id=det(ii1).id
	ntt=(id-ioff)/QSEG
410	if(ni2.LT.ii2) then		! isola il prossimo clover
	  id=det(ni2+1).id
	  ntn=(id-ioff)/QSEG
	  if(ntn.EQ.ntt) then
	    ni2=ni2+1
	    goto 410
	  endif
	endif
	nn=ni2-ni1+1
	if(dopat) then
	  ipat=0
	  etot=0
	  emax=-1
	  imax=-1
	  idmax=-1
	  do jj=ni1,ni2
	    id=det(jj).id
	    det(jj).id=-1
	    njj=mod(id-ioff,QSEG)
	    ipat=ipat+pattern(njj)
	    ener=det(jj).xval(jp)
	    etot=etot+ener
	    if(ener.GE.emax) then
	      emax=ener
	      imax=jj
	      idmax=id
	    endif
	  end do
	  det(imax).id=idmax
	  det(imax).xval(jp)=etot
	  det(imax).ival(jp)=etot
	  det(imax).xval(jpatpar)=ipat
	  det(imax).ival(jpatpar)=ipat
	  nelim=nelim+nn-1
	elseif(nn.EQ.1) then
	  continue
	elseif(nn.EQ.2) then
	  if(tgate) then
	    valid=abs(det(ni1).ival(jtpar)-det(ni2).ival(jtpar)) .LE. jtval
	  else
	    valid=.TRUE.
	  endif
	  is1=mod(det(ni1).id-ioff,QSEG)
	  is2=mod(det(ni2).id-ioff,QSEG)
	  if(valid .AND. qadd(is1,is2)) then
	    ener1=det(ni1).xval(jp)
	    ener2=det(ni2).xval(jp)
	    etot=ener1+ener2
	    rctet1 = 1.00 - (etot/ener1-1.000)*511.00/etot
	    if( rctet1 .gt. 1.000 .or. rctet1 .lt. -1.000)then
	     possible1 = .false.
	    else
	     possible1 = .true.
	    endif
	    rctet2 = 1.00 - (etot/ener2-1.000)*511.00/etot
	    if( rctet2 .gt. 1.000 .or. rctet2 .lt. -1.000)then
	     possible2 = .false.
	    else
	     possible2 = .true.
	    endif
	    
	    if( possible1 .and. possible2 )then
	      if( rctet1 .ge. rctet2 )then
	           possible2 = .false.
	      else
	           possible1 = .false.
	      endif
	    endif

	    if(possible1) then      !(ener1.GT.ener2) then
              det(ni1).xval(jp)=etot
              det(ni1).ival(jp)=etot
	      det(ni2).id=-1
	      if( abpl.nsegs .eq. 4)then
	       if (rctet1.le. 0.99000 .and. rctet1.ge. -0.99000) then
		abpl.polar(QOFF+det(ni1).id)=Qpol(is1,is2)
	       else
		abpl.polar(QOFF+det(ni1).id)=Qpol(is1,is2) !0
	       endif
	      elseif ( abpl.nsegs .eq. -1) then
	       if (rctet1.le. 0.99000 .and. rctet1.ge. -0.99000) then
	        abpl.polar(det(ni1).id)=Qpol(is1,is2)
	       else
		abpl.polar(det(ni1).id)=Qpol(is1,is2) !0
	       endif
	      endif
	    elseif (possible2) then
	      det(ni2).xval(jp)=etot
	      det(ni2).ival(jp)=etot
	      det(ni1).id=-1
	      if( abpl.nsegs .eq. 4)then
	       if (rctet2.le. 0.99000 .and. rctet2.ge. -0.99000) then
		abpl.polar(QOFF+det(ni2).id)=Qpol(is1,is2)
	       else
		abpl.polar(QOFF+det(ni2).id)=Qpol(is1,is2)!0
	       endif
	      elseif ( abpl.nsegs .eq. -1) then
	       if (rctet2.le. 0.99000 .and. rctet2.ge. -0.99000) then
	        abpl.polar(det(ni2).id)=Qpol(is1,is2)
	       else
		abpl.polar(det(ni2).id)=Qpol(is1,is2) !0
	       endif
	      endif

	    else 
	     if(ener1 .gt. ener2)then
	      det(ni1).xval(jp)=etot
	      det(ni1).ival(jp)=etot
	      det(ni2).id=-1
	      if( abpl.nsegs .eq. 4)then
		abpl.polar(QOFF+det(ni1).id)=Qpol(is1,is2) !0
	      elseif ( abpl.nsegs .eq. -1) then
		abpl.polar(det(ni1).id)=Qpol(is1,is2) !0
	      endif
	     else
	      det(ni2).xval(jp)=etot
	      det(ni2).ival(jp)=etot
	      det(ni1).id=-1
	      if( abpl.nsegs .eq. 4)then
		abpl.polar(QOFF+det(ni2).id)=Qpol(is1,is2) !0
	      elseif ( abpl.nsegs .eq. -1) then
		abpl.polar(det(ni2).id)=Qpol(is1,is2) !0
	      endif
             endif
	    endif
	    nelim=nelim+1
	  elseif(reject) then
	    det(ni1).id=-1
	    det(ni2).id=-1
	    nelim=nelim+2
	  endif
	else
	  do jj=ni1,ni2
	    det(jj).id=-1
	  end do
	  nelim=nelim+nn
	endif
	if(ni2.LT.ii2) then
	  ni2=ni2+1
	  ni1=ni2
	  ntt=ntn
	  goto 410
	endif
	ii1=ii2+1
	goto iret

700	ni1=ii1		!!! Cluster
	ni2=ii1
	id=det(ii1).id
	ntt=(id-ioff)/CSEG
710	if(ni2.LT.ii2) then		! isola il prossimo cluster
	  id=det(ni2+1).id
	  ntn=(id-ioff)/CSEG
	  if(ntn.EQ.ntt) then
	    ni2=ni2+1
	    goto 710
	  endif
	endif
	nn=ni2-ni1+1
	if(dopat) then
	  ipat=0
	  etot=0
	  emax=-1
	  imax=-1
	  idmax=-1
	  do jj=ni1,ni2
	    id=det(jj).id
	    det(jj).id=-1
	    njj=mod(id-ioff,CSEG)
	    ipat=ipat+pattern(njj)
	    ener=det(jj).xval(jp)
	    etot=etot+ener
	    if(ener.GE.emax) then
	      emax=ener
	      imax=jj
	      idmax=id
	    endif
	  end do
	  det(imax).id=idmax
	  det(imax).xval(jp)=etot
	  det(imax).ival(jp)=etot
	  det(imax).xval(jpatpar)=ipat
	  det(imax).ival(jpatpar)=ipat
	  nelim=nelim+nn-1
	elseif(nn.EQ.1) then
	  continue
	elseif(nn.EQ.2) then
	  if(tgate) then
	    valid=abs(det(ni1).ival(jtpar)-det(ni2).ival(jtpar)) .LE. jtval
	  else
	    valid=.TRUE.
	  endif
	  is1=mod(det(ni1).id-ioff,CSEG)
	  is2=mod(det(ni2).id-ioff,CSEG)
	  if(valid .AND. cadd(is1,is2)) then
	    ener1=det(ni1).xval(jp)
	    ener2=det(ni2).xval(jp)
	    etot=ener1+ener2
	    if(ener1.GT.ener2) then
              det(ni1).xval(jp)=etot
              det(ni1).ival(jp)=etot
	      det(ni2).id=-1
	    else
	      det(ni2).xval(jp)=etot
	      det(ni2).ival(jp)=etot
	      det(ni1).id=-1
	    endif
	    nelim=nelim+1
	  elseif(reject) then
	    det(ni1).id=-1
	    det(ni2).id=-1
	    nelim=nelim+2
	  endif
	else
	  do jj=ni1,ni2
	    det(jj).id=-1
	  end do
	  nelim=nelim+nn
	endif
	if(ni2.LT.ii2) then
	  ni2=ni2+1
	  ni1=ni2
	  ntt=ntn
	  goto 710
	endif
	ii1=ii2+1
	goto iret

1000	if(nelim.GT.0) call gs_checkdetector(ji)

	return

	end

	subroutine gs4_addbpol1

	return

	end
