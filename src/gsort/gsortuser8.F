	subroutine usersub8(flag)

	integer flag

#include "gsort.inc"
	structure /u8str/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  logical*1 INSIDE	! dentro/fuori
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	  integer deltan
	endstructure
	record/u8str/gate
	record/reorder/reo
	common /u8comm/gate,reo

	    if(flag.eq.0) then			! from GETINPUT
	  call gs0_us8
	elseif(flag.eq.1) then			! from LISTPROG
	  call gs1_us8
	elseif(flag.eq.2) then			! from INIT_RUN
	  call gs2_us8
	elseif(flag.eq.3) then			! from EVANA
	  call gs3_reorder(reo)
	  call gs3_us8
	elseif(flag.eq.4) then			! from FINIT_RUN
	  call gs4_us8
	endif

	return

	end


	subroutine gs0_us8

#include "gsort.inc"
	structure /u8str/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  logical*1 INSIDE	! dentro/fuori
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	  integer deltan
	endstructure
	record/u8str/gate
	record/reorder/reo
	common /u8comm/gate,reo

	logical*1 sign

	synt(1)='USERSUB8 Pn Ndet_to_look [IN|OUT]  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(gate.ind,gate.par,gate.res)
	reo.ind = gate.ind
	nn=istr_i1(comline,gate.deltan)
	if(nn.ne.1) gate.deltan = 1            !call gs_syntax('Wrong number of gates')
	call gs_getinout(gate.inside)
	call gs_getfoldgate(gate.ind,gate.fl,gate.fh,gate.same)

	return

	end

	subroutine gs1_us8

#include "gsort.inc"
	structure /u8str/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  logical*1 INSIDE	! dentro/fuori
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	  integer deltan
	endstructure
	record/u8str/gate
	record/reorder/reo
	common /u8comm/gate,reo

	call gs_putindpar(gate.ind,gate.par)

	llist=lengthc(list)+2
	write(list(llist:),'(i2)')gate.deltan
	llist=lengthc(list)+2
	call gs_putinout(gate.inside)

	llist=lengthc(list)+2
	call gs_putfoldgate(gate.ind,gate.fl,gate.fh,gate.same)
***	write(6,*)' USERSUB8 WARNING - undefined behaviour if not REORDER-ed first'

	return

	end

	subroutine gs2_us8

	return

	end

	subroutine gs3_us8

#include "gsort.inc"
	structure /u8str/
	  INTEGER IND		! Tipo di parametro
	  INTEGER PAR		! Quale parametro
	  INTEGER RES		! Risoluzione del parametro
	  logical*1 INSIDE	! dentro/fuori
	  INTEGER FL		! Minfold
	  INTEGER FH		! Minfold
	  logical*1 SAME		! Same as before gate
	  integer deltan
	endstructure
	record/u8str/gate
	record/reorder/reo
	common /u8comm/gate,reo
	integer u8kill(MAXDET)
	integer u8keep(MAXDET)
	integer nmpx1,nmpx2

	if( ndet(gate.ind) .lt. 2)return
*	call gs3_reorder(reo)
	ji=gate.ind
	jp=gate.par
	nelim=0
	
	nmpx1 = 0
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  if( det(jx).id .lt. 24 )nmpx1 = nmpx1+1
	enddo
	nmpx2 = ndet(ji) - nmpx1     
	
	n_to_kill=0
	n_to_keep=0
	if ( nmpx1 .gt. 1) then
	 do jx=doff(ji)+1,doff(ji)+nmpx1-1
	   rr = -1.000
	   do jy = max(doff(ji),jx-gate.deltan),jx-1
	        if( det(jy).xval(jp) .ge. rr ) rr = det(jy).xval(jp)
	   enddo
	   if ( det(jx).xval(jp) .lt. rr )then
	      n_to_kill = n_to_kill+1
	      u8kill(n_to_kill)=jx
	   else
	      n_to_keep = n_to_keep+1
	      u8keep(n_to_keep)=jx
	   endif
	 enddo
	endif

	if ( nmpx2 .gt. 1) then
	 do jx=doff(ji)+nmpx1+1,doff(ji)+ndet(ji)-1
	   rr = -1.000
	   do jy = max(doff(ji)+nmpx1,jx-gate.deltan),jx-1
	        if( det(jy).xval(jp) .ge. rr ) rr = det(jy).xval(jp)
	   enddo
	   if ( det(jx).xval(jp) .lt. rr )then
	      n_to_kill = n_to_kill+1
	      u8kill(n_to_kill)=jx
	   else
	      n_to_keep = n_to_keep+1
	      u8keep(n_to_keep)=jx
	   endif
	 enddo
	endif
	


	if(gate.inside) then
	  do jx=1,n_to_kill                !doff(ji),doff(ji)+ndet(ji)-1
	      det(u8kill(jx)).id = -1
	      nelim=nelim+1
	  end do
	else
	  do jx=1,n_to_keep                !doff(ji),doff(ji)+ndet(ji)-1
	      det(u8keep(jx)).id = -1
	      nelim=nelim+1
	  end do
	endif
	if(nelim.ne.0 .and. gate.same) goto 9000
	nx=ndet(ji)-nelim
	if(nx.LT.gate.fl .or. nx.GT.gate.fh) goto 9000
	if(nelim.GT.0) call gs_checkdetector(ji)
	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_us8

	return

	end
