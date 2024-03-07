CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       GS0_... definizione      C
C       GS1_... lista            C
C       GS3_... inizializzazione C
C       GS3_... esecuzione       C
C       GS4_... finalizzazione   C
C       GS5_... opzionale        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCC Filtri, ricalibrazioni e varie CCCCCCCCCCCCC

	subroutine gs0_fold(fold)

#include "gsort.inc"
	record/fold/ fold

	synt(1)='FOLD  D  Min Max'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getind(fold.ind)
	if(fold.ind.EQ.0) call gs_syntax('Meaningless for header')
	nn=istr_i2(comline,fold.min,fold.max)
	if(nn.ne.2) call gs_syntax('Wrong number of parameters')

	return

	end

	subroutine gs1_fold(fold)

#include "gsort.inc"
	record/fold/fold

	call gs_putind(fold.ind)

	write(list(llist+1:),'(2i6)') fold.min,fold.max

	return

	end

	subroutine gs2_fold

	return

	end

	subroutine gs3_fold(fold)

#include "gsort.inc"
	record/fold/ fold

	kfold=ndet(fold.ind)
	IF(kfold .LT. fold.min    .OR.    kfold .GT. fold.max) killev=.TRUE.

	return

	end

	subroutine gs4_fold

	return

	end



	subroutine gs0_gate(gate)

#include "gsort.inc"
	record/gate/gate

	logical*1 sign

	synt(1)='GATE Pn  Low High  [IN|OUT]  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(gate.ind,gate.par,gate.res)
	call gs_get2ilim(gate.lo,gate.hi,0,gate.res-1)
	call gs_getinout(gate.inside)
	call gs_getfoldgate(gate.ind,gate.fl,gate.fh,gate.same)

	return

	end

	subroutine gs1_gate(gate)

#include "gsort.inc"
	record/gate/gate

	call gs_putindpar(gate.ind,gate.par)

	write(list(llist+1:),'(2i6)') gate.lo,gate.hi

	llist=lengthc(list)+2
	call gs_putinout(gate.inside)

	llist=lengthc(list)+2
	call gs_putfoldgate(gate.ind,gate.fl,gate.fh,gate.same)

	return

	end

	subroutine gs2_gate

	return

	end

	subroutine gs3_gate(gate)

#include "gsort.inc"
	record/gate/gate

	ji=gate.ind
	jp=gate.par
	jlo=gate.lo
	jhi=gate.hi
	nelim=0
	if(gate.inside) then
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	    ICAN=det(jx).ival(jp)
	    if(ican.LT.jlo .or. ican.GT.jhi) then
	      nelim=nelim+1
	      det(jx).id=-1
	    endif
	  end do
	else
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	    ICAN=det(jx).ival(jp)
	    if(ican.GE.jlo .and. ican.le.jhi) then
	      nelim=nelim+1
	      det(jx).id=-1
	    endif
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

	subroutine gs4_gate

	return

	end



	subroutine gs0_gates(gate)

#include "gsort.inc"
	record/gate/gate

	logical*1 getmem
	logical*1 sign
	logical*1 good(0:MAXRES)

	synt(1)='GATES Pn #gates (Low High){#gates_times} [In|Out] FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(gate.ind,gate.par,gate.res)

	nn=istr_i1(comline,gate.ngates)
	if(nn.ne.1) call gs_syntax('Wrong number of gates')
	if(gate.ngates.LT.1) call gs_syntax('At least one gate must be given')
	do ii=0,MAXRES-1
	  good(ii)=.FALSE.
	end do
	do nnn=1,gate.ngates
	  call gs_get2ilim(jwlo,jwhi,0,gate.res-1)
	  do ii=jwlo,jwhi
	    good(ii)=.TRUE.
	  end do
	end do

	call gs_getinout(gate.inside)
	if(gate.inside) then
	  do ii=0,gate.res-1
	    good(ii)=.NOT.good(ii)
	  end do
	endif

	call gs_getfoldgate(gate.ind,gate.fl,gate.fh,gate.same)

	ibsize=4*gate.res
	if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for GATES'
	com(icmd).size=ibsize
	call vmovl(good,1,%val(com(icmd).addr),1,gate.res)	! riporta i valori

	return

	end

	subroutine gs1_gates(gate,dat)

#include "gsort.inc"
	record/gate/gate
	record/gatesdat/dat

	logical*1 inside

	call gs_putindpar(gate.ind,gate.par)

	write(list(llist:),'(i5)') gate.ngates
	llist=lengthc(list)+2

	if(gate.inside) then	! go to the "good" representation
	  do ii=0,gate.res-1
	    dat.bad(ii)=.NOT.dat.bad(ii)
	  end do
	endif

	ngates=0
	if(dat.bad(0)) then
	  jwlo=0
	  inside=.TRUE.
	else
	  inside=.FALSE.
	endif

	do ii=1,gate.res-1
	  if(dat.bad(ii).neqv.inside) then	! boundary
	    if(inside) then
	      jwhi=ii-1
	      ngates=ngates+1
	      write(list(llist:),'(2i5)') jwlo,jwhi
	      llist=lengthc(list)+2
	      inside=.FALSE.
	    else
	      jwlo=ii
	      inside=.TRUE.
	    endif
	  endif
	end do
	if(inside) then
	  jwhi=gate.res-1
	  ngates=ngates+1
	  write(list(llist:),'(2i5)') jwlo,jwhi
	  llist=lengthc(list)+2
	endif
	if(ngates.ne. gate.ngates) stop 'Inconsistency in GS1_GATES'

	if(gate.inside) then	! go back to the "bad" representation
	  do ii=0,gate.res-1
	    dat.bad(ii)=.NOT.dat.bad(ii)
	  end do
	endif

	llist=lengthc(list)+3
	call gs_putinout(gate.inside)

	llist=llist+2
	call gs_putfoldgate(gate.ind,gate.fl,gate.fh,gate.same)

	return

	end

	subroutine gs2_gates

	return

	end

	subroutine gs3_gates(gate,dat)

#include "gsort.inc"
	record/gate/gate
	record/gatesdat/dat

	ji=gate.ind
	jp=gate.par
	ires=gate.res
	nelim=0
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  ICAN=det(jx).ival(jp)
	  if(ican.GE.0 .and. ican.LT.ires) then
	    if(dat.bad(ican)) then
	      nelim=nelim+1
	      det(jx).id=-1
	    endif
	  else
	    nelim=nelim+1
	    det(jx).id=-1
	  endif
	end do
	if(nelim.ne.0 .and. gate.same) goto 9000
	nx=ndet(ji)-nelim
	if(nx.LT.gate.fl .or. nx.GT.gate.fh) goto 9000
	if(nelim.GT.0) call gs_checkdetector(ji)
	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_gates

	return

	end



	subroutine gs0_window(win)

#include "gsort.inc"
	record/window/win

	synt(1)='WINDOW  P  (Wl Wh){#parameters_times}  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getind(win.ind)
	do ii=0,TDPAR(win.ind)-1
	  nn=istr_i2(comline,win.lo(ii),win.hi(ii))
	  if(nn.ne.2) call gs_syntax('Wrong number of parameters')
	end do

	call gs_getfoldgate(win.ind,win.fl,win.fh,win.same)

	return

	end

	subroutine gs1_window(win)

#include "gsort.inc"
	record/window/win
#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif

	call gs_putind(win.ind)

	npar=TDPAR(win.ind)

#if defined( __GFORTRAN__ )
	write(dyn_format,'(a,i0,a)')'(',2*npar,'i5)'
	write(list(llist+3:),dyn_format) (win.LO(ii),win.HI(ii),ii=0,npar-1)
#else
	write(list(llist+3:),4201) (win.LO(ii),win.HI(ii),ii=0,npar-1)
4201	format(<2*npar>i5)
#endif
	llist=lengthc(list)+1	
	call gs_putfoldgate(win.ind,win.fl,win.fh,win.same)

	return

	end

	subroutine gs2_window

	return

	end

	subroutine gs3_window(win)

#include "gsort.inc"
	record/window/win

	ji=win.ind
	npar=TDPAR(ji)
	nx=ndet(ji)
	nelim=0
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  do jp=0,npar-1
	    ICAN=det(jx).ival(jp)
	    if(ICAN.LT.WIN.lo(jp) .OR. ICAN.GT.WIN.hi(jp)) then
	      det(jx).id=-1
	      nelim=nelim+1
	      goto 100
	    endif
	  end do
100	end do
	if(nelim.ne.0 .and. win.same) goto 9000
	nx=ndet(ji)-nelim
	if(nx.LT.win.fl .or. nx.GT.win.fh) goto 9000
	if(nelim.ne.0) call gs_checkdetector(ji)

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_window

	return

	end




	subroutine gs0_banana(ban)

#include "gsort.inc"
	record/banana/ban
	logical*1 getmem
	logical*1 sign
	
	character*72 banfile

	synt(1)='BANANA Px Py  banana_file  RES Rx Ry  [IN|OUT]  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	do nn=1,2
	  call gs_getindpar(ban.ind(nn),ban.par(nn),ires)
	end do
	if(ban.ind(1).ne.ban.ind(2)) call gs_syntax('Both parameters must be in the same detector')
	ban.nban=1

	call istr_ch(comline,banfile)
	if(lengthc(banfile).LT.1) call gs_syntax('Filename of banana expected')
        call filetype(banfile,'ban')

	call gs_getbanres(ban.res)

	call gs_getinout(ban.inside)

	call gs_getfoldgate(ban.ind(1),ban.fl,ban.fh,ban.same)

	namesize= 72
	ibsize= namesize + 4*(2*ban.res(1)*nitems(ban.ind(1)))
	if(.NOT.getmem(IBSIZE,com(icmd).addr)) Stop 'Error allocating memory for banana'
	com(icmd).size=ibsize
#if defined( __sun_c__ )
	call vmovb(banfile(1:1),1,%val(com(icmd).addr),1,namesize)	! riporta i nomi
#else
	call vmovb(%ref(banfile),1,%val(com(icmd).addr),1,namesize)	! riporta i nomi
#endif
	return

	end

	subroutine gs1_banana(ban,dat)

#include "gsort.inc"
	record/banana/ban
	record/bananadat/dat

	do nn=1,2
	  call gs_putindpar(ban.ind(nn),ban.par(nn))
	  llist=llist+3
	end do

	llist=lengthc(list)+2
	lbanfile=max(1,lengthc(dat.file))
	write(list(llist:),'(a)') dat.file(1:lbanfile)

	llist=lengthc(list)+2
	call gs_putbanres(ban.res)
	
	llist=llist+3
	call gs_putinout(ban.inside)

	llist=llist+2
	call gs_putfoldgate(ban.ind(1),ban.fl,ban.fh,ban.same)

	return

	end

	subroutine gs2_banana(ban,dat)

#include "gsort.inc"
	record/banana/ban
	record/bananadat/dat
	integer iok,nbanread,ii

	iresx=ban.res(1)
	iresy=ban.res(2)

	if(init_mode.EQ.0) then
	 lbanfile=max(1,lengthc(dat.file))
	 WRITE(LU1,*)
	 WRITE(LU1,'('' banana data for  '',2(3X,A,I1),3X,A)')
	1      dname(ban.ind(1)),ban.par(1),dname(ban.ind(2)),ban.par(2),
	1      'Taken from File '//dat.file(1:lbanfile)
	  call lib$get_lun(lun)
	  open(unit=lun,file=dat.file(1:lbanfile),status='old',readonly,err=20)
	  ban.multiadc = .false.
	  do ii = 0, nitems(ban.ind(1))-1
	     call ban_findbanana(lun,-1,ii,iok)
	     if( iok .gt. 0 ) then
	       ban.multiadc = .true.
	       goto 10
	     endif
	  enddo
10	  close(lun)
	  
	  if( ban.multiadc ) then
	    nbanread = 0
	    do ii = 0, nitems(ban.ind(1))-1
	      ipos=ii*ban.res(1)
	      nbpoints = -1
	      call ban_getbanomap(LU1,dat.file,-1,ii,dat.ban(1,ipos),iresx,iresy,nbpoints)
	      if( nbpoints .gt. 0 ) then 
	        ban.ignore(ii) = .false.
		nbanread = nbanread + 1
	      else 
	        ban.ignore(ii) = .true.
	      endif
	    enddo
	    nbpoints = nbanread
	  else
	    call ban_getbanomap(LU1,dat.file,-1,-1,dat.ban(1,0),iresx,iresy,nbpoints)
	  endif
	  if(nbpoints.le.0) stop 'Error reading banana'

	else
	  lbanfile=max(1,lengthc(dat.file))
	  WRITE(LU2,*)
	  WRITE(LU2,'('' Memory map for banANA  '',2(3X,A,I1),3X,A)')
	1      dname(ban.ind(1)),ban.par(1),dname(ban.ind(2)),ban.par(2),
	1      'Taken from File '//dat.file(1:lbanfile)
	  call ban_listbanana(LU2,dat.ban(1,0),iresx,iresy,64,32)
	endif
	break=.FALSE.

	return
20	write(lu1,*) 'Error opening   ',dat.file(1:lbanfile)
        stop
	
	end

	subroutine gs3_banana(ban,dat)

#include "gsort.inc"
	record/banana/ban
	record/bananadat/dat

	logical*1 offban

	ji=ban.ind(1)
	jn1=ban.par(1)
	jn2=ban.par(2)
	ires=ban.res(1)
	nelim=0
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  ICAN=det(jx).ival(jn1)
	  if(ican.GE.0 .and. ican.LT.ires) then
	    IVAL=det(jx).ival(jn2)
	    if ( ban.multiadc )then
	      if( ban.ignore(det(jx).id) ) goto 10
	      ipos = det(jx).id*ires
	    else
	      ipos = 0
	    endif
	    offban=IVAL.LT. dat.ban(1,ican+ipos) .OR. IVAL.GT. dat.ban(2,ican+ipos)
	    if(offban .neqv. ban.inside) goto 100
	  endif
10	  det(jx).id=-1
	  nelim=nelim+1
100	end do
	if(nelim.ne.0 .and. ban.same) goto 9000
	nx=ndet(ji)-nelim
	if(nx.LT.ban.fl .or. nx.GT.ban.fh) goto 9000
	if(nelim.ne.0) call gs_checkdetector(ji)

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_banana

	return

	end



	subroutine gs0_bananas(ban)

#include "gsort.inc"
	record/banana/ban
	logical*1 getmem
	logical*1 sign
	
	character*72 banfile(MAXNban)

	synt(1)='BANANAS Px Py  #bananas banana_file{#bananas_times} RES Rx Ry  [IN|OUT] FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	do nn=1,2
	  call gs_getindpar(ban.ind(nn),ban.par(nn),ires)
	end do
	if(ban.ind(1).ne.ban.ind(2)) call gs_syntax('Both parameters must be in the same detector')

	nn=istr_i1(comline,ban.nban)
	if(nn.ne.1) call gs_syntax('Number of bananas expected')
	if(ban.nban.LT.1 .or. ban.nban .GT. MAXNban) call gs_syntax1('Number of bananas must be between 1 and',MAXNban)
	do nban=1,ban.nban
	  call istr_ch(comline,banfile(nban))
	  if(lengthc(banfile(nban)).LT.1) call gs_syntax('Filename of banana expected')
          call filetype(banfile(nban),'ban')
	end do

	call gs_getbanres(ban.res)

	call gs_getinout(ban.inside)

	call gs_getfoldgate(ban.ind(1),ban.fl,ban.fh,ban.same)

	namesize= 72*MAXNban
	ibsize= namesize + 4*(2*ban.res(1)*ban.nban)
	if(.NOT.getmem(IBSIZE,com(icmd).addr)) Stop 'Error allocating memory for banANAS'
	com(icmd).size=ibsize
	call vmovb(%ref(banfile),1,%val(com(icmd).addr),1,namesize)	! riporta i nomi

	return

	end

	subroutine gs1_bananas(ban,dat)

#include "gsort.inc"
	record/banana/ban
	record/bananasdat/dat

	do nn=1,2
	  call gs_putindpar(ban.ind(nn),ban.par(nn))
	  llist=llist+3
	end do

	write(list(llist+1:),'(i5)') ban.nban
	do ii=1,ban.nban
	  llist=lengthc(list)+2
	  lbanfile=max(1,lengthc(dat.file(ii)))
	  write(list(llist:),'(a)') dat.file(ii)(1:lbanfile)
	end do

	llist=max(1,lengthc(list))+2
	call gs_putbanres(ban.res)

	llist=llist+3
	call gs_putinout(ban.inside)

	llist=llist+2
	call gs_putfoldgate(ban.ind(1),ban.fl,ban.fh,ban.same)

	return

	end

	subroutine gs2_bananas(ban,dat)

#include "gsort.inc"
	record/banana/ban
	record/bananasdat/dat

	iresx=ban.res(1)
	iresy=ban.res(2)

	if(init_mode.EQ.0) then
	  do nn=1,ban.nban
	    lbanfile=max(1,lengthc(dat.file(nn)))
	    WRITE(LU1,*)
	    WRITE(LU1,'('' banana data for  '',2(3X,A,I1),3X,A)')
	1      dname(ban.ind(1)),ban.par(1),dname(ban.ind(2)),ban.par(2),
	1      'Taken from File '//dat.file(nn)(1:lbanfile)
	    ipos=(nn-1)*ban.res(1)
	    call ban_getbanana(LU1,dat.file(nn),-1,-1,dat.ban(1,ipos),iresx,iresy,nbpoints)
	    if(nbpoints.le.0) stop 'Error reading banana'
	  end do
	else
	  do nn=1,ban.nban
	    lbanfile=max(1,lengthc(dat.file(nn)))
	    WRITE(LU2,*)
	    WRITE(LU2,'('' Memory map for banANA  '',2(3X,A,I1),3X,A)')
	1      dname(ban.ind(1)),ban.par(1),dname(ban.ind(2)),ban.par(2),
	1      'Taken from File '//dat.file(nn)(1:lbanfile)
	    ipos=(nn-1)*ban.res(1)
	    call ban_listbanana(LU2,dat.ban(1,ipos),iresx,iresy,64,32)
	  end do
	endif
	break=.FALSE.

	return

	end

	subroutine gs3_bananas(ban,dat)

#include "gsort.inc"
	record/banana/ban
	record/bananasdat/dat

	logical*1 offban

	ji=ban.ind(1)
	jn1=ban.par(1)
	jn2=ban.par(2)
	ires=ban.res(1)
	nelim=0
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  ICAN=det(jx).ival(jn1)
	  if(ican.GE.0 .and. ican.LT.ires) then
	    IVAL=det(jx).ival(jn2)
	    jpos=0
	    do nn=1,ban.nban
	      ipos=ican+jpos
	      offban=IVAL.LT. dat.ban(1,ipos) .OR. IVAL.GT. dat.ban(2,ipos)
	      if(offban .neqv. ban.inside) goto 100
	      jpos=jpos+ires
	    end do
	  endif
	  det(jx).id=-1
	  nelim=nelim+1
100	end do
	if(nelim.ne.0 .and. ban.same) goto 9000
	nx=ndet(ji)-nelim
	if(nx.LT.ban.fl .or. nx.GT.ban.fh) goto 9000
	if(nelim.ne.0) call gs_checkdetector(ji)

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_bananas

	return

	end
***************************************************************************
*       XBAN

	subroutine gs0_xban(ban)

#include "gsort.inc"
	record/banana/ban
	logical*1 getmem
	logical*1 twopar
	logical*1 gs_testindpar
	external gs_testindpar

	character*72 banfile

	synt(1)='XBAN Px [Py] banana_file  RES Rx [Ry] [IN|OUT]  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(ban.ind(1),ban.par(1),ires)
	if( gs_testindpar(ban.ind(2),ban.par(2),ires) )then
	       call gs_getindpar(ban.ind(2),ban.par(2),ires)
		if( ban.ind(2) .ne. ban.ind(1) )call gs_syntax('Parameters must be in the same detector')
		twopar = .TRUE.
	else
	       ban.ind(2) = ban.ind(1)
		ban.par(2) = ban.par(1)
		twopar = .FALSE.
	endif
	
	if( ban.par(1).eq.ban.par(2) ) then
          if( nitems(ban.ind(1)) .lt. 2 )call gs_syntax('Cannot XBAN on the same parameter with detector fold less than 2')
	endif
	  
	ban.nban=1

	call istr_ch(comline,banfile)
	if(lengthc(banfile).LT.1) call gs_syntax('Filename of banana expected')
        call filetype(banfile,'ban')

	if(twopar)then 
	  call gs_getbanres(ban.res)
	else
	  call gs_getxbanres(ban.res)
	endif

	call gs_getinout(ban.inside)

	call gs_getfoldgate(ban.ind(1),ban.fl,ban.fh,ban.same)

	namesize= 72
	ibsize= namesize + 4*(2*ban.res(1))
	if(.NOT.getmem(IBSIZE,com(icmd).addr)) Stop 'Error allocating memory for banana'
	com(icmd).size=ibsize
#if defined( __sun_c__ )
	call vmovb(banfile(1:1),1,%val(com(icmd).addr),1,namesize)	! riporta i nomi
#else
	call vmovb(%ref(banfile),1,%val(com(icmd).addr),1,namesize)	! riporta i nomi
#endif
	return

	end

	subroutine gs1_xban(ban,dat)

#include "gsort.inc"
	record/banana/ban
	record/bananadat/dat

	if( ban.par(1) .eq. ban.par(2) )then
	  call gs_putindpar(ban.ind(1),ban.par(1))
	  llist=llist+3
	else
	  do nn=1,2
	    call gs_putindpar(ban.ind(nn),ban.par(nn))
	    llist=llist+3
	  end do
	endif

	llist=lengthc(list)+2
	lbanfile=max(1,lengthc(dat.file))
	write(list(llist:),'(a)') dat.file(1:lbanfile)

	llist=lengthc(list)+2
	if( ban.par(1) .eq. ban.par(2) )then
	  call gs_putxbanres(ban.res)
	else
	  call gs_putbanres(ban.res)
	endif
	
	llist=llist+3
	call gs_putinout(ban.inside)

	llist=llist+2
	call gs_putfoldgate(ban.ind(1),ban.fl,ban.fh,ban.same)

	return

	end

	subroutine gs2_xban(ban,dat)

#include "gsort.inc"
	record/banana/ban
	record/bananadat/dat
	integer iok,nbanread,ii

	iresx=ban.res(1)
	iresy=ban.res(2)

	if(init_mode.EQ.0) then
	 lbanfile=max(1,lengthc(dat.file))
	 WRITE(LU1,*)
	 WRITE(LU1,'('' XBAN banana data for  '',3X,A,I1,3X,A)')
	1      dname(ban.ind(1)),ban.par(1),
	1      'Taken from File '//dat.file(1:lbanfile)
	  call lib$get_lun(lun)
	  open(unit=lun,file=dat.file(1:lbanfile),status='old',readonly,err=20)
	  ban.multiadc = .false.
	  do ii = 0, nitems(ban.ind(1))-1
	     call ban_findbanana(lun,-1,ii,iok)
	     if( iok .gt. 0 ) then
	       ban.multiadc = .true.
	       goto 10
	     endif
	  enddo
10	  close(lun)
	  if( ban.multiadc ) then
	     WRITE(LU1,'('' Banana for XBAN cannot be ADC-dependent'')')
	     stop
	  endif
	  
	  call ban_getbanomap(LU1,dat.file,-1,-1,dat.ban(1,0),iresx,iresx,nbpoints)
	  if(nbpoints.le.0) stop 'Error reading banana'

*	else
*	  lbanfile=max(1,lengthc(dat.file))
*	  WRITE(LU2,*)
*	  WRITE(LU2,'('' Memory map for XBAN banana  '',3X,A,I1,3X,A)')
*	1      dname(ban.ind(1)),ban.par(1),
*	1      'Taken from File '//dat.file(1:lbanfile)
*	  call ban_listbanana(LU2,dat.ban(1,0),iresx,iresx,64,32)
	endif
	break=.FALSE.

	return
20	write(lu1,*) 'Error opening   ',dat.file(1:lbanfile)
        stop
	
	end

	subroutine gs3_xban(ban,dat)

#include "gsort.inc"
	record/banana/ban
	record/bananadat/dat

	if( ban.par(1) .eq. ban.par(2) )then
	  call gs3_samepar_xban(ban,dat)
	else
	  call gs3_diffpar_xban(ban,dat)
	endif

	return
	end

	subroutine gs3_samepar_xban(ban,dat)

#include "gsort.inc"
	record/banana/ban
	record/bananadat/dat

	logical*1 offban

	
	if( ndet(ban.ind(1)).LT.(ban.fl+2) ) goto 9000
	ji=ban.ind(1)
	jn1=ban.par(1)
	ires=ban.res(1)
	nelim = 0
	
	do jx=doff(ji),doff(ji)+ndet(ji)-2
	  ICAN=det(jx).ival(jn1)
	  if(ican.GE.0 .and. ican.LT.ires )then
	  itimes = 0
	  do jy=jx+1,doff(ji)+ndet(ji)-1
	     if(det(jy).ival(jn1).GE.0 .and. det(jy).ival(jn1).LT.ires) then
	       IVAL=det(jy).ival(jn1)
	       if( (IVAL.LT. dat.ban(1,ican)) .OR. (IVAL.GT. dat.ban(2,ican)) )itimes = 1
	       if( (ICAN.LT. dat.ban(1,ival)) .OR. (ICAN.GT. dat.ban(2,ival)) )itimes = itimes + 1
	       offban = itimes .ne. 1
	       if(offban .neqv. ban.inside) then
                   det(jx).id = -1
		   det(jy).id = -1
	           nelim=2
	           goto 100
	       endif
	     endif
	  enddo
	  endif
	end do
	goto 9000

100	nx=ndet(ji)-nelim
	if(nx.LT.ban.fl .or. nx.GT.ban.fh) goto 9000
	call gs_checkdetector(ji)

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs3_diffpar_xban(ban,dat)

#include "gsort.inc"
	record/banana/ban
	record/bananadat/dat

	logical*1 offban

	
	if( ndet(ban.ind(1)).LT.(ban.fl+1) ) goto 9000
	ji=ban.ind(1)
	jn1=ban.par(1)
	jn2=ban.par(2)
	ires=ban.res(1)
	nelim = 0
	
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	 ICAN=det(jx).ival(jn1)
	 if(ican.GE.0 .and. ican.LT.ires )then
	  IVAL=det(jx).ival(jn2)
	  offban = (IVAL.LT. dat.ban(1,ican)) .OR. (IVAL.GT. dat.ban(2,ican))
	  if(offban .neqv. ban.inside) then
            det(jx).id = -1
	    nelim=1
	    goto 100
	  endif
	 endif
	end do
	goto 9000

100	nx=ndet(ji)-nelim
	if(nx.LT.ban.fl .or. nx.GT.ban.fh) goto 9000
	call gs_checkdetector(ji)

	return

9000	killev=.TRUE.
	return

	end


	subroutine gs4_xban

	return

	end


****************************************************************************

	subroutine gs0_gain(rcl)

#include "gsort.inc"
	record/recal/rcl
	character*10 ch1

	synt(1)='GAIN  Pn  offset gain  [INTEGER] Wl Wh  FOLD_GATE '

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(rcl.ind,rcl.par,ires)

	rcl.run=.FALSE.
	rcl.round = .FALSE.

	ii=istr_r2(comline,rcl.offs,rcl.gain)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')
	
	ii = lengthn(comline)
	if( ii .lt. 0 )then
	   call istr_ch2up(comline,ch1)
	   if( ch1(1:1) .eq. 'I')rcl.round = .true.
	endif

	ii=istr_i2(comline,rcl.wlo,rcl.whi)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	call gs_getfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	return

	end

	subroutine gs1_gain(rcl)

#include "gsort.inc"
	record/recal/rcl

	call gs_putindpar(rcl.ind,rcl.par)

	if( rcl.round )then
	  write(list(llist+1:),'(F12.2,F12.4,''  INTEGER'',2I6)') rcl.offs,rcl.gain,rcl.wlo,rcl.whi
	else
	  write(list(llist+1:),'(F12.2,F12.4,2I6)') rcl.offs,rcl.gain,rcl.wlo,rcl.whi
	endif

	llist=lengthc(list)+2
	call gs_putfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	return

	end

	subroutine gs2_gain

	return

	end

	subroutine gs3_gain(rcl)

#include "gsort.inc"
	record/recal/rcl

	integer nelim,jx,nx

	ji=rcl.ind
	jp=rcl.par
	nelim = 0
	if(ji.EQ.0) then
	  ican=det(0).ival(jp)
	  if(ican.GT.0) then
	    xcan=rcl.offs+det(0).xval(jp)*rcl.gain
	    ican=xcan
	  endif
	  if(ican.LT.rcl.wlo .or. ican.GT.rcl.whi) goto 9000
	  det(0).ival(jp)=ican
	  det(0).xval(jp)=xcan
	  if( rcl.round )det(0).xval(jp) = ican
	else
	  nelim=0
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	    ican=det(jx).ival(jp)
	    if(ican.GT.0) then
	      xcan=rcl.offs+det(jx).xval(jp)*rcl.gain
	      ican=xcan
	    endif
	    if(ican.GE.rcl.wlo .and. ican.le.rcl.whi) then
	      det(jx).ival(jp)=ican
	      det(jx).xval(jp)=xcan
	      if( rcl.round )det(jx).xval(jp) = ican
	    else
	      det(jx).id=-1
	      nelim=nelim+1
	    endif
	  end do
	endif
	if(nelim.ne.0 .and. rcl.same) goto 9000
	nx=ndet(ji)-nelim
	if(nx.LT.rcl.fl .or. nx.GT.rcl.fh) goto 9000
	if(nelim.GT.0) call gs_checkdetector(ji)

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_gain

	return

	end


************************************ Thresholds

	subroutine gs0_thresholds(rcl)

#include "gsort.inc"
	record/recal/rcl
	character*4 aaa
	logical*1 getmem

	synt(1)='THRESHOLDS  Pn  file.thr [NORUN|RUN] [ZERO]  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(rcl.ind,rcl.par,ires)
	rcl.wlo=0
	rcl.whi=ires-1
	rcl.offs=0.
	rcl.gain=1.
	call istr_ch(comline,rcl.file)
	if(lengthc(rcl.file).LT.1) call gs_syntax('Filename expected')
	call filetype(rcl.file,'thr')

	call gs_getrunmode(rcl.run)
	
	if( lengthn(comline) .le. 0 )then
	call istr_ch(comline, aaa)
	if(lengthc(aaa).EQ.4)then
	   call str_toupper(aaa)
	   if( aaa .eq. 'ZERO' ) rcl.zero = .TRUE.
	elseif( lengthc(aaa).GT.0 )then
	   call gs_syntax('Invalid keyword')
	endif
	endif


	call gs_getfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	ibsize=MAXDET + MAXRCL_ORD*MAXDET
	ibsize=4*ibsize
	if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for THRESHOLDS command'
	com(icmd).size=ibsize

	return

	end

	subroutine gs1_thresholds(rcl)

#include "gsort.inc"
	record/recal/rcl

	call gs_putindpar(rcl.ind,rcl.par)

	lcalfile=max(1,lengthc(rcl.file))
	write(list(llist+2:),'(A)') rcl.file(1:lcalfile)

	llist=lengthc(list)+3
	call gs_putrunmode(rcl.run)
	
	if( rcl.zero )then
	   llist=lengthc(list)+2
	   write(list(llist:),'(''ZERO'')') 
           llist=lengthc(list)+2
	else
	   llist = lengthc(list)+2
	endif
	
	call gs_putfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	return

	end

	subroutine gs2_thresholds(rcl,dat)

#include "gsort.inc"
#if defined( __GFORTRAN__ )
	logical*1 inp_yes
	external inp_yes
#endif
	record/recal/rcl
	record/calcoef/dat(0:1)

	REAL	  TEMP   (0:MAXDET-1)
	LOGICAL	  MISSING(0:MAXDET-1)

	if(init_mode.EQ.0) then
	  if(rcl.run) return
	else
	  if(.NOT.rcl.run) return
	endif

	j_ind=rcl.ind
	j_par=rcl.par

	ntoget=nitems(j_ind)
	ntoget_t=ntoget

	if(j_ind.EQ.0) then
	  missing(j_par)=.TRUE.
	else
	  DO II=0,ntoget_t-1
	    MISSING(II)=.TRUE.
	  end do
	endif

2	lrcl_file=max(1,lengthc(rcl.file))
	OPEN(UNIT=gs2lun,FILE=rcl.file,STATUS='OLD',READONLY,err=5)
	DO ILU=LU1,LU2
	 WRITE(ILU,*)
	 WRITE(ILU,'(A,I1)')' Threshold values for '//dname(j_ind),j_par
	 WRITE(ILU,*)'Taken from File '//rcl.file(1:lrcl_file)
	end do
	goto 10

5	write(6,*) 'Error reading ',rcl.file(1:lrcl_file)
	if(inp_yes('Retry')) goto 2
	IOK=-1
	RETURN	

10	READ(gs2lun,*,ERR=20,END=20) ICTAP,ICADC,(TEMP(J),J=1,2)
	JPAR = 2
	IF(ICTAP.EQ.iio.run .OR. .NOT. rcl.run) THEN
	  IF(MISSING(ICADC)) THEN
	    if(j_ind.ne.0) then
	      WRITE(LU2,4201)ICADC,(TEMP(J),J=1,JPAR)
	      dat(ICADC).ord=JPAR
	      DO J=1,JPAR
	        dat(ICADC).coef(J)=TEMP(j)
	      end do
	    else
	      WRITE(LU2,4202)ICADC,(TEMP(J),J=1,JPAR)
	      rcl.offs=temp(1)
	      rcl.gain=temp(2)
	    endif
	    MISSING(ICADC)=.FALSE.
	    ntoget=ntoget-1
	    IF(ntoget.EQ.0) GOTO 20
	  ENDIF
	ENDIF
	GOTO 10

#if defined( __GFORTRAN__ )
4201	format(' ADC#',I5,2G0)
4202	format(' FPAR#',I4,2G0)
#else
4201	format(' ADC#',I5,<JPAR>G)
4202	format(' FPAR#',I4,<JPAR>G)
#endif

20	CLOSE(gs2lun)

	if(j_ind.EQ.0) then
	  if(missing(j_par)) then
	    DO ILU=LU1,LU2
		rcl.offs=0
		rcl.gain=0
		WRITE(ILU,'('' FPAR#'',I4,3X,A)') II,' is missing ===> set to zero'
	    end do
	  endif
	  break=.FALSE.
	  return
	endif

	DO ii=0,ntoget_t-1
	   IF(MISSING(II)) THEN
	     DO ILU=LU1,LU2
		dat(ii).ord=0
		WRITE(ILU,'('' ADC#'',I5,3X,A)') ii,' is missing ===> set to zero'
	     end do
	   ENDIF
	end do


	break=.FALSE.
	return

	end

	subroutine gs3_thresholds(rcl,dat)

#include "gsort.inc"
	record/recal/rcl
	record/calcoef/dat(0:1)

	ji=rcl.ind
	jp=rcl.par
	if(ji.EQ.0) then
	  if(det(0).ival(jp).LT.rcl.offs .or. det(0).ival(jp).GT.rcl.gain) then
	    if( rcl.zero ) then
	        det(0).ival(jp) = 0
		det(0).xval(jp) = 0.0E0
	    else
	      goto 9000
	    endif
	  endif
	  
	else
	  nelim=0
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	    id=det(jx).id
	    ncoef=dat(id).ord
	    if(ncoef.le.0) then
	      det(jx).id=-1			! mancano i coefficienti ==> elimina l'ADC
	      nelim=nelim+1
	    else
	      ICAN=det(jx).ival(jp)
	      if(ican.lt.dat(id).coef(1) .or. ican.gt.dat(id).coef(2)) then
	        det(jx).ival(jp)=0
	        det(jx).xval(jp)=0.00
	        if( .not. rcl.zero ) then
	          det(jx).id=-1			! elimina l'ADC
	          nelim=nelim+1
		endif
	      endif
	    endif
	  end do
	  if(nelim.ne.0 .and. rcl.same) goto 9000
	  nx=ndet(ji)-nelim
	  if(nx.LT.rcl.fl .or. nx.GT.rcl.fh) goto 9000
	  if(nelim.GT.0) call gs_checkdetector(ji)
	endif

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_thresholds

	return

	end

************************************ end Thresholds

	subroutine gs0_recal(rcl)

#include "gsort.inc"
	record/recal/rcl
	character*4 aaa
	logical*1 getmem

	synt(1)='RECAL  Pn  file.cal [NORUN|RUN] [ZERO] offset gain  Wl Wh  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(rcl.ind,rcl.par,ires)
	rcl.wlo=0
	rcl.whi=ires-1
	rcl.offs=0.
	rcl.gain=1.
	call istr_ch(comline,rcl.file)
	if(lengthc(rcl.file).LT.1) call gs_syntax('Filename expected')
	call filetype(rcl.file,'cal')

	call gs_getrunmode(rcl.run)
	
	if( lengthn(comline) .le. 0 )then
	call istr_ch(comline, aaa)
	if(lengthc(aaa).EQ.4)then
	   call str_toupper(aaa)
	   if( aaa .eq. 'ZERO' ) rcl.zero = .TRUE.
	elseif( lengthc(aaa).GT.0 )then
	   call gs_syntax('Invalid keyword')
	endif
	endif

	ii=istr_r2(comline,rcl.offs,rcl.gain)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	ii=istr_i2(comline,rcl.wlo,rcl.whi)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	call gs_getfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	ibsize=MAXDET + MAXRCL_ORD*MAXDET
	ibsize=4*ibsize
	if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for RECAL'
	com(icmd).size=ibsize

	return

	end

	subroutine gs1_recal(rcl)

#include "gsort.inc"
	record/recal/rcl

	call gs_putindpar(rcl.ind,rcl.par)

	lcalfile=max(1,lengthc(rcl.file))
	write(list(llist+2:),'(A)') rcl.file(1:lcalfile)

	llist=lengthc(list)+3
	call gs_putrunmode(rcl.run)
	
	if( rcl.zero )then
	   llist=lengthc(list)+2
	   write(list(llist:),'(''ZERO'')') 
           llist=lengthc(list)+2
	endif
	
	write(list(llist+1:),'(F12.2,F12.4,2I6)') rcl.offs,rcl.gain,rcl.wlo,rcl.whi

	llist=lengthc(list)+2
	call gs_putfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	return

	end

	subroutine gs2_recal(rcl,dat)

#include "gsort.inc"
#if defined( __GFORTRAN__ )
	character*64 dyn_format
	logical*1 inp_yes
	external inp_yes
#endif
	record/recal/rcl
	record/calcoef/dat(0:1)

	REAL	  TEMP   (0:MAXDET-1)
	LOGICAL	  MISSING(0:MAXDET-1)

	if(init_mode.EQ.0) then
	  if(rcl.run) return
	else
	  if(.NOT.rcl.run) return
	endif

	j_ind=rcl.ind
	j_par=rcl.par

	ntoget=nitems(j_ind)
	ntoget_t=ntoget

	if(j_ind.EQ.0) then
	  missing(j_par)=.TRUE.
	else
	  DO II=0,ntoget_t-1
	    MISSING(II)=.TRUE.
	  end do
	endif

2	lrcl_file=max(1,lengthc(rcl.file))
	OPEN(UNIT=gs2lun,FILE=rcl.file,STATUS='OLD',READONLY,err=5)
	DO ILU=LU1,LU2
	 WRITE(ILU,*)
	 WRITE(ILU,'(A,I1)')' Recalibration coefficients for '//dname(j_ind),j_par
	 WRITE(ILU,*)'Taken from File '//rcl.file(1:lrcl_file)
	end do
	goto 10

5	write(6,*) 'Error reading ',rcl.file(1:lrcl_file)
	if(inp_yes('Retry')) goto 2
	IOK=-1
	RETURN	

10	READ(gs2lun,*,ERR=20,END=20) ICTAP,ICADC,JPAR,(TEMP(J),J=1,JPAR)
#if defined( __GFORTRAN__ )
	write(dyn_format,'(i0)')jpar
#endif
	IF(ICTAP.EQ.iio.run .OR. .NOT. rcl.run) THEN
	  IF(MISSING(ICADC)) THEN
	    if(j_ind.ne.0) then
#if defined( __GFORTRAN__ )
	      WRITE(LU2,'(''ADC#'',I5,'//dyn_format(1:len(dyn_format))//'G0)')ICADC,(TEMP(J),J=1,JPAR)
	      if(jpar.GT.maxRCL_ORD) then
	        WRITE(LU1,'(''ADC#'',I5,'//dyn_format(1:len(dyn_format))//'G0)')ICADC,(TEMP(J),J=1,JPAR)
#else
	      WRITE(LU2,4201)ICADC,(TEMP(J),J=1,JPAR)
	      if(jpar.GT.maxRCL_ORD) then
	        WRITE(LU1,4201)ICADC,(TEMP(J),J=1,JPAR)
#endif
	        stop 'Too high order of recalibration'
	      endif
	      dat(ICADC).ord=JPAR
	      DO J=1,JPAR
	        dat(ICADC).coef(J)=TEMP(j)
	      end do
	    else
#if defined( __GFORTRAN__ )
	      WRITE(LU2,'(''FPAR#'',I4,'//dyn_format(1:len(dyn_format))//'G0)')ICADC,(TEMP(J),J=1,JPAR)
	      if(jpar.GT.2) then
	        WRITE(LU1,'(''FPAR#'',I4,'//dyn_format(1:len(dyn_format))//'G0)')ICADC,(TEMP(J),J=1,JPAR)
#else
	      WRITE(LU2,4202)ICADC,(TEMP(J),J=1,JPAR)
	      if(jpar.GT.2) then
	        WRITE(LU1,4202)ICADC,(TEMP(J),J=1,JPAR)
#endif
	        stop 'Too high order of recalibration'
	      endif
	      rcl.offs=temp(1)
	      rcl.gain=temp(2)
	    endif
	    MISSING(ICADC)=.FALSE.
	    ntoget=ntoget-1
	    IF(ntoget.EQ.0) GOTO 20
	  ENDIF
	ENDIF
	GOTO 10

#if !defined( __GFORTRAN__ )
4201	format(' ADC#',I5,<JPAR>G)
4202	format(' FPAR#',I4,<JPAR>G)
#endif

20	CLOSE(gs2lun)

	if(j_ind.EQ.0) then
	  if(missing(j_par)) then
	    DO ILU=LU1,LU2
		rcl.offs=0
		rcl.gain=0
		WRITE(ILU,'('' FPAR#'',I4,3X,A)') II,' is missing ===> set to zero'
	    end do
	  endif
	  break=.FALSE.
	  return
	endif

	DO ii=0,ntoget_t-1
	   IF(MISSING(II)) THEN
	     DO ILU=LU1,LU2
		dat(ii).ord=0
		WRITE(ILU,'('' ADC#'',I5,3X,A)') ii,' is missing ===> set to zero'
	     end do
	   ENDIF
	end do

	do ii=0,ntoget_t-1
	  if(dat(ii).ord .GT. 0) then
	    do jj=1,dat(ii).ord
	      dat(ii).coef(jj)=dat(ii).coef(jj)*rcl.gain
	    end do
	    dat(ii).coef(1)=dat(ii).coef(1)+rcl.offs
	  endif
	end do

	break=.FALSE.
	return

	end

	subroutine gs3_recal(rcl,dat)

#include "gsort.inc"
	record/recal/rcl
	record/calcoef/dat(0:1)

	ji=rcl.ind
	jp=rcl.par
	if(ji.EQ.0) then
	  ican=det(0).ival(jp)
	  if(ican.GT.0) then
	    xcan=rcl.offs+det(0).xval(jp)*rcl.gain
	    ican=xcan
	  endif
	  if(ican.LT.rcl.wlo .or. ican.GT.rcl.whi) goto 9000
	  det(0).ival(jp)=ican
	  det(0).xval(jp)=xcan
	else
	  nelim=0
	  jwlo=rcl.wlo
	  jwhi=rcl.whi
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	    id=det(jx).id
	    ncoef=dat(id).ord
	    if(ncoef.le.0) then
	      det(jx).id=-1			! mancano i coefficienti ==> elimina l'ADC
	      nelim=nelim+1
	    else
	      ICAN=det(jx).ival(jp)
	      XCAN=det(jx).xval(jp)
	      if(ican.GT.0) then
	        ENX=dat(id).coef(ncoef)
	        DO II=ncoef-1,1,-1
	          ENX=ENX*XCAN+dat(id).coef(ii)
	        end do
		if(rcl.zero) then
		  if( enx .lt. 0.0E0 ) enx = 0.0E0
		endif
	        xcan=enx			! offset,gain inclusi nei coefficienti
	        ICAN=xcan
	      endif
	      if(ican.GE.jwlo .and. ican.le.jwhi) then
	        det(jx).ival(jp)=ican
	        det(jx).xval(jp)=xcan
	      else
	        det(jx).id=-1			! elimina l'ADC
	        nelim=nelim+1
	      endif
	    endif
	  end do
	  if(nelim.ne.0 .and. rcl.same) goto 9000
	  nx=ndet(ji)-nelim
	  if(nx.LT.rcl.fl .or. nx.GT.rcl.fh) goto 9000
	  if(nelim.GT.0) call gs_checkdetector(ji)
	endif

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_recal

	return

	end



************************************** N.M *** recal with different polynomial on different regions

	subroutine gs0_recal_mult(rcl)

#include "gsort.inc"
	record/recal/rcl
	logical*1 getmem

	synt(1)='RECAL_MULT  Pn  file.mcal [NORUN|RUN]  offset gain  Wl Wh  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(rcl.ind,rcl.par,ires)
	if(rcl.ind .eq. 0)call gs_syntax('Cannot apply to header parameters')
	rcl.wlo=0
	rcl.whi=ires-1
	rcl.offs=0.
	rcl.gain=1.
	call istr_ch(comline,rcl.file)
	if(lengthc(rcl.file).LT.1) call gs_syntax('Filename expected')
	call filetype(rcl.file,'mcal')

	call gs_getrunmode(rcl.run)

	ii=istr_r2(comline,rcl.offs,rcl.gain)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	ii=istr_i2(comline,rcl.wlo,rcl.whi)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	call gs_getfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	ibsize=MAXDET*(2*MAXRCL_REGS + 2 + MAXRCL_ORD*MAXRCL_REGS)
	ibsize=4*ibsize
	if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for RECAL'
	com(icmd).size=ibsize

	return

	end

	subroutine gs1_recal_mult(rcl)

#include "gsort.inc"
	record/recal/rcl

	call gs_putindpar(rcl.ind,rcl.par)

	lcalfile=max(1,lengthc(rcl.file))
	write(list(llist+2:),'(A)') rcl.file(1:lcalfile)

	llist=lengthc(list)+3
	call gs_putrunmode(rcl.run)

	write(list(llist+1:),'(F12.2,F12.4,2I6)') rcl.offs,rcl.gain,rcl.wlo,rcl.whi

	llist=lengthc(list)+2
	call gs_putfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	return

	end

	subroutine gs2_recal_mult(rcl,dat)

#include "gsort.inc"
#if defined( __GFORTRAN__ )
	character*64 dyn_format
	logical*1 inp_yes
	external inp_yes
#endif
	record/recal/rcl
	record/mcalcoef/dat(0:1)

	REAL	  TEMP   (0:MAXDET-1)
	real mrcl_limit(0:maxrcl_regs)
	integer mrcl_npar(maxrcl_regs)
	LOGICAL	  MISSING(0:MAXDET-1)

	if(init_mode.EQ.0) then
	  if(rcl.run) return
	else
	  if(.NOT.rcl.run) return
	endif

	j_ind=rcl.ind
	j_par=rcl.par

	ntoget=nitems(j_ind)
	ntoget_t=ntoget

	if(j_ind.EQ.0) then
	  missing(j_par)=.TRUE.
	else
	  DO II=0,ntoget_t-1
	    MISSING(II)=.TRUE.
	  end do
	endif

2	lrcl_file=max(1,lengthc(rcl.file))
	OPEN(UNIT=gs2lun,FILE=rcl.file,STATUS='OLD',READONLY,err=5)
	DO ILU=LU1,LU2
	 WRITE(ILU,*)
	 WRITE(ILU,'(A,I1)')' Recalibration coefficients for '//dname(j_ind),j_par
	 WRITE(ILU,*)'Taken from File '//rcl.file(1:lrcl_file)
	end do
	goto 10

5	write(6,*) 'Error reading ',rcl.file(1:lrcl_file)
	if(inp_yes('Retry')) goto 2
	IOK=-1
	RETURN	
	
*10	READ(gs2lun,*,ERR=20,END=20) ICTAP,ICADC,JPAR,(TEMP(J),J=1,JPAR)
10	READ(gs2lun,*,ERR=20,END=20) ICTAP,ICADC,nregs,(mrcl_npar(ireg),(temp(j), 
     &				     j=(ireg-1)*maxRCL_ORD+1,(ireg-1)*maxRCL_ORD+mrcl_npar(ireg)),mrcl_limit(ireg),
     &                               ireg=1,nregs)
	IF(ICTAP.EQ.iio.run .OR. .NOT. rcl.run) THEN
	  IF(MISSING(ICADC)) THEN
	    if(j_ind.ne.0) then
	     dat(ICADC).limit(0)=0.000000
	     dat(ICADC).nregs=nregs
	     do ireg=1,nregs
#if defined( __GFORTRAN__ )
	      write(dyn_format,'(a,i0,a)')'2I5,',mrcl_npar(ireg),'G0,A,G0)'
	      WRITE(LU2,'(''ADC#'''//dyn_format(1:len(dyn_format)) )ICADC,ireg,(TEMP(J),j=(ireg-1)*maxRCL_ORD+1,
     &                       (ireg-1)*maxRCL_ORD+mrcl_npar(ireg)),' Upper limit: ',mrcl_limit(ireg)
	      if(mrcl_npar(ireg).GT.maxRCL_ORD) then
	      WRITE(LU1,'(''ADC#'''//dyn_format(1:len(dyn_format)) )ICADC,ireg,(TEMP(J),j=(ireg-1)*maxRCL_ORD+1,
     &                       (ireg-1)*maxRCL_ORD+mrcl_npar(ireg)),' Upper limit: ',mrcl_limit(ireg)
#else
	      WRITE(LU2,4201)ICADC,ireg,(TEMP(J),j=(ireg-1)*maxRCL_ORD+1,
     &                       (ireg-1)*maxRCL_ORD+mrcl_npar(ireg)),' Upper limit: ',mrcl_limit(ireg)
	      if(mrcl_npar(ireg).GT.maxRCL_ORD) then
	      WRITE(LU1,4201)ICADC,ireg,(TEMP(J),j=(ireg-1)*maxRCL_ORD+1,
     &                       (ireg-1)*maxRCL_ORD+mrcl_npar(ireg)),' Upper limit: ',mrcl_limit(ireg)
#endif
	        stop 'Too high order of recalibration'
	      endif
	      dat(ICADC).ord(ireg)=mrcl_npar(ireg)
	      DO J=1,mrcl_npar(ireg)
	        dat(ICADC).coef(J,ireg)=TEMP(j+(ireg-1)*maxRCL_ORD)
	      end do
	      dat(ICADC).limit(ireg)=mrcl_limit(ireg)
	     enddo
	    else
	      stop 'Cannot apply to header parameters'
	    endif
	    MISSING(ICADC)=.FALSE.
	    ntoget=ntoget-1
	    IF(ntoget.EQ.0) GOTO 20
	  ENDIF
	ENDIF
	GOTO 10

#if !defined( __GFORTRAN__ )
4201	format(' ADC#',2I5,<mrcl_npar(ireg)>G,A,G)
#endif

20	CLOSE(gs2lun)


	DO ii=0,ntoget_t-1
	   IF(MISSING(II)) THEN
	     DO ILU=LU1,LU2
		dat(ii).nregs=0
		WRITE(ILU,'('' ADC#'',I5,3X,A)') ii,' is missing ===> set to zero'
	     end do
	   ENDIF
	end do

	do ii=0,ntoget_t-1
	 do ireg=1,dat(ii).nregs
	  if(dat(ii).ord(ireg) .GT. 0) then
	    do jj=1,dat(ii).ord(ireg)
	      dat(ii).coef(jj,ireg)=dat(ii).coef(jj,ireg)*rcl.gain
	    end do
	    dat(ii).coef(1,ireg)=dat(ii).coef(1,ireg)+rcl.offs
	  endif
	 enddo
	end do

	break=.FALSE.
	return

	end

	subroutine gs3_recal_mult(rcl,dat)

#include "gsort.inc"
	record/recal/rcl
	record/mcalcoef/dat(0:1)

	ji=rcl.ind
	jp=rcl.par


	  nelim=0
	  jwlo=rcl.wlo
	  jwhi=rcl.whi
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	    id=det(jx).id
	    nregs=dat(id).nregs
	    if(nregs.le.0) then
	      det(jx).id=-1			! mancano i coefficienti ==> elimina l'ADC
	      nelim=nelim+1
	    else
	     ireg=0
	     ICAN=det(jx).ival(jp)
	     XCAN=det(jx).xval(jp)
	     ireg=0
	     do while ( (xcan .gt. dat(id).limit(ireg)) .and. (ireg .le. dat(id).nregs ) )
	       ireg=ireg+1
	     enddo
	     if (ireg .gt. dat(id).nregs) then
	       det(jx).id=-1			! fuori di tutte le regioni ==> elimina l'ADC
	       nelim=nelim+1
	       goto 101
	     endif
	     ncoef=dat(id).ord(ireg)
	     if(ncoef .le. 0)then
	       det(jx).id=-1			! mancano i coefficienti ==> elimina l'ADC
	       nelim=nelim+1
	       goto 101
	     endif
	      if(ican.GT.0) then
	        ENX=dat(id).coef(ncoef,ireg)
	        DO II=ncoef-1,1,-1
	          ENX=ENX*XCAN+dat(id).coef(ii,ireg)
	        end do
	        xcan=enx			! offset,gain inclusi nei coefficienti
	        ICAN=xcan
	      endif
	      if(ican.GE.jwlo .and. ican.le.jwhi) then
	        det(jx).ival(jp)=ican
	        det(jx).xval(jp)=xcan
	      else
	        det(jx).id=-1			! elimina l'ADC
	        nelim=nelim+1
	      endif
	    endif
101	    continue
	  end do
	  if(nelim.ne.0 .and. rcl.same) goto 9000
	  nx=ndet(ji)-nelim
	  if(nx.LT.rcl.fl .or. nx.GT.rcl.fh) goto 9000
	  if(nelim.GT.0) call gs_checkdetector(ji)

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_recal_mult

	return

	end



************************************* N.M. *** end of multi-recal
**** N.M.  ****  RECAL_AND_CHOOSE implementation ****************



	subroutine gs0_recal_choose(rcl)

#include "gsort.inc"
	record/recal_choose/rcl
	logical*1 getmem

	synt(1)='RECAL_CHOOSE  Pn file.cal(Pn)  Pm file.cal(Pm)  VAL [NORUN|RUN]  offset gain  Wl Wh  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	do ii = 1,2
	  call gs_getindpar(rcl.ind(ii),rcl.par(ii),ires)
	  if( ii .eq. 1)then
	    rcl.wlo=0
	    rcl.whi=ires-1
	    rcl.offs=0.
	    rcl.gain=1.
	    call istr_ch(comline,rcl.file1)
	    if(lengthc(rcl.file1).LT.1) call gs_syntax('Filename expected')
	    call filetype(rcl.file1,'cal')
	  else
	    call istr_ch(comline,rcl.file2)
	    if(lengthc(rcl.file2).LT.1) call gs_syntax('Filename expected')
	    call filetype(rcl.file2,'cal')
	  endif
	enddo
	if( rcl.ind(1) .ne. rcl.ind(2) )call gs_syntax('Must be in the same detector')
	if( rcl.par(1) .eq. rcl.par(2) )call gs_syntax('Not allowed for the same parameter')
	if( rcl.ind(1) .eq. 0 )call gs_syntax('Not allowed for header parameters')

	ii=istr_r1(comline,rcl.val)
	if(ii.ne.1) call gs_syntax('Numeric VALUE expected here')

	call gs_getrunmode(rcl.run)

	ii=istr_r2(comline,rcl.offs,rcl.gain)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	ii=istr_i2(comline,rcl.wlo,rcl.whi)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	call gs_getfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	ibsize= (1 + MAXRCL_ORD)*2*MAXDET
	ibsize=8*ibsize
	if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for RECAL'
	com(icmd).size=ibsize

	return

	end

	subroutine gs1_recal_choose(rcl)

#include "gsort.inc"
	record/recal_choose/rcl

	do ii = 1,2
	  call gs_putindpar(rcl.ind(ii),rcl.par(ii))
	  if( ii .eq. 1)then
	   lcalfile=max(1,lengthc(rcl.file1))
	   write(list(llist+2:),'(A)') rcl.file1(1:lcalfile)
	  else
	   lcalfile=max(1,lengthc(rcl.file2))
	   write(list(llist+2:),'(A)') rcl.file2(1:lcalfile)
	  endif
	  llist=lengthc(list)+3
	enddo
	
	write(list(llist+1:),'(F12.1)')rcl.val
	llist=lengthc(list)+2	
	call gs_putrunmode(rcl.run)

	write(list(llist+1:),'(F12.2,F12.4,2I6)') rcl.offs,rcl.gain,rcl.wlo,rcl.whi

	llist=lengthc(list)+2
	call gs_putfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	return

	end

	subroutine gs2_recal_choose(rcl,dat)

#include "gsort.inc"
#if defined( __GFORTRAN__ )
	character*64 dyn_format
	logical*1 inp_yes
	external inp_yes
#endif
	record/recal_choose/rcl
	record/calcoef/dat(0:MAXDET-1,2)

	REAL	  TEMP   (0:MAXDET-1)
	LOGICAL	  MISSING(0:MAXDET-1)

	if(init_mode.EQ.0) then
	  if(rcl.run) return
	else
	  if(.NOT.rcl.run) return
	endif

        do kk = 1,2

	j_ind=rcl.ind(kk)
	j_par=rcl.par(kk)

	ntoget=nitems(j_ind)
	ntoget_t=ntoget

	if(j_ind.EQ.0) then
	  missing(j_par)=.TRUE.
	else
	  DO II=0,ntoget_t-1
	    MISSING(II)=.TRUE.
	  end do
	endif

	if( kk .eq. 1) then
2	 lrcl_file=max(1,lengthc(rcl.file1))
	 OPEN(UNIT=gs2lun,FILE=rcl.file1,STATUS='OLD',READONLY,err=5)
	 DO ILU=LU1,LU2
	  WRITE(ILU,*)
	  WRITE(ILU,'(A,I1)')' Recalibration coefficients for '//dname(j_ind),j_par
	  WRITE(ILU,*)'Taken from File '//rcl.file1(1:lrcl_file)
	 end do
	 goto 10

5	 write(6,*) 'Error reading ',rcl.file1(1:lrcl_file)
	 if(inp_yes('Retry')) goto 2
	 IOK=-1
	 RETURN	
	else
201	 lrcl_file=max(1,lengthc(rcl.file2))
	 OPEN(UNIT=gs2lun,FILE=rcl.file2,STATUS='OLD',READONLY,err=501)
	 DO ILU=LU1,LU2
	  WRITE(ILU,*)
	  WRITE(ILU,'(A,I1)')' Recalibration coefficients for '//dname(j_ind),j_par
	  WRITE(ILU,*)'Taken from File '//rcl.file2(1:lrcl_file)
	 end do
	 goto 10

501	 write(6,*) 'Error reading ',rcl.file2(1:lrcl_file)
	 if(inp_yes('Retry')) goto 201
	 IOK=-1
	 RETURN	
	endif

10	READ(gs2lun,*,ERR=20,END=20) ICTAP,ICADC,JPAR,(TEMP(J),J=1,JPAR)
#if defined( __GFORTRAN__ )
	write(dyn_format,'(i4)')jpar
	dyn_format = adjustl(dyn_format)
#endif
	IF(ICTAP.EQ.iio.run .OR. .NOT. rcl.run) THEN
	  IF(MISSING(ICADC)) THEN
	    if(j_ind.ne.0) then
#if defined( __GFORTRAN__ )
	      WRITE(LU2,'(''ADC#'',I5,'//dyn_format(1:len(dyn_format))//'G0)')ICADC,(TEMP(J),J=1,JPAR)
	      if(jpar.GT.maxRCL_ORD) then
	        WRITE(LU1,'(''ADC#'',I5,'//dyn_format(1:len(dyn_format))//'G0)')ICADC,(TEMP(J),J=1,JPAR)
#else
	      WRITE(LU2,4201)ICADC,(TEMP(J),J=1,JPAR)
	      if(jpar.GT.maxRCL_ORD) then
	        WRITE(LU1,4201)ICADC,(TEMP(J),J=1,JPAR)
#endif
	        stop 'Too high order of recalibration'
	      endif
	      dat(ICADC,kk).ord=JPAR
	      DO J=1,JPAR
	        dat(ICADC,kk).coef(J)=TEMP(j)
	      end do
	    else
#if defined( __GFORTRAN__ )
	      WRITE(LU2,'(''FPAR#'',I4,'//dyn_format(1:len(dyn_format))//'G0)')ICADC,(TEMP(J),J=1,JPAR)
	      if(jpar.GT.2) then
	        WRITE(LU1,'(''FPAR#'',I4,'//dyn_format(1:len(dyn_format))//'G0)')ICADC,(TEMP(J),J=1,JPAR)
#else
	      WRITE(LU2,4202)ICADC,(TEMP(J),J=1,JPAR)
	      if(jpar.GT.2) then
	        WRITE(LU1,4202)ICADC,(TEMP(J),J=1,JPAR)
#endif
	        stop 'Too high order of recalibration'
	      endif
	      rcl.offs=temp(1)
	      rcl.gain=temp(2)
	    endif
	    MISSING(ICADC)=.FALSE.
	    ntoget=ntoget-1
	    IF(ntoget.EQ.0) GOTO 20
	  ENDIF
	ENDIF
	GOTO 10

#if !defined( __GFORTRAN__ )
4201	format(' ADC#',I5,<JPAR>G)
4202	format(' FPAR#',I4,<JPAR>G)
#endif

20	CLOSE(gs2lun)

	if(j_ind.EQ.0) then
	  if(missing(j_par)) then
	    DO ILU=LU1,LU2
		rcl.offs=0
		rcl.gain=0
		WRITE(ILU,'('' FPAR#'',I4,3X,A)') II,' is missing ===> set to zero'
	    end do
	  endif
	  break=.FALSE.
	  return
	endif

	DO ii=0,ntoget_t-1
	   IF(MISSING(II)) THEN
	     DO ILU=LU1,LU2
		dat(ii,kk).ord=0
		WRITE(ILU,'('' ADC#'',I5,3X,A)') ii,' is missing ===> set to zero'
	     end do
	   ENDIF
	end do

	do ii=0,ntoget_t-1
	  if(dat(ii,kk).ord .GT. 0) then
	    do jj=1,dat(ii,kk).ord
	      dat(ii,kk).coef(jj)=dat(ii,kk).coef(jj)*rcl.gain
	    end do
	    dat(ii,kk).coef(1)=dat(ii,kk).coef(1)+rcl.offs
	  endif
	end do

	enddo
	
	break=.FALSE.
	return

	end

	subroutine gs3_recal_choose(rcl,dat)

#include "gsort.inc"
	record/recal_choose/rcl
	record/calcoef/dat(0:MAXDET-1,2)

	if( ndet(ji) .lt. 1 )return
	ji=rcl.ind(1)
	jp1=rcl.par(1)
	jp2=rcl.par(2)

	  nelim=0
	  jwlo=rcl.wlo
	  jwhi=rcl.whi
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	    id=det(jx).id
	    ncoef=dat(id,1).ord + dat(id,2).ord
	    if(ncoef.le.0) then
	      det(jx).id=-1			! mancano i coefficienti ==> elimina l'ADC
	      nelim=nelim+1
	    else
	      ncoef1 = dat(id,1).ord
	      ncoef2 = dat(id,2).ord
	      ICAN1=det(jx).ival(jp1)
	      XCAN1=det(jx).xval(jp1)
	      ICAN2=det(jx).ival(jp2)
	      XCAN2=det(jx).xval(jp2)

	      if(ican1.GT.0 .and. ncoef1.GT.0) then
	        ENX=dat(id,1).coef(ncoef1)
	        DO II=ncoef1-1,1,-1
	          ENX=ENX*XCAN1+dat(id,1).coef(ii)
	        end do
	        xcan1=enx			! offset,gain inclusi nei coefficienti
	        ICAN1=xcan1
	      else
	        xcan1 = 0.0000000
		ican1 = 0
	      endif

	      if(ican2.GT.0 .and. ncoef2.GT.0) then
	        ENX=dat(id,2).coef(ncoef2)
	        DO II=ncoef2-1,1,-1
	          ENX=ENX*XCAN2+dat(id,2).coef(ii)
	        end do
	        xcan2=enx			! offset,gain inclusi nei coefficienti
	        if( xcan2 .le. 0.0000 ) xcan2 = 0.0000000
		ICAN2=xcan2
	      else
	        xcan2 = 0.0000000
		ican2 = 0
	      endif
	      
	      if( abs( xcan1-rcl.val) .le. abs( xcan2-rcl.val) )then
	        xcan = xcan1
		ican = ican1
	      else
	        xcan = xcan2
		ican = ican2
	      endif

	      if(ican.GE.jwlo .and. ican.le.jwhi) then
	        det(jx).ival(jp1)=ican
	        det(jx).xval(jp1)=xcan
		det(jx).ival(jp2)=ican2
	        det(jx).xval(jp2)=xcan2
	      else
	        det(jx).id=-1			! elimina l'ADC
	        nelim=nelim+1
	      endif
	    endif
	  end do
	  if(nelim.ne.0 .and. rcl.same) goto 9000
	  nx=ndet(ji)-nelim
	  if(nx.LT.rcl.fl .or. nx.GT.rcl.fh) goto 9000
	  if(nelim.GT.0) call gs_checkdetector(ji)

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_recal_choose

	return

	end


************************************ N.M. *** end of RECAL_AND_CHOOSE





	subroutine gs0_recal_lut(rcl)

#include "gsort.inc"
	record/recal/rcl
	logical*1 getmem

	synt(1)='RECAL_LUT  Pn  file.lut [NORUN|RUN]  Wl Wh  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(rcl.ind,rcl.par,ires)

	rcl.offs=0.
	rcl.gain=1.
	rcl.run=.FALSE.

	call istr_ch(comline,rcl.file)
        call filetype(rcl.file,'lut')
	if(lengthc(rcl.file).LT.1) call gs_syntax('Filename expected')

	call gs_getrunmode(rcl.run)

	ii=istr_i2(comline,rcl.wlo,rcl.whi)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	call gs_getfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	ibsize=IRES
	ibsize=4*ibsize
	if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for RECAL_LUT'
	com(icmd).size=ibsize

	return

	end

	subroutine gs1_recal_lut(rcl)

#include "gsort.inc"
	record/recal/rcl

	call gs_putindpar(rcl.ind,rcl.par)

	lcalfile=max(1,lengthc(rcl.file))
	write(list(llist+2:),'(A)') rcl.file(1:lcalfile)

	llist=lengthc(list)+3
	call gs_putrunmode(rcl.run)

	write(list(llist+1:),'(2I8)') rcl.wlo,rcl.whi

	llist=lengthc(list)+1
	call gs_putfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	return

	end

	subroutine gs2_recal_lut(rcl,dat)

#include "gsort.inc"
#if defined( __GFORTRAN__ )
	logical*1 inp_yes
	external inp_yes
#endif
	record/recal/rcl
	integer*4 dat(0:1)

	if(init_mode.EQ.0) then
	  if(rcl.run) return
	else
	  if(.NOT.rcl.run) return
	endif

	j_ind=rcl.ind
	j_par=rcl.par

	j_res=parres(j_par,0)
	DO ii=0,j_res-1
	   dat(ii)=-1
	end do

2	lrcl_file=max(1,lengthc(rcl.file))
	call filetype(rcl.file,'lut')
	OPEN(UNIT=gs2lun,FILE=rcl.file,STATUS='OLD',READONLY,err=30)
	DO ILU=LU1,LU2
	 WRITE(ILU,*)
	 WRITE(ILU,'(A,I1)')' Recalibration lookup table for '//dname(j_ind),j_par
	 WRITE(ILU,*)'Taken from File '//rcl.file(1:lrcl_file)
	end do

	ival=0
	ipos=0

	if(rcl.run) then
	  irun=iio.run
	else
	  irun=-1
	endif

	call gs_findrunadc(gs2lun,irun,-1,iok)
	if(iok.le.0) goto 20

10	READ(gs2lun,'(a)',END=20) line
11	read(line,*,ERR=20) ican
	do ij=ipos,ican
	  dat(ij)=ival
	end do
	write(lu2,'(''  ['',2i6,'' ] ===> '',i6)') ipos,ican,ival
	ival=ival+1
	ipos=ican+1
	GOTO 10

20	CLOSE(gs2lun)
	break=.FALSE.
	RETURN

30	write(6,*) 'Error reading ',rcl.file(1:lrcl_file)
	if(inp_yes('Retry')) goto 2
	call exit(0)

	END

	subroutine gs3_recal_lut(rcl,dat)

#include "gsort.inc"
	record/recal/rcl
	integer*4 dat(0:1)

	ji=rcl.ind
	jp=rcl.par
	if(ji.EQ.0) then
	  ican=det(0).ival(jp)
	  if(ican.GE.0 .and. ican.LT.parres(jp,0)) then
	    ican=dat(ican)
	  else
	    ican=0
	  endif
	  if(ican.LT.rcl.wlo .or. ican.GT.rcl.whi) goto 9000
	  det(0).ival(jp)=ican
	  det(0).xval(jp)=ican+rand_real2()
	else
	  jwlo=rcl.wlo
	  jwhi=rcl.whi
	  nelim=0
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	    ican=det(jx).ival(jp)
	    if(ican.GE.0 .and. ican.LT.parres(jp,ji)) then
	    ican=dat(ican)
	    else
	      ican=0
	    endif
	    if(ican.GE.jwlo .and. ican.le.jwhi) then
	      det(jx).ival(jp)=ican
	      det(jx).xval(jp)=ican+rand_real2()
	    else
	      det(jx).id=-1
	      nelim=nelim+1
	    endif
	  end do
	  if(nelim.ne.0 .and. rcl.same) goto 9000
	  nx=ndet(ji)-nelim
	  if(nx.LT.rcl.fl .or. nx.GT.rcl.fh) goto 9000
	  if(nelim.GT.0) call gs_checkdetector(ji)
	endif

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_recal_lut

	return

	end



	subroutine gs0_recal_doppler(doppl)

#include "gsort.inc"
	record/doppler/doppl

	logical*1 getmem

	character*72 angfil

	synt(1)='RECAL_DOPPLER Pn v0_% [ch0 ch1 v1_%] [theta_rec phi_rec] (GASP|EBIII|EBIV|CLARA|Angle_file) 
     1offset gain Wl Wh FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(doppl.ind,doppl.par,doppl.res)
	IF(doppl.ind .le.0) call gs_syntax('Not suited for header parameters')

	ii=istr_r1(comline,doppl.vc0)
	if(ii.ne.1) call gs_syntax('Wrong number of parameters')
	doppl.vc0=doppl.vc0/100
	if(doppl.vc0 .GE. 1.) call gs_syntax('Recoil velocity too big')
	iin=lengthn(comline)
	if(iin.GT.0) then
	  ii=istr_r3(comline,doppl.e0,doppl.e1,doppl.vc1)
	  if( ii .eq. -2 )then
              doppl.rtheta = doppl.e0
              doppl.rphi   = doppl.e1
              doppl.RefChanged = .TRUE.
              doppl.const = .TRUE.
              goto 10
         endif
	  if(ii.ne.3) call gs_syntax('Wrong number of parameters')
	  doppl.vc1=doppl.vc1/100
	  if(doppl.vc1 .GE. 1.) call gs_syntax('Recoil velocity too big')
	  if(doppl.e1 .LE. doppl.e0) call gs_syntax('If given, ch1 must be greater than ch0')
	  if(doppl.vc1 .LE. doppl.vc0) call gs_syntax('If given, v1 must be greater than v0')
	  doppl.slope=(doppl.vc1-doppl.vc0)/(doppl.e1-doppl.e0)
	  doppl.const=.FALSE.
	else
	  doppl.const=.TRUE.
         doppl.fact1 = doppl.vc0
         doppl.fact2 = doppl.vc0
         doppl.fact1 = 1.0E0/sqrt(1.0E0-doppl.fact1*doppl.fact1)
         doppl.fact2 = -doppl.fact2*doppl.fact1
	endif

	iin=lengthn(comline)
	if( iin .gt. 0)then
	   ii=istr_r2(comline,doppl.rtheta,doppl.rphi)	
	   if(ii.ne.2) call gs_syntax('Wrong number of parameters')
          doppl.RefChanged = .TRUE.
	else
	   doppl.rtheta = 0.0000
	   doppl.rphi = 0.000
          doppl.RefChanged = .FALSE.
	endif

10	call istr_ch(comline,angfil)
	if(angfil.EQ.'GASP') then
	  doppl.file=gsdir(1:lgsdir)//'NUMBERING.GE'
	  doppl.ndet=40
	elseif(angfil.EQ.'CLARA') then
	  doppl.file=gsdir(1:lgsdir)//'CLARANUMBERING.GE'
	  doppl.ndet=100
	elseif(angfil.EQ.'EBIII') then
	  doppl.file=gsdir(1:lgsdir)//'EBNUMBERING.GE'
	  doppl.ndet=239
	elseif(angfil.EQ.'EBIV') then
	  doppl.file=gsdir(1:lgsdir)//'EBIVNUMBERING.GE'
	  doppl.ndet=239
	else
	  doppl.file=angfil	
	  doppl.ndet=MAXDET
	  if(lengthc(doppl.file).LT.1) call gs_syntax('Filename expected')
	endif

	ii=istr_r2(comline,doppl.offs,doppl.gain)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	ii=istr_i2(comline,doppl.wlo,doppl.whi)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	call gs_getfoldgate(doppl.ind,doppl.fl,doppl.fh,doppl.same)

	ibsize=4* (2*MAXDET)
	if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for RECAL_DOPPLER'
	com(icmd).size=ibsize

	return

	end

	subroutine gs1_recal_doppler(doppl)

#include "gsort.inc"
	record/doppler/doppl

	call gs_putindpar(doppl.ind,doppl.par)

	write(list(llist+1:),'(F9.4)') doppl.vc0*100
	if(.NOT.doppl.const) then
	  llist=lengthc(list)+1
	  write(list(llist:),'(2F9.1,F9.4)') doppl.e0,doppl.e1,doppl.vc1*100
	endif

       if( doppl.RefChanged )then
         llist = lengthc(list)+1
	  write(list(llist:),'(2F9.1)') doppl.rtheta,doppl.rphi
	endif

	llist=lengthc(list)+2
	lfile=max(1,lengthc(doppl.file))
	write(list(llist:),'(A)') doppl.file(1:lfile)

	llist=lengthc(list)+1
	write(list(llist:),'(F12.2,F12.4,2I6)') doppl.offs,doppl.gain,doppl.wlo,doppl.whi

	llist=lengthc(list)+2
	call gs_putfoldgate(doppl.ind,doppl.fl,doppl.fh,doppl.same)

	return

	end

	subroutine gs2_recal_doppler(doppl,dat)

#include "gsort.inc"
	record/doppler/doppl
	record/dopplerdat/dat

	real th(0:MAXDET-1)
	real ph(0:MAXDET-1)

       real u_x, u_y, u_z

	if(init_mode.ne.0) return

	call gs_get_angles(th,ph,doppl.ndet,doppl.file,LU2)
       if( doppl.RefChanged )then
          u_x = sind( doppl.rtheta )*cosd( doppl.rphi )
          u_y = sind( doppl.rtheta )*sind( doppl.rphi )
          u_z = cosd( doppl.rtheta )
          do ii = 0,doppl.ndet-1
            dat.costheta(ii) = u_x*sind(th(ii))*cosd(ph(ii))+u_y*sind(th(ii))*sind(ph(ii))+u_z*cosd(th(ii))
*            write(6,*)ii, cosd(th(ii)),' ---> ',dat.costheta(ii)
          enddo
       else
	  do ii=0,doppl.ndet-1
	    dat.costheta(ii)=cosd(th(ii))
	  end do
       endif

       if( doppl.const )then
         doppl.fact1 = doppl.vc0
         doppl.fact2 = doppl.vc0
         doppl.fact1 = 1.0E0/sqrt(1.0E0-doppl.fact1*doppl.fact1)
         doppl.fact2 = -doppl.fact2*doppl.fact1
       endif

	return

	end

	subroutine gs3_recal_doppler(doppl,dat)

#include "gsort.inc"
	record/doppler/doppl
	record/dopplerdat/dat

	logical*1 vconst

	ji=doppl.ind
	jp=doppl.par
	vc0=doppl.vc0
	if(doppl.const) then
	  vconst=.TRUE.
	else
	  vconst=.FALSE.
	  e0=doppl.e0
	  e1=doppl.e1
	  vc1=doppl.vc1
	  slope=doppl.slope
	endif
	jwlo=doppl.wlo
	jwhi=doppl.whi
	offs=doppl.offs
	gain=doppl.gain
	nelim=0
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  kger=det(jx).id
	  ican=det(jx).ival(jp)
	  xcan=det(jx).xval(jp)
	  if(ican.GT.0) then
	    if(vconst) then
*	      beta=vc0
             Xcan = Xcan*(doppl.fact1+doppl.fact2*dat.costheta(kger))
	    else
	      if(xcan.le.e0) then
	        beta=vc0
	      elseif(xcan.GE.e1) then
	        beta=vc1
	      else
	        beta=vc0+slope*(xcan-e0)
	      endif
	    if(beta.GT.0) Xcan=Xcan/(1.+beta*dat.costheta(kger))	! NR
C	    if(beta.GT.0) Xcan=Xcan*(1.-beta*dat.costheta(kger))/sqrt(1.-beta*beta)
	    endif
	    xcan=XCAN*gain + offs
	    ican=XCAN
	    if(ican.GE.jwlo .and. ican.le.jwhi) then
	      det(jx).ival(jp)=ican
	      det(jx).xval(jp)=xcan
	    else
	      det(jx).id=-1
	      nelim=nelim+1
	    endif
	  endif
	end do
	if(nelim.ne.0 .and. doppl.same) goto 9000
	nx=ndet(ji)-nelim
	if(nx.LT.doppl.fl .or. nx.GT.doppl.fh) goto 9000
	if(nelim.GT.0) call gs_checkdetector(ji)

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_recal_doppler

	return

	end

***************************************** begin recal_polar

	subroutine gs0_recal_polar(dpolar)

#include "gsort.inc"
	record/polar/dpolar

	logical*1 getmem
	logical*1 gs_testindpar
	external gs_testindpar

	character*72 angfil
	
	integer ind1, ind2, res

	synt(1)='RECAL_POLAR Dx (Pv|v/c[%]) Pm Pn (GASP|EBIII|EBIV|CLARA|Angle_file|Dm Dn) offset gain Wl Wh FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(dpolar.ind,dpolar.par,dpolar.res)
	IF(dpolar.ind .le.0) call gs_syntax('Not suited for header parameters')

       if( .not. gs_testindpar(dpolar.ind_vel,dpolar.pvel,res) )then
	   ii=istr_r1(comline,dpolar.vc0)
	   if(ii.ne.1) call gs_syntax('v/c[%] expected here')
	   dpolar.vc0=dpolar.vc0/100
	   if(dpolar.vc0 .GE. 1.) call gs_syntax('Recoil velocity too big')
          dpolar.fixed = .TRUE.
       else
          call gs_getindpar(dpolar.ind_vel,dpolar.pvel,res)
	  dpolar.fixed = .FALSE.
       endif

	
	call gs_getindpar(ind1,dpolar.ptheta,res)
	call gs_getindpar(ind2,dpolar.pphi,res)
	if( ind1 .ne. ind2 )then
	   call gs_syntax('Theta/Phi parameters must be in the same detector')
	else
	   dpolar.ind_polar = ind1
	endif
	if( dpolar.ind_polar .eq. dpolar.ind )then
	    if( dpolar.par .eq. dpolar.ptheta  .or.  dpolar.par .eq. dpolar.pphi ) call gs_syntax('All parameters must differ')
	endif
	
        if( .not. gs_testindpar(ind1,dpolar.dtheta,res) )then	
	   call istr_ch(comline,angfil)
	   if(angfil.EQ.'GASP') then
	     dpolar.file=gsdir(1:lgsdir)//'NUMBERING.GE'
	     dpolar.ndet=40
	   elseif(angfil.EQ.'CLARA') then                           
	     dpolar.file=gsdir(1:lgsdir)//'CLARANUMBERING.GE.original'
	     dpolar.ndet=100
	   elseif(angfil.EQ.'EBIII') then
	     dpolar.file=gsdir(1:lgsdir)//'EBNUMBERING.GE'
	     dpolar.ndet=239
	   elseif(angfil.EQ.'EBIV') then
	     dpolar.file=gsdir(1:lgsdir)//'EBIVNUMBERING.GE'
	     dpolar.ndet=239
	   else
	     dpolar.file=angfil	
	     dpolar.ndet=MAXDET
	     if(lengthc(dpolar.file).LT.1) call gs_syntax('Filename expected')
	   endif
           dpolar.VarDetAngle = .FALSE.
          else
            call gs_getindpar(ind1, dpolar.dtheta, res)
            call gs_getindpar(ind2, dpolar.dphi, res)
            if( ind1 .ne. dpolar.ind ) call gs_syntax("Detector angles must be in the same dector type as the energy")
            if( ind2 .ne. dpolar.ind ) call gs_syntax("Detector angles must be in the same dector type as the energy")
            dpolar.VarDetAngle = .TRUE.
          endif

	ii=istr_r2(comline,dpolar.offs,dpolar.gain)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	ii=istr_i2(comline,dpolar.wlo,dpolar.whi)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	call gs_getfoldgate(dpolar.ind,dpolar.fl,dpolar.fh,dpolar.same)

	if( dpolar.VarDetAngle )then
            ibsize = 0
        else
            ibsize=4* (6*MAXDET)
            if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for RECAL_POLAR'
        endif
            
	com(icmd).size=ibsize

	return

	end

	subroutine gs1_recal_polar(dpolar)

#include "gsort.inc"
	record/polar/dpolar

	call gs_putindpar(dpolar.ind,dpolar.par)
	llist = llist+3

       if( dpolar.fixed )then
	   write(list(llist+1:),'(F9.4)') dpolar.vc0*100
       else
          call gs_putindpar(dpolar.ind_vel,dpolar.pvel)
	  llist = llist+3
       endif

	call gs_putindpar(dpolar.ind_polar,dpolar.ptheta)
	llist = llist+3
	call gs_putindpar(dpolar.ind_polar,dpolar.pphi)
	llist = llist+3

	if( dpolar.VarDetAngle ) then
             call gs_putindpar(dpolar.ind, dpolar.dtheta)
             llist = llist+3
             call gs_putindpar(dpolar.ind, dpolar.dphi)
             llist = llist+2
        else     
             llist=lengthc(list)+2
	     lfile=max(1,lengthc(dpolar.file))
	     write(list(llist:),'(A)') dpolar.file(1:lfile)
        endif

	llist=lengthc(list)+1
	write(list(llist:),'(F12.2,F12.4,2I6)') dpolar.offs,dpolar.gain,dpolar.wlo,dpolar.whi

	llist=lengthc(list)+2
	call gs_putfoldgate(dpolar.ind,dpolar.fl,dpolar.fh,dpolar.same)

	return

	end

	subroutine gs2_recal_polar(dpolar,dat)

#include "gsort.inc"
	record/polar/dpolar
	record/polardat/dat

	real th(0:MAXDET-1)
	real ph(0:MAXDET-1)

	if(init_mode.ne.0) return

        if( dpolar.fixed )then
            dpolar.fact1 = dpolar.vc0
            dpolar.fact2 = dpolar.vc0
            dpolar.fact1 = 1.0E0/sqrt(1.0E0-dpolar.fact1*dpolar.fact1)
            dpolar.fact2 = -dpolar.fact2*dpolar.fact1
        endif
        
        if( dpolar.VarDetAngle ) return
                    
	call gs_get_angles(th,ph,dpolar.ndet,dpolar.file,LU2)
	do ii=0,dpolar.ndet-1
*	       write(*,'(f12.4,'' ==> '',$)')th(ii)
*	       th(ii) = atand( sind(th(ii))/(cosd(th(ii))-0.04385000E0) )
*	       if( th(ii) .lt. 0.00E0 ) th(ii) = 180.00E0 + th(ii)
*	       write(*,'(f12.4)') th(ii)
	    dat.cdir(1,ii)=sind(th(ii))*cosd(ph(ii))
	    dat.cdir(2,ii)=sind(th(ii))*sind(ph(ii))
	    dat.cdir(3,ii)=cosd(th(ii))
	end do
*	stop		    
	return

	end

	subroutine gs3_recal_polar(dpolar,dat)

#include "gsort.inc"
	record/polar/dpolar
	record/polardat/dat

	logical*1 vconst
	real*8 u_x, u_y, u_z, theta, phi

	ji=dpolar.ind
	jp=dpolar.par
	
	jwlo=dpolar.wlo
	jwhi=dpolar.whi
	offs=dpolar.offs
	gain=dpolar.gain
	nelim=0

	  if( ndet( dpolar.ind_polar ) .ne. 1 )then
	     u_x = 0.0D0
	     u_y = 0.0D0
	     u_z = 1.0D0
	     write(6,*)' Warning - incorrect fold on polar angles detector'
	     call flush(6)
	  else
             theta = det(doff(dpolar.ind_polar)).xval(dpolar.ptheta)/10.0D0
	     phi   = det(doff(dpolar.ind_polar)).xval(dpolar.pphi)/10.0D0
	     u_x = sind( theta )*cosd( phi )
             u_y = sind( theta )*sind( phi )
             u_z = cosd( theta )
	  endif
	  
       if( .not. dpolar.fixed )then
	  if( ndet( dpolar.ind_vel ) .ne. 1 )then
	     write(6,*)' Warning - incorrect fold on Doppler velocity detector, no correction done'
	     call flush(6)
            return
	  else
             dpolar.vc0 = det(doff(dpolar.ind_vel)).xval(dpolar.pvel)/100.0D0
             if( (dpolar.vc0 .gt. 1.0D0) .or. (dpolar.vc0 .lt. 0.000D0) )then
	          write(6,*)' Warning - incorrect value for Doppler velocity, no correction done'
	          call flush(6)
                 return
             endif
             dpolar.fact1 = dpolar.vc0 !/sqrt(u_x*u_x+u_z*u_z)
             dpolar.fact2 = dpolar.fact1
             dpolar.fact1 = 1.0E0/sqrt(1.0E0-dpolar.fact1*dpolar.fact1)
             dpolar.fact2 = -dpolar.fact2*dpolar.fact1
	  endif
       endif

	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  kger=det(jx).id
	  ican=det(jx).ival(jp)
	  xcan=det(jx).xval(jp)
          
          if( dpolar.VarDetAngle )then
              theta = det(jx).xval(dpolar.dtheta)/10.0D0
              phi   = det(jx).xval(dpolar.dphi)/10.0D0
              theta = dsind(theta)*(u_x*dcosd(phi)+u_y*dsind(phi))+u_z*dcosd(theta)
          else
              theta = u_x*dat.cdir(1,kger)+u_y*dat.cdir(2,kger)+u_z*dat.cdir(3,kger)
          endif
*	  theta = (theta - dpolar.vc0)/(1.0D0 - theta*dpolar.vc0)

*	  theta = ( theta + dpolar.vc0 )/sqrt(1.0D0+2.0D0*theta*dpolar.vc0+ dpolar.vc0* dpolar.vc0)   ! gresit!!
	  
!! 	  theta = dacosd( theta )
! 	  theta = theta + asind(dpolar.vc0*sind(theta))
! 	  theta = cosd(theta)
          
*           write(*,*)
*           write(*,*)'Before: ',theta
!! 	   theta = datand( dsind(theta)/(dcosd(theta)-dpolar.vc0)/dpolar.fact1)
!!            if( theta .lt. 0.0D0 ) theta = 180.0D0+theta
*          write(*,*)'Final: ',theta
!!            theta = dcosd(theta)
! 
	  if(ican.GT.0) then
           Xcan = Xcan*(dpolar.fact1+dpolar.fact2*theta)

	    xcan=XCAN*gain + offs
	    ican=XCAN
	    if(ican.GE.jwlo .and. ican.le.jwhi) then
	      det(jx).ival(jp)=ican
	      det(jx).xval(jp)=xcan
	    else
	      det(jx).id=-1
	      nelim=nelim+1
	    endif
	  endif
	end do
	if(nelim.ne.0 .and. dpolar.same) goto 9000
	nx=ndet(ji)-nelim
	if(nx.LT.dpolar.fl .or. nx.GT.dpolar.fh) goto 9000
	if(nelim.GT.0) call gs_checkdetector(ji)

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_recal_polar

	return

	end


***************************************** end recal_polar


	subroutine gs0_time_adjust(tadj)

#include "gsort.inc"
	record/tadjust/tadj

	integer ind,par,res

	synt(1)='TIME_ADJUST Pn [Qm ...] FinalPosition [RejectionFactor] [Pn [Qm ...]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	tadj.howmany1=0
	ii=lengthn(comline)
	do while (ii.lt.0)
	  call gs_getindpar(ind,par,res)
	  if(ind.eq.0) call gs_syntax('Not valid for header parameters')
	  if(par.lt.0) call gs_syntax('Not valid for index')
	  tadj.howmany1 = tadj.howmany1 + 1
	  if(tadj.howmany1.GT.MAXTADJ) call gs_syntax1('Maximum number of parameters for this command is',MAXTADJ)
	  tadj.ind1(tadj.howmany1)=ind
	  tadj.par1(tadj.howmany1)=par
	  tadj.res1(tadj.howmany1)=res
	  ii=lengthn(comline)
	end do

	ii=istr_r1(comline,tadj.position)
	if(ii.ne.1) call gs_syntax('Error reading FinalPosition')

	ii=lengthn(comline)
	if(ii.gt.0) then
	  ii=istr_r1(comline,tadj.factor)
	  if(ii.ne.1) call gs_syntax('Error reading RejectionFactor')
	else
	  tadj.factor=1.
	endif

	ii=lengthn(comline)
	if(ii.eq.0) then
	  tadj.howmany2=tadj.howmany1
	  do ii=1,tadj.howmany2
	    tadj.ind2(ii)=tadj.ind1(ii)
	    tadj.par2(ii)=tadj.par1(ii)
	    tadj.res2(ii)=tadj.res1(ii)
	  enddo
	else
	  tadj.howmany2=0
	  ii=lengthn(comline)
	  do while (ii.lt.0)
	    call gs_getindpar(ind,par,res)
	    if(ind.eq.0) call gs_syntax('Not valid for header parameters')
	    if(par.lt.0) call gs_syntax('Not valid for index')
	    tadj.howmany2 = tadj.howmany2 + 1
	    if(tadj.howmany2.GT.MAXTADJ) call gs_syntax1('Maximum number of parameters for this command is',MAXTADJ)
	    tadj.ind2(tadj.howmany2)=ind
	    tadj.par2(tadj.howmany2)=par
	    tadj.res2(tadj.howmany2)=res
	    ii=lengthn(comline)
	  end do
	endif

	return

	end

	subroutine gs1_time_adjust(tadj)

#include "gsort.inc"
	record/tadjust/tadj

	do nn=1,tadj.howmany1
	  call gs_putindpar(tadj.ind1(nn),tadj.par1(nn))
	  llist=llist+3
	end do

	write(list(llist:),'(F12.2,F12.4)') tadj.position,tadj.factor

	llist=lengthc(list)+4
	do nn=1,tadj.howmany2
	  call gs_putindpar(tadj.ind2(nn),tadj.par2(nn))
	  llist=llist+3
	end do

	return

	end

	subroutine gs2_time_adjust

	return

	end

	subroutine gs3_time_adjust(tadj)

#include "gsort.inc"
	record/tadjust/tadj

	aver=0				! calcola la prima media
	nntt=0
	do ii=1,tadj.howmany1
	  ji=tadj.ind1(ii)
	  jp=tadj.par1(ii)
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	    if(det(jx).ival(jp) .GT. 0) then
	      aver=aver + det(jx).xval(jp)
	      nntt=nntt+1
	    endif
	  end do
	end do
	if(nntt.lt.1) return
	aver=aver/nntt

	if(nntt.GT.2) then
	  avar=0			! somma degli scarti assoluti che e ~ FWHM
	  do ii=1,tadj.howmany1
	    ji=tadj.ind1(ii)
	    jp=tadj.par1(ii)
	    do jx=doff(ji),doff(ji)+ndet(ji)-1
	      if(det(jx).ival(jp) .GT. 0) then
	        dd=abs(det(jx).xval(jp)-aver)
	        avar =avar + dd
	      endif
	    end do
	  end do
	  avar =avar/nntt

	  distm=avar*tadj.factor	! la massima deviazione per i "buoni" 
	  averb=0			! calcola la media del "buoni"
	  nnttb=0
	  do ii=1,tadj.howmany1
	    ji=tadj.ind1(ii)
	    jp=tadj.par1(ii)
	    do jx=doff(ji),doff(ji)+ndet(ji)-1
	      if(det(jx).ival(jp) .GT. 0) then
	        if(abs(det(jx).xval(jp)-aver).le.distm) then
	          averb=averb + det(jx).xval(jp)
	          nnttb=nnttb+1
	        endif
	      endif
	    end do
	  end do
	  if(nnttb.GT.1) then
	    aver=averb/nnttb
	  endif
	endif

	delta=tadj.position-aver		! aggiusta i tempi
	do ii=1,tadj.howmany2
	  ji=tadj.ind2(ii)
	  jp=tadj.par2(ii)
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	    if(det(jx).ival(jp) .GT. 0) then
	      det(jx).xval(jp)=det(jx).xval(jp)+delta
	      det(jx).ival(jp)=det(jx).xval(jp)
	    endif
	  end do
	end do

	return

	end

	subroutine gs4_time_adjust

	return

	end
************************************************* ce vrea Calin (e lege!!!!)

	subroutine gs0_mean_value(tadj)

#include "gsort.inc"
	record/meanvalstr/tadj

	integer ind,par,res

	synt(1)='MEAN_VALUE Pn [Qm ...] Fn'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	tadj.howmany1=0
	ii=lengthn(comline)
	do while (ii.lt.0)
	  call gs_getindpar(ind,par,res)
	  ii=lengthn(comline)
	  if(ind.eq.0 .and. ii.ne.0) call gs_syntax('Not valid for header parameters')
	  if(par.lt.0) call gs_syntax('Not valid for index')
	  tadj.howmany1 = tadj.howmany1 + 1
	  if(tadj.howmany1.GT.MAXTADJ) call gs_syntax1('Maximum number of parameters for this command is',MAXTADJ)
	  tadj.ind1(tadj.howmany1)=ind
	  tadj.par1(tadj.howmany1)=par
	  tadj.res1(tadj.howmany1)=res
	end do

	if(ii.eq.0) then
	  if(tadj.howmany1 .eq. 1)call gs_syntax('Output parameter required')
	  ind = tadj.ind1(tadj.howmany1)
	  par = tadj.par1(tadj.howmany1)
	  res = tadj.res1(tadj.howmany1)
	  if(ind.ne.0) call gs_syntax('Only fixed parameters allowed')
	  if(par.lt.0) call gs_syntax('Not valid for index')
	  tadj.howmany1 = tadj.howmany1 - 1
	  tadj.ind2=ind
	  tadj.par2=par
	  tadj.res2=res
	elseif( ii .gt. 0 ) then
	  call gs_syntax('Only detector parameters allowed')
	endif	  

	do while (ii.lt.0)
	  call gs_getindpar(ind,par,res)
	  if(ind.ne.0) call gs_syntax('Only fixed parameters allowed')
	  if(par.lt.0) call gs_syntax('Not valid for index')
	  tadj.ind2=ind
	  tadj.par2=par
	  tadj.res2=res
	  ii=lengthn(comline)
	end do

	return

	end

	subroutine gs1_mean_value(tadj)

#include "gsort.inc"
	record/meanvalstr/tadj

	do nn=1,tadj.howmany1
	  call gs_putindpar(tadj.ind1(nn),tadj.par1(nn))
	  llist=llist+3
	end do

	call gs_putindpar(tadj.ind2,tadj.par2)
	llist=llist+3

	return

	end

	subroutine gs2_mean_value
	
	return

	end

	subroutine gs3_mean_value(tadj)

#include "gsort.inc"
	record/meanvalstr/tadj

	aver=0				! calcola la prima media
	nntt=0
	do ii=1,tadj.howmany1
	  ji=tadj.ind1(ii)
	  jp=tadj.par1(ii)
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
*	    if(det(jx).ival(jp) .GE. 0) then
	      aver=aver + det(jx).xval(jp)
	      nntt=nntt+1
*	    endif
	  end do
	end do
	if(nntt.lt.1) return
	aver=aver/nntt

*	if(nntt.GT.2) then
*	  avar=0			! somma degli scarti assoluti che e ~ FWHM
*	  do ii=1,tadj.howmany1
*	    ji=tadj.ind1(ii)
*	    jp=tadj.par1(ii)
*	    do jx=doff(ji),doff(ji)+ndet(ji)-1
*	      if(det(jx).ival(jp) .GT. 0) then
*	        dd=abs(det(jx).xval(jp)-aver)
*	        avar =avar + dd
*	      endif
*	    end do
*	  end do
*	  avar =avar/nntt
*
*	  distm=avar*tadj.factor	! la massima deviazione per i "buoni" 
*	   averb=0			 ! calcola la media del "buoni"
*	   nnttb=0
*	   do ii=1,tadj.howmany1
*	     ji=tadj.ind1(ii)
*	     jp=tadj.par1(ii)
*	     do jx=doff(ji),doff(ji)+ndet(ji)-1
*	       if(det(jx).ival(jp) .GT. 0) then
*		 if(abs(det(jx).xval(jp)-aver).le.distm) then
*		   averb=averb + det(jx).xval(jp)
*		   nnttb=nnttb+1
*		 endif
*	       endif
*	     end do
*	   end do
*	   if(nnttb.GT.1) then
*	    aver=averb/nnttb
*	  endif
*	endif

	det(0).xval(tadj.par2)=aver
	det(0).ival(tadj.par2)=aver

	return

	end


	subroutine gs4_mean_value

	return

	end
************************************************* ce vrea Calin (e lege!!!!)

	subroutine gs0_wmean_value(tadj)

#include "gsort.inc"
	record/meanvalstr/tadj

	integer ind,par,res

	synt(1)='WMEAN_VALUE Pn Pw Fn'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	tadj.howmany1=0
	ii=lengthn(comline)
	do while (ii.lt.0)
	  call gs_getindpar(ind,par,res)
	  ii=lengthn(comline)
	  if(ind.eq.0 .and. ii.ne.0) call gs_syntax('Not valid for header parameters')
	  if(par.lt.0) call gs_syntax('Not valid for index')
	  tadj.howmany1 = tadj.howmany1 + 1
	  if(tadj.howmany1.GT. 3) call gs_syntax1('Required number of parameters for this command is',3)
	  tadj.ind1(tadj.howmany1)=ind
	  tadj.par1(tadj.howmany1)=par
	  tadj.res1(tadj.howmany1)=res
	end do

	if( tadj.ind1(1) .ne. tadj.ind1(2) )call gs_syntax('First two parameters must be in the same detector')
	ind = tadj.ind1(tadj.howmany1)
	par = tadj.par1(tadj.howmany1)
	res = tadj.res1(tadj.howmany1)
	if(ind.ne.0) call gs_syntax('Only fixed parameters allowed')
	if(par.lt.0) call gs_syntax('Not valid for index')
	tadj.howmany1 = tadj.howmany1 - 1
	tadj.ind2=ind
	tadj.par2=par
	tadj.res2=res


	return

	end

	subroutine gs1_wmean_value(tadj)

#include "gsort.inc"
	record/meanvalstr/tadj

	do nn=1,tadj.howmany1
	  call gs_putindpar(tadj.ind1(nn),tadj.par1(nn))
	  llist=llist+3
	end do

	call gs_putindpar(tadj.ind2,tadj.par2)
	llist=llist+3

	return

	end

	subroutine gs2_wmean_value
	
	return

	end


	subroutine gs3_wmean_value(tadj)

#include "gsort.inc"
	record/meanvalstr/tadj

	aver=0				! calcola la prima media
	averw = 0

	ji = tadj.ind1(1)
	jp1 = tadj.par1(1)
	jp2 = tadj.par1(2)
	
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  if(det(jx).ival(jp2) .GT. 0) then
	    averw = averw + det(jx).xval(jp1)*det(jx).xval(jp2)
	    aver=aver + det(jx).xval(jp2)
	    nntt=nntt+1
	  endif
	end do

	if( aver .gt. 0.0000001 )then
		aver = averw/aver
	else
		aver = 0.000000
	endif

	det(0).xval(tadj.par2)=aver
	det(0).ival(tadj.par2)=aver

	return

	end

	subroutine gs4_wmean_value

	return

	end

************************************************* Nicu

	subroutine gs0_time_reference(tref)

#include "gsort.inc"
	record/tref/tref

	integer ind,par,res

	synt(1)='TIME_REFERENCE Pn FinalPosition det# '

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	ii=lengthn(comline)
	call gs_getindpar(ind,par,res)
	if(ind.eq.0) call gs_syntax('Not valid for header parameters')
	if(par.lt.0) call gs_syntax('Not valid for index')
	tref.ind=ind
	tref.par=par
	tref.res=res
	ii=lengthn(comline)

	ii=istr_r1(comline,tref.position)
	if(ii.ne.1) call gs_syntax('Error reading FinalPosition')

	ii=lengthn(comline)
	if(ii.gt.0) then
	  ii=istr_i1(comline,tref.refno)
	  if(ii.ne.1) call gs_syntax('Error reading reference detector number')
	else
	  call gs_syntax('Error reading reference detector number')
	endif
	
	return
	end
	
	
	subroutine gs1_time_reference(tref)

#include "gsort.inc"
	record/tref/tref

	llist = llist+1
	call gs_putindpar(tref.ind,tref.par)
	llist=llist+3

	write(list(llist:),'(F12.2,i10)') tref.position,tref.refno

	return

	end

	subroutine gs2_time_reference(tref)

	return

	end

	subroutine gs3_time_reference(tref)
	
#include "gsort.inc"
	record/tref/tref
	
	ji=tref.ind
	jp=tref.par
	idref=tref.refno
	
	if( ndet(ji) .le. 1)then
	   killev=.true.
	   return
	endif
	
	xref=-1
	
	do ii=doff(ji),doff(ji)+ndet(ji)-1
	   if(det(ii).id .eq. idref)xref=det(ii).xval(jp)
	enddo
	
	if(xref .le. 0.0000000000)then
	   killev=.true.
	   return
	endif
	
	do ii=doff(ji),doff(ji)+ndet(ji)-1
	    if(det(ii).ival(jp) .gt. 0)then
	        det(ii).xval(jp)=tref.position+(det(ii).xval(jp)-xref)
	        det(ii).ival(jp)=det(ii).xval(jp)
	    endif
	enddo
	
	return
	end



	subroutine gs4_time_reference(tref)

	return

	end
	
*******

	subroutine gs0_timing(tref)

#include "gsort.inc"
	record/tref/tref

	integer ind,par,res

	synt(1)='TIMING Pn FinalPosition [MinFold]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	ii=lengthn(comline)
	call gs_getindpar(ind,par,res)
	if(ind.eq.0) call gs_syntax('Not valid for header parameters')
	if(par.lt.0) call gs_syntax('Not valid for index')
	tref.ind=ind
	tref.par=par
	tref.res=res
	ii=lengthn(comline)

	ii=istr_r1(comline,tref.position)
	if(ii.ne.1) call gs_syntax('Error reading FinalPosition')
	ii=lengthn(comline)
	
	tref.refno=2
	if(ii .gt. 0)then
	  ii=istr_i1(comline,tref.refno)
	  if(ii.ne.1) call gs_syntax('Error reading MinFold')
	  if(tref.refno .le. 0)call gs_syntax('Invalid value for MinFold')
	  if(tref.refno .gt. 2)tref.refno=2
	endif
	return
	end
	
	
	subroutine gs1_timing(tref)

#include "gsort.inc"
	record/tref/tref

	call gs_putindpar(tref.ind,tref.par)
	llist=llist+3

	write(list(llist:),'(F12.2,I3)') tref.position,tref.refno

	return

	end

	subroutine gs2_timing(tref)

#include "gsort.inc"
	record/tref/tref
	
	tref.N = 0
	tref.SQSUM = 0.000000000
	return

	end

	subroutine gs3_timing(tref)
	
#include "gsort.inc"
	record/tref/tref
	
	ji=tref.ind
	jp=tref.par
	MinFold=tref.refno
	
	if( ndet(ji) .lt. MinFold )then
	   killev=.true.
	   return
	endif
	
	tmin1=1000000.0
	tmin2=tmin1
	imin1=-1
	imin2=-1
	
	do ii=doff(ji),doff(ji)+ndet(ji)-1
	  if(det(ii).ival(jp) .gt. 0)then
	   if(det(ii).xval(jp) .le. tmin1)then
	      tmin1=det(ii).xval(jp)
	      imin1=ii
	   endif
	  endif
	enddo
	
	if( MinFold .eq. 1 )then
	   if( imin1 .lt. doff(ji) )then
	     killev=.true.
	     return
	   endif
	   xref=det(imin1).xval(jp)
	   do ii=doff(ji),doff(ji)+ndet(ji)-1
	     if(det(ii).ival(jp) .gt. 0)then
	        det(ii).xval(jp)=tref.position+det(ii).xval(jp)-xref
	        det(ii).ival(jp)=det(ii).xval(jp)
	     endif
	   enddo
	   return
	 endif
	
	
	do ii=doff(ji),doff(ji)+ndet(ji)-1
	  if(det(ii).ival(jp) .gt. 0)then
	   if(det(ii).xval(jp) .le. tmin2 .and. ii .ne. imin1 )then
	      tmin2=det(ii).xval(jp)
	      imin2=ii
	   endif
	  endif
	enddo
	
	do ii=doff(ji),doff(ji)+ndet(ji)-2
	    do jj=ii+1,doff(ji)+ndet(ji)-1
	       tref.SQSUM = tref.SQSUM + (det(ii).xval(jp)-det(jj).xval(jp))**2
	       tref.N = tref.N +1
	    enddo
	enddo
	
	rrr = tref.SQSUM/tref.N
	
	rrr = 0.800*sqrt(rrr)
	
	if(imin1 .lt. doff(ji) .or. imin2 .lt. doff(ji) )then
	   killev=.true.
	   return
	endif
	
	if(tref.N .gt. 1000)then
	   if((det(imin2).xval(jp)-det(imin1).xval(jp)) .gt. rrr )then
	      killev = .true.
	      return
	   endif
	endif
	
	
	xref=(det(imin1).xval(jp)+det(imin2).xval(jp))/2.0
	do ii=doff(ji),doff(ji)+ndet(ji)-1
	    if(det(ii).ival(jp) .gt. 0)then
	        det(ii).xval(jp)=tref.position+det(ii).xval(jp)-xref
	        det(ii).ival(jp)=det(ii).xval(jp)
	    endif
	enddo
	
	return
	end



	subroutine gs4_timing(tref)

	return

	end
	




		
**********************************************************
	subroutine gs0_SLtoCM(sltocm)

#include "gsort.inc"
	record/sltocm/sltocm

	character*72 angfil
	
*	logical*1 getmem

	synt(1)='ESL_TO_ECM Pn Pm (ISIS|Agles_file_Si) MeV_per_channel v_CM/c[%]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	do nn = 1,2 
	  call gs_getindpar(sltocm.ind(nn),sltocm.par(nn),ires)
	  if(sltocm.ind(nn).le.0) call gs_syntax('Meaningless for header parameters')
	enddo
	if( sltocm.ind(1) .ne. sltocm.ind(2) )call gs_syntax('Energy and Mass parameters must be in the same detector')
	
	call istr_ch(comline,angfil)
	if(angfil.EQ.'ISIS') then
	  sltocm.afile_si=gsdir(1:lgsdir)//'NUMBERING.SI'
	  sltocm.ndet_si=40
*	elseif(angfil.EQ.'EB') then
*	  kine.afile_si=gsdir(1:lgsdir)//'EBNUMBERING.SI'
*	  kine.ndet_si=40
	else
	  sltocm.afile_si=angfil	
	  sltocm.ndet_si=40
	  if(lengthc(sltocm.afile_si).LT.1) call gs_syntax('Filename expected')
	endif
	
	nn = istr_r2(comline,sltocm.f(1),sltocm.f(2))
	if( nn .lt. 2 )call gs_syntax('MeV_per_channel and v_CM/c values expected')
	if( nn .gt. 2 )call gs_syntax('Too many parameters')
	
	return

	end
	

	subroutine gs1_SLtoCM(sltocm)

#include "gsort.inc"
	record/sltocm/sltocm

	do nn = 1,2
	  call gs_putindpar(sltocm.ind(nn),sltocm.par(nn))
	  llist = llist+2
	enddo

	lfile=max(1,lengthc(sltocm.afile_si))
	write(list(llist+1:),'(A)') sltocm.afile_si(1:lfile)

	llist=lengthc(list)+1
	write(list(llist:),'(F12.5,F10.2)') sltocm.f(1),sltocm.f(2)
	
	return
	end


	subroutine gs2_SLtoCM(sltocm)

#include "gsort.inc"
	record/sltocm/sltocm

	real ph_si(0:MAXDET-1)

	REAL XMNMEV
	PARAMETER (XMNMEV=931.49432)

	DEG2RAD=atan(1.)/45.

	call gs_get_angles(sltocm.cos_th,ph_si,sltocm.ndet_si,sltocm.afile_si,LU2)
	do ii=0,sltocm.ndet_si-1
	  sltocm.cos_th(ii)=sltocm.cos_th(ii)*DEG2RAD
	  sltocm.cos_th(ii)=cos(sltocm.cos_th(ii))
	end do
	sltocm.f(2) = sltocm.f(2)/100.000
	sltocm.f(2) = sltocm.f(2)*sltocm.f(2)*XMNMEV
	
	return
	end
	
	
	subroutine gs3_SLtoCM(sltocm)

#include "gsort.inc"
	record/sltocm/sltocm
	
#define ji sltocm.ind(1)
#define jp sltocm.par(1)
#define jm sltocm.par(2)

	if( ndet(ji) .le. 0 )return

	do ii=doff(ji),doff(ji)+ndet(ji)-1
	   if( (det(ii).ival(jm) .gt. 0) .and. (det(ii).xval(jp) .gt. 0.000) )then
	     rr = det(ii).xval(jm)*sltocm.f(2)/2.00
	     en = det(ii).xval(jp)*sltocm.f(1)
	     en = en + rr - sqrt(4.00*rr*en)*sltocm.cos_th(det(ii).id)
	     det(ii).xval(jp) = en/sltocm.f(1)
	     det(ii).ival(jp) =det(ii).xval(jp)
	   endif
	enddo
	
#undef ji
#undef jp
#undef jm

	return
	end
	
	
	subroutine gs4_SLtoCM(sltocm)
	
	return
	end
	
**********************************************************
**********************************************************
	subroutine gs0_recal_kinenew(kine)

#include "gsort.inc"
	record/kinenew/kine

	character*72 angfil

	logical*1 getmem

	synt(1)='RECAL_KINE Pn Qm Qe (GASP|EBIII|EBIV|Angles_file_Ge) offset gain Wl Wh FOLD_GATE [ExceptList_file]'
	synt(2)='          (ISIS|EUCLIDES|Angles_file_Si) channels/MeV_Si   A_CN  E_CN[MeV] [Recoil_Energy_Atenuation_Factor]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(kine.ind(1),kine.par(1),kine.res(1))
	if(kine.ind(1).le.0) call gs_syntax('Meaningless for header parameters')
	
	do nn = 2,3
	 call gs_getindpar(kine.ind(nn),kine.par(nn),kine.res(nn))
	 if(kine.ind(1) .eq. kine.ind(nn))call gs_syntax('This parameter cannot be in the same detector with the first one')
	 if(kine.ind(nn).le.0) call gs_syntax('Meaningless for header parameters')
	enddo
	if(kine.ind(2) .ne. kine.ind(3))call gs_syntax('The second and third parameters must be in the same detector')
	
	call istr_ch(comline,angfil)
	if(angfil.EQ.'GASP') then
	  kine.afile_ge=gsdir(1:lgsdir)//'NUMBERING.GE'
	  kine.ndet_ge=40
	elseif(angfil.EQ.'EBIII') then
	  kine.afile_ge=gsdir(1:lgsdir)//'EBNUMBERING.GE'
	  kine.ndet_ge=239
	elseif(angfil.EQ.'EBIV') then
	  kine.afile_ge=gsdir(1:lgsdir)//'EBIVNUMBERING.GE'
	  kine.ndet_ge=239
	else
	  kine.afile_ge=angfil	
	  kine.ndet_ge=MAXDET
	  if(lengthc(kine.afile_ge).LT.1) call gs_syntax('Filename expected')
	endif
	
	
	ii=istr_r2(comline,kine.offs,kine.gain)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	ii=istr_i2(comline,kine.wlo,kine.whi)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	call gs_getfoldgate(kine.ind,kine.fl,kine.fh,kine.same)

	iin=lengthn(comline)
	if(iin.ne.0) then
	   call istr_ch(comline,angfil)
	   kine.except_file = angfil
	   if(lengthc(kine.except_file).LT.1)call gs_syntax('Filename expected')
	   kine.except = .TRUE.
	else
	   kine.except = .FALSE.
	endif
	

	iin=lengthn(comline)
	if(iin.ne.0) call gs_syntax('Too many parameters')

	call gs_readline

	call istr_ch(comline,angfil)
	if(angfil.EQ.'ISIS') then
	  kine.afile_si=gsdir(1:lgsdir)//'NUMBERING.SI'
	  kine.ndet_si=40
	elseif(angfil(1:3).EQ.'EUC') then
	  kine.afile_si=gsdir(1:lgsdir)//'NUMBERING.EUCLIDES'
	  kine.ndet_si=160
	else
	  kine.afile_si=angfil	
	  kine.ndet_si=160
	  if(lengthc(kine.afile_si).LT.1) call gs_syntax('Filename expected')
	endif

	ii=istr_r1(comline,kine.ecal)
	if(ii.ne.1) call gs_syntax('channel/MeV value for Si energy expected')
	if( kine.ecal .le. 0.000001 )call gs_syntax('Negative or too small value')
	
	ii=istr_r2(comline,kine.acmp,kine.ecmp)
	if(ii.ne.2) call gs_syntax('Mass[a.m.u.] and energy[MeV] of compound nucleus expected')
	if( kine.acmp .lt. 1.00000 )call gs_syntax('Illegal mass value')
	if( kine.ecmp .lt. 0.000001 )call gs_syntax('Illegal energy value')

	iin=lengthn(comline)
	kine.atenuation = 1.0000000
	if(iin.ne.0)then
	   ii=istr_r1(comline,kine.atenuation)
	   if(ii.ne.1) call gs_syntax('Recoil energy atenuation factor expected')
	   if( kine.atenuation .lt. 0.000001 )call gs_syntax('Illegal energy atenuation factor value')
	   iin=lengthn(comline)
	   if(iin.ne.0) call gs_syntax('Too many parameters')
	endif
	

	isize= 6 +  6*MAXDET + 2*MAXRES
	isize= 4*isize
	isize=isize+16 ! di guardia
	if(.NOT.getmem(isize,com(icmd).addr)) Stop 'Error allocating memory for RECAL_KINE'
	com(icmd).size=isize

	return

	end
	

	subroutine gs1_recal_kinenew(kine)

#include "gsort.inc"
	record/kinenew/kine

	do nn = 1,3
	  call gs_putindpar(kine.ind(nn),kine.par(nn))
	  llist=llist+3
	enddo

	lfile=max(1,lengthc(kine.afile_ge))
	write(list(llist:),'(A)') kine.afile_ge(1:lfile)

	llist=lengthc(list)+1
	write(list(llist:),'(F12.2,F12.4)') kine.offs,kine.gain

	llist=lengthc(list)+1
	write(list(llist:),'(2I6)') kine.wlo,kine.whi

	llist=lengthc(list)+2
	call gs_putfoldgate(kine.ind,kine.fl,kine.fh,kine.same)
	
	if( kine.except ) then
	   lfile=max(1,lengthc(kine.except_file))
	   write(list(llist+3:),'(A)') kine.except_file(1:lfile)
	endif
	llist=lengthc(list)

	call gs_writeline(LLUN,list,INDENT)
	list=' '
	llist=INDENT

	lfile=max(1,lengthc(kine.afile_si))
	write(list(llist:),'(A)') kine.afile_si(1:lfile)
	
	llist=lengthc(list)+1
	write(list(llist:),'(F12.4,F8.1,F12.3,F12.4)') kine.ecal,kine.acmp,kine.ecmp,kine.atenuation
	

	return
	end


	subroutine gs2_recal_kinenew(kine,dat)

#include "gsort.inc"
#if defined( __GFORTRAN__ )
	logical*1 inp_yes
	external inp_yes
#endif
	record/kinenew/kine
	record/kinenewdat/dat

	REAL XMNMEV
	PARAMETER (XMNMEV=931.49432)

	real th_ge(0:MAXDET-1),ph_ge(0:MAXDET-1)
	real th_si(0:MAXDET-1),ph_si(0:MAXDET-1)
	integer inlu /-1/


	if(init_mode.ne.0) return


	dat.acmp = kine.acmp
	dat.acmp = dat.acmp*XMNMEV
	dat.ecmp = kine.ecmp
	dat.ecal = XMNMEV/kine.ecal
	dat.afac = sqrt(kine.atenuation)

	DEG2RAD=atan(1.0000000)/45.0000000

	call gs_get_angles(th_ge,ph_ge,kine.ndet_ge,kine.afile_ge,LU2)
	do ii=0,kine.ndet_ge-1
	  th_ge(ii)=th_ge(ii)*DEG2RAD
	  ph_ge(ii)=ph_ge(ii)*DEG2RAD
	  dat.cdir_ge(1,ii)=sin(th_ge(ii))*cos(ph_ge(ii))
	  dat.cdir_ge(2,ii)=sin(th_ge(ii))*sin(ph_ge(ii))
	  dat.cdir_ge(3,ii)=cos(th_ge(ii))
	end do

	call gs_get_angles(th_si,ph_si,kine.ndet_si,kine.afile_si,LU2)
	do ii=0,kine.ndet_si-1
	  th_si(ii)=th_si(ii)*DEG2RAD
	  ph_si(ii)=ph_si(ii)*DEG2RAD
	  dat.cdir_si(1,ii)=sin(th_si(ii))*cos(ph_si(ii))
	  dat.cdir_si(2,ii)=sin(th_si(ii))*sin(ph_si(ii))
	  dat.cdir_si(3,ii)=cos(th_si(ii))
	end do

	dat.xmomcm=sqrt(2*dat.ecmp*dat.acmp)
	
	if( kine.except )then
	  lfile = lengthc(kine.except_file)
	  if(inlu.le.0) call lib$get_lun(INLU)
6	  open(unit=INLU,file=kine.except_file(1:lfile),status='old',READONLY,ERR=7)
	  goto 8

7	  write(6,*) 'Error reading  '//kine.except_file(1:lfile)
	  if(inp_yes('Retry')) goto 6
	  call exit(0)

8	  do nregs = 1,MAXRES
	    read(inlu,*,end=9,err=9)dat.reg(nregs,1),dat.reg(nregs,2)
	    if( dat.reg(nregs,1) .gt. dat.reg(nregs,2) )then
	       rr = dat.reg(nregs,1)
	       dat.reg(nregs,1) = dat.reg(nregs,2)
	       dat.reg(nregs,2) = rr
	    endif
	  enddo
	  
9	  dat.nregs = nregs-1
	  write(LU1,'( i12,''  except regions for RECAL_KINE taken from '',a)')dat.nregs,kine.except_file(1:lfile)
	  write(LU2,'( i12,''  except regions for RECAL_KINE taken from '',a)')dat.nregs,kine.except_file(1:lfile)
	  if( dat.nregs .le. 0 )kine.except = .FALSE.
	  close(inlu)
	  call lib$free_lun(INLU)
	endif
		
**********
***	open(101,file='34s-cdir.dat',status='unknown')
**********

	write(LU1,*)

	return

	end


	subroutine gs3_recal_kinenew(kine,dat)

#include "gsort.inc"
	record/kinenew/kine
	record/kinenewdat/dat
	PARAMETER (XMNMEV=931.49432)

	integer mpart(MAXDET)

	logical*1 excepted(MAXRES)

	real*8 cdir_rec(3), beta, costh
	real*8 mom_cm(3),mom_rec(3),mom_si_tot(3)
	real*8 xmrec, xmom_si, xmomrec, xx, xcan, gain, offs

	jis =kine.ind(2)

	do jj=1,3
	  mom_si_tot(jj)=0
	end do
	xmrec=dat.acmp
*	eee = 0.000000

	nelims=0
	do js=doff(jis),doff(jis)+ndet(jis)-1

	  if( det(js).ival(kine.par(2)) .le. 0 )goto 9000
	  if( det(js).xval(kine.par(3)) .lt. 0.000 )goto 9000
	  xmrec = xmrec - det(js).ival(kine.par(2))*XMNMEV
	  xmom_si = 2*det(js).ival(kine.par(2))
	  xmom_si = xmom_si*det(js).xval(kine.par(3))*dat.ecal
	  xmom_si = sqrt(xmom_si)
*	  eee = eee + det(js).xval(kine.par(3))/kine.ecal
	  do jj=1,3
	    mom_si_tot(jj)=mom_si_tot(jj)+xmom_si*dat.cdir_si(jj,det(js).id)
	  end do

	end do

	mom_cm(1)=0.0000000000
	mom_cm(2)=0.0000000000
	mom_cm(3)=dat.xmomcm
	xmomrec=0.000000000000
	do jj=1,3
	  mom_rec(jj)=mom_cm(jj)-mom_si_tot(jj)
	  xmomrec=xmomrec+mom_rec(jj)*mom_rec(jj)
	end do
*	eee = eee + xmomrec/2/xmrec
*	det(0).xval(1) = eee
*	det(0).ival(1) = eee
	
	if(xmomrec.GT.0) then
	  xmomrec=sqrt(xmomrec)
	  beta=xmomrec/xmrec*dat.afac
C	  sqbetainv=1./sqrt(1.-beta*beta)
	  xx=1.00000000/xmomrec
	  do jj=1,3
	    cdir_rec(jj)=mom_rec(jj)*xx
	  end do
***	  write(101,'(3f8.4)')(cdir_rec(jj) ,jj=1,3)
	else
	  beta=0.
C	  sqbetainv=1.
	  cdir_rec(1)=0.00000000000
	  cdir_rec(2)=0.00000000000
	  cdir_rec(3)=1.00000000000
	endif

	ji=kine.ind(1)
	jp=kine.par(1)
	jwlo=kine.wlo
	jwhi=kine.whi
	offs=kine.offs
	gain=kine.gain
	nelimg=0

	do nreg = 1,dat.nregs
	  excepted(nreg) = .FALSE.
	enddo
	
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  ican=det(jx).ival(jp)
	  xcan=det(jx).xval(jp)
	  if( kine.except )then
	     do nreg = 1,dat.nregs
	        if( ican .ge. dat.reg(nreg,1)  .and. ican .le. dat.reg(nreg,2) )then
		 if(excepted(nreg))goto 5
		 xcan=XCAN*gain + offs
		 ican=XCAN
		 excepted(nreg) = .TRUE.
		 goto 10
		endif
	     enddo
	  endif
5	  ig=det(jx).id
	  costh=0.000000000000
	  do jj=1,3
	    costh=costh+dat.cdir_ge(jj,ig)*cdir_rec(jj)
	  end do
	  if(ican.GT.0 .and. xmomrec.GT.0) then
	    Xcan=Xcan/(1.+beta*costh)		! Non Relativistic
C	    Xcan=Xcan*(1.-beta*costh)*sqbetainv	! Relativistic
	    xcan=XCAN*gain + offs
	    ican=XCAN
	  endif
10	  if(ican.GE.jwlo .and. ican.le.jwhi) then
	    det(jx).ival(jp)=ican
	    det(jx).xval(jp)=xcan
	  else
	    det(jx).id=-1
	    nelimg=nelimg+1
	  endif
	end do

	if(nelimg.ne.0 .and. kine.same) goto 9000
	nx=ndet(ji)-nelimg
	if(nx.LT.kine.fl .or. nx.GT.kine.fh) goto 9000
	if(nelimg.GT.0) call gs_checkdetector(ji)


	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_recal_kinenew

	return

	end

	
		
**********************************************************
	subroutine gs0_recal_kine(kine)

#include "gsort.inc"
	record/kine/kine

	character*72 angfil
	
	logical*1 getmem

	synt(1)='RECAL_KINE Pn (GASP|EBIII|EBIV|Agles_file_Ge) offset gain Wl Wh FOLD_GATE'
	synt(2)='           Bx By [Fn] #bananas RES Rx Ry  description_file (GASP|EB|Angles_file_Si)'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(kine.ind,kine.par,kine.res)
	if(kine.ind.le.0) call gs_syntax('Meaningless for header parameters')

	call istr_ch(comline,angfil)
	if(angfil.EQ.'GASP') then
	  kine.afile_ge=gsdir(1:lgsdir)//'NUMBERING.GE'
	  kine.ndet_ge=40
	elseif(angfil.EQ.'EBIII') then
	  kine.afile_ge=gsdir(1:lgsdir)//'EBNUMBERING.GE'
	  kine.ndet_ge=239
	elseif(angfil.EQ.'EBIV') then
	  kine.afile_ge=gsdir(1:lgsdir)//'EBIVNUMBERING.GE'
	  kine.ndet_ge=239
	else
	  kine.afile_ge=angfil	
	  kine.ndet_ge=MAXDET
	  if(lengthc(kine.afile_ge).LT.1) call gs_syntax('Filename expected')
	endif

	ii=istr_r2(comline,kine.offs,kine.gain)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	ii=istr_i2(comline,kine.wlo,kine.whi)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	call gs_getfoldgate(kine.ind,kine.fl,kine.fh,kine.same)

	iin=lengthn(comline)
	if(iin.ne.0) call gs_syntax('Too many parameters')

	call gs_readline
	do nn=1,2
	  call gs_getindpar(kine.bind(nn),kine.bpar(nn),ires)
	end do
	if(kine.bind(1).ne.kine.bind(2)) call gs_syntax('Both parameters must be in the same detector')
	if(kine.bind(1).EQ.0) call gs_syntax('Meaningless for header parameters')

	iin=lengthn(comline)
	if(iin.lt.0) then
	  call gs_getindpar(kk,kine.pinfix,ires)
	  if(kk.ne.0) call gs_syntax('A HEADER parameter for optional PIN expected here')
	else
	  kine.pinfix=-1
	endif

	nn=istr_i1(comline,kine.nban)
	if(nn.ne.1) call gs_syntax('Number of bananas expected')
	if(kine.nban.LT.1 .or. kine.nban .GT. MAXNban) call gs_syntax1('Number of bananas must be between 1 and',MAXNban)

	call gs_getbanres(kine.bres)

	iin=lengthn(comline)
	if(iin.LT.0) then
	  call istr_ch(comline,kine.kinefile)
	  if(lengthc(kine.kinefile).LT.1) call gs_syntax('Filename expected')
	endif

	call istr_ch(comline,angfil)
	if(angfil.EQ.'GASP') then
	  kine.afile_si=gsdir(1:lgsdir)//'NUMBERING.SI'
	  kine.ndet_si=40
	elseif(angfil.EQ.'EB') then
	  kine.afile_si=gsdir(1:lgsdir)//'EBNUMBERING.SI'
	  kine.ndet_si=40
	else
	  kine.afile_si=angfil	
	  kine.ndet_si=40
	  if(lengthc(kine.afile_si).LT.1) call gs_syntax('Filename expected')
	endif

	isize=3 + 4*MAXNban +  MAXDET*MAXNban + 2*3*MAXDET
	namesize= 72*MAXNban
	isize= 8*isize +namesize + 8*(2*kine.bres(1)*kine.nban)
	isize=isize+16 ! di guardia
	if(.NOT.getmem(isize,com(icmd).addr)) Stop 'Error allocating memory for RECAL_KINE'
	com(icmd).size=isize

	return

	end

	subroutine gs1_recal_kine(kine)

#include "gsort.inc"
	record/kine/kine

	call gs_putindpar(kine.ind,kine.par)

	lfile=max(1,lengthc(kine.afile_ge))
	write(list(llist+3:),'(A)') kine.afile_ge(1:lfile)

	llist=lengthc(list)+1
	write(list(llist:),'(F12.2,F12.4)') kine.offs,kine.gain

	llist=lengthc(list)+1
	write(list(llist:),'(2I6)') kine.wlo,kine.whi

	llist=lengthc(list)+2
	call gs_putfoldgate(kine.ind,kine.fl,kine.fh,kine.same)

	call gs_writeline(LLUN,list,INDENT)
	list=' '
	llist=INDENT

	do nn=1,2
	  call gs_putindpar(kine.bind(nn),kine.bpar(nn))
	  llist=llist+3
	end do

	if(kine.pinfix .GE. 0) call gs_putindpar(0,kine.pinfix)

	llist=lengthc(list)+3
	write(list(llist:),'(i4)') kine.nban

	llist=lengthc(list)+2
	call gs_putbanres(kine.bres)

	lkinefile=max(1,lengthc(kine.kinefile))
	write(list(llist+3:),'(A)') kine.kinefile(1:lkinefile)

	llist=lengthc(list)
	lfile=max(1,lengthc(kine.afile_si))
	write(list(llist+3:),'(A)') kine.afile_si(1:lfile)

	return

	end

	subroutine gs2_recal_kine(kine,dat)

#include "gsort.inc"
#if defined( __GFORTRAN__ )
	logical*1 inp_yes
	external inp_yes
#endif
	record/kine/kine
	record/kinedat/dat

	REAL XMNMEV
	PARAMETER (XMNMEV=931.49432)

	real th_ge(0:MAXDET-1),ph_ge(0:MAXDET-1)
	real th_si(0:MAXDET-1),ph_si(0:MAXDET-1)

	real edet(0:MAXDET-1,MAXNBAN)

	if(init_mode.ne.0) return

	lfile=max(1,lengthc(kine.kinefile))
6	OPEN(UNIT=gs2lun,FILE=kine.kinefile,STATUS='OLD',READONLY,ERR=8)
	write(LU1,*) 'Reading the setup from file '//kine.kinefile(1:lfile)
	write(LU2,*) 'Reading the setup from file '//kine.kinefile(1:lfile)

	read(gs2lun,*,err=9,end=9) dat.acmp,dat.ecmp
	write(LU2,*) dat.acmp,dat.ecmp
	dat.acmp=dat.acmp*XMNMEV

	kinoff=0
	do nn=1,kine.nban
	  read(gs2lun,*,err=9,end=15) dat.ad(nn)
	  write(LU2,*) dat.ad(nn)
	  dat.ad(nn)=dat.ad(nn)*XMNMEV
	  read(gs2lun,*,err=9,end=9) dat.mdet(nn)
	  write(LU2,*) dat.mdet(nn)
	  read(gs2lun,*,err=9,end=9) (edet(ii,nn),ii=0,dat.mdet(nn)-1)
	  write(LU2,*) (edet(ii,nn),ii=0,dat.mdet(nn)-1)
	  do ii=0,dat.mdet(nn)-1
	    dat.mom_si(ii,nn)=sqrt(2*edet(ii,nn)*dat.ad(nn))
	  end do
	  if(kine.pinfix .GT. 0) then
	    read(gs2lun,'(A)',end=9) comline
	    call str_tab2blank(comline)
	    ii=istr_i2(comline,dat.f1(nn),dat.f2(nn))
	    if(ii.ne.2) goto 9
	    call istr_ch(comline,dat.kban.file(nn))
	    write(LU2,'(2i6,4x,a)') dat.f1(nn),dat.f2(nn),dat.kban.file(nn)	
	  else
	    read(gs2lun,'(a)',err=9,end=9) dat.kban.file(nn)
	    write(LU2,*) dat.kban.file(nn)	
	    dat.f1(nn)=1
	    dat.f2(nn)=10
	  endif
	  call ban_getbanana(6,dat.kban.file(nn),-1,-1,dat.kban.ban(1,kinoff),kine.bres(1),kine.bres(2),nbpoints)
	  if(nbpoints.le.0) goto 10
	  kinoff=kinoff+kine.bres(1)
	  kine.nban=nn
	end do
	goto 15

8	write(LU1,*) 'Error opening '//kine.kinefile(1:lfile)
	goto 12

9	write(LU1,*) 'Error reading '//kine.kinefile(1:lfile)
	goto 12

10	write(LU1,*) 'Error with banana file: no particles given'
	kine.nban=0
	goto 12

12	if(inp_yes('Retry')) goto 6
	call exit(0)

15	close(unit=gs2lun)

	DEG2RAD=atan(1.)/45.

	call gs_get_angles(th_ge,ph_ge,kine.ndet_ge,kine.afile_ge,LU2)
	do ii=0,kine.ndet_ge-1
	  th_ge(ii)=th_ge(ii)*DEG2RAD
	  ph_ge(ii)=ph_ge(ii)*DEG2RAD
	  dat.cdir_ge(1,ii)=sin(th_ge(ii))*cos(ph_ge(ii))
	  dat.cdir_ge(2,ii)=sin(th_ge(ii))*sin(ph_ge(ii))
	  dat.cdir_ge(3,ii)=cos(th_ge(ii))
	end do

	call gs_get_angles(th_si,ph_si,kine.ndet_si,kine.afile_si,LU2)
	do ii=0,kine.ndet_si-1
	  th_si(ii)=th_si(ii)*DEG2RAD
	  ph_si(ii)   =ph_si(ii)*DEG2RAD
	  dat.cdir_si(1,ii)=sin(th_si(ii))*cos(ph_si(ii))
	  dat.cdir_si(2,ii)=sin(th_si(ii))*sin(ph_si(ii))
	  dat.cdir_si(3,ii)=cos(th_si(ii))
	end do

	dat.xmomcm=sqrt(2*dat.ecmp*dat.acmp)

	write(LU1,*)

	return

	end

	subroutine gs3_recal_kine(kine,dat)

#include "gsort.inc"
	record/kine/kine
	record/kinedat/dat

	integer mpart(MAXNBAN)
	real cdir_rec(3)
	real mom_cm(3),mom_rec(3),mom_si_tot(3)

	jis =kine.bind(1)
	jps1=kine.bpar(1)
	jps2=kine.bpar(2)
	ires=kine.bres(1)

	do ii=1,kine.nban
	  mpart(ii)=0
	end do
	do jj=1,3
	  mom_si_tot(jj)=0
	end do
	xmrec=dat.acmp

	nelims=0
	do js=doff(jis),doff(jis)+ndet(jis)-1
	  lx=det(js).ival(jps1)
	  if(lx.GE.0 .and. lx.LT.ires) then
	    is=det(js).id
	    ly=det(js).ival(jps2)
	    do ii=1,kine.nban
	      if(is.LT.dat.mdet(ii) .and.
	1       ly.GE. dat.kban.ban(1,lx) .and. ly.le. dat.kban.ban(2,lx) ) then
	        nnpart=dat.f1(ii)
	        mpart(ii)=mpart(ii)+nnpart
	        xmrec=xmrec-dat.ad(ii)
	        xmom_si=dat.mom_si(is,ii)*nnpart
	        if(xmom_si.GT.0) then
	          do jj=1,3
	            mom_si_tot(jj)=mom_si_tot(jj)+xmom_si*dat.cdir_si(jj,is)
	          end do
	        endif
	        goto 20
	      endif
	      lx=lx+ires
	    end do
	  endif
	  det(js).id=-1
	  nelims=nelims+1
20	end do

	mom_cm(1)=0
	mom_cm(2)=0
	mom_cm(3)=dat.xmomcm
	xmomrec=0
	do jj=1,3
	  mom_rec(jj)=mom_cm(jj)-mom_si_tot(jj)
	  xmomrec=xmomrec+mom_rec(jj)*mom_rec(jj)
	end do

	if(xmomrec.GT.0) then
	  xmomrec=sqrt(xmomrec)
	  beta=xmomrec/xmrec
C	  sqbetainv=1./sqrt(1.-beta*beta)
	  xx=1./xmomrec
	  do jj=1,3
	    cdir_rec(jj)=mom_rec(jj)*xx
	  end do
	else
	  beta=0.
C	  sqbetainv=1.
	  cdir_rec(1)=0.
	  cdir_rec(2)=0.
	  cdir_rec(3)=1.
	endif

	ji=kine.ind
	jp=kine.par
	jwlo=kine.wlo
	jwhi=kine.whi
	offs=kine.offs
	gain=kine.gain
	nelimg=0
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  ig=det(jx).id
	  costh=0
	  do jj=1,3
	    costh=costh+dat.cdir_ge(jj,ig)*cdir_rec(jj)
	  end do
	  ican=det(jx).ival(jp)
	  xcan=det(jx).xval(jp)
	  if(ican.GT.0 .and. xmomrec.GT.0) then
	    Xcan=Xcan/(1.+beta*costh)		! Non Relativistic
C	    Xcan=Xcan*(1.-beta*costh)*sqbetainv	! Relativistic
	    xcan=XCAN*gain + offs
	    ican=XCAN
	  endif
	  if(ican.GE.jwlo .and. ican.le.jwhi) then
	    det(jx).ival(jp)=ican
	    det(jx).xval(jp)=xcan
	  else
	    det(jx).id=-1
	    nelimg=nelimg+1
	  endif
	end do

	if(nelimg.ne.0 .and. kine.same) goto 9000
	nx=ndet(ji)-nelimg
	if(nx.LT.kine.fl .or. nx.GT.kine.fh) goto 9000
	if(nelimg.GT.0) call gs_checkdetector(ji)
	if(nelims.GT.0) call gs_checkdetector(jis)

	if(kine.pinfix .GE. 0) then
	  if(nx.GT.0) then
	    IPIN=0
	    do ii=1,kine.nban
	      IPIN=IPIN+dat.f2(II)*mpart(ii)
	    end do
	  else
	   IPIN=0
	  endif
	  det(0).ival(kine.pinfix)=IPIN
	  det(0).xval(kine.pinfix)=IPIN+0.5
	endif

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_recal_kine

	return

	end



	subroutine gs0_pin(pin)

#include "gsort.inc"
	record/pin/pin
	logical*1 getmem

	character*72 banfile(MAXNban)

	synt(1)='PIN  Px Py Fn Pm Pt #bananas  RES Rx Ry  FOLD_GATE'
	synt(2)='         {#particles_in_banana_1  weight_of_banana_1  A_of_the_particle  banana_file_1}'
	synt(3)='         {#particles_in_banana_2  weight_of_banana_2  A_of_the_particle  banana_file_2}'
	synt(4)='         {     .............................................       }'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	do nn=1,2
	  call gs_getindpar(PIN.ind(nn),PIN.par(nn),ires)
	  if(pin.ind(nn).EQ.0) call gs_syntax('Meaningless for header parameters')
	end do
	if(PIN.ind(1).ne.PIN.ind(2)) call gs_syntax('Both parameters must be in the same detector')

	call gs_getindpar(ii,PIN.FIX,pin.fixres)
	if(ii.ne.0) call gs_syntax('A HEADER parameter expected')

	do nn=3,4
	  call gs_getindpar(PIN.ind(nn),PIN.par(nn),ires)
	  if(pin.ind(nn).EQ.0) call gs_syntax('Meaningless for header parameters')
	  if(PIN.ind(1).ne.PIN.ind(nn)) call gs_syntax('This parameter must be in the same detector as the first two')
	end do


	ii=istr_i1(comline,PIN.Nban)
	if(ii.ne.1) call gs_syntax('Wrong number of parameters')

	if(PIN.Nban.LT.1 .OR. PIN.Nban.GT.maxNban)
	1	call gs_syntax1('Number of bananas must be between     1 and',MAXNban)

	call gs_getbanres(pin.res)

	call gs_getfoldgate(pin.ind(1),pin.fl,pin.fh,pin.same)

	iin=lengthc(comline)
	if(iin.ne.0) call gs_syntax('Too many parameters')
	do ii=1,PIN.Nban
	  call gs_readline
	  if(lcomline.LT.1) call gs_syntax('Missing input data')
	  iin=istr_i2(comline,pin.f1(ii),pin.f2(ii))
	  if(iin.ne.2) call gs_syntax('#particles_in_banana and weight_of_banana expected')
	  iin=istr_i1(comline,pin.f3(ii))
	  if(iin.ne.1) call gs_syntax('mass number of the particle in banana expected')
	  call istr_ch(comline,banfile(ii))
	  if(lengthc(banfile(ii)).LT.1) call gs_syntax('Filename of banana expected')
	  call filetype(banfile(ii),'ban')
	  iin=lengthc(comline)
	  if(iin.ne.0) call gs_syntax('Too many parameters')
	end do

	namesize= 72*MAXNban
	ipsize= namesize + 4*(2*pin.res(1)*pin.nban*nitems(pin.ind(1)))
	if(.NOT.getmem(ipsize,com(icmd).addr)) Stop 'Error allocating memory for PIN'
	com(icmd).size=ipsize
	call vmovb(%ref(banfile),1,%val(com(icmd).addr),1,namesize)	! riporta i nomi

	return

	end

	subroutine gs1_pin(pin,dat)

#include "gsort.inc"
	record/pin/pin
	record/bananasdat/dat

	do nn=1,2
	  call gs_putindpar(pin.ind(nn),pin.par(nn))
	    llist=llist+3
	end do

	llist=lengthc(list)+3
	call gs_putindpar(0,pin.fix)
	llist=lengthc(list)+3

	do nn=3,4
	  call gs_putindpar(pin.ind(nn),pin.par(nn))
	    llist=llist+3
	end do

	write(list(llist+2:),'(i4)') pin.nban

	llist=lengthc(list)+2
	call gs_putbanres(pin.res)

	llist=llist+2
	call gs_putfoldgate(pin.ind(1),pin.fl,pin.fh,pin.same)

	call gs_writeline(LLUN,list,INDENT)

	do ii=1,pin.nban
	  list=' '
	  llist=INDENT
	  lfile=lengthc(dat.file(ii))
	  write(list(llist:),'(3i5,3x,A)') pin.f1(ii),pin.f2(ii),pin.f3(ii),dat.file(ii)(1:lfile)
	  call gs_writeline(llun,list,INDENT)
	end do

	return

	end

	subroutine gs2_pin(pin,dat)

#include "gsort.inc"
	record/pin/pin
	record/bananasdat/dat

	if(init_mode.ne.0) return

	iresx=pin.res(1)
	iresy=pin.res(2)

	DO ILU=LU1,LU2
	  WRITE(ILU,*)
	  WRITE(ILU,'(A,2(3X,A,I1))')' Reading PIN bananas for  ',
	1	 dname(pin.ind(1)),pin.par(1),dname(pin.ind(2)),pin.par(2)
	end do
	call lib$get_lun(lun)

	lboff=0
	do kk=1,pin.nban
	  lbanfile=max(1,lengthc(dat.file(kk)))
	  DO ILU=LU1,LU2
	    WRITE(ILU,'(i5,a)') kk,'    taken from  '//dat.file(kk)(1:lbanfile)
	  end do

	  open(unit=lun,file=dat.file(kk),status='old',readonly,err=20)
	  pin.multiadc(kk) = .false.
	  do ii = 0, nitems(pin.ind(1))-1
	     call ban_findbanana(lun,-1,ii,iok)
	     if( iok .gt. 0 ) then
	       pin.multiadc(kk) = .true.
	       goto 10
	     endif
	  enddo
10	  close(lun)

	  if( pin.multiadc(kk) ) then
	    do ii = 0, nitems(pin.ind(1))-1
	      ipos=ii*pin.res(1)+lboff
	      nbpoints = -1
	      call ban_getbanomap(LU1,dat.file(kk),-1,ii,dat.ban(1,ipos),iresx,iresy,nbpoints)
	      pin.ignore(kk,ii) = nbpoints .le. 0
	    enddo

	  else
	    iadc=-1
	    IRUN=-1
	    call ban_getbanomap(lu1,dat.file(kk),IRUN,iadc,dat.ban(1,lboff),iresx,iresy,nbpoints)
	    if(nbpoints.le.0) call exit(0)
	    write(lu2,*) nbpoints,'    values read'
	    write(lu2,*) 'Corresponding memory map'
	    call ban_listbanana(lu2,dat.ban(1,lboff),iresx,iresy,64,32)
	  endif
	  lboff=lboff+pin.res(1)*nitems(pin.ind(1))
	end do

	break=.FALSE.

	return
20	write(lu1,*) 'Error opening   ',dat.file(kk)
        stop

	end

	subroutine gs3_pin(pin,dat)

#include "gsort.inc"
	record/pin/pin
	record/bananasdat/dat

	integer ipin
	integer npart(MAXNban)


	if( (ndet(pin.ind(1)).LT.pin.fl) .or. (ndet(pin.ind(1)).GT.pin.fh) ) then
	  det(0).ival(pin.fix)= pin.fixres+1
	  det(0).xval(pin.fix)=det(0).ival(pin.fix)
	  return
	endif
	   

	ji=pin.ind(1)

	do ii=1,pin.nban
	  npart(ii)=0
	end do
	ntpart = 0

	n1=pin.par(1)
	n2=pin.par(2)

	ires=pin.res(1)

	do jx=doff(ji),doff(ji)+ndet(ji)-1

	  det(jx).ival(pin.par(3)) = 0
	  det(jx).xval(pin.par(3)) = det(jx).ival(pin.par(3))
	  det(jx).ival(pin.par(4)) = 0
	  det(jx).xval(pin.par(4)) = det(jx).ival(pin.par(4))
	  
	  if(det(jx).ival(n1).GE.0 .and. det(jx).ival(n1).LT.ires) then
	    ly=det(jx).ival(n2)
	    do ii=1,pin.nban

	      if( pin.multiadc(ii) .and. (.not.pin.ignore(ii,det(jx).id)) )then
	        lx = (det(jx).id + (ii-1)*nitems(pin.ind(1)))*ires + det(jx).ival(n1)
	        if(ly.GE.dat.ban(1,lx) .and. ly.le.dat.ban(2,lx)) then
	          npart(ii)=npart(ii)+pin.f1(ii)
		  ntpart = ntpart + 1
		  det(jx).ival(pin.par(3)) = pin.f1(ii)*pin.f3(ii)
		  det(jx).xval(pin.par(3)) = det(jx).ival(pin.par(3))
		  det(jx).ival(pin.par(4)) = pin.f1(ii)*pin.f2(ii)
		  det(jx).xval(pin.par(4)) = det(jx).ival(pin.par(4))
	        endif
		
	      elseif( .not.pin.multiadc(ii) ) then
	        lx = (ii-1)*nitems(pin.ind(1))*ires + det(jx).ival(n1)
	        if(ly.GE.dat.ban(1,lx) .and. ly.le.dat.ban(2,lx)) then
	          npart(ii)=npart(ii)+pin.f1(ii)
		  ntpart = ntpart + 1
		  det(jx).ival(pin.par(3)) = pin.f1(ii)*pin.f3(ii)
		  det(jx).xval(pin.par(3)) = det(jx).ival(pin.par(3))
		  det(jx).ival(pin.par(4)) = pin.f1(ii)*pin.f2(ii)
		  det(jx).xval(pin.par(4)) = det(jx).ival(pin.par(4))
	        endif
	      endif
	      
	    end do
	  endif
	end do

	if(ntpart .eq. ndet(ji)) then
	  IPIN=0
	  do ii=1,pin.nban
	    IPIN=IPIN+PIN.f2(II)*npart(ii)
	  end do
	else
	   IPIN=pin.fixres+1
	endif
	det(0).ival(pin.fix)=IPIN
	det(0).xval(pin.fix)=IPIN

	return

	end

	subroutine gs4_pin

	return

	end



	subroutine gs0_hk(hk)

#include "gsort.inc"
	record/hk/hk

	synt(1)='HK   Dn  Fh Fk   offset gain  Wl Wh'
	synt(2)='                (offset gain  Wl Wh refer to sum of the Dn values H)'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(hk.ind,hk.par,ires)
	if(.NOT.EXISTS(hk.ind)) call gs_syntax('No such detector')

	call gs_getindpar(ii,hk.fixh,ires)
	if(ii.ne.0) call gs_syntax('A HEADER parameter expected')

	call gs_getindpar(ii,hk.fixk,ires)
	if(ii.ne.0) call gs_syntax('A HEADER parameter expected')

	ii=istr_r2(comline,hk.offs,hk.gain)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	ii=istr_i2(comline,hk.wlo,hk.whi)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	return

	end

	subroutine gs1_hk(hk)

#include "gsort.inc"
	record/hk/hk

	call gs_putindpar(hk.ind,hk.par)

	llist=llist+3
	call gs_putindpar(0,hk.fixh)

	llist=llist+3
	call gs_putindpar(0,hk.fixk)

	write(list(llist+1:),'(F12.2,F12.4,2I6)') hk.offs,hk.gain,hk.wlo,hk.whi

	return

	end

	subroutine gs2_hk

	return

	end

	subroutine gs3_hk(hk)

#include "gsort.inc"
	record/hk/hk

	ji =hk.ind
	jp =hk.par
	etot=0
	ifold=0
	do ii=doff(ji),doff(ji)+ndet(ji)-1
	  if(det(ii).ival(jp) .GT. 0) then
	    etot=etot+det(ii).xval(jp)
	    ifold=ifold+1
	  endif
	end do
	if(ifold.GT.0) etot=hk.offs + etot*hk.gain
	ietot=etot 
	if(ietot.LT.hk.wlo .or. ietot.GT.hk.whi) goto 9000

	det(0).ival(hk.fixH)=ietot
	det(0).xval(hk.fixH)=etot
	
	det(0).ival(hk.fixk)=ifold
	det(0).xval(hk.fixk)=ifold

	return
	
9000	killev=.TRUE.
	return

	end

	subroutine gs4_hk

	return

	end



	subroutine gs0_add(add)

#include "gsort.inc"
	record/add/add

	character ch1*10

	synt(1)='ADD   P1 P2 P3  [FACTORS f1 f2] [OFFSET off] [GAIN g]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	do nn=1,3
	  call gs_getindpar(ADD.ind(nn),ADD.par(nn),ires)
	end do

	if( ((ADD.ind(2).ne.ADD.ind(1)) .and. (ADD.ind(2).ne.0)) .or.
	1   (ADD.ind(3).ne.ADD.ind(1)) )
	1	call gs_syntax('All parameters must be in the same detector or the second one fixed')

	add.mult=.FALSE.
	add.offset=0.0
	add.gain=1.0

	iin=lengthn(comline)
        do while(iin .LT. 0) 
	  call istr_ch2up(comline,ch1)
	  if (ch1(1:1).EQ.'F') then
	    iin=istr_r2(comline,ADD.FAC(1),add.fac(2))
	    if(iin.ne.2) call gs_syntax('Wrong number of parameters')
*	    if(add.fac(1).EQ.0.) call gs_syntax('0 is not a legal value')
*	    if(add.fac(2).EQ.0.) call gs_syntax('0 is not a legal value')
	    add.mult=.TRUE.
	  elseif (ch1(1:1).EQ.'O') then
	    iin=istr_r1(comline,add.offset)
	    if(iin.ne.1) call gs_syntax('Wrong number of parameters')
	  elseif (ch1(1:1).EQ.'G') then
	    iin=istr_r1(comline,add.gain)
	    if(iin.ne.1) call gs_syntax('Wrong number of parameters')
	  else
	    call gs_syntax('FACTOR or OFFSET or GAIN expected')
	  end if
	  iin=lengthn(comline)
	enddo

	return

	end

	subroutine gs1_add(add)

#include "gsort.inc"
	record/add/add

	do nn=1,3
	  call gs_putindpar(add.ind(nn),add.par(nn))
	  llist=llist+3
	end do

	if(add.mult) then
	  f1=add.fac(1)
	  f2=add.fac(2)
	  llist=lengthc(list)+3
	  write(list(llist:),'(2f12.5)') f1,f2
	  llist=lengthc(list)+2
	endif
        if(add.offset .NE. 0.0) then
	  write(list(llist:),'(f12.5)') add.offset
	  llist=lengthc(list)+2
	endif
        if(add.gain .NE. 1.0) then
	  write(list(llist:),'(f12.5)') add.gain
	  llist=lengthc(list)+2
	endif

	return

	end

	subroutine gs2_add

	return

	end

	subroutine gs3_add(add)

#include "gsort.inc"
	record/add/add

	ji =add.ind(1)
	jk =add.ind(2)
	jn1=add.par(1)
	jn2=add.par(2)
	jn3=add.par(3)

	if(add.mult) then
	  f1=add.fac(1)
	  f2=add.fac(2)
	  do ii=doff(ji),doff(ji)+ndet(ji)-1
	   if (jk .ne. 0)then
	    xx=add.offset + add.gain*( f1*det(ii).xval(jn1) + f2*det(ii).xval(jn2))
	   else
	    xx=add.offset + add.gain*( f1*det(ii).xval(jn1) + f2*det(0).xval(jn2))
	   endif	   
	    det(ii).xval(jn3)=xx
	    det(ii).ival(jn3)=xx
	  end do
	else
	  do ii=doff(ji),doff(ji)+ndet(ji)-1
	   if (jk .ne. 0)then
	    xx=add.offset + add.gain*( det(ii).xval(jn1) + det(ii).xval(jn2))
	   else
	    xx=add.offset + add.gain*( det(ii).xval(jn1) + det(0).xval(jn2))
	   endif
	    det(ii).xval(jn3)=xx
	    det(ii).ival(jn3)=xx
	  end do
	endif

	return

	end

	subroutine gs4_add

	return

	end

	subroutine gs0_mult(add)

#include "gsort.inc"
	record/add/add

	character ch1*10

	synt(1)='MULT  P1 P2 P3  [OFFSET off] [GAIN g]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	do nn=1,3
	  call gs_getindpar(ADD.ind(nn),ADD.par(nn),ires)
	end do

	if( (ADD.ind(2).ne.ADD.ind(1)) .or.
	1   (ADD.ind(3).ne.ADD.ind(1)) )
	1	call gs_syntax('All parameters must be in the same detector')

	add.mult=.FALSE.
	add.offset=0.0
	add.gain=1.0

	iin=lengthn(comline)
        do while(iin .LT. 0) 
	  call istr_ch2up(comline,ch1)
	  if (ch1(1:1).EQ.'O') then
	    iin=istr_r1(comline,add.offset)
	    if(iin.ne.1) call gs_syntax('Wrong number of parameters')
	  elseif (ch1(1:1).EQ.'G') then
	    iin=istr_r1(comline,add.gain)
	    if(iin.ne.1) call gs_syntax('Wrong number of parameters')
	  else
	    call gs_syntax('OFFSET or GAIN expected')
	  end if
	  iin=lengthn(comline)
	enddo

	return

	end

	subroutine gs1_mult(add)

#include "gsort.inc"
	record/add/add

	do nn=1,3
	  call gs_putindpar(add.ind(nn),add.par(nn))
	  llist=llist+3
	end do

        if(add.offset .NE. 0.0) then
	  llist=lengthc(list)+3
	  write(list(llist:),'(f12.5)') add.offset
	  llist=lengthc(list)+2
	endif
        if(add.gain .NE. 1.0) then
	  write(list(llist:),'(f12.5)') add.gain
	  llist=lengthc(list)+2
	endif

	return

	end

	subroutine gs2_mult

	return

	end

	subroutine gs3_mult(add)

#include "gsort.inc"
	record/add/add

	ji =add.ind(1)
	jn1=add.par(1)
	jn2=add.par(2)
	jn3=add.par(3)

	if(add.mult) then
	  do ii=doff(ji),doff(ji)+ndet(ji)-1
	    xx=add.offset + add.gain* det(ii).xval(jn1) *det(ii).xval(jn2)
	    det(ii).xval(jn3)=xx
	    det(ii).ival(jn3)=xx
	  end do
	else
	  do ii=doff(ji),doff(ji)+ndet(ji)-1
	    xx=add.offset + add.gain * det(ii).xval(jn1) * det(ii).xval(jn2)
	    det(ii).xval(jn3)=xx
	    det(ii).ival(jn3)=xx
	  end do
	endif

	return

	end

	subroutine gs4_mult

	return

	end


	subroutine gs0_div(add)

#include "gsort.inc"
	record/add/add

	character ch1*10

	synt(1)='DIVIDE  P1 P2 P3  [OFFSET off] [GAIN g]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	do nn=1,3
	  call gs_getindpar(ADD.ind(nn),ADD.par(nn),ires)
	end do

	if( (ADD.ind(2).ne.ADD.ind(1)) .or.
	1   (ADD.ind(3).ne.ADD.ind(1)) )
	1	call gs_syntax('All parameters must be in the same detector')

	add.mult=.FALSE.
	add.offset=0.0
	add.gain=1.0

	iin=lengthn(comline)
        do while(iin .LT. 0)
	  call istr_ch2up(comline,ch1)
	  if (ch1(1:1).EQ.'O') then
	    iin=istr_r1(comline,add.offset)
	    if(iin.ne.1) call gs_syntax('Wrong number of parameters')
	  elseif (ch1(1:1).EQ.'G') then
	    iin=istr_r1(comline,add.gain)
	    if(iin.ne.1) call gs_syntax('Wrong number of parameters')
	  else
	    call gs_syntax('OFFSET or GAIN expected')
	  end if
	  iin=lengthn(comline)
	enddo

	return

	end

	subroutine gs1_div(add)

#include "gsort.inc"
	record/add/add

	do nn=1,3
	  call gs_putindpar(add.ind(nn),add.par(nn))
	  llist=llist+3
	end do

        if(add.offset .NE. 0.0) then
	  llist=lengthc(list)+3
	  write(list(llist:),'(f12.5)') add.offset
	  llist=lengthc(list)+2
	endif
        if(add.gain .NE. 1.0) then
	  write(list(llist:),'(f12.5)') add.gain
	  llist=lengthc(list)+2
	endif

	return

	end

	subroutine gs2_div

	return

	end

	subroutine gs3_div(add)

#include "gsort.inc"
	record/add/add

	ji =add.ind(1)
	jn1=add.par(1)
	jn2=add.par(2)
	jn3=add.par(3)

	nelim = 0

	do ii=doff(ji),doff(ji)+ndet(ji)-1
	  if( det(ii).xval(jn2) .lt. 0.0001 )then
	    det(ii).id = -1
	    nelim = nelim+1
	  else
	    xx=add.offset + add.gain* det(ii).xval(jn1) /det(ii).xval(jn2)
	    det(ii).xval(jn3)=xx
	    det(ii).ival(jn3)=xx
	  endif
	end do

	if( nelim .gt. 0)call gs_checkdetector(ji)
	return

	end

	subroutine gs4_div

	return

	end

************************************* begin power
	subroutine gs0_pow(add)

#include "gsort.inc"
	record/add/add

	character ch1*10

	synt(1)='POWER  P1 exponent  [OFFSET off] [GAIN g]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(ADD.ind(1),ADD.par(1),ires)

	add.mult=.FALSE.
	add.offset=0.0
	add.gain=1.0

	iin=lengthn(comline)
       iin = istr_r1(comline,add.fac(1))
       if(iin.ne.1) call gs_syntax('Exponent value expected here')
	iin=lengthn(comline)

       do while(iin .LT. 0)
	  call istr_ch2up(comline,ch1)
	  if (ch1(1:1).EQ.'O') then
	    iin=istr_r1(comline,add.offset)
	    if(iin.ne.1) call gs_syntax('Wrong number of parameters')
	  elseif (ch1(1:1).EQ.'G') then
	    iin=istr_r1(comline,add.gain)
	    if(iin.ne.1) call gs_syntax('Wrong number of parameters')
	  else
	    call gs_syntax('OFFSET or GAIN expected')
	  end if
	  iin=lengthn(comline)
	enddo

	return

	end

	subroutine gs1_pow(add)

#include "gsort.inc"
	record/add/add

	call gs_putindpar(add.ind(1),add.par(1))
	llist=llist+3

       write(list(llist:),'(f12.5)') add.fac(1)
	llist=lengthc(list)+2

        if(add.offset .NE. 0.0) then
	  llist=lengthc(list)+3
	  write(list(llist:),'(f12.5)') add.offset
	  llist=lengthc(list)+2
	endif
       if(add.gain .NE. 1.0) then
	  write(list(llist:),'(f12.5)') add.gain
	  llist=lengthc(list)+2
	endif

	return

	end

	subroutine gs2_pow

	return

	end

	subroutine gs3_pow(add)

#include "gsort.inc"
	record/add/add

	ji =add.ind(1)
	jn1=add.par(1)

	nelim = 0

	do ii=doff(ji),doff(ji)+ndet(ji)-1
	  if( det(ii).xval(jn1) .lt. 0.00000 )then
	    det(ii).id = -1
	    nelim = nelim+1
	  else
	    xx=add.offset + add.gain* ( det(ii).xval(jn1)**add.fac(1) )
	    det(ii).xval(jn1)=xx
           det(ii).ival(jn1)=xx
	  endif
	end do

	if( nelim .gt. 0)call gs_checkdetector(ji)
	return

	end

	subroutine gs4_pow

	return

	end
***************************************** end power



	subroutine gs0_kill(kill)

#include "gsort.inc"
	record/kill/kill

	synt(1)='KILL   D  {list_of_detectors_to_kill}'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getind(kill.ind)
	if(kill.ind.EQ.0) call gs_syntax('Detector Identifier expected')

	ji=kill.ind
	do ii=0,nitems(ji)-1
	  kill.det(ii)=.FALSE.
	end do

	iin=lengthn(comline)
	do while(iin.GT.0)
	  jjdmmy = istr_i1(comline,jj)
	  if(jj.LT.0 .or. jj.GE.nitems(ji)) call gs_syntax1('Detector index must be between 0 and',nitems(ji)-1)
	  kill.det(jj)=.TRUE.
	  iin=lengthn(comline)
	end do

	return

	end

	subroutine gs1_kill(kill)

#include "gsort.inc"
	record/kill/kill

	call gs_putind(kill.ind)

	nn=nitems(kill.ind)
	do ii=0,nn-1
	  if(kill.det(ii)) then
	    llist=lengthc(list)+1
	    write(list(llist:),'(i4)') ii
	  endif
	end do

	return

	end

	subroutine gs2_kill

	return

	end

	subroutine gs3_kill(kill)

#include "gsort.inc"
	record/kill/kill

	ji=kill.ind
	nelim=0
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  if(kill.det(det(jx).id)) then
	    det(jx).id=-1
	    nelim=nelim+1
	  endif
	end do
	if(nelim.GT.0) call gs_checkdetector(ji)

	return

	end

	subroutine gs4_kill

	return

	end



	subroutine gs0_select(sel)

#include "gsort.inc"
	record/select/sel

	synt(1)='SELECT  D  {list_of_detectors_to_select}'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getind(sel.ind)
	if(sel.ind.EQ.0) call gs_syntax('Detector Identifier expected')

	ji=sel.ind
	do ii=0,nitems(ji)-1
	  sel.det(ii)=.FALSE.
	end do

	iin=lengthn(comline)
	do while(iin.GT.0)
	  jjdmmy = istr_i1(comline,jj)
	  if(jj.LT.0 .or. jj.GE.nitems(ji)) call gs_syntax1('Detector index must be between 0 and',nitems(ji)-1)
	  sel.det(jj)=.TRUE.
	  iin=lengthn(comline)
	end do

	return

	end

	subroutine gs1_select(sel)

#include "gsort.inc"
	record/select/sel

	call gs_putind(sel.ind)

	nn=nitems(sel.ind)
	do ii=0,nn-1
	  if(sel.det(ii)) then
	    llist=max(1,lengthc(list))+1
	    write(list(llist:),'(i4)') ii
	  endif
	end do

	return

	end

	subroutine gs2_select

	return

	end

	subroutine gs3_select(sel)

#include "gsort.inc"
	record/select/sel

	ji=SEL.ind
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  if(sel.det(det(jx).id)) return
	end do

	killev=.TRUE.
	return

	end

	subroutine gs4_select

	return

	end



	subroutine gs0_store_event

#include "gsort.inc"

	synt(1)='STORE_EVENT'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	return

	end

	subroutine gs1_store_event

	return

	end

	subroutine gs2_store_event

	return

	end

cpgi$r opt=1
	subroutine gs3_store_event

#include "gsort.inc"

	do ji=0,NTIPI
	  if(EXISTS(ji)) then
	    do ii=doff(ji),doff(ji)+ndet(ji)-1
	      sdet(ii).id=det(ii).id
	      do jj=0,TDPAR(ji)-1
	        sdet(ii).ival(jj)=det(ii).ival(jj)
	        sdet(ii).xval(jj)=det(ii).xval(jj)
	      end do
	    end do
	    sndet(ji)=ndet(ji)
	    sdoff(ji)=doff(ji)
	  endif
	end do
	STORED_EV=.TRUE.

	return

	end

	subroutine gs4_store_event

	return

	end



	subroutine gs0_recall_event(rcll)

#include "gsort.inc"

	record /recall/rcll
	character*32 ch1

	synt(1)='RECALL_EVENT  ALWAYS or IFVALID'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	iin=lengthn(comline)
        if (iin .LT. 0) then

	  call istr_ch2up(comline,ch1)
	  if (ch1(1:1).EQ.'A') then
	    rcll.always = .true.
	  elseif (ch1(1:1).EQ.'I') then
	    rcll.always = .false.
	  else
	    call gs_syntax(' ALWAYS or IFVALID expected')
	  end if

	else
	    call gs_syntax(' ALWAYS or IFVALID expected')
	endif


	return

	end

	subroutine gs1_recall_event(rcll)
	
#include "gsort.inc"
	record /recall/rcll

	llist=max(1,lengthc(list))+1
	if (rcll.always) then
	  write(list(llist:),'(2x,''ALWAYS'')')
	else	
	 write(list(llist:),'(2x,''IFVALID'')')
	endif

	return

	end

	subroutine gs2_recall_event

	return

	end

cpgi$r opt=1
	subroutine gs3_recall_event(rcll)

#include "gsort.inc"
	record /recall/rcll


*??	if( killev .and. (.not.rcll.always) )STORED_EV = .FALSE.

	if( killev .and. (.not.rcll.always) )return	
	if( rcll.always .and. STORED_EV ) killev = .FALSE.
	
	if(.NOT.STORED_EV)return
	
	do ji=0,NTIPI
	  if(EXISTS(ji)) then
	    do ii=sdoff(ji),sdoff(ji)+sndet(ji)-1
	      det(ii).id=sdet(ii).id
	      do jj=0,TDPAR(ji)-1
	        det(ii).ival(jj)=sdet(ii).ival(jj)
	        det(ii).xval(jj)=sdet(ii).xval(jj)
	      end do
	    end do
	    ndet(ji)=sndet(ji)
	    doff(ji)=sdoff(ji)
	  endif
	end do

	return

	end

	subroutine gs4_recall_event

	return

	end



	subroutine gs0_write_event

#include "gsort.inc"

	CHARACTER CH1*1
	INTEGER VECTOR(0:MAXPAR-1)

	synt(1)='WRITE_EVENT   [Tape|Disk]  [Reduce {0|1 for_every_defined_parameter}]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(oev.defined) then
	write(6,*) ' GSORT%Warning - will write same structure as first WRITE command'
	return
	endif
	
	oio.datafile=.FALSE.
	oio.wlen=MAXWORDS
	oio.run=1
	oio.rec=0
	oio.maxrec=1000000
	oio.evcount=0
	oio.name=' '
	oio.closed=.TRUE.

	oev.reduce=.FALSE.
	oev.same=.TRUE.
	oev.ask=.FALSE.
	oev.evlen=0
	oev.pointer=0

	iin=lengthn(comline)
	if(iin.ne.0) then
	  call istr_ch2up(comline,ch1)
	  if(ch1.EQ.'D') then
	    oio.datafile=.TRUE.
	    oio.wlen=8192
	  elseif(ch1.EQ.'T') then
	    oio.datafile=.FALSE.
	    oio.wlen=MAXWORDS
	    oio.flag=MTFLAGW
	  elseif(ch1.EQ.'R') then
	    oev.reduce=.TRUE.
	  else
	    call gs_syntax('Tape/Disk/Reduce expected')
	  endif
	endif
	if(.NOT.oev.reduce) then
	  iin=lengthn(comline)
	  if(iin.ne.0) then
	    call istr_ch2up(comline,ch1)
	    if(ch1.EQ.'R') then
	      oev.reduce=.TRUE.
	    else
	      call gs_syntax('Reduce expected')
	    endif
	  endif
	endif
	if(oev.reduce) then
	  do ji=0,NTIPI
	    oev.det(ji)=.FALSE.
	    if(TDPAR(ji).GT.0) then
	      ii=istr_ia(comline,VECTOR,TDPAR(ji))
	      if(ii.ne.TDPAR(ji)) call gs_syntax('Wrong number of parameters')
	      do jp=0,TDPAR(ji)-1
	        if(vector(jp).GT.0) then
	          oev.det(ji)=.TRUE.
	          oev.par(jp,ji)=.TRUE.
	        else
	          oev.par(jp,ji)=.FALSE.
	        endif
	      end do
	    endif
	  end do
	endif
	oev.defined=.TRUE.

	return

	end

	subroutine gs1_write_event

#include "gsort.inc"

	if(oio.datafile) then
	  write(list(llist:),'(a)') 'DISK'
	else
	  write(list(llist:),'(a)') 'TAPE'
	endif

	if(oev.reduce) then
	  llist=lengthc(list)+2
	  write(list(llist:),'(a)') '  REDUCE'
	  llist=lengthc(list)+1
	  do ji=0,NTIPI
	    if(TDPAR(ji).GT.0) then
	      llist=llist+1
	      do jp=0,TDPAR(ji)-1
	        if(oev.par(jp,ji)) then
	          list=list(1:llist)//'1'
	        else
	          list=list(1:llist)//'0'
	        endif
	        llist=llist+2
	      end do
	    endif
	  end do
	endif

	return

	end

	subroutine gs2_write_event

#include "gsort.inc"
	integer NWrComm,NWrCRT,NWrApp
	save NWrComm,NWrCRT,NWrApp
	
	character*256 tmp_ofile_name
	
	
	if(init_mode.EQ.0) then
	  NWrComm=NWrComm+1
	  NWrCRT=-1
	  if(NWrComm .gt. 1)return
	  write(6,*)
	  if(oio.datafile) then
	    do while(lengthc(oio.name).LT.1)
	      call inp_str('Filename of the  O U T P U T  file',oio.name)
	      call filetype(iio.name,'dat')
	    end do
	    call lib$get_lun(oio.lun)
	    oio.closed=.TRUE.
	  else
#ifdef Digital
	    oio.name='/dev/nrmt0h'
#endif
#ifdef sun
	    oio.name='/dev/rmt/0mbn'
#endif
#ifdef __linux__
	    oio.name='/dev/nst0'
#endif
#ifdef __APPLE__
	    oio.name='/dev/nrst0'
#endif
	    write(6,*) 'Definition of the  O U T P U T  tape'
	    CALL TAPE_ASSIGN (oio.name,oio.mtch)	! Scelta dell' unita` nastro
	    CALL TAPE_CONTROL(oio.name,oio.mtch)	! su cui scrivere
CVMS	    CALL STR_TOUPPER(oio.name)

	  endif
	  call inp_ask('Keep the input FILE structure on the output stream',oev.same)
	  call inp_ask('Keep the input RUN number on the output stream',oev.sameRUN)
	  call inp_ask('Want to be asked wether to continue after every EOF',oev.ask)
*	  if(.NOT.oev.sameRUN .and. .NOT.oev.ask) then
	  if( .NOT.oev.sameRUN ) then
	    call inp_i1('RUN# to start with on the output stream',oio.run)
	  endif
	  call inp_i1('Maximum number of records for the output unit',oio.maxrec)
	  if(oio.datafile .and.oio.closed) then
	    write( tmp_ofile_name, '(a,i4.4)') oio.name(1:lengthc(oio.name)),infiles+1
	    write(6,*) 'Opening  ',tmp_ofile_name(1:lengthc(tmp_ofile_name))
	    open(oio.lun,file=tmp_ofile_name(1:lengthc(tmp_ofile_name)),status='new',access='direct',
#if  defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER)
#if  defined(Digital)
	1      initialsize=900,extendsize=1800,
#endif
	2	recl=oio.wlen/2)
#else
	1      recl=oio.wlen*2)
#endif
	    oio.closed=.FALSE.
	  endif
	  return
	endif
	
	NWrApp=NWrApp+1
	if( NWrCRT .lt. (NWrApp/NWrComm) )then
	    NWrCRT=NWrCRT+1
	else
	    return
	endif

	if(oio.datafile .and.oio.closed) then
	  write( tmp_ofile_name, '(a,i4.4)') oio.name(1:lengthc(oio.name)),infiles
	  write(6,*) 'WRITE_EVENT  Opening  ',tmp_ofile_name(1:lengthc(tmp_ofile_name))
	  open(oio.lun,file=tmp_ofile_name(1:lengthc(tmp_ofile_name)),status='new',access='direct',
#if defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER)
#if defined(Digital)
	1      initialsize=900,extendsize=1800,
#endif
	2	recl=oio.wlen/2)
#else
	1      recl=oio.wlen*2)
#endif
	  oio.closed=.FALSE.
	endif

	if(oev.ask) then
	  call inp_i1('RUN number for the output stream',oio.run)
	  call bytes_to_char(ievheader(17),line,72)
	  call inp_str('Comment',line)
	  call char_to_bytes(line,ievheader(17),72)
	endif

	if(oev.sameRUN) oio.run=iio.run
	call gs5_OEVheader(-1)			! recall stored header
	if(.not.oio.datafile) call gs5_OEVflush			! and write it
	if(break) call exit(0)

	call gs5_OEVheader(1)			! set header word for data
#if defined( _GW_BIG_ENDIAN )
	     call gs_swap_bytes(oevbuf(1),16)
#endif

	return

	end

	subroutine gs3_write_event

#include "gsort.inc"
	integer*2 nm_mask

	if(oev.done)return           ! execute only first WRITE request (if many)
	oev.done=.true.
	
	oev.evlen=1					! spazio per SW
	if(oev.reduce) then			!!!! Event in reduced format
	  if(oev.det(0)) then
	    do jp=0,TDPAR(0)-1
	      if(oev.par(jp,0)) then
	        oev.evlen=oev.evlen+1
	        OEVent(oev.evlen)=det(0).ival(jp)	! Fpars
	      endif
	    end do
	  endif
	  do ji=1,NTIPI
	    if(oev.det(ji)) then
	      oev.evlen=oev.evlen+1
	      OEVent(oev.evlen)=ndet(ji)		! # rivelatori
	    endif
	  end do
	  do ji=1,NTIPI
	    if(oev.det(ji)) then
	      do jk=doff(ji),doff(ji)+ndet(ji)-1
	        oev.evlen=oev.evlen+1
	        OEVent(oev.evlen)=det(jk).id		! id
	        do jp=0,TDPAR(ji)-1
	          if(oev.par(jp,ji)) then
	            oev.evlen=oev.evlen+1
	            OEVent(oev.evlen)=det(jk).ival(jp) 	! parameters
	          endif
	        end do
	      end do
	    endif
	  end do
	else					!!!! Event in standard NEO format
	  oev.evlen=oev.evlen+1
	  OEVent(oev.evlen)=0				! classification
	  oev.evlen=oev.evlen+1
	  OEVent(oev.evlen)=0				! tag
	  do jp=0,TDPAR(0)-1
	    oev.evlen=oev.evlen+1
	    OEVent(oev.evlen)=det(0).ival(jp)		! Fpars
	  end do
	  ipoint=oev.evlen+2*NTIPI+1
	  do ji=1,NTIPI
	    oev.evlen=oev.evlen+1
	    OEVent(oev.evlen)=ndet(ji)			! # rivelatori
	    oev.evlen=oev.evlen+1
	    OEVent(oev.evlen)=ipoint			! pointer
	    ipoint=ipoint+ndet(ji)*(TDPAR(ji)+1)
	  end do
	  do ji=1,NTIPI
	    do jk=doff(ji),doff(ji)+ndet(ji)-1
	      do jp=-1,TDPAR(ji)-1
	        oev.evlen=oev.evlen+1
	        OEVent(oev.evlen)=det(jk).ival(jp)	! id and parameters
	      end do
	    end do
	  end do
	endif
	if(oev.evlen.GT.maxEVL) then
	  break=.TRUE.
	  write(LU1,*) 'Output event is too long',oev.evlen
	  return
	endif
	OEVent(1)=oev.evlen-1
	nm_mask = 'F000'X
	OEVent(1)=IOR(nm_mask,OEVent(1))

	if((oev.pointer+oev.evlen).GT.oio.wlen)then
	  call gs5_OEVflush
	  if(break) return
	endif

	do ii=1,oev.evlen
	  oevbuf(oev.pointer+ii)=OEVent(ii)
	end do
	oev.pointer=oev.pointer+oev.evlen
	oev.evlen=0
	oio.evcount=oio.evcount+1

	return

	end

	subroutine gs4_write_event

#include "gsort.inc"
#if defined( __GFORTRAN__ )
	logical*1 inp_not
	external inp_not
#endif

	if(finit_mode.EQ.0) return
	if(FINISHED_OUT)return
	FINISHED_OUT=.true.

	lname=max(1,lengthc(oio.name))
	if(finit_mode.EQ.1) then
	  call gs5_OEVflush
	  oev.evlen=0
	  oev.pointer=0
	  do ilu=lu1,lu2
	    WRITE(ILU,*) oio.rec  ,' Records written'
	    WRITE(ILU,*) oio.evcount,' Events  written'
	  end do
	  lname=max(1,lengthc(oio.name))
	  if(oio.datafile) then
	   if(oev.same) then
	      write(6,*) 'Closing   ',oio.name(1:lname)
	      close(oio.lun)
	      oio.closed=.TRUE.
	      oio.rec=0
	      oio.evcount=0
	    endif
	  elseif(oev.same .and. oev.sameRUN .and. (.NOT.oev.ask)) then
	    write(LU1,*) 'Writing EOF on   ',oio.name(1:lname)
	    write(LU2,*) 'Writing EOF on   ',oio.name(1:lname)
	    CALL TAPE_WRITEOF(oio.mtch)
	    oio.rec=0
	    oio.evcount=0
	  elseif(oev.same .and. (.NOT.oev.sameRUN) .and. (.NOT.oev.ask)) then
	    write(LU1,*) 'Writing EOF on   ',oio.name(1:lname)
	    write(LU2,*) 'Writing EOF on   ',oio.name(1:lname)
	    CALL TAPE_WRITEOF(oio.mtch)
	    oio.run = oio.run+1
	    oio.rec = 0
	    oio.evcount = 0
	  elseif(oev.ask) then
	    CALL TAPE_CONTROL(oio.name,oio.mtch)
	    if(inp_not('Did you close the output stream')) then
	      if(.NOT.oev.sameRUN) oio.run=oio.run+1
	      oio.rec=0
	      oio.evcount=0
	    endif
	  endif
	else
	  call gs5_OEVflush
	  if(oio.datafile) then
	    close(oio.lun)
	  elseif(oev.same) then
	    write(LU1,*) 'Writing EOF on   ',oio.name(1:lname)
	    write(LU2,*) 'Writing EOF on   ',oio.name(1:lname)
	    CALL TAPE_WRITEOF(oio.mtch)
	  elseif(oev.ask) then
	    CALL TAPE_CONTROL(oio.name,oio.mtch)
	  endif
	endif

	return

	end

	subroutine gs5_OEVflush

#include "gsort.inc"
#if defined( __GFORTRAN__ )
	logical*1 inp_yes
	external inp_yes
#endif
	integer*2 OWW(2)
	integer   OLW
	equivalence (OWW,OLW)

	INTEGER TAPE_WRITE

	if(oev.pointer.le.16) return		! do not write if empty

10	do ii=oev.pointer+1,oio.wlen
	  oevbuf(ii)=0
	end do
	oev.pointer=oio.wlen

#if defined( _GW_BIG_ENDIAN )
	OWW(2)=0				! per evitare il relativo warning
	OLW=oio.rec
	OEVbuf(2)=OWW(2)
	OEVbuf(3)=oio.run
#else
	OWW(1)=0				! per evitare il relativo warning
	OLW=oio.rec
	OEVbuf(2)=OWW(1)
	OEVbuf(3)=oio.run
#endif

#if defined( _GW_BIG_ENDIAN )
	  if(OEVbuf( 4).eq.'4748'X)then
	     call gs_swap_bytes(oevbuf(1),16)
	  else
	     call gs_swap_bytes(oevbuf(3),1)
	     call gs_swap_bytes(oevbuf(17),oio.wlen-16)
	  endif
#endif

	if(oio.datafile) then
	  write(oio.lun,rec=(oio.rec+1),err=50) (oevbuf(ii),ii=1,oio.wlen)
	  oio.rec=oio.rec+1
	else
	  nbyout=2*oio.wlen
	  ISTAT=TAPE_WRITE(oevbuf,nbyout,oio.mtch,oio.flag,oio.iosb)
	  IF(ISTAT .ne. 1) goto 50
	  oio.rec=oio.rec+1
	  CALL TAPE_SYNCW(oio.flag,oio.rec,ISTW,oio.iosb,oio.name,oio.mtch,1)
	  IF(ISTW.LT.0) then
	    write(6,*) 'Error writing record',oio.rec
	    CALL TAPE_CONTROL(oio.name,oio.mtch)
	    if(inp_yes('Want to stop analysys')) then
	      BREAK=.TRUE.
	      return
	    elseif(inp_yes('Continuing with a new tape')) then
	      oio.rec=0
	      CALL TAPE_CONTROL(oio.name,oio.mtch)
	      CALL STR_TOUPPER(oio.name)
	      if(inp_yes('Write the header record')) then
	        call gs5_OEVheader(-1)
	        goto 10
	      endif
	    else
	      BREAK=.TRUE.
	      return
	    endif
	  endif
	endif

	if(oio.rec.EQ.1) then
	  call gs5_OEVheader(1)			! set header for data
#if defined( _GW_BIG_ENDIAN )
	     call gs_swap_bytes(oevbuf(1),16)
#endif
	endif

	if(oio.rec.GE.oio.maxrec) break=.TRUE.

	oev.pointer=16
	return

50	write(6,*) 'Error writing record',oio.rec+1
51	if(inp_yes('Want to stop analysys')) then
	  BREAK=.TRUE.
	  oev.pointer=0
	endif

	return

	end

	subroutine gs5_OEVheader(iflag)

#include "gsort.inc"

	integer*2 OWW(2)
	integer   OLW
	equivalence (OWW,OLW)

	if(iflag.EQ.1) then	! set header for data records
	  OEVbuf(1)=oio.wlen/512
#if defined( _GW_BIG_ENDIAN )
	  OWW(2)=0		! per evitare il relativo warning
	  OLW=oio.rec
	  OEVbuf(2)=OWW(2)
#else
	  OWW(1)=0		! per evitare il relativo warning
	  OLW=oio.rec
	  OEVbuf(2)=OWW(1)
#endif
	  OEVbuf(3)=oio.run
	  if(oev.reduce) then
	    OEVbuf(4)='4758'X	! GX
	  else
	    OEVbuf(4)='4744'X	! GD
	  endif
	  OEVbuf(5)=16
	  do ii=6,16
	    OEVbuf(ii)=0
	  end do
	  oev.pointer=16
	elseif(iflag.EQ.-1) then		! set header block
	  do ii=1,oio.wlen
	    OEVbuf(ii)=ievheader(ii)
	  end do
	  OEVbuf( 1)=oio.wlen/512
	  OEVbuf( 2)=oio.rec
	  OEVbuf( 3)=oio.run
	  OEVbuf( 4)='4748'X	! GH
	  OEVbuf( 5)=16
	  OEVbuf( 6)=16711
	  OEVbuf( 7)=20563
	  OEVbuf( 8)=32
	  OEVbuf( 9)=-256
	  OEVbuf(10)=-4081
	  do ii=11,16
	    OEVbuf(ii)=0
	  end do
	  oev.pointer=oio.wlen
	endif

	RETURN

	END



	subroutine gs0_list_event(lev)

#include "gsort.inc"
	record/listev/lev

	synt(1)='LIST_EVENT     [filename]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	lev.file='TT'
	lev.tofile=.FALSE.
	lev.isopen=.FALSE.
	lev.lun=6

	iin=lengthn(comline)
	if(iin.ne.0) then
	  call istr_ch(comline,lev.file)
	  if(lengthc(lev.file).LT.0) call gs_syntax('Filename expected')
	  call filetype(lev.file,'dat')
	  lev.tofile=.TRUE.
	  lev.lun=-1
	endif

	return

	end

	subroutine gs1_list_event(lev)

#include "gsort.inc"
	record/listev/lev

	if(lev.tofile) then
	  lfile=max(1,lengthc(lev.file))
	  write(list(llist:),'(a)') lev.file(1:lfile)
	else
	  write(list(llist:),'(a)') 'TERMINAL'
	endif

	return

	end

	subroutine gs2_list_event(lev)

#include "gsort.inc"
	record/listev/lev

	if(lev.tofile .and. .NOT.lev.isopen) then
	  if(lev.lun .le.0) then
	    call lib$get_lun(lev.lun)
	    if(lev.lun.le.0) stop 'Error opening file in LIST_EVENT'
	  endif
	  open(lev.lun,file=lev.file,status='new')
	  lev.isopen=.TRUE.
	endif

	return

	end

	subroutine gs3_list_event(lev)

#include "gsort.inc"
	record/listev/lev

	if(iio.evnumber.EQ.0) then
	  write(lev.lun,'(1x,a,i8,a,i7)')      'Ev#',iio.evcount,'    Rec#',iio.rec
	else
	  write(lev.lun,'(1x,a,i8,a,i7,a,Z8)') 'Ev#',iio.evcount,'    Rec#',iio.rec,'  EvC ',iio.evnumber
	endif

	if(TDPAR(0).GT.0) then
	  line='   '//dname(0)
	  lpnt=14
	  do ii=0,TDPAR(0)-1
	    write(line(lpnt:),'(i7)') det(0).ival(ii)
	    lpnt=lpnt+7
	    if(lpnt.GT.73) then
	      write(lev.lun,'(a)')line(1:lpnt-1)
	      line=' '
	      lpnt=14
	    endif
	  end do
	  if(lpnt.GT.14) write(lev.lun,'(a)')line(1:lpnt-1)
	endif

	do ji=1,NTIPI
	  if(ndet(ji).GT.0) then
	    line='   '//dname(ji)
	    lpnt=8
	    do ii=doff(ji),doff(ji)+ndet(ji)-1
	      write(line(lpnt:),'(i5)') det(ii).id
	      lpnt=14
	      do jj=0,TDPAR(ji)-1
	        write(line(lpnt:),'(i7)') det(ii).ival(jj)
	        lpnt=lpnt+7
	        if(lpnt.GT.73) then
	          write(lev.lun,'(a)')line(1:lpnt-1)
	          line=' '
	          lpnt=14
	        endif
	      end do
	      if(lpnt.GT.14) then
	         write(lev.lun,'(a)')line(1:lpnt-1)
	         line=' '
	         lpnt=8
	       endif
	    end do
	  endif
	end do

	return

	end

	subroutine gs4_list_event

	return

	end



	subroutine gs0_reorder(reo)

#include "gsort.inc"
	record/reorder/reo

	synt(1)='REORDER   [D]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	reo.ind=-1
	iin=lengthn(comline)
	if(iin.ne.0) then
	  call gs_getind(reo.ind)
	  if(reo.ind.EQ.0) call gs_syntax('Detector Identifier expected')
	endif

	return

	end

	subroutine gs1_reorder(reo)

#include "gsort.inc"
	record/reorder/reo

	if(reo.ind.GT.0) call gs_putind(reo.ind)

	return

	end

	subroutine gs2_reorder

	return

	end

	subroutine gs3_reorder(reo)

#include "gsort.inc"
	record/reorder/reo

	logical*1 ripeti

	if(reo.ind .GT.0) then
	  ji1=reo.ind
	  ji2=reo.ind
	else
	  ji1=1
	  ji2=NTIPI
	endif
	do ji=ji1,ji2
	  ripeti=ndet(ji).GT.0
	  do while(ripeti)
	    ripeti=.FALSE.
	    do ii=doff(ji),doff(ji)+ndet(ji)-2
	      if(det(ii).id .GT. det(ii+1).id) then
	        ripeti=.TRUE.
	        ltmp         = det(ii  ).id
	        det(ii  ).id = det(ii+1).id
	        det(ii+1).id = ltmp
	        do jj=0,TDPAR(ji)-1
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
	end do

	return

	end

	subroutine gs4_reorder

	return

	end



	subroutine gs0_statistics(stat)

#include "gsort.inc"
	record/statistics/stat

	synt(1)='STATISTICS'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	nstatcom=nstatcom+1
	stat.which=nstatcom

	return

	end

	subroutine gs1_statistics(stat)

#include "gsort.inc"
	record/statistics/stat

	if(stat.which.LT.10) then
	  write(list(llist:),'(''(STAT'',I1,'')'')') stat.which
	else
	  write(list(llist:),'(''(STAT'',I2,'')'')') stat.which
	endif

	return

	end

	subroutine gs2_statistics(stat,dat)

#include "gsort.inc"
	record/statistics/stat
	record/statdat/dat

	if(init_mode.ne.0) return

	com(icmd).addr=stataddr

	return

	end

	subroutine gs3_statistics(stat,dat)

#include "gsort.inc"
	record/statistics/stat
	record/statdat/dat

	do ji=1,NTIPI
	  dat.fold(ndet(ji),ji,stat.which)=dat.FOLD(ndet(ji),ji,stat.which)+1
	end do

	return

	end

	subroutine gs4_statistics

	return

	end


	subroutine gs0_swap(swap)

#include "gsort.inc"
	record/swap/swap

	synt(1)='SWAP  Px Py'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	do nn=1,2
	  call gs_getindpar(swap.ind(nn),swap.par(nn),ires)
	end do
	if(swap.ind(1).ne.swap.ind(2)) call gs_syntax('Both parameters must be in the same detector')

	return

	end

	subroutine gs1_swap(swap)

#include "gsort.inc"
	record/swap/swap

	do nn=1,2
	  call gs_putindpar(swap.ind(nn),swap.par(nn))
	    llist=llist+3
	end do

	return

	end

	subroutine gs2_swap

	return

	end

	subroutine gs3_swap(swap)

#include "gsort.inc"
	record/swap/swap

	ji=swap.ind(1)
	jp1=swap.par(1)
	jp2=swap.par(2)
	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  itmp=det(jx).ival(jp1)
	  det(jx).ival(jp1)=det(jx).ival(jp2)
	  det(jx).ival(jp2)=itmp
	  xtmp=det(jx).xval(jp1)
	  det(jx).xval(jp1)=det(jx).xval(jp2)
	  det(jx).xval(jp2)=xtmp
	end do

	return

	end

	subroutine gs4_swap

	return

	end


	subroutine gs0_mask(mask)

#include "gsort.inc"
	record/mask/mask

	synt(1)='MASK  P[x] mask'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	ll=lengthn(comline)
	if(ll.EQ.-1) then
	  call gs_getind(mask.ind)
	  mask.one=.FALSE.
	else
	  call gs_getindpar(mask.ind,mask.par,ires)
	  mask.one=.TRUE.
	endif

	jjdmmy = istr_i1(comline,mask.mask)

	return

	end

	subroutine gs1_mask(mask)

#include "gsort.inc"
	record/mask/mask

	if(mask.one) then
	  call gs_putindpar(mask.ind,mask.par)
	else
	  call gs_putind(mask.ind)
	endif

#if defined( __GFORTRAN__ )
	write(list(llist+4:),'(i0)') mask.mask
#else
	write(list(llist+4:),'(i)') mask.mask
#endif

	return

	end

	subroutine gs2_mask

	return

	end

	subroutine gs3_mask(mask)

#include "gsort.inc"
	record/mask/mask

	ji=mask.ind
	lmask=mask.mask
	if(mask.one) then
	  jp=mask.par
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	    iold=det(jx).ival(jp)
	    inew=iand(iold,lmask)
	    if(inew.ne.iold) then
	      det(jx).ival(jp)=inew
	      det(jx).xval(jp)=inew + rand_real2()
	    endif
	  end do
	else
	  jp1=TDPAR(ji)-1
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	  do jp=0,jp1
	    iold=det(jx).ival(jp)
	    inew=iand(iold,lmask)
	    if(inew.ne.iold) then
	      det(jx).ival(jp)=inew
	      det(jx).xval(jp)=inew + rand_real2()
	    endif
	  end do
	  end do
	endif

	return

	end

	subroutine gs4_mask

	return

	end


	subroutine gs0_useful(useful)

#include "gsort.inc"
	record/useful/useful

	synt(1)='USEFUL  Fa Fb Fc Fd'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	nn=0

	ll=lengthn(comline)
	if(ll.LT.0) then
	  call gs_getindpar(useful.ind,useful.par1,ires)
	  if(useful.ind.NE.0) call gs_syntax('Must be a HEADER parameter')
	  if(useful.par1.LT.0) call gs_syntax('Illegal parameter number')
	  nn=nn+1
	endif

	ll=lengthn(comline)
	if(ll.LT.0) then
	  call gs_getindpar(useful.ind,useful.par2,ires)
	  if(useful.ind.NE.0) call gs_syntax('Must be a HEADER parameter')
	  if(useful.par2.LT.0) call gs_syntax('Illegal parameter number')
	  nn=nn+1
	endif

	ll=lengthn(comline)
	if(ll.LT.0) then
	  call gs_getindpar(useful.ind,useful.par3,ires)
	  if(useful.ind.NE.0) call gs_syntax('Must be a HEADER parameter')
	  if(useful.par2.LT.0) call gs_syntax('Illegal parameter number')
	  nn=nn+1
	endif

	ll=lengthn(comline)
	if(ll.LT.0) then
	  call gs_getindpar(useful.ind,useful.par4,ires)
	  if(useful.ind.NE.0) call gs_syntax('Must be a HEADER parameter')
	  if(useful.par2.LT.0) call gs_syntax('Illegal parameter number')
	  nn=nn+1
	endif

	if(nn.ne.4) call gs_syntax('Please give 4 HEADER parameters')

	return

	end

	subroutine gs1_useful(useful)

#include "gsort.inc"
	record/useful/useful

	call gs_putindpar(useful.ind,useful.par1)
	llist= lengthc(list)+4
	call gs_putindpar(useful.ind,useful.par2)
	llist= lengthc(list)+4
	call gs_putindpar(useful.ind,useful.par3)
	llist= lengthc(list)+4
	call gs_putindpar(useful.ind,useful.par4)

	return

	end

	subroutine gs2_useful

	return

	end

	subroutine gs3_useful(useful)

#include "gsort.inc"
	record/useful/useful

	jx=doff(useful.ind)

	jp=useful.par1
	lval=iio.run
	det(jx).ival(jp)=lval
	det(jx).xval(jp)=lval

	jp=useful.par2
	lval=iio.rec
	det(jx).ival(jp)=lval
	det(jx).xval(jp)=lval

	jp=useful.par3
	lval=iio.evcount
	det(jx).ival(jp)=lval
	det(jx).xval(jp)=lval

	jp=useful.par4
	lval=iio.evnumber
	det(jx).ival(jp)=lval
	det(jx).xval(jp)=lval

	return

	end

	subroutine gs4_useful

	return

	end


	subroutine gs0_move(move)

#include "gsort.inc"
	record/move/move

	synt(1)='MOVE  D [list_of_detectors] N [id_of_first] [GATE Pn Low High]' ! [IN|OUT] [FOLD_GATE]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getind(move.ind1)

	do ii=0,MAXDET-1
	   move.which(ii)=-1
	end do

	nmove=0
	do while(lengthn(comline).GT.0)
	  jjdmmy = istr_i1(comline,ii)
	  if(ii.GE.0 .AND. ii.LT.nitems(move.ind1)) then
	    move.which(ii)=nmove
	    nmove=nmove+1
	  else
	    call gs_syntax('Detector number out of range')
	  endif
	  if(move.ind1.EQ.0 .AND. nmove.GT.1) call gs_syntax('You can move only 1 detector to the Header')
	end do
	move.howmany=nmove
	if(nmove.EQ.0) then
	  do ii=0,nitems(move.ind1)-1
	    move.which(ii)=ii
	  end do
	endif
	
	call gs_getind(move.ind2)
	if(nitems(move.ind2) .LT. nmove) call gs_syntax('Too many detectors for destination')
	
	move.offset=0
	if(lengthn(comline).GT.0) jjdmmy = istr_i1(comline,move.offset)
	if(move.offset.LT.0 .OR. (move.offset+nmove).GT.nitems(move.ind2) ) call gs_syntax('Out of range in destination')
	do ii=0,MAXDET-1
	  if(move.which(ii).GE.0) move.which(ii)=move.which(ii)+move.offset
	end do

	move.condition=.FALSE.
	if(lengthn(comline).LT.0) then
	  call istr_ch2up(comline,comsort)
	  if(comsort(1:1).NE.'G') call gs_syntax('GATE expected')
	  call gs_getindpar(move.ind,move.par,move.res)
	  if(move.ind.ne.move.ind1) then
	     list='Detector type  '//dname(move.ind1)//'  expected'
	     llist=lengthc(list)
	     call gs_syntax(list(1:llist))
	  endif
	  call gs_get2ilim(move.lo,move.hi,0,move.res-1)
c	  call gs_getinout(move.inside)
c	  call gs_getfoldgate(move.ind,move.fl,move.fh,move.same)
	  move.condition=.TRUE.
	endif

	return

	end

	subroutine gs1_move(move)

#include "gsort.inc"
	record/move/move

	call gs_putind(move.ind1)
	if(move.howmany .GT.0) then
	  llist=llist+3
	  do ii=0,MAXDET-1
	    if(move.which(ii).GE.0) then
              write(list(llist:),'(i4)') ii
	      llist=lengthc(list)+1
	     endif
	  end do
	endif

	llist=lengthc(list)+4
	call gs_putind(move.ind2)
	if(move.offset.gt.0) then
	  llist=lengthc(list)+2
	  write(list(llist:),'(i5)') move.offset
	endif

	if(move.condition) then
	  call gs_writeline(LLUN,list,INDENT)
	  list=' '
	  list(INDENT:)='GATE'
	  llist=lengthc(list)+2
	  call gs_putindpar(move.ind,move.par)
	  write(list(llist+2:),'(2i6)') move.lo,move.hi

c	  llist=lengthc(list)+2
c	  call gs_putinout(move.inside)
c
c	  llist=lengthc(list)+2
c	  call gs_putfoldgate(move.ind,move.fl,move.fh,move.same)

	endif

	return

	end

	subroutine gs2_move

	return

	end

	subroutine gs3_move(move)

cpgi$r opt=1
#include "gsort.inc"
	record/move/move

	logical*1 nocheck
	
	if(ndet(move.ind1).LT.1) return

	ioff=0
	ind1=move.ind1
	ind2=move.ind2
	if(move.condition) then
	  nocheck=.FALSE.
	  jp=move.par
	  jlo=move.lo
	  jhi=move.hi
	else
	  nocheck=.TRUE.
	endif

	do nn=0,NTIPI
	  if(nn.EQ.ind1) then
	    mdoff(nn)=ioff
	    mndet(nn)=0
	    if(nocheck) then				! riporta in mdet(nn) solo quelli da non spostare
	      do ii=doff(nn),doff(nn)+ndet(nn)-1
	        id=det(ii).id
	        if(move.which(id).LT.0) then
	          mdet(ioff).id=det(ii).id
	          do jj=0,TDPAR(nn)-1
	            mdet(ioff).ival(jj)=det(ii).ival(jj)
	            mdet(ioff).xval(jj)=det(ii).xval(jj)
	          end do
	          ioff=ioff+1
	          mndet(nn)=mndet(nn)+1
	        endif
	      end do
	    else					! riporta in mdet(nn) sia quelli da non spostare
	      do ii=doff(nn),doff(nn)+ndet(nn)-1	! che e quelli che stanno fuori finestra
	        id=det(ii).id
	        ican=det(ii).ival(jp)
	        if(move.which(id).LT.0 .OR. ican.LT.jlo .OR. ican.GT.jhi) then
	          mdet(ioff).id=det(ii).id
	          do jj=0,TDPAR(nn)-1
	            mdet(ioff).ival(jj)=det(ii).ival(jj)
	            mdet(ioff).xval(jj)=det(ii).xval(jj)
	          end do
	          ioff=ioff+1
	          mndet(nn)=mndet(nn)+1
	        endif
	      end do
	    endif
	  elseif(nn.EQ.ind2) then
	    mdoff(nn)=ioff
	    mndet(nn)=ndet(nn)
	    if(ndet(nn).GT.0) then			! riporta quelli gia esistenti
	      do ii=doff(nn),doff(nn)+ndet(nn)-1
	        mdet(ioff).id=det(ii).id
	        do jj=0,TDPAR(nn)-1
	          mdet(ioff).ival(jj)=det(ii).ival(jj)
	          mdet(ioff).xval(jj)=det(ii).xval(jj)
	        end do
	        ioff=ioff+1
	      end do
	    endif
	    if(nocheck) then				! riporta in mdet(nn) quelli da spostare
	      do ii=doff(ind1),doff(ind1)+ndet(ind1)-1
	        id=det(ii).id
	        if(move.which(id).GE.0) then
	          mdet(ioff).id=move.which(id)
	          do jj=0,TDPAR(nn)-1
	            mdet(ioff).ival(jj)=det(ii).ival(jj)
	            mdet(ioff).xval(jj)=det(ii).xval(jj)
	          end do
	          ioff=ioff+1
	          mndet(nn)=mndet(nn)+1
	        endif
	      end do
	    else					! riporta in mdet(nn) quelli da spostare
	      do ii=doff(ind1),doff(ind1)+ndet(ind1)-1	! solo ste il GATE e' soddisfatto
	        id=det(ii).id
	        ican=det(ii).ival(jp)
	        if(move.which(id).GE.0 .AND. ican.GE.jlo .AND. ican.le.jhi) then
	          mdet(ioff).id=move.which(id)
	          do jj=0,TDPAR(nn)-1
	            mdet(ioff).ival(jj)=det(ii).ival(jj)
	            mdet(ioff).xval(jj)=det(ii).xval(jj)
	          end do
	          ioff=ioff+1
	          mndet(nn)=mndet(nn)+1
	        endif
	      end do
	    endif
	  else						! questi non sono coinvolti
	    mndet(nn)=ndet(nn)
	    mdoff(nn)=ioff
	    if(mndet(nn).GT.0) then
	      do ii=doff(nn),doff(nn)+ndet(nn)-1
	        mdet(ioff).id=det(ii).id
	        do jj=0,TDPAR(nn)-1
	          mdet(ioff).ival(jj)=det(ii).ival(jj)
	          mdet(ioff).xval(jj)=det(ii).xval(jj)
	        end do
	        ioff=ioff+1
	      end do
	    endif
	  endif
	end do

	do nn=0,NTIPI					! ricostruisce l'evento
	  ndet(nn)=mndet(nn)
	  if(ndet(nn).GT.0) then
	    doff(nn)=mdoff(nn)
	    do ii=doff(nn),doff(nn)+ndet(nn)-1
	      det(ii).id=mdet(ii).id
	      do jj=0,TDPAR(nn)-1
	        det(ii).ival(jj)=mdet(ii).ival(jj)
	        det(ii).xval(jj)=mdet(ii).xval(jj)
	      end do
	    end do
	  endif
	end do	
	return

	end

	subroutine gs4_move

	return

	end



	subroutine gs0_copy(move)

#include "gsort.inc"
	record/move/move

	synt(1)='COPY  Da Db  or  D  N'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(lengthn(comline) .EQ. -1) then
	  call gs_getind(move.ind1)
	  call gs_getind(move.ind2)
	  if(nitems(move.ind2) .LT. nitems(move.ind1)) call gs_syntax('Not enough detectors in destination')
	  move.condition=.TRUE.
	else
	  call gs_getindpar(move.ind1,move.par1,ires)
	  call gs_getindpar(move.ind2,move.par2,ires)
	  if(move.ind1 .NE. move.ind2) call gs_syntax('Must be in the same detector')
	  if(move.par1 .EQ. move.par2) call gs_syntax('Must be different parameter')
	  move.condition=.FALSE.
	endif

	return

	end

	subroutine gs1_copy(move)

#include "gsort.inc"
	record/move/move

	if(move.condition) then
	  call gs_putind(move.ind1)
	  llist=lengthc(list)+3
	  call gs_putind(move.ind2)
	else
	  call gs_putindpar(move.ind1,move.par1)
	  llist=lengthc(list)+3
	  call gs_putindpar(move.ind2,move.par2)
	endif

	return

	end

	subroutine gs2_copy

	return

	end

	subroutine gs3_copy(move)

#include "gsort.inc"
	record/move/move

	logical*1 check
	
	if(move.condition) then
	  jind1=move.ind1
	  jind2=move.ind2
	  ndet(jind2)=0
	  if(ndet(jind1).LT.1) return
	  ioff=0
	  do nn = 0, NTIPI
	    ioff=max(ioff,doff(nn)+ndet(nn))
	  enddo
c	manca la verifica che ci sia effettivamente posto
	  ioff=ioff+1
	  doff(jind2)=ioff
	  ndet(jind2)=ndet(jind1)
	  do ii=doff(jind1),doff(jind1)+ndet(jind1)-1
	    det(ioff).id = det(ii).id 
	    do jj=0,TDPAR(jind1)-1
	      det(ioff).ival(jj)=det(ii).ival(jj)
	      det(ioff).xval(jj)=det(ii).xval(jj)
	    end do
	    ioff=ioff+1
	  end do
	else
	  jind=move.ind1
	  if(ndet(jind).LT.1) return
	  jp1=move.par1
	  jp2=move.par2
	  do ii=doff(jind),doff(jind)+ndet(jind)-1
	    det(ii).ival(jp2)=det(ii).ival(jp1)
	    det(ii).xval(jp2)=det(ii).xval(jp1)
	  enddo
	endif

	return

	end

	subroutine gs4_copy

	return

	end

	subroutine gs0_merge(mrg)

#include "gsort.inc"
	record/splitmerge/mrg

	synt(1)='MERGE  {list_of_detector_types_to_merge_into_destination_D}  D'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mrg.nind=0

	do while(lengthn(comline).ne.0)
	  call gs_getind(nn)
	  if(nn.EQ.0) call gs_syntax('Detector Identifier expected')
	  mrg.ind(mrg.nind)=nn
	  mrg.nind=mrg.nind+1
	end do
	if(mrg.nind .LT. 3) call gs_syntax('Meaningful only for more than 2 detectors')

	do nn=0,NDTYPE
	  mrg.remove(nn)=.FALSE.
	  if(.NOT.EXISTS(nn)) mrg.remove(nn)=.TRUE.
	end do
	joff=0
	do nn=0,mrg.nind-2
	  mrg.remove(mrg.ind(nn))=.TRUE.
	  mrg.offset(nn)=joff
	  joff=joff+nitems(mrg.ind(nn))
	end do
	mrg.remove(mrg.ind(mrg.nind-1))=.TRUE.	! the destination

	return

	end

	subroutine gs1_merge(mrg)

#include "gsort.inc"
	record/splitmerge/mrg

	do nn=0,mrg.nind-1
	  call gs_putind(mrg.ind(nn))
	  llist=llist+2
	  if(nn .EQ. mrg.nind-2) llist=llist+2
	end do

	return

	end

	subroutine gs2_merge

	return

	end

cpgi$r opt=1
	subroutine gs3_merge(mrg)

#include "gsort.inc"
	record/splitmerge/mrg

	mdest=mrg.ind(mrg.nind-1)

	do nn=0,NTIPI
	  mndet(nn)=0
	end do
	ioff=0

	do nn=0,NTIPI
	  if( .NOT. mrg.remove(nn)) then
	    mdoff(nn)=ioff
	    mn=ndet(nn)
	    if(mn.GT.0) then
	      mndet(nn)=mn
	      do ii=doff(nn),doff(nn)+mn-1
	        mdet(ioff).id=det(ii).id
	        do jj=0,TDPAR(nn)-1
	          mdet(ioff).ival(jj)=det(ii).ival(jj)
	          mdet(ioff).xval(jj)=det(ii).xval(jj)
	        end do
	        ioff=ioff+1
	      end do
	    endif
	  elseif(nn.EQ.mdest) then
	    mdoff(nn)=ioff
	    mndet(nn)=0
	    do mm=0,mrg.nind-1
	      mind=mrg.ind(mm)
	      mn=ndet(mind)
	      if(mn.GT.0) then
		moff=mrg.offset(mm)
	        mndet(nn)=mndet(nn)+mn
	        do ii=doff(mind),doff(mind)+mn-1
	          mdet(ioff).id=det(ii).id + moff
	          do jj=0,TDPAR(mind)-1
	            mdet(ioff).ival(jj)=det(ii).ival(jj)
	            mdet(ioff).xval(jj)=det(ii).xval(jj)
	          end do
	          ioff=ioff+1
	        end do
	      endif
	    end do
	  endif
	end do

	do nn=0,NTIPI
	  ndet(nn)=mndet(nn)
	  if(ndet(nn).GT.0) then
	    doff(nn)=mdoff(nn)
	    do ii=doff(nn),doff(nn)+ndet(nn)-1
	      det(ii).id=mdet(ii).id
	      do jj=0,TDPAR(nn)-1
	        det(ii).ival(jj)=mdet(ii).ival(jj)
	        det(ii).xval(jj)=mdet(ii).xval(jj)
	      end do
	    end do
	  endif
	end do	
	return

	end

	subroutine gs4_merge

	return

	end


	subroutine gs0_split(spl)

#include "gsort.inc"
	record/splitmerge/spl

	synt(1)='SPLIT  D {list_of_destination_detector_types_into_which_D_is_split}'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	spl.nind=0

	do while(lengthn(comline).LT.0)
	  call gs_getind(nn)
	  if(nn.EQ.0) call gs_syntax('Detector Identifier expected')
	  spl.ind(spl.nind)=nn
	  spl.nind=spl.nind+1
	end do
	if(spl.nind .LT. 3) call gs_syntax('Meaningful only for more than 2 detectors')
	
	do nn=0,NDTYPE
	  spl.remove(nn)=.FALSE.
	  if(.NOT.EXISTS(nn)) spl.remove(nn)=.TRUE.
	end do
	joff=0
	do nn=1,spl.nind-1
	  spl.remove(spl.ind(nn))=.TRUE.
	  spl.offset(nn)=joff
	  joff=joff+nitems(spl.ind(nn))
	end do
	spl.remove(spl.ind(0))=.TRUE.	! the source

	return

	end

	subroutine gs1_split(spl)

#include "gsort.inc"
	record/splitmerge/spl

	do nn=0,spl.nind-1
	  call gs_putind(spl.ind(nn))
	  llist=llist+2
	  if(nn .EQ. 0) llist=llist+2
	end do

	return

	end

	subroutine gs2_split

	return

	end

cpgi$r opt=1
	subroutine gs3_split(spl)

#include "gsort.inc"
	record/splitmerge/spl

	morig=spl.ind(0)
	if(ndet(morig).le.0) return

	do nn=0,NTIPI
	  mndet(nn)=0
	end do
	ioff=0

	do nn=0,NTIPI
	  if(.NOT. spl.remove(nn)) then
	    mdoff(nn)=ioff
	    mn=ndet(nn)
	    if(mn.GT.0) then
	      mndet(nn)=mn
	      do ii=doff(nn),doff(nn)+mn-1
	        mdet(ioff).id=det(ii).id
	        do jj=0,TDPAR(nn)-1
	          mdet(ioff).ival(jj)=det(ii).ival(jj)
	          mdet(ioff).xval(jj)=det(ii).xval(jj)
	        end do
	        ioff=ioff+1
	      end do
	    endif
	  elseif(nn.EQ.morig) then
	    ii1=doff(morig)
	    ii2=ii1+ndet(morig)-1
	    mdoff(nn)=ioff
	    mndet(nn)=0
	    do mm=1,spl.nind-1
	      mind=spl.ind(mm)
	      mdoff(mind)=ioff
	      mndet(mind)=0
	      mm1=spl.offset(mm)
	      mm2=mm1+nitems(mind)
	      do ii=ii1,ii2
	        id=det(ii).id
	        if(id.GE.mm1 .and. id.LT.mm2) then
	          mndet(mind)=mndet(mind)+1
	          mdet(ioff).id=det(ii).id - mm1
	          do jj=0,TDPAR(mind)-1
	            mdet(ioff).ival(jj)=det(ii).ival(jj)
	            mdet(ioff).xval(jj)=det(ii).xval(jj)
	          end do
	          ioff=ioff+1
	        endif
	      end do
	    end do
	  endif
	end do

	do nn=0,NTIPI
	  ndet(nn)=mndet(nn)
	  if(ndet(nn).GT.0) then
	    doff(nn)=mdoff(nn)
	    do ii=doff(nn),doff(nn)+ndet(nn)-1
	      det(ii).id=mdet(ii).id
	      do jj=0,TDPAR(nn)-1
	        det(ii).ival(jj)=mdet(ii).ival(jj)
	        det(ii).xval(jj)=mdet(ii).xval(jj)
	      end do
	    end do
	  endif
	end do	


	return

	end

	subroutine gs4_split

	return

	end



	subroutine gs0_newid(newid)

#include "gsort.inc"
	record/newid/newid

	synt(1)='NEWID  D  file [REORDER]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getind(newid.ind)
	if(newid.ind.EQ.0) call gs_syntax('Detector name expected')

	call istr_ch(comline,newid.file)
	if(lengthc(newid.file).LT.1) call gs_syntax('Filename expected')
	call filetype(newid.file,'dat')

	newid.reorder=.FALSE.
	iin=lengthn(comline)
	if(iin.LT.0) then
	  call istr_ch2up(comline,comsort)
	  if(comsort(1:1).EQ.'R') then
	    newid.reorder=.TRUE.
	  else
	    call gs_syntax('REORDER expected')
	  endif
	endif

	return

	end

	subroutine gs1_newid(newid)

#include "gsort.inc"
	record/newid/newid

	call gs_putind(newid.ind)

	lfile=max(1,lengthc(newid.file))
	write(list(llist+3:),'(A)') newid.file(1:lfile)

	llist=lengthc(list)+3
	if(newid.reorder) list(llist:)='REORDER'

	return

	end

	subroutine gs2_newid(newid)

#include "gsort.inc"
	record/newid/newid
	integer dat(0:1)
#if defined( __GFORTRAN__ )
	logical*1 inp_yes
	external inp_yes
#endif

	if(init_mode.ne.0) return

	ind=newid.ind

2	DO ii=0,MAXDET-1
	   newid.lut(ii)=ii
	end do

	lfile=max(1,lengthc(newid.file))
	OPEN(UNIT=gs2lun,FILE=newid.file,STATUS='OLD',READONLY,err=30)
	DO ILU=LU1,LU2
	 WRITE(ILU,*)
	 WRITE(ILU,'(A)')' NewID table for '//dname(ind)//'   taken from file '//newid.file(1:lfile)
	end do

5	READ(gs2lun,*,END=20) i1,i2
	if(i1.GE.0 .and. i1.LT.nitems(ind)) then
 	  if(i2.GE.0 .and. i2.LT.nitems(ind)) then
	    newid.lut(i1)=i2
	    write(lu1,'(i6,''  ===> '',i6)') i1,i2
	    write(lu2,'(i6,''  ===> '',i6)') i1,i2
	  else
	    write(6,*) ' Id2 out of range',i2
	    CLOSE(gs2lun)
	    if(inp_yes('Retry')) goto 2
	    call exit(0)
	  endif
	else
	  write(6,*) ' Id1 out of range',i1
	  CLOSE(gs2lun)
	  if(inp_yes('Retry')) goto 2
	  call exit(0)
	endif
	goto 5

20	CLOSE(gs2lun)
	break=.FALSE.
	RETURN

30	write(6,*) 'Error reading ',newid.file(1:lfile)
	if(inp_yes('Retry')) goto 2
	call exit(0)

	END

	subroutine gs3_newid(newid)

#include "gsort.inc"
	record/newid/newid

	logical*1 ripeti

	ji=newid.ind
	if(ndet(ji).GT.0) then
	  do jx=doff(ji),doff(ji)+ndet(ji)-1
	    det(jx).id=newid.lut(det(jx).id)
	  end do
	  if(newid.reorder) then
	    ripeti=.TRUE.
	    do while(ripeti)
	      ripeti=.FALSE.
	      do ii=doff(ji),doff(ji)+ndet(ji)-2
	        if(det(ii).id .GT. det(ii+1).id) then
	          ripeti=.TRUE.
	          ltmp         = det(ii  ).id
	          det(ii  ).id = det(ii+1).id
	          det(ii+1).id = ltmp
	          do jj=0,TDPAR(ji)-1
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
	  endif
	endif

	return

	end

	subroutine gs4_newid

	return

	end



	subroutine gs0_ebkill(kill)

#include "gsort.inc"
	record/kill/kill

	logical*1 getmem

	synt(1)='EBKILL  D  file  [NORUN|RUN]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getind(kill.ind)
	if(kill.ind.EQ.0) call gs_syntax('Detector name expected')

	kill.file=' '
	call istr_ch(comline,kill.file)
	lfile=lengthc(kill.file)
	if(lfile.LT.1) call gs_syntax('Filename expected')

	call gs_getrunmode(kill.run)

	ibsize=MAXDET * MAXDET
	ibsize=4*ibsize
	if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for EBKILL'
	com(icmd).size=ibsize

	return

	end

	subroutine gs1_ebkill(kill)

#include "gsort.inc"
	record/kill/kill

	call gs_putind(kill.ind)

	lfile=max(1,lengthc(kill.file))
	write(list(llist+3:),'(a)') kill.file(1:lfile)

	llist=lengthc(list)+3
	call gs_putrunmode(kill.run)

	return

	end

	subroutine gs2_ebkill(kill,dat)

#include "gsort.inc"
	record/kill/kill
	record/killdat/dat
#if defined( __GFORTRAN__ )
	logical*1 inp_yes
	external inp_yes
#endif

	integer datinp(0:MAXDET-1)

	if(init_mode.EQ.0) then
	  if(kill.run) return
	else
	  if(.NOT.kill.run) return
	endif
	
	lfile=max(1,lengthc(kill.file))

24	OPEN(UNIT=gs2lun,FILE=kill.file,STATUS='OLD',READONLY,err=25)
	goto 26
25	write(6,*) 'Error reading  '//kill.file(1:lfile)
	if(inp_yes('Retry')) goto 24
	call exit(0)
26	do ii=0,MAXDET-1
	  kill.det(ii)=.FALSE.
	  do jj=0,MAXDET-1
	    dat.remove(ii,jj)=.FALSE.
	  enddo
	enddo

	if(kill.run) then
	  irun=iio.run
	else
	  irun=-1
	endif

	call gs_findrunadc(gs2lun,irun,-1,iok)
	if(iok.GT.0) then
27	  read(gs2lun,'(a)',end=28,err=28) line
	  lline=lengthn(line)
	  if(lline.GT.0) then
	    nn=istr_ia(line,datinp,MAXDET)
	    write(LU1,*)
	    write(lu1,*) 'Detectors to discard in EBKILL'
	    write(LU2,*)
	    write(lu2,*) 'Detectors to discard in EBKILL'
	    if( NSEGS(kill.ind) .le. 1 )then                    ! supposed Euroball packing
	    do ii=0,nn-1
	      if(datinp(ii).LT.0 .or. datinp(ii).GE. FOFF) then	! non valido
	        write(6,*) 'Invalid detector number',datinp(ii)
	        if(inp_yes('Retry')) goto 24
	      elseif(datinp(ii).LT.QOFF) then				! cluster
	        kill.det(datinp(ii))=.TRUE.
	        write(LU1,'(I4,'' ==>'',$)') datinp(ii)
	        write(LU2,'(I4,'' ==>'',$)') datinp(ii)
	        ll=((datinp(ii)-COFF)/CSEG)*CSEG +COFF
	        do jj=ll,ll+CSEG-1
	          dat.remove(jj+COFF,datinp(ii))=.TRUE.
	          write(LU1,'(1h+i4,$)') jj
	          write(LU2,'(1h+i4,$)') jj
	        enddo
	      elseif(datinp(ii).LT.TOFF) then				! clover
	        kill.det(datinp(ii))=.TRUE.
	        write(LU1,'(I4,'' ==>'',$)') datinp(ii)
	        write(LU2,'(I4,'' ==>'',$)') datinp(ii)
	        ll=((datinp(ii)-QOFF)/QSEG)*QSEG +QOFF
	        do jj=ll,ll+QSEG-1
	          dat.remove(jj,datinp(ii))=.TRUE.
	          write(LU1,'(1h+i4,$)') jj
	          write(LU2,'(1h+i4,$)') jj
	        enddo
	      elseif(datinp(ii).LT.FOFF) then				! tapered
	        kill.det(datinp(ii))=.TRUE.
	        write(LU1,'(I4)') datinp(ii)
	        write(LU2,'(I4)') datinp(ii)
	        dat.remove(datinp(ii),datinp(ii))=.TRUE.
	      endif
	    enddo
	    else
	    do ii=0,nn-1
	      if(datinp(ii).LT.0 .or. datinp(ii).GE. FOFF) then	! non valido
	        write(6,*) 'Invalid detector number',datinp(ii)
	        if(inp_yes('Retry')) goto 24
	      else				! clover
	        kill.det(datinp(ii))=.TRUE.
	        write(LU1,'(I4,'' ==>'',$)') datinp(ii)
	        write(LU2,'(I4,'' ==>'',$)') datinp(ii)
	        ll=(datinp(ii)/NSEGS(kill.ind))*NSEGS(kill.ind) 
	        do jj=ll,ll+NSEGS(kill.ind)-1
	          dat.remove(jj,datinp(ii))=.TRUE.
	          write(LU1,'(1h+i4,$)') jj
	          write(LU2,'(1h+i4,$)') jj
	        enddo
	       endif
	    enddo
	    endif
	    goto 27
	  endif
	endif

28	close(unit=gs2lun)

	return

	END

	subroutine gs3_ebkill(kill,dat)

#include "gsort.inc"
	record/kill/kill
	record/killdat/dat

	ji=kill.ind
	nn=0
	do ii=doff(ji),doff(ji)+ndet(ji)-1
	  idii=det(ii).id
	  if(idii.GE.0 .and. kill.det(idii)) then
	    do jj=doff(ji),doff(ji)+ndet(ji)-1
	      idjj=det(jj).id
	      if(idjj.GE.0 .AND. dat.remove(idjj,idii)) then
	        det(jj).id=-1
	        nn=nn+1
	      endif
	    enddo
	  endif
	enddo
	if(nn.GT.0)  call gs_checkdetector(ji)

	return

	end

	subroutine gs4_ebkill

	return

	end


	subroutine gs0_combine(add)

#include "gsort.inc"
	record/add/add

	synt(1)='COMBINE   P1 P2 P3  LIMIT chan [delta]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	do nn=1,3
	  call gs_getindpar(ADD.ind(nn),ADD.par(nn),ires)
	end do
	if( (ADD.ind(2).ne.ADD.ind(1)) .or.
	1   (ADD.ind(3).ne.ADD.ind(1)) )
	1	call gs_syntax('All parameters must be in the same detector')
	iin=lengthn(comline)
	if(iin.GE.0) call gs_syntax('LIMIT chan  expected')
	if(iin.LT.0) then
	  if(comline(1:1).EQ.'L' .OR. comline(1:1).EQ.'l') then
	    call istr_ch(comline,comsort)
	    iin=lengthn(comline)
	    if(iin.LE.0) call gs_syntax('Missing  chan  value')
	    ii=istr_i2(comline,add.ichan,add.delta)
	    if(ii.eq.1) add.delta=min(100,add.ichan/10)
	  else
	    call gs_syntax('LIMIT chan  expected')
	  endif
	endif

	return

	end

	subroutine gs1_combine(add)

#include "gsort.inc"
	record/add/add

	do nn=1,3
	  call gs_putindpar(ADD.ind(nn),ADD.par(nn))
	  llist=llist+3
	end do

	write(list(llist:),'('' LIMIT'',2i8)') add.ichan,add.delta

	return

	end

	subroutine gs2_combine(add)

#include "gsort.inc"
	record/add/add

	add.slide=0

	return

	end

	subroutine gs3_combine(add)

#include "gsort.inc"
	record/add/add

	ji =add.ind(1)
	jn1=add.par(1)
	jn2=add.par(2)
	jn3=add.par(3)

	add.slide=add.slide+1
	if(add.slide .GT. add.delta) add.slide=-add.delta
	ilimit=add.ichan+add.slide
	llimit=ilimit - add.delta

	do ii=doff(ji),doff(ji)+ndet(ji)-1
	  id  =det(ii).id
	  ival=det(ii).ival(jn1)
	  if(ival.GT.0 .AND. ival.LE.ilimit) then
	    det(ii).ival(jn3)=ival
	    det(ii).xval(jn3)=det(ii).xval(jn1)
	  elseif(det(ii).ival(jn2) .GE. llimit) then
	    det(ii).ival(jn3)=det(ii).ival(jn2)
	    det(ii).xval(jn3)=det(ii).xval(jn2)
	  else
	    det(ii).ival(jn3)=0
	    det(ii).xval(jn3)=0.
	  endif
c	  write(6,'(4i8)') det(ii).id,det(ii).ival(jn1),det(ii).ival(jn2),det(ii).ival(jn3)
	end do

	return

	end

	subroutine gs4_combine

	return

	end



	subroutine gs0_addback(abck)

#include "gsort.inc"
	record/addback/abck

	synt(1)='ADDBACK Dn  [Pattern Dm | Keep2 | Tgate Dm tval]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(abck.ind,abck.par,ires)
	if(abck.ind .EQ. 0) call gs_syntax('Not valid for header parameters')
	if(NSEGS(abck.ind) .le. 1) then  	! not defined as a composite
	  abck.nsegs=-1				! assumes EB packed C(15,7),Q(26,4),T(rest,1)
	elseif( (NSEGS(abck.ind) .EQ. QSEG) .OR.
	1       (NSEGS(abck.ind) .EQ. CSEG) ) then
	  abck.nsegs=NSEGS(abck.ind)
	else
	  call gs_syntax1('Addback not yet inplemented for #Segments=',NSEGS(abck.ind))
	endif

	abck.pattern=.FALSE.
	abck.reject =.TRUE.
	abck.tgate  =.FALSE.
	if(lengthn(comline).LT.0) then
	  call istr_ch2up(comline,comsort)
	  if(comsort(1:1).EQ.'P') then
	    call gs_getindpar(ind,abck.patpar,ires)
	    if(ind.NE.abck.ind) call gs_syntax('Must be in the same detector')
	    if(abck.patpar.LT.0 .OR.
	1      abck.patpar.GE.TDPAR(abck.ind) .OR.
	2      abck.patpar.EQ.abck.par) call gs_syntax('Illegal parameter value')
	    abck.pattern=.TRUE.
	  elseif(comsort(1:1).EQ.'K') then
	    abck.reject=.FALSE.
	  elseif(comsort(1:1).EQ.'T') then
	    call gs_getindpar(ind,abck.tpar,ires)
	    if(ind.NE.abck.ind) call gs_syntax('Must be in the same detector')
	    if(abck.tpar.LT.0 .OR.
	1      abck.tpar.GE.TDPAR(abck.ind) .OR.
	2      abck.tpar.EQ.abck.par) call gs_syntax('Illegal parameter value')
	    nn=istr_i1(comline,abck.tval)
	    if(nn.ne.1) call gs_syntax('Valid range for time difference expected')
	    abck.tgate=.TRUE.
	  else
	    call gs_syntax('Pattern Dm  or  Keep2  or  Tgate Dm tval  expected')
	  endif
	endif
	  
	return

	end

	subroutine gs1_addback(abck)

#include "gsort.inc"
	record/addback/abck

	call gs_putindpar(abck.ind,abck.par)

	if(abck.pattern) then
	  llist=lengthc(list)+4
	  write(list(llist:),'(A)') 'Pattern'
	  llist=lengthc(list)+4
	  call gs_putindpar(abck.ind,abck.patpar)
	endif

	if(.NOT.abck.reject) then
	  llist=lengthc(list)+4
	  write(list(llist:),'(A)') 'Keep2'
	endif

	if(abck.tgate) then
	  llist=lengthc(list)+4
	  write(list(llist:),'(A)') 'Tgate'
	  llist=lengthc(list)+4
	  call gs_putindpar(abck.ind,abck.tpar)
	  llist=lengthc(list)+1
	  write(list(llist:),'(i6)') abck.tval
	endif

	llist=lengthc(list)+5
	if(abck.nsegs .EQ.4) then
	  write(list(llist:),'(A)') '(Clover)'
	elseif(abck.nsegs .EQ.7) then
	  write(list(llist:),'(A)') '(Cluster)'
	elseif(abck.nsegs .EQ.-1) then
	  write(list(llist:),'(A)') '(Cluster and Clover)'
	endif

	return

	end

	subroutine gs2_addback

	return

	end

	subroutine gs3_addback(abck)

#include "gsort.inc"
	record/addback/abck
	
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
	6                        .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .FALSE./


*  LEGNARO numbering
*	logical*1 Qadd(0:3,0:3) / .FALSE.,  .TRUE., .FALSE.,  .TRUE.,
*	1                        .TRUE., .FALSE.,  .TRUE., .FALSE.,
*	2                       .FALSE.,  .TRUE., .FALSE.,  .TRUE.,
*	3                        .TRUE., .FALSE.,  .TRUE., .FALSE./

*  STRASBOURG numbering
*	logical*1 Qadd(0:3,0:3) / .FALSE.,  .FALSE., .TRUE.,  .TRUE.,
*	1                       .FALSE.,  .FALSE.,  .TRUE., .TRUE.,
*	2                        .TRUE.,  .TRUE., .FALSE.,  .FALSE.,
*	3                        .TRUE., .TRUE.,  .FALSE., .FALSE./

*  Compromise - sum everything
	logical*1 Qadd(0:3,0:3) / .FALSE.,  .TRUE.,  .TRUE.,  .TRUE.,
	1                        .TRUE., .FALSE.,  .TRUE.,  .TRUE.,
	2                        .TRUE.,  .TRUE., .FALSE.,  .TRUE.,
	3                        .TRUE.,  .TRUE.,  .TRUE., .FALSE./

*	integer nii(0:6),iss(0:6)

	logical*1 ripeti,dopat,reject,tgate,valid

	ji=abck.ind
	if(ndet(ji).LT.1) return
	
	if(ndet(ji).EQ.1 .AND. .NOT.abck.pattern) return

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

	jp=abck.par
	reject=abck.reject
	dopat=abck.pattern
	jpatpar=abck.patpar
	tgate=abck.tgate
	jtpar=abck.tpar
	jtval=abck.tval
	ii1=doff(ji)
	ii2=doff(ji)+ndet(ji)-1
	nelim=0

	ioff=0
	assign 1000 to iret
	if(abck.nsegs .EQ. 4) goto 400		! Clover  da soli
	if(abck.nsegs .EQ. 7) goto 700		! Cluster da soli

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
	  goto 410
	endif
	ii1=ii2+1
	goto iret

700	ni1=ii1		!!! Cluster
	ni2=ii1
	id=det(ii1).id
	ntt=(id-ioff)/CSEG
710	if(ni2.LT.ii2) then		! isola il prossimo clover
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

	subroutine gs4_addback

	return

	end



	subroutine gs0_break

#include "gsort.inc"

	synt(1)='BREAK'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif
	return 
	end


	subroutine gs1_break

#include "gsort.inc"

	return
	end
	
	subroutine gs2_break

#include "gsort.inc"

	return
	end

	subroutine gs3_break

#include "gsort.inc"

	killev=.true.
	stored_ev=.false.
	
	return
	end

	subroutine gs4_break

#include "gsort.inc"

	return
	end


	subroutine gs0_pairgate(pgate)

#include "gsort.inc"
	record/pairgate/pgate

	synt(1)='PAIRGATE <pair_filename> DetId Low High  [IN|OUT]  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call istr_ch(comline,pgate.filename)
	if(lengthc(pgate.filename).LT.1) call gs_syntax('Filename of pairs expected')

	call gs_getind(pgate.ind)
	call gs_get2ilim(pgate.lo,pgate.hi,0,32000)
	call gs_getinout(pgate.inside)
	call gs_getfoldgate(pgate.ind,pgate.fl,pgate.fh,pgate.same)

	return

	end

	subroutine gs1_pairgate(pgate)

#include "gsort.inc"
	record/pairgate/pgate

	call gs_putind(pgate.ind)
	
	llist=lengthc(list)+2
	write(list(llist:),'(a72)')pgate.filename
	
	llist=lengthc(list)+1
	write(list(llist+1:),'(2i6)') pgate.lo,pgate.hi

	llist=lengthc(list)+2
	call gs_putinout(pgate.inside)

	llist=lengthc(list)+2
	call gs_putfoldgate(pgate.ind,pgate.fl,pgate.fh,pgate.same)

	return

	end

	subroutine gs2_pairgate(pgate)
	
#include "gsort.inc"
	record/pairgate/pgate
	
	open(gs2lun,file=pgate.filename,status='old',err=10)
	goto 11
10	ii=lengthc(pgate.filename)	
	write(6,'(''%%%GS2$ERROR - cannot open pair file '',a)')pgate.filename(1:ii+1)
	stop
11	do while(.true.)
	  read(gs2lun,*,end=12)i,j,pgate.pval(i,j)
	enddo
12	close(gs2lun)

	return

	end


	subroutine gs3_pairgate(pgate)
	
#include "gsort.inc"
	record/pairgate/pgate
	
	NofPairs=0
	ji=pgate.ind
	
	if(pgate.inside)then
	  do ii=doff(ji),doff(ji)+ndet(ji)-1
	     do jj=ii+1,doff(ji)+ndet(ji)-1
	       kk=pgate.pval(det(ii).id,det(jj).id)
	       if(kk.le.pgate.hi .and. kk.ge.pgate.lo)NofPairs=NofPairs+1
	       if(pgate.pval(det(jj).id,det(ii).id) .ne. kk)then
	          kk=pgate.pval(det(jj).id,det(ii).id)
		  if(kk.le.pgate.hi .and. kk.ge.pgate.lo)NofPairs=NofPairs+1
	       endif
	     enddo
	  enddo
	else
	  do ii=doff(ji),doff(ji)+ndet(ji)-1
	     do jj=ii+1,doff(ji)+ndet(ji)-1
	       kk=pgate.pval(det(ii).id,det(jj).id)
	       if(kk.gt.pgate.hi .and. kk.lt.pgate.lo)NofPairs=NofPairs+1
	       if(pgate.pval(det(jj).id,det(ii).id) .ne. kk)then
	          kk=pgate.pval(det(jj).id,det(ii).id)
		  if(kk.gt.pgate.hi .and. kk.lt.pgate.lo)NofPairs=NofPairs+1
	       endif
	     enddo
	  enddo
	endif
	if(NofPairs.lt.pgate.fl .or. NofPairs.gt.pgate.fh)killev=.true.
	
	return
	end

	subroutine gs4_pairgate

	return

	end
	

********* PAIRKILL   ****************************************
	subroutine gs0_pairkill(pgate)

#include "gsort.inc"
	record/pairgate/pgate

	synt(1)='PAIRKILL <pair_filename> Pn Low High  [IN|OUT]  FOLD_GATE'
	synt(2)='         eliminate detectors according to a list of pairs' 

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call istr_ch(comline,pgate.filename)
	if(lengthc(pgate.filename).LT.1) call gs_syntax('Filename of pairs expected')

	call gs_getindpar(pgate.ind,pgate.par,pgate.res)
	call gs_get2ilim(pgate.lo,pgate.hi,0,32000)
	call gs_getinout(pgate.inside)
	call gs_getfoldgate(pgate.ind,pgate.fl,pgate.fh,pgate.same)

	return

	end

	subroutine gs1_pairkill(pgate)

#include "gsort.inc"
	record/pairgate/pgate

	call gs_putindpar(pgate.ind,pgate.par)
	
	llist=lengthc(list)+2
	write(list(llist:),'(a72)')pgate.filename
	
	llist=lengthc(list)+1
	write(list(llist+1:),'(2i6)') pgate.lo,pgate.hi

	llist=lengthc(list)+2
	call gs_putinout(pgate.inside)

	llist=lengthc(list)+2
	call gs_putfoldgate(pgate.ind,pgate.fl,pgate.fh,pgate.same)

	return

	end

	subroutine gs2_pairkill(pgate)
	
#include "gsort.inc"
	record/pairgate/pgate
	
	if( pgate.inside ) then
	    kk = 0
	else
	    kk = 1
	endif
	
	do ii = 0, MAXDET-1
	   do jj = 0, MAXDET-1
	     pgate.pval(ii,jj) = kk
	   enddo
	enddo

	if( pgate.inside ) then
	    kk = 1
	else
	    kk = 0
	endif
		
	open(gs2lun,file=pgate.filename,status='old',err=10)
	goto 11
10	ii=lengthc(pgate.filename)	
	write(6,'(''%%%GS2$ERROR - cannot open pair file '',a)')pgate.filename(1:ii+1)
	stop
11	do while(.true.)
	  read(gs2lun,*,end=12)i,j
	  pgate.pval(i,j) = kk
	  pgate.pval(j,i) = kk
	enddo
12	close(gs2lun)

	return

	end


	subroutine gs3_pairkill(pgate)
	
#include "gsort.inc"
	record/pairgate/pgate
	
	nn=0
	ji=pgate.ind
	
	do ii=doff(ji),doff(ji)+ndet(ji)-1
	   pgate.list(ii) = 0
	enddo

	do ii=doff(ji),doff(ji)+ndet(ji)-1
	  do jj=ii+1,doff(ji)+ndet(ji)-1
	     if( pgate.pval(det(ii).id,det(jj).id) .eq. 1 ) then
	        ival = det(ii).ival(pgate.par)+det(jj).ival(pgate.par)
		if( ival.le.pgate.hi .and. ival.ge.pgate.lo ) then
		   pgate.list(ii) = 1
		   pgate.list(jj) = 1
		endif
             endif
	   enddo
	 enddo
	 
	do ii=doff(ji),doff(ji)+ndet(ji)-1
	   if( pgate.list(ii) .eq. 1 ) then
	      nn = nn+1
	      det(ii).id = -1
	   endif
	enddo
	
	nn = ndet(ji) - nn

	if(nn.lt.pgate.fl .or. nn.gt.pgate.fh) then
	    killev=.true.
	    return
	else
	    if(ndet(ji) .gt. nn) call gs_checkdetector(ji)
	endif
	
	return
	end

	subroutine gs4_pairkill

	return

	end
	



*********   FILTER   ****************************************
	subroutine gs0_filter(filter)

#include "gsort.inc"
	record/filter/filter

	synt(1)='FILTER Pn Pm Spectrum|Format:Length  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(filter.ind,filter.parS,filter.res)
	ind = filter.ind
	call gs_getindpar(filter.ind,filter.parD,filter.res)
	if ( filter.ind .ne. ind)then
	   call gs_syntax(' The parameters must be in the same detector')
	   return
	endif
	if( filter.parS .eq. filter.parD )then
	   call gs_syntax(' Cannot put the result in the same parameter')
	   return
	endif

#define ErrNoValidFFL call gs_syntax('Valid FILENAME|FORMAT:LENGTH of spectrum expected')

	call istr_ch(comline,filter.file)
	if(lengthc(filter.file).LT.5) ErrNoValidFFL
	ibar  = index(filter.file,'|')
	idots = index(filter.file,':')
	if( ibar .lt. 2) ErrNoValidFFL
	if( idots .lt. 4) ErrNoValidFFL
	if( idots .le. ibar+1) ErrNoValidFFL
#undef ErrNoValidFFL
	if( (filter.file(ibar+1:ibar+1) .eq. 'r') .or. (filter.file(ibar+1:ibar+1) .eq. 'R'))then
	   filter.iform = 3
	elseif( (filter.file(ibar+1:ibar+1) .eq. 'l') .or. (filter.file(ibar+1:ibar+1) .eq. 'L'))then
	   filter.iform = 4
	else
	   call gs_syntax('Spectrum format can be R or L')
	   return
	endif
	if( lengthc(filter.file) .gt. idots+1 )then
	   call gs_syntax('Length can be in range 1-9')
	   return
	endif
	read(filter.file(idots+1:idots+1),'(i1)',err=10)filter.res
	if(filter.res .lt. 1)goto 10
	filter.res = filter.res*1024
	
	call gs_getfoldgate(filter.ind,filter.fl,filter.fh,filter.same)
	filter.initialized = .false.
	return
	
10	call gs_syntax('Length can be in range 1-9')
	return

	end

	subroutine gs1_filter(filter)

#include "gsort.inc"
	record/filter/filter

	call gs_putindpar(filter.ind,filter.parS)
	call gs_putindpar(filter.ind,filter.parD)

	write(list(llist+1:),'(a)') filter.file
	lll = index(filter.file,'|')
	do ii = lll ,80
	  filter.file(ii:ii) = ' '
	enddo

	llist=lengthc(list)+2
	call gs_putfoldgate(filter.ind,filter.fl,filter.fh,filter.same)

	return

	end

	subroutine gs2_filter(filter)

#include "gsort.inc"
	record/filter/filter
	integer kv

	if(filter.initialized)return

	do ii=0,maxres
	   filter.f(ii) = -1.0000
	enddo
	call readdatr(-1,filter.file,filter.f(0),filter.res,filter.iform,kv)
	
	if( kv .le. 0 )stop '  Cannot read FILTER spectrum'

	do ii = 0,filter.res-1
	   if ( filter.f(ii) .gt. 1.0000 )then
	      filter.f(ii) = log(filter.f(ii))
	   elseif ( filter.f(ii) .eq. 1.0000 )then
	      filter.f(ii) = 0.0000
	   else
	      filter.f(ii) = -1.0000
	   endif
	enddo
	
	filter.initialized = .true.

	return

	end

	subroutine gs3_filter(filter)

#include "gsort.inc"
	record/filter/filter

	ji=filter.ind
	js=filter.parS
	jd=filter.parD
	nelim=0

	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  ICAN=det(jx).ival(js)
	  if(ican.LT.0 .or. ican.Ge.filter.res) then
	    nelim=nelim+1
	    det(jx).id=-1
	  endif
	  if( filter.f(ican) .lt. 0.000 ) then
	    nelim=nelim+1
	    det(jx).id=-1
	  endif
	end do

	if(nelim.ne.0 .and. filter.same) goto 9000
	nx=ndet(ji)-nelim
	if(nx.LT.filter.fl .or. nx.GT.filter.fh) goto 9000
	if(nelim.GT.0) call gs_checkdetector(ji)

	do jx=doff(ji),doff(ji)+ndet(ji)-1
	  det(jx).xval(jd) = 0.000
	  do jy = doff(ji),doff(ji)+ndet(ji)-1
	    if( jy .ne. jx ) det(jx).xval(jd) = det(jx).xval(jd) + filter.f(det(jy).ival(js))
	  enddo
	  det(jx).ival(jd) = det(jx).xval(jd)
	end do


	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_filter

	return

	end
********************* End FILTER ***************


*********************** PRISMA stuff **************

	subroutine gs0_track_pms( pms )
	

#include "gsort.inc"
	record /pms_str/pms
	
	integer ind_r, ind_d, ind_e ii, ibsize
	logical*1 getmem

	
	
	synt(1)='TRACK_PRISMA  Fn ( TOF-A/Q) Fm (path length) Fe (IC energy) Fp(mean path in IC) (all in output) [QFACT]'
	synt(2)='              Ah (entrance angle in horizontal plane -> 2000 + Theta[deg]*100)   Ap (target-MCP path[mm])'
	synt(3)='              Px (PPAC X [mm])'
	synt(4)='              Ia Ib Ic Id [low_thr] (individual energies in the sections of the IC and lower threshold)'
	

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif
	
	call gs_getindpar(ind_r, pms.par_r,ii)
	if( ind_r .ne. 0) call gs_syntax('Fixed parameter expected here')
	
	call gs_getindpar(ind_d, pms.par_p,ii)
	if( ind_d .ne. 0) call gs_syntax('Fixed parameter expected here')
	pms.ind = ind_r
	
	if( pms.par_r .eq. pms.par_p ) call gs_syntax('All parameters must be different')
	
	call gs_getindpar(ind_e, pms.par_e,ii)
	if( ind_e .ne. 0) call gs_syntax('Fixed parameter expected here')
	
	if( pms.par_r .eq. pms.par_e ) call gs_syntax('All parameters must be different')
	if( pms.par_p .eq. pms.par_e ) call gs_syntax('All parameters must be different')

	ind_e = -10
	call gs_getindpar(ind_e, pms.par_icp,ii)
	if( ind_e .ne. 0) call gs_syntax('Fixed parameter expected here')
	
	if( pms.par_r .eq. pms.par_icp ) call gs_syntax('All parameters must be different')
	if( pms.par_p .eq. pms.par_icp ) call gs_syntax('All parameters must be different')
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
	
	pms.sin_70  = dsind(  70.0D0)
	pms.sin_110 = dsind( 110.0D0)
	pms.cos_20  = dcosd(  20.0D0)
	pms.tan_20  = dtand(20.0D0)
	
        ibsize  = 4096*8
	if(.NOT.getmem( ibsize ,com(icmd).addr ) ) Stop 'Error allocating memory for TRACK_PRISMA'
            
	com(icmd).size=ibsize
	return
	end
	
	subroutine gs1_track_pms( pms )

	
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

	subroutine gs2_track_pms( pms, dat )
	
#include "gsort.inc"
	record /pms_str/pms
	record /pms_dat/dat
	
	if( init_mode .ne. 0) return
	do ii = 1,40
	  do jj = 0,100
	    dat.R(ii,jj) = 1200.00D0
	  enddo
	enddo
	
*	open( unit=21, file='geom.dat' )
	
	return
	end

****  begin 29/03/2007

        subroutine gs3_track_pms( pms, dat )
	
#include "gsort.inc"
	record /pms_str/pms
	record /pms_dat/dat
	
	real*8 d1, d2, xx, zz, tmp, theta, gamma, x1, x2, xtmp, ztmp, etmp, ethr, alpha!, rfact
	real*8 tau, theta0, qk, r0,p1xx, p1tmp, d01, p1tq, tmp_a, dq, dtq, dqd, dtd, qfact
	real*8 k1,sh_qk, ch_qk, fqfact, zppac
	real*8 de_A, de_B, de_C, de_D, cos_gamma
	integer lastid(4), firstid(4), lastsection
	integer x_idx, theta_idx
        logical converged, keepderiv
	
	real*8 gs_sint, gs_cost
	external gs_sint, gs_cost
	

#define SINHX( x ) ( (x*x-1.00D0)/2.0D0/x )
#define COSHX( x ) ( (x*x+1.00D0)/2.0D0/x )
	
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

	dtd = 1600.0D0
	dq  = 480.0D0
	dtq = 750.0D0 - dq/2.0D0
	dqd = dtd - dtq - dq
	
	theta0 = det(doff(pms.ind_theta)).xval(pms.par_theta)
	theta_idx = theta0/100.0D0
	theta0 = (theta0-2000.0D0)/100.0D0
	p1tq = dtand(theta0)
	p1tmp = dtq*dabs( p1tq )
	
	d01 = det(doff(pms.ind_path)).xval(pms.par_path)
	tmp_a = pms.tan_20
        xx = det(doff(pms.ind_x)).xval(pms.par_x)
	x_idx = xx/10.0D0
	
        r0 = dat.R(theta_idx,x_idx)
	xx = 1000.0D0 - xx
	zppac = 3252.0D0
*	zppac = 3140.0D0
	zz = zppac + xx*0.08748866352592401D0
        
        converged = .FALSE.
	keepderiv = .FALSE.
        
        do iter = 1,25
	  qk = pms.qfact*dq/dsqrt(157.0D0*r0)
	  sh_qk = dexp(qk)
	  ch_qk = COSHX( sh_qk )
	  sh_qk = SINHX( sh_qk )

	  theta = qk*p1tq*dtq/dq*sh_qk + p1tq*ch_qk 
	  theta = datand( theta )

	  if( dabs(qk) .le. .0100D0 )then
	      p1xx = p1tmp*ch_qk  + dabs(p1tq)*dq
	  else
	      p1xx = p1tmp*ch_qk + dabs(p1tq)/qk*dq*sh_qk 
	  endif

	  if( theta .ge. 0.0D0 )then
	      r1 = 1091.72370D0 - p1xx/pms.cos_20 - (dqd+p1xx*tmp_a)*gs_sint(theta)/gs_sint(70.0D0-theta)
	  else
	      r1 = 1091.72370D0 + p1xx/pms.cos_20 - (dqd-p1xx*tmp_a)*gs_sint(theta)/gs_sint(110.0D0+theta)
	  endif

	  k1 =  r1/r0
	  alpha =  dacosd(gs_cost(theta+35.0D0)-k1*0.96592582628907D0) - theta - 35.00D0 !gs_trig_root( theta, k1 )

	  tmp = gs_sint(alpha*0.500D0)

	  r2 = 2.0705524D0*tmp*r0*gs_sint(70.00D0-theta-alpha*0.500D0)
	  gamma = alpha + theta + 30.00D0
	  
*	  tmp = ( r2 - 272.381630D0 )*gs_cost(5.0D0)
	  tmp = ( r2 - 774.291550D0 )*gs_cost(5.0D0) + 500.0D0
	  tmp = tmp + ( zppac + tmp*0.08748866352592401D0 )*dtand(-gamma+90.0D0)
	  
* 	  xtmp = gs_sint(gamma-90.0D0)/gs_sint(175.0D0-gamma)
*	  xtmp = (r2-261.5113870D0-3235.0D0*xtmp)/(1.003819837543347+0.08748866352592401D0*tmp)
	  
*	  write(*,'(5(f12.3))' ) tmp, xx, r2, r1, r0


	  if( dabs(tmp-xx) .GT. 1.0000D-1 ) then
	    if( keepderiv )then
	      r0 = r0 - qfact*(tmp-xx)
	      keepderiv = .FALSE.
	    else
	      xtmp = tmp
	      r0 = r0 + 1.00D0     
	      qk = pms.qfact*dq/dsqrt(157.0D0*r0)

	      sh_qk = dexp(qk)
	      ch_qk = COSHX( sh_qk )
	      sh_qk = SINHX( sh_qk )

	      theta = qk*p1tq*dtq/dq*sh_qk + p1tq*ch_qk 
	      theta = datand( theta )

	      if( dabs(qk) .le. .0100D0 )then
	          p1xx = p1tmp*ch_qk  + dabs(p1tq)*dq
	      else
	          p1xx = p1tmp*ch_qk + dabs(p1tq)/qk*dq*sh_qk 
	      endif

	      if( theta .ge. 0.0D0 )then
	          r1 = 1091.72370D0 - p1xx/pms.cos_20 - (dqd+p1xx*tmp_a)*gs_sint(theta)/gs_sint(70.0D0-theta)
	      else
	          r1 = 1091.72370D0 + p1xx/pms.cos_20 - (dqd-p1xx*tmp_a)*gs_sint(theta)/gs_sint(110.0D0+theta)
	      endif

	      k1 =  r1/r0
	      alpha =  dacosd(gs_cost(theta+35.0D0)-k1*0.96592582628907D0) - theta - 35.00D0 !gs_trig_root( theta, k1 )    

	      tmp = gs_sint(alpha*0.500D0)
*	      r2 = r0*dsqrt( k1*k1 + 4.00D0*tmp*( tmp - k1*gs_cost(70.00D0-theta-alpha*0.500D0)))
	      r2 = 2.0705524D0*r0*tmp*gs_sint(70.00D0-theta-alpha*0.500D0)
	      gamma = alpha + theta + 30.00D0
	  
	      tmp = ( r2 - 774.291550D0 )*gs_cost(5.0D0) + 500.0D0
	      tmp = tmp + ( zppac + tmp*0.08748866352592401D0 )*dtand(-gamma+90.0D0)

	      if( dabs( tmp - xx ) .LT. 1.000D-1 )then
	         converged = .TRUE.
	         goto 10
	      endif
	       
	      if( dabs(tmp-xtmp) .GT. 1.00D-8) then
	        qfact = (xtmp - xx )/(tmp - xtmp)
	        r0 = r0 - 1.00D0 - qfact
	        if( dabs( qfact ) .LT. 2.00D0 ) then
	           qfact = 1.00D0/(tmp - xtmp)
	           keepderiv = .TRUE.
	        endif
	      else
	        r0 = r0 + 10.00D0
	      endif
	    endif
	  else  
	    converged = .TRUE.
            goto 10
	  endif 
        enddo
                                       
10	if( .NOT.converged ) then
	  killev = .TRUE.
	  return
	endif

*  ********* geom. test *********
*  	
*	r0 = 1.097690D0*r0 - 81.7240D0
*  	     
*  ************* end geom. test *********************


	
	if( theta .ge. 0.0D0 )then
	   d1 = d01 + (dqd+p1xx*tmp_a)*pms.sin_110/gs_sint(70.0D0-theta)
	else
	   d1 = d01 + (dqd-p1xx*tmp_a)*pms.sin_70/gs_sint(110.0D0+theta)
	endif
*	d01 = d1 + (dsqrt(dq*dq + (p1xx-p1tmp)**2) + p1xx-(dtq+dq)*dabs(p1tq) + dq/gs_cost(theta0))/2.0D0
	d2 = zz*0.9961946980917455D0/gs_sint(175.0D0-gamma)


        dq = dq/12.00D0
	do iter = 1,12
	  qk = pms.qfact*dq/dsqrt(157.0D0*r0)
	  sh_qk = dexp(qk)
	  ch_qk = COSHX( sh_qk )
	  sh_qk = SINHX( sh_qk )

	  theta = qk*p1tq*dtq/dq*sh_qk + p1tq*ch_qk 

	  if( dabs(qk) .le. .0100D0 )then
	      p1xx = p1tmp*ch_qk  + dabs(p1tq)*dq
	  else
	      p1xx = p1tmp*ch_qk + dabs(p1tq)/qk*dq*sh_qk 
	  endif
	  
	  d1 = d1 + dsqrt(dq*dq + (p1xx-p1tmp)**2)
	  
	  p1tq = theta
	  p1tmp = p1xx
	enddo
	
*	write(*,*) d01, d1


* 	rfact = r2-798.555D0
*	rfact = rfact + r1-1123.935D0
* 	rfact = 1.0D0 + dexp((abs(rfact)-1000.0D0)/70.0D0)
*	r0 =r0/rfact	

	det(pms.ind).xval(pms.par_p) = d1 + d2 + r0*alpha* 0.0174532925199433  ! store the path
	det(pms.ind).ival(pms.par_p) = det(pms.ind).xval(pms.par_p)
*	  write(*,'(6(f12.3))' ) tmp, xx, r2, r1, r0, det(pms.ind).xval(pms.par_p)

	xx = det(doff(pms.ind_x)).xval(pms.par_x)
	det(doff(pms.ind_x)).xval(pms.par_x) = r0                              ! store the radius
	det(doff(pms.ind_x)).ival(pms.par_x) = det(doff(pms.ind_x)).xval(pms.par_x)
	dat.R(theta_idx, x_idx) = r0

*	    write(*,*) r0-5.0D0, xtmp-xx, det(pms.ind).xval(pms.par_p) 
        
        
	
 	det(pms.ind).xval(pms.par_r) = 0.0D0 !zz+xx*det(doff(pms.ind_x)).xval(pms.par_x) -2000.0D0
 	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)    ! drE

	det(pms.ind).xval(pms.par_e) = 0.0D0 
	det(pms.ind).ival(pms.par_e) = det(pms.ind).xval(pms.par_e)
	
	det(pms.ind).xval(pms.par_icp) = 0.0D0 
	det(pms.ind).ival(pms.par_icp) = det(pms.ind).xval(pms.par_icp)

	if( ndet( pms.ind_ic ) .lt. 1 )then
	   return
	endif
		
	zz = 0.0D0
	etmp = 0.0D0
	ethr = pms.thr
	de_A = 0.0D0
	de_B = 0.0D0
	de_C = 0.0D0
	de_D = 0.0D0
	
	ztmp = 720.0D0
	tmp = -gs_cost(gamma)/gs_sint(gamma)
	do ii = 1,4
	  firstid(ii) = (xx+ztmp*tmp)/100.00D0
	  ztmp = ztmp + 250.00D0
	  lastid(ii)= (xx+ztmp*tmp)/100.00D0
	  if( firstid(ii) .gt. lastid(ii) )then
	    itmp = firstid(ii)
	    firstid(ii) = lastid(ii)
	    lastid(ii) = itmp
	  endif
	enddo

#define ANG_TOLLERANCE 1.250D0
	
	ztmp = 720.0D0
	tmp = -gs_cost(gamma-ANG_TOLLERANCE)/gs_sint(gamma-ANG_TOLLERANCE)
	do ii = 1,4
	  iif = (xx+ztmp*tmp)/100.00D0
	  ztmp = ztmp + 250.00D0
	  iil = (xx+ztmp*tmp)/100.00D0
	  if( iif .gt. iil )then
	    itmp = iif
	    iif = iil
	    iil = itmp
	  endif
	  if( firstid(ii).gt.iif )firstid(ii) = iif
	enddo
	
	ztmp = 720.0D0
	tmp = -gs_cost(gamma+ANG_TOLLERANCE)/gs_sint(gamma+ANG_TOLLERANCE)
	do ii = 1,4
	  iif = (xx+ztmp*tmp)/100.00D0
	  ztmp = ztmp + 250.00D0
	  iil = (xx+ztmp*tmp)/100.00D0
	  if( iif .gt. iil )then
	    itmp = iif
	    iif = iil
	    iil = itmp
	  endif
	  if( lastid(ii).lt.iil )lastid(ii) = iil
	enddo
	
	tmp = 0.0D0
	lastsection = 0
	n_ic = 0
#undef ANG_TOLLERANCE
	
*	if( ndet(pms.ind_ic) .lt. 1 )return
	
		
	ixx = xx
201	ipass = 0
	irej = 0
	iacc = 0
	do ii =doff(pms.ind_ic), doff(pms.ind_ic)+ndet(pms.ind_ic)-1
	   if( det(ii).id .ge. firstid(1) .AND. det(ii).id .le. lastid(1) )then
	   if(det(ii).xval(pms.par_a) .gt. ethr)then
		if( lastsection .lt. 1 ) lastsection = 1
		iacc = iacc+1 
	   endif
	   else
	     if(det(ii).ival(pms.par_a).gt.ethr) irej = irej+1
*	     if(det(ii).ival(pms.par_a).gt.0)write(*,*) 'det A#',det(ii).id,' value',det(ii).ival(pms.par_a),' rejected',ixx,firstid(1),lastid(1)
	   endif
	   
	   if( det(ii).id .ge. firstid(2) .AND. det(ii).id .le. lastid(2) )then
	   if(det(ii).xval(pms.par_b) .gt. ethr)then
		if( lastsection .lt. 2 ) lastsection = 2
		iacc = iacc+1 
	   endif
	   else
	     if(det(ii).ival(pms.par_b).gt.ethr) irej = irej+1
*	     if(det(ii).ival(pms.par_b).gt.0)write(*,*) 'det B#',det(ii).id,' value ',det(ii).ival(pms.par_b),' rejected',ixx,firstid(2),lastid(2)
	   endif
	   
	   if( det(ii).id .ge. firstid(3) .AND. det(ii).id .le. lastid(3) )then
	   if(det(ii).xval(pms.par_c) .gt. ethr)then
		if( lastsection .lt. 3 ) lastsection = 3
		iacc = iacc+1 
	   endif
	   else
	     if(det(ii).ival(pms.par_c).gt.ethr) irej = irej+1
*	     if(det(ii).ival(pms.par_c).gt.0)write(*,*) 'det C#',det(ii).id,' value',det(ii).ival(pms.par_c),' rejected',ixx,firstid(3),lastid(3)
	   endif
	   
	   if( det(ii).id .ge. firstid(4) .AND. det(ii).id .le. lastid(4) )then
	   if(det(ii).xval(pms.par_d) .gt. ethr)then
		if( lastsection .lt. 4 ) lastsection = 4
		iacc = iacc+1 
	   endif
	   else
	     if(det(ii).ival(pms.par_d).gt.ethr) irej = irej+1
*	     if(det(ii).ival(pms.par_d).gt.0)write(*,*) 'det D#',det(ii).id,' value',det(ii).ival(pms.par_d),' rejected',ixx,firstid(4),lastid(4)
	   endif
	enddo

          if( irej .gt. 1 ) return
		
	if( lastsection .eq. 0)then
*	  killev = .true.
	  return
	endif
	
	
	do ii =doff(pms.ind_ic), doff(pms.ind_ic)+ndet(pms.ind_ic)-1
	   xtmp = det(ii).id*100.0D0 + 50.0D0
	   
	   ztmp = 845.0D0
	   if( det(ii).id .ge. firstid(1) .AND. det(ii).id .le. lastid(1) )then
	   if(det(ii).xval(pms.par_a) .gt. ethr)then
	   	zz = zz + det(ii).xval(pms.par_a)*ztmp

	   	tmp = tmp + det(ii).xval(pms.par_a)
		de_A = de_A  + det(ii).xval(pms.par_a)
		n_ic = n_ic+1
*		if( lastsection .lt. 1 ) lastsection = 1
	   endif
	   else
	     if( det(ii).id .eq. firstid(1)-1 .OR. det(ii).id .eq. lastid(1)+1 )then
	     if(det(ii).xval(pms.par_a) .gt. ethr)then
*	  write(*,'(a,3i5,2f8.2,i5,2i3)') '1',det(ii).id, firstid(1), lastid(1),xx, gamma, lastsection, firstid(lastsection), lastid(lastsection)
*	        killev = .true.
		return
	     endif
	     endif 
	   endif
	   
	   ztmp = 1095.0D0
	   if( det(ii).id .ge. firstid(2) .AND. det(ii).id .le. lastid(2) )then
	   if(det(ii).xval(pms.par_b) .gt. ethr)then
	   	zz = zz + det(ii).xval(pms.par_b)*ztmp

	   	tmp = tmp + det(ii).xval(pms.par_b)
		de_B = de_B + det(ii).xval(pms.par_b)
		n_ic = n_ic+1
*		if( lastsection .lt. 2 ) lastsection = 2
	   endif
	   else
	     if( det(ii).id .eq. firstid(2)-1 .OR. det(ii).id .eq. lastid(2)+1 )then
	     if(det(ii).xval(pms.par_b) .gt. ethr)then
*	  write(*,'(a,3i5,2f8.2,i5,2i3)') '2',det(ii).id, firstid(2), lastid(2),xx, gamma, lastsection, firstid(lastsection), lastid(lastsection)
*	        killev = .true.
		return
	     endif
	     endif 
	   endif
	   
	   ztmp = 1345.0D0
	   if( det(ii).id .ge. firstid(3) .AND. det(ii).id .le. lastid(3) )then
	   if(det(ii).xval(pms.par_c) .gt. ethr)then
	   	zz = zz + det(ii).xval(pms.par_c)*ztmp

	   	tmp = tmp + det(ii).xval(pms.par_c)
		de_C = de_C + det(ii).xval(pms.par_c)
		n_ic = n_ic+1
*		if( lastsection .lt. 3 ) lastsection = 3
	   endif
	   else
	     if( det(ii).id .eq. firstid(3)-1 .OR. det(ii).id .eq. lastid(3)+1 )then
	     if(det(ii).xval(pms.par_c) .gt. ethr)then
*	  write(*,'(a,3i5,2f8.2,i5,2i3)') '3',det(ii).id, firstid(3), lastid(3),xx, gamma, lastsection, firstid(lastsection), lastid(lastsection)
*	        killev = .true.
		return
	     endif
	     endif 
	   endif
	   
	   ztmp = 1595.0D0
	   if( det(ii).id .ge. firstid(4) .AND. det(ii).id .le. lastid(4) )then
	   if(det(ii).xval(pms.par_d) .gt. ethr)then
	   	zz = zz + det(ii).xval(pms.par_d)*ztmp

	   	tmp = tmp + det(ii).xval(pms.par_d)
		de_D = de_D + det(ii).xval(pms.par_d)
		n_ic = n_ic+1
*		if( lastsection .lt. 4 ) lastsection = 4
	   endif
	   else
	     if( det(ii).id .eq. firstid(4)-1 .OR. det(ii).id .eq. lastid(4)+1 )then
	     if(det(ii).xval(pms.par_d) .gt. ethr)then
*	  write(*,'(a,3i5,2f8.2,i5,2i3)') '4',det(ii).id, firstid(4), lastid(4),xx, gamma, lastsection, firstid(lastsection), lastid(lastsection)
*	        killev = .true.
		return
	     endif
	     endif 
	   endif
	   
	enddo
	if( tmp .lt. 1.0D-3 .OR. n_ic .lt. 1 )then
*	  killev = .true.
	  return
	else
	  etmp = tmp
	endif
	
	
*	write(*,*)
	if( killev ) return
	zz = zz/tmp
	cos_gamma = gs_cost(gamma-90.0D0)
	ztmp = (zz-720.0D0)/ cos_gamma
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
	if    ( lastsection .eq. 4 )then       ! drE
		det(pms.ind).xval(pms.par_r) =  (etmp/ztmp*250.0D0 + (- 3.0D0*de_A + etmp)/ztmp*875.0D0)/cos_gamma + 4000.0D0  
	elseif( lastsection .eq. 3 )then
 		det(pms.ind).xval(pms.par_r) =  (etmp/ztmp*125.0D0 + (- 2.0D0*de_A + etmp)/ztmp*625.0D0)/cos_gamma + 4000.0D0  
 	elseif( lastsection .eq. 2 )then
 		det(pms.ind).xval(pms.par_r) =  (- 1.0D0*de_A + etmp)/ztmp*375.0D0/cos_gamma + 4000.0D0  
 	elseif( lastsection .eq. 1 )then
 		det(pms.ind).xval(pms.par_r) =  (- 0.0D0*de_A + etmp)/ztmp*125.0D0 + 4000.0D0  
 	endif	

		
 	det(pms.ind).xval(pms.par_r) = det(pms.ind).xval(pms.par_r)/2.0D0 !zz+xx*det(doff(pms.ind_x)).xval(pms.par_x) -2000.0D0
 	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)    ! drE

	det(pms.ind).xval(pms.par_e) = etmp 
	det(pms.ind).ival(pms.par_e) = det(pms.ind).xval(pms.par_e)
	
	det(pms.ind).xval(pms.par_r) = ztmp*5.0D0 
	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)
	
	det(pms.ind).xval(pms.par_r) = etmp/de_A/5.0D0 
	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)
	
	etmp = de_A*de_A+3.0D0*de_B*de_B+5.0D0*de_C*de_C+7.0D0*de_D*de_D
	etmp = etmp*125.00D0/(de_A*de_A+de_B*de_B+de_C*de_C+de_D*de_D)

	tmp = sqrt(de_A)+3.0D0*sqrt(de_B)+5.0D0*sqrt(de_C)+7.0D0*sqrt(de_D)
	tmp = tmp*125.00D0/(sqrt(de_A)+sqrt(de_B)+sqrt(de_C)+sqrt(de_D))

	det(pms.ind).xval(pms.par_icp) = ( ztmp - (etmp+tmp-2.0D0*ztmp)/4.0D0 ) *5.00D0
	det(pms.ind).ival(pms.par_icp) = det(pms.ind).xval(pms.par_icp)

*	det(pms.ind).xval(pms.par_r) = (det(pms.ind).xval(pms.par_r)* det(pms.ind).xval(pms.par_icp))**(0.50D0)*50.0D0
	det(pms.ind).xval(pms.par_r)=(det(pms.ind).xval(pms.par_r)*(det(pms.ind).xval(pms.par_icp)-400.0D0))**(0.282830D0)*400.0D0
	if( (lastsection .gt. 2) .and. ( det(pms.ind).xval(pms.par_r) .lt. 1810.0E0 ))det(pms.ind).xval(pms.par_r) = 0
	if( (lastsection .gt. 3) .and. ( det(pms.ind).xval(pms.par_r) .lt. 2600.0E0 ))det(pms.ind).xval(pms.par_r) = 0
	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)

	det(pms.ind).xval(pms.par_icp) = de_A+de_B	
	det(pms.ind).ival(pms.par_icp) = det(pms.ind).xval(pms.par_icp)

*** common dE	
*	if( det(pms.ind).ival(pms.par_r) .lt. 1900 .or. (de_C .le.0.10E0) ) then
*		det(pms.ind).xval(pms.par_icp) = 2.260*de_A
*	elseif( (det(pms.ind).ival(pms.par_r) .ge. 1900) .and. (det(pms.ind).ival(pms.par_r) .lt. 2700) .and. (de_C .gt. 0.10E0) ) then
*		tmp = (-det(pms.ind).xval(pms.par_r) + 2700.0E0 )/800.0E0
*		det(pms.ind).xval(pms.par_icp) = de_A*(1.00E0+tmp*(1.00E0+0.30E0*sqrt(tmp)+0.10*tmp)) 
*	1	+ (det(pms.ind).xval(pms.par_r) - 1900.0E0 )/800.0E0*de_B
*	else
*		det(pms.ind).xval(pms.par_icp) = de_A+de_B
*	endif
*	det(pms.ind).ival(pms.par_icp) = det(pms.ind).xval(pms.par_icp)
***************
	return
	end

****  end 29/03/2007

        subroutine gs3_track_pms11( pms, dat )
	
#include "gsort.inc"
	record /pms_str/pms
	record /pms_dat/dat
	
	real*8 d1, d2, xx, zz, tmp, theta, gamma, x1, x2, xtmp, ztmp, etmp, ethr, alpha!, rfact
	real*8 tau, theta0, qk, r0,p1xx, p1tmp, d01, p1tq, tmp_a, dq, dtq, dqd, dtd, qfact
	real*8 k1,sh_qk, ch_qk, fqfact
	real*8 de_A, de_B, de_C, de_D, cos_gamma
	integer lastid(4), firstid(4), lastsection
	integer x_idx, theta_idx
        logical converged, keepderiv
	
	real*8 gs_sint, gs_cost
	external gs_sint, gs_cost
	

#define SINHX( x ) ( (x*x-1.00D0)/2.0D0/x )
#define COSHX( x ) ( (x*x+1.00D0)/2.0D0/x )
	
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

	dtd = 1600.0D0
	dq  = 480.0D0
	dtq = 750.0D0 - dq/2.0D0
	dqd = dtd - dtq - dq
	
	theta0 = det(doff(pms.ind_theta)).xval(pms.par_theta)
	theta_idx = theta0/100.0D0
	theta0 = (theta0-2000.0D0)/100.0D0
	p1tq = dtand(theta0)
	p1tmp = dtq*dabs( p1tq )
	
	d01 = det(doff(pms.ind_path)).xval(pms.par_path)
	tmp_a = pms.tan_20
        xx = det(doff(pms.ind_x)).xval(pms.par_x)
	x_idx = xx/10.0D0
	
        r0 = dat.R(theta_idx,x_idx)
	xx = 1035.0D0 - xx
	zz = 3235.0D0 + xx*0.08748866352592401D0
        
        converged = .FALSE.
	keepderiv = .FALSE.
        
        do iter = 1,25
	  qk = pms.qfact*dq/dsqrt(200.0D0*r0)
	  sh_qk = dexp(qk)
	  ch_qk = COSHX( sh_qk )
	  sh_qk = SINHX( sh_qk )

	  theta = qk*p1tq*dtq/dq*sh_qk + p1tq*ch_qk 
	  theta = datand( theta )

	  if( dabs(qk) .le. .0100D0 )then
	      p1xx = p1tmp*ch_qk  + dabs(p1tq)*dq
	  else
	      p1xx = p1tmp*ch_qk + dabs(p1tq)/qk*dq*sh_qk 
	  endif

	  if( theta .ge. 0.0D0 )then
	      r1 = 1123.9350D0 - p1xx/pms.cos_20 - (dqd+p1xx*tmp_a)*gs_sint(theta)/gs_sint(70.0D0-theta)
	  else
	      r1 = 1123.9350D0 + p1xx/pms.cos_20 - (dqd-p1xx*tmp_a)*gs_sint(theta)/gs_sint(110.0D0+theta)
	  endif

	  k1 =  r1/r0
	  alpha =  dacosd(gs_cost(theta+35.0D0)-k1*0.96592582628907D0) - theta - 35.00D0 !gs_trig_root( theta, k1 )

	  tmp = gs_sint(alpha*0.500D0)
*	  r2 = r0*dsqrt( k1*k1 + 4.00D0*tmp*( tmp - k1*gs_cost(70.00D0-theta-alpha*0.500D0)))
	  r2 = 2.0705524D0*tmp*r0*gs_sint(70.00D0-theta-alpha*0.500D0)
	  gamma = alpha + theta + 30.00D0
	  
	  tmp = gs_sint(gamma-90.0D0)/gs_sint(175.0D0-gamma)
	  tmp = (r2-261.5113870D0-3235.0D0*tmp)/(1.003819837543347+0.08748866352592401D0*tmp)
*	  write(*,'(i4,4f15.4)')iter,r0, alpha,xx,tmp	


	  if( dabs(tmp-xx) .GT. 1.0000D-1 ) then
	    if( keepderiv )then
	      r0 = r0 - qfact*(tmp-xx)
	      keepderiv = .FALSE.
	    else
	      xtmp = tmp
	      r0 = r0 + 1.00D0     
	      qk = pms.qfact*dq/dsqrt(200.0D0*r0)

	      sh_qk = dexp(qk)
	      ch_qk = COSHX( sh_qk )
	      sh_qk = SINHX( sh_qk )

	      theta = qk*p1tq*dtq/dq*sh_qk + p1tq*ch_qk 
	      theta = datand( theta )

	      if( dabs(qk) .le. .0100D0 )then
	          p1xx = p1tmp*ch_qk  + dabs(p1tq)*dq
	      else
	          p1xx = p1tmp*ch_qk + dabs(p1tq)/qk*dq*sh_qk 
	      endif

	      if( theta .ge. 0.0D0 )then
	          r1 = 1123.9350D0 - p1xx/pms.cos_20 - (dqd+p1xx*tmp_a)*gs_sint(theta)/gs_sint(70.0D0-theta)
	      else
	          r1 = 1123.9350D0 + p1xx/pms.cos_20 - (dqd-p1xx*tmp_a)*gs_sint(theta)/gs_sint(110.0D0+theta)
	      endif

	      k1 =  r1/r0
	      alpha =  dacosd(gs_cost(theta+35.0D0)-k1*0.96592582628907D0) - theta - 35.00D0 !gs_trig_root( theta, k1 )    

	      tmp = gs_sint(alpha*0.500D0)
*	      r2 = r0*dsqrt( k1*k1 + 4.00D0*tmp*( tmp - k1*gs_cost(70.00D0-theta-alpha*0.500D0)))
	      r2 = 2.0705524D0*r0*tmp*gs_sint(70.00D0-theta-alpha*0.500D0)
	      gamma = alpha + theta + 30.00D0
	  
	      tmp = gs_sint(gamma-90.0D0)/gs_sint(175.0D0-gamma)
	      tmp = (r2-261.5113870D0-3235.0D0*tmp)/(1.003819837543347D0+0.08748866352592401D0*tmp)
*	      write(*,*) r0-5.0D0, xtmp, xx, (tmp - xtmp)*0.200D0
	      if( dabs( tmp - xx ) .LT. 1.000D-1 )then
	         converged = .TRUE.
	         goto 10
	      endif
	       
	      if( dabs(tmp-xtmp) .GT. 1.00D-8) then
	        qfact = (xtmp - xx )/(tmp - xtmp)
	        r0 = r0 - 1.00D0 - qfact
	        if( dabs( qfact ) .LT. 2.00D0 ) then
	           qfact = 1.00D0/(tmp - xtmp)
	           keepderiv = .TRUE.
	        endif
	      else
	        r0 = r0 + 10.00D0
	      endif
	    endif
	  else  
	    converged = .TRUE.
            goto 10
	  endif 
        enddo
                                       
10	if( .NOT.converged ) then
	  killev = .TRUE.
	  return
	endif

*  ********* geom. test *********
*  	
*	r0 = 1.097690D0*r0 - 81.7240D0
*  	     
*  ************* end geom. test *********************


	
	if( theta .ge. 0.0D0 )then
	   d1 = d01 + (dqd+p1xx*tmp_a)*pms.sin_110/gs_sint(70.0D0-theta)
	else
	   d1 = d01 + (dqd-p1xx*tmp_a)*pms.sin_70/gs_sint(110.0D0+theta)
	endif
*	d01 = d1 + (dsqrt(dq*dq + (p1xx-p1tmp)**2) + p1xx-(dtq+dq)*dabs(p1tq) + dq/gs_cost(theta0))/2.0D0
	d2 = zz*0.9961946980917455D0/gs_sint(175.0D0-gamma)


        dq = dq/12.00D0
	do iter = 1,12
	  qk = pms.qfact*dq/dsqrt(200.0D0*r0)
	  sh_qk = dexp(qk)
	  ch_qk = COSHX( sh_qk )
	  sh_qk = SINHX( sh_qk )

	  theta = qk*p1tq*dtq/dq*sh_qk + p1tq*ch_qk 

	  if( dabs(qk) .le. .0100D0 )then
	      p1xx = p1tmp*ch_qk  + dabs(p1tq)*dq
	  else
	      p1xx = p1tmp*ch_qk + dabs(p1tq)/qk*dq*sh_qk 
	  endif
	  
	  d1 = d1 + dsqrt(dq*dq + (p1xx-p1tmp)**2)
	  
	  p1tq = theta
	  p1tmp = p1xx
	enddo
	
*	write(*,*) d01, d1


* 	rfact = r2-798.555D0
*	rfact = rfact + r1-1123.935D0
* 	rfact = 1.0D0 + dexp((abs(rfact)-1000.0D0)/70.0D0)
*	r0 =r0/rfact	

	det(pms.ind).xval(pms.par_p) = d1 + d2 + r0*alpha* 0.0174532925199433  ! store the path
	det(pms.ind).ival(pms.par_p) = det(pms.ind).xval(pms.par_p)

	xx = det(doff(pms.ind_x)).xval(pms.par_x)
	det(doff(pms.ind_x)).xval(pms.par_x) = r0                              ! store the radius
	det(doff(pms.ind_x)).ival(pms.par_x) = det(doff(pms.ind_x)).xval(pms.par_x)
	dat.R(theta_idx, x_idx) = r0

*	    write(*,*) r0-5.0D0, xtmp-xx, det(pms.ind).xval(pms.par_p) 
        
        
	
 	det(pms.ind).xval(pms.par_r) = 0.0D0 !zz+xx*det(doff(pms.ind_x)).xval(pms.par_x) -2000.0D0
 	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)    ! drE

	det(pms.ind).xval(pms.par_e) = 0.0D0 
	det(pms.ind).ival(pms.par_e) = det(pms.ind).xval(pms.par_e)
	
	det(pms.ind).xval(pms.par_icp) = 0.0D0 
	det(pms.ind).ival(pms.par_icp) = det(pms.ind).xval(pms.par_icp)

	if( ndet( pms.ind_ic ) .lt. 1 )then
	   return
	endif
		
	zz = 0.0D0
	etmp = 0.0D0
	ethr = pms.thr
	de_A = 0.0D0
	de_B = 0.0D0
	de_C = 0.0D0
	de_D = 0.0D0
	
	ztmp = 720.0D0
	tmp = -gs_cost(gamma)/gs_sint(gamma)
	do ii = 1,4
	  firstid(ii) = (xx+ztmp*tmp)/100.00D0
	  ztmp = ztmp + 250.00D0
	  lastid(ii)= (xx+ztmp*tmp)/100.00D0
	  if( firstid(ii) .gt. lastid(ii) )then
	    itmp = firstid(ii)
	    firstid(ii) = lastid(ii)
	    lastid(ii) = itmp
	  endif
	enddo

#define ANG_TOLLERANCE 1.250D0
	
	ztmp = 720.0D0
	tmp = -gs_cost(gamma-ANG_TOLLERANCE)/gs_sint(gamma-ANG_TOLLERANCE)
	do ii = 1,4
	  iif = (xx+ztmp*tmp)/100.00D0
	  ztmp = ztmp + 250.00D0
	  iil = (xx+ztmp*tmp)/100.00D0
	  if( iif .gt. iil )then
	    itmp = iif
	    iif = iil
	    iil = itmp
	  endif
	  if( firstid(ii).gt.iif )firstid(ii) = iif
	enddo
	
	ztmp = 720.0D0
	tmp = -gs_cost(gamma+ANG_TOLLERANCE)/gs_sint(gamma+ANG_TOLLERANCE)
	do ii = 1,4
	  iif = (xx+ztmp*tmp)/100.00D0
	  ztmp = ztmp + 250.00D0
	  iil = (xx+ztmp*tmp)/100.00D0
	  if( iif .gt. iil )then
	    itmp = iif
	    iif = iil
	    iil = itmp
	  endif
	  if( lastid(ii).lt.iil )lastid(ii) = iil
	enddo
	
	tmp = 0.0D0
	lastsection = 0
	n_ic = 0
#undef ANG_TOLLERANCE
	
*	if( ndet(pms.ind_ic) .lt. 1 )return
	
		
	ixx = xx
201	ipass = 0
	irej = 0
	iacc = 0
	do ii =doff(pms.ind_ic), doff(pms.ind_ic)+ndet(pms.ind_ic)-1
	   if( det(ii).id .ge. firstid(1) .AND. det(ii).id .le. lastid(1) )then
	   if(det(ii).xval(pms.par_a) .gt. ethr)then
		if( lastsection .lt. 1 ) lastsection = 1
		iacc = iacc+1 
	   endif
	   else
	     if(det(ii).ival(pms.par_a).gt.ethr) irej = irej+1
*	     if(det(ii).ival(pms.par_a).gt.0)write(*,*) 'det A#',det(ii).id,' value',det(ii).ival(pms.par_a),' rejected',ixx,firstid(1),lastid(1)
	   endif
	   
	   if( det(ii).id .ge. firstid(2) .AND. det(ii).id .le. lastid(2) )then
	   if(det(ii).xval(pms.par_b) .gt. ethr)then
		if( lastsection .lt. 2 ) lastsection = 2
		iacc = iacc+1 
	   endif
	   else
	     if(det(ii).ival(pms.par_b).gt.ethr) irej = irej+1
*	     if(det(ii).ival(pms.par_b).gt.0)write(*,*) 'det B#',det(ii).id,' value ',det(ii).ival(pms.par_b),' rejected',ixx,firstid(2),lastid(2)
	   endif
	   
	   if( det(ii).id .ge. firstid(3) .AND. det(ii).id .le. lastid(3) )then
	   if(det(ii).xval(pms.par_c) .gt. ethr)then
		if( lastsection .lt. 3 ) lastsection = 3
		iacc = iacc+1 
	   endif
	   else
	     if(det(ii).ival(pms.par_c).gt.ethr) irej = irej+1
*	     if(det(ii).ival(pms.par_c).gt.0)write(*,*) 'det C#',det(ii).id,' value',det(ii).ival(pms.par_c),' rejected',ixx,firstid(3),lastid(3)
	   endif
	   
	   if( det(ii).id .ge. firstid(4) .AND. det(ii).id .le. lastid(4) )then
	   if(det(ii).xval(pms.par_d) .gt. ethr)then
		if( lastsection .lt. 4 ) lastsection = 4
		iacc = iacc+1 
	   endif
	   else
	     if(det(ii).ival(pms.par_d).gt.ethr) irej = irej+1
*	     if(det(ii).ival(pms.par_d).gt.0)write(*,*) 'det D#',det(ii).id,' value',det(ii).ival(pms.par_d),' rejected',ixx,firstid(4),lastid(4)
	   endif
	enddo

          if( irej .gt. 1 ) return
	
*       if( ipass .eq. 0)then
*          ipass = 1
*          if( irej .gt. iacc+1)then
*            do ii = 1,4
*	  	    firstid(ii) = firstid(ii)-1
*	  	    if( firstid(ii) .lt. 0 )firstid(ii) = 0
*	  	    lastid(ii) = lastid(ii)+1
*	  	    if( lastid(ii) .gt. 9 )lastid(ii) = 9
*            enddo
*	    goto 201
*          endif
*       else
*          if( irej .gt. 1 ) return
*       endif

*	write(*,*)'accepted',iacc,' rejected',irej
	
	if( lastsection .eq. 0)then
*	  killev = .true.
	  return
	endif
	
	
	do ii =doff(pms.ind_ic), doff(pms.ind_ic)+ndet(pms.ind_ic)-1
	   xtmp = det(ii).id*100.0D0 + 50.0D0
	   
	   ztmp = 845.0D0
	   if( det(ii).id .ge. firstid(1) .AND. det(ii).id .le. lastid(1) )then
	   if(det(ii).xval(pms.par_a) .gt. ethr)then
	   	zz = zz + det(ii).xval(pms.par_a)*ztmp

	   	tmp = tmp + det(ii).xval(pms.par_a)
		de_A = de_A  + det(ii).xval(pms.par_a)
		n_ic = n_ic+1
*		if( lastsection .lt. 1 ) lastsection = 1
	   endif
	   else
	     if( det(ii).id .eq. firstid(1)-1 .OR. det(ii).id .eq. lastid(1)+1 )then
	     if(det(ii).xval(pms.par_a) .gt. ethr)then
*	  write(*,'(a,3i5,2f8.2,i5,2i3)') '1',det(ii).id, firstid(1), lastid(1),xx, gamma, lastsection, firstid(lastsection), lastid(lastsection)
*	        killev = .true.
		return
	     endif
	     endif 
	   endif
	   
	   ztmp = 1095.0D0
	   if( det(ii).id .ge. firstid(2) .AND. det(ii).id .le. lastid(2) )then
	   if(det(ii).xval(pms.par_b) .gt. ethr)then
	   	zz = zz + det(ii).xval(pms.par_b)*ztmp

	   	tmp = tmp + det(ii).xval(pms.par_b)
		de_B = de_B + det(ii).xval(pms.par_b)
		n_ic = n_ic+1
*		if( lastsection .lt. 2 ) lastsection = 2
	   endif
	   else
	     if( det(ii).id .eq. firstid(2)-1 .OR. det(ii).id .eq. lastid(2)+1 )then
	     if(det(ii).xval(pms.par_b) .gt. ethr)then
*	  write(*,'(a,3i5,2f8.2,i5,2i3)') '2',det(ii).id, firstid(2), lastid(2),xx, gamma, lastsection, firstid(lastsection), lastid(lastsection)
*	        killev = .true.
		return
	     endif
	     endif 
	   endif
	   
	   ztmp = 1345.0D0
	   if( det(ii).id .ge. firstid(3) .AND. det(ii).id .le. lastid(3) )then
	   if(det(ii).xval(pms.par_c) .gt. ethr)then
	   	zz = zz + det(ii).xval(pms.par_c)*ztmp

	   	tmp = tmp + det(ii).xval(pms.par_c)
		de_C = de_C + det(ii).xval(pms.par_c)
		n_ic = n_ic+1
*		if( lastsection .lt. 3 ) lastsection = 3
	   endif
	   else
	     if( det(ii).id .eq. firstid(3)-1 .OR. det(ii).id .eq. lastid(3)+1 )then
	     if(det(ii).xval(pms.par_c) .gt. ethr)then
*	  write(*,'(a,3i5,2f8.2,i5,2i3)') '3',det(ii).id, firstid(3), lastid(3),xx, gamma, lastsection, firstid(lastsection), lastid(lastsection)
*	        killev = .true.
		return
	     endif
	     endif 
	   endif
	   
	   ztmp = 1595.0D0
	   if( det(ii).id .ge. firstid(4) .AND. det(ii).id .le. lastid(4) )then
	   if(det(ii).xval(pms.par_d) .gt. ethr)then
	   	zz = zz + det(ii).xval(pms.par_d)*ztmp

	   	tmp = tmp + det(ii).xval(pms.par_d)
		de_D = de_D + det(ii).xval(pms.par_d)
		n_ic = n_ic+1
*		if( lastsection .lt. 4 ) lastsection = 4
	   endif
	   else
	     if( det(ii).id .eq. firstid(4)-1 .OR. det(ii).id .eq. lastid(4)+1 )then
	     if(det(ii).xval(pms.par_d) .gt. ethr)then
*	  write(*,'(a,3i5,2f8.2,i5,2i3)') '4',det(ii).id, firstid(4), lastid(4),xx, gamma, lastsection, firstid(lastsection), lastid(lastsection)
*	        killev = .true.
		return
	     endif
	     endif 
	   endif
	   
	enddo
	if( tmp .lt. 1.0D-3 .OR. n_ic .lt. 1 )then
*	  killev = .true.
	  return
	else
	  etmp = tmp
	endif
	
	
*	write(*,*)
	if( killev ) return
	zz = zz/tmp
	cos_gamma = gs_cost(gamma-90.0D0)
	ztmp = (zz-720.0D0)/ cos_gamma
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
	if    ( lastsection .eq. 4 )then       ! drE
		det(pms.ind).xval(pms.par_r) =  (etmp/ztmp*250.0D0 + (- 3.0D0*de_A + etmp)/ztmp*875.0D0)/cos_gamma + 4000.0D0  
	elseif( lastsection .eq. 3 )then
 		det(pms.ind).xval(pms.par_r) =  (etmp/ztmp*125.0D0 + (- 2.0D0*de_A + etmp)/ztmp*625.0D0)/cos_gamma + 4000.0D0  
 	elseif( lastsection .eq. 2 )then
 		det(pms.ind).xval(pms.par_r) =  (- 1.0D0*de_A + etmp)/ztmp*375.0D0/cos_gamma + 4000.0D0  
 	elseif( lastsection .eq. 1 )then
 		det(pms.ind).xval(pms.par_r) =  (- 0.0D0*de_A + etmp)/ztmp*125.0D0 + 4000.0D0  
 	endif	

		
 	det(pms.ind).xval(pms.par_r) = det(pms.ind).xval(pms.par_r)/2.0D0 !zz+xx*det(doff(pms.ind_x)).xval(pms.par_x) -2000.0D0
 	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)    ! drE

	det(pms.ind).xval(pms.par_e) = etmp 
	det(pms.ind).ival(pms.par_e) = det(pms.ind).xval(pms.par_e)
	
	det(pms.ind).xval(pms.par_r) = ztmp*5.0D0 
	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)
	
	det(pms.ind).xval(pms.par_r) = etmp/de_A/5.0D0 
	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)
	
	etmp = de_A*de_A+3.0D0*de_B*de_B+5.0D0*de_C*de_C+7.0D0*de_D*de_D
	etmp = etmp*125.00D0/(de_A*de_A+de_B*de_B+de_C*de_C+de_D*de_D)

	tmp = sqrt(de_A)+3.0D0*sqrt(de_B)+5.0D0*sqrt(de_C)+7.0D0*sqrt(de_D)
	tmp = tmp*125.00D0/(sqrt(de_A)+sqrt(de_B)+sqrt(de_C)+sqrt(de_D))

	det(pms.ind).xval(pms.par_icp) = ( ztmp - (etmp+tmp-2.0D0*ztmp)/4.0D0 ) *5.00D0
	det(pms.ind).ival(pms.par_icp) = det(pms.ind).xval(pms.par_icp)

*	det(pms.ind).xval(pms.par_r) = (det(pms.ind).xval(pms.par_r)* det(pms.ind).xval(pms.par_icp))**(0.50D0)*50.0D0
	det(pms.ind).xval(pms.par_r)=(det(pms.ind).xval(pms.par_r)*(det(pms.ind).xval(pms.par_icp)-400.0D0))**(0.282830D0)*400.0D0
	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)

	return
	end

	subroutine gs3_track_pms_old( pms )
	
#include "gsort.inc"
	record /pms_str/pms
	
	real*8 d1, d2, xx, zz, tmp, theta, gamma, x1, x2, xtmp, ztmp, etmp, ethr, alpha!, rfact
	real*8 tau, theta0, qk, r0,p1xx, p1tmp, d01, p1tq, tmp_a, dq, dtq, dqd, dtd, qfact
	real*8 theta_quad
	real*8 de_A, de_B, de_C, de_D, cos_gamma
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
	de_A = 0.0D0
	de_B = 0.0D0
	de_C = 0.0D0
	de_D = 0.0D0
	
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
		de_A = de_A  + det(ii).xval(pms.par_a)
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
		de_B = de_B + det(ii).xval(pms.par_b)
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
		de_C = de_C + det(ii).xval(pms.par_c)
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
		de_D = de_D + det(ii).xval(pms.par_d)
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
		
	if( firstid(2) .ge. 0)then
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
	
	if( firstid(3) .ge. 0)then
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
	
	if( firstid(4) .ge. 0)then
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
	cos_gamma = dcosd(gamma-90.0D0)
	ztmp = (zz-720.0D0)/ cos_gamma

*	d2 = 3235.0D0/dcosd(gamma-90.0D0)
	tmp = 1035.0D0 - tmp
	zz = 3235.0D0 + tmp*0.08748866352592401D0
	d2 = zz*0.9961946980917455D0/dsind(175.0D0-gamma)
* 	r2 = 261.5113870D0 + tmp*1.003819837543347 + zz*dsind(gamma-90.0D0)/dsind(175.0D0-gamma)
 	r2 = 261.5113870D0 + tmp*1.003819837543347 + zz*dsind(gamma-90.0D0)/dsind(175.0D0-gamma)
	
	if( r2 .lt. 261.60D0 .or. r2 .gt. 1261.0D0 ) then
	  killev = .true.
	  return
	endif
	
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
	    d1 = dtd*pms.sin_110/dsind(70.0D0-theta0) 
	    r1 = 1123.9350D0 - dtd*dsind(theta0)/dsind(70.0D0-theta0)
	else
	    d1 = dtd*pms.sin_70/dsind(110.0D0+theta0) 
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
**	d01 = dtq/dcosd(theta0)
	d01 = det(doff(pms.ind_path)).xval(pms.par_path)
	
	tmp_a = pms.tan_20
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
	    d1 = d01 + (dqd+p1xx*tmp_a)*pms.sin_110/dsind(70.0D0-theta)
	    r1 = 1123.9350D0 - p1xx/pms.cos_20 - (dqd+p1xx*tmp_a)*dsind(theta)/dsind(70.0D0-theta)
	else
	    d1 = d01 + (dqd-p1xx*tmp_a)*pms.sin_70/dsind(110.0D0+theta)
	    r1 = 1123.9350D0 + p1xx/pms.cos_20 - (dqd-p1xx*tmp_a)*dsind(theta)/dsind(110.0D0+theta)
	endif
       d1 = d1 + (dsqrt(dq*dq + (p1xx-p1tmp)**2) + p1xx-(dtq+dq)*dabs(p1tq) + dq/dcosd(theta0))/2.0D0

**	d1 = d1-det(doff(pms.ind_path)).xval(pms.par_path)
	
	alpha = gamma - 35.0D0 - theta !*(1.00D0+theta_quad) 
*	alpha = gamma - theta - 35.0D0
	
	tmp = dsqrt( (r1*r1 + r2*r2 - 0.5176380902050415D0*r1*r2)/2.0D0/(1.0D0-dcosd(alpha)))
*       write(*,'(7F12.3)')theta0, theta, alpha, tmp, d1, r1 ,p1xx
	
	if( r1 .lt. 798.60D0 .or. r1 .gt. 1768.0D0 )then
	   if( iter .eq. 1 .or. iter .ge. 15 ) then
	      killev = .TRUE.
	      return
	   endif
	   tmp = 0.75000D0*tmp
	endif
	   
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

! 	rfact = 1.0D0 + dexp((abs(rfact)-1000.0D0)/77.0D0)	
 	rfact = 1.0D0 + dexp((abs(rfact)-1000.0D0)/70.0D0)	

! 	xx = det(doff(pms.ind_theta)).xval(pms.par_theta)
! 	zz =  9031.7 -13.502*xx +0.010306*xx*xx -3.5314D-06*xx*xx*xx +4.4847D-10*xx*xx*xx*xx
! 	xx = -9.9674 +0.02149*xx -1.6428D-05*xx*xx +5.5329D-09*xx*xx*xx -6.8563D-13*xx*xx*xx*xx

	if( firstid(4) .ge. 0 )then       ! drE
		det(pms.ind).xval(pms.par_r) =  (etmp/ztmp*250.0D0 + (- 3.0D0*de_A + etmp)/ztmp*875.0D0)/cos_gamma + 4000.0D0  
		elseif( firstid(3) .ge. 0 )then
 		det(pms.ind).xval(pms.par_r) =  (etmp/ztmp*125.0D0 + (- 2.0D0*de_A + etmp)/ztmp*625.0D0)/cos_gamma + 4000.0D0  
 	elseif( firstid(2) .ge. 0 )then
 		det(pms.ind).xval(pms.par_r) =  (- 1.0D0*de_A + etmp)/ztmp*375.0D0/cos_gamma + 4000.0D0  
 	elseif( firstid(1) .ge. 0 )then
 		det(pms.ind).xval(pms.par_r) =  (- 0.0D0*de_A + etmp)/ztmp*125.0D0 + 4000.0D0  
 	endif	

!  	if( firstid(4) .ge. 0 )then
!  		det(pms.ind).xval(pms.par_r) =  (de_A + de_B + de_C)*cos_gamma/3.0 
!  	elseif( firstid(3) .ge. 0 )then
!  		det(pms.ind).xval(pms.par_r) =  (de_A + de_B )*cos_gamma/2.0 
! 	elseif( firstid(2) .ge. 0 )then
! 		det(pms.ind).xval(pms.par_r) =  de_A *cos_gamma
! 	else
! 		det(pms.ind).xval(pms.par_r) =  0.0  
! 	endif	
		
 	det(pms.ind).xval(pms.par_r) = det(pms.ind).xval(pms.par_r)/2.0D0 !zz+xx*det(doff(pms.ind_x)).xval(pms.par_x) -2000.0D0
 	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)    ! drE
	
!	det(pms.ind).ival(pms.par_r) = det(pms.ind).xval(pms.par_r)

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
	
	subroutine gs4_track_pms( pms )
	
#include "gsort.inc"
	record /pms_str/pms
	
	return
	end

	
	subroutine gs0_qvalue_pms( qvalue )
	

#include "gsort.inc"
	record /qvalue_str/qvalue
	
	integer ii

	
	
	synt(1)='QVALUE_PRISMA Fq(output) Offset Gain Wl Wh '
	synt(2)='              Ap(proj. mass)      At(target mass)    Ep(proj. energy[MeV]) Theta_PRISMA[deg]'
	synt(3)='              Pm (ejectile mass)  Pa (polar THETA)   Pb (polar PHI)        Pc (v/c[%])'
	

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif
	
	call gs_getindpar(qvalue.ind_q, qvalue.par_q,ii)
	if( qvalue.ind_q .ne. 0) call gs_syntax('Fixed parameter expected here')
        
	if( istr_r2(comline, qvalue.offset, qvalue.gain) .ne. 2 ) then
	   call gs_syntax('Offset and Gain expected here')
	endif
	
	if( istr_i2(comline, qvalue.low, qvalue.high) .ne. 2 ) then
	   call gs_syntax('Output limits (low high) expected here')
	endif
	call gs_readline
	if(lcomline.LT.1) call gs_syntax('Incomplete command, reaction & PRISMA parameters expected here')

	if( istr_i2(comline, qvalue.AP, qvalue.AT) .ne. 2 ) then
	   call gs_syntax('Projectile and target masses expected here')
	endif
	if( istr_r2(comline, qvalue.ep, qvalue.thp) .ne. 2 ) then
	   call gs_syntax('Projectile energy[MeV] and PRISMA angle[deg] expected here')
	endif
	call gs_readline
	if(lcomline.LT.1) call gs_syntax('Incomplete command, parameters for the detected ion expected here')
	
	call gs_getindpar(qvalue.ind_mass, qvalue.par_mass,ii)
	call gs_getindpar(qvalue.ind_theta, qvalue.par_theta,ii)
	call gs_getindpar(qvalue.ind_phi, qvalue.par_phi,ii)
	call gs_getindpar(qvalue.ind_beta, qvalue.par_beta,ii)

	qvalue.costhp = qvalue.thp
	qvalue.sinthp = qvalue.costhp
	qvalue.costhp = dcosd(qvalue.costhp)	
	qvalue.sinthp = dsind(qvalue.sinthp)
	qvalue.amu = 931.494320D0*5.00D-5
	qvalue.ATOT = qvalue.AP + qvalue.AT
	
	return
	end

	subroutine gs1_qvalue_pms( qvalue )
	

#include "gsort.inc"
	record /qvalue_str/qvalue
	
	call gs_putindpar( qvalue.ind_q, qvalue.par_q )	
	llist = llist+3
	write(list(llist+1:), '(2F8.3,2I8)')qvalue.offset,qvalue.gain,qvalue.low,qvalue.high
	call gs_writeline( LLUN, list, INDENT )
	
	llist = INDENT
	write(list(llist+1:), '(2I8,2F10.3)')qvalue.AP,qvalue.AT,qvalue.EP,qvalue.THP
	call gs_writeline( LLUN, list, INDENT )
	
	llist = INDENT
	call gs_putindpar( qvalue.ind_mass, qvalue.par_mass )	
	llist = llist+3
	call gs_putindpar( qvalue.ind_theta, qvalue.par_theta )	
	llist = llist+3
	call gs_putindpar( qvalue.ind_phi, qvalue.par_phi )	
	llist = llist+3
	call gs_putindpar( qvalue.ind_beta, qvalue.par_beta )	
	call gs_writeline( LLUN, list, INDENT )
	
	return
	end
	
	subroutine gs2_qvalue_pms( qvalue )
	
#include "gsort.inc"
	record /qvalue_str/qvalue
	
	return
	end
	
	subroutine gs3_qvalue_pms( qvalue )
	
#include "gsort.inc"
	record /qvalue_str/qvalue
	
	real*8 A1, A3, A4, E1, E3, tt , pp
	
	if( ndet(qvalue.ind_q) .ne. 1 )then
	    killev = .TRUE.
	    return
	endif
	
	if( ndet(qvalue.ind_mass) .ne. 1 )then
	    killev = .TRUE.
	    return
	else
	    A3 =  det(doff(qvalue.ind_mass)).xval(qvalue.par_mass)	    
	endif
	
	A1 = qvalue.AP
	E1 = qvalue.EP
	A4 = qvalue.ATOT - A3
	
	if( ndet(qvalue.ind_beta) .ne. 1 )then
	    killev = .TRUE.
	    return
	else
	    E3 =  det(doff(qvalue.ind_beta)).xval(qvalue.par_beta)
	    E3 = E3*E3*qvalue.AMU*A3	    
	endif
	
	if( ndet(qvalue.ind_theta) .ne. 1 )then
	    killev = .TRUE.
	    return
	else
	    tt =  det(doff(qvalue.ind_theta)).xval(qvalue.par_theta)
	    tt = tt/10.0D0	    
	endif
	if( ndet(qvalue.ind_phi) .ne. 1 )then
	    killev = .TRUE.
	    return
	else
	    pp =  det(doff(qvalue.ind_phi)).xval(qvalue.par_phi)
	    pp = pp/10.0D0	    
	endif
	pp = qvalue.costhp*dcosd(tt)+dsind(pp)*qvalue.sinthp*dsind(tt)
*	write(*,*) E3, dacosd(pp), A3
	tt = (A1/A4-1.00D0)*E1 + (A3/A4+1.00D0)*E3 - 2.0D0/A4*dsqrt(A1*A3*E1*E3)*pp
	tt = (-tt)*qvalue.gain + qvalue.offset
	if( tt .ge. qvalue.low  .AND.  tt .le. qvalue.high )then
	    det(doff(qvalue.ind_q)).xval(qvalue.par_q) = tt
	    det(doff(qvalue.ind_q)).ival(qvalue.par_q) =  det(doff(qvalue.ind_q)).xval(qvalue.par_q)
	else
	    killev = .true.
	endif
	
	return
	end

    	subroutine gs4_qvalue_pms( qvalue )

#include "gsort.inc"
	record /qvalue_str/qvalue

	return
	end

		
    	subroutine gs0_angles_pms( ang )

#include "gsort.inc"
	record /prisma_angles_str/ang
        integer  ind1, ind2, ind3, ind4, ind5, ind6, ind7, res
        real*4 rprisma_angle
	

        synt(1)='ANGLES_PRISMA Dx Dy Dz Dq Dt Dp Dd prisma_angle [coeff. file]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

        call gs_getindpar(ind1, ang.parx, res)
	 if( ind1 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind2, ang.pary, res)
	 if( ind2 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind3, ang.parz, res)
	 if( ind3 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind4, ang.parq, res)
	 if( ind4 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind5, ang.part, res)
	 if( ind5 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind6, ang.parp, res)
	 if( ind6 .lt. 0 ) call gs_syntax('Detector parameter expected here')
	 call gs_getindpar(ind7, ang.pard, res)
	 if( ind7 .lt. 0 ) call gs_syntax('Detector parameter expected here')

	 ang.ind = ind1

        if( ang.ind .ne. ind2 )then
	   call gs_syntax('All parameters must be in the same detector')
	 endif
	 if( ang.ind .ne. ind3 )then
	   call gs_syntax('All parameters must be in the same detector')
	 endif
	 if( ang.ind .ne. ind4 )then
	   call gs_syntax('All parameters must be in the same detector')
	 endif
	 if( ang.ind .ne. ind5 )then
	   call gs_syntax('All parameters must be in the same detector')
	 endif

	 if( ang.ind .ne. ind6 )then
	   call gs_syntax('All parameters must be in the same detector')
	 endif

	 if( ang.ind .ne. ind7 )then
	   call gs_syntax('All parameters must be in the same detector')
	 endif

	 if( ang.parx .eq. ang.pary ) call gs_syntax('All five parameters must be different')
	 if( ang.parx .eq. ang.parz ) call gs_syntax('All five parameters must be different')
	 if( ang.parz .eq. ang.pary ) call gs_syntax('All five parameters must be different')
	 if( ang.parx .eq. ang.parq ) call gs_syntax('All five parameters must be different')
	 if( ang.pary .eq. ang.parq ) call gs_syntax('All five parameters must be different')
	 if( ang.parz .eq. ang.parq ) call gs_syntax('All five parameters must be different')
	 if( ang.parx .eq. ang.part ) call gs_syntax('All five parameters must be different')
	 if( ang.pary .eq. ang.part ) call gs_syntax('All five parameters must be different')
	 if( ang.parz .eq. ang.part ) call gs_syntax('All five parameters must be different')
	 if( ang.parq .eq. ang.part ) call gs_syntax('All five parameters must be different')

	 if( ang.parx .eq. ang.parp ) call gs_syntax('All five parameters must be different')
	 if( ang.pary .eq. ang.parp ) call gs_syntax('All five parameters must be different')
	 if( ang.parz .eq. ang.parp ) call gs_syntax('All five parameters must be different')
	 if( ang.parq .eq. ang.parp ) call gs_syntax('All five parameters must be different')
	 if( ang.part .eq. ang.parp ) call gs_syntax('All five parameters must be different')

	 if( ang.parx .eq. ang.pard ) call gs_syntax('All five parameters must be different')
	 if( ang.pary .eq. ang.pard ) call gs_syntax('All five parameters must be different')
	 if( ang.parz .eq. ang.pard ) call gs_syntax('All five parameters must be different')
	 if( ang.parq .eq. ang.pard ) call gs_syntax('All five parameters must be different')
	 if( ang.part .eq. ang.pard ) call gs_syntax('All five parameters must be different')
	 if( ang.parp .eq. ang.pard ) call gs_syntax('All five parameters must be different')

        if( istr_r1(comline, rprisma_angle) .ne. 1 )call gs_syntax('PRISMA angle expected here')
        if( rprisma_angle .lt. 0.0E0 .or. rprisma_angle .gt. 180.0E0 ) call gs_syntax('Invalid value for PRISMA angle [0-180]')
        ang.prisma_angle = rprisma_angle

	call istr_ch(comline,ang.calfile)
	
	if( lengthc(ang.calfile).LT.1 )then
		ang.oldstyle = 1
	else
		ang.oldstyle = 0
		call filetype(ang.calfile,'coef')
	endif
	


	return
	end


       subroutine gs1_angles_pms( ang )

#include "gsort.inc"
	record /prisma_angles_str/ang

	 call gs_putindpar( ang.ind, ang.parx )
	 llist = llist+3
	 call gs_putindpar( ang.ind, ang.pary )
	 llist = llist+3
	 call gs_putindpar( ang.ind, ang.parz )
	 llist = llist+3
	 call gs_putindpar( ang.ind, ang.parq )
 	 llist = llist+3
	 call gs_putindpar( ang.ind, ang.part )
 	 llist = llist+3
	 call gs_putindpar( ang.ind, ang.parp )
 	 llist = llist+3
	 call gs_putindpar( ang.ind, ang.pard )
        llist=lengthc(list)+2
        write(list(llist:),'(f5.1)')ang.prisma_angle


	return
	end


       subroutine gs2_angles_pms( ang )

#include "gsort.inc"
	record /prisma_angles_str/ang
	
	integer ix, ncoef, ii
	real*8 temp(16)

        if( init_mode .ne. 0 ) return
	
	ang.D = 250.0D0
	ang.sinalpha = 1.00D0/dsqrt(2.0D0)!sind( 45.0D0 )
	ang.cosalpha = 1.00D0/dsqrt(2.0D0)!cosd( 45.0D0 )
        ang.costhp = dcosd( ang.prisma_angle )
        ang.sinthp = dsind( ang.prisma_angle )
	
	if( ang.oldstyle .eq. 0 ) then
		ang.degx = 0
		ang.degy = 0
		open( UNIT=gs2lun, file=ang.calfile, status='OLD', err = 20, READONLY )
		
		do while( .true. )
			read( gs2lun,* , err=10, end = 10) ix, ncoef, ( temp(ii), ii = 1,ncoef)

			if( ix .eq. 1 ) then
				if( ncoef .lt. 2 )then
					write(*,'(''  ERROR - inconsistent X coefficients in file '', A)')ang.calfile
					call exit(0)
				endif
				if( ang.degx .lt. 1 )then
					if( ncoef .gt. 4 )then
						write(*,'('' ERROR - higher accepted polynomial degree is 3'')')
						call exit(0)
					endif
					ang.degx = ncoef-1
					do ii = 0, ang.degx
						ang.cx(ii) = temp(ii+1)
					enddo
				else
					write(*,'(''  ERROR - inconsistent X coefficients in file '', A)')ang.calfile
					call exit(0)
				endif
			endif

			if( ix .eq. 2 ) then
				if( ncoef .lt. 2 )then
					write(*,'(''  ERROR - inconsistent Y coefficients in file '', A)')ang.calfile
					call exit(0)
				endif
				if( ang.degy .lt. 1 )then
					if( ncoef .gt. 4 )then
						write(*,'('' ERROR - higher accepted polynomial degree is 3'')')
						call exit(0)
					endif
					ang.degy = ncoef-1
					do ii = 0, ang.degy
						ang.cy(ii) = temp(ii+1)
					enddo
				else
					write(*,'(''  ERROR - inconsistent Y coefficients in file '', A)')ang.calfile
					call exit(0)
				endif
			endif
		enddo
		
10		if( (ang.degx .lt. 1) .or. (ang.degy .lt. 1) )then
			write(*,'(''  ERROR - inconsistent coefficients in file '', A)')ang.calfile
			call exit(0)
		endif
		goto 25

20		write(*,'(''  ERROR - cannot open file '',A)') ang.calfile
		call exit(0)
		
25		close(gs2lun)
30		return	
	endif


	return
	end

       subroutine gs3_angles_pms_old( ang )

#include "gsort.inc"
	record /prisma_angles_str/ang
       real*8 xx, yy, tmp, tmp1, r, tt, dtq

         if( ndet( ang.ind ) .le. 0 )return
	  do ix = doff(ang.ind),doff(ang.ind)+ndet(ang.ind)-1
	    if( det(ix).ival(ang.parx).lt.0 .or. det(ix).ival(ang.pary).lt.0 )then
	      det(ix).ival(ang.parx) = 0
	      det(ix).xval(ang.parx) = 0.0E0
	      det(ix).ival(ang.pary) = 0
	      det(ix).xval(ang.pary) = 0.0E0
	      det(ix).ival(ang.parz) = 10.0E0*ang.prisma_angle
	      det(ix).xval(ang.parz) = 10.0E0*ang.prisma_angle
            else
	      xx = det(ix).xval(ang.parx)
	      xx = -110.479 + 1.06345*xx
	      xx = xx/8.0D0
	      yy = det(ix).xval(ang.pary)
	      yy = -126.545 + 1.06265*yy
	      yy = yy/8.0D0

	      r =  -65.9869627096222D0 -0.0682862486136746D0*xx +0.000399384732948225*xx*xx -6.43084039314526d-07*xx*xx*xx
	      r = r + 0.341239035721098D0*yy +0.000271273723848787D0*xx*yy -2.3505358573592d-07*xx*xx*yy + 4.30039734549059d-05*yy*yy
	      tmp = r -5.81684294026954d-07*xx*yy*yy -3.60561786459707d-08*yy*yy*yy

	      r = 51.0606413225447D0 -0.0635862335147582D0*xx -0.00223427469485911*xx*xx +4.03404471030435d-06*xx*xx*xx
	      r = r -0.00643019790615745D0*yy -0.000123865271631079D0*xx*yy +9.70694922484791d-07*xx*xx*yy
	      r = r + 0.000104365951382891D0*yy*yy -3.85790951372853d-07*xx*yy*yy -6.69758637684284d-08*yy*yy*yy

	      xx = tmp +9.0D0
	      yy = r +3.0D0



	     dtq  = 480.0D0
	     dtq = 750.0D0 - dtq/2.0D0

	      r = xx*xx*ang.sinalpha*ang.sinalpha + yy*yy
	      tmp = ang.D - xx*ang.cosalpha

             tt = tmp/dsqrt( r + tmp*tmp )
	     dtq = dtq/tt
	     tt = dacosd( tt )
             det(ix).xval(ang.parx) = 10.0D0 * tt
	     det(ix).ival(ang.parx) = det(ix).xval(ang.parx)

*	      det(ix).xval(ang.parz) = dsqrt(r + tmp*tmp-yy*yy)
	      det(ix).xval(ang.parz) = dtq - dsqrt(r + tmp*tmp)
	      det(ix).ival(ang.parz) = det(ix).xval(ang.parz)

	      tmp1 = dasind( xx*ang.sinalpha/dsqrt(r + tmp*tmp - yy*yy) )
	      det(ix).xval(ang.parq) = 2000.0D0 + 100.0D0*tmp1
	      det(ix).ival(ang.parq) = det(ix).xval(ang.parq)


	      r = sqrt(r)
	      if( r .lt. 1.0E-02 )then
	        det(ix).ival(ang.pary) = 0
		 det(ix).xval(ang.pary) = 0.0E0
               tmp = 0.0D0
	      else
	        tmp = dasind( -xx*ang.sinalpha/R )
	        if( yy .ge. 0.00D0 )then
		   if( tmp .lt. 0.0D0 ) tmp = 360.0D0 + tmp
		else
		   tmp  = 180.0D0 -tmp
		endif
	        det(ix).xval(ang.pary) = 10.0D0*tmp
		 det(ix).ival(ang.pary) = det(ix).xval(ang.pary)
	      endif
	      
	      tmp1 =  dsind(tt)*dcosd(tmp)
              tmp = ang.costhp*dcosd(tt)+dsind(tmp)*ang.sinthp*dsind(tt)
              tmp = dacosd(tmp)
	      tmp1 = dacosd( tmp1/dsind(tmp) )
	     
	      det(ix).xval(ang.part) = 10.0D0*tmp
	      det(ix).ival(ang.part) = det(ix).xval(ang.part)

	      det(ix).xval(ang.parp) = 10.0D0*tmp1
	      det(ix).ival(ang.parp) = det(ix).xval(ang.parp)
	    endif
	enddo

	return
	end



       subroutine gs3_angles_pms( ang )

#include "gsort.inc"
	record /prisma_angles_str/ang
       real*8 xx, yy, tmp, tmp1, r, tt, dtq

         if( ndet( ang.ind ) .le. 0 )return
	  do ix = doff(ang.ind),doff(ang.ind)+ndet(ang.ind)-1
	    if( det(ix).ival(ang.parx).lt.0 .or. det(ix).ival(ang.pary).lt.0 )then
	      det(ix).ival(ang.parx) = 0
	      det(ix).xval(ang.parx) = 0.0E0
	      det(ix).ival(ang.pary) = 0
	      det(ix).xval(ang.pary) = 0.0E0
	      det(ix).ival(ang.parz) = 10.0E0*ang.prisma_angle
	      det(ix).xval(ang.parz) = 10.0E0*ang.prisma_angle
            else
	      if( ang.oldstyle .eq. 1)then
		xx = det(ix).xval(ang.parx)
		xx = -110.479 + 1.06345*xx
		xx = xx/8.0D0
		yy = det(ix).xval(ang.pary)
		yy = -126.545 + 1.06265*yy
		yy = yy/8.0D0
 
		r = -65.9869627096222D0 -0.0682862486136746D0*xx +0.000399384732948225*xx*xx -6.43084039314526d-07*xx*xx*xx
		r = r + 0.341239035721098D0*yy +0.000271273723848787D0*xx*yy 
		r = r -2.3505358573592d-07*xx*xx*yy + 4.30039734549059d-05*yy*yy
		tmp = r -5.81684294026954d-07*xx*yy*yy -3.60561786459707d-08*yy*yy*yy
 
		r = 51.0606413225447D0 -0.0635862335147582D0*xx -0.00223427469485911*xx*xx +4.03404471030435d-06*xx*xx*xx
		r = r -0.00643019790615745D0*yy -0.000123865271631079D0*xx*yy +9.70694922484791d-07*xx*xx*yy
		r = r + 0.000104365951382891D0*yy*yy -3.85790951372853d-07*xx*yy*yy -6.69758637684284d-08*yy*yy*yy
 		xx = tmp +9.0D0
	      	yy = r +3.0D0
	      else
	        xx = ang.cx(ang.degx)
		yy = ang.cy(ang.degy)
		
		do ii = ang.degx-1, 0, -1
		    xx = xx*det(ix).xval(ang.parx) + ang.cx(ii)
		enddo 
		do ii = ang.degy-1, 0, -1
		    yy = yy*det(ix).xval(ang.pary) + ang.cy(ii)
		enddo
	      endif 


*SeU		tmp =  det(ix).xval(ang.pary)*(-0.033448810D0) + 64.59298 !-0.50D0
*SeU		r =   det(ix).xval(ang.parx)*0.034683580D0 -  73.089870D0 !+2.50E0

*		tmp =  det(ix).xval(ang.pary)*(-0.032964360D0) + 61.699010D0 +1.80000D0 !  ArEr
*		r =   det(ix).xval(ang.parx)*0.034182910D0 -  71.419490D0  +2.2300D0  !  ArEr

*		tmp =  det(ix).xval(ang.pary)*(0.02731840D0) -64.27221D0 +1.80000D0 !  PlungerTest-150um
*		r =   det(ix).xval(ang.parx)*0.033333360D0 -56.266710D0  +2.2300D0  !  PlungerTest-150um
		
*		tmp =  det(ix).xval(ang.pary)*(0.02690750D0) -65.85450D0 +1.0000D0 !  64Ni-2
*		r =   det(ix).xval(ang.parx)*0.032288550D0 -57.226070D0  -2.000D0  !  64Ni-2
		
*	      xx = tmp  !+9.0D0
*	      yy = r !+3.0D0



	      dtq  = 480.0D0
	      dtq = 750.0D0 - dtq/2.0D0

	      r = xx*xx*ang.sinalpha*ang.sinalpha + yy*yy
	      tmp = ang.D - xx*ang.cosalpha

              tt = tmp/dsqrt( r + tmp*tmp )
	      dtq = dtq/tt
	      tt = dacosd( tt )
              det(ix).xval(ang.parx) = 10.0D0 * tt
	      det(ix).ival(ang.parx) = det(ix).xval(ang.parx)

	      det(ix).xval(ang.pard) = dsqrt(r + tmp*tmp)
	      det(ix).xval(ang.parz) = dtq - det(ix).xval(ang.pard)
	      det(ix).ival(ang.parz) = det(ix).xval(ang.parz)
	      det(ix).ival(ang.pard) = det(ix).xval(ang.pard)

	      tmp1 = dasind( xx*ang.sinalpha/dsqrt(r + tmp*tmp - yy*yy) )
	      det(ix).xval(ang.parq) = 2000.0D0 + 100.0D0*tmp1
	      det(ix).ival(ang.parq) = det(ix).xval(ang.parq)


	      r = sqrt(r)
	      if( r .lt. 1.0E-02 )then
	        det(ix).ival(ang.pary) = 0
		det(ix).xval(ang.pary) = 0.0E0
                tmp = 0.0D0
	      else
	        tmp = dasind( -xx*ang.sinalpha/R )
	        if( yy .ge. 0.00D0 )then
		   if( tmp .lt. 0.0D0 ) tmp = 360.0D0 + tmp
		else
		   tmp  = 180.0D0 -tmp
		endif
	        det(ix).xval(ang.pary) = 10.0D0*tmp
		det(ix).ival(ang.pary) = det(ix).xval(ang.pary)
	      endif
	      
	      tmp1 =  dsind(tt)*dcosd(tmp)
              tmp = ang.costhp*dcosd(tt)+dsind(tmp)*ang.sinthp*dsind(tt)
              tmp = dacosd(tmp)
	      tmp1 = dacosd( tmp1/dsind(tmp) )
	     
	      det(ix).xval(ang.part) = 10.0D0*tmp
	      det(ix).ival(ang.part) = det(ix).xval(ang.part)

	      det(ix).xval(ang.parp) = 10.0D0*tmp1
	      det(ix).ival(ang.parp) = det(ix).xval(ang.parp)
	    endif
	enddo


	return
	end

       subroutine gs4_angles_pms( ang )

#include "gsort.inc"
	record /prisma_angles_str/ang



	return
	end




	subroutine GS0_BPVELOCITY_PMS( qvalue )
	

#include "gsort.inc"
	record /qvalue_str/qvalue
	
	integer ii

	
	
	synt(1)='BP_VELOCITY_PRISMA Ap(proj. mass)	At(target mass)    Ep(proj. energy[MeV]) Theta_PRISMA[deg]'
	synt(2)='                   Pm (detected mass)  Pa (polar THETA)   Pb (polar PHI)        Pc (v/c[%])'
	

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif
	

	if( istr_i2(comline, qvalue.AP, qvalue.AT) .ne. 2 ) then
	   call gs_syntax('Projectile and target masses expected here')
	endif
	if( istr_r2(comline, qvalue.ep, qvalue.thp) .ne. 2 ) then
	   call gs_syntax('Projectile energy[MeV] and PRISMA angle[deg] expected here')
	endif
	call gs_readline
	if(lcomline.LT.1) call gs_syntax('Incomplete command, parameters for the detected ion expected here')
	
	call gs_getindpar(qvalue.ind_mass, qvalue.par_mass,ii)
	call gs_getindpar(qvalue.ind_theta, qvalue.par_theta,ii)
	call gs_getindpar(qvalue.ind_phi, qvalue.par_phi,ii)
	call gs_getindpar(qvalue.ind_beta, qvalue.par_beta,ii)

	qvalue.costhp = qvalue.thp
	qvalue.sinthp = qvalue.costhp
	qvalue.costhp = dcosd(qvalue.costhp)	
	qvalue.sinthp = dsind(qvalue.sinthp)
	qvalue.amu = 931.494320D0
	qvalue.ATOT = qvalue.AP + qvalue.AT
	
	return
	end

	subroutine GS1_BPVELOCITY_PMS( qvalue )
	

#include "gsort.inc"
	record /qvalue_str/qvalue
	
	write(list(llist+1:), '(2I8,2F10.3)')qvalue.AP,qvalue.AT,qvalue.EP,qvalue.THP
	call gs_writeline( LLUN, list, INDENT )
	
	llist = INDENT
	call gs_putindpar( qvalue.ind_mass, qvalue.par_mass )	
	llist = llist+3
	call gs_putindpar( qvalue.ind_theta, qvalue.par_theta )	
	llist = llist+3
	call gs_putindpar( qvalue.ind_phi, qvalue.par_phi )	
	llist = llist+3
	call gs_putindpar( qvalue.ind_beta, qvalue.par_beta )	
	call gs_writeline( LLUN, list, INDENT )
	
	return
	end
	
	subroutine GS2_BPVELOCITY_PMS( qvalue )
	
#include "gsort.inc"
	record /qvalue_str/qvalue
	
	return
	end
	
	subroutine GS3_BPVELOCITY_PMS( qvalue )
	
#include "gsort.inc"
	record /qvalue_str/qvalue
	
	real*8 A1, A3, A4, P1, P3, P4, tt , pp, bptheta, bpphi
	
	if( ndet(qvalue.ind_q) .ne. 1 )then
	    killev = .TRUE.
	    return
	endif
	
	if( ndet(qvalue.ind_mass) .ne. 1 )then
	    killev = .TRUE.
	    return
	else
	    A3 =  det(doff(qvalue.ind_mass)).xval(qvalue.par_mass)	    
	endif
	
	A1 = qvalue.AP
	E1 = qvalue.EP
	A4 = qvalue.ATOT - A3
	
	P1 =  dsqrt(2.00D4*qvalue.EP/qvalue.AMU*A1)
	
	if( ndet(qvalue.ind_beta) .ne. 1 )then
	    killev = .TRUE.
	    return
	else
	    P3 =  det(doff(qvalue.ind_beta)).xval(qvalue.par_beta)
	    P3 = P3*A3	    
	endif
	
	if( ndet(qvalue.ind_theta) .ne. 1 )then
	    killev = .TRUE.
	    return
	else
	    tt =  det(doff(qvalue.ind_theta)).xval(qvalue.par_theta)
	    tt = tt/10.0D0	    
	endif
	if( ndet(qvalue.ind_phi) .ne. 1 )then
	    killev = .TRUE.
	    return
	else
	    pp =  det(doff(qvalue.ind_phi)).xval(qvalue.par_phi)
	    pp = pp/10.0D0	    
	endif
	
	P4 = dsqrt( P1*P1 + P3*P3 - 2.00D0*P1*P3*(qvalue.costhp*dcosd(tt)+dsind(pp)*qvalue.sinthp*dsind(tt)) )

	bptheta = (P1*qvalue.costhp - P3*dcosd(tt))/P4
	bptheta = dacosd( bptheta )
	det(doff(qvalue.ind_theta)).xval(qvalue.par_theta) = 10.00D0*bptheta
	det(doff(qvalue.ind_theta)).ival(qvalue.par_theta) = det(doff(qvalue.ind_theta)).xval(qvalue.par_theta)

	if( dabs( dsind( bptheta ) ) .gt. 1.00D-04 )then
	    bpphi = ( P1*qvalue.sinthp - P3*dsind(tt)*dsind(pp) )/P4/dsind( bptheta )
	else
	    bpphi = 0.00D0
	endif
	
	if( dabs( dcosd( bptheta ) ) .gt. 1.00D-04 )then
	    bptheta = ( - P3*dsind(tt)*dcosd(pp) )/P4/dsind( bptheta )
	else
	    bptheta = 1.00D0
	endif

	if( bpphi .ge. 0.00D0 .and. bptheta .ge. 0.00D0 )then
	    bpphi = dasind( bpphi )
	elseif( bpphi .ge. 0.00D0 .and. bptheta .lt. 0.00D0 )then
	    bpphi = 180.00D0 - dasind( bpphi )
	elseif( bpphi .lt. 0.00D0 .and. bptheta .lt. 0.00D0 )then
	    bpphi = 180.00D0 + dasind( -bpphi )
	else
	    bpphi = 360.00D0 - dasind( -bpphi )
	endif

	det(doff(qvalue.ind_phi)).xval(qvalue.par_phi) = 10.00D0*bpphi
	det(doff(qvalue.ind_phi)).ival(qvalue.par_phi) = det(doff(qvalue.ind_phi)).xval(qvalue.par_phi)
	    
	det(doff(qvalue.ind_beta)).xval(qvalue.par_beta) = P4/A4
	det(doff(qvalue.ind_beta)).ival(qvalue.par_beta) = det(doff(qvalue.ind_beta)).xval(qvalue.par_beta)
	    	
	return
	end

    	subroutine GS4_BPVELOCITY_PMS( qvalue )

#include "gsort.inc"
	record /qvalue_str/qvalue

	return
	end



*** N.M.  ****  XFPD_PRISMA_CHOOSE implementation ****************



	subroutine gs0_xfp_pms(rcl)

#include "gsort.inc"
	record/recal_choose/rcl
	logical*1 getmem

	synt(1)='XFP_PRISMA  Xl <file.cal(Xl)>  Xr <file.cal(Xr)>  Xc [NORUN|RUN]  offset gain  Wl Wh  FOLD_GATE'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	do ii = 1,2
	  call gs_getindpar(rcl.ind(ii),rcl.par(ii),ires)
	  if( ii .eq. 1)then
	    rcl.wlo=0
	    rcl.whi=ires-1
	    rcl.offs=0.
	    rcl.gain=1.
	    call istr_ch(comline,rcl.file1)
	    if(lengthc(rcl.file1).LT.1) call gs_syntax('Filename expected')
	    call filetype(rcl.file1,'cal')
	  else
	    call istr_ch(comline,rcl.file2)
	    if(lengthc(rcl.file2).LT.1) call gs_syntax('Filename expected')
	    call filetype(rcl.file2,'cal')
	  endif
	enddo
	if( rcl.ind(1) .ne. rcl.ind(2) )call gs_syntax('Must be in the same detector')
	if( rcl.par(1) .eq. rcl.par(2) )call gs_syntax('Not allowed for the same parameter')
	if( rcl.ind(1) .eq. 0 )call gs_syntax('Not allowed for header parameters')

	call gs_getindpar(rcl.ind(3),rcl.par(3),ires)
	if( rcl.ind(1) .ne. rcl.ind(3) )call gs_syntax('Must be in the same detector')
	if( rcl.par(1) .eq. rcl.par(3) )call gs_syntax('Not allowed for the same parameter')
	if( rcl.par(2) .eq. rcl.par(3) )call gs_syntax('Not allowed for the same parameter')

	call gs_getrunmode(rcl.run)

	ii=istr_r2(comline,rcl.offs,rcl.gain)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	ii=istr_i2(comline,rcl.wlo,rcl.whi)
	if(ii.ne.2) call gs_syntax('Wrong number of parameters')

	call gs_getfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	ibsize=(MAXRCL_ORD+1)*MAXDET
	ibsize=8*ibsize
	if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for XFP_PRISMA'
	com(icmd).size=ibsize

	return

	end

	subroutine gs1_xfp_pms(rcl)

#include "gsort.inc"
	record/recal_choose/rcl

	do ii = 1,2
	  call gs_putindpar(rcl.ind(ii),rcl.par(ii))
	  if( ii .eq. 1)then
	   lcalfile=max(1,lengthc(rcl.file1))
	   write(list(llist+2:),'(A)') rcl.file1(1:lcalfile)
	  else
	   lcalfile=max(1,lengthc(rcl.file2))
	   write(list(llist+2:),'(A)') rcl.file2(1:lcalfile)
	  endif
	  llist=lengthc(list)+3
	enddo
	
	call gs_putindpar(rcl.ind(3),rcl.par(3))
	llist=lengthc(list)+2	
	call gs_putrunmode(rcl.run)

	write(list(llist+1:),'(F12.2,F12.4,2I6)') rcl.offs,rcl.gain,rcl.wlo,rcl.whi

	llist=lengthc(list)+2
	call gs_putfoldgate(rcl.ind,rcl.fl,rcl.fh,rcl.same)

	return

	end

	subroutine gs2_xfp_pms(rcl,dat)

#include "gsort.inc"
	record/recal_choose/rcl
	record/calcoef/dat(0:MAXDET-1,2)

	REAL	  TEMP   (0:MAXDET-1)
	LOGICAL	  MISSING(0:MAXDET-1)
#if defined( __GFORTRAN__ )
	character*64 dyn_format
	logical*1 inp_yes
	external inp_yes
#endif


	if(init_mode.EQ.0) then
	  if(rcl.run) return
	else
	  if(.NOT.rcl.run) return
	endif

        do kk = 1,2

	j_ind=rcl.ind(kk)
	j_par=rcl.par(kk)

	ntoget=nitems(j_ind)
	ntoget_t=ntoget

	if(j_ind.EQ.0) then
	  missing(j_par)=.TRUE.
	else
	  DO II=0,ntoget_t-1
	    MISSING(II)=.TRUE.
	  end do
	endif

	if( kk .eq. 1) then
2	 lrcl_file=max(1,lengthc(rcl.file1))
	 OPEN(UNIT=gs2lun,FILE=rcl.file1,STATUS='OLD',READONLY,err=5)
	 DO ILU=LU1,LU2
	  WRITE(ILU,*)
	  WRITE(ILU,'(A,I1)')' Recalibration coefficients for '//dname(j_ind),j_par
	  WRITE(ILU,*)'Taken from File '//rcl.file1(1:lrcl_file)
	 end do
	 goto 10

5	 write(6,*) 'Error reading ',rcl.file1(1:lrcl_file)
	 if(inp_yes('Retry')) goto 2
	 IOK=-1
	 RETURN	
	else
201	 lrcl_file=max(1,lengthc(rcl.file2))
	 OPEN(UNIT=gs2lun,FILE=rcl.file2,STATUS='OLD',READONLY,err=501)
	 DO ILU=LU1,LU2
	  WRITE(ILU,*)
	  WRITE(ILU,'(A,I1)')' Recalibration coefficients for '//dname(j_ind),j_par
	  WRITE(ILU,*)'Taken from File '//rcl.file2(1:lrcl_file)
	 end do
	 goto 10

501	 write(6,*) 'Error reading ',rcl.file2(1:lrcl_file)
	 if(inp_yes('Retry')) goto 201
	 IOK=-1
	 RETURN	
	endif

10	READ(gs2lun,*,ERR=20,END=20) ICTAP,ICADC,JPAR,(TEMP(J),J=1,JPAR)
#if defined( __GFORTRAN__ )
	write(dyn_format,'(i4)')jpar
	dyn_format = adjustl(dyn_format)
#endif
	IF(ICTAP.EQ.iio.run .OR. .NOT. rcl.run) THEN
	  IF(MISSING(ICADC)) THEN
	    if(j_ind.ne.0) then
#if defined( __GFORTRAN__ )
	      WRITE(LU2,'(''ADC#'',I5,'//dyn_format(1:len(dyn_format))//'G0)')ICADC,(TEMP(J),J=1,JPAR)
	      if(jpar.GT.maxRCL_ORD) then
	        WRITE(LU1,'(''ADC#'',I5,'//dyn_format(1:len(dyn_format))//'G0)')ICADC,(TEMP(J),J=1,JPAR)
#else
	      WRITE(LU2,4201)ICADC,(TEMP(J),J=1,JPAR)
	      if(jpar.GT.maxRCL_ORD) then
	        WRITE(LU1,4201)ICADC,(TEMP(J),J=1,JPAR)
#endif
	        stop 'Too high order of recalibration'
	      endif
	      dat(ICADC,kk).ord=JPAR
	      DO J=1,JPAR
	        dat(ICADC,kk).coef(J)=TEMP(j)
	      end do
	    else
#if defined( __GFORTRAN__ )
	      WRITE(LU2,'(''FPAR#'',I4,'//dyn_format(1:len(dyn_format))//'G0)')ICADC,(TEMP(J),J=1,JPAR)
	      if(jpar.GT.2) then
	        WRITE(LU1,'(''FPAR#'',I4,'//dyn_format(1:len(dyn_format))//'G0)')ICADC,(TEMP(J),J=1,JPAR)
#else
	      WRITE(LU2,4202)ICADC,(TEMP(J),J=1,JPAR)
	      if(jpar.GT.2) then
	        WRITE(LU1,4202)ICADC,(TEMP(J),J=1,JPAR)
#endif
	        stop 'Too high order of recalibration'
	      endif
	      rcl.offs=temp(1)
	      rcl.gain=temp(2)
	    endif
	    MISSING(ICADC)=.FALSE.
	    ntoget=ntoget-1
	    IF(ntoget.EQ.0) GOTO 20
	  ENDIF
	ENDIF
	GOTO 10

#if !defined( __GFORTRAN__ )
4201	format(' ADC#',I5,<JPAR>G)
4202	format(' FPAR#',I4,<JPAR>G)
#endif

20	CLOSE(gs2lun)

	if(j_ind.EQ.0) then
	  if(missing(j_par)) then
	    DO ILU=LU1,LU2
		rcl.offs=0
		rcl.gain=0
		WRITE(ILU,'('' FPAR#'',I4,3X,A)') II,' is missing ===> set to zero'
	    end do
	  endif
	  break=.FALSE.
	  return
	endif

	DO ii=0,ntoget_t-1
	   IF(MISSING(II)) THEN
	     DO ILU=LU1,LU2
		dat(ii,kk).ord=0
		WRITE(ILU,'('' ADC#'',I5,3X,A)') ii,' is missing ===> set to zero'
	     end do
	   ENDIF
	end do

	do ii=0,ntoget_t-1
	  if(dat(ii,kk).ord .GT. 0) then
	    do jj=1,dat(ii,kk).ord
	      dat(ii,kk).coef(jj)=dat(ii,kk).coef(jj)*rcl.gain
	    end do
	    dat(ii,kk).coef(1)=dat(ii,kk).coef(1)+rcl.offs
	  endif
	end do

	enddo
	
	break=.FALSE.
	return

	end

	subroutine gs3_xfp_pms(rcl,dat)

#include "gsort.inc"
	record/recal_choose/rcl
	record/calcoef/dat(0:MAXDET-1,2)
	
	integer iside

	if( ndet(ji) .lt. 1 )return
	ji=rcl.ind(1)
	
	jxl=rcl.par(1)
	jxr=rcl.par(2)
	jc=rcl.par(3)
	
	
	nelim=0
	jwlo=rcl.wlo
	jwhi=rcl.whi  
	
		  
	do jx=doff(ji),doff(ji)+ndet(ji)-1					    
	  id=det(jx).id
	  
	  xl = det(jx).xval(jxl)
	  xr = det(jx).xval(jxr)
	  xc = det(jx).xval(jc)
	  
	  if( (xl .ge. 4.00E0) .and. (xr .ge. 4.00E0) ) then
	  	ENX = ( xr - xl )*0.2500E0 + 2000.00E0
		ENX = ENX*rcl.gain + rcl.offs
		iside = 0
	  elseif((xl .ge. 4.00E0) .and. (xr .lt. 4.00E0) .and. (xc .ge. 4.00E0) )then
	  	ENX = ( xc - xl )*0.500E0 + 2000.00E0
		iside = 1
	  elseif((xl .lt. 4.00E0) .and. (xr .ge. 4.00E0) .and. (xc .ge. 4.00E0) )then
	  	ENX = ( xr - xc )*0.500E0 + 2000.00E0
		iside = 2
	  else
	         det(jx).id = -1
		 nelim = nelim + 1
		 goto 10
	  endif
	   							    
	  if( iside .gt. 0 )then
	    ncoef=dat(id,iside).ord					      
	    if(ncoef.le.0) then 						      
	      det(jx).id=-1			! mancano i coefficienti ==> elimina   l'ADC
	      nelim=nelim+1
	      goto 10
	    endif							      
	    								      
	    XCAN=ENX						      

	    ENX=dat(id,iside).coef(ncoef)					  
	    DO II=ncoef-1,1,-1  						  
	      ENX=ENX*XCAN+dat(id,iside).coef(ii)				  
	    end do								  
	    			    ! offset,gain inclusi nei coefficienti
	  endif
	  ICAN=enx				      	   		  

	  if(ican.GE.jwlo .and. ican.le.jwhi) then	   			      
	    det(jx).ival(jxl)=ican			   			      
	    det(jx).xval(jxl)=enx			   			      
	  else  					   			      
	    det(jx).id=-1		    ! elimina l'ADC			      
	    nelim=nelim+1				   			      
	  endif 					   			      
	  
10	  continue
	end do
										    
	if(nelim.ne.0 .and. rcl.same) goto 9000 				    
	nx=ndet(ji)-nelim							    
	if(nx.LT.rcl.fl .or. nx.GT.rcl.fh) goto 9000				    
	if(nelim.GT.0) call gs_checkdetector(ji)				    

	return

9000	killev=.TRUE.
	return

	end

	subroutine gs4_xfp_pms

	return

	end


************************************ N.M. *** end of XFPD_PRISMA_CHOOSE
