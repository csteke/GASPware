#ifdef Digital
#  define POINTERSIZE integer*8
#else
#  define POINTERSIZE integer*4
#endif

	subroutine usersub9(flag)

	integer flag

#include "gsort.inc"

#define  U9MAX 512
	
	structure /U9_TYPE/
	    integer ind
	    integer from, to
	    character*32 File
	    integer    dat(0:U9MAX,0:U9MAX)
	    integer    counter(0:U9MAX)
	    integer    hit(0:U9MAX)	    
	end structure
	
	record /U9_TYPE/u9
	save u9
	
	integer nn,ii,jj
	
	if ( flag .eq. 0 ) then
	  synt(1) = 'USERSUB9 D From To OUTFILE'

	  if(syntax) then
	    call gs_syntax(' ')
	    return
	  endif

	  call gs_getind( u9.ind )
	  if( u9.ind .eq. 0 ) call gs_syntax('Detector name expected here')
	  nn = istr_i2(comline, u9.from, u9.to)
	  if(nn.ne.2) call gs_syntax('Wrong number of parameters')
	  if( u9.from .gt. u9.to )then
	      nn = u9.from
	      u9.from = u9.to
	      u9.to = nn
	  endif
	  if( u9.from .lt. 1 )call gs_syntax('Min. multiplicity must be at least 1')
	  if( u9.to .gt. U9MAX )call gs_syntax('Max. multiplicity currently limited to no more than 512')

	  call istr_ch(comline,u9.file)
	  if(lengthc(u9.file).LT.1) call gs_syntax('Name for the output file expected here')

	elseif ( flag .eq. 1 ) then
	  call gs_putind( u9.ind )
	  write(list(llist+1:),'(2x,2(i3,1x),2x,A)')u9.from, u9.to, u9.file
	
	elseif( flag .eq. 2 )then
	  if( init_mode .eq. 0 )then
	     do ii = 1,U9MAX
	        u9.counter(ii) = 0
		u9.hit(ii) = 0
		do jj = 1,U9MAX
		  u9.dat(ii,jj) = 0
		enddo
	     enddo
	  endif
	
	elseif( flag .eq. 3 )then   
	   if( ndet(u9.ind) .ge. 0 )then
	     do nn = u9.from, u9.to
	         u9.counter(nn) = u9.counter(nn) + 1
		 u9.hit(nn) = u9.hit(nn) + ndet(u9.ind)
		 u9.counter(nn) = u9.counter(nn) - ((u9.counter(nn)/nn)*nn)
		 if( u9.counter(nn) .eq. 0 )then
		      u9.dat(nn,u9.hit(nn)) = u9.dat(nn,u9.hit(nn))+1
		      u9.hit(nn) = 0
		 endif
	      enddo
	   endif
	
	elseif( flag .eq. 4 )then
	   call lib$get_lun(nn)
	   open(unit=nn, file=u9.file, status='unknown')
	   do ii = u9.from, u9.to
	      write(nn,'('' Buffer of Mult '',i3.3)')ii
	      write(nn,*)( jj,u9.dat(ii,jj), jj=0,U9MAX )
	   enddo
	   close(unit=nn)
	
	else
	   stop 'USERSUB9 : must never get here'
	endif
	
	end
	

	subroutine usersub99(flag)

	integer flag

#include "gsort.inc"

	integer iind,ipar,nfold,iemin,iemax,mcounts,icounts,ival(0:MAXDET-1)
	save    iind,ipar,nfold,iemin,iemax,mcounts,icounts,ival

	if(flag.eq.0) then			! from GETINPUT
	  synt(1)='USERSUB9  Dn  Fold minVal maxVal Counts'
	  synt(2)='         generate flat distribution of values in detector D parameter n'

	  if(syntax) then
	    call gs_syntax(' ')
	    return
	  endif

	  call gs_getindpar(iind,ipar,ires)
	  if(iind.EQ.0) call gs_syntax('Detector name expected')

	  nn=istr_i1(comline,nfold)
	  if(nn.ne.1) call gs_syntax('Wrong number of parameters')
	  if(nfold.gt.nitems(iind)) call gs_syntax('Fold too high')

	  nn=istr_i2(comline,iemin,iemax)
	  if(nn.ne.2) call gs_syntax('Wrong number of parameters')
	  if(iemin.lt.0 .or. iemin.ge.ires) call gs_syntax('Illegal value')
	  if(iemax.lt.0 .or. iemax.ge.ires) call gs_syntax('Illegal value')
	  if(iemax.lt.iemin )               call gs_syntax('Illegal value')

	  nn=istr_i1(comline,mcounts)
	  if(nn.ne.1) call gs_syntax('Wrong number of parameters')
	  mcounts=abs(mcounts)

	elseif(flag.eq.1) then			! from LISTPROG
	  call gs_putindpar(iind,ipar)
	  write(list(llist+1:),'(3i8,i10)') nfold,iemin,iemax,mcounts

	elseif(flag.eq.2) then			! from INIT_RUN
	  if(init_mode.EQ.0) then
	    do ii=0,nfold-1
	      ival(ii)=iemin
	    enddo
	    ival(0)=ival(0)-1
	    icounts=1
	  endif

	elseif(flag.eq.3) then			! from EVANA
	  if(icounts.gt.mcounts) then
	    break=.true.
	    killev=.true.
	    return
	  endif
	  do jj=0,nfold-1
	     ival(jj)=ival(jj)+1
 	     if(ival(jj).le.iemax) goto 10
	     ival(jj)=iemin
	  end do
	  icounts=icounts+1	    
	  if(icounts.gt.mcounts) then 
	    break=.true.
	    killev=.true.
	    return
	  endif
10	  ii=0		!!!! attenzione che fatto cosi' puo' invadere il successivo rivelatore
	  do jj=doff(iind),doff(iind)+nfold-1
	    det(jj).id=ii
	    det(jj).ival(ipar)=ival(ii)
	    det(jj).xval(ipar)=ival(jj)+0.5
	    ii=ii+1
	  enddo
	  ndet(iind)=nfold

	elseif(flag.eq.4) then			! from FINIT_RUN

	endif

	return

	end
