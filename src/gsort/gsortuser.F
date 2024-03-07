
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  USERSUBS

	subroutine usersub1(flag)

	integer flag

	INCLUDE 'gsort.inc'

	logical*1 todo(0:43)

	    if(flag.eq.0) then			! from GETINPUT
c	  call gs_syntax('Please replace this Subroutine with your own version')
	elseif(flag.eq.1) then			! from LISTPROG
	  write(list(llist:),'(A,''  35 36 37 38  to 40 41 42 43'')') 'Copy '//dname(4)
	elseif(flag.eq.2) then			! from INIT_RUN
	  do ii=0,43
	    todo(ii)=.false.
	  enddo
	  todo(35)=.true.
	  todo(36)=.true.
	  todo(37)=.true.
	  todo(38)=.true.
	elseif(flag.eq.3) then			! from EVANA
	  if(ndet(4).le.0) return
	  ntodo=0
	  do ii=doff(4),doff(4)+ndet(4)-1
	    if(todo(det(ii).id)) ntodo=ntodo+1
	  enddo
	  if(ntodo.eq.0) return
	  imdoff=0
	  do ii=doff(4),doff(4)+ndet(4)-1
	    mdet(imdoff).id=det(ii).id
	    do jp=0,TDPAR(4)-1
	      mdet(imdoff).ival(jp)=det(ii).ival(jp)
	      mdet(imdoff).xval(jp)=det(ii).xval(jp)
	    enddo
	    imdoff=imdoff+1
	    id=det(ii).id
	    if(todo(id)) then
	      mdet(imdoff).id=det(ii).id+5
	      do jp=1,TDPAR(4)-1,2
	        mdet(imdoff).ival(jp-1)=det(ii).ival(jp)
	        mdet(imdoff).xval(jp-1)=det(ii).xval(jp)
	        mdet(imdoff).ival(jp)=0
	        mdet(imdoff).xval(jp)=0.0
	      enddo
	      imdoff=imdoff+1
	    endif
	  enddo
	  doff(4)=doff(NTIPI)+ndet(NTIPI)
	  ndet(4)=ndet(4)+ntodo
	  imdoff=doff(4)
	  do ii=0,ndet(4)-1
	    det(imdoff).id=mdet(ii).id
	    do jp=0,TDPAR(4)-1
	      det(imdoff).ival(jp)=mdet(ii).ival(jp)
	      det(imdoff).xval(jp)=mdet(ii).xval(jp)
	    enddo
	    imdoff=imdoff+1
	  enddo 
	elseif(flag.eq.4) then			! from FINIT_RUN
	endif

	return

	end

	subroutine usersub2(flag)

	integer flag

	INCLUDE 'gsort.inc'

	    if(flag.eq.0) then			! from GETINPUT
	  call gs_syntax('Please replace this Subroutine with your own version')
	elseif(flag.eq.1) then			! from LISTPROG
	elseif(flag.eq.2) then			! from INIT_RUN
	elseif(flag.eq.3) then			! from EVANA
	elseif(flag.eq.4) then			! from FINIT_RUN
	endif

	return

	end

	subroutine usersub3(flag)

	integer flag

	INCLUDE 'gsort.inc'

	    if(flag.eq.0) then			! from GETINPUT
	  call gs_syntax('Please replace this Subroutine with your own version')
	elseif(flag.eq.1) then			! from LISTPROG
	elseif(flag.eq.2) then			! from INIT_RUN
	elseif(flag.eq.3) then			! from EVANA
	elseif(flag.eq.4) then			! from FINIT_RUN
	endif

	return

	end

	subroutine usersub4(flag)

	integer flag

	INCLUDE 'gsort.inc'

	integer spec(0:1023)
	parameter kind=2
	integer pattern(0:31)
	data pattern/	'00000001'X,'00000002'X,'00000004'X,'00000008'X,'00000010'X,'00000020'X,'00000040'X,'00000080'X,
	1		'00000100'X,'00000200'X,'00000400'X,'00000800'X,'00001000'X,'00002000'X,'00004000'X,'00008000'X,
	2		'00010000'X,'00020000'X,'00040000'X,'00080000'X,'00100000'X,'00200000'X,'00400000'X,'00800000'X,
	3		'01000000'X,'02000000'X,'04000000'X,'08000000'X,'10000000'X,'20000000'X,'40000000'X,'80000000'X/

	    if(flag.eq.0) then			! from GETINPUT

	elseif(flag.eq.1) then			! from LISTPROG
	  
	  write(list(llist:),'(a)') 'Pattern spettro di S2+S3   test AC 30/12/97'

	elseif(flag.eq.2) then			! from INIT_RUN
	  do ii=0,1023
	    spec(ii)=0
	  end do
	  
	elseif(flag.eq.3) then			! from EVANA

	  if(ndet(kind).lt.1) return
	  jx=DOFF(kind)
	  ldat=ior(iand(det(jx).ival(2),'00003FF0'X)/16,iand(det(jx).ival(3),'00000FF0'X)*64)
	  do ii=0,31
	    if(iand(ldat,pattern(ii)).ne.0) then
	      spec(ii)=spec(ii)+1
	    endif
	  enddo
	  
	elseif(flag.eq.4) then			! from FINIT_RUN
	  CALL WRITESPECL('pattern.dat',spec,'L',1024,KV)

	endif

	return

	end

	subroutine usersub5(flag)

	integer flag

	INCLUDE 'gsort.inc'

	    if(flag.eq.0) then			! from GETINPUT
	  call gs_syntax('Please replace this Subroutine with your own version')
	elseif(flag.eq.1) then			! from LISTPROG
	elseif(flag.eq.2) then			! from INIT_RUN
	elseif(flag.eq.3) then			! from EVANA
	elseif(flag.eq.4) then			! from FINIT_RUN
	endif

	return

	end

	subroutine usersub6(flag)

	integer flag

	INCLUDE 'gsort.inc'

	    if(flag.eq.0) then			! from GETINPUT
	  call gs_syntax('Please replace this Subroutine with your own version')
	elseif(flag.eq.1) then			! from LISTPROG
	elseif(flag.eq.2) then			! from INIT_RUN
	elseif(flag.eq.3) then			! from EVANA
	elseif(flag.eq.4) then			! from FINIT_RUN
	endif

	return

	end

	subroutine usersub7(flag)

	integer flag

	INCLUDE 'gsort.inc'

	    if(flag.eq.0) then			! from GETINPUT
	  call gs_syntax('Please replace this Subroutine with your own version')
	elseif(flag.eq.1) then			! from LISTPROG
	elseif(flag.eq.2) then			! from INIT_RUN
	elseif(flag.eq.3) then			! from EVANA
	elseif(flag.eq.4) then			! from FINIT_RUN
	endif

	return

	end

	subroutine usersub8(flag)

	integer flag

	INCLUDE 'gsort.inc'

	    if(flag.eq.0) then			! from GETINPUT
	  call gs_syntax('Please replace this Subroutine with your own version')
	elseif(flag.eq.1) then			! from LISTPROG
	elseif(flag.eq.2) then			! from INIT_RUN
	elseif(flag.eq.3) then			! from EVANA
	elseif(flag.eq.4) then			! from FINIT_RUN
	endif

	return

	end

	subroutine usersub9(flag)

	integer flag

	INCLUDE 'gsort.inc'

	    if(flag.eq.0) then			! from GETINPUT
	  call gs_syntax('Please replace this Subroutine with your own version')
	elseif(flag.eq.1) then			! from LISTPROG
	elseif(flag.eq.2) then			! from INIT_RUN
	elseif(flag.eq.3) then			! from EVANA
	elseif(flag.eq.4) then			! from FINIT_RUN
	endif

	return

	end
