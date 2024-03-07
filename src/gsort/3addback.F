To be inserted sometime ...
************************************** test 3 addback
	elseif(nn.EQ.3) then
*****************
	  ni3 = ni1+1
	  is1=mod(det(ni1).id-ioff,QSEG)
	  is2=mod(det(ni2).id-ioff,QSEG)
	  is3=mod(det(ni3).id-ioff,QSEG)
	  ener1=det(ni1).xval(jp)
	  ener2=det(ni2).xval(jp)
	  ener3=det(ni3).xval(jp)
	  etot=ener1+ener2+ener3
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
	  rctet3 = 1.00 - (etot/ener3-1.000)*511.00/etot
	  if( rctet3 .gt. 1.000 .or. rctet3 .lt. -1.000)then
	   possible3 = .false.
	  else
	   possible3 = .true.
	  endif
	  
	  if( possible1 .and. possible2 .and. possible3 )then
	    if( rctet1 .ge. rctet2 .and. rctet1 .ge. rctet3 )then
	  	 possible2 = .false.
	  	 possible3 = .false.
	    elseif( rctet2 .gt. rctet1 .and. rctet2 .ge. rctet3 )then
	  	 possible1 = .false.
	  	 possible3 = .false.
	    elseif( rctet3 .gt. rctet1 .and. rctet3 .gt. rctet2 )then
	  	 possible1 = .false.
	  	 possible2 = .false.
	    endif
	  elseif( possible1 .and. possible2 .and. (.not.possible3) )then
	    if( rctet1 .ge. rctet2 )then
	  	 possible2 = .false.
	    else
	         possible1 = .false.
	    endif
	  elseif( possible1 .and. (.not.possible2) .and. possible3 )then
	    if( rctet1 .ge. rctet3 )then
	  	 possible3 = .false.
	    else
	         possible1 = .false.
	    endif
	  elseif( (.not.possible1) .and. possible2 .and. possible3 )then
	    if( rctet2 .ge. rctet3 )then
	  	 possible3 = .false.
	    else
	         possible2 = .false.
	    endif
	  endif
	  
	  NextHit = -1
	  if( possible1 )then
	    etot2 = etot - ener1
	    rctet2 = 1.00 - (etot2/ener2-1.000)*511.00/etot2
	    if( rctet2 .gt. 1.000 .or. rctet2 .lt. -1.000)then
	     possible2 = .false.
	    else
	     possible2 = .true.
	    endif
	    rctet3 = 1.00 - (etot2/ener3-1.000)*511.00/etot2
	    if( rctet3 .gt. 1.000 .or. rctet3 .lt. -1.000)then
	     possible3 = .false.
	    else
	     possible3 = .true.
	    endif
	    
	    if( possible2 .and. possible3 )then
	      if( rctet2 .ge. rctet3 )then
	           possible3 = .false.
	      else
	           possible2 = .false.
	      endif
	    endif
	    if (possible2) NextHit = is2
	    if (possible3) NextHit = is3
	    possible2 = .false.
	    possible3 = .false.
	  elseif( possible2 )then
	    etot2 = etot - ener2
	    rctet1 = 1.00 - (etot2/ener1-1.000)*511.00/etot2
	    if( rctet1 .gt. 1.000 .or. rctet1 .lt. -1.000)then
	     possible1 = .false.
	    else
	     possible1 = .true.
	    endif
	    rctet3 = 1.00 - (etot2/ener3-1.000)*511.00/etot2
	    if( rctet3 .gt. 1.000 .or. rctet3 .lt. -1.000)then
	     possible3 = .false.
	    else
	     possible3 = .true.
	    endif
	    
	    if( possible1 .and. possible3 )then
	      if( rctet1 .ge. rctet3 )then
	           possible3 = .false.
	      else
	           possible1 = .false.
	      endif
	    endif
	    if (possible1) NextHit = is1
	    if (possible3) NextHit = is3
	    possible1 = .false.
	    possible3 = .false.

	  elseif( possible3 )then
	    etot2 = etot - ener3
	    rctet1 = 1.00 - (etot2/ener1-1.000)*511.00/etot2
	    if( rctet1 .gt. 1.000 .or. rctet1 .lt. -1.000)then
	     possible1 = .false.
	    else
	     possible1 = .true.
	    endif
	    rctet2 = 1.00 - (etot2/ener2-1.000)*511.00/etot2
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
	    if (possible1) NextHit = is1
	    if (possible2) NextHit = is2
	    possible1 = .false.
	    possible2 = .false.
	  endif	     

	    if(possible1 .and. (NextHit .ne. -1)) then      
              det(ni1).xval(jp)=etot
              det(ni1).ival(jp)=etot
	      det(ni2).id=-1
	      det(ni3).id=-1
	      if( abpl.nsegs .eq. 4)then
		abpl.polar(QOFF+det(ni1).id)=Qpol(is1,NextHit)
	      elseif ( abpl.nsegs .eq. -1) then
	        abpl.polar(det(ni1).id)=Qpol(is1,NextHit)
	      endif
	      nelim = nelim + 2
	    elseif(possible2 .and. (NextHit .ne. -1)) then      
              det(ni2).xval(jp)=etot
              det(ni2).ival(jp)=etot
	      det(ni1).id=-1
	      det(ni3).id=-1
	      if( abpl.nsegs .eq. 4)then
		abpl.polar(QOFF+det(ni2).id)=Qpol(is2,NextHit)
	      elseif ( abpl.nsegs .eq. -1) then
	        abpl.polar(det(ni2).id)=Qpol(is2,NextHit)
	      endif
	      nelim = nelim + 2
	    elseif(possible3 .and. (NextHit .ne. -1)) then      
              det(ni3).xval(jp)=etot
              det(ni3).ival(jp)=etot
	      det(ni1).id=-1
	      det(ni2).id=-1
	      if( abpl.nsegs .eq. 4)then
		abpl.polar(QOFF+det(ni3).id)=Qpol(is3,NextHit)
	      elseif ( abpl.nsegs .eq. -1) then
	        abpl.polar(det(ni3).id)=Qpol(is3,NextHit)
	      endif
	      nelim = nelim + 2
	    else
	     do jj=ni1,ni2
	      det(jj).id=-1
	     end do
	     nelim=nelim+nn
	    endif
	    


************************************* end 3 addback
