c	program ban_testbanana
c
c
c	PARAMETER BRES1=64
c	PARAMETER BRES2=64
c
c	integer banana(2,0:bres1-1)
c	character*60 banfile
c
c	irun=-1
c	iadc=-1
c	call inp_ch('Banana file',banfile)
c	call inp_i2('RUN, ADC ',irun,iadc)
c
c	call ban_getbanana(6,banfile,irun,iadc,banana,bres1,bres2,iok)
c	if(iok.lt.0) write(6,*) 'Failed'
c
c	do ii=0,BRES1-1
c	  if(banana(1,ii).GE.0 .OR. banana(2,ii).ge.0) write(6,*) ii,banana(1,ii),banana(2,ii)
c	enddo
c
c	call exit
c
c	end

	subroutine ban_getbanana(iol,banfile,irun,iadc,banana,bres1,bres2,np)

	logical*1 inp_yes
	external inp_yes
	integer iol,irun,iadc,bres1,bres2,np
	character banfile*(*)
	integer banana(2,0:bres1-1)

	integer lun /-1/

10	if(lun.lt.0) call lib$get_lun(lun)
cvax	open(unit=lun,file=banfile,status='old',defaultfile='.ban',readonly,err=20)
	open(unit=lun,file=banfile,status='old',readonly,err=20)

	call ban_findbanana(lun,irun,iadc,iok)
	if(iok.le.0)  goto 22

	call ban_readbanana(lun,banana,bres1,bres2,nbpoints)
	if(nbpoints.lt.1) goto 24

	write(iol,*) nbpoints,'  banana points read'
	write(iol,*) 'Corresponding memory map'
	call ban_listbanana(iol,banana,bres1,bres2,64,32)

	close(unit=lun)
	np=nbpoints
	return

20	lbanfile=lengthc(banfile)
	write(iol,*) 'Error opening   ',banfile(1:lbanfile)
	goto 50

*22	write(iol,'(a,i4,a,i3,a)') ' Banana for  RUN',irun,'    ADC',iadc,'    Not found'
*	goto 51
22	write(iol,'(a,i3,a)') ' Banana for  ADC',iadc,' not found, ADC will be ignored'
	goto 51

24	write(iol,*) 'No banana points'
	goto 50
	
50	if(inp_yes('Retry')) goto 10
51	np=-1
	continue
	return

	end


	subroutine ban_getbanomap(iol,banfile,irun,iadc,banana,bres1,bres2,np)

	logical*1 inp_yes
	external inp_yes
	integer iol,irun,iadc,bres1,bres2,np
	character banfile*(*)
	integer banana(2,0:bres1-1)

	integer lun /-1/

10	if(lun.lt.0) call lib$get_lun(lun)
cvax	open(unit=lun,file=banfile,status='old',defaultfile='.ban',readonly,err=20)
	open(unit=lun,file=banfile,status='old',readonly,err=20)

	call ban_findbanana(lun,irun,iadc,iok)
	if(iok.le.0)  goto 22

	call ban_readbanana(lun,banana,bres1,bres2,nbpoints)
	if(nbpoints.lt.1) goto 24

*	write(iol,*) nbpoints,'  banana points read'
*	write(iol,*) 'Corresponding memory map'
*	call ban_listbanana(iol,banana,bres1,bres2,64,32)

	close(unit=lun)
	np=nbpoints
	return

20	lbanfile=lengthc(banfile)
	write(iol,*) 'Error opening   ',banfile(1:lbanfile)
	goto 50

*22	write(iol,'(a,i4,a,i3,a)') ' Banana for  RUN',irun,'    ADC',iadc,'    Not found'
*	goto 51
22	write(iol,'(a,i3,a)') ' Banana for  ADC',iadc,' not found, ADC will be ignored'
	goto 51

24	write(iol,*) 'No banana points'
	goto 50
	
50	if(inp_yes('Retry')) goto 10
51	np=-1
	continue
	return

	end



	subroutine ban_findbanana(lun,run,adc,iok)

	integer lun,run,adc,iok

	character*80 line

	iok=1
	if(run.lt.0) then
	  if(adc.lt.0) then
5	    read(lun,*,err=5,end=100) ix,iy ! strip-off all comments
	    backspace(lun)
	    return
	  else
	    rewind(lun)
10	    read(lun,'(a)',err=10,end=100) line
	    call str_toupper(line)
	    ladc=index(line,'ADC')
	    if(ladc.le.0) goto 10
	    read(line(ladc+3:),*,err=10,end=10) iadc
	    if(iadc.ne.adc) goto 10
	    return
	  endif
	else
20	  read(lun,'(a)',err=20,end=100) line
	  call str_toupper(line)
	  lrun=index(line,'RUN')
	  if(lrun.le.0) goto 20
	  read(line(lrun+3:),*,err=20,end=20) irun
	  if(irun.ne.run) goto 20
	  if(adc.lt.0) return
	  ladc=index(line,'ADC')
	  if(ladc.le.0) goto 20
	  read(line(ladc+3:),*,err=20,end=20) iadc
	  if(iadc.ne.adc) goto 20
	  return
	endif

100	iok=0
	return

	end

	subroutine ban_readbanana(lun,banana,bres1,bres2,nbpoints)

	integer lun,bres1,bres2,nbpoints
	integer banana(2,0:bres1-1)
	logical last

	nbpoints=0
	do ii=0,bres1-1
	  banana(1,ii)=-1
	  banana(2,ii)=-1
	enddo

	read(lun,*,err=90,end=90) ix,iy

	ix0=min(max(ix,0),bres1-1)
	iy0=min(max(iy,0),bres2-1)
	nbpoints=1

	ix1=ix0
	iy1=iy0
	last=.false.

10	read(lun,*,err=80,end=80) ix2,iy2
	ix2=min(max(ix2,0),bres1-1)
	iy2=min(max(iy2,0),bres2-1)
	nbpoints=nbpoints+1

20	if(ix2.ge.ix1) then
	  istep=1
	else
	  istep=-1
	endif
	if(ix2.ne.ix1) then
	  slope=float(iy2-iy1)/float(ix2-ix1)
	  offset=iy1-slope*ix1
	else
	  slope=0
	  offset=iy2
	endif
	do ix=ix1,ix2,istep
	  iy=offset+slope*ix
	  iy=min(max(0,iy),bres2-1)
	  if(banana(1,ix).eq.-1) then
	    banana(1,ix)=iy
	  elseif(banana(2,ix).eq.-1) then
	    banana(2,ix)=iy
	  else
	    banana(1,ix)=min(iy,banana(1,ix))
	    banana(2,ix)=max(iy,banana(2,ix))
	  endif
	  if(banana(1,ix).gt.banana(2,ix)) call swapl(banana(1,ix),banana(2,ix))
	enddo

	if(last) goto 70
	ix1=ix2
	iy1=iy2
	goto 10

70	do ii=0,bres1-1
	  if(banana(1,ii).eq.-1) then
	    banana(2,ii)=-1
	  elseif(banana(2,ii).ne.-1) then
	    if(banana(1,ii).gt.banana(2,ii)) call swapl(banana(1,ii),banana(2,ii))
	  endif
	enddo
	return

80	if(ix1.eq.ix0 .AND. iy1.eq.iy0) goto 70
	last=.true.
	ix2=ix0
	iy2=iy0
	goto 20

90	return

	end

	subroutine ban_makebanana(xy,npoints,banana,bres1,bres2)

	integer bres1,bres2,nbpoints
	integer banana(2,0:bres1-1)
*	integer xy(2,max(1,npoints))
	integer xy(2,1)
	logical last

	do ii=0,bres1-1
	  banana(1,ii)=-1
	  banana(2,ii)=-1
	enddo
	nbpoints=0

c	read(lun,*,err=90,end=90) ix,iy
	ix=xy(1,1)
	iy=xy(2,1)

	ix0=min(max(ix,0),bres1-1)
	iy0=min(max(iy,0),bres2-1)
	nbpoints=1

	ix1=ix0
	iy1=iy0
	last=.false.

c10	read(lun,*,err=80,end=80) ix2,iy2
10	if(nbpoints.gt.npoints) goto 80
	ix2=xy(1,nbpoints)
	iy2=xy(2,nbpoints)
	ix2=min(max(ix2,0),bres1-1)
	iy2=min(max(iy2,0),bres2-1)
	nbpoints=nbpoints+1

20	if(ix2.ge.ix1) then
	  istep=1
	else
	  istep=-1
	endif
	if(ix2.ne.ix1) then
	  slope=float(iy2-iy1)/float(ix2-ix1)
	  offset=iy1-slope*ix1
	else
	  slope=0
	  offset=iy2
	endif
	do ix=ix1,ix2,istep
	  iy=offset+slope*ix
	  iy=min(max(0,iy),bres2-1)
	  if(banana(1,ix).eq.-1) then
	    banana(1,ix)=iy
	  elseif(banana(2,ix).eq.-1) then
	    banana(2,ix)=iy
	  else
	    banana(1,ix)=min(iy,banana(1,ix))
	    banana(2,ix)=max(iy,banana(2,ix))
	  endif
	  if(banana(1,ix).gt.banana(2,ix)) call swapl(banana(1,ix),banana(2,ix))
	enddo

	if(last) goto 70
	ix1=ix2
	iy1=iy2
	goto 10

70	do ii=0,bres1-1
	  if(banana(1,ii).eq.-1) then
	    banana(2,ii)=-1
	  elseif(banana(2,ii).ne.-1) then
	    if(banana(1,ii).gt.banana(2,ii)) call swapl(banana(1,ii),banana(2,ii))
	  endif
	enddo
	return

80	if(ix1.eq.ix0 .AND. iy1.eq.iy0) goto 70
	last=.true.
	ix2=ix0
	iy2=iy0
	goto 20

c90	return

	end

	subroutine ban_listbanana(iol,banana,bres1,bres2,lres1,lres2)

	integer iol,bres1,bres2,lres1,lres2
	integer banana(2,0:bres1-1)

	character*256 line
	character*3 nm_fmt

	fx=bres1/lres1
	fy=bres2/lres2

        write(nm_fmt,'(i3)')lres1+2
	write(iol,'(7x,'//nm_fmt//'(''-''))')
*	write(iol,'(7x,<lres1+2>(''-''))')
	do iy=lres2-1,0,-1
	  iy1=iy*fy
	  iy2=(iy+1)*fy-1
	  line=' '
	  do ix=0,lres1-1
	    ix1=ix*fx
	    ix2=(ix+1)*fx-1
	    do ixx=ix1,ix2
	    do iyy=iy1,iy2
	      if(banana(1,ixx).le.iyy .and. iyy.le.banana(2,ixx)) goto 10
	    enddo
	    enddo
	    goto 20
10	    line(ix+1:ix+1)='*'
20	    continue
	  enddo
	  if(iy.eq.0) then
	    write(iol,'(1x,i5,1x,A)') 0,'|'//line(1:lres1)//'|'
	  elseif(iy.eq.lres2-1) then
	    write(iol,'(1x,i5,1x,A)') bres2-1,'|'//line(1:lres1)//'|'
	  else
	    write(iol,'(7x,A)') '|'//line(1:lres1)//'|'
	  endif
	enddo
        write(nm_fmt,'(i3)')lres1+2
	write(iol,'(7x,'//nm_fmt//'(''-''))')
*	write(iol,'(7x,<lres1+2>(''-''))')
	line=' '
	write(line(1:1),'(i1)') 0
	write(line(lres1-4:),'(i5)') bres1-1
	write(iol,'(8x,A)') line(1:lres1)

	return

	end
