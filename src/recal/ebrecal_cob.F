	PROGRAM COBRECAL

! test di rivelatori con la sorgente di 60Co

	PARAMETER (MAXRES=8192)
	PARAMETER (NBORDER=100)

	real spek(0:MAXRES-1)
	real sp1(0:MAXRES-1)
	real sp2(0:MAXRES-1)
	common /spe/sp1,spek,sp2

	character*1 ch
	character fname*260
	character Tfname*260
	character*78 comment
	character*8 ebdetclass(3)

	CHARACTER CDATE*9,CTIME*8

	logical*1 graphics	/.FALSE./
	logical*1 classcycle	/.FALSE./
	logical*1 argcycle	/.FALSE./
	character*128 dirname, tagname

	PARAMETER (MAXSPE=256)
	logical*1 present(MAXSPE)
	integer hpge(MAXSPE)
	real	rvec(15,MAXSPE)
	real	raver(15),waver(15),graver(15),gwaver(15)
	integer ebgetfname
	external ebgetfname
	logical*1 inp_not
	external inp_not
	data ebdetclass /'Cluster ','Clover  ','Tapered '/

	call xinpmode(1)

	fname='GE_ENER#00'
	nchan=8192
	iform=6
	comment=' '
	
	if(iargc() .eq. 1)then
	   write(6,'('' Will cycle on Cluster Clover Tapered'')')
*	   write(ebdetclass(1),'(a8)')'Cluster '
*	   write(ebdetclass(2),'(a8)')'Clover  '
*	   write(ebdetclass(1),'(a8)')'Tapered '
	   classcycle = .TRUE.
	   iform = 8
	   call getarg(1,dirname)
	   call makecstring(dirname)
	elseif(iargc() .gt. 1)then
	   call getarg(1,dirname)
	   call makecstring(dirname)
	   argcycle = .TRUE.
	   iform = 8
	else

1	call readdat(1,FNAME,spek,nchan,iform,kv)
	IF(KV.LE.0) THEN
	  lfname=max(1,lengthc(fname))
	  write(6,*) fname(1:lfname),'   Read error',kv
	  goto 1
	ELSE
	  IFORM=MOD(KV,100)
	  NKAP=(KV/100+1023)/1024
	  NCHAN=NKAP*1024
	  if(nchan.le.2*NBORDER) then
	    lfname=max(1,lengthc(fname))
	    write(6,*) 'Spectrum   ',fname(1:lfname),'   is too short',kv
	    call exit
	  endif
	endif
	
	endif

!	call str_toupper(fname)
	tfname=fname

	nspectra=40
	modprint=8
	if( .not.classcycle .and. .not.argcycle) then
	  nn=inp_i2('# of spectra',nspectra,modprint)
	  if(nn.gt.1) then
	   icoff=-1
	  else
	   icoff=0
	  endif
	  nspectra=min(nspectra,MAXSPE)
	else
	  nspectra = 0
	endif
	  

	reslim=3.0
	call inp_r1('Limiting resolution for the ''good'' average (keV) ',reslim)

	call inp_ask('Graphics',graphics)

	CALL DATE(CDATE)
	CALL TIME(CTIME)

	if( .not.classcycle .and. .not.argcycle) then
	do nspec=1,nspectra
10	  if(graphics) call disp_initt
	  call readdat(0,FNAME,spek,nchan,iform,kv)
	  IF(KV.LE.0) THEN
	    lfname=max(1,lengthc(fname))
	    write(6,*) fname(1:lfname),'   Read error',kv
	    call exit
	  endif
	  itot=0
	  do ii=NBORDER,nchan-NBORDER
	    itot=itot+spek(ii)
	  enddo
	  if(itot.lt.1000) then
	    present(nspec)=.FALSE.
	    call ansi_bell(6)
	  else
	    do ii=0,nchan-1
	      spek(ii)=spek(ii)+1	! per evitare problemi vari
	    enddo
	    call anal_60co(fname,spek,nchan,6,graphics,rvec(1,nspec),iok)
	    if(iok.gt.0) then
	      present(nspec)=.true.
	      if(graphics) then
	        ch=' '
	        ii=inp_ch(' ',ch)
	        if(ii.lt.0) call exit
	        if(ch.eq.'s' .or. ch.eq.'S') goto 10
	        if(ch.eq.'-') then
	          call fndecrem(fname)
	          goto 10
	        endif
	      endif
	    else
	    present(nspec)=.FALSE.
	    call ansi_bell(6)
	    endif
	  endif
	  call fnincrem(fname)
	enddo

	elseif (argcycle) then
	  do iiarg = 2,iargc()
	    call getarg(iiarg,tagname)
	    call makecstring(tagname)
	    do while( ebgetfname(dirname, tagname, fname) .eq.1)
	       nspectra = nspectra+1
	      if(graphics) then
	        call tekanmode
	        call disp_initt
	      endif
	      call readdat(0,FNAME,spek,nchan,iform,kv)
	      IF(KV.LE.0) THEN
	        lfname=max(1,lengthc(fname))
	        write(6,*) fname(1:lfname),'   Read error',kv
	        call exit
	      endif
	      itot=0
	      do ii=NBORDER,nchan-NBORDER
	        itot=itot+spek(ii)
	      enddo
	     if(itot.lt.1000) then
	        present(nspectra)=.FALSE.
	        call ansi_bell(6)
	     else
	        do ii=0,nchan-1
	          spek(ii)=spek(ii)+1	! per evitare problemi vari
	        enddo
	        call anal_60co(fname,spek,nchan,6,graphics,rvec(1,nspectra),iok)
	        if(iok.gt.0) then
	          present(nspectra)=.true.
	          if(graphics) then
	            ch=' '
	            ii=inp_ch(' ',ch)
	            if(ii.lt.0) call exit
	          endif
	        else
	          present(nspectra)=.FALSE.
	          call ansi_bell(6)
	        endif
	     endif
	     do ii=1,60
	       fname(ii:ii)=' '
	     enddo
	      enddo
	     enddo

	elseif (classcycle) then
	  do iiarg = 1,3
	    call makecstring(ebdetclass(iiarg))
	    do while( ebgetfname(dirname, ebdetclass(iiarg), fname) .eq. 1)
	      nspectra = nspectra+1
	      if(graphics) then
	        call tekanmode
	        call disp_initt
	      endif
	      call readdat(0,FNAME,spek,nchan,iform,kv)
	      IF(KV.LE.0) THEN
	        lfname=max(1,lengthc(fname))
	        write(6,*) fname(1:lfname),'   Read error',kv
	        call exit
	      endif
	      itot=0
	      do ii=NBORDER,nchan-NBORDER
	        itot=itot+spek(ii)
	      enddo
	     if(itot.lt.1000) then
	        present(nspectra)=.FALSE.
	        call ansi_bell(6)
	     else
	        do ii=0,nchan-1
	          spek(ii)=spek(ii)+1	! per evitare problemi vari
	        enddo
	        call anal_60co(fname,spek,nchan,6,graphics,rvec(1,nspectra),iok)
	        if(iok.gt.0) then
	          present(nspectra)=.true.
	          if(graphics) then
	            ch=' '
	            ii=inp_ch(' ',ch)
	            if(ii.lt.0) call exit
	          endif
	        else
	          present(nspectra)=.FALSE.
	          call ansi_bell(6)
	        endif
	     endif
	     do ii=1,60
	       fname(ii:ii)=' '
	     enddo
	      enddo
	     enddo
	 endif

	
	

	do jj=1,15
	  raver(jj)=0
	  waver(jj)=0
	  graver(jj)=0
	  gwaver(jj)=0
	enddo

	npres=0
	do ii=1,nspectra
	  if(present(ii)) then
	    do jj=1,15
	      raver(jj)=raver(jj)+rvec(jj,ii)
	    enddo
	    npres=npres+1
	  endif
	enddo
	if(npres.lt.1) call exit
	do ii=1,15
	  raver(ii)=raver(ii)/npres
	enddo

	wpres=0
	do ii=1,nspectra
	  if(present(ii)) then
	    peso=rvec(2,ii)/raver(2)
	    do jj=1,15
	      waver(jj)=waver(jj)+rvec(jj,ii)*peso
	    enddo
	    wpres=wpres+peso
	  endif
	enddo
	do ii=1,15
	  waver(ii)=waver(ii)/wpres
	enddo

	npres=0
	do ii=1,nspectra
	  if(present(ii) .AND. rvec(3,ii).le.reslim) then
	    do jj=1,15
	      graver(jj)=graver(jj)+rvec(jj,ii)
	    enddo
	    npres=npres+1
	  endif
	enddo
	if(npres.lt.1) call exit
	do ii=1,15
	  graver(ii)=graver(ii)/npres
	enddo

	wpres=0
	do ii=1,nspectra
	  if(present(ii) .AND. rvec(3,ii).le.reslim) then
	    peso=rvec(2,ii)/graver(2)
	    do jj=1,15
	      gwaver(jj)=gwaver(jj)+rvec(jj,ii)*peso
	    enddo
	    wpres=wpres+peso
	  endif
	enddo
	do ii=1,15
	  gwaver(ii)=gwaver(ii)/wpres
	enddo

	write(6,*)

	do ii=1,MAXSPE
	  hpge(ii)=-1
	enddo
	open(8,file='GE_POS.DB',status='old',readonly,err=20)
15	read(8,*,end=18,err=18) ii,jj
	if(ii.gt.0 .and.ii.le.MAXSPE) hpge(II)=jj
	goto 15
18	close(8)

20	comment=' '
	do lun=6,7
	  lfname=max(1,lengthc(tfname))
	  write(lun,'(2x,a,2x,a,4x,a,4x,a)') cdate,ctime,'60Co spectra from',tfname(1:lfname)
	  if(lun.eq.7) call inp_str('Comment',comment)
	  lcomment=max(1,lengthc(comment))
	  if(lcomment.gt.0) write(lun,'(2x,a)') comment(1:lcomment)
	  write(lun,*)
	  write(lun,*) ' ##  (Ge)  Ch(1333) A(1333)   FWHM  WTML WTMR  Chi2       P/T'
	  write(lun,*) '           -<Pos>   /<Area>  (keV)    /WHM          ( 50  100  200)'
	  write(lun,*)
	  do ii=1,nspectra
	    if(present(ii)) then
              if(rvec(3,ii).le.reslim) then
	        if(hpge(ii).ne.-1) then
	          write(lun,'(i4,2H (,i3,1H),f8.1,f9.2,f8.2,1x,1H(,f4.2,f5.2,1h),f5.1,f6.1,f5.1,f5.1)')
 	1	    ii+icoff,hpge(ii),rvec(1,ii)-raver(1),rvec(2,ii)/raver(2),
	1	    rvec(3,ii),rvec(4,ii),rvec(5,ii),min(99.9,rvec(6,ii)),
	1	    rvec(7,ii),rvec(8,ii),rvec(9,ii)
	        else
	          write(lun,'(i4,6x         ,f8.1,f9.2,f8.2,1x,1H(,f4.2,f5.2,1h),f5.1,f6.1,f5.1,f5.1)')
 	1	    ii+icoff,         rvec(1,ii)-raver(1),rvec(2,ii)/raver(2),
	1	    rvec(3,ii),rvec(4,ii),rvec(5,ii),min(99.9,rvec(6,ii)),
	1	    rvec(7,ii),rvec(8,ii),rvec(9,ii)
                endif
	      else
	        if(hpge(ii).ne.-1) then
	          write(lun,'(i4,2H*(,i3,1H),f8.1,f9.2,f8.2,1x,1H(,f4.2,f5.2,1h),f5.1,f6.1,f5.1,f5.1)')
 	1	    ii+icoff,hpge(ii),rvec(1,ii)-raver(1),rvec(2,ii)/raver(2),
	1	    rvec(3,ii),rvec(4,ii),rvec(5,ii),min(99.9,rvec(6,ii)),
	1	    rvec(7,ii),rvec(8,ii),rvec(9,ii)
	        else
	          write(lun,'(i4,1h*,5x     ,f8.1,f9.2,f8.2,1x,1H(,f4.2,f5.2,1h),f5.1,f6.1,f5.1,f5.1)')
 	1	    ii+icoff,         rvec(1,ii)-raver(1),rvec(2,ii)/raver(2),
	1	    rvec(3,ii),rvec(4,ii),rvec(5,ii),min(99.9,rvec(6,ii)),
	1	    rvec(7,ii),rvec(8,ii),rvec(9,ii)
                endif
	      endif
	    else
	      write(lun,'(i4)') ii+icoff
	    endif
	    if(mod(ii,modprint).eq.0) write(lun,*)
	  enddo
	  write(lun,'(''   Average'',      f8.1,f9.0,f8.2,1x,1H(,f4.2,f5.2,1h),f5.1,f6.1,f5.1,f5.1)')
 	1	raver(1),raver(2),
	1	raver(3),raver(4),raver(5),min(99.9,raver(6)),
	1	raver(7),raver(8),raver(9)
	  write(lun,'(''   (<'',f4.1,'')'',f8.1,f9.0,f8.2,1x,1H(,f4.2,f5.2,1h),f5.1,f6.1,f5.1,f5.1)')
	1	reslim,
 	1	graver(1),graver(2),
	1	graver(3),graver(4),graver(5),min(99.9,graver(6)),
	1	graver(7),graver(8),graver(9)
	  write(lun,*)
	  write(lun,'(''  wAverage'',      f8.1,f9.0,f8.2,1x,1H(,f4.2,f5.2,1h),f5.1,f6.1,f5.1,f5.1)')
 	1	waver(1),waver(2),
	1	waver(3),waver(4),waver(5),min(99.9,waver(6)),
	1	waver(7),waver(8),waver(9)
	  write(lun,'(''   (<'',f4.1,'')'',f8.1,f9.0,f8.2,1x,1H(,f4.2,f5.2,1h),f5.1,f6.1,f5.1,f5.1)')
	1	reslim,
 	1	gwaver(1),gwaver(2),
	1	gwaver(3),gwaver(4),gwaver(5),min(99.9,gwaver(6)),
	1	gwaver(7),gwaver(8),gwaver(9)
	  write(lun,*)
	  if(lun.eq.6) then
	    if(.not.inp_not('Write these results on FOR007.DAT')) goto 40
	  endif
	enddo

40	if(inp_not('Write calibration coefficents on FOR008.DAT')) then
	  LUN=8
	  call inp_i1('Tape number',ntape)
	  ntape=min(max(0,ntape),9999)
	  do ii=1,nspectra
	    if(present(ii)) then
	      WRITE(lun,'(I5,I5,I3,F10.3,F10.6)') ntape,ii-1,2,rvec(11,ii),rvec(12,ii)
	    endif
	  enddo

	endif

	CALL EXIT

	END

	subroutine anal_60co(fname,spek,nchan,lun1,graphics,rvec,iok)

	PARAMETER (MAXRES=8192)

	character fname*(*)
	integer nchan,lun1
	real spek(0:nchan-1)
	logical*1 graphics
	real rvec(*)

	real speklog(0:MAXRES-1)
	real spektmp(0:MAXRES-1)

	real	par1(10),par2(10)

	external DFCFfun,DFCFbac

	PARAMETER (NBORDER=100)
	PARAMETER (NSMOO=10)

	PARAMETER (ENER1=1173.238)
	PARAMETER (ENER2=1332.513)

	real TWIN0(4)	/100.,900.,400.,700./
	real TWIN1(4)	/100.,500.,100.,400./
	real TWIN2(4)	/500.,900.,100.,400./

	real dwin0(4),dwin1(4),dwin2(4)
	real XSCA0,YSCA0,XSCA1,YSCA1,XSCA2,YSCA2
	
	spek(0)=0
	spek(1)=0
	spek(nchan-1)=0
	do ii=0,nchan-1
	  spektmp(ii)=spek(ii)
	enddo

	do nn=1,NSMOO
	  xx=spektmp(0)
	  do ii=1,nchan-2
	    yy=xx+2*spektmp(ii)+spektmp(ii+1)
	    xx=spektmp(ii)
	    spektmp(ii)=0.25*yy
	  enddo
	enddo

C  trova il picco piu' alto

	valmax1=spektmp(nborder)
	maxcan1=nborder
	do ii=nborder,nchan-nborder
	  val=spektmp(ii)
	  if(val.gt.valmax1) then
	    valmax1=val
	    maxcan1=ii
	  endif
	enddo
	lim1l=0.99*maxcan1
	lim1h=1.01*maxcan1

C  trova il secondo picco piu' alto

	valmax2=spektmp(nborder)
	maxcan2=nborder
	do ii=nborder,nchan-nborder
	 if(ii.lt.lim1l .or. ii.gt.lim1h) then
	  val=spektmp(ii)
	  if(val.gt.valmax2) then
	    valmax2=val
	    maxcan2=ii
	  endif
	 endif
	enddo
	lim2l=0.99*maxcan2
	lim2h=1.01*maxcan2

	if(maxcan1.gt.maxcan2) then
	  call swapl(maxcan1,maxcan2)
	  call swapl(lim1l,lim2l)
	  call swapl(lim1h,lim2h)
	endif

	iok=0
	itot=0
	do ii=lim1l,lim1h
	  itot=itot+spek(ii)
	enddo
	if((itot-(lim1h-lim1l+1)).lt.100) return
	itot=0
	do ii=lim2l,lim2h
	  itot=itot+spek(ii)
	enddo
	if((itot-(lim2h-lim2l+1)).lt.100) return
	iok=1

	call anal_line(spek(lim1l),lim1h-lim1l+1,pos1,area1,fwhm1,back1)
	xpos1=pos1+lim1l
	call anal_line(spek(lim2l),lim2h-lim2l+1,pos2,area2,fwhm2,back2)
	xpos2=pos2+lim2l

	xkevch=(ener2-ener1)/(xpos2-xpos1)
	offset=ener2-xpos2*xkevch

	if(graphics) then
	  call disp_initt
	  nsum=max(1,nchan/2048)
	  nnch=0
	  do ii=0,nchan-1,nsum
	    yy=0
	    do jj=ii,ii+nsum-1
	      yy=yy+spek(ii)
	    enddo
	    if(yy.gt.0) then
	      speklog(nnch)=log(yy)
	    else
	      speklog(nnch)=0.
	    endif
	    nnch=nnch+1
	  enddo
	  
	  call rminmax(speklog(1),nnch-2,ymin,ymax)	! esclusi i margini
	  ymax=ymax*1.05
	  CALL put4LW(dwin0,0.,float(nnch),ymin,ymax)
	  call disp_setscale(dwin0,twin0,xsca0,ysca0)
	  CALL DISP_window(TWIN0)
	  CALL DISP_Yvec(SPEKlog(0),1,nchan,1,2,dwin0,twin0)
	  call disp_anmode
	endif

	lim1l=xpos1-6*fwhm1
	lim1h=xpos1+5*fwhm1
	pos1=xpos1-lim1l
	if(graphics) then
	  do ii=lim1l,lim1h
	    yy=spek(ii)
	    if(yy.gt.0) then
	      speklog(ii)=log(yy)
	    else
	      speklog(ii)=0.
	    endif
	  enddo
	  call rminmax(speklog(lim1l),lim1h-lim1l+1,ymin,ymax)
	  ymin=0.
	  ymax=ymax*1.05
	  CALL put4LW(dwin1,float(lim1l),float(lim1h),ymin,ymax)
	  call disp_setscale(dwin1,twin1,xsca1,ysca1)
	  CALL DISP_window(TWIN1)
	  CALL DISP_Yvec(SPEKlog(1),lim1l,lim1h, 1 ,1,dwin1,twin1)	!!! (1) fino ad aggiustare
	  call disp_anmode
	endif

	call fit_line(spek(lim1l),lim1h-lim1l+1,pos1,area1,fwhm1,back1,par1,chi1)
	call anal_par(par1,area1,pos1,phml1,phmr1,ptml1,ptmr1,pfml1,pfmr1)

	if(graphics) then
	  xsize=lim1h-lim1l+1
	  xstep=2*xsize/(twin1(2)-twin1(1)+1)
	  CALL put4LW(dwin1,0.,xsize,ymin,ymax)
	  CALL DISP_YFUN(DFCFBAC,0.,xsize,xstep,2,dwin1,twin1)
	  CALL DISP_YFUN(DFCFFUN,0.,xsize,xstep,2,dwin1,twin1)
	  call kill_tails
	  CALL DISP_YFUN(DFCFFUN,0.,xsize,xstep,2,dwin1,twin1)
	  call disp_map(phml1+0.5,dwin1(3),tx,ty,dwin1,TWIN1)
	  call disp_line(tx,twin1(3),tx,twin1(4))
	  call disp_map(phmr1+0.5,dwin1(3),tx,ty,dwin1,TWIN1)
	  call disp_line(tx,twin1(3),tx,twin1(4))
	  call disp_anmode
	endif

	lim2l=xpos2-6*fwhm2
	lim2h=xpos2+5*fwhm2
	pos2=xpos2-lim2l+1
	if(graphics) then
	  do ii=lim2l,lim2h
	    yy=spek(ii)
	    if(yy.gt.0) then
	      speklog(ii)=log(yy)
	    else
	      speklog(ii)=0.
	    endif
	  enddo
	  call rminmax(speklog(lim2l),lim2h-lim2l+1,ymin,ymax)
	  ymin=0.
	  ymax=ymax*1.05
	  CALL put4LW(dwin2,float(lim2l),float(lim2h),ymin,ymax)
	  call disp_setscale(dwin2,twin2,xsca2,ysca2)
	  CALL DISP_window(TWIN2)
	  CALL DISP_Yvec(SPEKlog(1),lim2l,lim2h,1,1,dwin2,twin2)
	  call disp_anmode
	endif

	call fit_line(spek(lim2l),lim2h-lim2l+1,pos2,area2,fwhm2,back2,par2,chi2)
	call anal_par(par2,area2,pos2,phml2,phmr2,ptml2,ptmr2,pfml2,pfmr2)

	if(graphics) then
	  xsize=lim2h-lim2l+1
	  xstep=2*xsize/(twin2(2)-twin2(1)+1)
	  CALL put4LW(dwin2,0.,xsize,ymin,ymax)
	  CALL DISP_YFUN(DFCFBAC,0.,xsize,xstep,2,dwin2,twin2)
	  CALL DISP_YFUN(DFCFFUN,0.,xsize,xstep,2,dwin2,twin2)
	  call kill_tails
	  CALL DISP_YFUN(DFCFFUN,0.,xsize,xstep,2,dwin2,twin2)
	  call disp_map(phml2+0.5,dwin2(3),tx,ty,dwin2,TWIN2)
	  call disp_line(tx,twin2(3),tx,twin2(4))
	  call disp_map(phmr2+0.5,dwin2(3),tx,ty,dwin2,TWIN2)
	  call disp_line(tx,twin2(3),tx,twin2(4))
	  call disp_anmode
	endif

	xpos1=pos1+lim1l
	xpos2=pos2+lim2l
	xkevch=(ener2-ener1)/(xpos2-xpos1)
	offset=ener2-xpos2*xkevch

	fwhm1=(phmr1-phml1)*xkevch
	wtml1=( pos1-ptml1)/(pos1-phml1)
	wtmr1=(ptmr1- pos1)/(phmr1-pos1)

	fwhm2=(phmr2-phml2)*xkevch
	wtml2=( pos2-ptml2)/(pos2-phml2)
	wtmr2=(ptmr2- pos2)/(phmr2-pos2)

	nch050=( 50.-offset)/xkevch
	nch100=(100.-offset)/xkevch
	nch200=(200.-offset)/xkevch
	nch300=(300.-offset)/xkevch
	nchtot=(ENER2+10*fwhm2-offset)/xkevch

	nch050=min(nchan,max(1,nch050))
	nch100=min(nchan,max(1,nch100))
	nch200=min(nchan,max(1,nch200))
	nch300=min(nchan,max(1,nch300))
	nchtot=min(nchan,max(1,nchtot))
	if(graphics) then
	  call disp_vecmod
	  call disp_map(float(nch050)/nsum,dwin0(3),tx,ty,dwin0,TWIN0)
	  call disp_line(tx,twin0(3),tx,twin0(4))
	  call disp_map(float(nch100)/nsum,dwin0(3),tx,ty,dwin0,TWIN0)
	  call disp_line(tx,twin0(3),tx,twin0(4))
	  call disp_map(float(nch200)/nsum,dwin0(3),tx,ty,dwin0,TWIN0)
	  call disp_line(tx,twin0(3),tx,twin0(4))
	  call disp_map(float(nch300)/nsum,dwin0(3),tx,ty,dwin0,TWIN0)
	  call disp_line(tx,twin0(3),tx,twin0(4))
	  call disp_map(float(nchtot)/nsum,dwin0(3),tx,ty,dwin0,TWIN0)
	  call disp_line(tx,twin0(3),tx,twin0(4))
	  call disp_anmode
	endif
	area050=0
	do ii=nch050,nchtot
	  area050=area050+spek(ii)-1
	enddo
	area100=0
	do ii=nch100,nchtot
	  area100=area100+spek(ii)-1
	enddo
	area200=0
	do ii=nch200,nchtot
	  area200=area200+spek(ii)-1
	enddo
	area300=0
	do ii=nch300,nchtot
	  area300=area300+spek(ii)-1
	enddo
	areap=area1-(pfmr1-pfml1+1)+area2-(pfmr2-pfml2+1)
	pt050=areap/area050*100
	pt100=areap/area100*100
	pt200=areap/area200*100
	pt300=areap/area300*100

	call textmode
	if(lun1.gt.0) then
	  lfname=lengthc(fname)
	  if(graphics) then
*	    call disp_posabs(0.,twin2(3)-10)
	    write(lun1,*)
	    write(lun1,'(1x,a       ,f8.1,f10.0,f6.2,1x,1H(,f4.2,1H-,f4.2,1h),f5.1,f6.1,f5.1,f5.1)')
 	1	fname(1:lfname),xpos1,area1,fwhm1,wtml1,wtmr1,min(99.9,chi1)
	    write(lun1,'(<lfname+1>x,f8.1,f10.0,f6.2,1x,1H(,f4.2,1H-,f4.2,1h),f5.1,f6.1,f5.1,f5.1)')
 	1	                xpos2,area2,fwhm2,wtml2,wtmr2,min(99.9,chi2),pt050,pt100,pt200
	  else
	    write(lun1,'(1x,a,f8.1,f10.0,f6.2,1x,1H(,f4.2,1H-,f4.2,1h),f5.1,f6.1,f5.1,f5.1)')
 	1	fname(1:lfname),xpos2,area2,fwhm2,wtml2,wtmr2,min(99.9,chi2),pt050,pt100,pt200
	  endif
	endif

	rvec( 1)=xpos2
	rvec( 2)=area2
	rvec( 3)=fwhm2
	rvec( 4)=wtml2
	rvec( 5)=wtmr2
	rvec( 6)=chi2
	rvec( 7)=pt050
	rvec( 8)=pt100
	rvec( 9)=pt200
	rvec(10)=pt300
	rvec(11)=offset
	rvec(12)=xkevch

	return

	end

	subroutine anal_line(region,ncan,pos,area,fwhm,back)

	integer ncan
	real region(0:ncan-1)
	real pos,area,fwhm,back

	valmax=region(0)
	maxcan=0
	do ii=1,ncan-1
	  val=region(ii)
	  if(val.gt.valmax) then
	    valmax=val
	    maxcan=ii
	  endif
	enddo

	nback=max(1,ncan/10)	! 10% di fondo
	liml=0      + (nback-1)
	limh=ncan-1 - (nback-1)

	back=0
	do ii=0,liml
	  back=back+region(ii)
	enddo
	do ii=limh,ncan-1
	  back=back+region(ii)
	enddo
	back=back/(2*nback)
	
	xc0=0
	xc1=0
	xc2=0
	xd0=0
	xd1=0
	xd2=0
	do ii=liml+1,limh-1
	   xii=ii+0.5		! al centro del canale
	   xyy=region(ii)
	   xsy=xyy-back
	   xsy=ABS(xsy)+ABS(back)
	   xc0=xc0+xyy
	   xc1=xc1+xyy*xii
	   xc2=xc2+xyy*xii*xii
	   xd0=xd0+xsy
	   xd1=xd1+xsy*xii
	   xd2=xd2+xsy*xii*xii
	enddo
	area=xc0
	darea=sqrt(xd0)
	if(xc0.eq.0) xco=1
	pos=xc1/xc0
	dpos=(xd2-2*xd1*pos+xd0*pos**2)/xc0**2
	dpos=sqrt(ABS(dpos))
	fwhm=xc2/xc0-pos**2
	fwhm=2.35482*sqrt(fwhm)

	return

	end

	subroutine fit_line(region,ncan,pos,area,fwhm,back,fpar,chi)

	PARAMETER (SQ2PI=2.506628275)

	external CFfun,CFder,CFchi


	integer ncan
	real region(ncan)
	real pos,area,fwhm,back
	real fpar(10),chi

	integer npar
	real par(10)
	integer free(10)
	real amin,amax,pmin,pmax,smin,smax
	integer ndat
	real dat(1000),err2(1000)
	common /fitcom/ npar,par,free,amin,amax,pmin,pmax,smin,smax,
	1		ndat,dat,err2

	ndat=ncan

	do ii=1,ndat
	  yy=region(ii)
	  dat(ii)=yy
	  if(yy.ne.0) then
	    err2(ii)=1./dat(ii)
	  else
	    err2(ii)=1.
	  endif
	enddo

	sigma=fwhm/2.35482
	ym=dat(int(pos))
	do ii=int(pos-fwhm),int(pos+fwhm)
	  if(dat(ii).gt.ym) then
	    ym=dat(ii)
	    pos=ii
	  endif
	enddo

	par(1)=back
	par(2)=area/SQ2PI/sigma
	par(3)=pos
	par(4)=sigma
	par(5)=0.001
	par(6)=pos-1000.*sigma
	par(7)=pos+1000.*sigma

	amin=par(2)/2
	amax=par(2)*2
	pmin=par(3)-fwhm
	pmax=par(3)+fwhm
	smin=par(4)/4
	smax=par(4)*4

	npar=7
	do ii=1,5
	  free(ii)=0
	enddo
	free(6)=-1
	free(7)=-1
	call curfit(CFder,CFchi,chi)

	amin=par(2)*0.8
	amax=par(2)*1.2
	pmin=par(3)-par(4)/5
	pmax=par(3)+par(4)/5
	smin=par(4)*0.8
	smax=par(4)*1.2

	par(6)=par(3)-2*par(4)
	par(7)=par(3)+2*par(4)
	free(6)=0
	free(7)=0
	call curfit(CFder,CFchi,chi)

	amin=par(2)*0.8
	amax=par(2)*1.2
	pmin=par(3)-par(4)/5
	pmax=par(3)+par(4)/5
	smin=par(4)*0.8
	smax=par(4)*1.2

	do ii=1,7
	  free(ii)=0
	enddo
	call curfit(CFder,CFchi,chi)

	do ii=1,10
	  fpar(ii)=par(ii)
	enddo
	fpar(3)=fpar(3)-0.5
	chi=chi/(ndat-npar)

	return

	end

	subroutine anal_par(par,area,pos,phml,phmr,ptml,ptmr,pfml,pfmr)

	real par(10)
	real area,phml,phmr,ptml,ptmr,pfml,pfmr

	real xpar(10)

	do ii=1,10
	  xpar(ii)=par(ii)
	enddo
	xpar(1)=0
	xpar(5)=0

	step=par(4)/50
	pos=par(3)
	amp=CFfun(pos,xpar)
	do xx=par(3)-par(4),par(3)+par(4)
	  yval=CFfun(xx,xpar)
	  if(yval.gt.amp) then
	    amp=yval
	    pos=xx
	  endif	  
	enddo

	amph=amp/2
	ampt=amp/10
	ampf=amp/50

	phml=pos-step
	yval=CFfun(phml,xpar)
	dowhile (yval.gt.amph)
	  phml=phml-step
	  yval=CFfun(phml,xpar)
	enddo
	
	phmr=pos+step
	yval=CFfun(phmr,xpar)
	dowhile (yval.gt.amph)
	  phmr=phmr+step
	  yval=CFfun(phmr,xpar)
	enddo
	
	ptml=pos-step
	yval=CFfun(ptml,xpar)
	dowhile (yval.gt.ampt)
	  ptml=ptml-step
	  yval=CFfun(ptml,xpar)
	enddo
	
	ptmr=pos+step
	yval=CFfun(ptmr,xpar)
	dowhile (yval.gt.ampt)
	  ptmr=ptmr+step
	  yval=CFfun(ptmr,xpar)
	enddo
	
	area=0

	pfml=pos-step
	yval=CFfun(pfml,xpar)
	area=area+yval
	dowhile (yval.gt.ampf)
	  pfml=pfml-step
	  yval=CFfun(pfml,xpar)
	  area=area+yval
	enddo
	
	pfmr=pos+step
	yval=CFfun(pfmr,xpar)
	area=area+yval
	dowhile (yval.gt.ampf)
	  pfmr=pfmr+step
	  yval=CFfun(pfmr,xpar)
	  area=area+yval
	enddo
	
	area=area*step
	pos=par(3)

	return

	end
	  
	subroutine kill_tails

	integer npar
	real par(10)
	integer free(10)
	real amin,amax,pmin,pmax,smin,smax
	integer ndat
	real dat(1000),err2(1000)
	common /fitcom/ npar,par,free,amin,amax,pmin,pmax,smin,smax,
	1		ndat,dat,err2

	par(6)=par(3)-1000.*par(4)
	par(7)=par(3)+1000.*par(4)

	return

	end

	function CFfun(xx,xpar)

	real xx
	real xpar(10)

	integer npar
	real par(10)
	integer free(10)
	real amin,amax,pmin,pmax,smin,smax
	integer ndat
	real dat(1000),err2(1000)
	common /fitcom/ npar,par,free,amin,amax,pmin,pmax,smin,smax,
	1		ndat,dat,err2

C	Par(1)=B	fondo costante
C	Par(2)=A	ampiezza
C	Par(3)=P	posizione
C	Par(4)=S	sigma
C	Par(5)=s	ampiezza step
C	Par(6)=L	attacco coda sinistra
C	Par(7)=R	attacco coda destra
C
C	y=(xx-P)/S
C	yL=(L-P)/S
C	yR=(R-P)/S
C
C	Funct(y)= B + A * ( F1 + F2 )
C
C	  = exp(-0.5*yL*(2*y-yL))
C	F1= exp(-0.5*y**2)
C	  = exp(-0.5*yR*(2*y-yR))
C	F2=s/(1+exp(y))**2

	amp=xpar(2)
	pos=xpar(3)
	sig=xpar(4)
	y =(xx-pos)/sig
	xL=xpar(6)
	xR=xpar(7)
	if(xx.lt.xL) then
	  yL=(xL-pos)/sig
	  yy=0.5*yL*(2.*y-yL)
	elseif(xx.gt.xR) then
	  yR=(xR-pos)/sig
	  yy=0.5*yR*(2.*y-yR)
	else
	  yy=0.5*y*y
	endif
	F1=exp(-yy)
	if(y.lt.-10) then
	  F2=xpar(5)
	elseif(y.gt.10) then
	  F2=0.
	else
	  ey=exp(y)
	  eyp1i=1./(1.+ey)
	  F2=xpar(5)*eyp1i*eyp1i
	endif

	CFfun= xpar(1)+amp*(F1+F2)

	return

	end

	function CFder(id,deriv,dyi,wi)

	integer id
	real deriv(*)

	integer npar
	real par(10)
	integer free(10)
	real amin,amax,pmin,pmax,smin,smax
	integer ndat
	real dat(1000),err2(1000)
	common /fitcom/ npar,par,free,amin,amax,pmin,pmax,smin,smax,
	1		ndat,dat,err2

	xx=id
	dyi=dat (id)-CFFUN(xx,par)
	wi =err2(id)

	amp=par(2)
	pos=par(3)
	sig=par(4)
	y=(xx-par(3))/par(4)
	xL=par(6)
	xR=par(7)

	if(y.lt.-10) then
	  F2=par(5)
	  DYF2=0.
	  deriv(5)= amp
	elseif(y.gt.10) then
	  F2=0.
	  DYF2=0.
	  deriv(5)= 0.
	else
	  ey=exp(y)
	  eyp1i=1./(1.+ey)
	  F2=par(5)*eyp1i*eyp1i
	  DYF2=-2.*ey*F2*eyp1i
	  deriv(5)= amp*eyp1i*eyp1i
	endif

	amps=amp/sig

	if(xx.lt.xL) then
	  yL=(xL-pos)/sig
	  yy=0.5*yL*(2.*y-yL)
	  F1=exp(-yy)
	  deriv(6)= amps*F1*(yL-y)
	  deriv(7)= 0	  
	elseif(xx.gt.xR) then
	  yR=(xR-pos)/sig
	  yy=0.5*yR*(2.*y-yR)
	  F1=exp(-yy)
	  deriv(6)= 0	  
	  deriv(7)= amps*F1*(yR-y)
	else
	  yy=0.5*y*y
	  F1=exp(-yy)
	  deriv(6)= 0	  
	  deriv(7)= 0	  
	endif

	deriv(1)=1.
	deriv(2)=F1+F2
	deriv(3)= amps*(  F1*y  - DYF2)
	deriv(4)= amps*(2*F1*yy - DYF2*y)

	do ii=1,7
	  if(free(ii).lt.0) deriv(ii)=0.
	enddo

	CFder=1

	return

	end

	function CFchi(xpar)

	real xpar(*)

	integer npar
	real par(10)
	integer free(10)
	real amin,amax,pmin,pmax,smin,smax
	integer ndat
	real dat(1000),err2(1000)
	common /fitcom/ npar,par,free,amin,amax,pmin,pmax,smin,smax,
	1		ndat,dat,err2

	chi=0
	do ii=1,ndat
	  xval=CFfun(float(ii),xpar)
	  chi=chi+err2(ii)*(dat(ii)-xval)**2
	enddo

	CFchi=chi

	return

	end

	function DFCFfun(xx)

	real xx

	integer npar
	real par(10)
	integer free(10)
	real amin,amax,pmin,pmax,smin,smax
	integer ndat
	real dat(1000),err2(1000)
	common /fitcom/ npar,par,free,amin,amax,pmin,pmax,smin,smax,
	1		ndat,dat,err2

	yy=CFfun(xx,par)

	if(yy.gt.0) then
	   DFCFfun=log(yy)
	else
	   DFCFfun=0.
	endif

	return

	end

	function DFCFbac(xx)

	real xx

	integer npar
	real par(10)
	integer free(10)
	real amin,amax,pmin,pmax,smin,smax
	integer ndat
	real dat(1000),err2(1000)
	common /fitcom/ npar,par,free,amin,amax,pmin,pmax,smin,smax,
	1		ndat,dat,err2

	y=(xx-par(3))/par(4)
	if(y.lt.-10) then
	  step=par(2)*par(5)
	elseif(y.gt.10) then
	  step=0
	else
	  step=par(2)*par(5)/(1+exp(y))**2
	endif

	yy= par(1)+ step

	if(yy.gt.0) then
	   DFCFbac=log(yy)
	else
	   DFCFbac=0.
	endif

	return

	end

	subroutine curfit(Fderiv,Fchisq,chisqr)

	integer npar
	real par(10)
	integer free(10)
	real amin,amax,pmin,pmax,smin,smax
	integer ndat
	real dat(1000),err2(1000)
	common /fitcom/ npar,par,free,amin,amax,pmin,pmax,smin,smax,
	1		ndat,dat,err2

	Real Deriv(100)
	Real alpha(5050),beta(100),sq(100)
	Real array(5050),bvec(100),var(100)

	NTpar=(NPar*(NPar+1))/2

	chisqr=Fchisq(par)

	DO Ii=1,NPar
	   Var(Ii)=Par(Ii)
	enddo

	FLAMDA=0.001
	iter=0

300	chisqt=chisqr
	DO J=1,NPar
	   BETA(J)=0.
	enddo
	DO J=1,NTpar
	   ALPHA(J)=0.
	enddo

	DO i=1,Ndat
	  Kv=Fderiv(i,Deriv,dyi,wi)
	  DO J=1,NPar
	    derj=deriv(j)
	    if(derj.ne.0.) then
		BETA(J)=BETA(J)+wi*dyi*DERJ
		widerj=wi*derj
		L=(J*(J-1))/2
		DO  K=1,J
		   L=L+1
		   ALPHA(L)=ALPHA(L)+DERIV(K)*wiDERJ
		enddo
	    endif
	  enddo
	enddo

	DO J=1,NPar
	   JJ=J*(J+1)/2    
	   IF(ALPHA(JJ).LT.1.E-15) ALPHA(JJ)=1.E-15  
	   SQ(J)=SQRT(ALPHA(JJ))    
	   BETA(J)=BETA(J)/SQ(J)    
	enddo

100	L=1
	DO J=1,NPar
	   bvec(j)=beta(j)
	   DO K=1,J
		ARRAY(L)=ALPHA(L)/(SQ(J)*SQ(K)) 
		L=L+1                           
	   enddo
	   ARRAY(J*(J+1)/2)=1.+FLAMDA 
	enddo

	CALL LinGls(Array,Bvec,NPar,KV)
	IF(KV.NE.0) then
	   chisqr=-1
	   return
	endif

	DO J=1,NPar
	   Var(J)=Par(J)+Bvec(J)/SQ(J)
	enddo

c	Limiti sulle variabili 

	var(1)=max(1.0   ,var(1))
	var(2)=min(amax  ,max(amin,var(2)))
	var(3)=min(pmax  ,max(pmin,var(3)))
	var(4)=min(smax  ,max(smin,var(4)))
	var(5)=max(1.0E-5,var(5))
	var(6)=min(var(3),var(6))
	var(7)=max(var(3),var(7))

	chisq1=Fchisq(var)

	if (chisq1.gt.chisqr) then
	   FLAMDA=10.*FLAMDA            
	   IF(FLAMDA.le.1.E20) GOTO 100
	   chisqr=-2
	   return
	endif

	DO J=1,NPar  
	   Par(J)=Var(J)    
	enddo

	CHISQR=CHISQ1  
	if(chisqr.ne.0) then
	   DCHI=(CHISQT-CHISQR)/CHISQR*100.  
	else
	   dchi=0
	endif


	FLAMDA=0.1*FLAMDA  
	flamda=max(flamda,1.e-7)
	ITER=ITER+1   
	IF ((DCHI.GT.0.1).and.(iter.lt.20))  GOTO 300 

	RETURN

	END

	SUBROUTINE LinGls(Array,Par,Nord,Iflag)

C         PARAMETER
C            Array  ARRAY WITH PACKED MATRIX
C            Par    PARAMETER VECTOR FOR I/O
C            Nord   ORDER OF MATRIX <=100
C            Iflag  <> 0  ERROR

	DIMENSION Array(1),Par(1)

	INTEGER   Lsign(100)
	DATA Epsi /1.E-18/	! square of MAXIMUM RELATIV PRECISION

	Iflag=0
	IF(Nord.LT.1) RETURN
	
	DO Iord=1,Nord
	   Lsign(Iord)=0
	   NN=Iord*(Iord-1)/2
	   II=NN+Iord
	   Sum=Array(II)
	   IF(Iord.ne.1) then
	      DO JJ=1,Iord-1
		JI=NN+JJ
		IF(Lsign(JJ).eq.0) then
		   Sum=Sum-Array(JI)*Array(JI)
		else
		   Sum=Sum+Array(JI)*Array(JI)
		endif
	      enddo
	   endif
	   IF(Sum.lt.0) then
		Sum=-Sum
		Lsign(Iord)=1
	   endif
	   IF(Sum.LT.Epsi) then
		Iflag=1
		RETURN
	   endif
	   Array(II)=SQRT(Sum)
	   IF(Iord.ne.Nord) then
	     DO KK=Iord+1,Nord
		NK=KK*(KK-1)/2
		IK=NK+Iord
		Sum=Array(IK)
		IF(Iord.ne.1) then
		  DO JJ=1,Iord-1
		    JI=NN+JJ
		    JK=NK+JJ
		    IF(Lsign(JJ).eq.0) then
			Sum=Sum-Array(JI)*Array(JK)
		    else
			Sum=Sum+Array(JI)*Array(JK)
		    endif
		  enddo
		endif
		IF(Lsign(Iord).NE.0) Sum=-Sum
		Array(IK)=Sum/Array(II)
	     enddo
	   endif
	   Sum=Par(Iord)
	   IF(Iord.ne.1) then
	     DO JJ=1,Iord-1
		JI=NN+JJ
		IF(Lsign(JJ).eq.0) then
		   Sum=Sum-Array(JI)*Par(JJ)
		else
		   Sum=Sum+Array(JI)*Par(JJ)
		endif
	     enddo
	   endif
	   IF(Lsign(Iord).NE.0) Sum=-Sum
	   Par(Iord)=Sum/Array(II)
	enddo

	DO II=Nord,1,-1
	   Sum=Par(II)
	   IF(II.ne.Nord) then
	      DO JJ=II+1,Nord
		IJ=JJ*(JJ-1)/2+II
		Sum=Sum-Par(JJ)*Array(IJ)
	      enddo
	   endif
	   Par(II)=Sum/Array(II*(II+1)/2)
	enddo

	RETURN

	END

	subroutine rminmax(spek,nn,ymin,ymax)

	real spek(nn)

	ymin=spek(1)
	ymax=spek(nn)

	do ii=1,nn
	  ymin=min(ymin,spek(ii))
	  ymax=max(ymax,spek(ii))
	enddo

	return

	end
