	PROGRAM EFFI

	external efteo
	PARAMETER NSOURCES=11	
	PARAMETER MAXNGAMMA=20
	PARAMETER NPEAKS=NSOURCES*MAXNGAMMA
	PARAMETER MAXNTERMS=5
	logical*1 DIS1,DIS2,WRIT,WRIN,CADC,NORM,AUTO
	character*120 fdata,filres,fname,fout,line,dummy

	character*5 source(0:NSOURCES)
	integer       ngamma(NSOURCES),isource(NSOURCES),hm(NSOURCES)

	real  gamma(MAXNGAMMA,NSOURCES),dgamma(MAXNGAMMA,NSOURCES)
	real egamma(MAXNGAMMA,NSOURCES),fgamma(MAXNGAMMA,NSOURCES)

	character*5 lab(NPEAKS)
	integer    nsou(NPEAKS),nref(NPEAKS)
	real     energy(NPEAKS),area(NPEAKS),inten(NPEAKS),einte(NPEAKS)
	real        fac(NPEAKS),dfac(NPEAKS),  eff(NPEAKS), deff(NPEAKS)
	real          x(NPEAKS),   y(NPEAKS),   dy(NPEAKS)
	real         xx(NPEAKS),  yy(NPEAKS),  ddy(NPEAKS)

	real coe(MAXNTERMS),kpch
        common /fit/ nterms,coe
	double precision chi
	integer inp_i1
	external inp_i1

	data ibaud/19200/
	data source/'     ',
	1           ' 22Na',' 56Co',' 57Co',' 60Co',' 88Y ','133Ba',
	2	    '134Cs','137Cs','152Eu','226Ra','241Am'/

	data ngamma / 2,14,3,2,3,9,9,1,11,17,2/

	data gamma  / 511.006 ,1274.545 ,  18*0.0 ,
	1	      846.772 ,1037.840 ,1175.102 ,1238.282 , 1360.25 ,
	1	     1771.351 ,2015.181 ,2034.755 ,2598.458 ,3201.962 ,
	1	     3253.416 ,3272.990 ,3451.152 ,3547.925 ,   6*0.0 ,
	2 	      14.4130 ,122.0614 ,136.4743 ,  17*0.0 ,
	3	     1173.238 ,1332.513 ,  18*0.0 ,
	4 	      898.045 ,1836.062 ,2734.087 ,  17*0.0 ,
	5	       53.156 ,  79.623 ,  80.999 , 160.609 , 223.116 ,
	5	      276.404 , 302.858 , 356.014 , 383.859 ,  11*0.0 ,
	6	       475.36 ,  563.27 ,  569.30 ,  604.68 ,  795.78 ,
	6	       801.86 , 1038.53 , 1167.89 , 1365.17 ,  11*0.0 ,
	7	      661.661 ,  19*0.0 ,
	8	     121.7793 ,244.6927 ,344.2724 , 411.111 , 443.979 ,
	8	      778.890 , 964.014 ,1085.793 ,1089.700 ,1112.070 ,
	8	     1407.993 ,   9*0.0 ,
	9 	      186.211 , 241.981 , 295.213 , 351.921 , 609.312 ,
	9 	      768.356 , 934.061 ,1120.287 ,1238.110 ,1377.669 ,
	9 	     1509.228 ,1729.595 ,1764.494 ,1847.420 ,2118.551 ,
	9 	     2204.215 ,2447.810 ,   3*0.0 ,
	1 	       26.345 ,  59.537 ,  18*0.0 /
	data dgamma /   0.001 ,   0.017 ,  18*0.0 ,
	1	        0.013 ,   0.015 ,   0.016 ,   0.017 ,    0.07 ,
	1	        0.026 ,   0.028 ,   0.029 ,   0.033 ,   0.046 ,
	1	        0.045 ,   0.045 ,   0.047 ,   0.061 ,   6*0.0 ,
	2 	       0.0003 ,  0.0001 ,  0.0003 ,  17*0.0 ,
	3	        0.015 ,   0.018 ,  18*0.0 ,
	4 	        0.012 ,   0.025 ,   0.030 ,  17*0.0 ,
	5	        0.005 ,   0.005 ,   0.004 ,   0.025 ,   0.035 ,
	5	        0.007 ,   0.005 ,   0.009 ,   0.009 ,  11*0.0 ,
	6	         0.05 ,    0.05 ,    0.03 ,    0.02 ,    0.02 ,
	6	         0.03 ,    0.05 ,    0.06 ,    0.10 ,  11*0.0 ,
	7	        0.003 ,  19*0.0 ,
	8	       0.0003 ,  0.0008 ,   0.0017,   0.008 ,   0.006 ,
	8	        0.009 ,   0.034 ,   0.034 ,   0.034 ,   0.070 ,
	8	        0.035 ,   9*0.0 ,
	9	        0.010 ,   0.008 ,   0.008 ,   0.008 ,   0.007 ,
	9	        0.010 ,   0.012 ,   0.010 ,   0.012 ,   0.012 ,
	9	        0.015 ,   0.015 ,   0.014 ,   0.025 ,   0.030 ,
	9	        0.040 ,   0.100 ,   3*0.0 ,
	1	        0.001 ,   0.001 ,  18*0.0 /
	data egamma /  199.86 ,   99.93 ,  18*0.0 ,
	1	      100000. ,  14000. ,   2280. ,  67600. ,   4330. ,
	1	       15700. ,   3080. ,   7890. ,  16900. ,   3040. ,
	1	        7410. ,   1750. ,    875. ,    180. ,   6*0.0 ,
	2 	          9.8 ,    85.6 ,    11.1 ,  17*0.0 ,
	3	         100. ,    100. ,  18*0.0 ,
	4 	         91.3 ,   99.34 ,    0.72 ,  17*0.0 ,
	5	         34.8 ,    37.7 ,    512. ,    10.5 ,     7.1 ,
	5	         113. ,    292. ,   1000. ,    145. ,  11*0.0 ,
	6	         15.1 ,    85.9 ,    158. ,   1000. ,    875. ,
	6	         89.5 ,    10.2 ,    18.5 ,    31.1 ,  11*0.0 ,
	7	          85. ,  19*0.0 ,
	8	        1362. ,    358. ,   1275. ,    107. ,    148. ,
	8	         619. ,    692. ,    465. ,     82. ,    649. ,
	8	        1000. ,   9*0.0 ,
	9                  9. ,   16.06 ,   42.01 ,   80.42 ,    100. ,
	9                10.9 ,    6.93 ,   32.72 ,   12.94 ,    8.87 ,
	9	         4.78 ,    6.29 ,   34.23 ,    4.52 ,    2.53 ,
	9	        10.77 ,    3.32 ,   3*0.0 ,
	1 	          2.4 ,    35.7 ,  18*0.0 /
	data fgamma /    0.04 ,    0.02 ,  18*0.0 ,
	1	           0. ,    100. ,     20. ,    700. ,     40. ,
	1	         150. ,     30. ,     70. ,    150. ,     30. ,
	1	          65. ,     20. ,     10. ,      5. ,   6*0.0 ,
	2 	          0.4 ,     0.4 ,     0.3 ,  17*0.0 ,
	3	          0.0 ,     0.0 ,  18*0.0 ,
	4 	          0.7 ,    0.07 ,    0.07 ,  17*0.0 ,
	5	          0.7 ,     0.9 ,      4. ,     0.3 ,     0.2 ,
	5	           2. ,      3. ,      3. ,      2. ,  11*0.0 ,
	6	          0.4 ,     0.9 ,      2. ,      3. ,      8. ,
	6	          0.9 ,     0.2 ,     0.3 ,     0.4 ,  11*0.0 ,
	7	          0.5 ,  19*0.0 ,
	8	          16. ,      6. ,     19. ,      1. ,      2. ,
	8	           8. ,      9. ,      7. ,      1. ,      9. ,
	8	           3. ,   9*0.0 ,
	9 	          0.0 ,     0.0 ,     0.0 ,     0.0 ,     0.0 ,
	9 	          0.0 ,     0.0 ,     0.0 ,     0.0 ,     0.0 ,
	9 	          0.0 ,     0.0 ,     0.0 ,     0.0 ,     0.0 ,
	9 	          0.0 ,     0.0 ,   3*0.0 ,
	1 	          0.1 ,     0.5 ,  18*0.0 /

10	format(1x,i2,2x,f9.4,2x,f9.1,2x,f10.4,3x,a5,1x,a5)

C	call errset(29,.TRUE.,.FALSE.,.TRUE.,.FALSE.)
	call initt(ibaud)
	write(6,*)

9	iend=1
	iread=0
	CADC=.FALSE.
	NORM=.TRUE.
	WRIT=.TRUE.
	WRIN=.FALSE.
	DIS1=.FALSE.
	DIS2=.TRUE.
	AUTO=.FALSE.
	fdata='fit00.dat'
	fout='spec.eff'
	filres='coef.eff'

	call anmode
	call inp_str('Filename to fit: ',fdata)
	open(unit=7,file=fdata,status='old',readonly)
	CADC=inp_NOT('Want to cycle on ADC''s')
	if (CADC) then
	  iend=0
	  NORM=inp_YES('Normalize efficiencies')
	endif
	WRIT=inp_YES('Write out efficiency spectra')
	if (WRIT) then
	  nchan=8192
	  kpch=1.0
	  call inp_i1('Number of channels ',nchan)
	  call inp_r1('keV/chan ',kpch)
	endif
	WRIN=inp_NOT('Write out fit results')
	if (WRIN) then
	  call inp_str('Filename output: ',filres)
	  open(unit=2,file=filres,status='new')
	  write(2,*)'        Log10(Efficiency)=Pol(Log10(Energy))'
	  write(2,*)'        -----------NOT NORMALIZED-----------' 
	endif
	DIS2=inp_YES('Display fit')

	write(6,*)
	do i=1,NSOURCES
   	  write(6,'(i5,3x,a)') i,source(i)
	enddo
	nis=inp_ia('Input source number(s)',isource,NSOURCES)
	if(nis.le.0) then
	  nis=NSOURCES
	  do i=1,nis
            isource(i)=i
	  enddo
	endif

11	do i=1,NPEAKS
	  energy(i)=0.	
	  area(i)=0.			
	  inten(i)=0.			
	  einte(i)=0.			
	  eff(i)=0.			
	  deff(i)=0.			
	  fac(i)=0.
	  dfac(i)=0.
	  lab(i)='     '
	  nsou(i)=0			
	  nref(i)=0			
	enddo

	do i=1,NSOURCES
   	  hm(i)=0
	enddo

	ipk=0
	ipkold=0
	itot=0		
12      read(7,'(1x,a,t20,i5,4g)',end=14)line,ipk,pp,en,ar,fw
	if (ipk.gt.ipkold) then
	  iline=iline+1
	  if (iline.eq.1 .and. CADC) then
	    read(line(1:19),'(a)') fname
	    lenf=index(fname,' ')-1
	    ldir=index(line,']')
	    ldot=index(line(ldir+1:),'.')
	    ldot=ldot+ldir
	    if(ldot.gt.2) read(line(ldot-2:ldot-1),'(i2)') iadc
	  endif
	  itot=itot+1
	  do j=1,nis
	    do i=1,ngamma(isource(j))
	      gm=gamma(i,isource(j))
	      energy(itot)=en
	      area(itot)=ar
	      if (en.ge.(gm-1.).and.en.le.(gm+1.)) then
	        hm(isource(j))=hm(isource(j))+1
	        inten(itot)=egamma(i,isource(j))
	        einte(itot)=fgamma(i,isource(j))
	        if (nsou(itot).eq.0) then
		  nsou(itot)=isource(j)
		else
		  write(6,*)
		  write(6,'(a,i4,a,a5,a,a5)') ' Gamma',itot,'. Possible sources:',
	1	    source(isource(nsou(itot))),' ',source(isource(j))
		  write(6,*)
		  close(7)
	          goto 9
	        endif
	      endif
	    enddo
	  enddo
	  ipkold=ipk
	elseif (ipk.lt.ipkold) then
	  backspace(7)
	  iline=0
	  goto 15
	endif
	goto 12

14	iend=1
	close(7)
15	iread=iread+1
	if (iread.eq.1) then
	  write(6,*)
	  do k=1,nis
	    write(6,'(a,i2,a,a)')' ',hm(isource(k)),' gammas from source ', source(isource(k)) 
	  enddo
	endif

	do i=1,itot
	  if (nsou(i).gt.0) then
	    eff(i)=area(i)/inten(i)
	    def1=sqrt(area(i))/inten(i)
	    def2=area(i)*einte(i)/(inten(i))**2
	    deff(i)=sqrt(def1**2+def2**2)
	  endif
	enddo

	mng=0
	do i=1,nis
	  if (hm(isource(i)).gt.mng) then
	    mng=hm(isource(i))	
	    ns=isource(i)		
	  endif
	enddo

	write(6,*)
	if (nis.gt.1 .and. iread.eq.1) then
	   call inp_i1('Source to make pre-fit',ns)	
	   if (DIS2) DIS1=inp_NOT('Display pre-fit')
	endif

20	if (NORM .or.(.not.NORM .and. iread.eq.1)) then
	  efm1=0.
	  defm1=0.
	  do i=1,itot
	    if (nsou(i).eq.ns .and. eff(i).gt.efm1) then
	      efm1=eff(i)
	      defm1=deff(i)
	    endif
	  enddo
	endif

	do i=1,itot
	  if (nsou(i) .eq. ns) then
	    eff(i)=eff(i)/efm1		
	    def1=deff(i)/efm1
	    def2=eff(i)*defm1/(efm1*efm1)	
	    deff(i)=sqrt(def1**2+def2**2)
	    fac(i)=1.
	    dfac(i)=0.
	    lab(i)='    *'
	  endif
	enddo

C	call errset(29,.TRUE.,.FALSE.,.TRUE.,.FALSE.)
	call initt(ibaud)
	write(6,'(a20,a<lenf>)') '                    ',fname
	write(6,*)
	write(6,*) ' #    Energy     Area      Efficiency  Source   Fit'
	write(6,*)
	do i=1,itot
	  write(6,10) i,energy(i),area(i),eff(i),source(nsou(i)),lab(i)
	enddo
	if (AUTO) goto 23
	ii=0
        IIC=inp_i1('To delete a line type its number ( <RET> to continue )',ii)
        if(ii.le.0 .or. ii.gt.itot) goto 23
        itot=itot-1
        do jj=ii,itot
          energy(jj)=energy(jj+1)
	  area(jj)=area(jj+1)
	  inten(jj)=inten(jj+1)	   
	  einte(jj)=einte(jj+1)
          eff(jj)=eff(jj+1)
	  deff(jj)=deff(jj+1)
	  fac(jj)=fac(jj+1)
	  dfac(jj)=dfac(jj+1)
	  lab(jj)=lab(jj+1)
	  nsou(jj)=nsou(jj+1)
	  nref(jj)=nref(jj+1)
	enddo
	goto 20

23	if (nis.eq.1) goto 24
	jcount=0			
	do i=1,itot
	  if (fac(i).gt.0.) then
	    jcount=jcount+1
	    x(jcount)=energy(i)		
	    y(jcount)=eff(i)		
	    dy(jcount)=deff(i)		
	  endif
	enddo

	nor=jcount-1		
	if (nor.gt.4) nor=4		
	nterms=nor+1
	do i=1,MAXNTERMS
	  coe(i)=0.
	enddo
	do i=1,jcount
	   xx(i)=log10(x(i))
	   yy(i)=log10(y(i))
	   ddy(i)=log10(dy(i))
	enddo
	call polifit(xx,yy,ddy,jcount,nterms,1,coe,chi)
	if (DIS1) then
	  call display(x,y,dy,jcount,fname,lenf)
	  if (.not.AUTO) call inp_str('Press <RET> to continue: ',dummy)
	endif
C	call errset(29,.TRUE.,.FALSE.,.TRUE.,.FALSE.)
	call initt(ibaud)
	write(6,'(a20,a<lenf>)') '                    ',fname
	write(6,*)
	write(6,*) ' #    Energy     Area      Efficiency   Source   Fit'
	write(6,*)
	do i=1,itot
	  write(6,10) i,energy(i),area(i),eff(i),source(nsou(i)),lab(i)
	enddo

	do j=1,nis
	  igref=0
	  aref=0
	  if (isource(j).ne.ns .and. hm(isource(j)).gt.0) then
 	    do i=1,itot
	      if ((nsou(i).eq.isource(j)) .and. (area(i).gt.aref)
	1	  .and. (energy(i).ge.350.)) then
	        igref=i
	        aref=area(i)
	      endif
	    enddo
19	    if (iread.eq.1) then
	      write(6,'(a,a5)') ' For source ',source(isource(j))
	      call inp_i1('Gamma to normalize ',igref)
	      if (nsou(igref).ne.isource(j)) then
	        write(6,'(a,i2,a,a5)') ' Gamma # ',igref,' is not in source ',source(isource(j)) 
	        goto 19
	      endif
	    endif
	    do k=1,itot
	      if (nsou(k).eq.isource(j)) nref(k)=igref
	    enddo
	  endif
	enddo

	do l=1,itot
	  if (nsou(l).gt.0 .and. nsou(l).ne.ns) then
	    fac(l)=efteo(energy(nref(l)))/eff(nref(l))
	    dfac(l)=efteo(energy(nref(l)))*deff(nref(l))/
	1                     (eff(nref(l))*eff(nref(l)))
	  endif
	enddo

	do i=1,itot
	  if (fac(i).gt.0.) then
	    eff(i)=eff(i)*fac(i)
	    def1=deff(i)*fac(i)
	    def2=eff(i)*dfac(i)
	    deff(i)=sqrt(def1**2+def2**2)
	  endif
	enddo

18	if (NORM.or.(.not.NORM .and. iread.eq.1)) then
	  efm2=0.
	  do i=1,itot	
	    if (nsou(i).gt.0) then
	      if (eff(i).gt.efm2) then
	        efm2=eff(i)
	        defm2=deff(i)
	      endif
	    endif
	  enddo
	endif

	do i=1,itot
	  if (nsou(i).gt.0) then
	    eff(i)=eff(i)/efm2
	    def1=deff(i)/efm2
	    def2=eff(i)*defm2/(efm2*efm2)	
	    deff(i)=sqrt(def1**2+def2**2)
	    lab(i)='    *'
	  endif
	enddo

C	call errset(29,.TRUE.,.FALSE.,.TRUE.,.FALSE.)
	call initt(ibaud)
	write(6,'(a20,a<lenf>)') '                    ',fname
	write(6,*)
	write(6,*) ' #    Energy     Area      Efficiency  Source   Fit'
	write(6,*)
	do i=1,itot
	  write(6,10) i,energy(i),area(i),eff(i),source(nsou(i)),lab(i)
	enddo
	if (AUTO) goto 24
	ii=0
        IIC=inp_i1('To delete a line type its number ( <RET> to continue )',ii)
        if(ii.le.0 .or. ii.gt.itot) goto 24
        itot=itot-1
        do jj=ii,itot
          energy(jj)=energy(jj+1)
	  area(jj)=area(jj+1)
	  inten(jj)=inten(jj+1)	   
	  einte(jj)=einte(jj+1)
          eff(jj)=eff(jj+1)
	  deff(jj)=deff(jj+1)
	  fac(jj)=fac(jj+1)
	  dfac(jj)=dfac(jj+1)
	  lab(jj)=lab(jj+1)
	  nsou(jj)=nsou(jj+1)
	  nref(jj)=nref(jj+1)
	enddo
	goto 18

24	jcount=0
	do i=1,itot
	  if (nsou(i).gt.0) then
	    jcount=jcount+1
	    x(jcount)=energy(i)
	    y(jcount)=eff(i)
	    dy(jcount)=deff(i)
	   endif
	enddo

	nor=jcount-1
	if (nor.gt.4) nor=4
	nterms=nor+1
	do i=1,MAXNTERMS
	  coe(i)=0.
	enddo
	do i=1,jcount
	  xx(i)=log10(x(i))
	  yy(i)=log10(y(i))
	  ddy(i)=log10(dy(i))
	enddo
	call polifit(xx,yy,ddy,jcount,nterms,1,coe,chi)
	if (WRIN) then
	  write(2,*)
	  write(2,'(a20,a<lenf>)') '                    ',fname
	  do i=1,nterms
	    write(2,*) i-1,'  order coefficient: ',coe(i)
	  enddo
	  write(2,*)
	endif
	if (DIS2) call display(x,y,dy,jcount,fname,lenf)
	if (.not.AUTO) call inp_str
	1 ('Press <RET> to continue, "g" to go: ',dummy)
	if (dummy.eq.'g' .or. dummy.eq.'G') AUTO=.TRUE.
	if (WRIT) call writef(nchan,kpch,NORM,CADC,iadc,iread,iend,fout)
	if (CADC .and. iend.eq.0) goto 11	
	if (WRIN) close(2)
	call exit
	end

	subroutine writef(nchan,kpch,NORM,CADC,iadc,iread,iend,fname)
	external efteo
	PARAMETER MAXRES=8192
	PARAMETER MAXGER=40
	PARAMETER MAXNCHAN=MAXRES*MAXGER
	logical*1 NORM,CADC

	character*120 fname
	real kpch,spec(MAXNCHAN)
	efmin=0.0001
	efmax=1000.
	istart=(iadc*nchan)+1
	istop=(iadc*nchan)+nchan
	do i=istart,istop
	  j=i-istart+1
	  spec(i)=efteo(float(j)*kpch)
	enddo
	if (NORM .or. (.not.NORM .and. iread.eq.1)) then
	  emax=0.
	  do i=istart,istop
	    if (spec(i).gt.emax) emax=spec(i)
	  enddo
	endif
	do i=istart,istop
	  spec(i)=spec(i)/emax
	  if (spec(i).lt.efmin) spec(i)=0.
	  if (spec(i).gt.efmax) spec(i)=efmax
	enddo

	if (iend.eq.1) then
	  if (CADC) then
	    ntot=MAXGER*nchan
	  else
	    ntot=nchan
	  endif
	  call writedatr(1,fname,spec,ntot,3,KV)
	  if(kv.gt.0) then
	    write(6,*) kv/100,' channels written'
	  else
	    write(6,*) ' error writing spectrum'
	  endif
	endif
	return
	end

	function efteo(x)
	PARAMETER XMIN=30
	real coe(1)
	common /fit/ nterms,coe

	eft=0.
	if (x.le.XMIN) then
	  efteo=0.
	  return
	endif
	do i=1,nterms
	  y=(log10(x))**(i-1)
	  eft=eft+(coe(i)*y)
	enddo
	efteo=10.**eft
	return
	end

	subroutine display(energy,effi,erreffi,nn,fname,lenf)
	PARAMETER XLABELMAX=9
	PARAMETER YLABELMAX=18
	PARAMETER BB=200.
	PARAMETER AA=1000.-BB
	PARAMETER DD=160.
	PARAMETER CC=755.-DD
	dimension energy(1),effi(1),erreffi(1)
	dimension xlabel(XLABELMAX),ylabel(YLABELMAX)
	DATA ibaud/19200/
	DATA xlabel/10.,20.,50.,100.,200.,500.,1000.,2000.,5000./ 
	DATA ylabel/0.01,0.02,0.05,0.1,0.2,0.5,1.0,2.0,5.0,10.0,
	1	    20.0,50.0,100.0,200.0,500.0,1000.0,2000.0,5000.0/

	character*10 line
	character*120 fname
	character*6 xxlabel(XLABELMAX),yylabel(YLABELMAX)

	DATA XXLABEL/'10','20','50','100',
	1 '200','500','1000','2000','5000'/
	DATA YYLABEL/'0.01','0.02','0.05','0.1',
	1 '0.2','0.5','1.0','2.0','5.0','10.0','20.0','50.0',
	2 '100.0','200.0','500.0','1000.0','2000.0','5000.0'/

        xmax=-100.
        xmin=100.
        ymax=-100.
        ymin=100.
        do i=1,nn
          if (energy(i).gt.xmax) xmax=energy(i)
          if (energy(i).lt.xmin) xmin=energy(i)
          if (effi(i).gt.ymax) ymax=effi(i)
          if (effi(i).lt.ymin) ymin=effi(i)
        enddo
        xmax=xmax+xmax/3.
        xmin=xmin-xmin/3.
        ymax=ymax+ymax/3.
        ymin=ymin-ymin/3.
        xnorm=xmax-xmin
        ynorm=ymax-ymin
	
	a=log10(xmax)
	b=0.
	if (xmin.gt.0) b=log10(xmin)
	c=a-b
	d=log10(ymax)
	e=0.
	if (ymin.gt.0) e=log10(ymin)
	f=d-e

C	call errset(29,.TRUE.,.FALSE.,.TRUE.,.FALSE.)
	call initt(ibaud)

	call vecmod
	call movabs(int(BB),int(DD))
	call drwabs(int(AA+BB),int(DD))
	call drwabs(int(AA+BB),int(CC+DD))
	call drwabs(int(BB),int(CC+DD))
	call drwabs(int(BB),int(DD))

	do i=1,XLABELMAX
	  if (xlabel(i) .ge. xmin .and. xlabel(i) .le. xmax) then
	    posx=(log10(xlabel(i))-b)/c
 	    iposx=int((AA*posx)+BB)
	    call movabs(iposx,int(DD))
	    call drwabs(iposx,int(DD)+5)
	    call movabs(iposx,int(CC+DD))
	    call drwabs(iposx,int(CC+DD)-5)

	    len=lengthc(xxlabel(i))
	    lp=(len-1)*5.5
	    call movabs(iposx-lp,int(DD)-20)	
	    call anmode
	    write(6,'(a)') xxlabel(i)
	  endif
	enddo

	do i=1,YLABELMAX
	  if (ylabel(i) .ge. ymin .and. ylabel(i) .le. ymax) then
	    posy=(log10(ylabel(i))-e)/f
 	    iposy=int((CC*posy)+DD)
	    call movabs(int(BB),iposy)
	    call drwabs(int(BB)+5,iposy)
	    call movabs(int(AA+BB),iposy)
	    call drwabs(int(AA+BB)-5,iposy)

	    len=lengthc(yylabel(i))
	    lp=(len-1)*12.5
c	    call movabs(int(BB)-lp,iposy-5)	
	    call movabs(150,iposy-5)	
	    call anmode
	    write(6,'(a)') yylabel(i)
	  endif
	enddo
	call vecmod
	call movabs(0,740)

	do i=1,nn	
	  if (energy(i).gt.0. .and. effi(i).gt.0.) then
	    posx=(log10(energy(i))-b)/c
	    posy=(log10(effi(i))-e)/f
	    iposx=int((AA*posx)+BB)
	    iposy=int((CC*posy)+DD)

	    call movabs(iposx-2,iposy-2)
	    call drwabs(iposx+2,iposy-2)
	    call drwabs(iposx+2,iposy+2)
	    call drwabs(iposx-2,iposy+2)
	    call drwabs(iposx-2,iposy-2)

	    pinf=effi(i)-erreffi(i)
	    posy=(log10(pinf)-e)/f
	    iposy=int((CC*posy)+DD)
	    call movabs(iposx,iposy)
	    psup=effi(i)+erreffi(i)
	    posy=(log10(psup)-e)/f
	    iposy=int((CC*posy)+DD)
	    call drwabs(iposx,iposy)
	  endif
	enddo

	l=0
	do x=xmin,xmax
	  y=efteo(x)
	  if (x.gt.0. )xf=(log10(x)-b)/c
	  if (y.gt.0.) yf=(log10(y)-e)/f
	  if (y .gt. ymin .and. y .lt. ymax) then
	    l=l+1
	    iposx=int((AA*xf)+BB)
	    iposy=int((CC*yf)+DD)
	    if(l.eq.1) call movabs(iposx,iposy)
	    call drwabs(iposx,iposy)
	  endif
	enddo
	call movabs(900,110)
	call anmode
c	write(6,'(a)')'Energy'
	call movabs(20,700)
	call anmode
c	write(6,'(a)')'Efficiency'
	call vecmod
	call movabs(800,700)
	call anmode
	write(6,'(a<lenf>)') FNAME

	call movabs(30,100)
	call anmode
	return
	end

	SUBROUTINE POLIFIT(X,Y,SIGMAY,NPTS,NTERMS,MODE,A,CHISQR)
	DOUBLE PRECISION SUMX,SUMY,XTERM,YTERM,ARRAY,CHISQ
	DIMENSION X(1),Y(1),SIGMAY(1),A(1)
	DIMENSION SUMX(19),SUMY(10),ARRAY(10,10)
11	NMAX=2*NTERMS-1
	DO 13 N=1,NMAX
13 	SUMX(N)=0.
	DO 15 J=1,NTERMS
15	SUMY(J)=0.
	CHISQ=0.
21 	DO 50 I=1,NPTS
	XI=X(I)
	YI=Y(I)
31	IF (MODE)32,37,39
32	IF (YI)35,37,33
33	WEIGHT=1./YI
	GOTO 41
35	WEIGHT=1./(-YI)
	GOTO 41
37	WEIGHT=1.
	GOTO 41
39	WEIGHT=1./SIGMAY(I)**2
41	XTERM=WEIGHT
	DO 44 N=1,NMAX
	SUMX(N)=SUMX(N)+XTERM
44	XTERM=XTERM*XI
45	YTERM=WEIGHT*YI
	DO 48 N=1,NTERMS
	SUMY(N)=SUMY(N)+YTERM
48	YTERM=YTERM*XI
49 	CHISQ=CHISQ+WEIGHT*YI**2
50	CONTINUE
51	DO 54 J=1,NTERMS
	DO 54,K=1,NTERMS
	N=J+K-1
54	ARRAY(J,K)=SUMX(N)
	DELTA=DETERM(ARRAY,NTERMS)
	IF (DELTA)61,57,61
57	CHISQR=0.
	DO 59 J=1,NTERMS
59	A(J)=0.
	GOTO 80
61	DO 70 L=1,NTERMS
62	DO 66 J=1,NTERMS
	DO 65 K=1,NTERMS
	N=J+K-1
65	ARRAY(J,K)=SUMX(N)
66	ARRAY(J,L)=SUMY(J)
70	A(L)=DETERM(ARRAY,NTERMS)/DELTA
71	DO 75 J=1,NTERMS
	CHISQ=CHISQ-2.*A(J)*SUMY(J)
	DO 75 K=1,NTERMS
	N=J+K-1
75	CHISQ=CHISQ+A(J)*A(K)*SUMX(N)
76	FREE=NPTS-NTERMS
	IF (FREE.GT.0) THEN
77	CHISQR=CHISQ/FREE
	ELSE
	CHISQR=0.
	ENDIF
80	RETURN
	END

	FUNCTION DETERM (ARRAY,NORDER)
	DOUBLE PRECISION ARRAY,SAVE
	DIMENSION ARRAY(10,10)
10	DETERM=1.
11	DO 50 K=1,NORDER
	IF (ARRAY(K,K)) 41,21,41
21	DO 23 J=K,NORDER
	IF (ARRAY(K,J)) 31,23,31
23 	CONTINUE
	DETERM=0.
	GOTO 60
31	DO 34 I=K,NORDER
	SAVE=ARRAY(I,J)
	ARRAY(I,J)=ARRAY(I,K)
34	ARRAY(I,K)=SAVE
	DETERM=-DETERM
41    	DETERM=DETERM*ARRAY(K,K)
	IF (K-NORDER) 43,50,50
43	K1=K+1
	DO 46 I=K1,NORDER
	DO 46 J=K1,NORDER
46	ARRAY(I,J)=ARRAY(I,J)-ARRAY(I,K)*ARRAY(K,J)/ARRAY(K,K)
50  	CONTINUE
60	RETURN
	END
