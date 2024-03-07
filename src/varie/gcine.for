	PROGRAM QCINE

	REAL MASSP,MASST,MASSC,MASSD,MASSR
	INTEGER*4 ZP,ZT,ZC,ZD,ZR
	INTEGER*4 AP,AT,AC,AD,AR
	CHARACTER*6 NAMP,NAMT,NAMC,NAMD,NAMR,NAMNUCL
	PARAMETER CCMSEC=29979250000.
	PARAMETER XMNMEV=931.478
	PARAMETER PIGRE=3.141592654
	PARAMETER DEGSURAD=57.29577951

	PARAMETER MAXDET=80

	character*60 file_si
	character*80 line

	logical*1 out7 /.false./

	real theta_si(MAXDET),phi_si(MAXDET)
	real cosdir_si(3,MAXDET)

	real xmom_CM(3),xmom_D(3),xmom_R(3)

	do ii=1,maxdet
	  theta_si(ii)=-1
	enddo
	nsil=0

CCCCCCCCCCCCCCCCCCCC Angles of si detectors

	file_si='~bazzacco/bin/NUMBERING.SI'
	call inp_ch('File with angles of si detectors',file_si)
	open(1,file=file_si,status='old')
3	read(1,'(A)',ERR=4,END=4) line
	lline=lengthc(line)
	if(lline.gt.0) then
	  call str_toupper(line)
	  lnn=index(line,'RIV#')
	  if(lnn.gt.0) then
	    read(line(lnn+5:),*) nn
	    lth=index(line,'THETA=')
	    read(line(lth+7:),*) th
	    lph=index(line,'PHI=')
	    read(line(lph+5:),*) ph
	    theta_si(nn+1)=th
	    phi_si(nn+1)=ph
	    nsil=max(nsil,nn+1)
	  endif
	endif
	goto 3

4	close(1)
	do ii=1,nsil
	  if(theta_si(ii).ge.0) then
	    th=theta_si(ii)/DEGSURAD
	    ph=phi_si(ii)/DEGSURAD
	    cosdir_si(1,ii)=sin(th)*cos(ph)
	    cosdir_si(2,ii)=sin(th)*sin(ph)
	    cosdir_si(3,ii)=cos(th)
	  endif
	enddo


CCCCCCCCCCCCCCCCCCCC Reaction
 
10	WRITE(6,*)
	CALL INP_CH('Projectile        ',NAMP)
	CALL NUCLNAM(NAMP,ZP,AP)
	IF(ZP.LT.0 .OR. AP.LE.0) GOTO 10
	CALL XMASSM(ZP,AP,XMASSP,DMASSP)
	MASSP=XMASSP+AP*XMNMEV

	CALL INP_CH('Target            ',NAMT)
	CALL NUCLNAM(NAMT,ZT,AT)
	IF(ZT.LT.0 .OR. AT.LE.0) GOTO 10
	CALL XMASSM(ZT,AT,XMASST,DMASST)
	MASST=XMASST+AT*XMNMEV
 
	ZC=ZP+ZT
	AC=AP+AT
	CALL XMASSM(ZC,AC,XMASSC,DMASSC)
	MASSC=XMASSC+AC*XMNMEV
 
	QVALC=XMASSP+XMASST-XMASSC
	DQVALC=SQRT(DMASSP**2+DMASST**2+DMASSC**2)

	CALL INP_R1('Projectile energy (MeV) ',EPROJ)
	IF(EPROJ.LE.0) GOTO 10

	EofCM=EPROJ*MASSP/MASSC
	VofCM=SQRT(2*EofCM/MASSC)
	ECCM=EPROJ-EofCM

	CALL INP_CH('Detected particle ',NAMD)
	CALL NUCLNAM(NAMD,ZD,AD)
	IF(ZD.LT.0 .OR. AD.LE.0) GOTO 10
	MASSD=0
	IF(ZD.NE.0 .OR. AD.NE.0) THEN
	  CALL XMASSM(ZD,AD,XMASSD,DMASSD)
	  MASSD=XMASSD+AD*XMNMEV
	ENDIF
 
	ZR=ZC-ZD
	AR=AC-AD
	IF(ZR.LT.0 .OR. AR.LT.0) GO TO 10
	CALL XMASSM(ZR,AR,XMASSR,DMASSR)
	MASSR=XMASSR+AR*XMNMEV
 
	QVALD=XMASSC-(XMASSR+XMASSD)
	DQVALD=SQRT(DMASSC**2+DMASSR**2+DMASSD**2)

	CALL INP_R1('Detected Particle Energy (in CM MeV)',EDCM)
	IF(EDCM.LE.0) GOTO 10
	VDCM=SQRT(2*EDCM/MASSD)

	ERCM=ECCM+QVALD-EDCM
	IF(ERCM.LT.0) GOTO 10

	WRITE(6,*)
	WRITE(6,'('' Projectile        '',I3,A2,6X,''MASS ='',F8.4,'' AMU       E ='',F6.1,'' MeV'')')
	1	 AP,NAMNUCL(ZP),MASSP/XMNMEV,EPROJ
	WRITE(6,'('' Target            '',I3,A2,6X,''MASS ='',F8.4,'' AMU'')')
	1	 AT,NAMNUCL(ZT),MASST/XMNMEV
	WRITE(6,'('' Compound nucleus  '',I3,A2,6X,''MASS ='',F8.4,'' AMU       Ex='',f6.1,'' MeV'')')
	1	 AC,NAMNUCL(ZC),MASSC/XMNMEV,ECCM
	WRITE(6,'('' Detected particle '',I3,A2,6X,''MASS ='',F8.4,'' AMU       E ='',f6.1,'' MeV in CM'')')
	1	 AD,NAMNUCL(ZD),MASSD/XMNMEV,EDCM
	WRITE(6,'('' Residue           '',I3,A2,6X,''MASS ='',F8.4,'' AMU       Ex='',f6.1,'' MeV'')')
	1	 AR,NAMNUCL(ZR),MASSR/XMNMEV,ERCM

	WRITE(6,'('' v/c of Center of Mass    '',f10.4 ''%'')') VofCM*100
	WRITE(6,'('' v/c of Det. Part. in CM  '',f10.4 ''%'')') VDCM*100

	ThCM=0
	PhCM=0
	xmom_CM(1)=massC*vofCM*sin(thCM)*cos(phCM)
	xmom_CM(2)=massC*vofCM*sin(thCM)*sin(phCM)
	xmom_CM(3)=massC*vofCM*cos(thCM)

	WRITE(6,*)
	CALL INP_R1('Minimum LAB Energy of Detected Particle (MeV)',EDMIN)

	WRITE(6,*)
	call inp_ask('Write also for007.dat file',out7)

	WRITE(6,*)
	write(6,*) '  SI#   Theta  ThetaCM   Ed(MeV)   v/c(%)   ThetaR  vR/c(%)'
	WRITE(6,*)

	xxx=vdcm/vofcm
	xx2=xxx*xxx
	DO II=1,nsil
	  THD=Theta_si(ii)/DEGSURAD
	  if(thD.ge.0) then
	    PHD=Theta_si(ii)/DEGSURAD
	    sss=xx2-sin(thd)**2
	    if(sss.ge.0) then
	      sss=sqrt(sss)
	      vd1=vofcm*(cos(thd)+sss)
	      ed1=0.5*massd*vd1**2
	      if(vd1.ge.0 .AND. ed1.ge.edmin) then
		xmom_D(1)=massd*vd1*sin(THD)*cos(PHD)
		xmom_D(2)=massd*vd1*sin(THD)*sin(PHD)
		xmom_D(3)=massd*vd1*cos(THD)
		xmomr=0
		do jj=1,3
		  xmom_r(jj)=xmom_cm(jj)-xmom_d(jj)
		  xmomr=xmomr+xmom_r(jj)**2
		enddo
		ER=xmomr/2./massr
		xmomr=sqrt(xmomr)
		vr=xmomr/massr
		THR=acos(xmom_r(3)/xmomr)*DEGSURAD
		THDCM=atan2(sin(THD)*vd1/vdcm,cos(THD)*vd1/vdcm-1./xxx)*DEGSURAD
	        write(6,'(i5,f9.2,f9.2,f9.1,f9.3,f9.2,f9.3)') ii,Theta_si(ii),THDCM,ed1,vd1*100,THR,vr*100
	if(out7)write(7,'(i5,          f6.1               )') ii,                   ed1
	        vd2=vofcm*(cos(thd)-sss)
	        ed2=0.5*massd*vd2**2
	        if(vd2.ge.0 .AND. ed2.ge.edmin) then
	          xmom_D(1)=massd*vd2*sin(THD)*cos(PHD)
		  xmom_D(2)=massd*vd2*sin(THD)*sin(PHD)
		  xmom_D(3)=massd*vd2*cos(THD)
		  xmomr=0
		  do jj=1,3
		    xmom_r(jj)=xmom_cm(jj)-xmom_d(jj)
		    xmomr=xmomr+xmom_r(jj)**2
		  enddo
		  ER=xmomr/2./massr
		  xmomr=sqrt(xmomr)
		  vr=xmomr/massr
		  THR=acos(xmom_r(3)/xmomr)*DEGSURAD
		  THDCM=atan2(sin(THD)*vd2/vdcm,cos(THD)*vd2/vdcm-1./xxx)*DEGSURAD
	          write(6,'(14x    ,f9.2,f9.1,f9.3,f9.2,f9.3)') THDCM,ed2,vd2*100,THR,vr*100
	  if(out7)write(7,'(5x,          f6.1               )')       ed2
		endif
	      endif
	    endif
	  endif
	enddo

	END


	subroutine xmassm(iz,ia,ex,dex)

c	reads for given values of iz,ia
c	the mass excess ex and its error dex, both in MeV
c	from file ~bazzacco/bin/AWMASS.DAT

	character namnucl*2
	data lun /0/
	
	if(lun .LE. 0) call lib$get_lun(lun)

	if(iz.eq.0 .and. ia.eq.0) then
	  ex=0
	  dex=0
	  return
	endif

	open(unit=lun,file='~bazzacco/bin/AWMASS.DAT',type='old',form='formatted')
	read(lun,*)
	read(lun,*)
	read(lun,*)

10      read(lun,'(2I3,1X,F11.3,F9.3)',err=100,end=100) na,nz,ex,dex
	if(na.EQ.ia .AND. nz.EQ.iz) then
	  ex=ex/1000.
	  dex=dex/1000.
	  goto 200
	endif
	if(na .LE. ia) goto 10

100	write(6,'(/'' MASS-EXCESS OF '',i3,a2,'' NOT FOUND'')')ia,namnucl(iz)
	call inp_r2('INPUT MASS-EXCESS AND ERROR (MeV) :',ex,dex)
        	
200	close(unit=lun)
	return

	end
