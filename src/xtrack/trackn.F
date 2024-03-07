cpgi$g novector
	PROGRAM TRACKN
	
#include "trackn.inc"

	CHARACTER*2 CMD
	logical*1 MARKOUT

	CHARACTER AUTOCOM(0:9)*60
	INTEGER   LAUTOCOM(0:9)
	logical*1   AUTOSPEC(0:9)
	CHARACTER AUTOLIST(0:9)*60
	INTEGER   LUNLIST(0:9)
	INTEGER   IAUTO	/1/

	CHARACTER DISPCOM*60
	INTEGER   IDISPCOM
	character*200 string
	integer*4 lstring, ii1,ii2
	character*60 lunout_fname
	external DFAUTOBG
	logical*1 getspecname

	integer*4 filexist
	external filexist
	integer*4 fileremove
	external fileremove
	

	CALL STARTPARAMETERS
	call xinpmode(1)
	getspecname = .true.

	AUTOCOM(1)='N'
	LAUTOCOM(1)=1
	AUTOSPEC(1)=.TRUE.
	AUTOLIST(1)='+'
	LUNLIST(1)=0

	AUTOCOM(2)='N'
	LAUTOCOM(2)=1
	AUTOSPEC(2)=.TRUE.
	AUTOLIST(2)='-'
	LUNLIST(2)=0

	AUTOCOM(3)='N'
	LAUTOCOM(3)=1
	AUTOSPEC(3)=.TRUE.
	AUTOLIST(3)='=.+'
	LUNLIST(3)=0

	AUTOCOM(4)='N'
	LAUTOCOM(4)=1
	AUTOSPEC(4)=.TRUE.
	AUTOLIST(4)='=.-'
	LUNLIST(4)=0

	AUTOCOM(0)='CWMWMZ'
	LAUTOCOM(0)=6
	AUTOSPEC(0)=.FALSE.
	AUTOLIST(0)=' '
	LUNLIST(0)=0

	DISPCOM=' '
	IDISPCOM=0

	LUNOUT=0
	CMD='= '			! EMPTY SPECTRUM & FULL DISPLAY
	NCMD=1
	emptyframe=.true.

	GOTO 20

CCCCCCCCCCCCCCCC  Command Input from Keyboard       CCCCCCCCCCCCCC
CCCCCCCCCCCCCCCC             or from command string CCCCCCCCCCCCCC

10	call flush(6)
	IF(CYCLE) THEN
	  NCMD=NCMD+1
	  NAUTO=NAUTO+1
	  IF(NAUTO.GT.LAUTOCOM(IAUTO)) THEN
	    NAUTO=0
	    NCICLO=NCICLO+1
	    BREAK=NCICLO.GT.MAXCICLI
	  ENDIF
	  IF(BREAK) GOTO 100
	  CMD(NCMD:NCMD)=AUTOCOM(IAUTO)(NAUTO:NAUTO)
	  MARKOUT=.TRUE.
	  ix=twin(1)
	  iy=twin(4)
	ELSE
	  CALL xtpget(xinpf,yinpf,IZ)
	  getspecname = .true.
	  if( IZ .gt. 256)then
	     NCMD=2
	     iizz=iz/256
	     iz=iz-256*iizz
	     CMD(1:1)=char(iizz)
	     cmd(2:2)=char(iz)
	  else
	     NCMD=NCMD+1
	     CMD(NCMD:NCMD)=CHAR(IZ)
	  endif
	  IF(NCMD.EQ.2 .AND. CMD(2:2).EQ.' ') THEN
	    CALL xtpbell
	    GOTO 100
	  ENDIF
	  MARKOUT= .false.
	ENDIF
	call getyvalF(yinp,yinpf,ifunct)
	call getxvalF(iinp,xinpf)
	xinp=iinp

	CALL STR_TOUPPER(CMD)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCC  Command Identification Loop CCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

20	IF(CMD(1:NCMD) .EQ. ' ') THEN
	  MARKLE=MARKRI
	  RMARKLE=RMARKRI
	  RMARKRI=XINP
	  MARKRI=XINP
	  jjnm=xinpf
	  xinpf=jjnm
	  IF(.NOT.MARKOUT) then
	     CALL xtpMARKER(XINPF+0.5,5)
	  endif

	ELSEIF(CMD(1:NCMD) .EQ. '*1') THEN
	    getspecname = .true.
	    call xtpgetcomment(string,lstring,ii1,ii2)
	    if( ii1 .gt. 0 )then
	         write(fnamei,'(60('' ''))')
		 call xtpgetcomment(fnamei,lstring,ii1,ii2)
		 ifrmi = ii1
		 nkap = ii2
		 cmd(1:1)='1'
		 ncmd = 1
	         getspecname = .false.
		 goto 20
	     else
	        call xtpbell
	    endif
	ELSEIF(CMD(1:NCMD) .EQ. '*2') THEN
	    getspecname = .true.
	    call xtpgetcomment(string,lstring,ii1,ii2)
	    if( ii1 .gt. 0 )then
	         write(fnamei,'(60('' ''))')
		 call xtpgetcomment(fnamei,lstring,ii1,ii2)
		 ifrmi = ii1
		 nkap = ii2
		 cmd(1:1)='2'
		 ncmd = 1
	         getspecname = .false.
		 goto 20
	     else
	        call xtpbell
	    endif
	ELSEIF(CMD(1:NCMD) .EQ. '*3') THEN
	    getspecname = .true.
	    call xtpgetcomment(string,lstring,ii1,ii2)
	    if( ii1 .gt. 0 )then
	         write(fnamei,'(60('' ''))')
		 call xtpgetcomment(fnamei,lstring,ii1,ii2)
		 ifrmi = ii1
		 nkap = ii2
		 cmd(1:1)='3'
		 ncmd = 1
	         getspecname = .false.
		 goto 20
	     else
	        call xtpbell
	    endif
	ELSEIF(CMD(1:NCMD) .EQ. '*4') THEN
	    getspecname = .true.
	    call xtpgetcomment(string,lstring,ii1,ii2)
	    if( ii1 .gt. 0 )then
	         write(fnamei,'(60('' ''))')
		 call xtpgetcomment(fnamei,lstring,ii1,ii2)
		 ifrmi = ii1
		 nkap = ii2
		 cmd(1:1)='4'
		 ncmd = 1
	         getspecname = .false.
		 goto 20
	     else
	        call xtpbell
	    endif

	ELSEIF(CMD(1:NCMD) .EQ. '*E') THEN
	    call ecalibration(0)
	    call checkgates(0)
	ELSEIF(CMD(1:NCMD) .EQ. '*K') THEN
	   IF(RMARKRI.NE.RMARKLE) then
	     if( aener1 .eq. aener2 )then
	       jjj = inp_r2(' Enter ENERGY(1),ENERGY(2)',aener1,aener2)
	       if (jjj .ne. 2)then 
	         aener1 = -1.00
		 aener2 = aener1
	       endif
	     endif
	     if( aener1 .ne. aener2 )then
	       ECAL(2)=(aENER2-aENER1)/(RMARKRI-RMARKLE)
	       ECAL(1)=aENER1-ECAL(2)*RMARKLE
	       DO II=3,m$k+1
		  ECAL(II)=0.
	       end do
	       sqrterm=.false.
	       IECAL=1
	       call checkgates(0)
	       write(6,'( ''Calibration coef : '',f10.4,2x,f10.4)')ecal(1),ecal(2)
	     else
	       write(6,'( '' No calibration done '')')
	       call xtpbell
	     endif
	   else
	     write(6,'( '' No calibration done '')')
	     call xtpbell
	   endif
	ELSEIF(CMD(1:NCMD) .EQ. '*T') THEN
*	  IF(CYCLE) GOTO 100
	  CALL TRACKFITDEF(0)

	ELSEIF(CMD(1:NCMD) .EQ. 'A') THEN
	  GOTO 10

	ELSEIF(CMD(1:NCMD) .EQ. 'AJ') THEN
	  CALL AUTOPEAK(XINP,1)
	  IF(XINP.GT.0) THEN
	    MARKLE=MARKRI
	    RMARKLE=RMARKRI
	    RMARKRI=XINP
	    MARKRI=XINP
	  ENDIF

	ELSEIF(CMD(1:NCMD) .EQ. 'AK') THEN
	  CALL autoECALIBRATION(1)
	  CALL xtplotnew
	  call PUT_COMMENT
	  if(iecal.eq.1) then
	    call xtpshowpeaks
	    WRITE(6,'(''  A(1) = '',1PG,''       A(2) = '',1PG)')
	1          ECAL(1),ECAL(2)
	  endif

	ELSEIF(CMD(1:NCMD) .EQ. 'AG') THEN
	  IF(MARKOUT) GOTO 100
	  CALL AUTOPEAK(XINP,2)
	  IF(XINP.GT.0) THEN
	    MARKLE=MARKRI
	    RMARKLE=RMARKRI
	    RMARKRI=XINP
	    MARKRI=XINP
	  ENDIF

	ELSEIF(CMD(1:NCMD) .EQ. 'AT') THEN
	  CALL autoECALIBRATION(0)
	  CALL TRACKFITDEF(0)

	ELSEIF(CMD(1:NCMD) .EQ. 'B') THEN
	  IF(MARKOUT) GOTO 100
	  IF(NMBGD.EQ.12) THEN
	    CALL xtpbell
	  ELSE
	    NMBGD=NMBGD+1
	    XMBGD(NMBGD)=int(XINP)+0.50000000
	    IF(MOD(NMBGD,2).EQ.0)then
	     CALL xtpdoubleMARKER(XMBGD(NMBGD-1),xmbgd(nmbgd),4)
	    else
	     CALL xtpMARKER(xmbgd(nmbgd),4)
	    endif
	  ENDIF

	ELSEIF(CMD(1:NCMD) .EQ. 'C') THEN
	  GOTO 10

	ELSEIF(CMD(1:NCMD) .EQ. 'C1' .OR.
	2      CMD(1:NCMD) .EQ. 'C2' .OR.
	2      CMD(1:NCMD) .EQ. 'C3' .OR.
	2      CMD(1:NCMD) .EQ. 'C4' .OR.
	2      CMD(1:NCMD) .EQ. 'C5' .OR.
	2      CMD(1:NCMD) .EQ. 'C6' .OR.
	2      CMD(1:NCMD) .EQ. 'C7' .OR.
	2      CMD(1:NCMD) .EQ. 'C8' .OR.
	2      CMD(1:NCMD) .EQ. 'C9' .OR.
	2      CMD(1:NCMD) .EQ. 'C0' .OR.
	2      CMD(1:NCMD) .EQ.  '1' .OR.
	2      CMD(1:NCMD) .EQ.  '2' .OR.
	2      CMD(1:NCMD) .EQ.  '3' .OR.
	2      CMD(1:NCMD) .EQ.  '4' .OR.
	2      CMD(1:NCMD) .EQ.  '5' .OR.
	2      CMD(1:NCMD) .EQ.  '6' .OR.
	2      CMD(1:NCMD) .EQ.  '7' .OR.
	2      CMD(1:NCMD) .EQ.  '8' .OR.
	2      CMD(1:NCMD) .EQ.  '9' .OR.
	2      CMD(1:NCMD) .EQ.  '0'      ) then
	  getspecname = .false.
	  IF(CYCLE) GOTO 100
	  READ(CMD(NCMD:NCMD),*) IAUTO
	  IF(LAUTOCOM(IAUTO).GT.0) THEN
	    MAXCICLI=1
	    if(CMD(1:1) .EQ. 'C') then
	      call xtptermfocus
	      CALL INP_I1('#Cicli',MAXCICLI)
	      IF(MAXCICLI.LE.0) GOTO 100
	    endif
	    CYCLE=.TRUE.
	    BREAK=.FALSE.
	    NAUTO=0
	    NCICLO=1
	    IF(AUTOSPEC(IAUTO)) THEN
	      if(LUNLIST(IAUTO).lt.0) then
		CALL INP_str('File with the list of spectra',SPECLIST)
		if(speclist(1:1).eq.'=' .or.
	1	   speclist(1:1).eq.'+' .or.
	1	   speclist(1:1).eq.'-') then 
		   LUNLIST(IAUTO)=0
		else
		   OPEN(UNIT=4,FILE=SPECLIST,FORM='FORMATTED',STATUS='OLD',ERR=200)
		   CLOSE(UNIT=4)
		   LUNLIST(IAUTO)=4
		endif
	        autolist(iauto)=speclist
	      endif
	      if(LUNLIST(IAUTO).lt.0) goto 200
	      speclist=autolist(iauto)
	      listlun=LUNLIST(IAUTO)
	      if(listlun.gt.0) OPEN(UNIT=LISTLUN,FILE=SPECLIST,FORM='FORMATTED',STATUS='OLD',ERR=200)
	    ENDIF
	  ELSE
	    CYCLE=.FALSE.
	  ENDIF

	ELSEIF(CMD(1:NCMD) .EQ. 'CB') THEN
	  CALL BGDLIN(1)

	ELSEIF(CMD(1:NCMD) .EQ. 'CC') THEN
	  CALL TRACKFIT_2G

	ELSEIF(CMD(1:NCMD) .EQ. 'CG') THEN
	  IFBGD=0
	  CALL GAUSSFIT(1,LUNOUT,2)

	ELSEIF(CMD(1:NCMD) .EQ. 'CV') THEN
	  CALL BGDLIN(1)
	  CALL GAUSSFIT(1,LUNOUT,2)

	ELSEIF(CMD(1:NCMD) .EQ. 'CI') THEN
	  IFBGD=0
	  CALL INTEGR(1,LUNOUT)

	ELSEIF(CMD(1:NCMD) .EQ. 'CJ') THEN
	  CALL BGDLIN(1)
	  CALL INTEGR(1,LUNOUT)

	ELSEIF(CMD(1:NCMD) .EQ. 'CP') THEN
	  CALL PEAKSEARCH(1)
	  CALL xtpSHOWPEAKS

	ELSEIF(CMD(1:NCMD) .EQ. 'CT') THEN
	  CALL TRACKFIT

	ELSEIF(CMD(1:NCMD) .EQ. 'CW') THEN
	  CALL WESEGUI(iok)
	  if(iok.eq.1) then
	    if(.not.keepgates) then
	      ngates=0
	      ngnext=0
	    endif
	    NMIN=IKAW
	    NMAX=IKLW
	    CALL FINDYLIMITS
	    CALL xtplotnew
	    call PUT_COMMENT
	  endif

	ELSEIF(CMD(1:NCMD) .EQ. 'D') THEN
	  GOTO 10

	ELSEIF(CMD(1:NCMD) .EQ. 'D1' .OR.
	2      CMD(1:NCMD) .EQ. 'D2' .OR.
	3      CMD(1:NCMD) .EQ. 'D3' .OR.
	4      CMD(1:NCMD) .EQ. 'D4' .OR.
	5      CMD(1:NCMD) .EQ. 'D5' .OR.
	6      CMD(1:NCMD) .EQ. 'D6' .OR.
	7      CMD(1:NCMD) .EQ. 'D7' .OR.
	8      CMD(1:NCMD) .EQ. 'D8' .OR.
	9      CMD(1:NCMD) .EQ. 'D9' .OR.
	1      CMD(1:NCMD) .EQ. 'D0'      ) then
	  IF(CYCLE) GOTO 100
	  READ(CMD(NCMD:NCMD),*) IAUTO
	  call xtptermfocus
	  CALL INP_STR('Automatic command string #'//cmd(2:2)//'# ',AUTOCOM(IAUTO))
	  CALL STR_TOUPPER(AUTOCOM(IAUTO))
	  LAUTOCOM(IAUTO)=LENGTHC(AUTOCOM(IAUTO))
	  AUTOSPEC(IAUTO)=INDEX(AUTOCOM(IAUTO),'N').GT.0
	  IF(AUTOSPEC(IAUTO)) THEN
	    LUNLIST(IAUTO)=-1
	    CALL INP_str('File with the list of spectra',AUTOLIST(IAUTO))
	    if(AUTOLIST(IAUTO)(1:1).eq.'=' .or.
	1      AUTOLIST(IAUTO)(1:1).eq.'+' .or.
	1      AUTOLIST(IAUTO)(1:1).eq.'-') then
	       LUNLIST(IAUTO)=0
	    else
	      OPEN(UNIT=4,FILE=AUTOLIST(IAUTO),FORM='FORMATTED',STATUS='OLD',ERR=200)
	      CLOSE(UNIT=4)
	      LUNLIST(IAUTO)=4
	    endif
	  ENDIF

	ELSEIF(CMD(1:NCMD) .EQ. 'DD') THEN
	  IF(CYCLE) GOTO 100
	  call xtptermfocus
	  CALL DEFINEDISPLAY
	  call xtplotsame

	ELSEIF(CMD(1:NCMD) .EQ. 'DE') THEN
	  efficor = eff_define()

	ELSEIF(CMD(1:NCMD) .EQ. 'DG') THEN
	  call inp_ask('Consider individual peak width for fit',VarWidth)
	  
	ELSEIF(CMD(1:NCMD) .EQ. 'DK') THEN
	  IF(CYCLE) GOTO 100
	  call xtptermfocus
	  CALL ECALIBRATION(0)
	  CALL WCALIBRATION
	  CALL CHECKGATES(0)

	ELSEIF(CMD(1:NCMD) .EQ. 'DL' .or. CMD(1:NCMD) .EQ. 'DF') THEN
	  IF(CYCLE) GOTO 100
	  call xtptermfocus
	  call inp_str('Name for output file',lunout_fname)
	  if( lengthc(lunout_fname) .gt. 0) then 
            if( lunout .eq. 21)close(lunout)
	    if( filexist(lunout_fname) .eq. 0)then
	      write(6,'( '' WARNING - the file '',a,'' already exists'')')lunout_fname(1:lengthc(lunout_fname))
	      if( inp_yes(' --> Do you want to APPEND ( if NOT will OVERWRITE )') )then
	        open(21,file=lunout_fname,status='old')
		do while (.TRUE.)
		  read(21,*,end=3001)
		enddo
	      else
	        open(21,file=lunout_fname,status='unknown')
		goto 3001
	      endif
	    endif
	    open(21,file=lunout_fname,status='unknown')
3001	    lunout=21
	    lstring = lengthc(lunout_fname)
	    call xtpoutfile(lunout_fname,lstring)
	  endif
	  IF(LUNOUT.EQ.1) LUNOUT=6
	  IF(LUNOUT.EQ.LISTLUN .AND. LISTLUN.GT.0) LUNOUT=6

	ELSEIF(CMD(1:NCMD) .EQ. 'DN') THEN
	  IF(CYCLE) GOTO 100
	  CALL INP_STR('Command string',DISPCOM)
	  CALL STR_TOUPPER(DISPCOM)
	  ll=lengthc(dispcom)
	  if(ll.eq.0) then
	    idispcom=0
	  elseif(ll.eq.1 .AND. dispcom(1:1).eq.'=') then
	    idispcom=1
	  elseif(ll.eq.2 .AND. dispcom(1:2).eq.'FY') then
	    idispcom=2
	  elseif(ll.eq.2 .AND. dispcom(1:2).eq.'FX') then
	    idispcom=3
	  elseif(ll.eq.2 .AND. dispcom(1:2).eq.'FF') then
	    idispcom=4
	  else
	    dispcom=' '
	    idispcom=0
	  endif

	ELSEIF(CMD(1:NCMD) .EQ. 'DP') THEN
	  IF(CYCLE) GOTO 100
	  CALL DEFINEPEAK

	ELSEIF(CMD(1:NCMD) .EQ. 'DQ') THEN
	  CALL QDEFINE
	  NMIN=IKA
	  NMAX=IKL
	  r_min=nmin
	  r_max=nmax
	  CALL FINDYLIMITS
	  CALL xtplotnew
	  if(autobg.yes .and. backsub)call xtpoverlay(dfautobg,r_min,r_max,1.0)
	  call PUT_COMMENT

	ELSEIF(CMD(1:NCMD) .EQ. 'DT') THEN
	  IF(CYCLE) GOTO 100
	  CALL TRACKFITDEF(1)

	ELSEIF(CMD(1:NCMD) .EQ. 'DS') THEN
	  IF(CYCLE) GOTO 100
	  CALL SFONDEFINE

	ELSEIF(CMD(1:NCMD) .EQ. 'DW') THEN
	  IF(CYCLE) GOTO 100
	  call xtptermfocus
	  CALL WDEFINE

	ELSEIF(CMD(1:NCMD) .EQ. 'E') THEN
	  if(markle .lt. 0) markle=0
	  nmin=markle
	  if(markri .gt. ikl)markri=ikl-1
	  nmax=markri
	  if(nmin.gt.nmax) call swapl(nmin,nmax)
	  CALL xtplotsame

	ELSEIF(CMD(1:NCMD) .EQ. 'F ') THEN
	  GOTO 10

	ELSEIF(CMD(1:NCMD) .EQ. 'FX') THEN
	  NMIN=IKA
	  NMAX=IKL
	  CALL xtplotsame

	ELSEIF(CMD(1:NCMD) .EQ. 'FY') THEN
	  CALL FINDYLIMITS
	  CALL xtplotsame

	ELSEIF(CMD(1:NCMD) .EQ. 'FF') THEN
	  NMIN=IKA
	  NMAX=IKL
	  CALL FINDYLIMITS
	  CALL xtplotsame

	ELSEIF(CMD(1:NCMD) .EQ. 'FO') THEN
	  YMAX=YINP
	  CALL xtplotsame

	ELSEIF(CMD(1:NCMD) .EQ. 'FU') THEN
	  YMIN=YINP
	  CALL xtplotsame

	ELSEIF(CMD(1:NCMD) .EQ. 'G') THEN
	  IF(MARKOUT) GOTO 100
	  IF(NMGAU.EQ.9) THEN
	    CALL xtpbell
	  ELSE
	    NMGAU=NMGAU+1
	    XMGAU(NMGAU)=XINPF
	    CALL xtpMARKER(Xmgau(nmgau),3)
	  ENDIF

	ELSEIF(CMD(1:NCMD) .EQ. 'H' .OR. CMD(1:NCMD) .EQ. '?') THEN
	  IF(CYCLE) GOTO 100
	  CALL HELP

	ELSEIF(CMD(1:NCMD) .EQ. 'I') THEN
	  IF(MARKOUT) GOTO 100
	  IF(NMINT.EQ.40) THEN
	    CALL xtpbell
	  ELSE
	    NMINT=NMINT+1
	    XMINT(NMINT)=int(XINP)+0.5
	    IF(MOD(NMINT,2).EQ.0)then
	     CALL xtpdoubleMARKER(XMINT(NMINT-1),xmint(nmint),3)
	    else
	     CALL xtpMARKER(xmint(nmint),3)
	    endif
	  ENDIF

	ELSEIF(CMD(1:NCMD) .EQ. 'K') THEN
	  IF(CYCLE) GOTO 100
	  IF(MARKLE.EQ.MARKRI) THEN
	    CALL xtpbell
	    IECAL=0
	  ELSEIF(MARKLE.NE.NMIN .AND. MARKRI.NE.NMAX) THEN
	    CALL ECALIBRATION(1)
	  ENDIF

	ELSEIF(CMD(1:NCMD) .EQ. 'L') THEN
	  II=IFUNCT
	  IF(II.EQ.1) IFUNCT=2
	  IF(II.NE.1) IFUNCT=1
	  CALL FINDYLIMITS
	  CALL xtplotsame

	ELSEIF(CMD(1:NCMD) .EQ. 'M') THEN
	  GOTO 10

	ELSEIF(CMD(1:NCMD) .EQ. 'M1' .OR.
	2      CMD(1:NCMD) .EQ. 'M2' .OR.
	3      CMD(1:NCMD) .EQ. 'M3' .OR.
	4      CMD(1:NCMD) .EQ. 'M4' .OR.
	5      CMD(1:NCMD) .EQ. 'M5' .OR.
	6      CMD(1:NCMD) .EQ. 'M6' .OR.
	7      CMD(1:NCMD) .EQ. 'M7' .OR.
	8      CMD(1:NCMD) .EQ. 'M8' .OR.
	9      CMD(1:NCMD) .EQ. 'M9' .OR.
	1      CMD(1:NCMD) .EQ. 'M0'      ) then
	  READ(CMD(NCMD:NCMD),*) IAUTO
	  IF(LAUTOCOM(IAUTO).GT.0) THEN
	    CALL xtptermfocus
	    WRITE(6,*) ' '//AUTOCOM(IAUTO)(1:LAUTOCOM(IAUTO))
	    if(autospec(iauto)) then
	      ll=lengthc(AUTOLIST(IAUTO))
	      if(ll.gt.0) WRITE(6,*) ' '//AUTOLIST(IAUTO)(1:ll)
	    endif
	  ENDIF

	ELSEIF(CMD(1:NCMD) .EQ. 'MB') THEN
	  CALL SHOWMARKERS(XMBGD,NMBGD,2,4)

	ELSEIF(CMD(1:NCMD) .EQ. 'MG') THEN
	  CALL SHOWMARKERS(XMGAU,NMGAU,1,3)

	ELSEIF(CMD(1:NCMD) .EQ. 'MN') THEN
	  IF(idispcom.gt.0) then
	    WRITE(6,*) ' '//DISPCOM(1:2)
	  ENDIF

	ELSEIF(CMD(1:NCMD) .EQ. 'MV') THEN
	  CALL SHOWMARKERS(XMBGD,NMBGD,2,4)
	  CALL SHOWMARKERS(XMREG,NMREG,2,6)
	  CALL SHOWMARKERS(XMGAU,NMGAU,1,3)

	ELSEIF(CMD(1:NCMD) .EQ. 'MI') THEN
	  CALL SHOWMARKERS(XMINT,NMINT,2,3)

	ELSEIF(CMD(1:NCMD) .EQ. 'MJ') THEN
	  CALL SHOWMARKERS(XMBGD,NMBGD,2,4)
	  CALL SHOWMARKERS(XMINT,NMINT,2,3)

	ELSEIF(CMD(1:NCMD) .EQ. 'MP') THEN
	  CALL xtpSHOWPEAKS

	ELSEIF(CMD(1:NCMD) .EQ. 'MR') THEN
	  CALL SHOWMARKERS(XMREG,NMREG,2,6)

	ELSEIF(CMD(1:NCMD) .EQ. 'MS') THEN
*	  if(nwfon.gt.1) then
*	    call disp_pvec(Wfonx,Wfony,1,NWFON,1,2,dwin,twin)
*	  else
*	    CALL xtpbell
*	  endif

	ELSEIF(CMD(1:NCMD) .EQ. 'MW') THEN

	  CALL SHOWMARKERS(CGATE,NGATES*2+mod(NGNEXT,2),2,1)
	  
	ELSEIF(CMD(1:NCMD) .EQ. 'MZ') THEN
	  xlow=ika
	  xhigh=ikl
	  call xtpoverlay(dfzero,xlow,xhigh,10.0)

	ELSEIF(CMD(1:NCMD) .EQ. 'N') THEN
	  if( getspecname )then
	   call xtpgetcomment(string,lstring,ii1,ii2)
	   if( ii1 .gt. 0 )then
	  	write(fnamei,'(60('' ''))')
		call xtpgetcomment(fnamei,lstring,ii1,ii2)
	        ifrmi = ii1
	        nkap = ii2
           endif
	  endif
	  getspecname = .true.
	  IKAOLD=IKA
	  IKLOLD=IKL
	  CALL GETSPECTRUM(-1)
	  IF(BREAK) GOTO 100
	  IF(IKA.EQ.IKAOLD .AND. IKL.EQ.IKLOLD) THEN
	    if(idispcom.eq.0) then
	      IERASE=0
	    elseif(idispcom.eq.1) then
	      ierase=1
	    elseif(idispcom.eq.2) then
	      ierase=1
	      call findylimits
	    elseif(idispcom.eq.3) then
	      ierase=1
	      nmin=ika
	      nmax=ikl
	    elseif(idispcom.eq.4) then
	      ierase=1
	      nmin=ika
	      nmax=ikl
	      call findylimits
	    endif
	  ELSE
	    IERASE=1
	    NMIN=IKA
	    NMAX=IKL
	    CALL FINDYLIMITS
	  ENDIF
	  if(IERASE .eq. 0)then
	    if(emptyframe)then
	      call FINDYLIMITS
	      call xtplotnew
	      call PUT_COMMENT
	      emptyframe=.false.
	    else
	      call xtplotadd
	      call PUT_COMMENT
	    endif
	  else
	    call xtplotnew
	    call PUT_COMMENT
	      emptyframe=.false.
	  endif

	ELSEIF(CMD(1:NCMD) .EQ. 'O') THEN
	  GOTO 10

	ELSEIF(CMD(1:NCMD) .EQ. 'OS') THEN
	  IF(CYCLE) GOTO 100
	  call xtptermfocus
	  CALL INP_SPNAME('Filename[|Format:Length]',FNAMEO,FRMO,KLO)
	  KLCH=KLO*1024
	  if( index(fnameo,'#') .le. 0 )then
	    if( filexist(fnameo) .eq. 0 )then
	      write(6,'('' Spectrum file already exists'')')
	      if( inp_not('OVERWRITE') )then
	        if( fileremove(fnameo) .eq. -1)then
		  write(6,'('' ERROR - cannot overwrite spectrum file'')')
		  goto 100
		endif
	      endif
	    endif
	  endif
	  CALL WRITESPEC(FNAMEO,SPEK,FRMO,KLCH,KV)
	  IF(KV.LE.0) then
	     write(6,'('' ERROR - cannot write spectrum file, err='',i2)')kv
	     GOTO 100
	  endif
	  
	ELSEIF(CMD(1:NCMD) .EQ. 'O=') THEN
	  IF(CYCLE) GOTO 100
	  call xtptermfocus
	  call plotspectrum

	ELSEIF(CMD(1:NCMD) .EQ. 'P') THEN
	  XIN=-1
	  call xtptermfocus
	  CALL INSERTPEAK(XIN)
	  IF(XIN.GT.0) THEN
	    XPOS=XIN
	    NMIN=XPOS-75
	    NMIN=MAX(NMIN,IKA)
	    NMAX=NMIN+150
	    CALL xtplotsame
	    YPOS=DFSPEK(XPOS)
	    CALL xtpshowpeak(XPOS)
	  ENDIF

	ELSEIF(CMD(1:NCMD) .EQ. 'Q') THEN
	  if(.not.cmatrix) then
	    CALL xtpbell
	    goto 100
	  endif
	  call setproje
	  NMIN=IKA
	  NMAX=IKL
	  CALL FINDYLIMITS
	  CALL xtplotnew
	  if(autobg.yes .and. backsub)call xtpoverlay(dfautobg,r_min,r_max,1.0)
	  call PUT_COMMENT

	ELSEIF(CMD(1:NCMD) .EQ. 'R') THEN
	  IF(MARKOUT) GOTO 100
	  IF(NMREG.EQ.2) THEN
	    CALL xtpbell
	  ELSE
	    NMREG=NMREG+1
	    XMREG(NMREG)=int(XINP)+0.5
	    IF(MOD(NMREG,2).EQ.0)then
	     CALL xtpdoubleMARKER(XMREG(NMREG-1),xmreg(nmreg),6)
	    else
	     CALL xtpMARKER(xmreg(nmreg),6)
	    endif
	  ENDIF

	ELSEIF(CMD(1:NCMD) .EQ. 'V') THEN
*	  IF(MARKOUT) GOTO 100
*	  MARKLE=MARKRI
*	  RMARKLE=RMARKRI
*	  RMARKRI=XINP
*	  MARKRI=XINP
*	  CALL DRAWDMARKER(XINPF,YINPF,0,7)

	ELSEIF(CMD(1:NCMD) .EQ. 'S') THEN
	  goto 10

	ELSEIF(CMD(1:NCMD) .EQ. 'SX') THEN
	  call xtpsamex

	ELSEIF(CMD(1:NCMD) .EQ. 'SY') THEN
	  call xtpsamey
	  	  
	ELSEIF(CMD(1:NCMD) .EQ. 'X') THEN
	  NMIN=XINP-100
	  NMIN=MAX(NMIN,IKA)
	  NMAX=NMIN+200
	  NMAX=MIN(NMAX,IKL)
	  MARKLE=NMIN
	  RMARKLE=MARKLE
	  MARKRI=NMAX
	  RMARKRI=MARKRI
	  CALL xtplotsame

	ELSEIF(CMD(1:NCMD) .EQ. 'W') THEN
	  IF(MARKOUT) GOTO 100
	  NGNEXT=MOD(NGNEXT,2)
	  IF(NGNEXT.EQ.0) then
	    IF(NGATES.GE.M$GATE) THEN
	      CALL xtpbell
	      goto 10
	    endif
	    NGATES=NGATES+1
	    if(mdim .eq. 3)then
	       gclass(ngates)=ngates
	       jexist=0
	      do jj=1,ngates
	       gclass(ngates)=jj
	       do ii=1,ngates-1
	          if(gclass(ii) .eq. gclass(ngates))jexist=1
	       enddo
	       if(jexist .eq. 0) goto 1040
	       jexist = 0
	      enddo
1040	      continue
	     endif
	  ENDIF
	  NGNEXT=NGNEXT+1
	  IGATE(ngnext,ngates)=XINP
	  CGATE(ngnext,ngates)=IGATE(ngnext,ngates)+0.5
	  if(iecal.ne.1) then
	    EGATE(ngnext,ngates)=int(XINP)
	  else
	    EGATE(ngnext,ngates)=cpol(XINP,ECAL,-M$K)
	  endif
	  
	  IF(ngnext.EQ.2)then 
	    CALL xtpdoubleMARKER(CGATE(1,ngates),cgate(ngnext,ngates),1)
	  else
	    CALL xtpMARKER(cgate(ngnext,ngates),1)
	  endif
	  
	ELSEIF(CMD(1:NCMD) .EQ. 'Z') THEN
	  GOTO 10

	ELSEIF(CMD(1:NCMD) .EQ. 'Z1' .OR.
	2      CMD(1:NCMD) .EQ. 'Z2' .OR.
	3      CMD(1:NCMD) .EQ. 'Z3' .OR.
	4      CMD(1:NCMD) .EQ. 'Z4' .OR.
	5      CMD(1:NCMD) .EQ. 'Z5' .OR.
	6      CMD(1:NCMD) .EQ. 'Z6' .OR.
	7      CMD(1:NCMD) .EQ. 'Z7' .OR.
	8      CMD(1:NCMD) .EQ. 'Z8' .OR.
	9      CMD(1:NCMD) .EQ. 'Z9' .OR.
	1      CMD(1:NCMD) .EQ. 'Z0'      ) then
	  READ(CMD(NCMD:NCMD),*) IAUTO
	  LAUTOCOM(IAUTO)=0

	ELSEIF(CMD(1:NCMD) .EQ. 'ZA') THEN
	  NMBGD=0
	  IFBGD=0
	  NMREG=0
	  NMGAU=0
	  IFGAU=0
	  NMINT=0
	  IFINT=0

	ELSEIF(CMD(1:NCMD) .EQ. 'ZB') THEN
	  NMBGD=0
	  IFBGD=0

	ELSEIF(CMD(1:NCMD) .EQ. 'ZL' .or. CMD(1:NCMD) .EQ. 'ZF') THEN
            if( lunout .eq. 21)then
	       close(lunout)
	       write(6,'(''  OUTPUT file closed: '',a)')lunout_fname
	       lunout = -10
	       lstring = 0
	       call xtpoutfile(lunout_fname,lstring)
	    else
	       write(6,'(''  No OUTPUT file currently open'')')
	       lunout = -10
	    endif
	
	ELSEIF(CMD(1:NCMD) .EQ. 'ZN') THEN
	  idispcom=0
	  dispcom=' '

	ELSEIF(CMD(1:NCMD) .EQ. 'ZG') THEN
	  NMGAU=0
	  IFGAU=0

	ELSEIF(CMD(1:NCMD) .EQ. 'ZV') THEN
	  NMBGD=0
	  IFBGD=0
	  NMREG=0
	  NMGAU=0
	  IFGAU=0

	ELSEIF(CMD(1:NCMD) .EQ. 'ZI') THEN
	  NMINT=0
	  IFINT=0

	ELSEIF(CMD(1:NCMD) .EQ. 'ZJ') THEN
	  NMBGD=0
	  IFBGD=0
	  NMINT=0
	  IFINT=0

	ELSEIF(CMD(1:NCMD) .EQ. 'ZP') THEN
	  NPEAKS=0

	ELSEIF(CMD(1:NCMD) .EQ. 'ZR') THEN
	  NMREG=0

	ELSEIF(CMD(1:NCMD) .EQ. 'ZS') THEN
	  nwfon=1
	  wfonx(0)=0
	  wfony(0)=0

	ELSEIF(CMD(1:NCMD) .EQ. 'ZW') THEN
	  NGATES=0
	  NGNEXT=0

	ELSEIF(CMD(1:NCMD) .EQ. '=') THEN
          call xtplotnew
	  
	ELSEIF(CMD(1:NCMD) .EQ. '>') THEN
	  NCHAN=NMAX-NMIN+1
	  NMAX=NMAX+3*NCHAN/4
	  NMAX=MIN(NMAX,IKL)
	  NMIN=NMAX-(NCHAN-1)
	  CALL xtplotsame	  
	  CALL xtpSHOWPEAKS
*	    CALL SHOWMARKERS(CGATE,ngates*2+mod(ngnext,2),2,0.5,0)

	ELSEIF(CMD(1:NCMD) .EQ. '<') THEN
	  NCHAN=NMAX-NMIN+1
	  NMIN=NMIN-3*NCHAN/4
	  NMIN=MAX(NMIN,IKA)
	  NMAX=NMIN+(NCHAN-1)
	  CALL xtplotsame
	  CALL xtpSHOWPEAKS
*	    CALL SHOWMARKERS(CGATE,ngates+mod(ngnext,2),2,0.5,0)

	ELSEIF(CMD(1:NCMD) .EQ. '+') THEN
	  IF(MARKOUT) GOTO 100
	  XX=XINP
	  CALL INSERTPEAK(XX)
	  IF(XX.LE.0) GOTO 100
	  CALL xtpshowpeak(XINPF)

	ELSEIF(CMD(1:NCMD) .EQ. '-') THEN
	  IF(MARKOUT) GOTO 100
	  XPOS=XINP
	  CALL DELETEPEAK(XPOS)
*	  IF(XPOS.LE.0) GOTO 100
*	  YPOS=DFSPEK(XPOS)
*	  call disp_setscale(dwin,twin,xsca,ysca)
*	  CALL disp_map(XPOS,YPOS,tx,ty,dwin,twin)
*	  IF(TX.LT.TWIN(1) .OR. TX.GT.TWIN(2) ) goto 100
*	  call disp_line(TX-10,TY-10,TX+10,TY+10)
*	  call disp_line(TX-10,TY+10,TX+10,TY-10)

	ELSEIF(CMD(1:NCMD) .EQ. CHAR( 3) .OR.
	1      CMD(1:NCMD) .EQ. CHAR( 4) .OR.
	1      CMD(1:NCMD) .EQ. CHAR(25) .OR.
	1      CMD(1:NCMD) .EQ. CHAR(26) ) THEN
	  call xtptermfocus
	  IF(.NOT.INP_YES('You really want to exit this program')) then
	    CYCLE=.FALSE.
	    GOTO 100
	  ENDIF
	  CALL EXIT(0)

	ELSE
	  CALL xtpbell
	ENDIF

100	NCMD=0
	CMD='  '
	IF(BREAK) THEN
	  CYCLE=.FALSE.
	  LISTLUN=0
	ENDIF
	GOTO 10

200	CLOSE(LISTLUN)
	BREAK=.TRUE.
	CYCLE=.FALSE.
	LISTLUN=0
	GOTO 100

	END
 
	SUBROUTINE HELP

#include "trackn.inc"
	character*1 ch


	WRITE(6,'(''  ************  COMMAND-LIST  ***********'')')
	WRITE(6,*)
	WRITE(6,*) 'spacebar       Marker'
	WRITE(6,*) 'AJ AG          Automatic CJ, CG  at marker position'
	WRITE(6,*) 'B G I R S W    Insert a marker of type B, G, I, R, S  or W'
	WRITE(6,*) 'CB CI CJ MI MJ Background, Integration, CB+CI'
	WRITE(6,*) 'CG CV MG MV    Gaussfit, CB+CG. Show markers'
	WRITE(6,*) 'CP MP          Automatic peak search. Show peaks'
	WRITE(6,*) 'Dn Cn Mn Zn n  Define, Execute, Show, Erase command string n=1...9'
	WRITE(6,*) 'DD             Change the display parameters'
	WRITE(6,*) 'DE             Define how to do efficiency correction'
	WRITE(6,*) 'DG             Define peak width (individual/common) for fit'
	WRITE(6,*) 'DK AK          Energy and Width calibration'
	WRITE(6,*) 'DF DL          Define output file for Area calculations'
	WRITE(6,*) 'DT CT AT       Recalibration using Trackfit'
	WRITE(6,*) 'DW CW          Define, Estract cuts from compressed matrix'
	WRITE(6,*) 'DQ             Define matrix and background subtraction mode'
	WRITE(6,*) 'E              Expand between last two Markers'
	WRITE(6,*) 'X              Expand around current cursor position'
	WRITE(6,*) 'FF FX FY       Full display Full_x Full_y'
	WRITE(6,*) 'SX SY          same X or Y scale for all windows '
	WRITE(6,*) 'FO FU          Set Y-maximum or Y-minimum by marker'
	WRITE(6,*) 'H ?            Help (this list)'
	WRITE(6,*) 'K              Energy calibration from previous 2 energies'
	WRITE(6,*) 'L              Lin/Log'
	WRITE(6,*) 'N              Input new spectrum'
	WRITE(6,*) 'DN MN ZN       Define display behaviour at input of new spectrum'
	WRITE(6,*) 'OS             Write out current spectrum'
	WRITE(6,*) 'O=             Postscript plot of current display'
	WRITE(6,*) 'P              Insert a peak by energy'
	WRITE(6,*) 'Q              Display projection of compressed matrix'
*	WRITE(6,*) 'DS MS ZS S     Define, Show, Erase, Set smooth background markers'
	WRITE(6,*) 'V              Marker writing also counts in channel'
	WRITE(6,*) 'MZ             Draw a line at zero counts'
	WRITE(6,*) 'ZA             Delete all B/G/I markers'
	WRITE(6,*) 'ZB ZI ZJ ZG ZV Delete corresponding type of markers'
	WRITE(6,*) 'ZF ZL          Close output file for Area calculations'
	WRITE(6,*) 'DP MP ZP       Define, Show, Delete peaks in buffer'
	WRITE(6,*) '+ -            Insert/delete a peak by marker'
	WRITE(6,*) '=              Repeat the display'
	WRITE(6,*) '< >            Shift display 3/4 to Left, Rigth'
	WRITE(6,*) 'CTL_RIGHTARROW     Increase # of windows adding one column more' 
	WRITE(6,*) 'CTL_LEFTARROW      Decrease # of windows deleting last column' 
	WRITE(6,*) 'CTL_UPARROW        Increase # of windows adding one row more' 
	WRITE(6,*) 'CTL_DOWNARROW      Decrease # of windows deleting last row' 
	WRITE(6,*) 'CTL_C CTL_Y CTL_Z     Stop'


	RETURN

	END

	SUBROUTINE STARTPARAMETERS

#include "trackn.inc"

!	CALL ERRSET(29,.TRUE.,.FALSE.,.TRUE.,.FALSE.)

	TWx0=100.	! Finestra sul terminale
	TWx1=1000.
	TWy0=50.
	TWy1=650.

	ticx=100	! parametri per il disegno degli assi
	ticy=80
	ticl=8
	chrx=12
	chry=24
	labelx=' '
	lxcal=1		! scritte in energia
	islaser=.false.

	CYCLE=.false.
	break=.true.
	LISTLUN=0
	Lunout=0
	SPEClist=' '

	IFUNCT=1	! DISPLAY FUNCTION	1=NORMAL
C						2=LOGAR.
C						3=SQRT

	ITYP=-1		! DISPLAY TYPE		+-1=HISTOGRAM
C			! compression enabled	+-2=VECTOR
C						+-3=POINTS

	IFRMI=4		! INPUT  FORMAT	U 1=INTEGER*2
	IFRMO=4		! OUTPUT FORMAT	I 2=UNSIG. INTEGER*2
	FRMI='L'	!		R 3=REAL
	FRMO='L'	!		L 4=INTEGER*4
	KLI=8		!		A 5=ASCII(8I9)
	KLO=8		!		H 6=ASCII(1X,I5,6X,10I9)
	FNAMEI=' '
	FNAMEO=' '
	LFNAMEI=1
	LFNAMEO=1

	NKAP=8		! FILE LENGTH IN K
	IKA=0
	IKL=NKAP*1024
	NMIN=IKA
	NMAX=IKL
	YMIN=0.
	YMAX=1000.
	call setyvalf(ymin,yminf,ifunct)
	call setyvalf(ymax,ymaxf,ifunct)
	call setxvalf(nmin,xminf)
	call setxvalf(nmax,xmaxf)
	CALL put4LW(dwin,xMINf,xMAXf,yMINf,yMAXf)
	CALL put4LW(twin,TWx0,TWx1,TWy0,TWy1)
	call disp_setscale(dwin,twin,xsca,ysca)
	markle=nmin
	markri=nmax
	rmarkle=markle
	rmarkri=markri

	do ii=1,m$k+1
	   ecal(ii)=0
	   wcal(ii)=0
	end do
	ECAL(2)=1.
	sqrterm=.false.
	IECAL=0		! ENERGY CALIBRATION	1=YES 0=NO

	WCAL(1)=5.
	IWCAL=1		! FWHM CALIBRATION	1=YES 0=NO

	WFIND=8.	! default for peak significance in peaksearch
	VarWidth = .TRUE.  ! variable width for peak fit

	NMINT=0		! NO INTEGRATION
	IFINT=0

	NMBGD=0		! NO BACKGROUND
	IFBGD=0

	NPEAKS=0	! NO PEAKS IN POSITION BUFFER

	NGATE2=0	! NO gates in gate buffer
	NGNEXT=0

	NWFON=1		! No punti per smooth background
	wfonx(0)=0
	wfony(0)=0

	cmatrix=.false.	! matrice o semplice spettro
	isproje=.false.
	iscut=.false.
	keepgates=.true.
	spherical=.true.
	backsub=.false.
	backcom=.false.
	backpro=.true.
	efficor=.false.

	corrback(1)=1.
	corrback(2)=1.
	corrback(3)=1.
	corrback(4)=1.
	correffi(1)=1.
	correffi(2)=1.
	correffi(3)=1.
	correffi(4)=1.

	ikaw=ika
	iklw=ikl

CVMS	plotcom='LWP/DELETE'
	call getenv('TSPOOL',plotcom)
	lplotcom=lengthc(plotcom)
	if(lplotcom.gt.0) then
	  plotcom=plotcom(1:lplotcom)//' '
	  lplotcom=lplotcom+1
	endif

	spec_divfac=1.0    ! N.M.
	autobg.m=2
	autobg.fstep=0.1000
	autobg.itmax=2

       
        call xtpinit
	call xtpsetv(spek,err2,ika,ikl,ecal,peaks,npeaks,nmin,nmax,ymin,ymax,ifunct)
	
	WRITE(6,'(''********** PROGRAM XTRACKN -VERSION MAY-2001 **********'')')
	WRITE(6,*)
	
	RETURN

	END

	SUBROUTINE GETSPECTRUM(iadc)

#include "trackn.inc"
	character filesave*60

	isproje=.false.
	iscut=.false.
	filesave=fnamei

10	IF(CYCLE) THEN
	  if(listlun.gt.0) then
	    READ(LISTLUN,'(A)',ERR=200,END=200) FNAMEI
	    if(iadc.ge.0 .and. iadc.lt.100) then
		ldir=index(FNAMEI,']')
		ldot=index(FNAMEI(ldir+1:),'.')
		ldot=ldot+ldir
		if(ldot.gt.2) write(FNAMEI(ldot-2:ldot-1),'(i2.2)') iadc
	     endif
	     IASK=0
	  else
	     call inp_setbuff(speclist)
	     call inp_mode(2)
	     IASK=1
	  endif
	ELSE
	  IASK=1
	ENDIF

100	call xtptermfocus
        CALL READDAT(IASK,FNAMEI,SPEK(0),NKAP*1024,IFRMI,KV)
	LFNAMEI=MAX(1,LENGTHC(FNAMEI))
	if(cycle.and.listlun.eq.0) call inp_mode(0)
	IF(KV.LE.0) THEN
	   if(kv.ne.-1)write(6,*) FNAMEI(1:lFNAMEI),'   Read error',kv
	   if(CYCLE) goto 200
	   if(kv.eq.-1) goto 200	! CTRL_Z
	   goto 100
	ENDIF

	IFRMI=MOD(KV,100)
	NCHAN=KV/100
	NKAP=(NCHAN+1023)/1024
	IKA=0
	IKL=NKAP*1024
	do ii=nchan,ikl-1
	  spek(ii)=0
	end do

	do ii=ika,ikl-1
	  e2=abs(spek(ii))
	  if(e2.eq.0.) e2=1.
	  err2(ii)=e2
	end do

	BREAK=.FALSE.
	RETURN

200	if(listlun.gt.0) CLOSE(LISTLUN)
	BREAK=.TRUE.
	FNAMEI=FILESAVE
	RETURN

	END

	subroutine PUT_COMMENT

#include "trackn.inc"

	character string*200
	integer*4 lcomment,itmp1,itmp2
	
	lcmatfile=lengthc(cmatfile)

	  if(iscut) then
	    xgate=(egate(1,1)+egate(2,1))/2.
	    if(ngates.eq.1) then
	      WRITE(string,'(1x,A,f7.1,A)') CMATFILE(1:LCMATFILE)//'  Cut of',xgate,' keV'
	      if(mmode.eq.0) then
	        lstring=lengthc(string)
	        write(string(lstring+1:),'('' (gate on axis'', i2,'')'')') gside 
	      endif
	    else
	      WRITE(string,'(1x,A,f7.1,A)') CMATFILE(1:LCMATFILE)//'  Cuts of',xgate,', ... keV'
	      if(mmode.eq.0) then
	        lstring=lengthc(string)
	        write(string(lstring+1:),'('' ('',i2,'' gates on axis'', i2,'')'')') ngates,gside 
	      endif
	    endif
	  elseif(isproje) then
	    WRITE(string,'(1x,A)')          CMATFILE(1:LCMATFILE)//'   Projection'
	    if(mmode.eq.0) then
	      lstring=lengthc(string)
	      write(string(lstring+1:),'( '' on axis'', i2)') gside 
	    endif
	  else
	    WRITE(string,'(A)')FNAMEI(1:LFNAMEI)
	    lcomment=lengthc(string)
	    call xtpcomment(string,lcomment,ifrmi,nkap)
	    return
	  endif
	  
	  lcomment=lengthc(string)
	  itmp1 = 0
	  itmp2 = 0
	  call xtpcomment(string,lcomment,itmp1,itmp2)
	  return
	  end



	SUBROUTINE TKTDISPLAY(IERASE,IWRITE)

#include "trackn.inc"

	character string*200
	integer*4 lstring, ii1,ii2
	integer*2 tekdefault

	REAL*4 DFSPEK_PS
	EXTERNAL DFSPEK_PS

	tekdefault=1
	call setyvalf(ymin,yminf,ifunct)
	call setyvalf(ymax,ymaxf,ifunct)
	call setxvalf(nmin,xminf)
	call setxvalf(nmax,xmaxf)
	CALL put4LW(dwin,xminf,xmaxf,yminf,ymaxf)
	call disp_setscale(dwin,twin,xsca,ysca)
	call getyvalf(ymin,yminf,ifunct)
	call getyvalf(ymax,ymaxf,ifunct)
	call getxvalf(nmin,xminf)
	call getxvalf(nmax,xmaxf)

	IF(IERASE.EQ.1) then
	   call tkt__flush
*	   call tekvecmode 
	   call disp_tek_mode(xterm,tekmode)
	   CALL DISP_INITT
	endif
	call tkt__flush
*	call teksetcolor(tekdefault)
	IF(IWRITE.EQ.1) THEN
*	  if(iscut) then
*	    xgate=(egate(1,1)+egate(2,1))/2.
*	    if(ngates.eq.1) then
*	      WRITE(string,'(1x,A,f7.1,A)') CMATFILE(1:LCMATFILE)//'  Cut of',xgate,' keV'
*	      if(mmode.eq.0) then
*	        lstring=lengthc(string)
*	        write(string(lstring+1:),'('' (gate on axis'', i2,'')'')') gside 
*	      endif
*	    else
*	      WRITE(string,'(1x,A,f7.1,A)') CMATFILE(1:LCMATFILE)//'  Cuts of',xgate,', ... keV'
*	      if(mmode.eq.0) then
*	        lstring=lengthc(string)
*	        write(string(lstring+1:),'('' ('',i2,'' gates on axis'', i2,'')'')') ngates,gside 
*	      endif
*	    endif
*	  elseif(isproje) then
*	    WRITE(string,'(1x,A)')          CMATFILE(1:LCMATFILE)//'   Projection'
*	    if(mmode.eq.0) then
*	      lstring=lengthc(string)
*	      write(string(lstring+1:),'( '' on axis'', i2)') gside 
*	    endif
*	  else
*	    WRITE(string,'(1x,A)')          FNAMEI(1:LFNAMEI)
*	  endif
	  call xtpgetcomment(string,lstring,ii1,ii2)
	  do ii=lstring+1,200
	     string(ii:ii)=' '
	  enddo
	  
	  if(islaser) then
	    call disp_text(Twin(1)+50*chrx,TWIN(4)+1.0*chry,textsize,textang,string)
	  else
!	    call disp_text(Twin(1)+50*chrx,TWIN(4)+0.4*chry,textsize,textang,string)
	    call disp_text(Twin(1),TWIN(4)+0.4*chry,textsize,textang,string)
	  endif
	  call tkt__flush
	  call trackframe
	ENDIF

	CALL DISP_window(TWIN)

	if(tekcolinc)tekcolor=tekcolor+1
	call tkt__flush
*	call teksetcolor(tekcolor)
	CALL DISP_YFUN(DFSPEK_PS,xminf,xmaxf+1.,1.,ityp,dwin,twin)
	

	MARKLE=NMIN
	RMARKLE=MARKLE
	MARKRI=NMAX
	RMARKRI=MARKRI

	RETURN

	END
 
	SUBROUTINE DRAWDMARKER(XINP,YINP,xoffs,ilist)

#include "trackn.inc"
	character*200 string
	character*2 nm_char

c	ilist =	0	solo il marker
c		1	scritta energia
c	      	2	scritta canale
c		4	scritta valore
c	e tutte le possibili combinazioni

	call disp_setscale(dwin,twin,xsca,ysca)
	CALL disp_map(XINP+xoffs,YINP,tx,ty,dwin,twin)

	IF(TX.LT.TWIN(1) .OR. TX.GT.TWIN(2) ) RETURN
	IX=TX

	call disp_line(TX,TWIN(3)+10,TX,TWIN(4)-10)
	if(ilist.lt.1) return

	IF(TY.LT.TWIN(3) .OR. TY.GT.TWIN(4)) RETURN

	TYY=MAX(TY ,TWIN(3)+15)
	TYY=MIN(TYY,TWIN(4)-15)

	mym=0
	if( (mod(ilist,2).eq.1).AND.(IECAL.EQ.1) ) THEN
	  ENER=cpol(XINP,ECAL,-m$k)
	  ENER=INT(10*ENER)/10.
	  JJ=1
	  IF(ENER.NE.0.) JJ=ALOG10(ABS(ENER))+1
	  IF(ENER.LT.0.) JJ=JJ+1
	  JJ=JJ+2
	  write(nm_char,'(i2)')jj
	  WRITE(string,'(F'//nm_char//'.1)') ENER
	  call disp_text(TX+0.5*chrx,TYY-mym*chry,textsize,textang,string)
	  mym=mym+1
	endif
	if((mod(ilist/2,2).eq.1).OR.((mod(ilist,2).eq.1).AND.(.not.iecal)))then
	  LI=XINP
	  JJ=1
	  IF(LI.NE.0) JJ=LOG10(FLOAT(IABS(LI)))+1
	  IF(LI.LT.0) JJ=JJ+1
	  write(nm_char,'(i2)')jj
	  WRITE(string,'(I'//nm_char//')') LI
	  call disp_text(TX+0.5*chrx,TYY-mym*chry,textsize,textang,string)
	  mym=mym+1
	endif
	if(mod(ilist/4,2).eq.1) then
	  Chanval=spek(int(XINP))
	  JJ=1
	  IF(chanval.NE.0) JJ=ALOG10(ABS(chanval))+1
	  IF(chanval.LT.0.) JJ=JJ+1
	  JJ=JJ+2
	  write(nm_char,'(i2)')jj
	  WRITE(string,'(F'//nm_char//'.1)') chanval
	  call disp_text(TX+0.5*chrx,TYY-mym*chry,textsize,textang,string)
	  mym=mym+1
	ENDIF

	RETURN

	END

	SUBROUTINE DRAW2MARKER(XINP,xoffs)

#include "trackn.inc"
	real xinp(2)

	call disp_setscale(dwin,twin,xsca,ysca)
	CALL disp_map(XINP(1)+xoffs,DWIN(3),tx1,ty1,dwin,twin)
	CALL disp_map(XINP(2)+xoffs,DWIN(3),tx2,ty1,dwin,twin)

	IF(TX1.LT.TWIN(1) .AND. TX2.LT.TWIN(1)) RETURN
	IF(TX1.GT.TWIN(2) .AND. TX2.GT.TWIN(2)) RETURN

	TX1=MIN(MAX(TX1,TWIN(1)),TWIN(2))
	TX2=MIN(MAX(TX2,TWIN(1)),TWIN(2))

*	CALL tekvecmode 
	call DISP_VECMOD
*	call teksetcolor(tekcolor)
*	CALL DISP_MOVABS(TX1,TWIN(4)-10.)
*	CALL DISP_DRWABS(TX1,TWIN(3)+10.)
*	CALL DISP_DRWABS(TX2,TWIN(3)+10.)
*	CALL DISP_DRWABS(TX2,TWIN(4)-10.)
	call disp_line(TX1,TWIN(4)-10.,TX1,TWIN(3)+10.)
	call disp_line(TX1,TWIN(3)+10.,TX2,TWIN(3)+10.)
	call disp_line(TX2,TWIN(3)+10.,TX2,TWIN(4)-10.)

	RETURN

	END

	SUBROUTINE FINDYLIMITS

#include "trackn.inc"

	call minmax_r(SPEK(NMIN),nmax-nmin+1,1,ymin,ymax)
	IF(YMIN.GT.0.) YMIN=0.
	YMAX=YMAX*1.1
	IF(YMAX.LE.YMIN) YMAX=YMIN+10.
	call setyvalf(ymin,yminf,ifunct)
	call setyvalf(ymax,ymaxf,ifunct)
	call setxvalf(nmin,xminf)
	call setxvalf(nmax,xmaxf)
	CALL put4LW(dwin,xminf,xmaxf,yminf,ymaxf)
	call disp_setscale(dwin,twin,xsca,ysca)
	call getyvalf(ymin,yminf,ifunct)
	call getyvalf(ymax,ymaxf,ifunct)
	call getxvalf(nmin,xminf)
	call getxvalf(nmax,xmaxf)

	RETURN

	END

	SUBROUTINE SETyvalF(yval,yvalf,ifuncty)

	integer*4 ifuncty

	IF(IFUNCTy.EQ.1) THEN
	  	yvalf=yval
	elseIF(IFUNCTy.EQ.2) THEN
	   IF(Yval.GT.0.) then
		yvalf=ALOG10(Yval)
	   else
		yvalf=0
	   endif
	ELSEIF(IFUNCTy.EQ.3) THEN
	   IF(Yval.GT.0.) then
		yvalf=SQRT(Yval)
	   else
		yvalf=0.
	   endif
	ENDIF

	return

	end

	SUBROUTINE GETyvalF(yval,yvalf,ifuncty)

	integer*4 ifuncty
	
	IF(IFUNCTy.EQ.1) THEN
	  	yval=yvalf
	elseIF(IFUNCTy.EQ.2) THEN
	   IF(Yvalf.GT.0.) then
		yval=10.**Yvalf
	   else
		yval=0
	   endif
	ELSEIF(IFUNCTy.EQ.3) THEN
	   IF(Yvalf.GT.0.) then
		yval=Yvalf*yvalf
	   else
		yvalf=0.
	   endif
	ENDIF

	return

	end

	SUBROUTINE SETxvalF(ixval,xvalf)

	xvalf=ixval

	return

	end

	SUBROUTINE GETxvalF(ixval,xvalf)

	ixval=xvalf

	return

	end

	real*4 FUNCTION DIFUNCT(yy,ifunct)

	integer*4 ifunct
	
	IF(IFUNCT.EQ.1) THEN
	   fyy=YY
	ELSEIF(IFUNCT.EQ.2) THEN
	   if(YY.gt.0.)then
		fyy=ALOG10(YY)
	   else
		fyy=0
	   endif
	ELSEIF(IFUNCT.EQ.3) THEN
	   IF(YY.gt.0.) then
		fyy=SQRT(YY)
	   else
		fyy=0
	   endif
	else
	   fyy=0
	ENDIF

	DIFUNCT=fyy

	return

	end
	
	function DFZERO(CH)
	
	dfzero=0
	return
	end

	function DFAUTOBG(CH)
	
#include "trackn.inc"
	
	ich=ch
	dfautobg=autobg.bp(ich,gside)
	
	return
	end

	FUNCTION DFSPEK(CH)

#include "trackn.inc"

	ICH=CH
	ich=min(max(ika,ich),ikl)
	dfspek=spek(ich)
*	YY=SPEK(ICH)
*	DFSPEK=DIFUNCT(yy,ifunct)

	RETURN

	END

	REAL*4 FUNCTION DFSPEK_PS(CH)

#include "trackn.inc"
	real*4 DIFUNCT

	ICH=CH
	ich=min(max(ika,ich),ikl)
*	dfspek=spek(ich)
	YY=SPEK(ICH)
	DFSPEK_PS=DIFUNCT(yy,ifunct)

	RETURN

	END


	FUNCTION DFBGD(XII)

#include "trackn.inc"

	IF(IFBGD.EQ.1) THEN
	   YY=BG0+BG1*(XII-N0BGD)
	ELSE
	   YY=0
	ENDIF
	dfbgd=yy
*	DFBGD=DIFUNCT(yy,ifunct)

	RETURN

	END

	SUBROUTINE ECALIBRATION(ICOB)

#include "trackn.inc"

	CHARACTER*10 CMD(6)	/'NEW','FILE','SAME','NO','CHANNEL','K(1)'/
	CHARACTER*40 FILECAL
	character*9 strk
	real ener1	/0./
	real ener2	/0./

	IF(ICOB.EQ.1) THEN
	   IF(MARKLE.EQ.MARKRI) RETURN
	   IF(ENER1.EQ.0 .AND. ENER2.EQ.0) THEN   
	     ENER1=1173.238
	     ENER2=1332.513
	   ENDIF
	   IF(ENER1.EQ.ENER2) RETURN
	   DO II=1,m$k+1
		ECAL(II)=0.
	   end do
	   IF(RMARKRI.NE.RMARKLE) ECAL(2)=(ENER2-ENER1)/(RMARKRI-RMARKLE)
	   ECAL(1)=ENER1-ECAL(2)*RMARKLE
	   IECAL=1
	   WRITE(6,'(2H (,F8.2,2H ,,F9.2,'' )    ('',F8.2,2H ,,F9.2,2H ))')
	1		 RMARKLE,ENER1,RMARKRI,ENER2
	   WRITE(6,'(''  A(1) = '',1PG,''       A(2) = '',1PG)')ECAL(1),ECAL(2)
	   RETURN
	ENDIF

	WRITE(6,*)
	WRITE(6,*) ' Old Energy Calibration'
	WRITE(6,*)
	DO II=1,m$k
	   WRITE(6,'('' A('',I1,'')   = '',1PG)') II-1,ECAL(II)
	end do
	   write(6,'('' A(1/2) = '',1pg)')ecal(m$k+1)
	if(iecal.eq.0) write(6,*)'Energy calibration presently not enabled'

	WRITE(6,*)
	lnch=INP_CMD('(N)ew,(F)ile,(S)ame,No,(C)hannel ',CMD,6,ICMD)
	if(lnch.eq.-1) return
	IF(ICMD.EQ.1) THEN
	   WRITE(6,*)
	   WRITE(6,*) ' New Energy Calibration'
	   do ii=1,m$k
	      write(strk,'(''A('',i1,'')  '')') ii-1
	      lnch=INP_R1(strk,ECAL(ii))
	      if(lnch.eq.-1) return
	      IECAL=1
	   end do
	   write(strk,'(''A(1/2)   '')')
	   lnch=inp_r1(strk,ecal(m$k+1))
	   sqrterm=ecal(m$k+1) .ne. 0.0
	   if(lnch .eq. -1)return
	      IECAL=1
	ELSEIF(ICMD.EQ.2) THEN
	   WRITE(6,*)
	   lnch=INP_ch('Filename ',FILECAL)
	   if(lnch.eq.-1) return
	   OPEN(UNIT=1,FILE=FILECAL,STATUS='OLD',READONLY,ERR=440)
	   READ (1,*,END=430,ERR=440) ECAL
	   IECAL=1
	ELSEIF(ICMD.EQ.3) THEN
	   IECAL=1
	ELSEIF(ICMD.EQ.4) THEN
	   IECAL=0
	ELSEIF(ICMD.EQ.5) THEN
	   WRITE(6,*)
	   lnch=INP_R2('CHANNEL(1),ENERGY(1) ',RMARKLE,ENER1)
	   if(lnch.eq.-1) return
	   lnch=INP_R2('CHANNEL(2),ENERGY(2) ',RMARKRI,ENER2)
	   if(lnch.eq.-1) return
	   DO II=1,m$k+1
		ECAL(II)=0.
	   end do
	   IF(RMARKRI.NE.RMARKLE) ECAL(2)=(ENER2-ENER1)/(RMARKRI-RMARKLE)
	   ECAL(1)=ENER1-ECAL(2)*RMARKLE
	   aener1=ener1
	   aener2=ener2
	   sqrterm=.false.
	   IECAL=1
	ELSEIF(ICMD.EQ.6) THEN
	   ECAL(1)=SPEK(0)
	   ECAL(2)=SPEK(1)
	   do ii=3,m$k+1
	      ECAL(ii)=0.
	   end do
	   IECAL=1
	   sqrterm=.false.
	ENDIF
	RETURN

430	CLOSE(UNIT=1)
	IECAL=1
	RETURN

440	WRITE(6,*) 'Read Error'
	CALL xtpbell
	CLOSE(UNIT=1)
	IECAL=0
	RETURN

	END

	SUBROUTINE WCALIBRATION

#include "trackn.inc"

	CHARACTER*10 CMD(5)	/'NEW','FILE','SAME','NO','CHANNEL'/
	CHARACTER*40 FILECAL
	character*6 strk

	WRITE(6,*)
	WRITE(6,*) ' Old FWHM Calibration'
	WRITE(6,*)
	DO II=1,m$k
	   WRITE(6,'('' A('',I1,'') = '',1PG)') II-1,WCAL(II)
	end do
	if(iwcal.eq.0) write(6,*)'Width calibration presently not enabled'

	WRITE(6,*)
	lnch=INP_CMD('(N)ew,(F)ile,(S)ame,No,(C)hannel ',CMD,5,ICMD)
	if(lnch.eq.-1) return

	IF(ICMD.EQ.1) THEN
	   WRITE(6,*)
	   WRITE(6,*) ' New FWHM Calibration'
	   do ii=1,m$k
	      write(strk,'(''A('',i1,'')  '')') ii-1
	      lnch=INP_R1(strk,wCAL(ii))
	      if(lnch.eq.-1) return
	      IWCAL=1
	   end do
	ELSEIF(ICMD.EQ.2) THEN
	   WRITE(6,*)
	   lnch=INP_CH('Filename ',FILECAL)
	   if(lnch.eq.-1) return
	   OPEN(UNIT=1,FILE=FILECAL,STATUS='OLD',READONLY,ERR=440)
	   READ (1,*,END=430,ERR=440) WCAL
	   IWCAL=1
	ELSEIF(ICMD.EQ.3) THEN
	   IWCAL=1
	ELSEIF(ICMD.EQ.4) THEN
	   IWCAL=0
	ELSEIF(ICMD.EQ.5) THEN
	   WRITE(6,*)
	   lnch=INP_R2('CHANNEL(1),FWHM(1) ',RMARKLE,FWHM1)
	   if(lnch.eq.-1) return
	   lnch=INP_R2('CHANNEL(2),FWHM(2) ',RMARKRI,FWHM2)
	   if(lnch.eq.-1) return
	   DO II=1,m$k
		WCAL(II)=0.
	   end do
	   IF(RMARKLE.NE.RMARKRI) WCAL(2)=(FWHM2-FWHM1)/(RMARKRI-RMARKLE)
	   WCAL(1)=FWHM1-WCAL(2)*RMARKLE
	   IWCAL=1
	ENDIF

	WRITE(6,*)
	WRITE(6,*)  'Significance for peaksearch is ',WFIND
	lnch=INP_R1('Significance for peaksearch    ',WFIND)
	if(lnch.eq.-1) return
	WFIND=ABS(WFIND)
	wfind=max(1.,wfind)

	RETURN

430	CLOSE(UNIT=1)
	IWCAL=1
	RETURN

440	WRITE(6,*) 'Read Error'
	CALL xtpbell
	CLOSE(UNIT=1)
	IWCAL=1

	RETURN

	END

	SUBROUTINE DEFINEDISPLAY

#include "trackn.inc"

C	CHARACTER*10 DRAWCMD(3)	/'HISTOGRAM','VECTOR','POINTS'/
	CHARACTER*12 FUNCCMD(3)	/'NORMAL','LOGARITHMIC','SQRT'/

	call xtptermfocus

C	lnch=INP_CMD('Draw Type  (H)istogram,(V)ector,(P)oints ',DRAWCMD,3,IIC)
C	if(lnch.eq.-1) return
C	ITYP=-IIC	! se ityp negativo disabilita compressione disegno

	WRITE(6,*)
	lnch=INP_CMD('Y_display function   (N)orm, (L)og, (S)qrt    ',FUNCCMD,3,IIC)
	if(lnch.eq.-1) return
	IF(IIC.GT.0 .AND. IIC.NE.IFUNCT) THEN
	   IFUNCT=IIC
	   CALL FINDYLIMITS
	   RETURN
	ENDIF

	WRITE(6,*)
	WRITE(6, '('' Old  X-limits are   '',2I8)') NMIN,NMAX
	lnch=INP_I2('Input new         ',NMIN,NMAX)
	if(lnch.eq.-1) return
	IF(NMAX.LT.NMIN) THEN
	   NMIN=IKA
	   NMAX=IKL
	ELSE
	   NMIN=MIN(MAX(IKA,NMIN),IKL)
	   NMAX=MAX(MIN(IKL,NMAX),IKA)
	ENDIF
	IF(NMAX.LT.NMIN) CALL SWAPI(NMAIN,NMAX)

	WRITE(6,*)
	WRITE(6, '('' Old  Y-limits are   '',1PG13.6,5X,G13.6)') YMIN,YMAX
	lnch=INP_R2('Input new         ',YMIN,YMAX)
	if(lnch.eq.-1) return
	IF(YMAX.LE.YMIN) THEN
	   CALL FINDYLIMITS
	ENDIF

*	WRITE(6,*)
*	lnch=INP_ask('Are you using Xterm',xterm)
*	if(lnch.eq.-1) return

	WRITE(6,*)
	lnch=INP_r4('Input terminal window x0,x1,y0,y1',TWx0,TWx1,TWy0,TWy1)
	if(lnch.eq.-1) return
	if(twx0.ge.twx1) then
		TWx0=100.	! Finestra default
		TWx1=1000.
	endif
	if(twy0.ge.twy1) then
		TWy0=50.
		TWy1=650.
	endif
	CALL put4LW(twin,TWx0,TWx1,TWy0,TWy1)
	call disp_setscale(dwin,twin,xsca,ysca)

	write(6,*)
	write(6,*) 'Tutti i prossimo valori sono in unita'' Tektronix'
	lnch=INP_r2('Spaziatura tra le scritte x,y',ticx,ticy)
	if(lnch.eq.-1) return
	if(ticx.lt.40) ticx=100
	if(ticy.lt.20) ticy=80
	lnch=INP_r1('Lunghezza del trattino',ticl)
	if(lnch.eq.-1) return
	if(ticl.lt.0) ticl=8
	lnch=INP_r2('Dimensioni del carattere x,y',chrx,chry)
	if(lnch.eq.-1) return
	if(chrx.lt.0) chrx=12
	if(chry.lt.0) chrx=24	   
	write(6,*)
	lnch=INP_i1('Scritte sulle X in canali (0) o calibrate (1)',lxcal)
	if(lnch.eq.-1) return
	if(lxcal.ne.0) lxcal=1
	lnch=INP_str('Scritta sull''asse X',labelx)
	if(lnch.eq.-1) return

	RETURN

	END

	SUBROUTINE BGDLIN(ishow)
 
#include "trackn.inc"
	DIMENSION MBGD(12)

	IF(NMBGD.EQ.0) THEN
	   IFBGD=0
	   RETURN
	ENDIF

	IF(MOD(NMBGD,2).NE.0) GOTO 100

	DO II=1,NMBGD
	   MBGD(II)=XMBGD(II)
	end do
	DO II=1,NMBGD,2
	   IF(MBGD(II).GT.MBGD(II+1)) CALL SWAPI(MBGD(II),MBGD(II+1))
	end do

	N0BGD=MBGD(1)-1
	DO II=1,NMBGD
	   IF(MBGD(II).LT.N0BGD) N0BGD=MBGD(II)
	end do

	S1=0.
	SX=0.
	SXX=0.
	SY=0.
	SXY=0.

	DO II=1,NMBGD,2
	  DO JJ=MBGD(II),MBGD(II+1)
	    XX=JJ-N0BGD+0.5		! al centro del canale
	    YY=SPEK(JJ)
	    EI=1./ERR2(jj)
	    S1=S1+EI
	    SX=SX+XX*EI
	    SXX=SXX+XX*XX*EI
	    SY=SY+YY*EI
	    SXY=SXY+XX*YY*EI
	  end do
	end do

	DETER=S1*SXX-SX*SX
	IF(DETER.LE.0.) GOTO 100
	BG0=(SXX*SY-SX*SXY)/DETER
	BG1=(S1*SXY-SX*SY)/DETER
	E2BG0=SXX/DETER
	E2BG1=S1/DETER
	EBG01=-SX/DETER
	IFBGD=1
	if(ishow.eq.1) call showbgd
	RETURN

100	IFBGD=0
	BREAK=.TRUE.
	CALL xtpbell
	RETURN

	END

	SUBROUTINE SHOWBGD

#include "trackn.inc"

	EXTERNAL DFBGD

	IF(IFBGD.NE.1) RETURN

	call disp_setscale(dwin,twin,xsca,ysca)

	CH1=XMBGD(1)
	CH2=XMBGD(1)
	DO II=1,NMBGD
	   CH=XMBGD(II) !+0.5
	   CH1=MIN(CH1,CH)
	   CH2=MAX(CH2,CH)
	enddo

	CALL xtpoverlay(DFBGD,CH1,CH2,0.25)

	RETURN

	END

	SUBROUTINE INTEGR(ishow,llunout)

#include "trackn.inc"
	integer*4 ikkk

	IF(NMINT.lt.2) GOTO 100
	if( (nmint-(nmint/2)*2) .eq. 1 )then
	   nmint=nmint-1
	   write(6,'('' WARNING - odd number of markers, last deleted'')')
	endif
	if(IFBGD .eq. 1)then
	  nmbgd=(nmbgd/2)*2
	  if(nmbgd .lt. 2)then
	     write(6,'('' ERROR - less than TWO background markers'')')
	     goto 100
	  endif
	  iminbgd=xmbgd(1)
	  imaxbgd=xmbgd(1)
	  do kkk=1,nmbgd
	     iminbgd=min(iminbgd,int(xmbgd(kkk)))
	     imaxbgd=max(imaxbgd,int(xmbgd(kkk)))
	  enddo
	endif
	
	do kkk=1,nmint-1,2

	II1=XMINT(kkk)
	II2=XMINT(kkk+1)
	IF(II1.GT.II2) CALL SWAPI(II1,II2)
	if(IFBGD .eq. 1)then
	 if( (ii1 .lt. iminbgd) .or. (ii2 .gt. imaxbgd) )then
	   write(6,'('' ERROR - region '',i5,'':'',i5,'' skiped (outside background region)'')')
	   goto 10
	 endif
        endif

	XC0=0
	XC1=0
	XC2=0
	XD0=0
	XD1=0
	XD2=0

	NIREL=(II1+II2)/2
	DO II=II1,II2
	   XWW=II-NIREL+0.5		! al centro del canale
	   XYY=SPEK(II)
	   XSY=ERR2(II)
	   IF(IFBGD.EQ.1) THEN
		XBG=BG0+BG1*(II-N0BGD)
		XYY=XYY-XBG
		sbg_nm=e2bg0+e2bg1*(0.5+float(ii-n0bgd))**2   ! N.M.
		XSY=abs(XSY)+abs(sbg_nm)  !+abs(XBG)          ! N.M.
	   ENDIF
	   XC0=XC0+XYY
	   XC1=XC1+XYY*XWW
	   XC2=XC2+XYY*XWW*XWW
	   XD0=XD0+XSY
	   XD1=XD1+XSY*XWW
	   XD2=XD2+XSY*XWW*XWW
	end do
C	IF(XD0.LT.0) GOTO 100
	AREA=XC0
	DAREA=SQRT(XD0)
	IF(XC0.EQ.0) GOTO 100
	XPOS=XC1/XC0
	POSI=NIREL+XPOS
	DPOSI=(XD2-2*XD1*XPOS+XD0*XPOS**2)/XC0**2
C	IF(DPOSI.LE.0) GOTO 100
	DPOSI=SQRT(abs(DPOSI))
	FWHM=XC2/XC0-XPOS**2
	IF(FWHM.LE.0) GOTO 100
	FWHM=2.35482*SQRT(FWHM)
	IFINT=1
	ikkk=kkk/2+1
	if(ishow.eq.1) call showint(llunout,ikkk)
10	continue
	enddo
	RETURN

100	IFINT=0
	BREAK=.TRUE.
	CALL xtpbell
	RETURN

	END

	SUBROUTINE SHOWINT(lLUNOUT,ii)

#include "trackn.inc"
	character*80 string
	integer*4 ii

	IF(IFINT.NE.1) RETURN

	call showmarkers(xmint,nmint,2,3)
	call xtpshownumpeak(POSI,ii,3)

	  genergy = cpol(POSI,ECAL,-m$k)
	  sgenergy = (cpol(POSI+dposi/2,ECAL,-m$k)-cpol(POSI-dposi/2,ECAL,-m$k))

	  if( efficor )then
	    rrr = effvalue(genergy)
	    area = area/rrr
	    darea = darea/rrr
	 endif

	  k_se = nint(sgenergy*100.00)
	  if( k_se .ge. 0 .and. k_se .lt. 10 )then
	    write(string,'(''('',i1,'')      '')')k_se
	  elseif( k_se .ge. 10 .and. k_se .lt. 100 )then
	    write(string,'(''('',i2,'')     '')')k_se
	  elseif( k_se .ge. 100 .and. k_se .lt. 1000 )then 
	    write(string,'(''('',i3,'')    '')')k_se
	  elseif( k_se .ge. 1000 .and. k_se .lt. 10000 )then
	    write(string,'(''('',i4,'')   '')')k_se
	  else
	    write(string,'(''(err)         '')')
	  endif

	  IF(ABS(AREA).LT.9.9E7) THEN
	    WRITE(string(10:),'(i10,''('',i10,'')'')')nint(AREA),nint(darea)
          ELSE
	    WRITE(string(10:),'(1PE13.6,''('',e13.6,'')'')') AREA,darea
	  ENDIF
	  call string_strip(string(10:))
	  iss=lengthc(string(10:))+9
	  if(iecal.eq.0) then
	    rrr=FWHM
	  else
	    rrr=abs(cpol(POSI+FWHM/2,ECAL,-m$k)-cpol(POSI-FWHM/2,ECAL,-m$k))
	  endif
	  write(6,'(i3,2x,2f10.2,a,$)')ii,posi,genergy,string(1:iss)
	  write(6,'(''   FWHM : '',f10.2)')rrr

	  if(llunout.gt.0 .and. llunout.ne.6) then
	    ttt=abs(cpol(POSI+FWHM/2,ECAL,-m$k)-cpol(POSI-FWHM/2,ECAL,-m$k))
	    IF(ABS(AREA).LT.9.9E8) THEN
	      WRITE(lLUNOUT,'(1X,A,2F10.3,f8.3,F13.2,f13.2,2F9.3,i3)')
	1	FNAMEI(1:20),POSI,genergy,sgenergy,AREA,darea,FWHM,ttt,ii
	    ELSE
	      WRITE(lLUNOUT,'(1X,A,2F10.3,f8.3,1PE13.6,e13.6,2F9.3,i3)')
	1	FNAMEI(1:20),POSI,genergy,sgenergy,AREA,darea,FWHM,ttt,ii
	    ENDIF
	    call flush(llunout)
	  endif


	RETURN

	END

	SUBROUTINE AUTOPEAK(POSINP,itipo)

#include "trackn.inc"

#define	 SMOO(IP) SPEK(IP-1)+2*SPEK(IP)+SPEK(IP+1)	
	 ! SMOOTH DELLO SPETTRO

	IF(IWCAL.NE.1) THEN
	   WRITE(6,*)
	1	 'To perform this command you should set the FWHM calibration'
	   WRITE(6,*)
	   call xtptermfocus
	   CALL WCALIBRATION
	   IF(IWCAL.NE.1) RETURN
	ENDIF

CCCCCCCCCC TROVA IL MASSIMO LOCALE

	WW=cpol(POSINP,WCAL,-m$k)+0.5
	NWW=MAX(1.,WW)
	NWW3=3*NWW

	INPOS=POSINP
	SMOOMAX=SMOO(INPOS)
	DO NN=INPOS-NWW3,INPOS+NWW3
	   IF(SMOO(NN).GT.SMOOMAX) THEN
		SMOOMAX=SMOO(NN)
		INPOS=NN
	   ENDIF
	end do

CCCCCCCCCC DETERMINA LA REGIONE DI INTEGRAZIONE

	MINP=INPOS-1
	DO NN=1,NWW3
*	   IF(spek(MINP-1).GT. spek(MINP)+1.70*sqrt(err2(minp)+err2(minp-1)) ) GOTO 30
	   IF(spek(MINP)+spek(INPOS) .GT. 
     &   2.0*(spek((MINP+INPOS)/2)+2.00*sqrt(err2((minp+INPOS)/2)))
     &   .and. spek(MINP) .LT. spek(MINP-1) )goto 30
	   MINP=MINP-1 
	end do
30	CONTINUE

	MAXP=INPOS
	DO NN=1,NWW3
*	   IF(spek(MAXP+1).GT. spek(MAXP)+1.70*sqrt(err2(maxp)+err2(maxp+1)) ) GOTO 40	
	   IF(spek(maxp)+spek(INPOS) .GT.
     &    2.0*(spek((maxp+INPOS)/2)+2.00*sqrt(err2((maxp+INPOS)/2)))
     &    .AND. spek(MAXP) .LT. spek(MAXP+1) )goto 40
	   MAXP=MAXP+1
	end do
40	CONTINUE

	NCAN=MAXP-MINP+1
	NCANB=MAX(NCAN/5,2)
	XMBGD(1)=MINP-NCANB+1
	XMBGD(2)=MINP+0.500
	XMBGD(3)=MAXP+0.500
	XMBGD(4)=MAXP+NCANB-1
	NMBGD=4
	XMINT(1)=MINP+0.500
	XMINT(2)=MAXP+0.500
	NMINT=2
	XMREG(1)=MINP+0.500
	XMREG(2)=MAXP+0.500
	NMREG=2
	NMGAU=1
	XMGAU(1)=INPOS+0.5

	ifbgd=0
	ifmint=0
	ifgau=0
	if(itipo.eq.1) then
	   CALL BGDLIN(0)
	   CALL INTEGR(0,0)
	   IF(IFINT .NE. 1    .OR.
	1     POSI  .LE. MINP .OR.
	1     POSI  .GE. MAXP .OR.
	1     AREA  .LE. 0    .OR.
	1     FWHM  .LE. WW/2 .OR.
	1     FWHM  .GT. WW*2 ) THEN
	  	IFINT=0
		NMINT=0
		IFBGD=0
		NMBGD=0
		POSINP=-1
		RETURN
	   ENDIF
	   CALL SHOWBGD
	   CALL SHOWINT(lunout,1)
	   POSINP=POSI
	   RETURN
	elseif(itipo.eq.2) then
	   call bgdlin(0)
	   CALL INTEGR(0,0)
	   IF(IFINT .NE. 1    .OR.
	1     POSI  .LE. MINP .OR.
	1     POSI  .GE. MAXP .OR.
	1     AREA  .LE. 0    .OR.
	1     FWHM  .LE. WW/5 .OR.
	1     FWHM  .GT. WW*5 ) THEN
	  	IFINT=0
		NMINT=0
		IFBGD=0
		NMBGD=0
		POSINP=-1
		RETURN
	   ENDIF
	   call gaussfit(1,lunout,2)
	   if(ifgau.eq.1) then
		posi=GFPAR(4)+N0GAU
		sigma=GFPAR(3)
		area=GFPAR(5)*sqrt(2*3.141593*sigma**2)
		fwhm=GFPAR(3)*2.35482
		posinp=posi
		ifint=1
		ifbgd=1
		bg0=GFPAR(1)
		bg1=GFPAR(2)
		n0bgd=ifix(xmreg(1))-1
                call showbgd
	   else
		nmreg=0
		nmgau=0
		posinp=-1
	   endif
	   return
	endif
#undef SMOO
	END

	SUBROUTINE INSERTPEAK(XPOS)

#include "trackn.inc"

	IF(NPEAKS.EQ.M$PEAKS) THEN
		WRITE(6,*) ' Peak buffer full'
		XPOS=-1
		RETURN
	ENDIF

	IF(XPOS.LT.1) THEN
	   CALL INP_R2('Energy',ENER,EOFFS)
	   CALL INP_HOWMANY(IIC)
	   if(iic .lt. 0)return
	   IF(IIC.EQ.2) ENER=ENER-EOFFS
	   IF(IECAL.EQ.1) THEN
	      XPOS=cpolinv(ENER,ECAL,-m$k)
	   ELSE
	      XPOS=ENER
	   ENDIF
	ENDIF
	IF(XPOS.LT.IKA .OR. XPOS .GT. IKL) THEN
	   WRITE(6,*) ' Value out of range'
	   CALL xtpbell
	   XPOS=-1
	   RETURN
	ENDIF
	DO II=1,NPEAKS
	   IF(XPOS.LT.PEAKS(II))THEN
		DO JJ=NPEAKS,II,-1
		   PEAKS(JJ+1)=PEAKS(JJ)
		end do
		NPEAKS=NPEAKS+1
		PEAKS(II)=XPOS
		RETURN
	   ENDIF
	end do
	NPEAKS=NPEAKS+1
	PEAKS(NPEAKS)=XPOS
	RETURN

	END

	SUBROUTINE DELETEPEAK(XPOS)

#include "trackn.inc"

	IF(NPEAKS.LE.0) THEN
	   XPOS=-1
	   RETURN
	ENDIF
	IF(XPOS.LT.IKA .OR. XPOS .GT. IKL) THEN
	   CALL xtpbell
	   XPOS=-1
	   RETURN
	ENDIF

	DDXX=1.0E7
	IIXX=-1
	DO II=1,NPEAKS
	   DX=ABS(XPOS-PEAKS(II))
	   IF(DX.LT.DDXX) THEN
	      IIXX=II
	      DDXX=DX
	   ENDIF
	end do
	IF(DDXX.LT.10.) THEN
	   XPOS=PEAKS(IIXX)
	   DO JJ=IIXX,NPEAKS-1
	      PEAKS(JJ)=PEAKS(JJ+1)
	   end do
	   NPEAKS=NPEAKS-1
	ELSE
	   XPOS=-1
	ENDIF

	RETURN

	END

	SUBROUTINE DEFINEPEAK

#include "trackn.inc"

	CHARACTER*50 PFILE
*	logical*1 inp_not

	call xtptermfocus
	if(npeaks.gt.0) then
	   if(.not.inp_not('Add to the previous peaks')) npeaks=0
	endif
	IF(NPEAKS.EQ.M$PEAKS) THEN
	   WRITE(6,*) ' Peak buffer full'
	   RETURN
	ENDIF
	nm_dum=INP_CH('Filename',PFILE)
	OPEN(UNIT=1,FILE=PFILE,FORM='FORMATTED',STATUS='OLD',ERR=12)
10	READ(1,*,ERR=12,END=12) PENER
	IF(IECAL.EQ.1) THEN
	   XPOS=cpolinv(PENER,ECAL,-m$k)
	ELSE
	   XPOS=PENER
	ENDIF
	IF(XPOS.GT.0) CALL INSERTPEAK(XPOS)
	IF(NPEAKS.LT.M$PEAKS) GOTO 10
	WRITE(6,*) ' Peak buffer full'
12	CLOSE(1)

	RETURN

	END

	SUBROUTINE PEAKSEARCH(iredisp)

#include "trackn.inc"

*#if defined(__IFC) || defined(__INTEL_COMPILER)
*	automatic IC(64)
*#else
	DIMENSION IC(64)
*#endif
	IF(IWCAL.NE.1) THEN
	   WRITE(6,*)
	1	 'To perform this command you should set the FWHM calibration'
	   WRITE(6,*)
	   call xtptermfocus
	   CALL WCALIBRATION
	   IF(IWCAL.NE.1) RETURN
	ENDIF

	GMIN = cpol(float(nmin),WCAL,-m$k)
	GMAX = cpol(float(nmax),WCAL,-m$k)

	GG=MAX(GMIN,GMAX)
	IF(GG.GT.18. .and. GMIN .gt.18.0 .and. GMAX .gt. 18.0 ) RETURN
	IF(GG.GT.18.) then
	   GG = 17.999900
	   if(GMAX .gt. GG) GMAX = GG
	   if(GMIN .gt. GG) GMAX = GG
	endif
	   
	DO II =1,M$PEAKS
	   PEAKS(II) = 0
	end do
	NPEAKS=0

	M1=IFIX(0.3*GG)*5+1
        J1 = NMIN+M1
	J2 = NMAX-M1
	K1 = 32-M1
	K2 = 32+M1
	I1 = J1
	I2 = J1
	I3 = J1
	I5 = J1
	MM=0
	N1=0
	HWB1=gmin
	DELTA=(GMAX-GMIN)/MAX(1,NMAX-NMIN)

#if !defined(Digital)
!DEC$ NOUNROLL
#endif
	DO II = J1 ,J2
	   GAMMA = HWB1+DELTA*(II-NMIN)
	   WW=0.6*GAMMA
*	write(*,*)gamma, ww, HWB1
*	pause
	   MNEW=WW*0.5
	   SIG=GAMMA/2.35482
	   SISQ=SIG*SIG
	   WSQ=WW*WW

#if !defined(Digital)
!DEC$ NOUNROLL
#endif
	   DO IZ=1,5
		SIGM=(0.5+0.5*SQRT(1.+WSQ/(3.*SISQ)))*SISQ
		SISQ=SIGM
	   end do
	   N1NEW=2.*SQRT(SISQ)+0.5
	   IF(MM.NE.MNEW .OR. N1.NE.N1NEW) THEN
		MM=MNEW
		N1=N1NEW
		CALL SECO(IC,MM)
	   ENDIF
	   CALL MULT(K1,K2,II,SS,FF,IC)
	   IF(II.EQ.J1) GOTO 90
	   MT = 3
	   IF (SS.GT.0.) MT = 2
	   IF (SS.GT.FF) MT = 1
	   IF (S1.GT.F1) THEN
		IF (MT.EQ.1) GOTO 90
		IF (MT.EQ.3) I3 =II
		I2 = II-1
		GOTO 90
	   ENDIF
	   IF (S1.GT.0.) THEN
		IF (MT.EQ.2) GOTO 90
		IF (MT.EQ.3) I3=II
		IF (MT.NE.1) GOTO 90
		I1 = II
		GOTO 90
	   ENDIF
	   IF (MT.EQ.3) GOTO 90
	   I5 = II-1
	   SM = 0.
	   A0=0.
	   A1=0.
#if !defined(Digital)
!DEC$ NOUNROLL
#endif
	   DO JJ = I3,I5
		CALL MULT(K1,K2,JJ,S2,F2,IC)
		A0=A0+JJ*S2
		A1=A1+S2
		IF (SM.GE.S2) THEN
		   FM = F2
		   SM = S2
		ENDIF
	   end do
	   FM2 = WFIND*FM
	   SMA = ABS(SM)
	   IF (SMA.LE.FM2) GOTO 90
	   I53 = I5-I3+1
	   IE = IABS(I53-N1)
	   IF (IE.GT.2) GOTO 90
	   FS = FM/SMA
	   N2 = FS*0.5*(N1+2)+0.5
	   I32 = I3-I2-1
	   IF (N2.LT.1) N2 = 1
	   IF (I32.GT.N2) GOTO 90
	   FS2 = 1.-2.*FS
	   N3 = FS2*(N1-2)+0.5
	   I21 = I2-I1+1
	   IF (I21.LT.N3) GOTO 90
	   NPEAKS = NPEAKS+1
	   PEAKS(NPEAKS)=A0/A1
90	   S1 = SS
	   F1 = FF
	end do

	RETURN

	END
 
	SUBROUTINE SECO(IC,MM)

	DIMENSION IB(32),IC(64)

	DO II=1,32
	   IB(II)=0
	   IC(II)=0
	end do
	IC(31)=1
	IC(32)=-2
	IC(33)=1
	DO II=34,64
	   IC(II)=0
	end do
	I1=31
	DO IZ=1,5
	   I1=I1-MM
	   DO II=I1,32
		L1=II-MM
		L2=II+MM
		DO LL=L1,L2
		   IB(II)=IB(II)+IC(LL)
		end do
	   end do
	   DO II=I1,32
		IC(II)=IB(II)
		I2=64-II
		IC(I2)=IB(II)
		IB(II)=0
	   end do
	end do

	RETURN

	END
 
	SUBROUTINE MULT(K1,K2,II,SS,FF,IC)

#include "trackn.inc"

	DIMENSION IC(64)

	SS=0.
	FF=0.
	LL=II-33+K1
	DO KK=K1,K2
	   LL=LL+1
	   CK=IC(KK)
	   XG=CK*max(0.,SPEK(LL))
	   SS=SS+XG
	   FF=FF+CK*XG
	end do
	FF=SQRT(FF)

	RETURN

	END

	SUBROUTINE SHOWMARKERS(XMARK,NMARK,npair,icolor)

#include "trackn.inc"
	integer*4 icolor,jj

*	DIMENSION XMARK(MAX(1,NMARK))
	DIMENSION XMARK(1)

        jj=0
	DO II=1,NMARK
	   XINP=XMARK(II)
	   IF(NPAIR.EQ.2)then
	    if( MOD(II,2).EQ.0) CALL xtpdoubleMARKER(XMARK(II-1),XMARK(II),icolor)
	    jj=jj+2
	   ELSE
	    CALL xtpMARKER(XINP,icolor)
	    jj=jj+1
	   endif
	end do
        if(jj .lt. nmark)CALL xtpMARKER(XMARK(NMARK),icolor)
	
	RETURN

	END

	SUBROUTINE SHOWTOT(NTAPES,POSTOT,DPOSTOT,NQREG)

	PARAMETER (N$T=1000)
	PARAMETER (N$P=50)

	DIMENSION POSTOT(N$T,N$P),DPOSTOT(N$T,N$P)
	character*80 string
	dimension dwin(4),twin(4)

	CALL DISP_INITT

	DO IQREG=1,NQREG
	   call minmaxeb_r(postot(1,iqreg),dpostot(1,iqreg),ntapes,1,ymin,ymax)
	   dyy=ymax-ymin
	   if(dyy.lt.2.) then
	     yym=(ymin+ymax)/2.
	     ymax=yym+1.
	     ymin=yym-1.
	   endif
	   dyy=(ymax-ymin)/20.
	   ymin=ymin-dyy
	   ymax=ymax+dyy
	   CALL put4LW(dwin,0.,NTAPES+1.,YMIN,YMAX)
	   CALL SETRFRAME(1,1,NQREG+1,IQREG,twin)
	   call disp_setscale(dwin,twin,xsca,ysca)
	   call disp_window(twin)
	   IYMIN=YMIN
	   IYMAX=YMAX+1
	   DO II=IYMIN,IYMAX
		call disp_map(1.,float(ii),tx,ty,dwin,twin)
		if(ty.ge.twin(3) .and. ty.le.twin(4)) then
		   CALL DISP_MOVABS(TWIN(1)   ,TY)
		   CALL DISP_DRWABS(TWIN(1)+5.,TY)
		   CALL DISP_MOVABS(TWIN(2)   ,TY)
		   CALL DISP_DRWABS(TWIN(2)-5.,TY)
		endif
	   end do
	   call DISP_Yvec_eb(postot(1,iqreg),dpostot(1,iqreg),
	1			1,ntapes,1,dwin,twin)
	   call DISP_Yvec(postot(1,iqreg),1,ntapes,1,2,dwin,twin)
	   WRITE(string,'(I4)') IQREG
	   call disp_text(TWIN(1)+20,TWIN(4)-20,textsize,textang,string)
	end do

	RETURN

	END

	SUBROUTINE SETRFRAME(NFRAMESX,IFRAMEX,NFRAMESY,IFRAMEY,twin)

	IIX=max(1,ABS(NFRAMESX))
	NNX=max(1,ABS(IFRAMEX))
	IIY=max(1,ABS(NFRAMESY))
	NNY=max(1,ABS(IFRAMEY))

	ITXD=1000/IIX
	XT0=5+(ITXD+2)*(NNX-1)
	XT1=XT0+ITXD
	ITYD=750/IIY
	YT0=5+(ITYD+2)*(NNY-1)
	YT1=YT0+ITYD
	call put4LW(twin,xt0,xt1,yt0,yt1)

	RETURN

	END

	SUBROUTINE MEANval_r(vect,Nn,istep,vmean)

	DIMENSION vect(1)

	vmean=0
	IF(Nn.LE.0) RETURN

	ii=1
	DO Inn=1,Nn
	   vmean=vmean+vect(II)
	   ii=ii+istep
	end do

	vmean=vmean/Nn

	RETURN

	END

	subroutine minmax_r(vect,nn,istep,vmin,vmax)

	dimension vect(1)

	if(nn.lt.1) then
	   vmin=0
	   vmax=0
	else
	   vmin=vect(1)
	   vmax=vmin
	   ii=1
	   do inn=2,nn
		ii=ii+istep
		vmin=min(vmin,vect(ii))
		vmax=max(vmax,vect(ii))
	   end do
	endif

	return

	end

	subroutine minmaxeb_r(vect,dvect,nn,istep,vmin,vmax)

	dimension vect(1),dvect(1)

	if(nn.lt.1) then
	   vmin=0
	   vmax=0
	else
	   dd=abs(dvect(1))
	   vmin=vect(1)
	   vmax=vect(1)
	   dd1=dvect(1)
	   ii=1
	   do inn=2,nn
		ii=ii+istep
		dd1=dd1 + dvect(ii)
		vmin=min(vmin,vect(ii))
		vmax=max(vmax,vect(ii))
	   end do
	   dd1=dd1/nn
	   dd2=0.
	   idd=0
	   do inn=1,nn
	     if(dvect(ii).le.dd1) then
	       dd2=dd2 + dvect(ii)
	       idd=idd+1
	     endif
	   end do
	   if(idd.gt.1) dd2=dd2/idd
	   vmin=vmin-dd2
	   vmax=vmax+dd2
	endif

	return

	end

	subroutine smooth(xx,nch,nsmoo)

*	dimension xx(max(1,nch))
	dimension xx(1)

	if(nsmoo.eq.0) return
	do ii=1,abs(nsmoo)
	  x1=0
	  do jj=1,nch-1
	    x2=xx(jj)
	    xx(jj)=(x1+2*x2+xx(jj+1))/4	     
	    x1=x2
	  end do
	  xx(nch)=(x1+2*xx(nch)+0.)/4
	end do

	return

	end

	subroutine Curfit(Par,NPar0,Ndat,Fderiv,Fchisq,Istat,ishow,chisqr)

	Real Par(100)

	Real Deriv(100)
	Real alpha(5050),beta(100),sq(100)
	Real array(5050),bvec(100),var(100)

	if(Npar0 .lt. 0)then
	  NPar=1-NPar0
	 else
	  Npar=NPar0
	endif

	NTpar=(NPar*(NPar+1))/2

	chisqr=Fchisq(Par,NPar)
	Istat=0

	DO Ii=1,NPar
	   Var(Ii)=Par(Ii)
	end do

	FLAMDA=0.001D0
	iter=0

300	chisqt=chisqr
	DO J=1,NPar
	   BETA(J)=0.
	end do
	DO J=1,NTpar
	   ALPHA(J)=0.
	end do

	DO i=1,Ndat
	  Kv=Fderiv(i,Par,NPar,Deriv,dyi,wi)
	  DO J=1,NPar
	    derj=deriv(j)
*	    if(derj.ne.0.) then
	    if(abs(derj).gt.1.0E-8) then
		BETA(J)=BETA(J)+wi*dyi*DERJ
		widerj=wi*derj
		L=(J*(J-1))/2
		DO  K=1,J
		   L=L+1
		   ALPHA(L)=ALPHA(L)+DERIV(K)*wiDERJ
		end do
	    endif
	  end do
	end do

	DO J=1,NPar
	   JJ=J*(J+1)/2    
	   IF(ALPHA(JJ).LT.1.E-15) ALPHA(JJ)=1.E-15  
	   SQ(J)=SQRT(ALPHA(JJ))    
	   BETA(J)=BETA(J)/SQ(J)    
	end do

100	L=1
	DO J=1,NPar
	   bvec(j)=beta(j)
	   DO K=1,J
		ARRAY(L)=ALPHA(L)/(SQ(J)*SQ(K)) 
		L=L+1                           
	   end do
	   ARRAY(J*(J+1)/2)=1.+FLAMDA 
	end do

	CALL LinGls(Array,Bvec,NPar,KV)
	IF(KV.NE.0) then
	   Istat=4    
	   return
	endif

	DO J=1,NPar
	   Var(J)=Par(J)+Bvec(J)/SQ(J)
	end do

	chisq1=Fchisq(var,nPar)

	if (chisq1.gt.chisqr) then
	   FLAMDA=10.*FLAMDA            
	   IF(FLAMDA.le.1.E20) GOTO 100
	   Istat=5
	   return
	endif

	DO J=1,NPar  
	   Par(J)=Var(J)    
	end do

	CHISQR=CHISQ1  
	if(chisqr.ne.0) then
	   DCHI=(CHISQT-CHISQR)/CHISQR*100.  
	else
	   dchi=0
	endif

	if(ishow.eq.1) Write (6,'(I3,1H.,3F)') Iter, flamda, chisqr, dchi

	FLAMDA=0.10D0*FLAMDA  
	flamda=max(flamda,1.0e-7)
	ITER=ITER+1   
	IF ((DCHI.GT.0.1).and.(iter.lt.20))  GOTO 300 

	RETURN

	END

	subroutine Curfit1(Par,Npar,Ndat,Fderiv,Fchisq,Fsetdat,
	1	Istat,ishow,chisqr)

*	Real Par(max(1,Npar))
	Real Par(1)

	Real Deriv(100),dyi,wi
	Real alpha(5050),beta(100),sq(100)
	Real array(5050),bvec(100),var(100)
	
	integer Fsetdat, Fderiv
	external Fsetdat, Fderiv, Fchisq

	NTpar=(Npar*(Npar+1))/2

	ii=Fsetdat(Par,Npar)
	chisqr=Fchisq(Par,Npar)
	Istat=0

	DO Ii=1,Npar
	   Var(Ii)=Par(Ii)
	end do

	FLAMDA=0.001D0
	iter=0
	nwri=0

300	chisqt=chisqr
	DO J=1,Npar
	   BETA(J)=0.0D0
	end do
	DO J=1,NTpar
	   ALPHA(J)=0.0D0
	end do

	DO i=1,Ndat
	  Kv=Fderiv(i,Par,Npar,Deriv,dyi,wi)
	  DO J=1,Npar
	    derj=deriv(j)
	    if(derj.ne.0.0D0) then
		BETA(J)=BETA(J)+wi*dyi*DERJ
		widerj=wi*derj
		L=(J*(J-1))/2
		DO  K=1,J
		   L=L+1
		   ALPHA(L)=ALPHA(L)+DERIV(K)*wiDERJ
		end do
	    endif
	  end do
	end do

	DO J=1,Npar
	   JJ=J*(J+1)/2    
*	   write(6,*)j,jj,alpha(jj)
	   IF(ALPHA(JJ).LT.1.E-15) ALPHA(JJ)=1.E-15  
	   SQ(J)=SQRT(ALPHA(JJ))    
	   BETA(J)=BETA(J)/SQ(J)    
	end do

100	L=1
	DO J=1,Npar
	   bvec(j)=beta(j)
	   DO K=1,J
		ARRAY(L)=ALPHA(L)/(SQ(J)*SQ(K)) 
		L=L+1                           
	   end do
	   ARRAY(J*(J+1)/2)=1.0D0+FLAMDA 
*	   write(6,*) Par(j), Var(j),Bvec(j),SQ(j)
	end do

	CALL LinGls(Array,Bvec,Npar,KV)
	IF(KV.NE.0) then
	   Istat=4    
	   return
	endif

	DO J=1,Npar
	   Var(J)=Par(J)+Bvec(J)/SQ(J)
	end do
	ii=Fsetdat(var,Npar)

	chisq1=Fchisq(var,npar)

	if (chisq1.gt.chisqr) then
	   ii=Fsetdat(Par,Npar)
	   FLAMDA=10.*FLAMDA            
	   IF(FLAMDA.le.1.E20) GOTO 100
	   Istat=5
	   return
	endif

	DO J=1,Npar  
	   Par(J)=Var(J)    
	end do
	ii=Fsetdat(Par,Npar)

	CHISQR=CHISQ1  
*	if(chisqr.ne.0) then
	if(chisqr.gt.1.0E-3) then
	   DCHI=(CHISQT-CHISQR)/CHISQR*100.  
	else
	   dchi=0
	endif

*	if(ishow.eq.1) then
*	   nwri=nwri+1
*	   if(nwri.eq.1) then
*		write(6,'('' Chisq'',f8.2,''..'',$)') chisqr
*	   else
*		write(6,'(''\r'',f8.2,''..'',$)') chisqr
*	   endif
*	endif

	FLAMDA=0.10D0*FLAMDA  
	flamda=max(flamda,1.0e-7)
	ITER=ITER+1   
	IF ((DCHI.GT.0.1).and.(iter.lt.100))  GOTO 300 

	if(ishow.eq.1) write(6,'('' Chisq'',f8.2)') chisqr

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
	real*8 Sum
	
	DATA Epsi /1.000E-18/	! square of MAXIMUM RELATIV PRECISION

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
	      end do
	   endif
	   IF(Sum.lt.0.00D0) then
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
		  end do
		endif
		IF(Lsign(Iord).NE.0) Sum=-Sum
		Array(IK)=Sum/Array(II)
	     end do
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
	     end do
	   endif
	   IF(Lsign(Iord).NE.0) Sum=-Sum
	   Par(Iord)=Sum/Array(II)
	end do

	DO II=Nord,1,-1
	   Sum=Par(II)
	   IF(II.ne.Nord) then
	      DO JJ=II+1,Nord
		IJ=JJ*(JJ-1)/2+II
		Sum=Sum-Par(JJ)*Array(IJ)
	      end do
	   endif
	   Par(II)=Sum/Array(II*(II+1)/2)
	end do

	RETURN

	END

	function pFDER(id,Par,Npar,Deriv,dyi,wi)

#include "trackn.inc"

	real Par(30),Deriv(30)

	xi=xfit(id)
	dyi=yfit(id)-cpol(xi,par,npar)
	wi=dyfit(id)
	fac=1
	do ii=1,npar
	   deriv(ii)=fac
	   fac=fac*xi
	end do
	pfder=0

	return

	end

	function pFCHI(Par,Npar)

#include "trackn.inc"

	dimension par(30)

	chisqr=0
	do ii=1,ndata
	   chisqr=chisqr+dyfit(ii)*(yfit(ii)-cpol(xfit(ii),Par,Npar))**2
	end do
	chisqr=chisqr/max(1,(ndata-npar))
	pFCHI=chisqr

	return

	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	SUBROUTINE GAUSSFIT(ishow,llunout,ibgord)

#include "trackn.inc"
***** N.M.
	real x_nm(M$CH),y_nm(M$CH),sy_nm(M$CH)
	real p_nm(M$P),sp_nm(M$P),pl_nm(M$P),pu_nm(M$P),cf_nm(M$P)
	real*8 pk_funct
	external pk_funct
	real*8 vw_funct
	external vw_funct
***** 
	external GFFUN,GFDER,GFCHI

	Character*30 Msg (7)
	Data Msg /'equal region markers',
	1	'peak mark outside fit region',
	1	'too less degrees of freedom',
	1	'matrix error',
	1	'F_lambda greater than 10**20',
	1	'unknown error',
	1	'secondary fit error'/

	If(NMREG.NE.2 .OR. NMGAU.EQ.0) then
	   WRITE(6,*) 'check your fit markers: 1..29 G, 2 R'
	   IFGAU=0
	   return
	Endif

	If (xmreg(1).gt.xmreg(2)) Call SWAPI(xmreg(1), xmreg(2))
	id1=xmreg(1)
	id2=xmreg(2)
	if (id1.eq.id2) then
	   kv=1
	   goto 99
	endif

	N0GAU=id1

	Ndata=0
	do ii=id1,id2
	  ix=ii-N0GAU
	  yyy=spek(ii)
	  Ndata=Ndata+1
	  xfit(Ndata)=ix
	  yfit(Ndata)=yyy
	  dyfit(Ndata)=1./err2(ii)
	end do

****  [Refit] data init.   N.M.
	np_nm=2*nmgau+1
*	allocate(x_nm(ndata),y_nm(ndata),sy_nm(ndata))
*	allocate(p_nm(np_nm),sp_nm(np_nm),pl_nm(np_nm),pu_nm(np_nm),cf_nm(np_nm))	
	do ii=1,Ndata
	  x_nm(ii)=xfit(ii)
	  y_nm(ii)=yfit(ii)
	  sy_nm(ii)=err2(ii+id1-1)
	  if(ifbgd.eq.1)then
	    y_nm(ii)=y_nm(ii)-bg0-bg1*(float(ii+id1-n0bgd)-0.5)
	    sy_nm(ii)=sy_nm(ii)+e2bg0+e2bg1*(float(ii+id1-n0bgd)-0.5)**2
	   else
	    y_nm(ii)=y_nm(ii)-spek(id1)-(spek(id2)-spek(id1))/float(Ndata-1)*(ii-1)
	  endif
	  sy_nm(ii)=sqrt(sy_nm(ii))
	enddo
****	


	if(ifbgd.eq.1) then
	   y1=bg0+bg1*(id1-n0bgd)
	   y2=bg0+bg1*(id2-n0bgd)
	   ifreepar(1)=0
	   ifreepar(2)=0
	else
	   y1=spek(id1)
	   y2=spek(id2)
	   ifreepar(1)=1
	   ifreepar(2)=2
	endif
	fitpar(1)=y1
	fitpar(2)=(y2-y1)/(id2-id1)
	if(ibgord.eq.1) then
	   fitpar(1)=(y1+y2)/2
	   fitpar(2)=0
	   ifreepar(2)=0
	endif

	fitpar(3) = 1.
	ifreepar(3)=3
	Nfitpar=3

	do i=1,Nmgau
	   xi=xmgau(i)
	   ix=xi
	   if( ix.lt.id1 .or. ix.gt.id2 ) then
		kv=2
		goto 99
	   endif
	   xr=xi-N0GAU
	   Nfitpar=Nfitpar+1
	   fitpar(Nfitpar)=xr
	   ifreepar(Nfitpar)=Nfitpar
	   Nfitpar=Nfitpar+1
	   fitpar(Nfitpar)=spek(ix)- ( fitpar(1)+fitpar(2)*xr )
	   ifreepar(Nfitpar)=Nfitpar
	end do

	Nfreepar=0
	do ii=1,Nfitpar
	   if(ifreepar(ii).gt.0) Nfreepar=Nfreepar+1
	end do
	if (Ndata.le.Nfreepar) then
	   kv=3
	   goto 99
	endif

c	write(6,*) '       F_lambda       Chi_square   d_Chi_square%'
c	write(6,*) '------------------------------------------------'
c	write(6,*) ' '

	call Curfit(fitpar,Nfitpar,Ndata,GFDER,GFCHI,kv,0,chichi)

****	[Refit] param. init. & fit    N.M.

	p_nm(1)=fitpar(3)
	do ii=1,nmgau
	  p_nm(2*ii)=fitpar(2*ii+2)
	  p_nm(2*ii+1)=fitpar(2*ii+3)
	enddo
	do ii=1,np_nm
	  sp_nm(ii)=abs(p_nm(ii))/100
	  pl_nm(ii)=p_nm(ii)/20
	  pu_nm(ii)=p_nm(ii)*15
	  cf_nm(ii)=0
*	  p_nm(ii)=1.05*p_nm(ii)
	enddo
	chq_nm=1e-6
	mc_nm=0
	it_nm=0
	niter_nm=1000
	call chqfit(Ndata,x_nm,y_nm,sy_nm,np_nm,p_nm,sp_nm,pl_nm,pu_nm,cf_nm,niter_nm,
     &		    chq_nm,mc_nm,it_nm,pk_funct)
	if(mc_nm.eq.1)then
	  fitpar(3)=p_nm(1)
	  sfitpar(3)=sp_nm(1)
	  chq_nm=sqrt(chq_nm/np_nm)
	  if( chq_nm .le. 1.000 )chq_nm=1.000
	  do ii=1,nmgau
	    fitpar(2*ii+2)=p_nm(2*ii)
	    fitpar(2*ii+3)=p_nm(2*ii+1)
	    sfitpar(2*ii+2)=sp_nm(2*ii)  !*chq_nm
	    sfitpar(2*ii+3)=sp_nm(2*ii+1)!*chq_nm
	  enddo
	else
	  do ii=1,Nfitpar
	    sfitpar(ii)=-1
	  enddo
	endif

	if( varwidth .and. (nmgau .gt. 1) )then
	   do ii = 0, nmgau-1
	      p_nm(2*ii+1) = fitpar(2*ii+5) 
	      pl_nm(2*ii+1) = 0.0000
	      pu_nm(2*ii+1) = 3.00*fitpar(2*ii+5)
	      p_nm(2*ii+2)  = fitpar(3)
	      pl_nm(2*ii+2)  = fitpar(3)/1.50
	      pu_nm(2*ii+2)  = fitpar(3)*1.50
	      sp_nm(2*ii+1) = p_nm(3*ii+2)/100.00
	      sp_nm(2*ii+2) = p_nm(3*ii+3)/10.0000
	   enddo
	   chq_nm=1e-6
	   mc_nm=0
	   it_nm=0
	   niter_nm=1000
	   np_nm = 2*nmgau
	   do ii = 1, np_nm
	     cf_nm(ii)=0
	   enddo
	   call chqfit(Ndata,x_nm,y_nm,sy_nm,np_nm,p_nm,sp_nm,pl_nm,pu_nm,cf_nm,niter_nm,
     &		    chq_nm,mc_nm,it_nm,vw_funct)
	    

	   Nfitpar = 3 + 3*nmgau
	   if(mc_nm.eq.1)then
	     chq_nm=sqrt(chq_nm/np_nm)
	     if( chq_nm .le. 1.000 )chq_nm=1.000
	     iwoff = 2*nmgau+3
	     do ii=1,nmgau
	       fitpar(2*ii+3)=p_nm(2*ii-1)
	       fitpar(ii+iwoff) = p_nm(2*ii)
	       sfitpar(2*ii+3)=sp_nm(2*ii-1)!*chq_nm
	       sfitpar(ii+iwoff) = sp_nm(2*ii)
	     enddo
	   else
	     do ii=1,Nfitpar
	       sfitpar(ii)=-1
	     enddo
	   endif
	endif

*	deallocate(x_nm,y_nm,sy_nm)
*	deallocate(p_nm,sp_nm,pl_nm,pu_nm,cf_nm)

99	If (kv.ne.0 .and. mc_nm .ne. 1) then
	   If (kv.lt.1.or.kv.gt.5) kv= 6
	   if( mc_nm .ne. 1 )kv = 7
	   WRITE(6,*) 'Fit error: '//Msg(kv)
	   IFGAU=0
	else
	   IFGAU=1
	   do ii=1,Nfitpar
		Gfpar(ii)=fitpar(ii)
	   end do
	   do ii=1,nmgau
	     GFPAR(2*ii+2)=GFPAR(2*ii+2)+0.5
	   end do
	   IF(ISHOW.EQ.1) CALL SHOWGAUSS(llunout)
	Endif

	RETURN

	END


	SUBROUTINE SHOWgauss(llunout)

#include "trackn.inc"
	character*80 string
	real*4 DFGFFUN,ch1,ch2
	EXTERNAL DFGFFUN
	integer*4 ii

	IF(IFGAU.NE.1) RETURN

*	CALL DISP_SETSCALE(DWIN,TWIN,XSCA,YSCA)

	CH1=XMREG(1)
	CH2=XMREG(1)
	DO II=1,NMREG
	   CH=XMREG(II) !+0.5
	   CH1=MIN(CH1,CH)
	   CH2=MAX(CH2,CH)
	end do
	CALL xtpoverlay(DFGFFUN,CH1,CH2,0.10000)
	write(6,'(''Peak#   Channel    Energy          Area                       Width'')')

	do ii=1,nmgau
	  gposi=GFPAR(2*ii+2)+N0GAU
	  call xtpshownumpeak(gposi,ii,3)
	  sigma=GFPAR(3)
	  garea=GFPAR(2*ii+3)*sqrt(2*3.141593*sigma**2)
***** N.M.
	   sgarea=(sfitpar(2*ii+3)*sigma)**2+(gfpar(2*ii+3)*sfitpar(3))**2
	   sgarea=sqrt(2*3.141593*sgarea)
	   sgposi=sfitpar(2*ii+2)/2.000
*****
	  genergy = cpol(gPOSI,ECAL,-m$k)
	  sgenergy = (cpol(gPOSI+sgposi,ECAL,-m$k)-cpol(gPOSI-sgposi,ECAL,-m$k))
	  if( efficor )then
	    rrr = effvalue(genergy)
	    garea = garea/rrr
	    sgarea = sgarea/rrr
	 endif
	  k_se = nint(sgenergy*100.00)
	  if( k_se .ge. 0 .and. k_se .lt. 10 )then
	    write(string,'(''('',i1,'')      '')')k_se
	  elseif( k_se .ge. 10 .and. k_se .lt. 100 )then
	    write(string,'(''('',i2,'')     '')')k_se
	  elseif( k_se .ge. 100 .and. k_se .lt. 1000 )then 
	    write(string,'(''('',i3,'')    '')')k_se
	  elseif( k_se .ge. 1000 .and. k_se .lt. 10000 )then
	    write(string,'(''('',i4,'')   '')')k_se
	  else
	    write(string,'(''(err)         '')')
	  endif
	  
	  if( VarWidth .and. (nmgau.gt.1) ) then
	  	gfwhm=GFPAR(3+2*nmgau+ii)*2.35482
	  else
	  	gfwhm=GFPAR(3)*2.35482
	  endif

	  IF(ABS(gAREA).LT.9.9E7) THEN
	    WRITE(string(10:),'(i10,''('',i10,'')'')')nint(gAREA),nint(sgarea)
          ELSE
	    WRITE(string(10:),'(1PE13.6,''('',e13.6,'')'')') gAREA,sgarea
	  ENDIF
	  call string_strip(string(10:))
	  iss=lengthc(string(10:))+9
	  ttt=abs(cpol(gPOSI+gFWHM/2,ECAL,-m$k)-cpol(gPOSI-gFWHM/2,ECAL,-m$k))
	  do jjj = iss+1,36
	  	string(jjj:jjj) = ' '
	  enddo
	  iss = 36
	  write(6,'(i3,2x,2f10.2,a,f7.3)')ii,gposi,genergy,string(1:iss),ttt

	  if(llunout.gt.0 .and. llunout.ne.6) then
	    IF(ABS(gAREA).LT.9.9E8) THEN
	      WRITE(lLUNOUT,'(1X,A,2F10.3,f8.3,F13.2,f13.2,2F9.3,i3)')
	1	FNAMEI(1:20),gPOSI,genergy,sgenergy,gAREA,sgarea,gFWHM,ttt,ii
	    ELSE
	      WRITE(lLUNOUT,'(1X,A,2F10.3,f8.3,1PE13.6,e13.6,2F9.3,i3)')
	1	FNAMEI(1:20),gPOSI,genergy,sgenergy,gAREA,sgarea,gFWHM,ttt,ii
	    ENDIF
	    call flush(llunout)
	  endif
	end do
	if(iecal.eq.0) then
	  rrr=gFWHM
	else
	  rrr=abs(cpol(gPOSI+gFWHM/2,ECAL,-m$k)-cpol(gPOSI-gFWHM/2,ECAL,-m$k))
	endif
*	write(6,'(''Common FWHM : '',f10.2)')rrr

	RETURN

	END

	real*4 function DFGFFUN(XII)

#include "trackn.inc"
	real*4 XII
	real*8 sigma, xr, yy, z, z2

	xr=xii-n0gau
	YY=GFPAR(1)+GFPAR(2)*xr
	iwoff = 2*nmgau+3
	Do J= 1, NMGAU
	   if( VarWidth .and. (nmgau .gt. 1) )then
	   	   sigma = gfpar(J+iwoff)
	   else
	   	   sigma=GFPAR(3)
	   endif
	   z=(xr-GFPAR(2*J+2))/sigma
	   z2=z*z/2.
	   YY=YY+GFPAR(2*J+3)*dexp(-z2)
	end do
	dfgffun=yy
*	DFGFFUN=DIFUNCT(yy,ifunct)

	RETURN

	END

	function GFFUN(xi,par,npar)

	real xi,par(30)
	real*8 zz, z2

	GFFUN=par(2)*xi+par(1)
        sigma=par(3)
	do ii=4,npar,2
         pos=par(ii)
	 hig=par(ii+1)
         zz=(xi-pos)/sigma
         z2=zz*zz/2.
         if (z2.lt.25.) then
            GFFUN=GFFUN+hig*dexp(-z2)
         endif
	end do

	return

	end

	function GFDER(id,Par,Npar,Deriv,dyi,wi)

#include "trackn.inc"
	
	real Par(30),Deriv(30)
	real*8 z,z2

	xi=xfit(id)
	dyi=yfit(id)-GFFUN(xi,par,npar)
	wi=dyfit(id)

	deriv(1)= 1
	deriv(2)=xi
	deriv(3)=0
	sigma=par(3)
	do i=4,npar,2
	   pos=xi-par(i)
	   hig=par(i+1)
	   z=pos/sigma
	   z2=z*z/2.
	   if (z2.lt.25.) then
		eexx=dexp(-z2)
		xeexx=z/sigma*hig*eexx
		deriv(i  )=xeexx			! for x:
		deriv(i+1)=eexx				! for h:
		deriv(3)=deriv(3)+xeexx*z		! for sigma
	   else
		deriv(i+1)=0
		deriv(i+2)=0
	   endif
	end do

	do i=1,npar
	   if(ifreepar(i).le.0) deriv(i)=0.
	end do

	GFDER=0

	return

	end

	function GFCHI(Par,Npar)

#include "trackn.inc"

	dimension par(30)

	chisqr=0
	do ii=1,ndata
	   chisqr=chisqr+dyfit(ii)*(yfit(ii)-GFFUN(xfit(ii),Par,Npar))**2
	end do
	chisqr=chisqr/(max(1,ndata-nfreepar))
	GFCHI=chisqr

	return

	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	SUBROUTINE TRACKFITDEF(iask)

#include "trackn.inc"

	external       pFDER,pFCHI

	dimension ecaltmp(m$k+1),wcaltmp(m$k+1)

	logical*1 svcoefs	/.true./
	logical*1 wrcoefs	/.false./
	logical*1 empty
*	logical*1 INP_YES
	character*6 nm_char
	integer*4 ncolsreg,nrowsreg
	
	EMPTY=.TRUE.
	II=2
	DOWHILE (EMPTY .AND. (II.LT.IKL))
	  IF(SPEK(II).GT.0) EMPTY=.FALSE.
	  II=II+1
	end do
	IF(EMPTY) THEN
	   CALL xtpbell
	   RETURN
	ENDIF

	call xtptermfocus
	
	if(iask.eq.1) then
	   CALL INPUTRENE
	endif
	IF(NRENE.LT.2) THEN
	   CALL xtpbell
	   RETURN
	ENDIF

	IF(IECAL.NE.1) THEN
	   WRITE(6,*)
	1	 'To perform this command you should set the Energy calibration'
	   WRITE(6,*)
	   CALL ECALIBRATION(0)
	   IF(IECAL.NE.1) RETURN
	ENDIF

	IF(IWCAL.NE.1) THEN
	   WRITE(6,*)
	1	 'To perform this command you should set the FWHM calibration'
	   WRITE(6,*)
	   CALL WCALIBRATION
	   IF(IWCAL.NE.1) RETURN
	ENDIF

	if(iask.eq.1) then
	  if(fwfact.le.0) fwfact=2.5
	  fwfact=fwfact*2
	  call inp_r1('Width of the fit region (in FWHM units) ',fwfact)
	  fwfact=fwfact/2
	endif
	if(fwfact.le.0) fwfact=2.5

	ireord=m$k-1
	do while (ireord.gt.1 .and. ecal(ireord+1).eq.0.)
	  ireord=ireord-1
	end do
	IREORD=MIN(IREORD,NRENE-1,m$k-1)

	irword=m$k-1
	do while (irword.gt.1 .and. wcal(irword+1).eq.0.)
	  irword=irword-1
	end do
 	IRWORD=MIN(IRWORD,NRENE-1,m$k-1)

	ifit=0
	shft0=0.
	shft1=1.
	if(iask.eq.1) then
	  call inp_r1('Gain factor for the energy calibration',shft1)
	  if(shft1.eq.0.) shft1=1.
	  call inp_ask('Consider SQRT term in energy calibration',sqrterm)
	endif

10	chichi=0
	call boundfit(ifit,shft0,shft1,chichi)	! fitta i picchi 

	nrowsreg=sqrt(float(ntrreg))
	ncolsreg=nrowsreg
	if(ncolsreg*nrowsreg .lt. ntrreg)ncolsreg=nrowsreg+1
	if(ncolsreg*nrowsreg .lt. ntrreg)nrowsreg=nrowsreg+1
	call xtpmaptmp(nrowsreg,ncolsreg)
	DO ii=1,ntrreg				! mostra il fit legato
	   call put_comment
	   call dispwreg(ii)
	   call showgauss1(ii)
	   call xtpnext
	end do
	DO ii= ntrreg+1, nrowsreg*ncolsreg
	   call xtplotreset
	   call xtpnext
	end do


	yyt=0			! ricava la calibrazione energetica
	do ii=1,NRENE		! temporanea per poter dare la FWHM in keV
	   aa=trpar(5*ii  )
	   pp=trpar(5*ii-1)+0.5	! al centro del canale
	   s2=trpar(5*ii-2)
	   ar=aa*sqrt(6.283185*s2)
	   ar=max(1.,ar)
	   xfit(ii)=pp
	   dxfit(ii)=sqrt(s2/ar)
	   yfit(ii)=RENE(ii)
	   dyfit(ii)=drene(ii)
	   yyt=yyt+yfit(ii)
	end do
	do ii=1,m$k+1
	   ecaltmp(ii)=0
	end do
	ecaltmp(1)=yyt/NRENE
	ncdata=nrene
	ncpar=ireord+1

	if(sqrterm)then
	  ncpar=ncpar+1
	  ncpar1=1-ncpar
	else
	  ncpar1=ncpar
	endif

	do ii=1,ncpar
	   ifreepar(ii)=ii
	end do
	nfreepar=ncpar
	call Curfit(ecaltmp,ncpar1,Ncdata,pFDER,pFCHI,kv,0,cchichi)

	do ii=1,NRENE				! risultati del boundfit
	   iipar=5*(ii-1)
	   bb=trpar(iipar+1)
	   s2=trpar(iipar+3)
	   pp=trpar(iipar+4)+0.5	! al centro del canale
	   aa=trpar(iipar+5)
	   ar=aa*sqrt(2*3.141593*s2)
	   fw=sqrt(s2)*2.35482
	   efw=cpol(pp+fw/2,ecaltmp,ncpar1)-cpol(pp-fw/2,ecaltmp,ncpar1)
	   if1=0
	   do inr=1,ntrreg
		if(ipack1(inr).eq.ii) if1=inr
	   end do
	   if(if1.eq.0) then
		write(6,'(i3,f9.2,1H(,f9.2,1H),g,f6.2,1H(,f6.2,1H))')
	1		 ii,pp,rene(ii),ar,fw,efw
	   else
		write(6,'(i3,f9.2,1H(,f9.2,1H),g,f6.2,1H(,f6.2,1H),f7.1)')
	1		 ii,pp,rene(ii),ar,fw,efw,min(trchichi(if1),9999.)
	   endif
	end do
	WRITE(6,'(1X,A,''    Chisquare='',F8.2)')
	1	 FNAMEI(1:LFNAMEI),chichi

	if(ifit.eq.0) then
	   JREORD=IREORD
	   JRWORD=IRWORD
	   ii=INP_I2('Give order of polynomials (-1 => no fit)',
	1		JREORD,JRWORD)
	   if(ii.lt.0) then
	      break=.true.
	      goto 200
	   endif
	   IF(JREORD.LT.1) GOTO 200
	   IF(JRWORD.LT.0) GOTO 200
	   IREORD=MIN(JREORD,NRENE-1,m$k-1)
	   IRWORD=MIN(JRWORD,NRENE-1,m$k-1)
	   IFIT=1
	   GOTO 10
	endif

	yyt=0
	do ii=1,NRENE
	   aa=trpar(5*ii  )
	   pp=trpar(5*ii-1)+0.5	! al centro del canale
	   s2=trpar(5*ii-2)
	   ar=aa*sqrt(6.283185*s2)
	   xfit(ii)=pp
	   dxfit(ii)=sqrt(s2/ar)
	   yfit(ii)=RENE(ii)
	   dyfit(ii)=drene(ii)
	   yyt=yyt+yfit(ii)
	end do
	do ii=1,m$k+1
	   ecaltmp(ii)=0
	end do
	ecaltmp(1)=yyt/NRENE
	ndata=nrene
	npar=ireord+1

	if(sqrterm)then
	  npar=npar+1
	  npar1=1-npar
	else
	  npar1=npar
	endif


	do ii=1,npar
	   ifreepar(ii)=ii
	end do
	nfreepar=npar
	call Curfit(ecaltmp,npar1,Ndata,pFDER,pFCHI,kv,0,chichi)

	yyt=0
	do ii=1,NRENE
	   aa=trpar(5*ii  )
	   pp=trpar(5*ii-1)+0.5	! al centro del canale
	   s2=trpar(5*ii-2)
	   ar=aa*sqrt(6.283185*s2)
	   xfit(ii)=pp
	   dxfit(ii)=sqrt(s2/ar)
	   yfit(ii)=sqrt(s2)*2.354820
	   dyfit(ii)=1.
	   yyt=yyt+yfit(ii)
	end do
	do ii=1,m$k
	   wcaltmp(ii)=0
	end do
	wcaltmp(1)=yyt/NRENE
	ndata=nrene
	npar=irword+1
	do ii=1,npar
	   ifreepar(ii)=ii
	end do
	nfreepar=npar
	call Curfit(wcaltmp,npar,Ndata,pFDER,pFCHI,kv,0,chichi)

	write(6,'('' Order of E_cal polynomial is'',i3)')IREORD
	write(nm_char,'(i6)')ireord
	WRITE(6,'(F9.3,F10.6,'//nm_char//'G)') (ecaltmp(LL),LL=1,IREORD+1)
	if(sqrterm)write(6,'(''  SQRT term : '',G)')ecaltmp(ireord+2)
	write(6,'('' Order of W_cal polynomial is'',i3)')IRWORD
	write(nm_char,'(i6)')irword
	WRITE(6,'(F9.3,F10.6,'//nm_char//'G)') (wcaltmp(LL),LL=1,IRWORD+1)

	IF(.NOT.INP_YES('OK')) THEN
	      IFIT=0
	      GOTO 10
	ENDIF	

	if(iask.eq.1) then
	   call INP_ASK
	1 ('Save coefficients as Energy & Width calibration',svcoefs)
	endif
	if(svcoefs) then
	   do ii=1,m$k+1
		ecal(ii)=0
	   end do
	   DO II=1,IREORD+1
		ECAL(II)=ecaltmp(II)
	   end do
	   if(sqrterm)ecal(m$k+1)=ecaltmp(ireord+2)
	   IRESAV=1
	   do ii=1,m$k+1
		Wcal(ii)=0
	   end do
	   DO II=1,IRWORD+1
		WCAL(II)=Wcaltmp(II)
	   end do
	   irwsav=1
	ELSE
	   IRESAV=0
	   irwsav=0
	ENDIF

	if(iask.eq.1) then
	   call inp_ask('Want to save coefficients on FOR008',wrcoefs)
	endif
	if(wrcoefs) then
	  ldir=index(FNAMEI,']')
	  ldot=index(FNAMEI(ldir+1:),'.')
	  if(ldot.gt.0) then
	     ldot=ldot+ldir
	  else
	     ldot=lfnamei+1
	  endif
	  call str_toint(FNAMEI(1:ldot-1),-10,ii,nposadc)
	  if(nposadc.gt.0) iadccc=ii
	  call str_toint(FNAMEI(ldot+1:) , 10,ii,npostap)
	  if(npostap.gt.0) itappp=ii
	  if(iask.eq.1 .or. nposadc.lt.1 .or. npostap.lt.1) then
	    idmmy=inp_i2('ADC , Tape',IADCCC,itappp)
	  endif
	  IREORD1=MAX(1,IREORD-1)
	  if(sqrterm)then
	    write(nm_char,'(i6)')ireord1+1
	    WRITE(8,'(I5,I5,I3,F9.3,F10.6,'//nm_char//'G14.6)')
	1   ITAPpp,IADCcc,-(IREORD+2),(ecaltmp(LL),LL=1,IREORD+1),
	2   ecaltmp(IREORD+2)
	  else
	    write(nm_char,'(i6)')ireord1
	    WRITE(8,'(I5,I5,I3,F9.3,F10.6,'//nm_char//'G14.6)')
	1   ITAPpp,IADCcc,IREORD+1,(ecaltmp(LL),LL=1,IREORD+1)
	  endif
	endif

200	NMIN=IKA
	NMAX=IKL
	CALL FINDYLIMITS

	RETURN

	END

	SUBROUTINE TRACKFIT_2G

#include "trackn.inc"

	external       pFDER,pFCHI

	dimension ecaltmp(2),wcaltmp(2)
	logical*1 empty
*	logical*1 INP_YES
	character*6 nm_char

	EMPTY=.TRUE.
	II=2
	DO WHILE (EMPTY .AND. (II.LT.IKL))
	  IF(SPEK(II).GT.0) EMPTY=.FALSE.
	  II=II+1
	end do
	IF(EMPTY) THEN
	   CALL xtpbell
	   RETURN
	ENDIF

	nrene=2
	rene(1)=1173.238
	rene(2)=1332.513
	drene(1)=0.015
	drene(2)=0.018
	a1172=0
	a1332=0
	CALL autoECALIBRATION(0)
	IF(IECAL.NE.1) THEN
	   WRITE(6,*)
	1	 'Failed to identify the two peaks'
	   WRITE(6,*)
	   goto 100
	ENDIF
	IF(IWCAL.NE.1) THEN
	   call xtptermfocus
	   WRITE(6,*)
	1	 'To perform this command you should set the FWHM calibration'
	   WRITE(6,*)
	   CALL WCALIBRATION
	   IF(IWCAL.NE.1) RETURN
	ENDIF

	ireord=1
 	irword=1
	ncpar=2

	if(fwfact.le.2) fwfact=3.

	ifit=1
	shft0=0.
	shft1=1.

10	chichi=0
	call boundfit(ifit,shft0,shft1,chichi)	! fitta i picchi 

	nrowsreg=sqrt(float(ntrreg))
	ncolsreg=nrowsreg
	if(ncolsreg*nrowsreg .lt. ntrreg)ncolsreg=nrowsreg+1
	if(ncolsreg*nrowsreg .lt. ntrreg)nrowsreg=nrowsreg+1
	call xtpmaptmp(nrowsreg,ncolsreg)
	DO ii=1,ntrreg				! mostra il fit legato
	   call dispwreg(ii)
	   call showgauss1(ii)
	   call xtpnext
	end do


	p1172=0
	p1332=0
	a1172=0
	a1332=0
	w1172=0
	w1332=0
	ppmax=0
	do ii=1,NRENE				! le calibrazioni
	   iipar=5*(ii-1)
	   bb=trpar(iipar+1)
	   s2=trpar(iipar+3)
	   pp=trpar(iipar+4)
	   p1172=p1332
	   p1332=pp
	   aa=trpar(iipar+5)
	   ar=aa*sqrt(2*3.141593*s2)
	   a1172=a1332
	   a1332=+ar
	   fw=sqrt(s2)*2.35482
	   ppmax=max(ppmax,pp+4*fw)
	   w1172=w1332
	   w1332=fw
	end do
	ecaltmp(2)=(rene(2)-rene(1))/(p1332-p1172)
	ecaltmp(1)=rene(2)-ecaltmp(2)*p1332
	wcaltmp(2)=(w1332-w1172)/(p1332-p1172)
	wcaltmp(1)=w1332-wcaltmp(2)*p1332

	do ii=1,NRENE				! risultati del boundfit
	   iipar=5*(ii-1)
	   bb=trpar(iipar+1)
	   s2=trpar(iipar+3)
	   pp=trpar(iipar+4)
	   aa=trpar(iipar+5)
	   ar=aa*sqrt(2*3.141593*s2)
	   fw=sqrt(s2)*2.35482
	   efw=cpol(pp+fw/2,ecaltmp,ncpar)-cpol(pp-fw/2,ecaltmp,ncpar)
	   if1=0
	   do inr=1,ntrreg
		if(ipack1(inr).eq.ii) if1=inr
	   end do
	   if(if1.eq.0) then
		write(6,'(i3,f9.2,1H(,f9.2,1H),g,f6.2,1H(,f6.2,1H))')
	1		 ii,pp,rene(ii),ar,fw,efw
	   else
		write(6,'(i3,f9.2,1H(,f9.2,1H),g,f6.2,1H(,f6.2,1H),f7.1)')
	1		 ii,pp,rene(ii),ar,fw,efw,min(trchichi(if1),9999.)
	   endif
	end do
	WRITE(6,'(1X,A,''    Chisquare='',F8.2)')
	1	 FNAMEI(1:LFNAMEI),chichi
	write(6,'('' Order of E_cal polynomial is'',i3)')IREORD
	write(nm_char,'(i6)')ireord
	WRITE(6,'(F9.3,F10.6,'//nm_char//'G)') (ecaltmp(LL),LL=1,IREORD+1)
	write(6,'('' Order of W_cal polynomial is'',i3)')IRWORD
	write(nm_char,'(i6)')irword
	WRITE(6,'(F9.3,F10.6,'//nm_char//'G)') (wcaltmp(LL),LL=1,IRWORD+1)

	artot=a1172+a1332
	if(artot.le.0) goto 100

	ii2=ppmax
	ii2=min(ii2,ikl-1)
	ii1=cpolinv(20.,ecaltmp,ncpar)
	ii1=max(20,ii1)
	a020=0.
	do ii=ii1,ii2
	   a020=a020+spek(ii)
	end do
	if(a020.gt.1) then
	   a020=artot/a020
	   write(6,'('' P/T( 20)='',f10.3)') a020
	else
	   a020=0
	endif
	ii1=cpolinv(100.,ecaltmp,ncpar)
	ii1=max(20,ii1)
	a100=0.
	do ii=ii1,ii2
	   a100=a100+spek(ii)
	end do
	if(a100.gt.1) then
	   a100=artot/a100
	   write(6,'('' P/T(100)='',f10.3)') a100
	else
	   a100=0
	endif
	ii1=cpolinv(200.,ecaltmp,ncpar)
	ii1=max(20,ii1)
	a200=0.
	do ii=ii1,ii2
	   a200=a200+spek(ii)
	end do
	if(a200.gt.1) then
	   a200=artot/a200
	   write(6,'('' P/T(200)='',f10.3)') a200
	else
	   a200=0
	endif
	ii1=cpolinv(300.,ecaltmp,ncpar)
	ii1=max(20,ii1)
	a300=0.
	do ii=ii1,ii2
	   a300=a300+spek(ii)
	end do
	if(a300.gt.1) then
	   a300=artot/a300
	   write(6,'('' P/T(300)='',f10.3)') a300
	else
	   a300=0
	endif

	NMIN=IKA
	NMAX=IKL
	CALL FINDYLIMITS

	call xtptermfocus
	IF(.not.INP_YES('OK')) goto 100

	do ii=1,m$k+1
	   ecal(ii)=0
	end do
	DO II=1,IREORD+1
	   ECAL(II)=ecaltmp(II)
	end do
	IRESAV=1
	do ii=1,m$k+1
	   Wcal(ii)=0
	end do
	DO II=1,IRWORD+1
	   WCAL(II)=Wcaltmp(II)
	end do
	irwsav=1

	CALL xtplotnew
	ew1332=ecal(2)*w1332
	WRITE(6,'(1x,a,f8.1,f10.0,f6.2,4f7.3)')
	1	FNAMEI(1:lfnamei),p1332,a1332,ew1332,a020,a100,a200,a300
	if(lunout.gt.0 .and. lunout.ne.6)
	1      WRITE(lunout,'(1x,a,f8.1,f10.0,f6.2,4f7.3)')
	1	FNAMEI(1:lfnamei),p1332,a1332,ew1332,a020,a100,a200,a300
	RETURN

100	if(lunout.gt.0 .and. lunout.ne.6) 
	1      WRITE(lunout,'(1x,a)') FNAMEI(1:lfnamei)
	call xtpbell
	NMIN=IKA
	NMAX=IKL
	CALL FINDYLIMITS
	RETURN

	END

	SUBROUTINE TRACKFIT

#include "trackn.inc"

	CHARACTER*30 FNcoefs,FNresult
	character*60 fnamei0
	character*1 loccom
	character*80 strask
	dimension ecaltmp(m$k+1)
	dimension Tmpspek(0:M$CH-1)

	logical*1 Wrresult	/.false./
	logical*1 Wrecoefs	/.true./
	logical*1 readtapnum	/.true./
	logical*1 readadcnum	/.true./
	logical*1 writadcnum	/.true./
	logical*1 listres		/.true./
	logical*1 prechange	/.false./
	logical	waitcom		/.false./
	logical*1 empty
	logical*1 logtemp

	character*6 nm_char

	external       pFDER,pFCHI

	if(NRENE.LT.2 .OR. IECAL.NE.1 .OR. IWCAL.NE.1) THEN
	   CALL xtpbell
	   RETURN
	ENDIF

	NTAPE=0
	call xtptermfocus

33	lnch=INP_str('File with the list of spectra',SPECLIST)
	if(lnch.eq.-1) goto 44
	if(speclist(1:1).eq.'=' .or.
	1  speclist(1:1).eq.'+' .or.
	1  speclist(1:1).eq.'-')then
	  LISTLUN=0
	else
	  LISTLUN=4
	  OPEN(UNIT=LISTLUN,FILE=SPECLIST,FORM='FORMATTED',STATUS='OLD',ERR=200)
	endif

	lnch=inp_ask('Want to read TAPE# from spectrum name',readtapnum)
	if(lnch.eq.-1) goto 44
	if(readtapnum) then
	else
	  call inp_i1('Tape #',ITAP)
	  if(itap.lt.0) goto 200
	endif

	lnch=inp_ask('Want to read  ADC# from spectrum name',readadcnum)
	if(lnch.eq.-1) goto 44
	if(readadcnum) then
	   writadcnum=.false.
	   ldir=index(FNAMEI,']')
	   ldot=index(FNAMEI(ldir+1:),'.')
	   if(ldot.gt.0) then
	     ldot=ldot+ldir
	   else
	     ldot=lfnamei+1
	   endif
	   call str_toint(FNAMEI(1:ldot-1),-10,IADC,nposadc)
	   jadc=-1
	else
	   write(strask,'(''ADC number ['',i3.3,''] '')')iadc
	   lstrask=lengthc(strask)
	   call inp_i1(strask(:lstrask),IADC)
	   if(lnch.eq.-1) goto 44
!	   call inp_ask('Want to write ADC# into spectrum name',writadcnum)	
	   if(writadcnum) then
	     jadc=iadc
	   else
	     jadc=-1
	   endif
	endif
	
	lnch=inp_ask('List Fit results on terminal',Listres)
	if(lnch.eq.-1) goto 44

	lnch=inp_ask('Write out Fit results',Wrresult)
	if(lnch.eq.-1) goto 44
	if(Wrresult) then
	   write(FNresult,'(''FIT'',I3.3,''.DAT'')',err=33) IADC
	   lfnresult=max(1,lengthc(fnresult))
	   write(strask,'(''Filename [ '',A,'' ] '')') fnresult(:lfnresult)
	   lstrask=lengthc(strask)
	   lnch=INP_CH(strask(:lstrask),FNresult)
c	   if(lnch.eq.-1) goto 44
	   lfnresult=max(1,lengthc(fnresult))
	   if(lfnresult.le.0) goto 200
	   if(lunoutr.eq.0) call lib$get_lun(lunoutr)
	   OPEN(UNIT=lunoutr,FILE=FNresult,FORM='FORMATTED',err=200,STATUS='unknown')
	endif

	lnch=inp_ask('Write out Energy Calibration coefficients',WRecoefs)
	if(lnch.eq.-1) goto 44
	if(WRecoefs) then
	   write(FNcoefs,'(''CAL'',I3.3,''.DAT'')',err=33) IADC
	   lfncoefs=max(1,lengthc(fncoefs))
	   write(strask,'(''Filename [ '',A,'' ] '')') fncoefs(:lfncoefs)
	   lstrask=lengthc(strask)
	   lnch=INP_CH(strask(:lstrask),FNcoefs)
c	   if(lnch.eq.-1) goto 44
	   lfncoefs=max(1,lengthc(fncoefs))
	   if(lfncoefs.le.0) goto 200
	   if(lunoutc.eq.0) call lib$get_lun(lunoutc)
	   OPEN(UNIT=lunoutc,FILE=FNcoefs,FORM='FORMATTED',ERR=200,STATUS='unknown')
	endif

	lnch=inp_ask('Want to use pre_fit of gain change',prechange)
	if(lnch.eq.-1) goto 44

	lnch=inp_ask('Wait for a command before continuing',waitcom)

44	logtemp=.true.
        lnch=inp_ask('OK to start',logtemp)
	if(lnch.eq.-1) return
	if(.not.logtemp) goto 33

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCC CICLA SUI FILES CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	fnamei0=fnamei
	CYCLE=.TRUE.
	BREAK=.FALSE.
	ifit=1

1	do ii=ika,ikl-1				! salva lo spettro precedente
	   Tmpspek(ii)=spek(ii)
	end do

	if(LISTLUN.EQ.0 .AND. NTAPE.EQ.0) then
	  continue		! se input tipo +=- usa anche il primo spettro
	else
	  CALL GETSPECTRUM(jadc)! legge il prossimo spettro
	  IF(BREAK) GOTO 200
	endif

	if(readadcnum) then
	   ldir=index(FNAMEI,']')
	   ldot=index(FNAMEI(ldir+1:),'.')
	   if(ldot.gt.0) then
	     ldot=ldot+ldir
	   else
	     ldot=lfnamei+1
	   endif
	   call str_toint(FNAMEI(1:ldot-1),-10,IADC,nposadc)
	endif

	EMPTY=.TRUE.
	II=2
	DO WHILE (EMPTY .AND. (II.LT.IKL))
	  IF(SPEK(II).GT.0) EMPTY=.FALSE.
	  II=II+1
	end do
	IF(EMPTY) THEN
	   CALL xtpbell
	   waitcom=.true.
	   shft0=0.
	   shft1=1.
	else
	   shft0=0.
	   shft1=1.
	   if(prechange) call gainfit(Tmpspek,spek,shft0,shft1,schichi)
	   if(abs(shft1-1.).gt.0.002) waitcom=.true.
	   if(abs(shft0)   .gt.2. ) waitcom=.true.
	endif

2	call boundfit(ifit,shft0,shft1,chichi)	! fitta i picchi 

	nrowsreg=sqrt(float(ntrreg))
	ncolsreg=nrowsreg
	if(ncolsreg*nrowsreg .lt. ntrreg)ncolsreg=nrowsreg+1
	if(ncolsreg*nrowsreg .lt. ntrreg)nrowsreg=nrowsreg+1
	call xtpmaptmp(nrowsreg,ncolsreg)
	DO ii=1,ntrreg				! mostra il fit legato
	   call put_comment
	   call dispwreg(ii)
	   call showgauss1(ii)
	   call xtpnext
	end do
	DO ii= ntrreg+1, nrowsreg*ncolsreg
	   call xtplotreset
	   call xtpnext
	end do

	NTAPE=NTAPE+1
	ldir=index(FNAMEI,']')
	ldot=index(FNAMEI(ldir+1:),'.')
	LEXT=ldot+ldir
	if(readtapnum)READ(FNAMEI(LEXT+1:LFNAMEI),*) ITAP

	call xtptermfocus

	WRITE(6,'(1X,A,I7,I5,''    Chisquare='',F8.2)')
	1	 FNAMEI(1:LFNAMEI),ITAP,IADC,chichi

	if(prechange) WRITE(6,'('' SF_chi='',f10.4,'' s0='',f8.3,'' s1='',f10.6)')
	1	 schichi,shft0,shft1

	yyt=0			! ricava la calibrazione energetica
	do ii=1,NRENE		! temporanea per poter dare la FWHM in keV
	   aa=trpar(5*ii  )
	   pp=trpar(5*ii-1)+0.5	! al centro del canale
	   s2=trpar(5*ii-2)
	   ar=aa*sqrt(6.283185*s2)
	   if(ar.le.0) ar=1.
	   xfit(ii)=pp
	   dxfit(ii)=sqrt(s2/ar)
	   yfit(ii)=RENE(ii)
	   dyfit(ii)=drene(ii)
	   yyt=yyt+yfit(ii)
	end do
	do ii=1,m$k
	   ecaltmp(ii)=0
	end do
	ecaltmp(1)=yyt/NRENE
	ncdata=nrene
	ncpar=ireord+1
	do ii=1,ncpar
	   ifreepar(ii)=ii
	end do
	nfreepar=ncpar
	call Curfit(ecaltmp,ncpar,Ndata,pFDER,pFCHI,kv,0,cchichi)

	do ii=1,NRENE				! risultati del boundfit
	   iipar=5*(ii-1)
	   bb=trpar(iipar+1)
	   s2=trpar(iipar+3)
	   pp=trpar(iipar+4)+0.5	! al centro del canale
	   aa=trpar(iipar+5)
	   ar=aa*sqrt(2*3.141593*s2)
	   fw=sqrt(s2)*2.35482
	   efw=cpol(pp+fw/2,ecaltmp,ncpar)-cpol(pp-fw/2,ecaltmp,ncpar)
	   if(listres) then
	     if1=0
	     do inr=1,ntrreg
		if(ipack1(inr).eq.ii) if1=inr
	     end do
	     if(if1.eq.0) then
		write(6,'(i3,f9.2,1H(,f9.2,1H),g,f6.2,1H(,f6.2,1H))')
	1		 ii,pp,rene(ii),ar,fw,efw
	     else
		write(6,'(i3,f9.2,1H(,f9.2,1H),g,f6.2,1H(,f6.2,1H),f7.1)')
	1		 ii,pp,rene(ii),ar,fw,efw,min(trchichi(if1),9999.)
	     endif
	   endif
	   POSTOT(NTAPE,Ii)=pp
	   if(ar.le.0) ar=1.
	   DPOSTOT(NTAPE,Ii)=fw/sqrt(ar)
	end do

	chiqftot(ntape)=chichi

	if(iresav.eq.1) then	! calibrazione energetica dal fit legato
	  yyt=0
	  do ii=1,NRENE
	    aa=trpar(5*ii  )
	    pp=trpar(5*ii-1)+0.5	! al centro del canale
	    s2=trpar(5*ii-2)
	    ar=aa*sqrt(6.283185*s2)
	    if(ar.le.0) ar=1.
	    xfit(ii)=pp
	    dxfit(ii)=sqrt(s2/ar)
	    yfit(ii)=RENE(ii)
	    dyfit(ii)=drene(ii)
	    yyt=yyt+yfit(ii)
	  end do
	  do ii=1,m$k
	    ecal(ii)=0
	  end do
	  ecal(1)=yyt/NRENE
	  ndata=nrene
	  npar=ireord+1
	  do ii=1,npar
	    ifreepar(ii)=ii
	  end do
	  nfreepar=npar
	  call Curfit(ecal,npar,Ndata,pFDER,pFCHI,kv,0,rchichi)
	endif

	if(irwsav.eq.1) then	! calibrazione fwhm dal fit legato
	  yyt=0
	  do ii=1,NRENE
	    aa=trpar(5*ii  )
	    pp=trpar(5*ii-1)+0.5	! al centro del canale
	    s2=trpar(5*ii-2)
	    ar=aa*sqrt(6.283185*s2)
	    xfit(ii)=pp
	    if(ar.le.0) ar=1.
	    dxfit(ii)=sqrt(s2/ar)
	    yfit(ii)=sqrt(s2)*2.354820
	    dyfit(ii)=1.
	    yyt=yyt+yfit(ii)
	  end do
	  do ii=1,m$k
	    wcal(ii)=0
	  end do
	  wcal(1)=yyt/NRENE
	  ndata=nrene
	  npar=irword+1
	  do ii=1,npar
	    ifreepar(ii)=ii
	  end do
	  nfreepar=npar
	  call Curfit(wcal,npar,Ndata,pFDER,pFCHI,kv,0,rchichi)
	endif

	write(nm_char,'(i6)')ireord
	WRITE(6,'('' ECAL'',F11.3,F10.6,'//nm_char//'G)') (ecal(LL),LL=1,IREORD+1)
	write(nm_char,'(i6)')irword
	WRITE(6,'('' WCAL'',F11.3,F10.6,'//nm_char//'G)') (wcal(LL),LL=1,IRWORD+1)

	if(waitcom) then
	  loccom=' '
	  idmmy_inp=INP_CH('<RET>, Dt, Go, Skip, Kal, Quit',loccom)
	  call str_toupper(loccom)
	  lloccom=lengthc(loccom)
	  if(lloccom.lt.0 .or. loccom.eq.'Q') then
	    NTAPE=NTAPE-1
	    goto 200
	  endif
	  if(loccom.eq.'D') then
	    call TRACKFITdef(1)
	    if(listlun.gt.0)backspace(LISTLUN)
	    NTAPE=NTAPE-1
	    goto 2
	  elseif(loccom.eq.'G') then
	    waitcom=.false.
	  elseif(loccom.eq.'S') then
	    NTAPE=NTAPE-1
	    goto 1
	  elseif(loccom.eq.'K') then
	    CALL ECALIBRATION(0)
	    CALL WCALIBRATION
	    if(listlun.gt.0)backspace(LISTLUN)
	    NTAPE=NTAPE-1
	    goto 2
	  endif
	else
	  CALL LIB$WAIT(0.5)
	endif

190	if(Wrresult) then
	 do ii=1,NRENE
	    aa=trpar(5*ii  )
	    pp=trpar(5*ii-1)+0.5	! al centro del canale
	    s2=trpar(5*ii-2)
	    fw=sqrt(s2)*2.354820
	    ar=aa*sqrt(6.283185*s2)
	    if(ii.eq.1) then	
		WRITE(lunoutr,'(1x,a,t20,i5,4g)')FNAMEI(1:LFNAMEI),
	1	ii,pp,cpol(pp,ecal,-m$k),ar,fw
	    else
		WRITE(lunoutr,'(1x  ,t20,i5,4g)')
	1	ii,pp,cpol(pp,ecal,-m$k),ar,fw
	    endif
	  end do
	endif

	IF(WRecoefs) then
	  IREORD1=MAX(1,IREORD-1)
	  write(nm_char,'(i6)')ireord1
	  WRITE(lunoutc,'(I5,I5,I3,F9.3,F10.6,'//nm_char//'G14.6,F8.2)')
	1   ITAP,IADC,IREORD+1,(ecal(LL),LL=1,IREORD+1)		!,CHICHI
	endif

	IF(.NOT.BREAK) GOTO 1

200	if(listlun.gt.0)CLOSE(LISTLUN)
	CALL xtpbell
	CYCLE=.false.

	IF(NTAPE.GT.0) THEN
*?	  call disp_tek_mode(xterm,tekmode)
*?	  CALL SHOWTOT(NTAPE,POSTOT,DPOSTOT,NRENE)
*?	  call DISP_alfa_home
	  WRITE(6,'(/1X,A,i8)') FNAMEI0(1:LFNAMEI)//'  ...  '//FNAMEI(1:LFNAMEI)
	  if(WRecoefs) WRITE(6,'(1X,A)') FNcoefs(1:lfncoefs)
	  if(WRresult) WRITE(6,'(1X,A)') FNresult(1:lfnresult)
	ENDIF

	IF(Wrresult .and. lunoutr.gt.0) then
	   CLOSE(lunoutr)
	   call lib$free_lun(lunoutr)
	   lunoutr=0
	endif
	IF(WRecoefs .and. lunoutc.gt.0) then
	   CLOSE(lunoutc)
	   call lib$free_lun(lunoutc)
	   lunoutc=0
	endif


	NMIN=IKA
	NMAX=IKL
	CALL FINDYLIMITS

	RETURN

	END

	SUBROUTINE gainfit(spekold,speknew,sf0,sf1,chichi)

#include "trackn.inc"

	dimension spekold(0:M$CH-1),speknew(0:M$CH-1)

	external       sFDER,sFCHI

	do ii=ika,ikl-1,NKAP	! Comprime i due spettri a 1024 canali
	   tmp1=0
	   tmp2=0
	   do jj=ii,ii+NKAP-1
		tmp1=tmp1+spekold(jj)
		tmp2=tmp2+speknew(jj)
	   end do
	   yfit(ii/NKAP) =tmp1		! questa sara' la funzione di fit
	   xfit(ii/NKAP) =tmp2		! e questi i dati da fittare
	end do
	xfit(0)=0
	xfit(1023)=0
	yfit(0)=0
	yfit(1023)=0

	call smooth(xfit,1024,2)	! smooth dello spettro seguente
	call smooth(yfit,1024,2)	! smooth dello spettro precedente

	totx=0				! aree dei due spettri
	toty=0
	do ii=0,1023
	   totx=totx+xfit(ii)
	   toty=toty+yfit(ii)
	end do
	facyfit=totx/toty
	facyfit2=facyfit*facyfit
	do ii=0,1023
	   yfit(ii)=yfit(ii)*facyfit
	   wifit=abs(xfit(ii))		!+facyfit2*abs(yfit(ii))
	   if(wifit.ne.0) then
	     dyfit(ii)=1./wifit
	   else
	     dyfit(ii)=1.
	   endif
	end do
	fitpar(1)=0.
	ifreepar(1)=1
	fitpar(2)=1.
	ifreepar(2)=2
	nfitpar=2
	nfreepar=2
	Ndata=0
	do ii=80/nkap,1024-160/nkap
	   Ndata=Ndata+1
	   ixfit(Ndata)=ii
	end do
	Ndata1=0
	Ndata2=1023
	call Curfit(fitpar,nfitpar,Ndata,sFDER,sFCHI,kv,0,chichi)

	!	write(6,'(1x,a,4g)') FNAMEI(1:lFNAMEI),chichi,
!	1			fitpar(1),fitpar(2),fitpar(3)

	sf0=-fitpar(1)/fitpar(2)*NKAP
	sf1=1./fitpar(2)

	return

	end

	function sFDER(id,Par,Npar,Deriv,dyi,wi)

#include "trackn.inc"

	dimension Par(100),Deriv(100)

	iid=ixfit(id)
	jd=par(1)+par(2)*iid
	if(jd.gt.Ndata1 .and. jd.lt.Ndata2) then
	   fyi=yfit(jd)
	   dydx=(yfit(jd+1)-yfit(jd-1))*0.5
	else
	   fyi=0
	   dydx=0
	endif

	dyi=xfit(iid)-fyi
	wi=dyfit(iid)

	deriv(1)=dydx
	deriv(2)=dydx*iid
	do ii=1,npar
	   if(ifreepar(ii).le.0) deriv(ii)=0
	end do

	sfDER=0

	return

	end

	function sFCHI(Par,Npar)

#include "trackn.inc"

	dimension par(30)

	ii=npar
	chisqr=0
	p0=par(1)
	p1=par(2)
	do ii=1,ndata
	   iii=ixfit(ii)
	   jj=p0+p1*iii
	   if(jj.gt.Ndata1 .and. jj.lt.ndata2) then
		yjval=yfit(jj)
	   else
		yjval=0.
	   endif
	   chisqr=chisqr+dyfit(iii)*( xfit(iii)-yjval )**2
	end do
	chisqr=chisqr/max(1,(ndata-nfreepar))
	sFCHI=chisqr

	return

	end

	SUBROUTINE boundfit(ifit,sf0,sf1,chichi)

#include "trackn.inc"

	dimension spar(M$P)

	integer TRSET, TRDER
	external TRFUN,TRDER,TRCHI,TRSET
	external       pFDER,pFCHI

	Character*30 Msg (6)
	Data Msg /'equal region markers',
	1	'peak mark outside fit region',
	1	'too less degrees of freedom',
	1	'matrix error',
	1	'F_lambda greater than 10**20',
	1	'unknown error'/


	yyt=0				! inverte la calibrazione energetica
	do ii=1,NRENE			! per settare le posizioni iniziali
	   xfit(ii)=RENE(ii)
	   yfit(ii)=cpolinv(xfit(ii),ecal,-m$k)
*	   print*,xfit(ii),yfit(ii)
	   yfit(ii)=sf0+yfit(ii)*sf1	! tenendo conto del gain_shift
	   yyt=yyt+yfit(ii)
	   dyfit(ii)=1
	end do
*	print*,(ecal(jj), jj=1,m$k+1)

	iestart=5*NRENE+1			! start parametri Ecal
	netrpar=ireord+1			! numero parametri Ecal
*** N.M
*	sqrterm=.true.

	if(sqrterm)then
	  netrpar=netrpar+1
	  netrpar1=1-netrpar
	 else
	  netrpar1=netrpar
	endif
*******
	trpar(iestart  )=yyt/NRENE
	do ii=1,m$k-1
	  trpar(iestart+ii)=0
	end do
	if(sqrterm)trpar(iestart+m$k)=0

	do ii=1,netrpar
	   ifreepar(ii)=ii
	end do
	ndata=NRENE

**       do ii=1,Nrene
**	 print*,ii,xfit(ii),yfit(ii),' -> ',cpol(xfit(ii),trpar(iestart),netrpar1),netrpar1
**      enddo
**       read(*,*)

	call Curfit(trpar(iestart),netrpar1,Ndata,pFDER,pFCHI,kv,0,rchichi)

**	 do ii=1,Nrene
**	   print*,ii,xfit(ii),yfit(ii),' -> ',cpol(xfit(ii),trpar(iestart),netrpar1)
**	 enddo
**	 read(*,*)


	yyt=0				! determina la calibrazione sigma**2
	do ii=1,NRENE
	   xfit(ii)=RENE(ii)
	   po=cpol(RENE(ii),trpar(iestart),netrpar1)
	   fw=cpol(po,wcal,m$k)
	   yfit(ii)=(fw/2.355)**2	! cioe' sigma**2
	   yyt=yyt+yfit(ii)
	   dyfit(ii)=1
	end do
	iwstart=5*NRENE+m$k+2			! start parametri Wcal
	nwtrpar=irword+1			! numero parametri Wcal
	trpar(iwstart  )=yyt/NRENE
	do ii=1,m$k-1
	  trpar(iwstart+ii)=0
	end do
	do ii=1,nwtrpar
	   ifreepar(ii)=ii
	end do
	ndata=NRENE
	call Curfit(trpar(iwstart),nwtrpar,Ndata,pFDER,pFCHI,kv,0,rchichi)

	Ntrpar=5*NRENE+2*m$k		! numero totale di parametri
	do ii=1,ntrpar
	   iwfree(ii)=0
	end do
	nfreepar=0

	do ii=1,NRENE			! assumendo tutte zone distinte
	   po= cpol(RENE(ii),trpar(iestart),netrpar1)
***	   write(6,*)po
***	   pause
	   s2= abs( cpol(RENE(ii),trpar(iwstart),nwtrpar) )
	   fw=sqrt(s2)*2.35482
	   bb= min( spek(NINT(po-2*fw)) , spek(NINT(po+2*fw)) )
	   bb=abs(bb)
	   aa=0
	   do ipo=NINT(po)-2,NINT(po)+2
	      aa=max(aa,spek(ipo))
	   end do
	   aa=abs(aa)
	   if(bb.ge.aa) bb=aa/5.
	   aa= aa-bb
	   
	   iioffs=5*(ii-1)
	   trpar(iioffs+1)=bb		! fondo
	   iwfree(iioffs+1)=iioffs+1	! libero

	   trpar(iioffs+2)=0		! pendenza
	   iwfree(iioffs+2)=0		! fissata a 0

	   trpar(iioffs+3)=s2		! sigma**2
	   iwfree(iioffs+3)=-2		! legato al secondo polinomio

	   trpar(iioffs+4)=po		! posizione
	   iwfree(iioffs+4)=-1		! legato al primo   polinomio

	   trpar(iioffs+5)=aa		! ampiezza
	   iwfree(iioffs+5)=iioffs+5	! libero
	end do
	do ii=iestart,ntrpar
	   iwfree(ii)=0
	end do
	do ii=iestart,iestart+netrpar-1
	   iwfree(ii)=ii
	end do
	do ii=iwstart,iwstart+nwtrpar-1
	   iwfree(ii)=ii
	end do

	do ii=1,ntrpar			! salva i parametri per confronto
	   spar(ii)=trpar(ii)
	end do
	call TRSETREG			! setta le regioni di fit
	call TRSETPAR			! impacca i parametri liberi

	if (Ndata.le.nfreepar) then	! ndata    e' settato da TRSET
	   kv=3				! nfreepar e' settato da TRSETPAR
	   goto 99
	endif
	
	kv=0
	if(ifit.le.0) then
	   chichi=TRCHI(fitpar,Nfitpar)
	   goto 99
	endif


	do ii=jwstart,jwstart+nwtrpar-1	! primo giro a s2 fissa
	    ifreepar(ii)=0
	    nfreepar=nfreepar-1
	end do
	call Curfit1(fitpar,Nfitpar,Ndata,TRDER,TRCHI,TRSET,kv,1,chichi)

	do ii=jwstart,jwstart+nwtrpar-1	! e poi la libera
	    ifreepar(ii)=ii
	    nfreepar=nfreepar+1
	end do
	call Curfit1(fitpar,Nfitpar,Ndata,TRDER,TRCHI,TRSET,kv,1,chichi)


99	If (kv.ne.0) then
	   If (kv.lt.1.or.kv.gt.5) kv= 6
	   WRITE(6,*) 'Fit error: '//Msg(kv)
	   BREAK=.true.
	endif

	call TRCHIreg(fitpar,Nfitpar)	! calcola chisq. per le varie regioni
	call TRGETPAR			! rimette i parametri

!	do ii=1,npar
!	   write(6,*) ii,spar(ii),trpar(ii)
!	end do

	return

	end

	SUBROUTINE INPUTRENE

#include "trackn.inc"

	CHARACTER*50 EFILE	/'RECAL_ENER'/
	character*50 linec
	logical*1 SCAMBIO,AGGIUNGI
	PARAMETER (NSOURCES=11)
	PARAMETER (MAXNGAMMA=20)
	INTEGER   NGAMMA(NSOURCES),ISOURCE(NSOURCES)
	DIMENSION GAMMA(MAXNGAMMA,NSOURCES),DGAMMA(MAXNGAMMA,NSOURCES)
	CHARACTER*5 SOURCE(NSOURCES)
	DATA SOURCE/' 22Na',' 56Co',' 57Co',' 60Co',' 88Y ','133Ba',
	1	    '134Cs','137Cs','152Eu','226Ra','241Am'/
	DATA NGAMMA / 2,16,3,2,3,9,9,1,12,17,2/
	DATA GAMMA  / 511.006 ,1274.545 , 18*0.0,
	1	      846.771 ,977.373, 1037.840 ,1175.102 ,1238.282 ,1360.215  ,
	1	     1771.351 ,2015.181 ,2034.755 ,2598.459 , 3009.596, 3201.962 ,
	1	     3253.416 ,3272.990 ,3451.152 ,3547.925 , 4*0.0,
	2 	       14.4130, 122.0614, 136.4743, 17*0.0,
	3	     1173.238 ,1332.513 , 18*0.0,
	4 	      898.045 ,1836.062 ,2734.087 , 17*0.0,
	5	       53.156 ,  79.623 ,  80.999 , 160.609 , 223.116 ,
	5	      276.404 , 302.858 , 356.014 , 383.859 , 11*0.0,
	6	      475.36  , 563.27  , 569.30  , 604.68  , 795.78  ,
	6	      801.86  ,1038.53  ,1167.89  ,1365.17  , 11*0.0,
	7	      661.661 , 19*0.0,
	8	      121.7817, 244.6975, 344.2785, 411.1165 , 443.965 ,
	8	      778.9040 , 964.079 ,1085.869 ,1089.737 ,1112.069 ,
	8	      1299.140, 1408.006 , 8*0.0,
	9 	      186.211 , 241.981 , 295.213 , 351.921 , 609.312 ,
	9 	      768.356 , 934.061 ,1120.287 ,1238.110 ,1377.669 ,
	9 	     1509.228 ,1729.595 ,1764.494 ,1847.420 ,2118.551 ,
	9 	     2204.215 ,2447.810 , 3*0.0,
	1 	       26.345 ,  59.537 , 18*0.0/
	DATA DGAMMA /   0.001 ,   0.017 , 18*0.0,
	1	        0.004 ,   0.004,   0.006 ,   0.016 ,   0.017 ,   0.07  ,
	1	        0.026 ,   0.028 ,   0.029 ,   0.033 ,   0.046 ,   0.046 ,
	1	        0.045 ,   0.045 ,   0.047 ,   0.061 , 4*0.0,
	2 	        0.0003,   0.0001,   0.0003, 17*0.0,
	3	        0.015 ,   0.018 , 18*0.0,
	4 	        0.012 ,   0.025 ,   0.030 , 17*0.0,
	5	        0.005 ,   0.005 ,   0.004 ,   0.025 ,   0.035 ,
	5	        0.007 ,   0.005 ,   0.009 ,   0.009 , 11*0.0,
	6	        0.05  ,   0.05  ,   0.03  ,   0.02  ,   0.02  ,
	6	        0.03  ,   0.05  ,   0.06  ,   0.10  , 11*0.0,
	7	        0.003 , 19*0.0,
	8	        0.0003,   0.0008,   0.0017,   0.008 ,   0.006 ,
	8	        0.009 ,   0.034 ,   0.034 ,   0.034 ,   0.070 ,
	8	        0.035 ,   0.035 , 8*0.0,
	9	        0.010 ,   0.008 ,   0.008 ,   0.008 ,   0.007 ,
	9	        0.010 ,   0.012 ,   0.010 ,   0.012 ,   0.012 ,
	9	        0.015 ,   0.015 ,   0.014 ,   0.025 ,   0.030 ,
	9	        0.040 ,   0.100 , 3*0.0,
	1	        0.001 ,   0.001 , 18*0.0/

	INTYP=0
	call xtptermfocus
	CALL INP_I1('Input Energies from File(1),Hand(2),Source(3)',INTYP)
	if(intyp.le.0 .or. intyp.gt.3) goto 50
	if(nrene.gt.0) then
	   aggiungi=inp_not('Want to add to the previous energies')
	else
	   aggiungi=.false.
	endif
	if(aggiungi) then
	   nrene1=nrene
	else
	   nrene1=0
	endif	   
	IF(INTYP.EQ.1) THEN
	   nrene=nrene1
	   nm_dum=INP_CH('Filename',EFILE)
	   OPEN(UNIT=1,FILE=EFILE,FORM='FORMATTED',STATUS='OLD',ERR=12)
10	   READ(1,*,ERR=12,END=12) TRENE,TDRENE
	   nrene=nrene+1
	   rene(nrene)=trene
	   drene(nrene)=tdrene
	   if(nrene.lt.N$P) goto 10
12	   CLOSE(1)
	ELSEIF(INTYP.EQ.2) THEN
	   DO II=nrene1+1,N$P
		write(linec,'(''Energy, Err. of peak # '',i2,'' ( <RET> to finish ) '')') ii
		llinec=lengthc(linec)
		IIC=INP_R2(linec(1:llinec),TRENE,TDRENE)
		if(iic.le.0) then
		   nrene=ii-1
		   goto 50
		endif
		rene(ii)=TRENE
		drene(ii)=TDRENE
	   end do	   
	ELSEIF(INTYP.EQ.3) THEN
	   do ii=1,nsources
		write(6,'(i5,3x,a)') ii,source(ii)
	   end do
	   nis=inp_ia('Input source number(s)',ISOURCE,NSOURCES)
	   if(nis.le.0) goto 50
	   do ii=1,nis
	   if(isource(ii).lt.1 .or. isource(ii).gt.nsources) goto 50
	     Nrene=Nrene1+ngamma(isource(ii))
	     nrene=min(nrene,N$P)
	     do iii=nrene1+1,nrene
		rene (iii)=gamma (iii-nrene1,isource(ii))
		drene(iii)=dgamma(iii-nrene1,isource(ii))
	     end do
	     if(nrene.gt.N$P) goto 50
	     nrene1=nrene
	   end do
	ELSEIF(INTYP.GT.0) THEN
	   RETURN
	ENDIF

50	if(nrene.lt.2) return

	scambio=.true.		! Li ordina in energia
	do while(scambio)
	  scambio=.false.
	  do ii=1,nrene-1
	    if(rene(ii+1).lt.rene(ii)) then
		xx=rene(ii)
		rene(ii)=rene(ii+1)
		rene(ii+1)=xx	
		xx=drene(ii)
		drene(ii)=drene(ii+1)
		drene(ii+1)=xx	
		scambio=.true.
	    endif
	  end do
	end do

	do ii=1,nrene
	   if(drene(ii).eq.0) drene(ii)=abs(rene(ii)/10000.)
	   write(6,*) ii,rene(ii),drene(ii)
	end do

60	call inp_i1('To delete a line type its number ( <RET> to continue )'
	1	,ii)

	if(ii.le.0 .or. ii.gt.nrene) RETURN

	nrene=nrene-1
	do jj=ii,nrene
	   rene(jj)=rene(jj+1)
	   drene(jj)=drene(jj+1)
	end do
	goto 50

	END

	subroutine TRSETREG

#include "trackn.inc"

	po=trpar(4)			! setta la prima regione
	fw=sqrt(trpar(3))*2.355
	itrr1(1)=po-fw*fwfact
	itrr2(1)=po+fw*fwfact
	ntrreg=1
	itrreg(1)=1			! in che regione di fit sta la riga
	ipack1(1)=1			! la prima di una regione di fit
	ipack2(1)=1			! e l'ultima

	do ii=2,NRENE			! setta le regioni di fit
	   iipar=5*(ii-1)
	   po=trpar(iipar+4)
	   fw=sqrt(trpar(iipar+3))*2.355
	   jtrr1=po-fw*fwfact
	   jtrr2=po+fw*fwfact
	   if(jtrr1.gt.itrr2(ntrreg)) then
		ntrreg=ntrreg+1		! nuova regione
		itrr1(ntrreg)=jtrr1
		itrr2(ntrreg)=jtrr2
		ipack1(ntrreg)=ii
		ipack2(ntrreg)=ii
	   else
		itrr2(ntrreg)=jtrr2	! estende la precedente
		ipack2(ntrreg)=ii
	   endif
	   itrreg(ii)=ntrreg
	end do

	do ii=1,ntrreg	
	   ip1=ipack1(ii)
	   ip2=ipack2(ii)
	   if(ip2.gt.ip1) then			! aggiusta il fondo
		bb=trpar(5*ip1-4)		! per le righe raggruppate
		do jj=ip1,ip2
		   bb=min(bb,trpar(5*jj-4))
		end do
		bb=max(bb,0.)
		do jj=ip1,ip2			! riporta il valore calcolato
		   trpar(5*jj-4)=bb
		end do
		do jj=ip1+1,ip2			! collega i parametri di fondo
		   iwfree(5*jj-4)=5*ip1-4
		end do
	   endif	   
	end do

	return

	end

	function TRSETPAR

#include "trackn.inc"
	integer TRSET
	external TRSET

	Nfitpar=0
	do ii=1,ntrpar
	  if(iwfree(ii).eq.ii) then
	    Nfitpar=Nfitpar+1
	    fitpar(Nfitpar)=trpar(ii)
	    i_w_e(ii)=Nfitpar
	    i_e_w(Nfitpar)=ii
	    ifreepar(Nfitpar)=Nfitpar
	    if(ii.eq.iestart) jestart=nfitpar
	    if(ii.eq.iwstart) jwstart=nfitpar
	  endif
	end do
	nfreepar=nfitpar

	do ii=ika,ikl-1		! riporta i dati nei common del fit
	  xfit(ii)=ii
	  yfit(ii)=spek(ii)
	  dyfit(ii)=1./err2(ii)
	end do

	ii = TRSET(fitpar,nfitpar)

	trsetpar=0

	return

	end

	function TRGETPAR

#include "trackn.inc"

	if(sqrterm)then
	  netrpar1=1-netrpar
	else
	  netrpar1=netrpar
	endif


	do ii=iestart,ntrpar	! riporta prima le calibrazioni
	   if(iwfree(ii).eq.ii) trpar(ii)=fitpar(i_w_e(ii))
	end do
	do ii=1,iestart-1
	   iptype=iwfree(ii)
	   if(iptype.eq.ii) then
		trpar(ii)=fitpar(i_w_e(ii))
	   elseif(iptype.eq.-1) then
		line=(ii+4)/5
		trpar(ii)=cpol(RENE(line),trpar(iestart),netrpar1)
	   elseif(iptype.eq.-2) then
		line=(ii+4)/5
		trpar(ii)=abs( cpol(RENE(line),trpar(iwstart),nwtrpar) )
	   endif
	end do
	do ii=1,iestart-1
	   iptype=iwfree(ii)
	   if(iptype.gt.0 .and. iptype.ne.ii) trpar(ii)=trpar(iptype)
	end do

	trgetpar=0
	return

	end

	FUNCTION DFTRFUN(XII)

#include "trackn.inc"

	real*8 zz
	
	yy=0
	bg=trpar(5*ipack1(ireg)-4)
	do ii=ipack1(ireg),ipack2(ireg)
	   po=trpar(5*ii-1)+0.5		! al centro del canale
	   s2=trpar(5*ii-2)
	   aa=trpar(5*ii  )
	   zz=(xii-po)**2/(2*s2)
	   if(zz.lt.20) yy=yy+aa*dexp(-zz)
	end do
	yy=yy+bg
	dftrfun=yy
*	DFTRFUN=DIFUNCT(yy,ifunct)

	RETURN

	END

	function TRFUN(id,par,npar)

#include "trackn.inc"

	dimension par(100)
	real*8 zz, z2

	TRFUN=0

	if(sqrterm)then
	  netrpar1=1-netrpar
	else
	  netrpar1=netrpar
	endif


	jd=npar
	jd=ixfit(id)
	xi=xfit(jd)

	irn=irfit(id)
	line1=ipack1(irn)
	line2=ipack2(irn)
	bg=par(i_w_e(5*line1-4))	! ricordarsi del fondo inclinato
	do line=line1,line2
	   energy=RENE(line)
	   po=cpol(energy,par(jestart),netrpar1)
	   s2=abs( cpol(energy,par(jwstart),nwtrpar) )
	   aa=par(i_w_e(5*line))
	   zz=(xi-po)
	   zz=zz*zz/(2*s2)
	   if(zz.lt.25) TRFUN=TRFUN+aa*dexp(-zz)
	end do
	TRFUN=TRFUN+bg

	return

	end

	integer function TRDER(id,par,npar,Deriv,dyi,wi)

#include "trackn.inc"

	real par(100),deriv(100)
	real dyi,wi
	real*8 ex2
	
	external TRFUN

*	do ii=1,nfitpar
	do ii=1,100
	   deriv(ii)=0.0E0
	end do

	if(sqrterm)then
	  netrpar1=1-netrpar
	else
	  netrpar1=netrpar
	endif

	jd=ixfit(id)
	xi=xfit(jd)
	yi=yfit(jd)
	fyi=TRFUN(id,par,npar)
	dyi=yi-fyi
	wi=dyfit(jd)

	nre=irfit(id)
	line1=ipack1(nre)
	line2=ipack2(nre)

	deriv(i_w_e(5*line1-4))=1.

	do line=line1,line2
	   energy=RENE(line)
	   po=cpol(energy,par(jestart),netrpar1)
	   s2=abs( cpol(energy,par(jwstart),nwtrpar) )
	   aa=par(i_w_e(5*line))
	   dx=xi-po
	   ex2=dx*dx/(2.00D0*s2)
	   if(ex2.lt.25) then
		eexx=dexp(-ex2)
		deriv(i_w_e(5*line))=eexx
		eexx=aa*eexx
		fe=eexx*dx/s2
		do jj=jestart,jestart+netrpar-1
		   deriv(jj)=deriv(jj)+fe
		   fe=fe*energy
		end do
		fe=eexx*ex2/s2
		do jj=jwstart,jwstart+nwtrpar-1
		   deriv(jj)=deriv(jj)+fe
		   fe=fe*energy
		end do
	   endif
	end do
	do ii=1,npar
	   if(ifreepar(ii).ne.ii) deriv(ii)=0
	end do

	TRDER=0

	return

	end

	function TRCHI(par,npar)

#include "trackn.inc"

	dimension par(100)

	chisqr=0
	do ii=1,ndata
	   fyfit=TRFUN(ii,par,npar)
	   jj=ixfit(ii)
	   chisqr=chisqr+dyfit(jj)*( yfit(jj) - fyfit )**2
	end do
	chisqr=chisqr/max(1,(ndata-nfreepar))
	TRCHI=chisqr

	return

	end

	subroutine TRCHIreg(par,npar)

#include "trackn.inc"

	dimension par(100)

	do ii=1,ntrreg
	   trchichi(ii)=0.
	end do
	do ii=1,ndata
	   fyfit=TRFUN(ii,par,npar)
	   jj=ixfit(ii)
	   chisqr=dyfit(jj)*( yfit(jj) - fyfit )**2
	   irn=irfit(ii)
	   trchichi(irn)=trchichi(irn)+chisqr	   
	end do
	do ii=1,ntrreg				! numero di parametri liberi
	   ifp=itrr2(ii)-itrr1(ii)		! numero di canali
	   ifp=ifp-2*(ipack2(ii)-ipack1(ii)+1)	! numero di picchi(pos+ampl)
	   ifp=ifp-1				! fondo comune
	   trchichi(ii)=trchichi(ii)/max(1,ifp)
	end do

	return

	end

	integer function TRSET(par,npar)

#include "trackn.inc"

	real par(100)
	real energy

	if(sqrterm)then
	  netrpar1=1-netrpar
	else
	  netrpar1=netrpar
	endif

	ii=npar
	par(jestart+1)=abs(par(jestart+1))
	do ii=jwstart,jwstart+1
	   par(ii)=abs(par(ii))
	end do
	do ii=1,jestart-1
	   par(ii)=abs(par(ii))		! posizioni e ampiezze
	end do

     	Ndata=0
	do ii=1,ntrreg
	   line1=ipack1(ii)
	   energy=RENE(line1)
	   po=cpol(energy,par(jestart),netrpar1)
	   s2=abs( cpol(energy,par(jwstart),nwtrpar) )
	   fw=sqrt(s2)*2.355
	   itrr1(ii)=po-fw*fwfact

	   line2=ipack2(ii)
	   energy=RENE(line2)
	   po=cpol(energy,par(jestart),netrpar1)
	   s2=abs( cpol(energy,par(jwstart),nwtrpar) )
	   fw=sqrt(s2)*2.355
	   itrr2(ii)=po+fw*fwfact

	   do jj=itrr1(ii),itrr2(ii)
		Ndata=Ndata+1
		ixfit(Ndata)=jj
		irfit(Ndata)=ii
	   end do

	end do


	TRSET=0

	return

	end

	SUBROUTINE dispwreg(jreg)

#include "trackn.inc"

	ireg=jreg

	CHMIN=itrr1(ireg)
	CHMAX=itrr2(ireg)

	DO II=ipack1(ireg),ipack2(ireg)
	   CHMIN=MIN(trpar(5*ii-1),chmin)
	   CHMAX=MAX(trpar(5*ii-1),chmax)
	end do

	DCH=(CHMAX-CHMIN)/2

	MARKLE=CHMIN-DCH
	MARKRI=CHMAX+DCH

	MARKLE=MAX(MARKLE,IKA)
	MARKRI=MIN(MARKRI,IKL)

	NMIN=MARKLE
	NMAX=MARKRI

	RMARKLE=MARKLE
	RMARKRI=MARKRI
	
	CALL FINDYLIMITS
	CALL xtplotnew

	RETURN

	END

	SUBROUTINE SHOWgauss1(jreg)

#include "trackn.inc"

	EXTERNAL DFTRFUN


	ireg=jreg

	CH1=itrr1(ireg)
	CH2=itrr2(ireg)

	DO II=ipack1(ireg),ipack2(ireg)
	   CH=trpar(5*ii-1)+0.5		! al centro del canale
	   CH1=MIN(CH1,CH)
	   CH2=MAX(CH2,CH)
	end do
	CALL xtpoverlay(DFTRFUN,CH1,CH2+1.,0.25,1,dwin,twin)

	RETURN

	END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	SUBROUTINE WDEFINE

#include "trackn.inc"

	character*50 gatefile
	integer nnsd	/0/

	logical*1 logtemp
	logical*1 reorder	/.false./

	call xtptermfocus

	ngnext=0
	if(ngates.eq.0) then
	  ngates=0
	  lnch=INP_CH('Name of file with gates',GATEFILE)
	  if(lnch.gt.0) then
	    lgatefile=lengthc(gatefile)
	    if(lgatefile.le.0) goto 99
	    call filetype(gatefile,'gates')
	    open(unit=3,file=gatefile,status='OLD',err=99)
	    ngates=0
	    do while(ngates.le.m$gate)
	      read(3,*,end=20,err=99) egate(1,ngates+1),egate(2,ngates+1)
	      ngates=ngates+1
	    end do
20	    close(3)
	  elseif(inp_not('Want generate SD gates')) then
	    sdnn=nnsd
	    lnch=inp_r4('E0,DE,WE,#',sde0,sdde,sdwe,sdnn)
	    if(lnch.eq.-1) return
	    nnsd=nint(sdnn)
	    nnsd=min(nnsd,m$gate)
	    sdnn=nnsd
	    do ii=0,sdnn-1
	      sdee=sde0+ii*sdde
	      ngates=ngates+1
	      egate(1,ngates)=sdee-sdwe/2
	      egate(2,ngates)=sdee+sdwe/2
	    end do
	  endif
	endif

	call checkgates(0)

	logtemp=.true.
	do while( (ngates.ge.1) .and.logtemp)
	  write(6,*)'    List of gates'
	  do ii=1,ngates
	   if(mdim .ne. 3)then
	    write(6,'(i6,f12.1,'' keV'',7x,2i8)') ii,(egate(1,ii)+egate(2,ii))/2.,igate(1,ii),igate(2,ii)
	   else
	    write(6,'(i6,''  group: '',i3,f12.1,'' keV'',7x,2i8)') ii,gclass(ii),(egate(1,ii)+egate(2,ii))/2.,
     &						  igate(1,ii),igate(2,ii)
           endif
	  end do
	  iic=inp_i1('To delete a line type its number ( <RET> to continue )',nn)
	  if(iic.gt.0) then
	    if(nn.gt.0 .AND. nn.le.ngates) then
	      do ii=nn+1,ngates
		egate(1,ii-1)=egate(1,ii)
		egate(2,ii-1)=egate(2,ii)
		cgate(1,ii-1)=cgate(1,ii)
		cgate(2,ii-1)=cgate(2,ii)
		igate(1,ii-1)=igate(1,ii)
		igate(2,ii-1)=igate(2,ii)
		gclass(ii-1)=gclass(ii)
	      end do
	      ngates=ngates-1
	    else
	      logtemp=.false.
	    endif
	  else
	    logtemp=.false.
	  endif	   
	end do

*****   3D class gates
       if( mdim .eq. 3)then
        logtemp=.true.
	do while( (ngates.ge.1) .and.logtemp)
	  write(6,*)'    List of gates'
	  do ii=1,ngates
	    write(6,'(i6,''  group: '',i2,f12.1,'' keV'',7x,2i8)') ii,gclass(ii),(egate(1,ii)+egate(2,ii))/2.,
     &						  igate(1,ii),igate(2,ii)
	  end do
	  nn=1
	  nnclass=gclass(1)
	  iic=inp_i2('To set group# for a line type its number and new group# ( <RET> to continue )',nn,nnclass)
	  if(iic.gt.0) then
	    if ( nnclass .ge. 0 .and. nnclass .lt. 1000 .and. nn.le.ngates .and. nn.gt.0)then
	       gclass(nn)=nnclass
	    else
	       write(6,'('' %%%input error - group not changed'')')
	    endif
	  else
	    logtemp=.false.
	  endif	   
	end do
       endif



	call checkgates(0)
	logtemp=.false.
	do ii=1,ngates-1
	  if(egate(1,ii).gt.egate(1,ii+1)) logtemp=.true.
	end do
	if(logtemp) then
	  call inp_ask('Want to reorder them',reorder)
	  if(reorder) then
	    call checkgates(1)
	    write(6,*)'    List of gates'
	    do ii=1,ngates
	   if(mdim .ne. 3)then
	    write(6,'(i6,f12.1,'' keV'',7x,2i8)') ii,(egate(1,ii)+egate(2,ii))/2.,igate(1,ii),igate(2,ii)
	   else
	    write(6,'(i6,''  group: '',i3,f12.1,'' keV'',7x,2i8)') ii,gclass(ii),(egate(1,ii)+egate(2,ii))/2.,
     &						  igate(1,ii),igate(2,ii)
           endif
	    end do
	  endif
	endif

	if(ngates.ge.1) then
	  if(inp_not('Want to write gates on a file')) then
	    idmmy_inp=INP_CH('Filename',GATEFILE)
	    lgatefile=lengthc(gatefile)
	    if(lgatefile.le.0) goto 99
	    call filetype(gatefile,'gates')
	    lgatefile=lengthc(gatefile)
	    open(unit=3,file=gatefile,status='new',err=99)
	    do ii=1,ngates
	      write(3,'(2f12.1)',err=99) egate(1,ii),egate(2,ii)
	    end do
	    write(6,'(i5,a)') ngates,'     Gates written on   '//gatefile(1:lgatefile)
	    close(3)
	  endif
	endif

	return

99	call xtpbell
	ngates=0
	ngnext=0
	return

	end

	SUBROUTINE checkgates(iord)

#include "trackn.inc"

	logical*1 repeat

	ngnext=0
	if(ngates.lt.1) return

	do ii=1,ngates
	  if(egate(1,ii).gt.egate(2,ii)) call swapr(egate(1,ii),egate(2,ii))
	end do

	if(iord.eq.1 .AND. ngates.gt.1) then		! li ordina in energia
	  nn=ngates-1
	  repeat=.true.
	  do while(repeat)
	    repeat=.false.
	    do jj=1,nn
	      if(egate(1,jj).gt.egate(1,jj+1)) then
		repeat=.true.
		call swapr(egate(1,jj),egate(1,jj+1))
		call swapr(egate(2,jj),egate(2,jj+1))
		kkk=gclass(jj)
		gclass(jj)=gclass(jj+1)
		gclass(jj+1)=kkk
	      endif
	    end do
	    nn=nn-1    
	  end do
	endif

	if(iecal.ne.1) then
	  do ii=1,ngates
	    igate(1,ii)=egate(1,ii)
	    igate(2,ii)=egate(2,ii)
	    cgate(1,ii)=igate(1,ii)+0.50000
	    cgate(2,ii)=igate(2,ii)+0.50000
	  end do
	else
	  do ii=1,ngates
	    igate(1,ii)=nint( cpolinv(egate(1,ii),ECAL,-M$K) )
	    igate(2,ii)=nint( cpolinv(egate(2,ii),ECAL,-M$K) )
	    cgate(1,ii)=igate(1,ii)+0.500000000
	    cgate(2,ii)=igate(2,ii)+0.500000000
	  end do
	endif

	igmin=0			! elimina quelli fuori range
	igmax=ikl-1
	if(mres(gside).gt.0) then
	   igmax=mres(gside)-1
	else
	   igmax=ikl-1
	endif
	nout=0
	do ii=1,ngates
	  if(igate(1,ii).lt.igmin .OR. igate(1,ii).gt.igmax .OR.
	1    igate(2,ii).lt.igmin .OR. igate(2,ii).gt.igmax) then
	    igate(1,ii)=-1
	    igate(2,ii)=-1
	    nout=nout+1
	  endif
	end do
	if(nout.gt.0) then
	  nn=0
	  do ii=1,ngates
	    if(igate(1,ii).ge.igmin) then
	      nn=nn+1
	      igate(1,nn)=igate(1,ii)
	      igate(2,nn)=igate(2,ii)
	      egate(2,nn)=egate(2,ii)
	      cgate(1,nn)=igate(1,nn)+0.500000
	      cgate(2,nn)=igate(2,nn)+0.500000
	    endif
	  end do
	  ngate=nn
	endif

	return

	end

	SUBROUTINE QDEFINE

#include "trackn.inc"

	character*60 oldcmat
	character*10 nm_string
	logical*1 ltemp
*	character cchh


	call xtptermfocus
	if(cmatrix) then
	  write(6,*) 'You are using  the compressed matrix   '//cmatfile(1:lcmatfile)
	  oldcmat=cmatfile
	  lnch=inp_CH( 'File_name of new compressed matrix (<RET> if same) ',CMATFILE)
	  if(lnch.eq.-1) return
	else
	  lnch=inp_CH( 'File_name of compressed matrix ',CMATFILE)
	  if(lnch.le.0) return
	endif

	if(lnch.gt.0) then
	  call filetype(cmatfile,'cmat')
	  lcmatfile=lengthc(cmatfile)
	  if(cmatrix) then			 ! cleanup from previous one
	    call cmt_readspec_reset		 ! reset internal buffers
	    if(.not.cmt_close(%val(cmt),oldcmat)) continue
	    cmatrix=.false.
	    isproje=.false.
	    iscut=.false.
	    if(autobg.useproj) then		       ! cleanup from previous one
	       call cmt_readspec_reset  	       ! reset internal buffers
	       if(.not.cmt_close(%val(autobg.cmt),autobg.projfile)) continue
	       autobg.cmt=0
	       autobg.useproj = .FALSE.
	    endif
	  endif
	  if(.not.cmt_open(cmatfile,0,cmt)) then
	    write(6,*) 'Matrix open error'
	    goto 99
	  endif
	  if(.not.cmt_info(%val(cmt),-1,mdim,mmode)) continue
	  if(.not.cmt_info(%val(cmt),-2,mres,mstep)) continue
	  call cmt_show(%val(cmt),6)
	  IF(mdim.gt.2 .AND. mmode.eq.0) then
	    write(6,*) 'Cubes and Hypercubes only if symmetrized'
	    if(.not.cmt_close(%val(cmt),cmatfile)) continue
	    goto 99
	  endif
	  do ii=1,mdim
	    if(.not.cmt_getproje(%val(cmt),ii,mproj(0,ii))) continue
	  end do
	  call cmt_readspec_reset		 ! reset internal buffers
	  cmatrix=.true.	   
	  isproje=.true.
	  iscut=.false.
	endif
	if(.not.cmatrix) return

	if(mmode.gt.0) then
	  transposed=.false.
	else
	  ltemp=transposed
	  call inp_ask('Want to use the matrix in transposed form',ltemp)
	  if(ltemp.ne.transposed) then
	    transposed=ltemp
	    if(transposed) then
	      gside=1
	      cside=2
	    else
	      gside=2
	      cside=1
	    endif
	    ikaw=0
	    iklw=mres(cside)
	    isproje=.true.
	    iscut=.false.
	  endif
	endif
	if(transposed) then
	  gside=1
	  cside=2
	else
	  gside=2
	  cside=1
	endif
	if(isproje) then
	  ikaw=min(max(ikaw,0),mres(cside)-1)
	  if(iklw.le.0) iklw=mres(cside)
	  iklw=min(iklw,mres(cside))
	  if(ikaw.gt.iklw) call swapl(ikaw,iklw)
	  call setproje
	endif

	if(.not.cmatrix) return

********  N.M. - real numbers
	if(spec_divfac .le. 0.0)spec_divfac=1.0000000000
*	do inm=1,10
*	 nm_string(inm:inm)=' '
*	enddo
*	write(nm_string,'(f10.1)')spec_divfac
*	call string_strip(nm_string)
*	nm_l=lengthc(nm_string)
*	write(*,'(a,$)')' Spectra will be divided by ['//nm_string(1:nm_l)//'] '
*	call inp_r1(' ',spec_divfac)
*	if(spec_divfac .le. 0.0)spec_divfac=1.0
	if(lnch .gt. 0)then
	autobg.tot=0
	do ii=1,mdim
	  do jj=0,mres(ii)-1
	    autobg.p(jj,ii)=mproj(jj,ii)
	    autobg.p(jj,ii)=autobg.p(jj,ii)/spec_divfac
	    if(ii.eq.1)autobg.tot=autobg.tot+autobg.p(jj,ii)
	    rr_nm=mproj(jj,ii)
	    mproj(jj,ii)=nint(rr_nm/spec_divfac)
	  enddo
	enddo
	endif
********

	ikaw=0
	iklw=mres(cside)
	jjj=inp_i2('Region to display after ''CW'' (channels)',ikaw,iklw)
	ikaw=min(max(ikaw,0),mres(cside))
	iklw=min(max(iklw,0),mres(cside))
	if(ikaw.gt.iklw) call swapl(ikaw,iklw)
	  

	call inp_ask('Keep gate markers after <CW>',keepgates)

	call inp_ask('Want to subtract the background',backsub)
	if(backsub) then
	  call wdef_cback(iok)
	  if(iok.ne.1) then
	    backsub=.false.
	    goto 99
	  endif
	  ! N.M. autobg.
	  if(autobg.yes)then
	   if(mmode .eq. 0) then
	    do ii=1,mdim
	      do jj=0,mres(ii)-1
	        autobg.sp(jj)=autobg.p(jj,ii)
	      enddo
*	      call autobgmin(autobg.sp(0),autobg.bg(0),mres(ii),1,mres(ii),1,4,0.00000)
*	      do jj=0,mres(ii)-1
*	        autobg.sp(jj)=autobg.bg(jj)
*		autobg.bg(jj)=0
*	      enddo
	      call autobgmin(autobg.sp(0),autobg.bg(0),mres(ii),1,mres(ii),autobg.m,autobg.itmax,autobg.fstep)
	      do jj=0,mres(ii)-1
	        autobg.bp(jj,ii)=autobg.bg(jj)
	      enddo
	    enddo
	 elseif (mmode .gt. 0) then
	   do jj=0,mres(1)-1
	     autobg.sp(jj)=autobg.p(jj,1)
	   end do
	   call autobgmin(autobg.sp(0),autobg.bg(0),mres(1),1,mres(1),autobg.m,autobg.itmax,autobg.fstep)
	   do ii=1,mdim
	      do jj=0,mres(ii)-1
	        autobg.bp(jj,ii)=autobg.bg(jj)
	      end do
	   end do
	 endif	 
	endif
	endif

**eff	call inp_ask('Want to have the efficiency correction',efficor)
**eff	if(efficor) then
**eff	  call wdef_effi(iok)
**eff	  if(iok.ne.1) then
**eff	    efficor=.false.
**eff	    goto 99
**eff	  endif
**eff	endif

	call setproje
	emptyframe=.false.
	return

99	call xtpbell
	return

	end

	SUBROUTINE WDEF_CBACK(iok)

#include "trackn.inc"

	data iformsback /4/
	data iformrback /4/
	character*50 sbackfile(2),rbackfile(2)
	character*1 chback
	logical*1 useproj
	character*60 oldcmat

	iok=0

	if(mdim.eq.2) goto 200

	if(backcom) then
	  chback='C'
	elseif (autobg.yes)then
	  chback='A'
	else
	  chback='L'
	endif
	if(mdim .eq. 3)then
	  nm_dum=inp_CH('Local, Auto or Common background ['//chback//'] ',chback)
	  call str_toupper(chback)
	  backcom=chback.eq.'C'
	  backpro=backcom
	  autobg.yes=chback.eq.'A'
	else
	  nm_dum=inp_CH('Local or Common background ['//chback//'] ',chback)
	  call str_toupper(chback)
	  backcom=chback.eq.'C'
	  backpro=backcom
	  autobg.yes= .false.
	endif
	if(backcom) then
	  do jj=1,mdim
	    sbacktot(jj)=0.
	    do ii=0,mres(jj)-1
	      sback(ii,jj)=mproj(ii,jj)
	      sbacktot(jj)=sbacktot(jj)+sback(ii,jj)
	    end do
	  end do
	  call inp_r1('Correction factor ',corrback(1))
	  do ii=2,mdim
	   corrback(ii)=corrback(1)
	  end do
	  iok=1
	  return
	elseif(autobg.yes) then
	   useproj=.false.
	   call inp_ask('Want to use 2D projection for fast access  ',useproj)
	   if(useproj)then
	    if(autobg.useproj) then
	      write(6,*) 'You are using  the compressed 2D projection   '//autobg.projfile(1:autobg.lprojfile)
	      oldcmat=autobg.projfile
	      lnch=inp_CH( 'File_name of new compressed 2D projection (<RET> if same) ',autobg.projfile)
	      if(lnch.eq.-1) useproj=.false.
	    else
	      lnch=inp_CH( 'File_name of compressed 2D projection ',autobg.projfile)
	      if(lnch.le.0) useproj=.false.
	    endif

	    if( (lnch.gt.0) .and. useproj ) then
	      call filetype(autobg.projfile,'cmat')
	      autobg.lprojfile=lengthc(autobg.projfile)
	      if(autobg.useproj) then			 ! cleanup from previous one
	         call cmt_readspec_reset		 ! reset internal buffers
	         if(.not.cmt_close(%val(autobg.cmt),oldcmat)) continue
		 autobg.cmt=0
	      endif
	      if(.not.cmt_open(autobg.projfile,0,autobg.cmt)) then
	         write(6,*) 'Matrix open error'
	         useproj=.false.
	       endif
	    endif	    
	  endif
	  autobg.useproj=useproj
	  goto 1200
	else
	  continue   ! eventualmente vedere di definire la largezza
	endif
	iok=1
	return


200	if(backcom) then
	  chback='C'
	else
	  chback='N'
	endif
	if(autobg.yes)chback='A'
	nm_dum=inp_CH('Normal, Auto or Common background ['//chback//'] ',chback)
	call str_toupper(chback)
	backcom=chback.eq.'C'
	autobg.yes=chback.eq.'A'
	if((.not.backcom) .and. (.not.autobg.yes)) then
	  iok=1
	  return
	endif
	
******N.M Auto bg. def.
1200	if(autobg.yes)then
	write(6,'(2x ''Current parameters values'')')
	write(6,'(5x,''Smoothing Window [channels] : '',i3)')autobg.m
	write(6,'(5x,''Slope to increase Sm. Win. : '',f8.5)')autobg.fstep
	write(6,'(5x,''Number of iterations : '',i3,/)')autobg.itmax
*	write(6,'(2x,''Do you want to change those values [Y/N] : '',$)')
*	read(*,'(a)')autobg.ch
	autobg.ch='N'
	nm_dum=inp_ch('Do you want to change those values ['//autobg.ch//']',autobg.ch)
	call str_toupper(autobg.ch)
	if(autobg.ch .eq. 'Y')then
*	 write(6,'(5x,''Smoothing Window [channels] : '',$)')
*	 read(*,*)autobg.m
	 call inp_i1('Smoothing Window [channels]',autobg.m)
*	 write(6,'(5x,''Slope to increase Sm. Win. : '',$)')
*	 read(*,*)autobg.fstep
1201	 call inp_r1('Slope to increase Sm. Window',autobg.fstep)
	 if(autobg.fstep .gt. 0.3000)then
	    write(6,'('' %%%ERROR - slope cannot be larger than 0.30'')')
	    goto 1201
	 endif
*	 write(6,'(5x,''Number of iterations : '',$)')
*	 read(*,*)autobg.itmax
	 call inp_i1('Number of iterations',autobg.itmax)
 	endif
	iok=1
	return
	endif
*********

	if(mmode.gt.0) then
	  call inp_ask('Want to use the projection  as background',backpro)
	else
	  call inp_ask('Want to use the projections as background',backpro)
	endif

	if(backpro) then
	  do jj=1,2
	    sbacktot(jj)=0.
	    do ii=0,mres(jj)-1
	      sback(ii,jj)=mproj(ii,jj)
	      sbacktot(jj)=sbacktot(jj)+sback(ii,jj)
	    end do
	  end do
	  if(mmode.gt.0) then
	    call inp_r1('Correction factor ',corrback(1))
	    corrback(2)=corrback(1)
	  endif
	  iok=1
	  return
	endif

	if(mmode.gt.0) then
	  write(6,*) 'Smooth background spectrum'
	  call readdat(1,sbackfile(1),sback(0,1),mres(1),iformsback,kv)
	  if(kv.le.0) return
	  nnchan=kv/100
	  iforsmback=mod(kv,100)
	  if(nnchan.ne.((mres(1)+1023)/1024)*1024) return
	  write(6,*) 'Background cut spectrum'
	  call readdat(1,rbackfile(1),rback(0,1),mres(1),iformrback,kv)
	  if(kv.le.0) return
	  nnchan=kv/100
	  iformrback=mod(kv,100)
	  if(nnchan.ne.((mres(1)+1023)/1024)*1024) return
	  do ii=0,mres(jj)-1
	    sback(ii,2)=sback(ii,1)
	    rback(ii,2)=rback(ii,1)
	  end do
	else
	  do jj=1,2
	    write(6,'(1x,a,i4)') 'Smooth background spectrum for matrix axis',jj
	    call readdat(1,sbackfile(jj),sback(0,jj),mres(jj),iformsback,kv)
	    if(kv.le.0) return
	    nnchan=kv/100
	    iforsmback=mod(kv,100)
	    if(nnchan.ne.((mres(jj)+1023)/1024)*1024) return
	  end do
	  do jj=1,2
	    write(6,'(1x,a,i4)') 'Background cut spectrum for matrix axis',jj
	    call readdat(1,rbackfile(jj),rback(0,jj),mres(jj),iformrback,kv)
	    if(kv.le.0) return
	    nnchan=kv/100
	    iformrback=mod(kv,100)
	    if(nnchan.ne.((mres(jj)+1023)/1024)*1024) return
	  end do
	endif

	do jj=1,2
	  sbacktot(jj)=0.
	  rbacktot(jj)=0.
	  do ii=0,mres(jj)-1
	    sbacktot(jj)=sbacktot(jj)+sback(ii,jj)
	    rbacktot(jj)=rbacktot(jj)+rback(ii,jj)
	  end do
	end do
	rbacktot12=0.
	do ii=0,mres(1)-1
	  datproj=mproj(ii,1)
	  datback=sback(ii,1)
	  errproj=sqrt(abs(datproj))
	  errback=sqrt(abs(datback))
	  if( (datproj-errproj).le. (datback+errback)) then
	    rbacktot12=rbacktot12+rback(ii,1)
	  endif
	end do

	iok=1
	return

	end

	SUBROUTINE WDEF_EFFI(iok)

#include "trackn.inc"
	character*50 effifile(2)
	character*50 line
	data iformeffi /3/

	iok=0
	if(mmode.gt.0) then
	  l2=1
	else
	  l2=mdim
	endif
	do ll=1,l2
	  if(mmode.gt.0) then
	    write(line,'(A)') 'Efficiency file'
	  else
	    write(line,'(A,i4)') 'Efficiency file for index',ll
	  endif
	  lline=lengthc(line)
	  write(6,'(1x,A)') line(1:lline)
	  call readdat(1,effifile(ll),reffi(0,ll),mres(ll),iformeffi,kv)
	  if(kv.le.0) goto 99
	  nnchan=kv/100
	  iformeffi=mod(kv,100)
	  if(nnchan.ne.mres(ll)) goto 99
	  effimax=0.
	  do ii=0,mres(ll)-1
	    tmp=reffi(ii,ll)
	    effimax=max(reffi(ii,ll),effimax)
	    if(tmp.gt.0) then
	      reffi(ii,ll)=1/tmp
	    else
	      reffi(ii,ll)=0.
	    endif
	  end do
	  if(effimax.gt.0) then
	    do ii=0,mres(ll)-1
	      reffi(ii,ll)=reffi(ii,ll)*effimax
	    end do
	  else
	    write(6,*) 'Efficiency spectrum is not acceptable'
	    goto 99
	  endif
	end do
	do ll=1,l2
	  if(mmode.gt.0) then
	    write(line,'(A)')'Scaling factor for the efficiency'
	  else
	    write(line,'(A,i4)')'Scaling factor for the efficiency of index',ll
	  endif	
	  lline=lengthc(line)
	  call inp_r1(line(1:lline),correffi(ll))
	  if(correffi(ll).le.0) then
	    correffi(ll)=1.
	    do ii=0,mres(ll)-1
	      reffi(ii,ll)=1.
	    end do
	  else
	    do ii=0,mres(ll)-1
	      reffi(ii,ll)=reffi(ii,ll)*correffi(ll)
	    end do
	  endif
	end do
	if(mmode.gt.0) then
	  do jj=2,mdim
	    effifile(jj)=effifile(1)
	    correffi(jj)=correffi(1)
	    do ii=0,mres(jj)-1
	      reffi(ii,jj)=reffi(ii,1)
	    end do
	  end do
	endif

	iok=1

99	return

	end

	subroutine setproje

#include "trackn.inc"

	nkap=(mres(gside)+1023)/1024
	ika=0
	ikl=mres(gside)
	do ii=ika,ikl-1
	  spek(ii)=mproj(ii,gside)
	  e2=abs(spek(ii))
	  if(e2.eq.0) e2=1
	  ERR2(ii)=e2
	end do
	do ii=mres(gside),1024*nkap-1
	  spek(ii)=0.
	  ERR2(ii)=1.
	end do
**eff	     if(efficor) then
**eff	       do ii=ika,ikl-1
**eff		 spek(ii)=spek(ii)*reffi(ii,gside)
**eff		 ERR2(ii)=ERR2(ii)*reffi(ii,gside)*reffi(ii,gside)
**eff	       end do
**eff	     endif
	isproje=.true.
	iscut=.false.

	return

	end

	SUBROUTINE WESEGUI(iok)

#include "trackn.inc"

	INTEGER speccut(0:MAXRES-1),specback(0:MAXRES-1)
	real    e2cut(0:MAXRES-1),e2back(0:MAXRES-1)
	real    centro(3),asse(3),distn2(3)
	real*8  pbackr,pbacks
	real	pfacr,pfacs,pfacf
	logical*1 putit
	
	integer ngroups
	integer igfound(M$GATE)

	PARAMETER (MAXNGG=64*1024)
	integer gates(MAXNGG)

	logical*1 cmt_readspecs
#ifdef __linux__
	integer*4 nn,ii,ngg,nggmax
	save nn,ii,ngg,nggmax
#endif

	if(.not.cmatrix .or. ngates.lt.(mdim-1)) goto 100

	call checkgates(0)
	if(ngates.lt.1) goto 100

	DO II=0,MAXRES-1
	   cut(II)=0.
	   e2cut(ii)=0.
	   back(II)=0.
	   e2back(ii)=0.
	   speccut(ii)=0
	end do

	write(6,*)

	cres=mres(cside)

	if(mdim.gt.2) goto 40

	nggmax=MAXNGG/2

	if((.not.backsub) .or. autobg.yes) then		!! no background subtraction
            nn =0
	  do nn=1,ngates
	    ngg=0
	    do iaddr=igate(1,nn),igate(2,nn)
	      if(ngg.ge.nggmax) then
	        write(6,*) nn,ngg,'   too many spectra in gate'
	        return
	      endif
	      gates(2*ngg+cside)=-1
	      gates(2*ngg+gside)=iaddr
	      ngg=ngg+1
	    end do
	    ich1=igate(1,nn)
	    ich2=igate(2,nn)
	    xgate=(egate(1,nn)+egate(2,nn))/2
	    write(6,'(i6,f12.1,'' keV'',5x,2i6,''  ('',i1,'')'')') nn,xgate,ich1,ich2,gside
	    if(.not.cmt_readspecs(%val(cmt),gates,ngg,speccut)) return
	    do ii=0,cres-1
	      cut(ii)=cut(ii)+speccut(ii)
	      e2cut(ii)=e2cut(ii)+abs(speccut(ii))
	    end do
	  end do
	  do ii=0,cres-1
	     cut(ii)=cut(ii)/spec_divfac
	     e2cut(ii) = e2cut(ii)/spec_divfac
	  enddo
	  if(.not.backsub)goto 50
	  autobg.gsc=0.0
	  autobg.gbc=0.0
	  do ii=0,cres-1
	    autobg.sp(ii)=cut(ii)
	    autobg.gsc=autobg.gsc+cut(ii)
	  enddo
	  do nn=1,ngates
	    do ii=igate(1,nn),igate(2,nn)
	      autobg.gbc=autobg.gbc+autobg.bp(ii,gside)
	    enddo
	  enddo
	  call autobgmin(autobg.sp(0),autobg.bg(0),cres,1,cres,autobg.m,autobg.itmax,autobg.fstep)
	  autobg.gsc=autobg.gsc/autobg.tot
	  autobg.gbc=autobg.gbc/autobg.tot
	  do ii=0,cres-1
	    autobg.rr=sqrt(4.0*autobg.bg(ii)*spec_divfac+1.96)+1.4
	    autobg.rr=1-4.0*autobg.bg(ii)/autobg.rr/autobg.rr*spec_divfac
	    autobg.bg(ii)=autobg.bg(ii)+autobg.gsc*autobg.rr*autobg.bp(ii,cside)
	  enddo
	  do ii=0,cres-1
	    autobg.bg(ii)=autobg.bg(ii)+(autobg.p(ii,cside)-autobg.bp(ii,cside))*autobg.gbc
	    e2cut(ii)=e2cut(ii)+abs(autobg.bg(ii))
	    cut(ii)=cut(ii)-autobg.bg(ii)
	    autobg.sp(ii)=0.00000
	    autobg.mark(ii)=.false.
	    if(cut(ii)+sqrt(e2cut(ii))*(1.00+2.00/(0.10+e2cut(ii))) .le. 0.0000)then
	      if(ii.gt.0)then 
	        if(.not. autobg.mark(ii-1))autobg.sp(ii-1)=cut(ii-1)+sqrt(e2cut(ii-1))
	      endif
	      autobg.sp(ii)=-cut(ii)
	      autobg.mark(ii)=.true.
	    else
	      if(ii.gt.0)then 
	        if(autobg.mark(ii-1))autobg.sp(ii)=cut(ii)+sqrt(e2cut(ii))
	      endif
	    endif
	  enddo
	  r_sum=0
	  do ii=1,cres-2
	    autobg.bg(ii)=(autobg.sp(ii-1)/2.000+autobg.sp(ii)+autobg.sp(ii+1)/2.000)/2.000
	    if(autobg.bg(ii) .gt.autobg.sp(ii))autobg.bg(ii)=autobg.bg(ii)*0.25+autobg.sp(ii)*0.75
	    if(autobg.bg(ii) .le. 0.000)autobg.bg(ii)=0.000
	    if(autobg.mark(ii))then 
	       cut(ii)=cut(ii)+autobg.bg(ii)
	       e2cut(ii)=e2cut(ii)+autobg.bg(ii)
	       if( cut(ii)+sqrt(e2cut(ii))*(1.00+2.00/(0.10+e2cut(ii))) .lt. 0.00000 )then
	           cut(ii) = cut(ii)+sqrt(e2cut(ii))/2.000
		   e2cut(ii) = e2cut(ii)*1.50000
	       endif
	    endif
	  enddo
	  do ii=0,cres-1
	     cut(ii)=cut(ii)*spec_divfac
	     e2cut(ii) = e2cut(ii)*spec_divfac
	  enddo
	  goto 50
	endif
	

	if(.not.backcom .and. .not. autobg.yes) then		!! normal background subtraction
	  itp1=igate(2,1) - igate(1,1) +1
	  itp2=0
	  do jj=2,ngates
	    itp2=itp2+ igate(2,jj) - igate(1,jj) +1
	  end do
	  backfac=1.
	  if(itp2.gt.0) backfac=float(itp1)/float(itp2)
	  do nn=1,ngates
	    ngg=0
	    do iaddr=igate(1,nn),igate(2,nn)
	      if(ngg.ge.nggmax) then
	        write(6,*) nn,ngg,'   too many spectra in gate'
	        return
	      endif
	      gates(2*ngg+cside)=-1
	      gates(2*ngg+gside)=iaddr
	      ngg=ngg+1
	    end do
	    ich1=igate(1,nn)
	    ich2=igate(2,nn)
	    xgate=(egate(1,nn)+egate(2,nn))/2
	    if(nn.eq.1) then
	      write(6,'(i6,f12.1,'' keV'',5x,2i6,''  ('',i1,'')'')') 1,xgate,ich1,ich2,gside
	    else
	      write(6,'(i6,f12.1,'' keV'',5x,2i6,''  ('',i1,'')'',f13.5)') nn,xgate,ich1,ich2,gside,-backfac
	    endif
	    if(.not.cmt_readspecs(%val(cmt),gates,ngg,speccut)) return
	    if(nn.eq.1) then
	      do ii=0,cres-1
	        cut(ii)=cut(ii)+speccut(ii)
	        e2cut(ii)=e2cut(ii)+abs(speccut(ii))
	      end do
	    else
	      do ii=0,cres-1
		back(ii)=back(ii)+speccut(ii)
	        e2back(ii)=e2back(ii)+abs(speccut(ii))
	      end do
	    endif
	  end do
	  backfac2=backfac**2
	  do ii=0,cres-1
	    backii=int(2.00*back(ii)*backfac +0.5)
	    cut(ii)=cut(ii)-backii/2.00
*	    cut(ii)=cut(ii)-back(ii)*backfac
	    e2cut(ii)= e2cut(ii) + e2back(ii)*backfac2
	  end do
	  goto 50
	endif

	if(backpro) then		!! Common background using projection(s)
	  pbacks=0.
	  do nn=1,ngates
	    ngg=0
	    do iaddr=igate(1,nn),igate(2,nn)
	      pbacks=pbacks+sback(iaddr,gside)
	      if(ngg.ge.nggmax) then
	        write(6,*) nn,ngg,'   too many spectra in gate'
	        return
	      endif
	      gates(2*ngg+cside)=-1
	      gates(2*ngg+gside)=iaddr
	      ngg=ngg+1
	    end do
	    ich1=igate(1,nn)
	    ich2=igate(2,nn)
	    xgate=(egate(1,nn)+egate(2,nn))/2
	    write(6,'(i6,f12.1,'' keV'',5x,2i6,''  ('',i1,'')'')') nn,xgate,ich1,ich2,gside
	    if(.not.cmt_readspecs(%val(cmt),gates,ngg,speccut)) return
	    do ii=0,cres-1
	      cut(ii)=cut(ii)+speccut(ii)
	      e2cut(ii)=e2cut(ii)+abs(speccut(ii))
	    end do
	  end do
	  pfacs=pbacks/sbacktot(gside)*corrback(cside)
	  pfacs2=pfacs**2
	  do ii=0,cres-1
	    backii=int(sback(ii,cside)*pfacs+0.5)
	    cut(ii)=cut(ii)-backii
	    e2cut(ii)=e2cut(ii) + abs(backii)*pfacs2
	  end do
	else				!! Background alla Palameta & Waddington
	  pbackr=0
	  pbacks=0
	  do jj=1,ngates
	    ngg=0
	    do iaddr=igate(1,nn),igate(2,nn)
	      pbackr=pbackr+rback(iaddr,gside)
	      pbacks=pbacks+sback(iaddr,gside)
	      if(ngg.ge.nggmax) then
	        write(6,*) nn,ngg,'   too many spectra in gate'
	        return
	      endif
	      gates(2*ngg+cside)=-1
	      gates(2*ngg+gside)=iaddr
	      ngg=ngg+1
	    end do
	    ich1=igate(1,nn)
	    ich2=igate(2,nn)
	    xgate=(egate(1,nn)+egate(2,nn))/2
	    write(6,'(i6,f12.1,'' keV'',5x,2i6,''  ('',i1,'')'')') nn,xgate,ich1,ich2,gside
	    if(.not.cmt_readspecs(%val(cmt),gates,ngg,speccut)) return
	    do ii=0,cres-1
	      cut(ii)=cut(ii)+speccut(ii)
	      e2cut(ii)=e2cut(ii)+abs(speccut(ii))
	    end do
	  end do
	  pfacr=pbackr/rbacktot(gside)
	  pfacs=pbacks/rbacktot(cside)
	  pfacf=pbacks/rbacktot(gside)/rbacktot(cside)*rbacktot12
	  pfacr2=pfacr**2
	  pfacs2=pfacs**2
	  pfacf2=pfacf**2
	  do ii=0,cres-1
	    backii =  sback(ii,cside)*pfacr
	1	    + rback(ii,cside)*pfacs
	1	    - sback(ii,cside)*pfacf
	    backii=int(backii+0.5)
	    cut(ii)=cut(ii)-backii
	    e2cut(ii)=e2cut(ii)	+ abs(sback(ii,cside))*pfacr2
	1			+ abs(rback(ii,cside))*pfacs2
	1			+ abs(sback(ii,cside))*pfacf2
	  end do
	endif
	goto 50


40	if(mdim.eq.3) then
	  nggmax=MAXNGG/3
***************** Autobg 3D
	 if(backsub .and. .not.backcom .and. autobg.yes) then
	  ngroups=1
	  igfound(1)=gclass(1)
	  kk=1
	  putit=.true.
	  do while(putit)
	   do jj=1,ngates
	    if( gclass(jj) .eq. igfound(ngroups))then
	       kk=kk+1
	       gclass(jj)=-(gclass(jj)+1)
	    endif
	   end do
	   jj=1
	   do while( (gclass(jj) .lt. 0) .and. (jj .lt. ngates) )
	     jj=jj+1
	   end do
	   
	   if(gclass(jj) .ge. 0)then
	     ngroups=ngroups+1
	     igfound(ngroups)=gclass(jj)
	   endif
	   
	   if(kk-1 .eq. ngates)putit=.false.
	 enddo

	 fnorm=0.0000
	 do ii=1,ngates
	    gclass(ii)=-(gclass(ii)+1)
	 enddo
	    
	  ttbg=0
	  do ii=0,cres-1
	    autobg.dbg(ii)=0.00000000
	    ttbg=ttbg+autobg.bp(ii,1)
	    if(autobg.p(ii,1) .gt. fnorm)fnorm=autobg.p(ii,1)
	  end do
	  ttbg=ttbg/autobg.tot
	  
*	  do nn1=1,ngates
	  do nn_gr=1,ngroups
	    do ii=0,cres-1
	      autobg.sp(ii)=0.00000000
	      autobg.wgh(ii)=0.00000000
	      specback(ii)=0
	    end do
	    total_bg=0.0000000
	    wgh=0.000000000
	    do ii_gr=1,ngates
	     if(gclass(ii_gr).eq.igfound(nn_gr))then
	      nn1=ii_gr
	      nggb=0
*	      nn=igate(2,nn1)-igate(1,nn1)+1
*	      fff=nggmax
*	      fff=fff/9.000/nn
*	      kutstep=fff-1
*	      kutstep=min(kutstep,mstep(1))
*	      kutstep=2*mstep(1)
	     
	     if(autobg.useproj) call cmt_readspec_reset
	     do iaddr1 = max(igate(1,nn1), 0),min(igate(2,nn1), cres-1)
	      if(autobg.useproj) then
	        nggb=1
		gates(cside)=-1
		gates(gside)=iaddr1
	        if(.not.cmt_readspecs(%val(autobg.cmt),gates,nggb,specback)) return
	      else		
	       nggb=0
	       call cmt_readspec_reset
	        do iaddr2= 0, cres-1      
	          if(nggb.ge.nggmax) then
	            write(6,*) nn1,nn2,nggb,'   too many spectra in gate'
	            return
	          endif
	          gates(3*nggb+1)=-1
	          gates(3*nggb+2)=iaddr1
	          gates(3*nggb+3)=iaddr2
	          nggb=nggb+1
	       end do
	       if(.not.cmt_readspecs(%val(cmt),gates,nggb,specback)) return
	      endif
#ifdef Digital
	       write(6,'(52(''\b''),6i8,'' b'',$)')igate(1,nn1),igate(2,nn1), nn1,gclass(nn1),iaddr1,nggb
#else
	       write(6,'(a3,6i8,'' b'')')GO_UP_ONE_LINE,igate(1,nn1),igate(2,nn1), nn1,gclass(nn1),iaddr1,nggb
#endif
	      call flush(6)
	      if( autobg.p(iaddr1,1) .gt. 0)then
	         rr=autobg.bp(iaddr1,1)/autobg.p(iaddr1,1)
		 fff=fnorm*( (1-rr)+rr*(rr-0.5000) )*(1.0000-ttbg)
		 wgh=wgh+fff
		 ppp=(1.000-autobg.bp(iaddr1,1)/autobg.p(iaddr1,1))/autobg.p(iaddr1,1)
	      else
	         fff=0
		 ppp=0
	      endif
	      do ii=0,cres-1
	        rr=specback(ii)
	        autobg.sp(ii)=autobg.sp(ii)+ppp*rr
	        autobg.wgh(ii)=autobg.wgh(ii)+fff*rr
		total_bg=total_bg+specback(ii)
		specback(ii)=0
	      end do
	     end do
	    endif
	   end do
*	      call autobgmin(autobg.sp(0),autobg.bg(0),cres,1,cres,autobg.m,autobg.itmax,autobg.fstep)
	      call autobgmin(autobg.wgh(0),autobg.bwgh(0),cres,1,cres,autobg.m,autobg.itmax,autobg.fstep)
	      fact_bg = 0.000000
	      autobg.gsc=0.000000
	      autobg.gbc=0.000000
	      if(spherical) then
	        centro(1)=(cgate(1,nn1) + cgate(2,nn1))/2.
	        asse  (1)= cgate(2,nn1)+0.5 - centro(1)
	      endif
	      do ii=1,ngates
	        if( ii .ne. nn1) then
	         rrr_tot=0.000000
	         rrr_in=0.000000

	         if(spherical) then
	           centro(2)=(cgate(1,ii) + cgate(2,ii))/2.
	           asse  (2)= cgate(2,ii)+0.5 - centro(2)
		   do iaddr2=igate(1,ii),igate(2,ii)
	             distn2(2)=((iaddr2+0.5-centro(2))/asse(2))**2 
	             do iaddr1=igate(1,nn1),igate(2,nn1)
	              distn2(1)=((iaddr1+0.5-centro(1))/asse(1))**2 
	              dist2=distn2(1)+distn2(2)
		      rrr_tot=rrr_tot+1
	              if(dist2.le.1)rrr_in=rrr_in+1
		     enddo
		   enddo
		   fact_bg=rrr_in/rrr_tot
		  else
		   fact_bg=1.0000
	          endif

		 if( gclass(nn1) .ne. gclass(ii) )then
		  do jj= igate(1,ii),igate(2,ii)
		    autobg.gbc=autobg.gbc+autobg.bwgh(jj)*fact_bg/wgh
		  end do
		 endif
		endif
	      end do
	      fact_bg = autobg.gbc
	      autobg.gbc=0.000000

	     do ii_gr=1,ngates
	      if( gclass(ii_gr) .eq. igfound(nn_gr) )then
	       nn1=ii_gr
	       do ii=igate(1,nn1),igate(2,nn1)
		if(autobg.p(ii,1) .gt. 0)then
		   rr=autobg.bp(ii,1)/autobg.p(ii,1)
		   rr=(rr-0.50000)*rr
		   autobg.gbc=autobg.gbc+rr
		endif
	       end do
	      endif
	     end do
	      do ii=0,cres-1
	        autobg.dbg(ii)=autobg.dbg(ii)+( autobg.sp(ii)+
     &	                         autobg.gbc*autobg.p(ii,1)/autobg.tot )*fact_bg
	      end do
	  end do
	  call autobgmin(autobg.dbg(0),autobg.bg(0),cres,1,cres,autobg.m,autobg.itmax,autobg.fstep)
	  do ii=0,cres-1
	    autobg.dbg(ii)=autobg.dbg(ii)-autobg.bg(ii)
	  enddo
	  call cmt_readspec_reset
	 endif
****************** End Autobg 3D
#ifdef Digital
	      write(6,'(54(''\b''),54('' ''),$)')
	      write(6,'(54(''\b''),$)')	      
#else
	      write(6,'(a3,54('' ''))')GO_UP_ONE_LINE
#endif	  
	  do nn1=1,ngates-1
	  do nn2=nn1+1,ngates

	    if(backsub .and. .not.backcom .and. .not.autobg.yes) then
	      nggb=0
	      nn=igate(2,nn2)-igate(1,nn2)+1
	      do iaddr2=max(igate(1,nn2)-nn , 0),min(igate(2,nn2)+nn , cres-1)
	        nn=igate(2,nn1)-igate(1,nn1)+1
	        do iaddr1=max(igate(1,nn1)-nn , 0),min(igate(2,nn1)+nn , cres-1)
	          if(nggb.ge.nggmax) then
	            write(6,*) nn1,nn2,nggb,'   too many spectra in gate'
	            return
	          endif
	          gates(3*nggb+1)=-1
	          gates(3*nggb+2)=iaddr1
	          gates(3*nggb+3)=iaddr2
	          nggb=nggb+1
	        end do
	      end do
#ifdef Digital
	      write(6,'(44(''\b''),3i8,'' b'',$)') nn1,nn2,nggb
#else
	      write(6,'(a3,3i8,'' b'')') GO_UP_ONE_LINE, nn1,nn2,nggb
#endif
	      call flush(6)
	      if(.not.cmt_readspecs(%val(cmt),gates,nggb,specback)) return
	    endif
	    
	    if( autobg.yes .or. (.not. backsub) )then
	        if( gclass(nn1) .eq. gclass(nn2) ) goto 2010
	    endif


	    nggp=0
	    naa=0
	    if(spherical) then
	      centro(1)=(cgate(1,nn1) + cgate(2,nn1))/2.
	      centro(2)=(cgate(1,nn2) + cgate(2,nn2))/2.
	      asse  (1)= cgate(2,nn1)+0.5 - centro(1)
	      asse  (2)= cgate(2,nn2)+0.5 - centro(2)
	    endif

	    do iaddr2=igate(1,nn2),igate(2,nn2)
	      if(spherical) distn2(2)=((iaddr2+0.5-centro(2))/asse(2))**2 
	      do iaddr1=igate(1,nn1),igate(2,nn1)
	        naa=naa+1
	        if(spherical) then
	          distn2(1)=((iaddr1+0.5-centro(1))/asse(1))**2 
	          dist2=distn2(1)+distn2(2)
	          putit=dist2.le.1
	        else
	          putit=.true.
	        endif
	        if(putit) then
	          if(nggp.ge.nggmax) then
	            write(6,*) nn1,nn2,nggp,'   too many spectra in gate'
	            return
	          endif
	          gates(3*nggp+1)=-1
	          gates(3*nggp+2)=iaddr1
	          gates(3*nggp+3)=iaddr2
	          nggp=nggp+1
	        endif
	      end do
	    end do
#ifdef Digital
	    write(6,'(34(''\b''),4i8,$)') nn1,nn2,naa,nggp
#else
	    write(6,'(a3,4i8)') GO_UP_ONE_LINE, nn1,nn2,naa,nggp
#endif     
	    call flush(6)
	    if(.not.cmt_readspecs(%val(cmt),gates,nggp,speccut)) return

	    if(backsub .and. .not.backcom .and. .not.autobg.yes) then
	      do ii=0,cres-1
	        back(ii)=specback(ii)-speccut(ii)
	      end do
	      nggb=nggb-nggp
	      pfacs=float(nggp)/float(nggb)
	      pfacs2=pfacs*pfacs
	      do ii=0,cres-1
	        cut(ii)  =cut(ii)   +     (speccut(ii)- back(ii)*pfacs )
	        e2cut(ii)=e2cut(ii) + abs((speccut(ii)+ back(ii)*pfacs2))
	      end do
	    elseif(backsub .and. backcom .and. .not.autobg.yes)then
	      do ii=0,cres-1
	        cut(ii)  =cut(ii)   +     speccut(ii)
	        e2cut(ii)=e2cut(ii) + abs(speccut(ii))
	      end do
	    elseif(.not.backsub )then
	      do ii=0,cres-1
	        cut(ii)  =cut(ii)   +     speccut(ii)
	        e2cut(ii)=e2cut(ii) + abs(speccut(ii))
	      end do
	    elseif(backsub .and. .not.backcom .and. autobg.yes)then
	      do ii=0,cres-1
	        cut(ii)  =cut(ii)   +     speccut(ii)
	        e2cut(ii)=e2cut(ii) + abs(speccut(ii))
	      end do
	    endif
2010	    continue
	  end do
	  end do
	  write(6,*)
*********** Autobg 3D 3D
	 if(backsub .and. .not.backcom .and. autobg.yes)then
	  do ii=0,cres-1
	    autobg.sp(ii)=cut(ii)
	  enddo
	  call autobgmin(autobg.sp(0),autobg.bg(0),cres,1,cres,autobg.m,autobg.itmax,autobg.fstep)
	  do ii=0,cres-1
	    e2cut(ii)=e2cut(ii)+abs(autobg.dbg(ii))+abs(autobg.bg(ii))
	    cut(ii)=cut(ii)-autobg.dbg(ii)-autobg.bg(ii)
	    autobg.sp(ii)=0.00000
	    autobg.mark(ii)=.false.
	    if(cut(ii)+sqrt(e2cut(ii))*(1.00+2.00/(0.10+e2cut(ii))) .le. 0.0000)then
	      if(ii.gt.0)then 
	        if(.not. autobg.mark(ii-1))autobg.sp(ii-1)=cut(ii-1)+sqrt(e2cut(ii-1))
	      endif
	      autobg.sp(ii)=-cut(ii)
	      autobg.mark(ii)=.true.
	    else
	      if(ii.gt.0)then 
	        if(autobg.mark(ii-1))autobg.sp(ii)=cut(ii)+sqrt(e2cut(ii))
	      endif
	    endif
	  enddo
	  r_sum=0
	  do ii=1,cres-2
	    autobg.bg(ii)=(autobg.sp(ii-1)/2.000+autobg.sp(ii)+autobg.sp(ii+1)/2.000)/2.000
	    if(autobg.bg(ii) .gt.autobg.sp(ii))autobg.bg(ii)=autobg.bg(ii)*0.25+autobg.sp(ii)*0.75
	    if(autobg.bg(ii) .le. 0.000)autobg.bg(ii)=0.000
	    if(autobg.mark(ii))then 
	       cut(ii)=cut(ii)+autobg.bg(ii)
	       e2cut(ii)=e2cut(ii)+autobg.bg(ii)
	       if( cut(ii)+sqrt(e2cut(ii))*(1.00+2.00/(0.10+e2cut(ii))) .lt. 0.00000 )then
	           cut(ii) = cut(ii)+sqrt(e2cut(ii))/2.000
		   e2cut(ii) = e2cut(ii)*1.50000
	       endif
	    endif
	  enddo
	  goto 50
	  endif
********** End Autobg 3D 3D

	elseif(mdim.eq.4) then
	  nggmax=MAXNGG/4
	  do nn1=1,ngates-2
	  do nn2=nn1+1,ngates-1
	  do nn3=nn2+1,ngates

	    if(backsub .and. .not.backcom) then
	      nggb=0
	      nn=max(1,(igate(2,nn3)-igate(1,nn3)+1)/2)
	      do iaddr3=max(igate(1,nn3)-nn , 0),min(igate(2,nn3)+nn , cres-1)
	        nn=igate(2,nn2)-igate(1,nn2)+1
	        do iaddr2=max(igate(1,nn2)-nn , 0),min(igate(2,nn2)+nn , cres-1)
	          nn=igate(2,nn1)-igate(1,nn1)+1
	          do iaddr1=max(igate(1,nn1)-nn , 0),min(igate(2,nn1)+nn , cres-1)
	            if(nggb.ge.nggmax) then
	              write(6,*) nn1,nn2,nn3,nggb,'   too many spectra in gate'
	              return
	            endif
	            gates(4*nggb+1)=-1
	            gates(4*nggb+2)=iaddr1
	            gates(4*nggb+3)=iaddr2
	            gates(4*nggb+4)=iaddr3
	            nggb=nggb+1
	          end do
	        end do
	      end do
	      write(6,'(4i8,'' b'')') nn1,nn2,nn3,nggb
	      if(.not.cmt_readspecs(%val(cmt),gates,nggb,specback)) return
	    endif

	    nggp=0
	    naa=0
	    if(spherical) then
	      centro(1)=(cgate(1,nn1) + cgate(2,nn1))/2.
	      centro(2)=(cgate(1,nn2) + cgate(2,nn2))/2.
	      centro(3)=(cgate(1,nn3) + cgate(2,nn3))/2.
	      asse  (1)= cgate(2,nn1)+0.5 - centro(1)
	      asse  (2)= cgate(2,nn2)+0.5 - centro(2)
	      asse  (3)= cgate(2,nn3)+0.5 - centro(3)
	    endif
	    do iaddr3=igate(1,nn3),igate(2,nn3)
	      if(spherical) distn2(3)=((iaddr3+0.5-centro(3))/asse(3))**2 
	      do iaddr2=igate(1,nn2),igate(2,nn2)
	        if(spherical) distn2(2)=((iaddr2+0.5-centro(2))/asse(2))**2 
	        do iaddr1=igate(1,nn1),igate(2,nn1)
	          naa=naa+1
	          if(spherical) then
	            distn2(1)=((iaddr1+0.5-centro(1))/asse(1))**2 
	            dist2=distn2(1)+distn2(2)+distn2(3)
	            putit=dist2.le.1
	          else
	            putit=.false.
	          endif
	          if(putit) then
	            if(nggp.ge.nggmax) then
	              write(6,*) nn1,nn2,nn3,nggp,'   too many spectra in gate'
	              return
	            endif
	            gates(4*nggp+1)=-1
	            gates(4*nggp+2)=iaddr1
	            gates(4*nggp+3)=iaddr2
	            gates(4*nggp+4)=iaddr3
	            nggp=nggp+1
	          endif
	        end do
	      end do
	    end do
	    write(6,'(8i8)') nn1,nn2,nn3,naa,nggp
	    if(.not.cmt_readspecs(%val(cmt),gates,nggp,speccut)) return

	    if(backsub .and. .not.backcom) then
	      do ii=0,cres-1
	        back(ii)=specback(ii)-speccut(ii)
	      end do
	      nggb=nggb-nggp
	      pfacs=float(nggp)/float(nggb)
	      pfacs2=pfacs*pfacs
	      do ii=0,cres-1
	        cut(ii)  =cut(ii)   +     (speccut(ii)- back(ii)*pfacs )
	        e2cut(ii)=e2cut(ii) + abs((speccut(ii)+ back(ii)*pfacs2))
	      end do
	    else
	      do ii=0,cres-1
	        cut(ii)  =cut(ii)   +     speccut(ii)
	        e2cut(ii)=e2cut(ii) + abs(speccut(ii))
	      end do
	    endif

	  end do
	  end do
	  end do

	else
	  goto 100
	endif

	if(backsub) then
	  if(backcom) then
	    pbacks=0.
	    do ii=0,cres-1
	      pbacks=pbacks+cut(ii)
	    enddo
	    pfacs=pbacks/sbacktot(gside)*corrback(gside)
	    pfacs2=pfacs*pfacs
	    do ii=0,cres-1
	      backii=int(sback(ii,cside)*pfacs+0.5)
	      cut(ii)=cut(ii)-backii
	      e2cut(ii)=e2cut(ii) + abs(backii)*pfacs2
	    end do
	  else
	    do ii=0,cres-1
	      cut(ii)=int(sign(abs(cut(ii))+0.5,cut(ii)))
	    end do
	  endif
	endif

50	iscut=.true.
	isproje=.false.
	nkap=(cres+1023)/1024

	ika=0
	ikl=cres
	do ii=ika,ikl-1
	  spek(ii)=cut(ii)/spec_divfac
	  e2=e2cut(ii)/spec_divfac
	  if(e2.eq.0) e2=1.
	  err2(ii)=e2
	end do
	do ii=cres,1024*nkap-1
	  spek(ii)=0
	  err2(ii)=1
	end do
	
	if( nkap .lt. 64 ) then
	   do i = 1024*nkap, 1024*(nkap+1)-1
	      spek(ii) = 0
	      err2(ii) = 1
	   enddo
	endif

**eff	     if(efficor) then
**eff	       do ii=ika,ikl-1
**eff		 spek(ii)=spek(ii)*reffi(ii,cside)
**eff		 err2(ii)=err2(ii)*reffi(ii,cside)*reffi(ii,cside)
**eff	       end do
**eff	     endif

	iok=1
	return

100	call xtpbell
	iok=0
	return

	END

	SUBROUTINE WbackESEGUI(wspek,iok)

#include "trackn.inc"

	REAL    WSPEK(0:MAXRES-1)
	integer gvec(2)
	INTEGER SPECCUT(0:MAXRES-1)


	if(.not.cmatrix) goto 100

	write(6,*)

	DO II=0,MAXRES-1
	  cut(II)=0
	end do

	gvec(cside)=-1

	DO ind=0,mres(cside)-1
	  datproj=mproj(ind,gside)
	  datback=wspek(ind)
	  errproj=sqrt(abs(datproj))
	  errback=sqrt(abs(datback))
	  if((datproj-errproj).le.(datback+errback)) then
	    gvec(gside)=ind
	    if(.not.cmt_readspec(%val(cmt),gvec,speccut)) then
	      write(6,*) 'Error from cmt_readspec at',ind
	      call exit(0)
	    endif
	    write(6,'(1H+,i)') ind
	    DO II=0,mres(cside)-1
	      cut(II)=cut(II)+speccut(II)
	    end do
	  endif
	end do
	nkap=(mres(cside)+1023)/1024
	ika=0
	ikl=mres(cside)
	do ii=ika,ikl-1
	  wspek(ii)=cut(ii)
	end do
	do ii=mres(cside),1024*nkap-1
	  wspek(ii)=0
	end do

	iok=1
	return

100	call xtpbell
	iok=0
	return

	END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	SUBROUTINE SFONDEFINE

#include "trackn.inc"

	character*50 wfonfile,smoothfile,backcutfile
	logical*1 logtemp
	real wspek(0:MAXRES-1)
	integer lunit	/0/
	integer nsmoo	/2/
	real fwfact	/5./

	call xtptermfocus

	if(nwfon.gt.1) goto 50
	logtemp=.false.
	lnch=inp_ask('Want to read smooth background points from file',logtemp)
	if(lnch.eq.-1) return
	if(logtemp) then
	  lnch=INP_CH('Filename',wfonfile)
	  if(lnch.eq.-1) return
	  lwfonfile=lengthc(wfonfile)
	  if(lwfonfile.le.0) goto 120
	  if(lunit.le.0) call lib$get_lun(lunit)
	  open(unit=lunit,file=wfonfile,status='OLD',err=120)
	  do while(nwfon.lt.(maxres-1))
	    read(lunit,*,end=20,err=119) wfonx(nwfon),wfony(nwfon)
	    wfonx(nwfon)=min(max(0,int(wfonx(nwfon))),maxres-1)
	    nwfon=nwfon+1
	  end do
20	  close(lunit)
	  logtemp=.true.
	  do while(logtemp)	! li ordina
	    logtemp=.false.
	    do ii=1,nwfon-1
	      if(wfonx(ii).lt.wfonx(ii-1)) then
		ttt=wfonx(ii)
		wfonx(ii)=wfonx(ii-1)
		wfonx(ii-1)=ttt
		ttt=wfony(ii)
		wfony(ii)=wfony(ii-1)
		wfony(ii-1)=ttt
		logtemp=.true.
	      endif
	    end do
	  end do
	  ii=0
	  dowhile(ii.lt.nwfon-1)	! toglie gli uguali
	    if(wfonx(ii).eq.wfonx(ii+1)) then
	      do jj=ii+1,nwfon-1
		wfonx(jj-1)=wfonx(jj)
		wfony(jj-1)=wfony(jj)
	      end do
	      nwfon=nwfon-1
	    else
	      ii=ii+1
	    endif		
	  end do
	endif

	if(nwfon.gt.1) goto 50
	logtemp=.false.
	lnch=inp_ask('Want to read smooth background spectrum',logtemp)
	if(lnch.eq.-1) return
	if(logtemp)then
	  CALL READDAT(1,smoothfile,wspek,NKAP*1024,IFRMI,KV)
	  if(kv.le.0) goto 120
	  nnchan=kv/100
	  IFORMI=mod(kv,100)
	  if(nnchan.gt.maxres) goto 120
	  nwfon=nnchan
	  do ii=0,nwfon-1
	    wfonx(ii)=ii
	    wfony(ii)=wspek(ii)
	  end do
	endif

	if(nwfon.gt.1) goto 50
	logtemp=.false.
	lnch=inp_ask('Want me to produce smooth background spectrum',logtemp)
	if(lnch.eq.-1) return
	if(logtemp)then
	  if(iwcal.ne.1) then
	     write(6,*)
	1	 'To perform this command you should set the FWHM calibration'
	     write(6,*)
	     call wcalibration
	     if(iwcal.ne.1) return
	  endif
	  xx=nsmoo
	  call inp_r2('Width of the smooth region (in FWHM units) ',fwfact,xx)
	  nsmoo=xx+0.5
	  if(fwfact.le.0) fwfact=5
	  do ii=0,MAXRES-1
	    wspek(ii)=0
	  end do

	  wspek(ika)=0
	  do ii=ika+1,ikl-1
	    iwf=pol(float(ii),wcal,m$k)*fwfact/2
	    iwfrom=max(ika+1,ii-iwf)
	    iwto  =min(ikl-1,ii+iwf)
	    wymin1=spek(iwfrom)
	    do jj=iwfrom+1,ii
	      wymin1=min(wymin1,spek(jj))
	    end do
	    wymin2=spek(iwto)
	    do jj=ii,iwto-1
	      wymin2=min(wymin2,spek(jj))
	    end do
	    wspek(ii)=(wymin1+wymin2)/2
	  end do
	  wspek(ikl)=wspek(ikl-1)

	  do jj=1,nsmoo
	    wspek(ika)=0
	    wpre=0
	    do ii=ika+1,ikl-1
	      wnew=(wpre+2*wspek(ii)+wspek(ii+1))/4
	      wpre=wspek(ii)
	      wspek(ii)=wnew
	    end do
	    wspek(ikl)=wspek(ikl-1)
	  end do

	  nwfon=ikl-ika
	  do ii=ika,ikl
	    wfonx(ii)=ii
	    wfony(ii)=wspek(ii)
	  end do
	endif

	if(nwfon.le.1) return

50	do ii=0,maxres-1
	  wspek(ii)=0.
	end do
	xx1=wfonx(0)
	yy1=wfony(0)
	do ii=1,nwfon-1
	  xx2=wfonx(ii)
	  yy2=wfony(ii)
	  delta=(yy2-yy1)/(xx2-xx1)
	  do ixx=int(xx1),int(xx2)
	    wspek(ixx)=yy1+(ixx-xx1)*delta
	  end do
	  xx1=xx2
	  yy1=yy2
	end do
	write(6,*) 'You have defined',nwfon,'   smooth background points'

	logtemp=.false.
	lnch=inp_ask('Want to list them',logtemp)
	if(lnch.eq.-1) return
	if(logtemp) then
	  write(6,*)' List of smooth background points'
	  do ii=0,nwfon-1
	    write(6,'(i6,i,g)') II,INT(WFONX(II)),WFONY(II)
	  end do
	endif

	logtemp=.false.
	lnch=inp_ask('Want to write them as a list',logtemp)
	if(lnch.eq.-1) return
	if(logtemp) then
	  lnch=INP_CH('Filename',WFONFILE)
	  if(lnch.eq.-1) return
	  lwfonfile=lengthc(wfonfile)
	  if(lwfonfile.le.0) goto 120
	  if(lunit.le.0) call lib$get_lun(lunit)
	  open(unit=lunit,file=wfonfile,status='new',err=120)
	  do ii=0,nwfon-1
	    write(lunit,'(i,g)') INT(WFONX(II)),WFONY(II)
	  end do
	  close(lunit)
	endif

	logtemp=.false.
	lnch=inp_ask('Want to write the smooth background spectrum',logtemp)
	if(lnch.eq.-1) return
	if(logtemp) then
	  CALL INP_SPNAME('Filename[|Format:Length]',smoothfile,FRMO,KLO)
	  KLCH=KLO*1024
	  CALL WRITESPEC(smoothfile,WSPEK,FRMO,KLCH,KV)
	  if(kv.le.0) goto 120
	endif

	if(cmatrix) then
	  logtemp=.false.
	  lnch=inp_ask('Want to produce the background cut',logtemp)
	  if(lnch.eq.-1) return
	  if(logtemp) then
	    call wbackesegui(wspek,iok)	
	    if(iok.ne.1) goto 120
	    logtemp=.false.
	    lnch=inp_ask('Want to write the background cut',logtemp)
	    if(lnch.eq.-1) return
	    if(logtemp) then
	      CALL INP_SPNAME('Filename[|Format:Length]',backcutfile,FRMO,KLO)
	      KLCH=KLO*1024
	      CALL WRITESPEC(backcutfile,WSPEK,FRMO,KLCH,KV)
	      if(kv.le.0) goto 120
	    endif
	  endif
	endif

60	iok=1
	return

119	close(lunit)
120	iok=0
	return

	end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine plotspectrum

#include "trackn.inc"
	real twold(4)
	character*50 lasfile 	/'trackn.ps'/
	logical*1 plotit		/.true./
	logical*1 landscape	/.true./
	logical*1 picchi		/.false./
	integer peakpre	/1/
	real px0,px1,py0,py1,xorig,yorig

	real textsizelas1	/3./	! per il frame
	real textsizelas2	/2./	! label picchi
	real textsizelas3	/5./	! scritta1
	real textsizelas4	/5./	! scritta2

	character*80 scritta1,scritta2

	PARAMETER (dimlasx=180)
	PARAMETER (dimlasy=270)

	call las_open(lasfile,laslun)
	if(laslun.le.0) return

	call xtptermfocus

	do ii=1,4
	   twold(ii)=twin(ii)
	end do
	itypold=ityp
	ticxold=ticx
	ticyold=ticy
	ticlold=ticl
	textsizeold=textsize
	chrxold=chrx
	chryold=chry
	islaser=.true.

	write(6,*) 'Dimensioni in mm'
	call inp_ask('Landscape',landscape)
	if(landscape) then
	   xorig=dimlasx
	   yorig=0.
	   call inp_r2('Origin x,y',xorig,yorig)
	   call las_origin(xorig,yorig)
	   call las_landscape
	   px0=twin(1)*dimlasy/1024.
	   px1=twin(2)*dimlasy/1024.
	   py0=twin(3)*dimlasx/768.
	   py1=twin(4)*dimlasx/768.
	else
	   xorig=0.
	   yorig=0.
	   call inp_r2('Origin x,y',xorig,yorig)
	   call las_origin(xorig,yorig)
	   px0=twin(1)*dimlasx/1024.
	   px1=twin(2)*dimlasx/1024.
	   py0=twin(3)*dimlasy/768.
	   py1=twin(4)*dimlasy/768.
	endif

	call inp_r4('x0,x1,y0,y1',px0,px1,py0,py1)
	call put4lw(twin,px0,px1,py0,py1)

	ityp=abs(ityp)		! disabilita la compressione
	ticx=ticx/4.
	ticy=ticy/4.
	ticl=ticl/4.
	if(ticlold.gt.0) ticl=max(1.,ticl)
	call inp_r1('Text size',textsizelas1)
	textsize=textsizelas1
	chrx=textsize*0.75
	chry=textsize
	textang=0

	call disp_setmode(1)

	call tktdisplay(0,1)

	if(npeaks.gt.0) then
	   call inp_ask('Plot peak labels',picchi)
	   if(picchi) then
	      xpre=peakpre
	      call inp_r2('Size, precision',textsizelas2,xpre)
	      peakpre=xpre
	      if(textsizelas2.gt.0) then
		textsize=textsizelas2
		call showpeaks(peakpre)
	      endif
	   endif
	endif

	call inp_str('General comment',scritta1)
	lscritta1=lengthc(scritta1)
	if(lscritta1.gt.0) then
	    call inp_r1('Size',textsizelas3)
	    if(textsizelas3.gt.0) then
		dx=2*textsizelas3
		dy=2*textsizelas1+1.5*textsizelas3
		call disp_text(TWIN(1)+dx,TWIN(3)-dy,
	1		textsizelas3,0,scritta1(1:lscritta1))
	    endif
	endif

	call inp_str('Inner comment',scritta2)
	lscritta2=lengthc(scritta2)
	if(lscritta2.gt.0) then
	    call inp_r1('Size',textsizelas4)
	    if(textsizelas4.gt.0) then
		dx=0.75*textsizelas4*(lscritta2+2)
		dy=textsizelas4*1.5
		call disp_text(TWIN(2)-dx,TWIN(4)-dy,
	1		textsizelas4,0,scritta2(1:lscritta2))
	    endif
	endif

	call disp_setmode(0)		! risistema le cose
	islaser=.false.
	ityp=itypold
	ticx=ticxold
	ticy=ticyold
	ticl=ticlold
	textsize=textsizeold
	chrx=chrxold
	chry=chryold
	do ii=1,4
	   twin(ii)=twold(ii)
	end do
	call disp_setscale(dwin,twin,xsca,ysca)

	call las_close(1)
	write(6,'('' PS output in file : trackn.ps'')')

	if(lplotcom.gt.0) then
	  call inp_ask('Spool the plot file',plotit)
	  if(plotit) then
	    call Lib$Spawn(plotcom(1:lplotcom)//LASFILE)
	    call lib$wait(1.)
	  endif
	endif


	return

	end

	subroutine trackframe

	parameter (nvalid=5)
	integer ivalid(nvalid)
	data ivalid/1,2,4,5,10/
	character string*80
	character*6 nm_char
#include "trackn.inc"

	RXVAL(XX)=MIN(MAX( ((XX-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(YY)=MIN(MAX( ((YY-DY0)*YSCA+TY0) , TY0),TY1)

	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

!!!!!!!!!!!!!!!!!!!!!! asse x in basso  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	if(lxcal.eq.0 .or. iecal.eq.0) then
	   edx0=dx0
	   edx1=dx1
	else
	   edx0=cpol(dx0,ECAL,-m$k)
	   edx1=cpol(dx1,ECAL,-m$k)
	endif
	ddx=abs(edx1-edx0)
	dtx=abs(tx1-tx0)

	nticx=dtx/ticx
	nticx=max(nticx,1)
	dticx=ddx/nticx
	factor=1
	if(dticx.le.1.) then
	   iticx=1
	else
	  do mult=0,8
	    do nv=2,nvalid
		if(dticx.le.factor*ivalid(nv)) then
		   cx1=dticx-factor*ivalid(nv-1)
		   cx2=factor*ivalid(nv)-dticx
		   if(cx1.lt.cx2) then
			iticx=factor*ivalid(nv-1)
		   else					   
			iticx=factor*ivalid(nv)
		   endif
		   goto 20
		endif
	    end do
	    factor=factor*10.
	  end do
	endif

20	Ry=twin(3)
	RRy=Ry-ticl
	do exx=0.,edx1+iticx/2.,iticx
	   if(exx.ge.edx0 .and. exx.le.edx1) then
		if(exx.gt.0) then
		   nch=alog10(exx)+1
		else
		   nch=1
		endif
		if(lxcal.eq.0 .or. iecal.eq.0) then
		   xx=exx
		else
		   xx=cpolinv(exx,ECAL,-m$k)
		endif
		Rx=RXVAL(xx)
		call disp_line(Rx,Ry,Rx,RRy)
		write(nm_char,'(i6)')nch
		write(string,'(i'//nm_char//')') int(exx+0.1)
		RRx=Rx-(nch*chrx)/2
		if(islaser) then
		  call disp_text(RRx,RRy-1.2*chry,textsize,textang,string)
		else
		  call disp_text(RRx,RRy-0.6*chry,textsize,textang,string)
		endif
	   endif
	end do
	llabelx=lengthc(labelx)
	if(llabelx.gt.0) then
	   WRITE(string,'(A)') labelx(1:llabelx)
	   if(islaser) then
	     rx=twin(2)-(llabelx+5)*chrx
	     call disp_text(Rx,RRy-3*chry,textsize,textang,string)
	   else
	     rx=twin(2)-(llabelx+10)*chrx
	     call disp_text(Rx,RRy-1.8*chry,textsize,textang,string)
	   endif
	endif

!!!!!!!!!!!!!!!!!!!!!! asse y a sinistra  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	ddy=abs(dy1-dy0)
	dty=abs(ty1-ty0)

	nticy=dty/ticy
	nticy=max(nticy,1)
	dticy=ddy/nticy
	factor=1
	if(dticy.le.1.) then
	   iticy=1
	else
	  do mult=0,8
	    do nv=2,nvalid
		if(dticy.le.factor*ivalid(nv)) then
		   cy1=dticy-factor*ivalid(nv-1)
		   cy2=factor*ivalid(nv)-dticy
		   if(cy1.lt.cy2) then
			iticy=factor*ivalid(nv-1)
		   else					   
			iticy=factor*ivalid(nv)
		   endif
		   goto 30
		endif
	    end do
	    factor=factor*10.
	  end do
	endif

30	Rx=twin(1)
	do yy=0.,dy1+iticy/2.,iticy
	   if(yy.ge.dy0 .and. yy.le.dy1) then
		if(yy.ne.0.) then
		   nch=alog10(abs(yy))+1
		else
		   nch=1
		endif
		Ry=RYVAL(yy)
		call disp_line(Rx,Ry,Rx-ticl,Ry)
		write(nm_char,'(i6)')nch
		write(string,'(i'//nm_char//')') int(yy+0.1)
		RRx=Rx-ticl-(nch+.5)*chrx
		if(islaser) then
		  call disp_text(RRx,Ry-0.5*chry,textsize,textang,string)
		else
		  call disp_text(RRx,Ry-0.2*chry,textsize,textang,string)
		endif
	   endif
	end do
	if(dy0.ge.0.) return

	Rx=twin(1)
	do yy=0.,dy0-iticy/2.,-iticy
	   if(yy.ge.dy0 .and. yy.le.dy1) then
		if(yy.ne.0.) then
		   nch=alog10(abs(yy))+2
		else
		   nch=1
		endif
		Ry=RYVAL(yy)
		call disp_line(Rx,Ry,Rx-ticl,Ry)
		write(nm_char,'(i6)')nch
		write(string,'(i'//nm_char//')') -int(abs(yy)+0.1)
		RRx=Rx-ticl-(nch+0.5)*chrx
		if(islaser) then
		  call disp_text(RRx,Ry-0.5*chry,textsize,textang,string)
		else
		  call disp_text(RRx,Ry-0.2*chry,textsize,textang,string)
		endif
	   endif
	end do


	return

	end

	SUBROUTINE SHOWPEAKS(IPRE)

#include "trackn.inc"
	character*80 string
	character*6 nm_char

	call disp_setscale(dwin,twin,xsca,ysca)

	DO IP = 1,NPEAKS
	  XPOS=PEAKS(IP)
	  IF(XPOS.GE.NMAX) RETURN
	  IF(XPOS.GT.NMIN) THEN
	    YY=DFSPEK(XPOS)
	    CALL disp_map(XPOS,YY,TX,TY,DWIN,TWIN)

	    TY=MAX( MIN(TY+chry,TWIN(4)-2*chry) , TWIN(3)+3*chry )
	    IF(IECAL.EQ.1) XPOS=cpol(XPOS,ECAL,-m$k)
	    JJ=1
	    IF(XPOS.NE.0) JJ=ALOG10(ABS(xPOS))+1
	    IF(XPOS.LT.0) JJ=JJ+1
	    DX=JJ*chrx/2
	    IF( (TX-DX).GT.TWIN(1) .AND. (TX+DX).LT.TWIN(2) ) THEN
		TY=MAX( MIN(TY+5,TWIN(4)-30) , TWIN(3)+20 )
		IF(IPRE.EQ.1) THEN
		  write(nm_char,'(i6)')jj+3
		  WRITE(string,'(F'//nm_char//'.2)')XPOS
		ELSE
		  write(nm_char,'(i6)')jj
		  WRITE(string,'(I'//nm_char//')') IFIX(XPOS)
		ENDIF
		tekcolor=6
*		call teksetcolor(tekcolor)
		if(islaser) then
		  call disp_text(TX-DX,TY+chry,textsize,textang,string)
		else
		  call disp_text(TX-DX,TY,textsize,textang,string)
		endif
		tekcolor=1
*		call teksetcolor(tekcolor)
	    ENDIF
	  ENDIF
	end do

	RETURN

	END

	SUBROUTINE autoECALIBRATION(iask)

#include "trackn.inc"

	external       pFDER,pFCHI

	integer hash(0:MAXRES-1)
	integer ipene(n$p),iepeak(n$p)
	data ak1min /.2/
	data ak1max /2./
	data ak0max /150./

	nm_sqrt=sqrterm
	sqrterm=.false.

	call peaksearch(0)
	if(npeaks.lt.2) goto 100

	if(iask.eq.1) then
	  call xtptermfocus
	  CALL INPUTRENE
	endif
	if(nrene.lt.2) goto 100

	if(iask.eq.1) then
	  call inp_r3('min,max (kev/chan) ',ak1min,ak1max,ak0max)
	  ak0max=abs(ak0max)
	endif
		
	do ii=0,MAXRES-1
	   hash(ii)=0
	end do
	do ii=1,npeaks
	   ipos=peaks(ii)
	   idpos=max(2.1,cpol(peaks(ii),wcal,-m$k))
	   do jj=max(0,ipos-idpos),min(MAXRES-1,ipos+idpos)
		hash(jj)=ii
	   end do
	   hash(ipos)=ii
	end do

	maxfound=0
	do i1=1,nrene-1
	do i2=nrene,i1+1,-1
	do j1=1,npeaks-1
	do j2=npeaks,j1+1,-1
	  a1=(rene(i2)-rene(i1))/(peaks(j2)-peaks(j1))
	  if(a1.ge.ak1min .and. a1.le.ak1max) then
	    a0=rene(i1)-a1*peaks(j1)
	    if(abs(a0).le.ak0max) then
	      b1=1/a1
	      b0=-a0/a1
	      nfound=0
	      do ii=1,nrene
		ipos=b0+b1*rene(ii)
		if(ipos.ge.0.and.ipos.lt.MAXRES) then
		   if(hash(ipos).ne.0) nfound=nfound+1
		endif
	      end do
	      if(nfound.gt.maxfound) then
		bb0=b0
		bb1=b1
		aa0=a0
		aa1=a1
		i1f=i1
		i2f=i2
		j1f=j1
		j2f=j2
		maxfound=nfound
	      endif
	      if(maxfound.eq.nrene) goto 50
	    endif
	  endif
	end do
	end do
	end do
	end do
	if(maxfound.le.2) goto 100
50	DO II=1,m$k+1
	   ECAL(II)=0.
	end do
	ecal(1)=aa0
	ecal(2)=aa1
	iecal=1
	do ii=1,nrene
	   ipos=bb0+bb1*rene(ii)
	   ipene(ii)=hash(ipos)
	end do
	do ii=1,npeaks
	  isee=0
	  do jj=1,nrene
	    if(ipene(jj).eq.ii) isee=jj
	  end do
	  iepeak(ii)=isee
	end do
	nnpeaks=0
	do ii=1,npeaks
	   if(iepeak(ii).ne.0) then
		nnpeaks=nnpeaks+1
		peaks(nnpeaks)=peaks(ii)
		iepeak(nnpeaks)=iepeak(ii)
	   endif
	end do
	npeaks=nnpeaks
	if(npeaks.eq.2) return

	do ii=1,npeaks
	   xfit(ii)=peaks(ii)
	   dxfit(ii)=1.
	   yfit(ii)=rene(iepeak(ii))
	   dyfit(ii)=drene(iepeak(ii))
	end do
	ncpar=2
	Ncdata=npeaks
	call Curfit(ecal,ncpar,Ncdata,pFDER,pFCHI,kv,0,cchichi)
	return
	
100	call xtpbell
	iecal=0
	sqrterm=nm_sqrt
	return

	end

	SUBROUTINE INSERTWFON(xpos,ypos,in)

#include "trackn.inc"

	ixpos=int(xpos)
	DO II=0,NWFON-1
	   IF(IXPOS.LT.INT(WFONX(II)))THEN
		DO JJ=NWFON-1,II,-1
		   WFONX(JJ+1)=WFONX(JJ)
		   WFONY(JJ+1)=WFONY(JJ)
		end do
		WFONX(II)=XPOS
		WFONY(II)=YPOS
		NWFON=NWFON+1
		IN=II
		RETURN
	   ELSEIF(IXPOS.EQ.INT(WFONX(II)))THEN
		WFONX(II)=XPOS
		WFONY(II)=YPOS
		IN=II
		RETURN
	   ENDIF
	end do
	WFONX(NWFON)=XPOS
	WFONY(NWFON)=YPOS
	IN=NWFON
	NWFON=NWFON+1
	RETURN

	END

	subroutine draw_zeroline

#include "trackn.inc"

	call disp_setscale(dwin,twin,xsca,ysca)
*	call tekvecmode !disp_vecmod
	xx=dwin(1)
	yy=0.
	CALL disp_map(xx,yy,TX,TY,DWIN,TWIN)
	TY=MAX(MIN(TY,TWIN(4)),TWIN(3))
*	call disp_movabs(twin(1),ty)
*	call disp_drwabs(twin(2),ty)
	call disp_line(twin(1),ty,twin(2),ty)
	return

	end



***************************|
*                          |
*                          |
*  N. Marginean            V


	subroutine STRING_STRIP(c)
	
	character*(*) c
	character*80 c1
	
	c1=c
	ll=lengthc(c)
	do i=1,ll
	  c(i:i)=' '
	enddo
	ii=2
	do i=2,ll
	  if(ichar(c1(i:i)) .ne. 32)then
	   c(ii:ii)=c1(i:i)
	   ii=ii+1
	  endif
	enddo
	return
	end 



	real*8 function PK_FUNCT(x,p)
	
#include "trackn.inc"
	real*8 x,p(20),dum,dum1
	
	dum=0
	do ii=1,nmgau
	  dum1=(x-p(2*ii))/p(1)
	  dum=dum+p(2*ii+1)*dexp(-dum1*dum1*0.5000000D0)
	enddo
	pk_funct=dum
	return
	end
	
	real*8 function VW_FUNCT(x,p)
	
#include "trackn.inc"
	real*8 x,p(20),dum,dum1
	
	dum=0
	do ii=0,nmgau-1
	  dum1=(x-fitpar(2*ii+4))/p(2*ii+2)
	  dum=dum+p(2*ii+1)*dexp(-dum1*dum1*0.500000D0)
	enddo
	vw_funct=dum
	return
	end
	


       subroutine CHQFIT(np,x,y,sy,m,p,sp,pl,pu,cf,nimax,
     &  chq,mc,it,funct)

*********************************************************
**       ***** Interface for FUMILI package *****
**       NDIM - abscise space dimension
**       NP - number of data points
**       M - number of parameters
**       X - abscise matrix (NDIM,NP)
**       Y - data vector
**       SY - standard deviation of data
**       P - parameters values
**       SP - as input values,steps for parameter ; as output
**            standard deviation of parameters
**       PL,PU - lower & upper limit allowed for parameters
**       CF - correlation factors
**       NIMAX - maximal number of iterations
**       CHQ - on input, desired precision for fit ;
**             on output,objective function value
**       MC - failure flag ( MC=1 => fit OK! )
**       IT - flag for objective function definition
**             0 ==> CHI-SQUARE
**             1 ==> LOG-LIKELIHOOD
**       FUNCT - fit function, defined as REAL*8 [FUNCT](X,A)
**               X - REAL*8 data vector
**               A - REAL*8 parameter vector
** Obs: Interface changed to fit only 1-dim data; is more convenient 
**      for TRACKN
***************************************************************

	 IMPLICIT REAL*8(A-H,O-Z)
	integer ndim,np,m,nimax,mc,it
	real x(np),y(np),sy(np),p(m),sp(m),pl(m),pu(m),
     &   cf(m),chq


	COMMON /A/A(20)/PL/PL0(20)/AU/AU(20)/AL/AL(20)
	COMMON /NED/NED(2)
*        COMMON /BARI/PARX,NSPA
	COMMON /EXDA/exda(20000)/SIGMA/SIGMA(20)/R/R(20)
*	COMMON /ERROR/ERROR(100)
	EXTERNAL FUNCT

	ndim=1
	n1=2
	n2=2
	n3=nimax
	ned(1)=np
	ned(2)=ndim+2
	do i=1,np
	  j=(i-1)*(ndim+2)+1
	  exda(j)=y(i)
	  j=j+1
	  exda(j)=sy(i)
*	  do j1=1,ndim
	     exda(j+1)=x(i)
*	  enddo
	enddo
	do i=1,m
	 a(i)=p(i)
	 pl0(i)=sp(i)
	 au(i)=pu(i)
	 al(i)=pl(i)
	enddo
	eps=chq

	CALL FUMILI(S,M,N1,N2,N3,EPS,funct,IT,MC)

	do i=1,m
	   p(i)=a(i)
	   sp(i)=sigma(i)
	   cf(i)=r(i)
	enddo
	chq=s
	return
	 END


      SUBROUTINE FUMILI (S,M,N1,N2,N3,EPS,funct,IT,MC )
C     ENTRY FOR CHISQ MINIMISATION                                              
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      COMMON /Z/Z(410)/G/G(20)/A/A(20)/PL/PL0(20)/SIGMA/SIGMA(20)/         
     1PLU/PL(20)/R/R(20)/DA/DA(20)/AU/AMX(20)/AL/AMN(20)/Z0/Z0(410        
     2)/ENDFLG/ENDFLG,NA,INDFLG(5)                                              
      COMMON /NED/NED(2)                                                        
      COMMON /NFREED/NFREED                                                     
      COMMON /PLOT/ MYPLT
      external funct

C-----  THE MAXIMUM RELATIVE PRECISION IN DOUBLE PRECISION ON IBM 370 IS 1.D-16
      DATA RP/1.D-14/                                                           
      INDFLG(3)=it
    1 continue  !IF(IT.GE.0) WRITE(2,84)
      NN2=0                                                                     
      N=M                                                                       
      FIXFLG=0.0D0                                                              
      ENDFLG=0.0D0                                                              
      INDFLG(2)=0                                                               
      IFIX1=0                                                                   
      FI=0.0D0                                                                  
      NN3=0                                                                     
      DO 2 I=1,N                                                                
      R(I)=0.0D0                                                                
      IF(EPS.GT.0.0D0) SIGMA(I)=0.0D0                                           
    2 PL(I)=PL0(I)                                                              
C-----  START NEW ITERATION                                                     
    3 NN1=1                                                                     
      T1=1.0D0                                                                  
C-----  REPEAT ITERATION WITH SMALIER STEP                                      
    4 S=0.0D0                                                                   
      N0=0                                                                      
      DO 7 I=1,N                                                                
      G(I)=0.0                                                                  
      IF(PL0(I)) 7,7,5                                                          
    5 N0=N0+1                                                                   
      IF(PL(I)) 7,7,6                                                           
    6 PL0(I)=PL(I)                                                              
    7 CONTINUE                                                                  
      NN0=N0*(N0+1)/2                                                           
C-----NUMBER OF FREEDOM                                                         
      NFREED=NED(1)-N0                                                          
      IF(NN0.LT.1) GO TO 9                                                      
      DO 8 I=1,NN0                                                              
      Z(I)=0.0D0                                                                
    8 CONTINUE                                                                  
    9 NA=M                                                                      
      INDFLG(1)=0                                                               
C-----  CALCULATE OBJECTIVE FUNCTION                                            
      CALL SGZ(M,S,funct)
      SP=RP*DABS(S)                                                             
      IF(NN0.LT.1) GO TO 11                                                     
      DO 10 I=1,NN0                                                             
      Z0(I)=Z(I)                                                                
   10 CONTINUE                                                                  
   11 IF(NN3) 19,19,12                                                          
   12 IF(NN1-N1) 13,13,19                                                       
   13 T=2.0D0*(S-OLDS-GT)                                                       
      IF(INDFLG(1))16,14,16                                                     
   14 IF(DABS(S-OLDS).LE.SP.AND.-GT.LE.SP) GO TO 19                             
      IF(0.59D0*T+GT) 19,15,15                                                  
   15 T=-GT/T                                                                   
      IF(T-0.25D0) 16,17,17                                                     
   16 T=0.25D0                                                                  
   17 GT=GT*T                                                                   
      T1=T1*T                                                                   
      NN2=0                                                                     
      DO 18 I=1,N                                                               
      IF(PL(I).LE.0.0D0) GO TO 18                                               
      A(I)=A(I)-DA(I)                                                           
      PL(I)=PL(I)*T                                                             
      DA(I)=DA(I)*T                                                             
      A(I)=A(I)+DA(I)                                                           
   18 CONTINUE                                                                  
      NN1=NN1+1                                                                 
      GO TO 4                                                                   
C-----  REMOVE CONTRIBUTION OF FIXED PARAMETERS FROM Z                          
   19 IF(INDFLG(1).EQ.0) GO TO 20                                               
      ENDFLG=-4.0D0                                                             
      GO TO 85                                                                  
   20 K1=1                                                                      
      K2=1                                                                      
      I1=1                                                                      
      DO 30 I=1,N                                                               
      IF(PL0(I)) 30,30,21                                                       
   21 IF(PL(I).EQ.0.0D0) PL(I)=PL0(I)                                           
      IF(PL(I)) 23,23,24                                                        
   22 PL(I)=0.0D0                                                               
   23 K1=K1+I1                                                                  
      GO TO 29                                                                  
   24 IF(A(I).GE.AMX(I).AND.G(I).LT.0.0D0) GO TO 22                             
      IF(A(I).LE.AMN(I).AND.G(I).GT.0.0D0) GO TO 22                             
      DO 28 J=1,I                                                               
      IF(PL0(J)) 28,28,25                                                       
   25 IF(PL(J)) 27,27,26                                                        
   26 Z(K2)=Z0(K1)                                                              
      K2=K2+1                                                                   
   27 K1=K1+1                                                                   
   28 CONTINUE                                                                  
   29 I1=I1+1                                                                   
   30 CONTINUE                                                                  
C-----  INVERT Z                                                                
      I1=1                                                                      
      L=I1                                                                      
      DO 32 I=1,N                                                               
      IF(PL(I)) 32,32,31                                                        
   31 R(I)=Z(L)                                                                 
      I1=I1+1                                                                   
      L=L+I1                                                                    
   32 CONTINUE                                                                  
      N0=I1-1                                                                   
      CALL MCONV(N0)                                                           
      IF(INDFLG(1)) 33,34,33                                                    
   33 INDFLG(1)=0                                                               
      INDFLG(2)=1                                                               
      GO TO 49                                                                  
   34 CONTINUE                                                                  
C-----  CALCULATE THEORETICAL STEP TO MINIMUM                                   
      I1=1                                                                      
      DO 41 I=1,N                                                               
      DA(I)=0.0D0                                                               
      IF(PL(I)) 41,41,35                                                        
   35 L1=1                                                                      
      DO 40 L=1,N                                                               
      IF(PL(L)) 40,40,36                                                        
   36 IF(I1-L1) 37,37,38                                                        
   37 K=L1*(L1-1)/2+I1                                                          
      GO TO 39                                                                  
   38 K=I1*(I1-1)/2+L1                                                          
   39 DA(I)=DA(I)-G(L)*Z(K)                                                     
      L1=L1+1                                                                   
   40 CONTINUE                                                                  
      I1=I1+1                                                                   
   41 CONTINUE                                                                  
C       CHECK FOR PARAMETERS ON BOUNDARY                                        
      AFIX=0.0D0                                                                
      IFIX=0                                                                    
      I1=1                                                                      
      L=I1                                                                      
      DO 47 I=1,N                                                               
      IF(PL(I)) 47,47,42                                                        
C-----THE FOLLOWING STATEMENT WAS MODIFIED                                      
C  42 SIGI=DSQRT(DABS(Z(L)))                                                    
   42 SIGI=DSQRT(S*DABS(Z(L))/DFLOAT(NFREED))                                   
      R(I)=R(I)*Z(L)                                                            
      IF(EPS) 44,44,43                                                          
   43 SIGMA(I)=SIGI                                                             
   44 IF((A(I).LT.AMX(I).OR.DA(I).LE.0.0D0).AND.                                
     1 (A(I).GT.AMN(I).OR.DA(I).GE.0.0D0)) GO TO 46                             
      AKAP=DABS(DA(I)/SIGI)                                                     
      IF(AKAP-AFIX) 46,46,45                                                    
   45 AFIX=AKAP                                                                 
      IFIX=I                                                                    

      IFIX1=I                                                                   
   46 I1=I1+1                                                                   
      L=L+I1                                                                    
   47 CONTINUE                                                                  
      IF(IFIX) 48,50,48                                                         
   48 PL(IFIX)=-1.0D0                                                           
   49 FIXFLG=FIXFLG+1.0D0                                                       
      FI=0.0D0                                                                  
C-----  REPEAT CALCULATION OF THEORETICAL STEP AFTER FIXING EACH PARAMETER      
      GO TO 19                                                                  
C-----  CALCULATE STEP CORRECTION FACTOR                                        
   50 ALAMBD=1.0D0                                                              
      AKAPPA=0.0D0                                                              
      IMAX=0                                                                    
      DO 60 I=1,N                                                               
      IF(PL(I)) 60,60,51                                                        
   51 BM=AMX(I)-A(I)                                                            
      ABI=A(I)+PL(I)                                                            
      ABM=AMX(I)                                                                
      IF(DA(I)) 52,52,53                                                        
   52 BM=A(I)-AMN(I)                                                            
      ABI=A(I)-PL(I)                                                            
      ABM=AMN(I)                                                                
   53 BI=PL(I)                                                                  
      IF(BI-BM) 55,55,54                                                        
   54 BI=BM                                                                     
      ABI=ABM                                                                   
   55 IF(DABS(DA(I))-BI) 58,58,56                                               
   56 AL=DABS(BI/DA(I))                                                         
      IF(ALAMBD-AL) 58,58,57                                                    
   57 IMAX=I                                                                    
      AIMAX=ABI                                                                 
      ALAMBD=AL                                                                 
   58 AKAP=DABS(DA(I)/SIGMA(I))                                                 
      IF(AKAP-AKAPPA) 60,60,59                                                  
   59 AKAPPA=AKAP                                                               
   60 CONTINUE                                                                  
C-----  CALCULATE NEW CORRECTED STEP                                            
      GT=0.0D0                                                                  
      AMB=1.D18                                                                 
      IF(ALAMBD) 62,62,61                                                       
   61 AMB=0.25D0/ALAMBD                                                         
   62 CONTINUE                                                                  
      DO 67 I=1,N                                                               
      IF(PL(I)) 67,67,63                                                        
   63 IF(NN2-N2) 66,66,64                                                       
   64 IF(DABS(DA(I)/PL(I))-AMB) 66,65,65                                        
   65 PL(I)=4.0D0*PL(I)                                                         
      T1=4.0D0                                                                  
   66 DA(I)=DA(I)*ALAMBD                                                        
      GT=GT+DA(I)*G(I)                                                          
   67 CONTINUE                                                                  
C-----  CHECK IF MINIMUM ATTAINED AND SET EXIT MODE                             
      IF(-GT.GT.SP.OR.T1.GE.1.0D0.OR.ALAMBD.GE.1.0D0) GO TO 68                  
      ENDFLG=-1.0D0                                                             
   68 IF(ENDFLG) 85,69,69                                                       
   69 IF(AKAPPA-DABS(EPS)) 70,75,75                                             
   70 IF(FIXFLG) 72,71,72                                                       
   71 ENDFLG=1.0D0                                                              
      GO TO 85                                                                  
   72 IF(ENDFLG) 85,77,73                                                       
   73 IF(IFIX1) 85,85,76                                                        
   74 IF(FI-FIXFLG) 76,76,77                                                    
   75 IF(FIXFLG) 74,76,74                                                       
   76 FI=FI+1.0D0                                                               
      ENDFLG=0.0D0                                                              
   85 IF(ENDFLG.EQ.0.0D0.AND.NN3.GE.N3) ENDFLG=-3.0D0                           
      IF(ENDFLG.GT.0.0D0.AND.INDFLG(2).GT.0) ENDFLG=-2.0D0                      
*      CALL MONIT(S,M,NN3,IT,EPS,GT,AKAPPA,ALAMBD)
*	IF(JJJ1.LT.0)RETURN
      IF(ENDFLG) 83,79,83                                                       
C-----  CHECK IF FIXING ON BOUND IS CORRECT                                     
   77 ENDFLG=1.0D0                                                              
      FIXFLG=0.0D0                                                              
      IFIX1=0                                                                   
      DO 78 I=1,M                                                               
   78 PL(I)=PL0(I)                                                              
      INDFLG(2)=0                                                               
      GO TO 19                                                                  
C-----  NEXT ITERATION                                                          
   79 ENDFLG=0.0D0                                                              
      DO 80 I=1,N                                                               
      A(I)=A(I)+DA(I)                                                           
   80 CONTINUE                                                                  
      IF(IMAX) 82,82,81                                                         
   81 A(IMAX)=AIMAX                                                             
   82 OLDS=S                                                                    
      NN2=NN2+1                                                                 
      NN3=NN3+1                                                                 
      GO TO 3                                                                   
   83 MC=ENDFLG                                                                 
      RETURN                                                                    
C-----  ENTRY FOR MAXIMUM LIKLEHOOD                                             
*      ENTRY LIKEL
*      INDFLG(3)=1
*      GO TO 1
C                                                                               
*   84 FORMAT(////,20X,'FUNCTION MINIMISATION BY SUBROUTINE *** MDF ***
*     1.'/1H,25X,'IN THE FOLLOWING PRINT-OUT'/1H,3X,'S = VALUE OF OBJE
*     2CTIVE FUNCTION,  EC = EXPECTED CHANGE IN S DURING NEXT ITERATION,'
*     3/1H,3X,'KAPPA = ESTIMATED DISTANCE TO MINIMUM,  LAMBDA = STEP LE
*     4NGTH MODIFIER'///)
	 END

      SUBROUTINE SGZ(M,S,funct)
C-----  SUBROUTINE SGZ SETS UP S(OBJECTIVE FUNCTION), G(GRADIENT OF S) AND      
C-----  Z(APPROXIMATE COVARIANCE MATRIX)                                        
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      COMMON /G/G(20)/Z/Z(410)/A/A(20)/DF/DF(20)/NED/NED(2)/EXDA/           
     1exda(20000)/X/X(10)/PL/PL(20)/ENDFLG/ENDFLG,NA,INDFLG(5)
      external funct
      K=NED(1)                                                                  
      K2=1                                                                      
      DO 12 L1=1,K                                                              
      K1=K2                                                                     
      NX=NED(2)-2                                                               
      IF(INDFLG(3)) 1,2,1                                                       
    1 NX=NED(2)                                                                 
      K1=K1-2                                                                   
    2 CONTINUE                                                                  
      DO 3 I=1,NX                                                               
      KI=K1+1+I                                                                 
      X(I)=EXDA(KI)                                                             
    3 CONTINUE                                                                  
      CALL ARITH(Y,funct)
      IF(INDFLG(3)) 4,6,4                                                       
    4 IF(Y) 13,13,5                                                             
C-----  MAXIMUM LIKLEHOOD                                                       
    5 S=S-DLOG(Y)                                                               
      Y=-Y                                                                      
      SIG=Y                                                                     
      GO TO 7                                                                   
C-----  CHI SQUARED                                                             
    6 SIG=EXDA(K2+1)                                                            
      Y=Y-EXDA(K1)                                                              
C-----THE FOLLOWING STATEMENT WAS MODIFIED                                      
C     S=S+((Y/SIG)**2)/2.0D0                                                    
      S=S+(Y/SIG)**2                                                            
    7 CONTINUE                                                                  
      N=0                                                                       
      DO 9 J=1,M                                                                
      IF(PL(J)) 9,9,8                                                           
    8 N=N+1                                                                     
      DF(N)=DF(J)/SIG                                                           
      G(J)=G(J)+DF(N)*(Y/SIG)                                                   
    9 CONTINUE                                                                  
      L=1                                                                       
      IF(N.LT.1) GO TO 11                                                       
      DO 10 I=1,N                                                               
      DO 10 J=1,I                                                               
      Z(L)=Z(L)+DF(I)*DF(J)                                                     
      L=L+1                                                                     
   10 CONTINUE                                                                  
   11 K2=K2+NED(2)                                                              
   12 CONTINUE                                                                  
      RETURN                                                                    
C----- -VE OR ZERO Y IN MAXIMUM LIKLEHOOD                                       
   13 INDFLG(1)=1                                                               
      S=1.0D10                                                                  
      RETURN                                                                    
      END
      SUBROUTINE SCAL(M,ER)                                                    
C       SUBROUTINE SCAL CALCULATES ER=VARIANCE OF THEORETICAL FUNCTION Y        
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      COMMON /Z/Z(410)/DF/DF(20)                                              
      COMMON /PLU/PL(20)                                                       
      ER=0.0D0                                                                  
      N=0                                                                       
      DO 2 J=1,M                                                                
      IF(PL(J)) 2,2,1                                                           
    1 N=N+1                                                                     
      DF(N)=DF(J)                                                               
    2 CONTINUE                                                                  
      IF(N.EQ.0) RETURN                                                         
      DO 6 I=1,N                                                                
      DO 6 L=1,N                                                                
      IF(I-L) 3,3,4                                                             
    3 K=L*(L-1)/2+I                                                             
      GO TO 5                                                                   
    4 K=I*(I-1)/2+L                                                             
    5 ER=ER+Z(K)*DF(I)*DF(L)                                                    
    6 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE MONIT (S,M,NN3,IT,EPS,GT,AKAPPA,ALAMBD)                       
C-----  ITERATION PROGRESS OUTPUT                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      COMMON /A/A(20)/SIGMA/SIGMA(20)/R/R(20)/PL/PL(20)/PLU/PL0(20)
     1/ENDFLG/ENDFLG,NA,INDFLG(5)                                               
      IF(IT) 11,3,1                                                             
    1 IF(NN3) 4,4,2                                                             
    2 IF(NM) 3,4,4                                                              
    3 IF(ENDFLG) 4,12,4                                                         
C----- PRINTER CARRIAGE CONTROL                                                 
    4 I1=6                                                                      
      IF(M.GT.6) I1=5                                                           
      IF(M.GT.12) I1=4                                                          
      IF(M.GT.23) I1=1                                                          
      WRITE(2,19)NN3,S,GT,AKAPPA,ALAMBD                                        
      DO 10 I=1,M                                                               
      IF(PL(I)) 9,9,5                                                           
    5 IF(PL0(I)) 8,7,6                                                          
    6 CONTINUE                                                                  
      WRITE(2,20) I,A(I),SIGMA(I),R(I)                                            
      GO TO 10                                                                  
    7 WRITE(2,21) I,A(I),SIGMA(I),R(I)                                            
      GO TO 10                                                                  
    8 IF(PL0(I).GE.-1.0D0) GO TO 7                                              
      WRITE(2,22) I,A(I)                                                          
      GO TO 10                                                                  
    9 WRITE(2,23) I,A(I)                                                          
   10 CONTINUE                                                                  
   11 NM=-IT                                                                    
   12 NM=NM+1                                                                   
      IF(ENDFLG) 13,14,14                                                       
   13 I=-ENDFLG                                                                 
      GO TO (15,16,17,18),I                                                     
   14 RETURN                                                                    
   15 WRITE(2,24)                                                                  
      GO TO 14                                                                  
   16 WRITE(2,25)                                                                  
      GO TO 14                                                                  
   17 WRITE(2,26)                                                                  
      GO TO 14                                                                  
   18 WRITE(2,27)                                                                  
      GO TO 14                                                                  
C                                                                               
   19 FORMAT(//1X,'ITERATION NO.',I3,', S=',E12.5,', EC = ',E12.5,' KAP
     1PA= ',E12.5,/1X,'LAMBDA= ',E12.5//1H,6X,'PARAMETER',6X,'PARAMETER'
     2,9X,'STANDARD',8X,'CORRELATION'/1H,8X,'NUMBER',9X,'VALUE',11X,'DEV        
     3IATION',9X,'FACTOR'/)                                                     
   20 FORMAT(1H ,8X,I3,4X,3(5X,E12.5))                                          
   21 FORMAT(1H ,8X,I3,4X,3(5X,E12.5),' PARAMETER ON BOUNDARY')                 
   22 FORMAT(1H ,8X,I3,9X,E12.5,5X,'INFINITE ERROR ESTIMATED')                  
   23 FORMAT(1H ,8X,I3,9X,E12.5,5X,'THIS PARAMETER FIXED')                      
   24 FORMAT(1H0,'MINIMISATION TERMINATED AS NO FURTHER DECREASE IN S IS        
     1 OBTAINABLE'/////)                                                        
   25 FORMAT(1H0,'MINIMISATION TERMINATED AS INFINITE ERRORS ESTIMATED'/        
     1////)                                                                     
   26 FORMAT(1H0,'MINIMISATION TERMINATED AS ITERATION LIMIT REACHED'///        
     1/)                                                                        
   27 FORMAT(1H0,'MINIMISATION TERMINATED AS NEGATIVE OR ZERO Y ENCOUNTE        
     1RED AS LOGARITHMIC ARGUMENT'////)                                         
      END                                                                       

      SUBROUTINE ARITH(Y,funct)
C----- ARITHM SETS Y AND CALCULATES DY/DA                                       
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      COMMON /DF/DF(20)/A/A(20)/X/X(10)                                       
      COMMON /PL/PL(20)/AU/AMX(20)/AL/AMN(20)                                
      COMMON /ENDFLG/ ENDFLG,NA,INDFLG(5)
      real*8 funct
      external funct
C----- MAXIMUM RELATIVE PRECISION IN DOUBLE PRECISION ON IBM 37O IS 1.0D-16     
      DATA RP/1.0D-14/                                                          
      Y=FUNCT(X,A)
      DO 6 I=1,NA                                                               
      DF(I)=0.0D0                                                               
      IF(PL(I)) 6,6,1                                                           
    1 AI=A(I)                                                                   
      HI=0.01D0*PL(I)                                                           
      PI=RP*DABS(A(I))                                                          
      IF(HI.LE.PI) HI=PI                                                        
      A(I)=AI+HI                                                                
      IF(A(I)-AMX(I)) 5,5,2                                                     
    2 A(I)=AI-HI                                                                
      HI=-HI                                                                    
      IF(A(I)-AMN(I)) 3,5,5                                                     
    3 A(I)=AMX(I)                                                               
      HI=AMX(I)-AI                                                              
      IF(AMN(I)-AI+HI) 4,5,5                                                    
    4 A(I)=AMN(I)                                                               
      HI=AMN(I)-AI                                                              
    5 DF(I)=(FUNCT(X,A)-Y)/HI
      A(I)=AI                                                                   
    6 CONTINUE                                                                  
      RETURN                                                                    
      END
      SUBROUTINE MCONV(N)                                                      
C-----  MCONV INVERTS THE POSITIVE DEFINITE PACKED SYMMETRIC MATRIX Z           
C-----  BY THE SQUARE-ROOT METHOD                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      COMMON /Z/Z(410)                                                         
      COMMON /PLU/PL0(20)/R/R(20)                                             
      COMMON /ENDFLG/ ENDFLG,NA,INDFLG(5)                                       
C-----  MAXIMUM REAL NUMBER AND MAXIMUM REL. PRECISION IN DOUBLE PRECISION      
C-----  ON IBM 370                                                              
      DATA AM,RP/1.D+35,1.D-14/                                                  
      IF(N.LT.1) RETURN                                                         
      APS=DSQRT(AM/DFLOAT(N))                                                   
      AP=1.0D0/(APS*APS)                                                        
      IR=0                                                                      
      DO 11 I=1,N                                                               
    1 IR=IR+1                                                                   
      IF(PL0(IR)) 1,1,2                                                         
    2 NI=I*(I-1)/2                                                              
      II=NI+I                                                                   
      K=N+1                                                                     
      IF(Z(II).LE.RP*DABS(R(IR)).OR.Z(II).LE.AP) GO TO 19                       
      Z(II)=1.0D0/DSQRT(Z(II))                                                  
      NL=II-1                                                                   
    3 IF(NL-NI) 5,5,4                                                           
    4 Z(NL)=Z(NL)*Z(II)                                                         
      IF(DABS(Z(NL)).GE.APS) GO TO 16                                           
      NL=NL-1                                                                   
      GO TO 3                                                                   
    5 IF(I-N) 6,12,12                                                           
    6 K=K-1                                                                     
      NK=K*(K-1)/2                                                              
      NL=NK                                                                     
      KK=NK+I                                                                   
      D=Z(KK)*Z(II)                                                             
      C=D*Z(II)                                                                 
      L=K                                                                       
    7 LL=NK+L                                                                   
      LI=NL+I                                                                   
      Z(LL)=Z(LL)-Z(LI)*C                                                       
      L=L-1                                                                     
      NL=NL-L                                                                   
      IF(L-I) 9,9,7                                                             
    8 LL=NK+L                                                                   
      LI=NI+L                                                                   
      Z(LL)=Z(LL)-Z(LI)*D                                                       
    9 L=L-1                                                                     
      IF(L) 10,10,8                                                             
   10 Z(KK)=-C                                                                  
      IF(K-I-1) 11,11,6                                                         
   11 CONTINUE                                                                  
   12 DO 14 I=1,N                                                               
      DO 14 K=I,N                                                               
      NL=K*(K-1)/2                                                              
      KI=NL+I                                                                   
      D=0.0D0                                                                   
      DO 13 L=K,N                                                               
      LI=NL+I                                                                   
      LK=NL+K                                                                   
      D=D+Z(LI)*Z(LK)                                                           
      NL=NL+L                                                                   
   13 CONTINUE                                                                  
      KI=K*(K-1)/2+I                                                            
      Z(KI)=D                                                                   
   14 CONTINUE                                                                  
   15 RETURN                                                                    
   16 K=I+NL-II                                                                 
      IR=0                                                                      
      DO 18 I=1,K                                                               
   17 IR=IR+1                                                                   
      IF(PL0(IR)) 17,17,18                                                      
   18 CONTINUE                                                                  
   19 PL0(IR)=-2.0D0                                                            
      R(IR)=0.0D0                                                               
      INDFLG(1)=IR                                                              
      GO TO 15                                                                  
      END                                                                       
                                                                 
*                          *
*                          *
****************************

