!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!         ROUTINES DI LETTURA E SCRITTURA SPETTRI           !!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!! Programmi che accedono al VME di GASP (/H e /G)           !!!!!
!!!!!          e all' HistoServer di EUROBALL (/E)              !!!!!
!!!!! vanno linkati per mezzo di un file di opzioni /OPT tipo   !!!!!
!!!!!   program_to_link                                         !!!!!
!!!!!   MULTINET_ROOT:[MULTINET.library]rpc/lib                 !!!!!
!!!!!   MultiNet:MultiNet_Socket_Library /Share                 !!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

CCCCC  commentate con CONLINE le righe che richiedono MULTINET

#include "../libr/types.def"

	SUBROUTINE READSPEC(FNAME,SPEK,FORM,NCAN,KV)

C	ROUTINE DI LETTURA SPETTRI
C
C	FNAME	CHARACTER STRING CONTENENTE IL FILENAME
C	SPEK	INTEGER/REAL ARRAY CONTENENTE I DATI
C	NCAN	NUMERO DI CANALI DA LEGGERE
C	FORM	CHARACTER STRING CON IL FORMATO DEL FILE DATI
C obsolete  U NSIGNED INTEGERS	(16 BITS)
C obsolete  I NTEGERS			(15 BITS PIU' SEGNO)
C           R EALS			(32 BITS)
C           L ONG INTEGERS		(31 BITS PIU' SEGNO)
C           A SCII			PDP/LNL or (10I9) 
C           H ISTOGRAMS from GASP online
C           G ANALYSIS  from GASP online
C           E FROM EUROBALL HISTOSERVER
C
C	KV	VARIABILE PER INFORMAZIONI DI RITORNO
C
C	  >0    OK  KV=#CANALI EFFETTIVAMENTE LETTI
C	  =0     ? NON DOVREBBE MAI ESSERE
C	  =-2	IFORM VALORE NON AMMESSO
C	  =-1	USCITO DOPO CTRL_Z	! MAI
C	  =-3	LUNGEZZA FILE ILLECITA	! MAI
C	  =-4	OPEN ERROR
C	  =-5	READ/WRITE ERROR
C

	CHARACTER FNAME*(*),FORM*(*)
*	DIMENSION SPEK(MAX(1,NCAN))
	DIMENSION SPEK(1)
	INTEGER NCAN,KV

	INTEGER DATATYPE	! 0=INTEGER  1=REAL

	CHARACTER*1   CH
	CHARACTER*80  LOCFNAME,EXTFILE

	INTEGER*4 LTEMP(128)
	INTEGER*2 ITEMP(256)
	EQUIVALENCE (LTEMP,ITEMP)

	byte cchz(100)

	logical*1 connected	/.false./

*	integer  EBH_connect_to_bridge,EBH_read_spectrum
*	external EBH_connect_to_bridge,EBH_read_spectrum
*	integer*4 EBH_read

	ENTRY READSPECR(FNAME,SPEK,FORM,NCAN,KV)
	DATATYPE=1
	GOTO 1

	ENTRY READSPECL(FNAME,SPEK,FORM,NCAN,KV)
	DATATYPE=0

1	KV=0

	CH=FORM(1:1)
	CALL STR_TOUPPER(CH)

	NKAP=MAX(1,(NCAN+1024-1)/1024)

	LOCFNAME=FNAME
	LLOCFNAME=LENGTHC(LOCFNAME)
	LPOND=index(locfname,'#')		! strip off #nnn part of filename
	if(lpond.gt.0) then
	  call str_toint(locfname(lpond+1:),10,nspec,ncifre)
	  locfname=locfname(1:lpond-1)//locfname(lpond+1+ncifre:)
	  llocfname=lengthc(locfname)
	else
	  nspec=0
	endif

10	IF(CH.EQ.'U') THEN			! UUUUUUUUUUUUUUUUUUUUUUUUUU
#if defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER) 
	  OPEN(UNIT=1,STATUS='OLD',READONLY,FILE=LOCFNAME,ACCESS='DIRECT',RECL=128,ERR=7)

#elif defined( __sun_c__ ) || defined( __GFORTRAN__ )
	  OPEN(UNIT=1,STATUS='OLD',READONLY,FILE=LOCFNAME,ACCESS='DIRECT',RECL=512,ERR=7)

#elif defined(__PGI)
	  OPEN(UNIT=1,status='OLD',file=LOCFNAME,ACCESS='DIRECT',RECL=512,ERR=7)
#else
#   error "Undefined FORTRAN compiler"
#endif

	  DO II=1,NCAN
	    SPEK(II)=0
	  ENDDO
	  NBLO=NKAP*4
	  IOFFS=NSPEC*NBLO
	  LL=0
	  DO II=IOFFS+1,IOFFS+NBLO
	    READ(1,rec=II,ERR=8)(ITEMP(JJ),JJ=1,256)
	    DO JJ=1,256
	      LL=LL+1
#if defined( _GW_BIG_ENDIAN )
	     call swap_bytes(ITEMP(JJ),2)
#endif	  
	      LCHAN=ITEMP(JJ)
	      IF(LCHAN.LT.0) LCHAN=LCHAN+65536
	      SPEK(LL)=LCHAN
	    ENDDO
	  ENDDO
	  if(datatype.eq.0) call real_to_lint(spek,ncan)

	ELSEIF(CH.EQ.'I') THEN			! IIIIIIIIIIIIIIIIIIIIIIIIII
#if defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER) 
	  OPEN(UNIT=1,STATUS='OLD',READONLY,FILE=LOCFNAME,ACCESS='DIRECT',RECL=128,ERR=7)

#elif defined( __sun_c__ ) || defined( __GFORTRAN__ )
	  OPEN(UNIT=1,STATUS='OLD',READONLY,FILE=LOCFNAME,ACCESS='DIRECT',RECL=512,ERR=7)

#elif defined(__PGI)
	  OPEN(UNIT=1,status='OLD',file=LOCFNAME,ACCESS='DIRECT',RECL=512,ERR=7)

#else
#   error "Undefined FORTRAN compiler"
#endif
 	  DO II=1,NCAN
	    SPEK(II)=0
	    ENDDO
	  NBLO=NKAP*4
	  IOFFS=NSPEC*NBLO
	  LL=0
	  DO II=IOFFS+1,IOFFS+NBLO
	    READ(1,rec=II,ERR=8)(ITEMP(JJ),JJ=1,256)
	    DO JJ=1,256
	      LL=LL+1
#if defined( _GW_BIG_ENDIAN )
	     call swap_bytes(ITEMP(JJ),2)
#endif	  
	      SPEK(LL)=ITEMP(JJ)
	    ENDDO
	  ENDDO
	  if(datatype.eq.0) call real_to_lint(spek,ncan)

	ELSEIF(CH.EQ.'R') THEN			! RRRRRRRRRRRRRRRRRRRRRRRRRRR
#if defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER) 
	  OPEN(UNIT=1,STATUS='OLD',READONLY,FILE=LOCFNAME,ACCESS='DIRECT',RECL=128,ERR=7)

#elif defined( __sun_c__ )|| defined( __GFORTRAN__ )
	  OPEN(UNIT=1,STATUS='OLD',READONLY,FILE=LOCFNAME,ACCESS='DIRECT',RECL=512,ERR=7)

#elif defined(__PGI)
	  OPEN(UNIT=1,status='OLD',file=LOCFNAME,ACCESS='DIRECT',RECL=512,ERR=7)

#else
#   error "Undefined FORTRAN compiler"
#endif
	  DO II=1,NCAN
	    SPEK(II)=0
	  ENDDO
	  NBLO=NKAP*8
	  IOFFS=NSPEC*NBLO
	  LL=0
	  DO II=IOFFS+1,IOFFS+NBLO
	    READ(1,rec=II,ERR=8)(SPEK(JJ),JJ=LL+1,LL+128)
	    LL=LL+128
	  ENDDO
	  if(datatype.eq.0) call real_to_lint(spek,ncan)
#if defined( _GW_BIG_ENDIAN )
	  do ii=1,ncan
	     call swap_bytes(spek(ii),4)
	  enddo
#endif	  

	ELSEIF(CH.EQ.'L') THEN			! LLLLLLLLLLLLLLLLLLLLLLLLLLLL
#if defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER) 
	  OPEN(UNIT=1,STATUS='OLD',READONLY,FILE=LOCFNAME,ACCESS='DIRECT',RECL=128,ERR=7)

#elif defined( __sun_c__ ) || defined( __GFORTRAN__ )
	  OPEN(UNIT=1,STATUS='OLD',READONLY,FILE=LOCFNAME,ACCESS='DIRECT',RECL=512,ERR=7)

#elif defined(__PGI)
	  OPEN(UNIT=1,status='OLD',file=LOCFNAME,ACCESS='DIRECT',RECL=512,ERR=7)

#else
#   error "Undefined FORTRAN compiler"
#endif
*	  write(6,*)' Ncan : ',ncan
*	  call flush(6)
*	  pause
	  
	  DO II=1,NCAN
	    SPEK(II)=0
	  ENDDO
	  NBLO=NKAP*8
	  IOFFS=NSPEC*NBLO
	  LL=0
	  DO II=IOFFS+1,IOFFS+NBLO
	    READ(1,rec=II,ERR=8)(ltemp(jj) ,jj=1,128)    !(SPEK(JJ),JJ=LL+1,LL+128)
	    do jj=1,128
#if defined( _GW_BIG_ENDIAN )
	     call swap_bytes(ltemp(jj),4)
#endif	  
	     spek(jj+ll)=ltemp(jj)
	    enddo
	    LL=LL+128
	  ENDDO
*	  if(datatype.eq.1) call lint_to_real(spek,ncan)
	  if(datatype.eq.0) call real_to_lint(spek,ncan)

	ELSEIF(CH.EQ.'A') THEN			! AAAAAAAAAAAAAAAAAAAAAAAAAAA
	  OPEN(UNIT=1,FILE=LOCFNAME,status='OLD',ERR=7)
	  DO II=1,NCAN
	    SPEK(II)=0
	  ENDDO
	  READ(1,*,END=55,ERR=55) (SPEK(JJ),JJ=1,NCAN)
55	  LL=max(0,jj-1)
	  if(datatype.eq.0 .and. ll.gt.0) call real_to_lint(spek,ll)

	ELSEIF(CH.EQ.'H' .OR.			! HHHHHHHHHHHHHHHHHHHHHHHHHHH
	1       CH.EQ.'G') THEN			! GGGGGGGGGGGGGGGGGGGGGGGGGGG
	  call str_toupper(locfname)
	  if(lpond.gt.0) then
	    ntype=7
	  else
	    ntype=6
	  endif
	  if(ch.eq.'H') then
	    open (unit=29,file='histocal.tabhist',STATUS='old',err=7)
	  elseif(ch.eq.'G') then
	    open (unit=29,file='analysis.tabhist',STATUS='old',err=7)
	  endif
	  read(29,'(Z8)',err=178) ioffset
	  call inp_showlun(jl1,jl2)
	  call inp_setlun(29,0) ! input from LU=29 and NO input prompts
175	  call inp_mode(0)		!! read loop from *.tabhist
	  istat=inp_i1('Spectrum type',isptype)
	  if(istat.lt.1) goto 178
	  if(isptype.ne.6 .and. isptype.ne.7) goto 175
	  call inp_mode(2)	! per leggere il resto della riga
	  istat=inp_ch('File_name ',extfile)
	  if(istat.lt.1) goto 178
	  lextfile=lengthc(extfile)
	  if(lextfile.ne.lLOCFNAME) goto 175
	  do ii=1,lextfile
	    if(extfile(ii:ii).eq.'.') extfile(ii:ii)='_'
	  enddo   
	  call STR_toupper(extfile)
	  if(extfile(1:LEXTFILE).ne.LOCFNAME(1:LLOCFNAME)) goto 175
	  if(isptype.eq.6) then
	    nspectra=0
	    istat=inp_i5(' ',loffs,nbytes,nbits,nch1,nch2)
	    if(istat.lt.5) goto 178
	    nchan=2**nbits
	    if(ncan.gt.0) nchan=min(nchan,ncan)
	    ilength=nbytes*nchan
	    iaddr=ioffset+loffs
	  elseif(isptype.eq.7) then
	    istat=inp_i6(' ',nspectra,loffs,nbytes,nbits,nch1,nch2)
	    if(istat.lt.6) goto 178
	    if(nspec.GE.nspectra) goto 178
	    nchan=2**nbits
	    if(ntype.eq.6) then
	    nchan=nchan*nspectra
	    if(ncan.gt.0) nchan=min(nchan,ncan)
	      ilength=nbytes*nchan
	      iaddr=ioffset+loffs
	    else
	      if(ncan.gt.0) nchan=min(nchan,ncan)
	        ilength=nbytes*nchan
	        iaddr=ioffset+loffs+nspec*ilength
	      endif
	    else
	      goto 175
	    endif
	  close(unit=29)

#ifdef ALLOWONLINE
	  if(nbytes.eq.2) then
	    call read_datar_2(spek,iaddr,ilength)
	  elseif(nbytes.eq.4) then
	    call read_datar_4(spek,iaddr,ilength)
	  else
	    goto 7		
	  endif
	  ll=nchan
	  call inp_mode(0)
	  call inp_setlun(jl1,jl2)
	  goto 200
#else
	   write(6,'('' ONLINE access not implemented'')')
	   goto 7
#endif

178	  close(unit=29)
179	  call inp_mode(0)
	  call inp_setlun(jl1,jl2)
	  goto 7

*	ELSEIF(CH.EQ.'E') THEN		! EEEEEEEEEEEEEEEEEEEEEEEEEEEE
*	  if(.not.connected) then
*CONLINE	    istat=EBH_connect_to_bridge()
*	     connected=.true.
*	   endif
*	   locfname=fname     ! per rimettere la parte #nn
*	   llocfname=lengthc(locfname)
*	   call char_to_bytes(LOCFNAME,cchz,lLOCFNAME)
*	   cchz(lLOCFNAME+1)=0
*CONLINE   istat=EBH_read_spectrum(cchz, spek)
*	   istat=EBH_read(cchz, spek)
*	   if(istat.gt.0) then
*	     if(datatype.eq.1) call lint_to_real(spek,istat)
*	     kv=istat
*	   else
*	     kv=-5
*	   endif
*	   goto 300

	ELSE					! ERROR
	  KV=-2					! INVALID FORMAT
	  GOTO 300
	ENDIF

200	CLOSE(UNIT=1)
	KV=LL					! TUTTO OK
	GOTO 300

7	KV=-4
	GOTO 300

8	CLOSE(UNIT=1)
	KV=-5

300	RETURN

	END

	SUBROUTINE WRITESPEC(FNAME,SPEK,FORM,NCAN,KV)

C	ROUTINE DI SCRITTURA SPETTRI
C
C	FNAME	CHARACTER STRING CONTENENTE IL FILENAME
C	SPEK	INTEGER/REAL ARRAY DOVE CONTENENTE I DATI
C	NCAN	NUMERO DI CANALI DA SCRIVERE
C	FORM	CHARACTER STRING CON IL FORMATO DEL FILE DATI
C obsolete   U NSIGNED INTEGERS	(16 BITS)
C obsolete   I NTEGERS		(15 BITS PIU' SEGNO)
C	     R EALS		(32 BITS)
C	     L ONG INTEGERS	(31 BITS PIU' SEGNO)
C	     A SCII		(10I9) 
C not valid  H ISTOGRAMS from GASP online
C not valid  G ANALYSIS  from GASP online
C not valid  E FROM EUROBALL HISTOSERVER
C
C	KV	VARIABILE PER INFORMAZIONI DI RITORNO
C
C	  >0    OK  KV=#CANALI EFFETTIVAMENTE SCRITTI
C	  =0    DATI NEGATIVI CON FORMAT U
C	  =-2	IFORM VALORE NON AMMESSO
C	  =-1	USCITO DOPO CTRL_Z	! MAI
C	  =-3	LUNGEZZA FILE ILLECITA	! MAI
C	  =-4	OPEN ERROR
C	  =-5	READ/WRITE ERROR
C

	CHARACTER FNAME*(*),FORM*(*)
*	INTEGER   SPEK(MAX(1,NCAN))
	INTEGER   SPEK(1)
	INTEGER NCAN,KV

	INTEGER DATATYPE	! 0=INTEGER  1=REAL

	CHARACTER*1  CH
	CHARACTER*8  FILESTAT
	CHARACTER*80 LOCFNAME

	INTEGER*4 LTEMP(128)


	ENTRY WRITESPECR(FNAME,SPEK,FORM,NCAN,KV)
	DATATYPE=1
	GOTO 1

	ENTRY WRITESPECL(FNAME,SPEK,FORM,NCAN,KV)
	DATATYPE=0

1	KV=0

	CH=FORM(1:1)
	CALL STR_TOUPPER(CH)

	NKAP=MAX(1,(NCAN+1024-1)/1024)

	LOCFNAME=FNAME
	LLOCFNAME=LENGTHC(LOCFNAME)
	LPOND=index(locfname,'#')
	if(lpond.gt.0) then
	  call str_toint(locfname(lpond+1:),10,nspec,ncifre)
	  locfname=locfname(1:lpond-1)//locfname(lpond+1+ncifre:)
	  llocfname=lengthc(locfname)
	  filestat='unknown'
	else
	  nspec=0
	  filestat='new'
	endif



	IF(CH.EQ.'U' .OR.			! UUUUUUUUUUUUUUUUUUUUUUUUUU
	1  CH.EQ.'I') THEN			! IIIIIIIIIIIIIIIIIIIIIIIIII
	  write(6,*) ' |U and |I spectrum format no more supported'
	  kv=-2					! NO MORE SUPPORTED

	ELSEIF(CH.EQ.'R') then			! RRRRRRRRRRRRRRRRRRRRRRRRRRR
	  NBLO=NKAP*8
#if defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER) 
	  OPEN(UNIT=1,STATUS=FILESTAT,FILE=LOCFNAME,ACCESS='DIRECT',RECL=128,
#if defined(Digital)
	1  INITIALSIZE=NBLO,
#endif
	2  ERR=7)

#elif defined( __sun_c__ ) || defined( __GFORTRAN__ )
	  OPEN(UNIT=1,STATUS=FILESTAT,FILE=LOCFNAME,ACCESS='DIRECT',RECL=512,ERR=7)

#elif defined(__PGI)
	  OPEN(UNIT=1,status=FILESTAT,file=LOCFNAME,ACCESS='DIRECT',RECL=512,ERR=7)
#else
#   error "Undefined FORTRAN compiler"
#endif

	  LL=0
	  ioff=nspec*nblo
	  DO II=ioff+1,ioff+nblo
	    do jj=ll+1,ll+128
	      LTEMP(jj-ll)=SPEK(jj)
	    enddo
	    IF(DATATYPE.EQ.0) CALL LINT_TO_REAL(LTEMP,128)
#if defined( _GW_BIG_ENDIAN )
	do ijj=1,128
	   call swap_bytes(ltemp(ijj),4)
	enddo
#endif
	    WRITE(1,rec=II,ERR=8) LTEMP
	    LL=LL+128
	  ENDDO

	ELSEIF(CH.EQ.'L' ) THEN			! LLLLLLLLLLLLLLLLLLLLLLLLLLLL
	  NBLO=NKAP*8
#if defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER) 
	  OPEN(UNIT=1,STATUS=FILESTAT,FILE=LOCFNAME,ACCESS='DIRECT',RECL=128,
#if defined(Digital)
	1  INITIALSIZE=NBLO,
#endif
	2  ERR=7)

#elif defined( __sun_c__ ) || defined( __GFORTRAN__ )
	  OPEN(UNIT=1,STATUS=FILESTAT,FILE=LOCFNAME,ACCESS='DIRECT',RECL=512,ERR=7)

#elif defined(__PGI)
	  OPEN(UNIT=1,status=FILESTAT,file=LOCFNAME,ACCESS='DIRECT',RECL=512,ERR=7)

#else
#   error "Undefined FORTRAN compiler"
#endif
	  LL=0
	  ioff=nspec*nblo
	  DO II=ioff+1,ioff+nblo
	    do jj=ll+1,ll+128
	      LTEMP(jj-ll)=SPEK(jj)
	    enddo
	    IF(DATATYPE.EQ.1) CALL REAL_TO_LINT(LTEMP,128)
#if defined( _GW_BIG_ENDIAN )
	do ijj=1,128
	   call swap_bytes(ltemp(ijj),4)
	enddo
#endif
	    WRITE(1,rec=II,ERR=8) LTEMP
	    LL=LL+128
	  ENDDO

	ELSEIF(CH.EQ.'A') THEN			! AAAAAAAAAAAAAAAAAAAAAAAAAAA
	  if(lpond.gt.0) then
	    write(6,*) ' # not allowed for |A spectrum format'
	    goto 7
	  endif
	  OPEN(UNIT=1,FILE=FNAME,STATUS='NEW',ERR=7)
	  LL=0
	  DO II=1,NCAN,10
	    NN=0
	    DO JJ=1,10
	      LL=LL+1
	      IF(LL.LE.NCAN) THEN
	        LTEMP(JJ)=SPEK(LL)
	        NN=NN+1
	      ELSE
	        LTEMP(JJ)=0
	      ENDIF
	    ENDDO
	    IF(DATATYPE.EQ.1) CALL REAL_TO_LINT(LTEMP,10)
  	    IF(NN.GT.0) WRITE(1,'(10I9)',ERR=8)(LTEMP(JJ),JJ=1,NN)
	  ENDDO

	ELSEIF(CH.EQ.'H' .OR.			! HHHHHHHHHHHHHHHHHHHHHHHHHHH
	1      CH.EQ.'G') THEN			! GGGGGGGGGGGGGGGGGGGGGGGGGGG
	   KV=-2				! INVALID FORMAT FOR WRITE
	   GOTO 300

	ELSE					! ???????????????????????????
	   KV=-2				! UNKNOWN FORMAT
	   GOTO 300
	ENDIF

200	CLOSE(UNIT=1)
	KV=LL			! TUTTO OK
	GOTO 300

7	KV=-4
	GOTO 300

8	CLOSE(UNIT=1)
	KV=-5

300	RETURN

	END

	SUBROUTINE READDAT(IASK,FNAME,SPEK,NCHAN,IFORM,KV)

C	ROUTINE DI LETTURA SPETTRI CON POSSIBILE DOMANDA DEL NOME
C
C	IASK= 0   FILENAME, FORMATO E DIMENSIONI PASSATE NELLA CHIAMATA
C	    = 1         "    "          "        CHIESTE AL TERMINALE
C	    =-1     COME =0 MA IN CASO DI ERRORE SI COMPORTA COME =1
C	FNAME	CHARACTER STRING CONTENENTE IL FILENAME
C	SPEK	INTEGER/REAL ARRAY CONTENENTE I DATI
C	NCHAN	NUMERO DI CANALI DA LEGGERE
C	IFORM	FORMATO DEL FILE DATI
C obsolete  1=U NSIGNED INTEGERS	(16 BITS)
C obsolete  2=I NTEGERS			(15 BITS PIU' SEGNO)
C           3=R EALS			(32 BITS)
C           4=L ONG INTEGERS		(31 BITS PIU' SEGNO)
C           5=A SCII			PDP/LNL or (10I9) 
C           6=H ISTOGRAMS from GASP online
C           7=G ANALYSIS  from GASP online
C           8=E FROM EUROBALL HISTOSERVER
C
C	VALORE DELLA FUNZIONE AL RITORNO
C
C	  >0   OK =100*(numero di canali effettivamente trattati)+IFORM
C	  =0	NON DOVREBBE MAI ESSERE
C	  =-1	USCITO DOPO CTRL_Z
C	  =-2	IFORM VALORE NON AMMESSO
C	  =-3	LUNGEZZA FILE ILLECITA
C	  =-4	OPEN ERROR
C	  =-5	READ ERROR

	INTEGER IASK,NCHAN,IFORM,KV
	CHARACTER*(*) FNAME
*	INTEGER SPEK(MAX(1,NCHAN))
	INTEGER SPEK(1)

	INTEGER DATATYPE	! 0=INTEGER  1=REAL

	CHARACTER FMT*12 	/'UIRLAHGE'/
	CHARACTER*1 CH
	integer ii


	ENTRY READDATR(IASK,FNAME,SPEK,NCHAN,IFORM,KV)
	DATATYPE=1
	GOTO 1

	ENTRY READDATL(IASK,FNAME,SPEK,NCHAN,IFORM,KV)
	DATATYPE=0

1	KV=0
	CH=' '
	IF(IFORM.GT.0 .AND. IFORM.LE.8)  CH=FMT(IFORM:IFORM)

	IF(NCHAN.GT.0) THEN
	  NKAP=MAX(1,(NCHAN+1024-1)/1024)
	ELSE
	  NKAP=1
	ENDIF

	IF(IASK.LE.0) GOTO 100

10	II=INP_SPNAME('Filename[|Format:Length]',FNAME,CH,NKAP)
	IF(II.LT.0) THEN
	  KV=-1			! CTRL_Z
	  GOTO 300
	ENDIF

100     if(datatype.eq.0) then
	  CALL READSPECL(FNAME,SPEK,CH,NKAP*1024,KV)
	else
	  CALL READSPECR(FNAME,SPEK,CH,NKAP*1024,KV)
	endif	  

	if(kv.gt.0) then
	  KV=KV*100+INDEX(FMT,CH)
	  GOTO 300
	endif

	IF(KV.EQ.-2) THEN
	  WRITE(6,*) ' Format not allowed'
	ELSEIF(KV.EQ.-3) THEN
	  WRITE(6,*) ' Error in file length'
	ELSEIF(KV.EQ.-4) THEN
	  ILE=MAX(LENGTHC(FNAME),1)
	  WRITE(6,'('' File OPEN error  '',A)') FNAME(1:ILE)
	ELSEIF(KV.EQ.-5) THEN
	  WRITE(6,*)' File READ error'
	ELSE
	  WRITE(6,*)' Error   KV=',KV
	ENDIF
	IF(IASK.EQ.0) GOTO 300

	ILE=MAX(LENGTHC(FNAME),1)
	do ii = 1,ILE
	   fname(ii:ii) = ' '
	enddo
	
	CALL INP_MODE(0)
	GOTO 10

300	RETURN

	END

	SUBROUTINE WRITEDAT(IASK,FNAME,SPEK,NCHAN,IFORM,KV)

C	ROUTINE DI SCRITTURA SPETTRI CON POSSIBILE DOMANDA DEL NOME
C
C	IASK= 0   FILENAME, FORMATO E DIMENSIONI PASSATE NELLA CHIAMATA
C	    = 1         "    "          "        CHIESTE AL TERMINALE
C	    =-1     COME =0 MA IN CASO DI ERRORE SI COMPORTA COME =1
C	FNAME	CHARACTER STRING CONTENENTE IL FILENAME
C	SPEK	INTEGER/REAL ARRAY CONTENENTE I DATI
C	NCHAN	NUMERO DI CANALI DA SCRIVERE
C	IFORM	FORMATO DEL FILE DATI
C obsolete   U NSIGNED INTEGERS	(16 BITS)
C obsolete   I NTEGERS		(15 BITS PIU' SEGNO)
C	     R EALS		(32 BITS)
C	     L ONG INTEGERS	(31 BITS PIU' SEGNO)
C	     A SCII		(10I9) 
C not valid  H ISTOGRAMS from GASP online
C not valid  G ANALYSIS  from GASP online
C not valid  E FROM EUROBALL HISTOSERVER
C
C	VALORE DELLA FUNZIONE AL RITORNO
C
C	  >0   OK =100*(numero di canali effettivamente trattati)+IFORM
C	  =0	NON DOVREBBE MAI ESSERE
C	  =-1	USCITO DOPO CTRL_Z
C	  =-2	IFORM VALORE NON AMMESSO
C	  =-3	LUNGEZZA FILE ILLECITA
C	  =-4	OPEN ERROR
C	  =-5	WRITE ERROR

	INTEGER IASK,NCHAN,IFORM,KV
	CHARACTER*(*) FNAME
*	INTEGER SPEK(MAX(1,NCHAN))
	INTEGER SPEK(1)

	INTEGER DATATYPE	! 0=INTEGER  1=REAL

	CHARACTER FMT*12 	/'UIRLAHGE'/
	CHARACTER*1 CH


	ENTRY WRITEDATR(IASK,FNAME,SPEK,NCHAN,IFORM,KV)
	DATATYPE=1
	GOTO 1

	ENTRY WRITEDATL(IASK,FNAME,SPEK,NCHAN,IFORM,KV)
	DATATYPE=0

1	KV=0
	CH=' '

	IF(IFORM.GT.0 .OR. IFORM.LE.8) CH=FMT(IFORM:IFORM)

	IF(NCHAN.GT.0) THEN
	  NKAP=MAX(1,(NCHAN+1024-1)/1024)
	ELSE
	  NKAP=1
	ENDIF

	IF(IASK.LE.0) GOTO 100

10	II=INP_SPNAME('Filename[|Format:Length]',FNAME,CH,NKAP)
	IF(II.LT.0) THEN
	  KV=-1			! CTRL_Z
	  GOTO 300
	ENDIF

100     if(datatype.eq.0) then
	  CALL WRITESPECL(FNAME,SPEK,CH,NKAP*1024,KV)
	else
	  CALL WRITESPECR(FNAME,SPEK,CH,NKAP*1024,KV)
	endif	  

	if(kv.gt.0) then
	  KV=KV*100+INDEX(FMT,CH)
	  GOTO 300
	endif

	IF(KV.EQ.-2) THEN
	  WRITE(6,*)' Format not allowed'
	ELSEIF(KV.EQ.-3) THEN
	  WRITE(6,*) ' Error in file length'
	ELSEIF(KV.EQ.-4) THEN
	  ILE=MAX(LENGTHC(FNAME),1)
	  WRITE(6,'('' File OPEN error  '',A)') FNAME(1:ILE)
	ELSEIF(KV.EQ.-5) THEN
	  WRITE(6,*)' File WRITE error'
	ELSE
	  WRITE(6,*)' Error   KV=',KV
	ENDIF
	IF(IASK.EQ.0) GOTO 300

	CALL INP_MODE(0)
	GOTO 10

300	RETURN

	END



	SUBROUTINE FNINCREM(NAME)

C	INCREMANTA DI UN'UNITA' LA STRINGA NAME (PER GESTIONE NOMI DI SPETTRI)

	CHARACTER NAME*(*),CH*1

	I1=1

	II=INDEX(NAME(I1:),'::')
	IF(II.NE.0)I1=I1+II+1

	II=INDEX(NAME(I1:),':')
	IF(II.NE.0)I1=I1+II

11	II=INDEX(NAME(I1:),']')
	IF(II.NE.0) THEN
		I1=I1+II
		GOTO 11
	ENDIF

	I2=LENGTHC(NAME)

	II=INDEX(NAME(I1:I2),';')
	IF(II.NE.0) I2=I1+II-2

13	II=INDEX(NAME(I1:I2),'.')
	IF(II.NE.0) THEN
		I2=I1+II-2
		GOTO 13
	ENDIF

	II=I2
20	IF(II.LT.I1) GOTO 300

	CH=NAME(II:II)

	IF(CH.EQ.'9') THEN
		NAME(II:II)='0'
		II=II-1
		GOTO 20
	ELSE IF(CH.EQ.'Z') THEN
		NAME(II:II)='A'
		II=II-1
		GOTO 20
	ELSE IF(CH.EQ.'z') THEN
		NAME(II:II)='a'
		II=II-1
		GOTO 20
	ELSE
		NAME(II:II)=CHAR(ICHAR(CH)+1)
	ENDIF

300	RETURN

	END

	SUBROUTINE FNDECREM(NAME)

C	DECREMANTA DI UN'UNITA' LA STRINGA NAME (PER GESTIONE NOMI DI SPETTRI)

	CHARACTER NAME*(*),CH*1

	I1=1

	II=INDEX(NAME(I1:),'::')
	IF(II.NE.0)I1=I1+II+1

	II=INDEX(NAME(I1:),':')
	IF(II.NE.0)I1=I1+II

11	II=INDEX(NAME(I1:),']')
	IF(II.NE.0) THEN
		I1=I1+II
		GOTO 11
	ENDIF

	I2=LENGTHC(NAME)

	II=INDEX(NAME(I1:I2),';')
	IF(II.NE.0) I2=I1+II-2

13	II=INDEX(NAME(I1:I2),'.')
	IF(II.NE.0) THEN
		I2=I1+II-2
		GOTO 13
	ENDIF

	II=I2
20	IF(II.LT.I1) GOTO 300

	CH=NAME(II:II)
	IF(CH.EQ.'0') THEN
		NAME(II:II)='9'
		II=II-1
		GOTO 20
	ELSE IF(CH.EQ.'A') THEN
		NAME(II:II)='Z'
		II=II-1
		GOTO 20
	ELSE IF(CH.EQ.'a') THEN
		NAME(II:II)='z'
		II=II-1
		GOTO 20
	ELSE
		NAME(II:II)=CHAR(ICHAR(CH)-1)
	ENDIF

300	RETURN

	END

	SUBROUTINE FNINCREM_TYPE(NAME)

C	INCREMANTA DI UN'UNITA' L'ESTENSIONE DEL FILENAME  NAME

	CHARACTER NAME*(*),CH*1

	I1=1

	II=INDEX(NAME(I1:),'::')
	IF(II.NE.0)I1=I1+II+1

	II=INDEX(NAME(I1:),':')
	IF(II.NE.0)I1=I1+II

11	II=INDEX(NAME(I1:),']')
	IF(II.NE.0) THEN
		I1=I1+II
		GOTO 11
	ENDIF

	II=INDEX(NAME(I1:),'.')
	IF(II.NE.0)I1=I1+II

	I2=LENGTHC(NAME)

	II=INDEX(NAME(I1:I2),';')
	IF(II.NE.0) I2=I1+II-2

	II=INDEX(NAME(I1:I2),'.')
	IF(II.NE.0) I2=I1+II-2

	II=I2
20	IF(II.LT.I1) GOTO 300

	CH=NAME(II:II)

	IF(CH.EQ.'9') THEN
		NAME(II:II)='0'
		II=II-1
		GOTO 20
	ELSE IF(CH.EQ.'Z') THEN
		NAME(II:II)='A'
		II=II-1
		GOTO 20
	ELSE IF(CH.EQ.'z') THEN
		NAME(II:II)='a'
		II=II-1
		GOTO 20
	ELSE
		NAME(II:II)=CHAR(ICHAR(CH)+1)
	ENDIF

300	RETURN

	END

	SUBROUTINE FNDECREM_TYPE(NAME)

C	DECREMANTA DI UN'UNITA' L'ESTENSIONE DEL FILENAME  NAME

	CHARACTER NAME*(*),CH*1

	I1=1

	II=INDEX(NAME(I1:),'::')
	IF(II.NE.0)I1=I1+II+1

	II=INDEX(NAME(I1:),':')
	IF(II.NE.0)I1=I1+II

11	II=INDEX(NAME(I1:),']')
	IF(II.NE.0) THEN
		I1=I1+II
		GOTO 11
	ENDIF

	II=INDEX(NAME(I1:),'.')
	IF(II.NE.0)I1=I1+II

	I2=LENGTHC(NAME)

	II=INDEX(NAME(I1:I2),';')
	IF(II.NE.0) I2=I1+II-2

	II=INDEX(NAME(I1:I2),'.')
	IF(II.NE.0) I2=I1+II-2

	II=I2

20	IF(II.LT.I1) GOTO 300

	CH=NAME(II:II)

	IF(CH.EQ.'0') THEN
		NAME(II:II)='9'
		II=II-1
		GOTO 20
	ELSE IF(CH.EQ.'A') THEN
		NAME(II:II)='Z'
		II=II-1
		GOTO 20
	ELSE IF(CH.EQ.'a') THEN
		NAME(II:II)='z'
		II=II-1
		GOTO 20
	ELSE
		NAME(II:II)=CHAR(ICHAR(CH)-1)
	ENDIF

300	RETURN

	END

	SUBROUTINE FNDECOMPOSE(FN,DIR,LDI,NAM,LNA,TYP,LTY,FOR,LFO)

C	SCOMPONE IL NOME DI SPETTRI   DIR/SDIR/ABCD.EF|F:L

	CHARACTER*(*)  FN,DIR,NAM,TYP,FOR

	LFN=LEN(FN)
	L1=1
	L2=LFN
	DO II=LFN,1,-1
		IF(FN(II:II).NE.' ' .AND. FN(II:II).NE.CHAR(0)) GOTO 10
		L2=L2-1
	ENDDO

10	LDI=0				! DIRECTORY
	DIR=' '
	II=INDEX(FN(L1:L2),'/')
	DO WHILE(II.GT.0)
	  DIR(LDI+1:)=FN(L1:L1-1+II)
	  LDI=LDI + (L1-1+II)-L1+1
	  L1=L1-1+II-1+2
	  II=INDEX(FN(L1:L2),'/')
	END DO

	LFO=0				! FORMAT
	FOR=' '
	II=INDEX(FN(L1:L2),'|')
	IF(II.GT.0) THEN
		FOR=FN(L1-1+II+1:L2)
		LFO=L2-(L1-1+II+1)+1
		L2=L1-1+II+1-1-1
	ENDIF

	LTY=0				! TYPE
	TYP=' '
	II=INDEX(FN(L1:L2),'.')
	IF(II.GT.0) THEN
		TYP=FN(L1-1+II+1:L2)
		LTY=L2-(L1-1+II+1)+1
		L2=L1-1+II+1-1-1
	ENDIF

	LNA=MAX(0,L2-L1+1)		! NAME
	NAM=' '
	IF(LNA.GT.0) NAM=FN(L1:L2)

	END

	SUBROUTINE FNRICOMPOSE(FN,DIR,LDI,NAM,LNA,TYP,LTY,FOR,LFO)

C	RICOMPONE IL NOME DI SPETTRI   DIR/ABCD.EF|F:L

	CHARACTER*(*)  FN,DIR,NAM,TYP,FOR

	LFN=LEN(FN)

	FN=' '
	L2=0

	IF(LDI.GT.0) THEN
		FN=DIR(1:LDI)
		L2=LDI
	ENDIF
	IF(LNA.GT.0) THEN
		FN=FN(1:L2)//NAM(1:LNA)
		L2=L2+LNA
	ENDIF
	IF(LTY.GT.0) THEN
		FN=FN(1:L2)//'.'//TYP(1:LTY)
		L2=L2+LTY+1
	ENDIF
	IF(LFO.GT.0) THEN
		FN=FN(1:L2)//'|'//FOR(1:LFO)
		L2=L2+LFO+1
	ENDIF

	RETURN

	END

	SUBROUTINE SPFORM(FORMI,FORMO,NKAP)

C	DELTERMINA FORMATO E LUNGHEZZA DALLA STRINGA FORMI
C
C	FORMI	CHARACTER STRING CONTENENTE UNA STRUTTURA TIPO d:nn
C	FORMO	FORMATO DEL FILE DATI  ==>  d
C	     U NSIGNED INTEGERS	(16 BITS)
C	     I NTEGERS		(15 BITS PIU' SEGNO)
C	     R EALS		(32 BITS)
C	     L ONG INTEGERS	(31 BITS PIU' SEGNO)
C	     A SCII		PDP/LNL or (10I9) 
C	     H ISTOGRAMS from GASP online
C	     G ANALYSIS  from GASP online
C	     E FROM EUROBALL HISTOSERVER
C
C	NKAP	NUMERO DI KAPPA_CANALI ==>  nn
C		AL RITORNO NKAP=-1 SE CI SONO STATI ERRORI
C	I VALORE DI INGRESSO DI NKAP E FORMO NON VENGONO MODIFICATI
C	SE IN FORMI=' '

	CHARACTER FORMI*(*),FORMO*(*),CH*1
	CHARACTER FMT*12 	/'UIRLAHGE'/

	LFORMI=LENGTHC(FORMI)
	IF(LFORMI.LT.1 .OR. FORMI.EQ.' ') RETURN
	CH=FORMI(1:1)
	CALL STR_TOUPPER(CH)
	II=INDEX(FMT,CH)
  	IF(II.EQ.0) GOTO 99
	FORMO=CH

	IF(LFORMI.LT.3) RETURN
	READ(FORMI(3:LFORMI),*,ERR=99,END=99) NKAP
	RETURN

99	NKAP=-1
	RETURN

	END

#ifdef ALLOWONLINE

	 SUBROUTINE READ_DATAR_4(SPEC,ADDRESS,LENGTH)

	 PARAMETER (MAXNLENGTH=32*1024)
	 INTEGER ADDRESS,LENGTH
	 REAL	 SPEC(1)
	 BYTE	 BBUFF(MAXNLENGTH)
	 BYTE	 BT(4)
	 INTEGER LW
	 EQUIVALENCE (BT,LW)
	 integer GET_VME_HIST

	 l_spec=0
	 l_address=address
	 l_length=length

10	 n_length=min(l_length,MAXNLENGTH)
	 if(n_length.le.0) RETURN
	 iret = GET_VME_HIST (bbuff,%val(l_address),%val(n_length))
		if (iret.ne.0) then
	   	  call segnala(iret)
		  write( *,'(''GET_VME_HIST error = '',i5)')iret
		end if
		do j=1,n_length,4
#if defined( _GW_LITTLE_ENDIAN )
		  BT(1)=BBUFF(j+3)
		  BT(2)=BBUFF(j+2)
		  BT(3)=BBUFF(j+1)
		  BT(4)=BBUFF(j  )
#elif defined( _GW_BIG_ENDIAN )
		  BT(1)=BBUFF(j)
		  BT(2)=BBUFF(j+1)
		  BT(3)=BBUFF(j+2)
		  BT(4)=BBUFF(j+3)
#else
#   error "Undefined endian type"
#endif
	   	  l_spec=l_spec+1
	   	  spec(l_spec)=lw
		end do
	 l_length=l_length-n_length
	 if(l_length.gt.0) then
	   l_address=l_address+n_length
	   goto 10
	 endif

	 RETURN

	 END

	 SUBROUTINE READ_DATAR_2(SPEC,ADDRESS,LENGTH)

	 PARAMETER (MAXNLENGTH=32*1024)
	 INTEGER ADDRESS,LENGTH
	 REAL	 SPEC(1)
	 BYTE	 BBUFF(MAXNLENGTH)
	 BYTE	 BT(2)
	 INTEGER WW
	 EQUIVALENCE (BT,WW)
	 integer GET_VME_HIST

	 l_spec=0
	 l_address=address
	 l_length=length

10	 n_length=min(l_length,MAXNLENGTH)
	 if(n_length.le.0) RETURN
	 iret = GET_VME_HIST (bbuff,%val(l_address),%val(n_length))
		if (iret.ne.0) then
	   	  call segnala(iret)
		  write( *,'(''GET_VME_HIST error = '',i5)')iret
		end if
		do j=1,n_length,2
#if defined( _GW_LITTLE_ENDIAN )
		  BT(1)=BBUFF(j+1)
		  BT(2)=BBUFF(j  )
#elif defined( _GW_BIG_ENDIAN )
		  BT(1)=BBUFF(j  )
		  BT(2)=BBUFF(j+1)
#else
#   error "Undefined endian type"
#endif
	   	  LW=WW
	   if(lw.lt.0) lw=lw+64*1024
	   l_spec=l_spec+1
	   spec(l_spec)=lw
		end do
	 l_length=l_length-n_length
	 if(l_length.gt.0) then
	   l_address=l_address+n_length
	   goto 10
	 endif

	 RETURN

	 END

	 SUBROUTINE READ_DATAL_4(SPEC,ADDRESS,LENGTH)

	 PARAMETER (MAXNLENGTH=32*1024)
	 INTEGER ADDRESS,LENGTH
	 INTEGER SPEC(1)
	 BYTE	 BBUFF(MAXNLENGTH)
	 BYTE	 BT(4)
	 INTEGER LW
	 EQUIVALENCE (BT,LW)
	 integer GET_VME_HIST

	 l_spec=0
	 l_address=address
	 l_length=length

10	 n_length=min(l_length,MAXNLENGTH)
	 if(n_length.le.0) RETURN
	 iret = GET_VME_HIST (bbuff,%val(l_address),%val(n_length))
		if (iret.ne.0) then
	   	  call segnala(iret)
		  write( *,'(''GET_VME_HIST error = '',i5)')iret
		end if
		do j=1,n_length,4
#if defined( _GW_LITTLE_ENDIAN )
		  BT(1)=BBUFF(j+3)
		  BT(2)=BBUFF(j+2)
		  BT(3)=BBUFF(j+1)
		  BT(4)=BBUFF(j  )
#elif defined( _GW_BIG_ENDIAN )
		  BT(1)=BBUFF(j)
		  BT(2)=BBUFF(j+1)
		  BT(3)=BBUFF(j+2)
		  BT(4)=BBUFF(j+3)
#else
#   error "Undefined endian type"
#endif
	   	  l_spec=l_spec+1
	          spec(l_spec)=lw
		end do
	 l_length=l_length-n_length
	 if(l_length.gt.0) then
	   l_address=l_address+n_length
	   goto 10
	 endif

	 RETURN

	 END

	 SUBROUTINE READ_DATAL_2(SPEC,ADDRESS,LENGTH)

	 PARAMETER (MAXNLENGTH=32*1024)
	 INTEGER ADDRESS,LENGTH
	 INTEGER SPEC(1)
	 BYTE	 BBUFF(MAXNLENGTH)
	 BYTE	 BT(2)
	 INTEGER WW
	 EQUIVALENCE (BT,WW)
	 integer GET_VME_HIST

	 l_spec=0
	 l_address=address
	 l_length=length

10	 n_length=min(l_length,MAXNLENGTH)
	 if(n_length.le.0) RETURN
	 iret = GET_VME_HIST (bbuff,%val(l_address),%val(n_length))
		if (iret.ne.0) then
	   	  call segnala(iret)
		  write( *,'(''GET_VME_HIST error = '',i5)')iret
		end if
		do j=1,n_length,2
#if defined( _GW_LITTLE_ENDIAN )
		  BT(1)=BBUFF(j+1)
		  BT(2)=BBUFF(j  )
#elif defined( _GW_BIG_ENDIAN )
		  BT(1)=BBUFF(j  )
		  BT(2)=BBUFF(j+1)
#else
#   error "Undefined endian type"
#endif
	          LW=WW
	          if(lw.lt.0) lw=lw+64*1024
	          l_spec=l_spec+1
	          spec(l_spec)=lw
	        end do
	 l_length=l_length-n_length
	 if(l_length.gt.0) then
	   l_address=l_address+n_length
	   goto 10
	 endif

	 RETURN

	 END
#endif
