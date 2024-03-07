	subroutine NEXTRECORD(iaddr,nseen,icont)

!  Utilizzando le routines della libreria TAPELIB (inclusa in INTER.OLB)
!  legge i record di dati dal nastro. I dati vengono letti con il metodo del
!  ping-pong e vengono piazzati alternativamente in IEVBUF(IAD1) e IEVBUF(IAD2)
!  IAD1 e IAD2 assumono i valori 1 e MAXWORDS+1 alternativamente
C	AL RITORNO	NSEEN >0  NUMERO DI BYTES LETTI
C			NSEEN  0  DOPO ERRORE CONTINUA  catturato internamente
C			NSEEN -1  ERRORE NON CONTINUARE
C			NSEEN -2  EOF
C			NSEEN -3  EOT

#include "gsort.inc"

	INTEGER TAPE_READ
	INTEGER IAD1,IAD2
	SAVE IAD1,IAD2

	if(iio.datafile) then
	  if(iio.rec.le.1) then
	    iio.rec=1
	    iad1=1
	    iad2=MAXWORDS+1
	  endif
	  if(.NOT.iio.virtuale) read(iio.lun,rec=(iio.REC+1),err=5) (ievbuf(IAD1+ii),ii=0,iio.wlen-1)
	  call swapl(IAD1,IAD2)
	  iaddr=iad2
	  nseen=iio.wlen*2
	  iio.rec=iio.rec+1
	  return
5	  nseen=-3
	  return
	endif

	IF(iio.REC.le.1) THEN		! primo record & start pong-pong
	   iio.rec=1
	   IAD1=1
	   IAD2=MAXWORDS+1
	   ISTAT=TAPE_READ(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
CVMS	   IF(.NOT.ISTAT) THEN

CVMS	     NSEEN=-1

CVMS	     RETURN
	     
CVMS	   ENDIF

	ENDIF

10	CALL TAPE_SYNCR(iio.flag,iio.REC,NSEEN,iio.iosb,iio.name,iio.mtch,ICONT)

	IF(NSEEN.LT.0) RETURN

	IF(NSEEN.EQ.0) THEN
	   ISTAT=TAPE_READ(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
CVMS	   IF(.NOT.ISTAT) THEN

CVMS	     NSEEN=-1

CVMS	     RETURN
	     
CVMS	   ENDIF

	   GOTO 10				! leggine un altro
	ENDIF

	IF(NSEEN.EQ.80 .OR. NSEEN.EQ.256) THEN	! Presumibilmente un header
	   WRITE(LU2,*)
	   CALL gs_list_bytes(ievbuf(IAD1),NSEEN,LU1)	
	   CALL gs_list_bytes(ievbuf(IAD1),NSEEN,LU2)	
	   WRITE(LU2,*)
	   ISTAT=TAPE_READ(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
CVMS	   IF(.NOT.ISTAT) THEN

CVMS	     NSEEN=-1

CVMS	     RETURN
	     
CVMS	   ENDIF

	   GOTO 10				! leggine un altro
	ENDIF

	iio.REC=iio.REC+1
	call swapl(IAD1,IAD2)			! Swap puntatori buffer per PING-PONG

!!!!!!!!!!!!!!! prima di uscire lancia la prossima lettura !!!!!!!!!!!!!!!!!!!!

	ISTAT=TAPE_READ(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
CVMS	IF(.NOT.ISTAT) THEN

CVMS	  NSEEN=-1

CVMS	  RETURN
	     
CVMS	ENDIF

	IADDR=IAD2

	RETURN

	END

	subroutine SORT_FAKE(IBUF,nwords,stat)

#include "gsort.inc"
	record/statdat/stat

	do nn=1,iio.vevents
	  iio.evcount=iio.evcount+1
	  do ii=0,ntipi
	    NDET(ii)=0
	  enddo
	  call EVANA
	  if(break) return
	enddo
	return

	end

	subroutine SORT_GASP(IBUF,nwords,stat)

!  decodifica l'evento e lo passa alla routine di analisi
C	Sw=F0nn
C   *	Classification	('4xxx == errore hardware)
C   *	Tag		non usata
C	fix0		Esum del filtro
C	fix1		molteplicita' del filtro
C	...
C	fixN		ultimo parametri fisso
C	Nger		numero rivelatori tipo GPAR
C   *	PointGer	germ=(KEY,E,T)
C	[Nsil]		numero rivelatori tipo SPAR (se definiti)
C   *	[PointSil]	sili=(KEY,E,T)
C
C   *  parametri presenti se 'GD', assenti se 'GR'

#include "gsort.inc"
	record/statdat/stat

	logical*1 original
	INTEGER*2 IBUF(MAXWORDS)

	INTEGER*2 JDAT(2)
	INTEGER*4 LDAT
	EQUIVALENCE (JDAT,LDAT)
	INTEGER*2 IDAT,nm_mask


	if(IBUF(4).eq.'4748'X) then	! this is a header GH
	  CALL gs_list_bytes(ibuf,256,LU1)	
	  CALL gs_list_bytes(ibuf,256,LU2)	
	  return
	endif

	if(IBUF(4).eq.'4758'X) then
	  original=.false.		! GX  dati in formato ridotto
	elseif(IBUF(4).eq.'4744'X) then
	  original=.true.		! GD  dati in formato originale
	else
	  return ! this is not a GASP data record GD
	endif
	

	ipunt=17
	ilast=nwords



30	IF (IPUNT.GT.ILAST) goto 9999		! Finito --> prossimo buffer
	IDAT=IBUF(IPUNT)
31	nm_mask='FF00'X
	if(iand(idat,nm_mask).ne.'F000'X) then
	   if(idat.ne.0) then
	      nlost=nlost+1
	   endif
32	   ipunt=ipunt+1			!! riposizionamento
	   if(ipunt.le.ilast) then
	     IDAT=ibuf(ipunt)
	     nm_mask='FF00'X
	     if(iand(idat,nm_mask).ne.'F000'X) goto 32
	   else
	     goto 9999
	   endif
	endif
	nm_mask='FF'X
	IEVL=IAND(IDAT,nm_mask)
	if(ievl.gt.MAXEVL) stop ' Event too long in SORT_GASP'

	iio.evcount=iio.evcount+1
	ipunt1=ipunt			! offset al primo parametro dell'evento
	IPUNT =IPUNT+ievl+1		! Punta alla testa del  prossimo evento
	if(ipunt.le.ilast) then
	   idat=ibuf(ipunt)
	   nm_mask='FF00'X
	   if(idat.ne.0 .and. iand(idat,nm_mask).ne.'F000'X) goto 31
	elseif(ipunt.gt.ilast+1) then
	   nlost=nlost+1
	   goto 9999
	endif

	ipuntev=ipunt1			!!!!!!!!!!!! riporta l'evento

	if(original) then
	  ipuntev=ipuntev+1
	  class=ibuf(ipuntev)
	  nm_mask='C000'X
	  if(iand(class,nm_mask).ne.0)then
	    nclass=nclass+1
	    goto 30		! classificato come errore dall'ONLINE
	  endif
	  ipuntev=ipuntev+1
	  tag=ibuf(ipuntev)
	endif

	do ii=0,ntipi
	  doff(ii)=0
	  ndet(ii)=0
	enddo

	if(EXISTS(0)) then
	  DOFF(0)=0
	  NDET(0)=1
	  det(0).id=0
	  do ii=0,NDPAR(0)-1
	    ipuntev=ipuntev+1
#if defined( _GW_BIG_ENDIAN )
	    jdat(1)=0
	    jdat(2)=ibuf(ipuntev)
#else
	    jdat(2)=0
	    jdat(1)=ibuf(ipuntev)
#endif
	    det(0).ival(ii)=ldat
	    if(ldat.gt.0) then
	      det(0).xval(ii)=ldat+ran(iseed)
	    else
	      det(0).xval(ii)=0.
	    endif
	  enddo
	  do ii=NDPAR(0),TDPAR(0)-1
	    det(0).ival(ii)=0
	    det(0).xval(ii)=0.
	  enddo
	endif

	do ji=1,ntipi
	  if(ndpar(ji) .GT. 0) then
	    ipuntev=ipuntev+1
	    NDET(ji) =ibuf(ipuntev)
*	    write(6,*)ji,ndet(ji)
*	    call flush(6)
	    DOFF(ji) = DOFF(ji-1) + NDET(ji-1)
	    if(NDET(ji).lt.FOLDMIN(ji)) goto 30
	    if(original) ipuntev=ipuntev+1
	  endif
	enddo

	do ji=1,ntipi
	  if(NDET(ji).gt.0) then
	    nonvalid=0
	    jj=DOFF(ji)
	    do ii=0,NDET(ji)-1
	      ipuntev=ipuntev+1
	      jg=ibuf(ipuntev)
	      if(jg.ge.0 .and. jg.lt.NITEMS(ji)) then
	        det(jj).id=jg
	        do kk=0,NDPAR(ji)-1
	          ipuntev=ipuntev+1
#if defined( _GW_BIG_ENDIAN )
	          jdat(1)=0
	          jdat(2)=ibuf(ipuntev)
#else
	          jdat(2)=0
	          jdat(1)=ibuf(ipuntev)
#endif
	          det(jj).ival(kk)=ldat
	          if(ldat.gt.0) then
	            det(jj).xval(kk)=ldat+ran(iseed)
	          else
	            det(jj).xval(kk)=0.
	          endif
	        enddo
	        do kk=NDPAR(ji),TDPAR(ji)-1
	          det(jj).ival(kk)=0
	          det(jj).xval(kk)=0.
	        enddo
	        jj=jj+1
	      else
	        nonvalid=nonvalid+1
	        ipuntev=ipuntev+NDPAR(ji)
	      endif
	    enddo
	    NDET(ji)=NDET(ji)-nonvalid
	    if(NDET(ji).lt.FOLDMIN(ji)) goto 30
	    STAT.FOLD(NDET(ji),ji,0)=STAT.FOLD(NDET(ji),ji,0)+1
	  endif
	enddo

	call EVANA
	if(break) return

	GOTO 30

9999	return

	end

	subroutine SORT_EURO(IBUF,nwords,stat)

!  decodifica l'evento e lo passa alla routine di analisi

#include "gsort.inc"
	record/statdat/stat

	INTEGER*2 IBUF(MAXWORDS)
	INTEGER*2 IDAT
	
	INTEGER*2 JDAT(2),nm_mask
	INTEGER*4 LDAT, nm_lmask
	EQUIVALENCE (JDAT,LDAT)

	integer*4 pattern(0:31)
	data pattern/	'00000001'X,'00000002'X,'00000004'X,'00000008'X,'00000010'X,'00000020'X,'00000040'X,'00000080'X,
	1		'00000100'X,'00000200'X,'00000400'X,'00000800'X,'00001000'X,'00002000'X,'00004000'X,'00008000'X,
	2		'00010000'X,'00020000'X,'00040000'X,'00080000'X,'00100000'X,'00200000'X,'00400000'X,'00800000'X,
	3		'01000000'X,'02000000'X,'04000000'X,'08000000'X,'10000000'X,'20000000'X,'40000000'X,'80000000'X/

	logical*1 initialize	/.TRUE./
	integer quale(0:31)

	if(initialize) then
	  do ii=0,31
	    quale(ii)=-1
	  enddo
	  do ii=0,NDTYPE
	    if(euromap(ii).ne.-1) then
	      quale(euromap(ii))=ii
	    endif
	  enddo
	  initialize=.FALSE.
	endif

	ipuntev=17				! inizio dati
	ilast=nwords

30	IF(IPUNTEV.GT.ILAST) goto 9999		! Finito --> prossimo buffer
	IDAT=IBUF(IPUNTEV)
31	nm_mask = 'FFFE'X
	if(iand(idat,nm_mask).ne.'FFF0'X) then
	   if(idat.ne.0) then
	      nlost=nlost+1
	   endif
32	   ipuntev=ipuntev+1			!! riposizionamento
	   if(ipuntev.le.ilast) then
	     IDAT=ibuf(ipuntev)
	     nm_mask = 'FFF0'X
	     if(iand(idat,nm_mask).ne.'FFF0'X) goto 32
	   else
	     goto 9999
	   endif
	endif
	ihead=iand(idat,1)
	ipunt=ipuntev+1
	IEVL=IBUF(IPUNT)/2
	if(ievl.gt.MAXEVL) stop ' Event too long in SORT_EURO'
	IPUNTEV=IPUNTEV+ievl		! Punta alla testa del  prossimo evento
	if(ipuntev.le.ilast) then
	   idat=ibuf(ipuntev)
	   nm_mask = 'FFF0'X
	   if(idat.ne.0 .and. iand(idat,nm_mask).ne.'FFF0'X) goto 31
	elseif(ipunt.gt.ilast+1) then
	   nlost=nlost+1
	   goto 9999
	endif
	iio.evcount=iio.evcount+1
	if(ihead.ne.0) then
#if defined( _GW_BIG_ENDIAN )
	  jdat(1)=ibuf(ipuntdet)
	  ipuntdet=ipuntdet+1
	  jdat(2)=ibuf(ipuntdet)
	  ipuntdet=ipuntdet+1
#else
	  jdat(2)=ibuf(ipuntdet)
	  ipuntdet=ipuntdet+1
	  jdat(1)=ibuf(ipuntdet)
	  ipuntdet=ipuntdet+1
#endif
	  iio.evnumber=ldat
	endif

	ipuntdet=ipunt+1			! inizio dati

	if(exists(0)) then
	  ndet(0)=1
	  do jp=0,TDPAR(0)-1
	    det(0).ival(jp)=0
	    det(0).xval(jp)=0.
	  enddo
	  jk=0
	else
	  ndet(0)=0
	  jk=-1
	endif
	
	DO II=1,NTIPI
	  doff(ii)=0
	  ndet(ii)=0
	enddo

	do while(ipuntdet.lt.ipuntev)
	  jdat(2)=0
	  jdat(1)=ibuf(ipuntdet)

	  ipp=ipuntdet
	  nm_lmask = '000001FF'X
	  idetn=iand(ldat,nm_lmask)
	  nm_lmask = '00003E00'X
	  itipo=iand(ldat,nm_lmask)/512
	  nm_lmask = '0000C000'X
	  iform=iand(ldat,nm_lmask)/16384

	  if(iform.eq.2) then
	    ipp=ipp+1
	    ifragl=ibuf(ipp)/2
	    ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	    jdat(1)=0
	    jdat(2)=ibuf(ipp)
#else
	    jdat(2)=0
	    jdat(1)=ibuf(ipp)
#endif
	    ipat=ldat
	  elseif(iform.eq.1) then
	    ipp=ipp+1
	    ifragl=ibuf(ipp)/2
	    ipat=0
	  elseif(iform.eq.0) then
	    ipat=0
	    ifragl=0
	  elseif(iform.eq.3) then
	    ipp=ipp+1
	    ifragl=ibuf(ipp)/2
	    ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	    jdat(1)=ibuf(ipp)
	    ipp=ipp+1
	    jdat(2)=ibuf(ipp)
#else
	    jdat(2)=ibuf(ipp)
	    ipp=ipp+1
	    jdat(1)=ibuf(ipp)
#endif
	    ipat=ldat
	  endif
	  IQ=quale(itipo)

	  if(IQ .LT. 0) then
	    if(iform.eq.0) then
	      write(6,*) ' Skipping rest of event because of undefined detector type',itipo
	      goto 30
	    endif
	    ipuntdet=ipuntdet+ifragl
	  	      
c	  elseif(itipo.le.4) then		! CLUSTERS & CLOVERS & TAPERED
c	    idoff=NSEGS(IQ)*idetn		! fatti in un unico loop
c	    do ii=0,NSEGS(IQ)-1
c	      if(iand(ipat,pattern(ii)).ne.0) then
c	        jk=jk+1
c	        if(NDET(IQ).eq.0) DOFF(IQ)=jk
c	        NDET(IQ)=NDET(IQ)+1
c	        det(jk).id=idoff+ii
c	        do jj=0,NDPAR(IQ)-1
c	          ipp=ipp+1
c	          jdat(2)=0
c	          jdat(1)=ibuf(ipp)
c	          det(jk).ival(jj)=ldat
c	          if(ldat.gt.0) then
c	            det(jk).xval(jj)=ldat+ran(iseed)
c	          else
c	            det(jk).xval(jj)=0.
c	          endif
c	        enddo
c	      endif
c	    enddo
c	    ipuntdet=ipuntdet+ifragl

	  elseif(itipo.eq.1) then		        !!!!!!!!!!!!!!!!! CLUSTER
	    idoff=CSEG*idetn
	    do ii=0,CSEG-1
	      if(iand(ipat,pattern(ii)).ne.0) then
	        jk=jk+1
	        if(NDET(IQ).eq.0) DOFF(IQ)=jk
	        NDET(IQ)=NDET(IQ)+1
	        det(jk).id=idoff+ii
	        do jj=0,NDPAR(IQ)-1
	          ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	          jdat(1)=0
	          jdat(2)=ibuf(ipp)
#else
	          jdat(2)=0
	          jdat(1)=ibuf(ipp)
#endif
	          det(jk).ival(jj)=ldat
	          if(ldat.gt.0) then
	            det(jk).xval(jj)=ldat+ran(iseed)
	          else
	            det(jk).xval(jj)=0.
	          endif
	        enddo
	      endif
            enddo
	    ipuntdet=ipuntdet+ifragl

	  elseif(itipo.eq.2) then			!!!!!!!!!!!!!!!!! CLOVER
	    idoff=QSEG*idetn
	    do ii=0,QSEG-1
	      if(iand(ipat,pattern(ii)).ne.0) then
	        jk=jk+1
	        if(NDET(IQ).eq.0) DOFF(IQ)=jk
	        NDET(IQ)=NDET(IQ)+1
	        det(jk).id=idoff+ii
	        do jj=0,NDPAR(IQ)-1
	          ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	          jdat(1)=0
	          jdat(2)=ibuf(ipp)
#else
	          jdat(2)=0
	          jdat(1)=ibuf(ipp)
#endif
	          det(jk).ival(jj)=ldat
	          if(ldat.gt.0) then
	            det(jk).xval(jj)=ldat+ran(iseed)
	          else
	            det(jk).xval(jj)=0.
	          endif
	        enddo
	      endif
            enddo
	    ipuntdet=ipuntdet+ifragl

	  elseif(itipo.eq.3) then        		!!!!!!!!!!!!!!!!! TAPERED
	    idoff=TSEG*idetn
c	    if(idetn.ge.25) then	!!!!!!!!!!!! modifica per misura 144gd 5/97
c	      itoffs=1			!!!!!!!!!!!! modifica per misura 144gd 5/97
c	    else			!!!!!!!!!!!! modifica per misura 144gd 5/97
c	      itoffs=0			!!!!!!!!!!!! modifica per misura 144gd 5/97
c	    endif			!!!!!!!!!!!! modifica per misura 144gd 5/97
	    do ii=0,TSEG-1
c	      if(iand(ipat,pattern(ii+itoffs)).ne.0) then	!!!!! "  144gd 5/97
	      if(iand(ipat,pattern(ii)).ne.0) then		!!! caso normale
	        jk=jk+1
	        if(NDET(IQ).eq.0) DOFF(IQ)=jk
	        NDET(IQ)=NDET(IQ)+1
	        det(jk).id=idoff+ii
	        do jj=0,NDPAR(IQ)-1
	          ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	          jdat(1)=0
	          jdat(2)=ibuf(ipp)
#else
	          jdat(2)=0
	          jdat(1)=ibuf(ipp)
#endif
	          det(jk).ival(jj)=ldat
	          if(ldat.gt.0) then
	            det(jk).xval(jj)=ldat+ran(iseed)
	          else
	            det(jk).xval(jj)=0.
	          endif
	        enddo
	      endif
            enddo
	    ipuntdet=ipuntdet+ifragl

	  elseif(itipo.GE.5 .OR. itipo.LE.8) then	!!!!!!!!!!!!!!!!! Nwall or ISIS
	    jk=jk+1
	    if(NDET(IQ).eq.0) DOFF(IQ)=jk
	    NDET(IQ)=NDET(IQ)+1
	    det(jk).id=idetn
	    do jj=0,NDPAR(IQ)-1
	      ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	      jdat(1)=0
	      jdat(2)=ibuf(ipp)
#else
	      jdat(2)=0
	      jdat(1)=ibuf(ipp)
#endif
	      det(jk).ival(jj)=ldat
	      if(ldat.gt.0) then
	        det(jk).xval(jj)=ldat+ran(iseed)
	      else
	        det(jk).xval(jj)=0.
	      endif
	    enddo
	    ipuntdet=ipp+1

	  elseif(itipo.eq.4) then		        !!!!!!!!!!!!!!!!! CLUSTER-FULL test 30/12/97
	    if(idetn.eq.8) then
	      idetn=0
	      idoff=CSEG*idetn
	      do ii=0,CSEG-1
	        if(iand(ipat,pattern(ii)).ne.0) then
	          jk=jk+1
	          if(NDET(IQ).eq.0) DOFF(IQ)=jk
	          NDET(IQ)=NDET(IQ)+1
	          det(jk).id=idoff+ii
	          do jj=0,2	! NDPAR(IQ)-1
	            ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	            jdat(1)=0
	            jdat(2)=ibuf(ipp)
#else
	            jdat(2)=0
	            jdat(1)=ibuf(ipp)
#endif
	            det(jk).ival(jj)=ldat
	            if(ldat.gt.0) then
	              det(jk).xval(jj)=ldat+ran(iseed)
	            else
	              det(jk).xval(jj)=0.
	            endif
	          enddo
	        endif
              enddo
	      do ii=7,7+1-1
	        if(iand(ipat,pattern(ii)).ne.0) then
	          jk=jk+1
	          if(NDET(IQ).eq.0) DOFF(IQ)=jk
	          NDET(IQ)=NDET(IQ)+1
	          det(jk).id=idoff+ii
	          do jj=0,1
	            ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	            jdat(1)=0
	            jdat(2)=ibuf(ipp)
#else
	            jdat(2)=0
	            jdat(1)=ibuf(ipp)
#endif
	            det(jk).ival(jj)=ldat
	            if(ldat.gt.0) then
	              det(jk).xval(jj)=ldat+ran(iseed)
	            else
	              det(jk).xval(jj)=0.
	            endif
	          enddo
	          ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	          jdat(2)=ibuf(ipp)
	          ipp=ipp+1
	          jdat(1)=ibuf(ipp)
#else
	          jdat(1)=ibuf(ipp)
	          ipp=ipp+1
	          jdat(2)=ibuf(ipp)
#endif
		  ldat=ior(iand(ldat,'00003FF0'X)/16,iand(ldat,'00000FF0'X)*64)
	          det(jk).ival(2)=ldat
	          det(jk).xval(2)=ldat
		endif
	      enddo
	      do ii=8,8+18-1
	        if(iand(ipat,pattern(ii)).ne.0) then
	          jk=jk+1
	          if(NDET(IQ).eq.0) DOFF(IQ)=jk
	          NDET(IQ)=NDET(IQ)+1
	          det(jk).id=idoff+ii
	          do jj=0,1
	            ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	            jdat(1)=0
	            jdat(2)=ibuf(ipp)
#else
	            jdat(2)=0
	            jdat(1)=ibuf(ipp)
#endif
	            det(jk).ival(jj)=ldat
	            if(ldat.gt.0) then
	              det(jk).xval(jj)=ldat+ran(iseed)
	            else
	              det(jk).xval(jj)=0.
	            endif
	          enddo
	          det(jk).ival(2)=0
	          det(jk).xval(2)=0
	        endif
              enddo
	    endif
	    ipuntdet=ipuntdet+ifragl

	  else
	    write(6,*) ' Undefined detector type',itipo
	    ipuntdet=ipuntdet+ifragl

	  endif

	enddo

40	itot=0
	DO II=1,NTIPI
	  nn=NDET(ii)	          
	  if(nn.lt.FOLDMIN(ii)) goto 30
	  STAT.FOLD(nn,ii,0)=STAT.FOLD(nn,ii,0)+1
	  if(nn.gt.0) then
	    itot=itot+nn
	    do jp=NDPAR(ii),TDPAR(ii)-1
	      det(ii).ival(jp)=0
	      det(ii).xval(jp)=0.
	    enddo
	  endif
	enddo

	if(itot.gt.0) then
	  call EVANA
	  if(break) return
	endif

	GOTO 30

9999	return

	end

	subroutine SORT_8PI(IBUF,nwords,stat)

!  decodifica l'evento e lo passa alla routine di analisi
C	fix0		Esum del filtro (-1==> fine dati)
C	fix1		molteplicita'-1 del filtro
C	fix2		molteplicita'-2 del filtro
C	Nger		numero rivelatori tipo GPAR
C   	Ge-E,
C	Ge-KEY
C	Ge-T
C	...
C
#include "gsort.inc"
	record/statdat/stat

	logical*1 original
	INTEGER*2 IBUF(MAXWORDS)

	INTEGER*2 JDAT(2)
	INTEGER*4 LDAT
	EQUIVALENCE (JDAT,LDAT)
	INTEGER*2 IDAT
	
	ipunt=0
	ilast=nwords
	goto 35

30	ipunt=ipunt+1
	IF(IPUNT.GT.ILAST) goto 9999		! Finito --> prossimo buffer
	IDAT=IBUF(IPUNT)
	IF (idat.ne.-1) goto 30

35	ipunt=ipunt+1
	IF(IPUNT.GT.ILAST) goto 9999		! Finito --> prossimo buffer
	IDAT=IBUF(IPUNT)
	IF (idat.eq.-1) goto 35

	iio.evcount=iio.evcount+1

	do ii=0,ntipi
	  doff(ii)=0
	  ndet(ii)=0
	enddo

	DOFF(0)=0
	NDET(0)=1
	det(0).id=0
	ipunt = ipunt - 1
	do ii=0,NDPAR(0)-1
	  ipunt = ipunt + 1
#if defined( _GW_BIG_ENDIAN )
	  jdat(1)=0
	  jdat(2)=ibuf(ipunt)
#else
	  jdat(2)=0
	  jdat(1)=ibuf(ipunt)
#endif
	  det(0).ival(ii)=ldat
	  if(ldat.gt.0) then
	    det(0).xval(ii)=ldat+ran(iseed)
	  else
	    det(0).xval(ii)=0.
	  endif
	enddo
	do ii=NDPAR(0),TDPAR(0)-1
	  det(0).ival(ii)=0
	  det(0).xval(ii)=0.
	enddo

	ipunt=ipunt+1
	NDET(1) =ibuf(ipunt)
	if(NDET(1).lt.0) then
C	  CALL ANSI_BELL(LU1)
	  write(LU2,*) ' NDET(1)= ',ndet(1),iio.rec,iio.evcount
	  write(LU2,'(10i7)') (ibuf(ii),ii=max(1,ipunt-4),min(ilast,ipunt+5))
	  nlost=nlost+1
	  goto 35	!errore
	elseif(NDET(1).ge.20) then
C	  CALL ANSI_BELL(LU1)
	  write(LU2,*) ' NDET(1)= ',ndet(1),iio.rec,iio.evcount
	  write(LU2,'(10i7)') (ibuf(ii),ii=max(1,ipunt-4),min(ilast,ipunt+5))
	  nlost=nlost+1
	  goto 30	!errore
	endif
	if(NDET(1).lt.FOLDMIN(1)) then
	  ipunt = ipunt + 3 * NDET(1)
	  goto 30
	endif

	if(NDET(1).gt.0) then
	  doff(1)=1
	  nonvalid=0
	  jj=1
	  do ii=0,NDET(1)-1
	    ipunt=ipunt+1
	    jg=ibuf(ipunt)
	    jg=iand(jg,31)
	    if(jg.ge.0 .and. jg.lt.NITEMS(1)) then
	      det(jj).id=jg
	      do kk=0,NDPAR(1)-1
	        ipunt=ipunt+1
#if defined( _GW_BIG_ENDIAN )
	        jdat(1)=0
	        jdat(2)=ibuf(ipunt)
#else
	        jdat(2)=0
	        jdat(1)=ibuf(ipunt)
#endif
		ldat=iand(ldat,8191)
	        det(jj).ival(kk)=ldat
	        if(ldat.gt.0) then
	          det(jj).xval(kk)=ldat+ran(iseed)
	        else
	          det(jj).xval(kk)=0.
	        endif
	      enddo
	      do kk=NDPAR(1),TDPAR(1)-1
	        det(jj).ival(kk)=0
	        det(jj).xval(kk)=0.
	      enddo
	      jj=jj+1
	    else
	      nonvalid=nonvalid+1
	      ipunt=ipunt+3
	    endif
	  enddo
	  NDET(1)=NDET(1)-nonvalid
	  if(NDET(1).lt.FOLDMIN(1)) goto 30
	  STAT.FOLD(NDET(1),1,0)=STAT.FOLD(NDET(1),1,0)+1
	endif

	call EVANA
	if(break) return

	GOTO 30

9999	return

	end
