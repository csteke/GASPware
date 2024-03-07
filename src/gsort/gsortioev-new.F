
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
	integer tape_aioread,tape_aiocomplete
	INTEGER IAD1,IAD2
	integer nextpending/0/
	SAVE IAD1,IAD2,nextpending
	integer*4 tnd_read
	external tnd_read

	if(iio.datafile) then

	  if(dataformat .eq. tndb)then
	    tnd.current=tnd.current+1
	    nseen=tnd_read(iio.name,tnd.current,ievbuf)
	    if(nseen .eq. 0)then
	      nseen=-3
	      iio.rec=iio.rec-1
	      return
	    else
	      iio.rec=iio.rec+1
	    endif
	    iaddr=1
	    tnd_nadc=0
	    return
	  endif

	  if(iio.rec.le.1) then
	    iio.rec=1
	    iad1=1
	    iad2=MAXWORDS+1
	  endif
#if defined(USE_AIO)

	if( (iio.rec .gt. 1) .and. (nextpending .eq. 1) )then
	   istat = tape_aiocomplete(ievbuf(IAD1),iio.wlen*2,iio.mtch,iio.flag,iio.iosb)
	   nextpending=0

	elseif( (iio.rec .gt. 1) .and. (nextpending .eq. 0) )then
	   nseen=0
	   goto 411

	elseif( (iio.rec .le. 1) .and. (nextpending .eq. 1) )then
	   istat = tape_aiocomplete(ievbuf(IAD1),iio.wlen*2,iio.mtch,iio.flag,iio.iosb)
	   iio.rec=1
	   nextpending=0

	else
	   iio.rec=1
	   IAD1=1
	   IAD2=MAXWORDS+1
	   ISTAT=TAPE_READ(ievbuf(IAD1),iio.wlen*2,iio.mtch,iio.flag,iio.iosb)
	endif


410	CALL DISK_SYNCR(iio.flag,iio.REC,NSEEN,iio.iosb,iio.name,iio.mtch,ICONT)

	IF(NSEEN.LT.0) RETURN

411	IF(NSEEN.EQ.0) THEN
	   ISTAT=TAPE_READ(ievbuf(IAD1),iio.wlen*2,iio.mtch,iio.flag,iio.iosb)
	   GOTO 410				! leggine un altro
	ENDIF

	iio.REC=iio.REC+1
	call swapl(IAD1,IAD2)			! Swap puntatori buffer per PING-PONG

!!!!!!!!!!!!!!! prima di uscire lancia la prossima lettura !!!!!!!!!!!!!!!!!!!!
	ISTAT=TAPE_AIOREAD(ievbuf(IAD1),iio.wlen*2,iio.mtch,iio.flag,iio.iosb)
	nextpending=1
	IADDR=IAD2

	RETURN

#else
	  if(.NOT.iio.virtuale) read(iio.lun,rec=(iio.REC+1),err=5) (ievbuf(IAD1+ii),ii=0,iio.wlen-1)
	  call swapl(IAD1,IAD2)
	  iaddr=iad2
	  nseen=iio.wlen*2
	  iio.rec=iio.rec+1
	  return
5	  nseen=-3
	  return
#endif
       endif


*	IF(iio.REC.le.1) THEN		! primo record & start pong-pong
*	   iio.rec=1
*	   IAD1=1
*	   IAD2=MAXWORDS+1
*	   ISTAT=TAPE_READ(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
*#ifdef USE_AIO
*	   nextpending = 0
*       else
*	  if(nextpending.eq.1)then
*	     istat = tape_aiocomplete(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
*	     nextpending=0
*	  else
*	     nseen=0
*	     nextpending=0
*	     goto 11
*	  endif 	     
*#endif
*	ENDIF


	if( (iio.rec .gt. 1) .and. (nextpending .eq. 1) )then
#if defined(USE_AIO)
	   istat = tape_aiocomplete(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
#endif
	   nextpending=0

	elseif( (iio.rec .gt. 1) .and. (nextpending .eq. 0) )then
#if defined(USE_AIO)
	   nseen=0
	   goto 11
#else
	   goto 10
#endif

	elseif( (iio.rec .le. 1) .and. (nextpending .eq. 1) )then
#if defined(USE_AIO)
	   istat = tape_aiocomplete(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
#endif
	   iio.rec=1
	   nextpending=0

	else
	   iio.rec=1
	   IAD1=1
	   IAD2=MAXWORDS+1
	   ISTAT=TAPE_READ(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
	endif


10	CALL TAPE_SYNCR(iio.flag,iio.REC,NSEEN,iio.iosb,iio.name,iio.mtch,ICONT)

	IF(NSEEN.LT.0) RETURN

11	IF(NSEEN.EQ.0) THEN
	   ISTAT=TAPE_READ(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
	   GOTO 10				! leggine un altro
	ENDIF

	IF(NSEEN.EQ.80 .OR. NSEEN.EQ.256) THEN	! Presumibilmente un header
	   WRITE(LU2,*)
	   CALL gs_list_bytes(ievbuf(IAD1),NSEEN,LU1)
	   CALL gs_list_bytes(ievbuf(IAD1),NSEEN,LU2)
	   WRITE(LU2,*)
	   ISTAT=TAPE_READ(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
	   GOTO 10				! leggine un altro
	ENDIF

	iio.REC=iio.REC+1
	call swapl(IAD1,IAD2)			! Swap puntatori buffer per PING-PONG

!!!!!!!!!!!!!!! prima di uscire lancia la prossima lettura !!!!!!!!!!!!!!!!!!!!
#ifndef USE_AIO
	ISTAT=TAPE_READ(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
#else
	ISTAT=TAPE_AIOREAD(ievbuf(IAD1),MAXBYTES,iio.mtch,iio.flag,iio.iosb)
	nextpending=1
#endif
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
	integer l_nm_mask


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
	  l_nm_mask='C000'X
	  if(iand(class,l_nm_mask).ne.0)then
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
	      det(0).xval(ii)=ldat+rand_real2()
	    else
	      det(0).xval(ii)=0.
	    endif
	  enddo
	  do ii=NDPAR(0),TDPAR(0)-1
	    det(0).ival(ii)=0
	    det(0).xval(ii)=0.0E0
	  enddo
	endif

	do ji=1,ntipi
	  if(ndpar(ji) .GT. 0) then
	    ipuntev=ipuntev+1	    
	    NDET(ji) = ibuf(ipuntev)
	    if(original) ipuntev=ipuntev+1
	  else
	    NDET(ji) = 0
	  endif	    
	    DOFF(ji) = DOFF(ji-1) + NDET(ji-1)
	    if(NDET(ji).lt.FOLDMIN(ji)) goto 30
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
	            det(jj).xval(kk)=ldat+rand_real2()
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
	
	INTEGER*2 JDAT(2),evtype,evlength
	INTEGER*2 evtype_next,evlength_next
	INTEGER*4 LDAT, ipuntev, getevtypelength
	INTEGER*4  ipuntev_next
	external getevtypelength
	EQUIVALENCE (JDAT,LDAT)

	integer pattern(0:31)
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
	
	

	ipuntev=16				! inizio dati
	ilast=nwords

*	ipuntev = getevtypelength(ibuf,nwords,evtype,evlength)
*	write(6,*)' Event off,type,length : ',ipuntev,evtype,evlength,nwords

30	ipuntev = ipuntev + getevtypelength(ibuf(ipuntev),nwords-ipuntev+1,evtype,evlength)
	IF(IPUNTEV.GT.ILAST) goto 9999		! Finito --> prossimo buffer
	if( evlength .le. 2) then
	   nlost = nlost + 1
	   goto 30
	endif
	
	ipuntev_next = ipuntev+1+getevtypelength(ibuf(ipuntev+1),nwords-ipuntev,evtype_next,evlength_next)
	ievl = evlength/2
	if(ievl.gt.MAXEVL) stop ' Event too long in SORT_EURO'
	ievl_check = ipuntev_next - ipuntev
*	write(6,*)'  Ev # ',iio.evcount+1
*	call flush(6)
	if(ievl.gt.ievl_check) then
*		write(6,*) 'WARNING : inconsistent event length ',ievl,ievl_check
*		ipuntev = ipuntev_next
		goto 9999
	endif
	
	if (evtype .eq. 0) then
	  ipunt = ipuntev + 2
	  ipuntdet = ipunt
	elseif (evtype .eq. 1) then
	  ipunt = ipuntev + 2
	  ipuntdet = ipunt
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
	elseif (evtype .eq. 2) then
	  ipunt = ipuntev + 3
	  ipuntdet = ipunt
	elseif (evtype .eq. 3) then
	  ipunt = ipuntev + 3
	  ipuntdet = ipunt
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
	else
	  nlost = nlost + 1 
	  goto 30
	endif

	iio.evcount=iio.evcount+1
	ipuntev = ipuntev + ievl

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

	ipuntdet_old = ipuntdet-1
	do while(ipuntdet.lt.ipuntev)
	   if(ipuntdet_old .eq. ipuntdet) goto 30
	   ipuntdet_old = ipuntdet
	   call flush(6)
#if defined( _GW_BIG_ENDIAN )
	  jdat(1)=0
	  jdat(2)=ibuf(ipuntdet)
#else
	  jdat(2)=0
	  jdat(1)=ibuf(ipuntdet)
#endif

	  ipp=ipuntdet
	  idetn=iand(ldat,'000001FF'X)
	  itipo=iand(ldat,'00003E00'X)/512
	  iform=iand(ldat,'0000C000'X)/16384

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
c	            det(jk).xval(jj)=ldat+rand_real2()
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
	            det(jk).xval(jj)=ldat+rand_real2()
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
	            det(jk).xval(jj)=ldat+rand_real2()
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
	            det(jk).xval(jj)=ldat+rand_real2()
	          else
	            det(jk).xval(jj)=0.
	          endif
	        enddo
	      endif
            enddo
	    ipuntdet=ipuntdet+ifragl

**********************************************
	  elseif(itipo.eq.14) then			!!!!!!!!!!!!!!!!! EUCLIDES
	   if( idetn .lt. NDETS(IQ) )then
	    idoff=SISEG*idetn
	    do ii=0,SISEG-1
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
	            det(jk).xval(jj)=ldat+rand_real2()
	          else
	            det(jk).xval(jj)=0.
	          endif
	        enddo
	      endif
            enddo
	    endif
	    ipuntdet=ipuntdet+ifragl
***************

	  elseif((itipo.GE.5 .OR. itipo.LE.8) .and. (itipo .ne. 14)) then	!!!!!!!!!!!!!!!!! Nwall or ISIS
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
	        det(jk).xval(jj)=ldat+rand_real2()
	      else
	        det(jk).xval(jj)=0.
	      endif
	    enddo
	    ipuntdet = ipp+1

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
	              det(jk).xval(jj)=ldat+rand_real2()
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
	              det(jk).xval(jj)=ldat+rand_real2()
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
	              det(jk).xval(jj)=ldat+rand_real2()
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
	    det(0).xval(ii)=ldat+rand_real2()
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
	          det(jj).xval(kk)=ldat+rand_real2()
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



	subroutine SORT_GAMMASPHERE(IBUF,nwords,stat)

!  decodifica l'evento e lo passa alla routine di analisi

#include "gsort.inc"
	record/statdat/stat

	INTEGER*2 IBUF(MAXWORDS)

	INTEGER*2 JDAT(2)
	INTEGER*4 LDAT
	EQUIVALENCE (JDAT,LDAT)
	INTEGER*2 IDAT,nm_mask, nw_lecroy
	
	integer get_gsph_headtype, get_gsph_byteorder, gsII_bufp, get_lecroy
	external get_gsph_headtype, get_gsph_byteorder, gsII_bufp, get_lecroy
	
	logical*1 gsph_bswap,badge
	save gsph_bswap
	
	structure /GSPH_HEADER/
	   integer*2 par(11)
	end structure
	record /GSPH_HEADER/gs2hdr

	structure /GSPH_DETECTOR/
		integer*2 id
		integer*2 gebit
		integer*2 bgohit
		integer*2 ehi
		integer*2 pu
		integer*2 over
		integer*2 tge
		integer*2 tc
		integer*2 elo
		integer*2 eside
		integer*2 tbgo
		integer*2 ebgo
	end structure
	
	structure /MICROBALL_DETECTOR/
		integer*2 id
		integer*2 e
		integer*2 t
		integer*2 p
	end structure
		
	structure /NSHELL_DETECTOR/
		integer*2 id
		integer*2 eh
		integer*2 el
		integer*2 p
		integer*2 tof
	end structure

	record /GSPH_DETECTOR/gs2det(120)
	record /MICROBALL_DETECTOR/ubdet(120)
	record /NSHELL_DETECTOR/nsdet(120)

	
	integer ix_ge, ix_ub, ix_ns, firstTime
	save ix_ge, ix_ub, ix_ns, firstTime

	if(get_gsph_headtype(ibuf(1)).eq.1) then	! this is a tape header
	  CALL gs_list_bytes(ibuf,256,LU1)	
	  CALL gs_list_bytes(ibuf,256,LU2)
	  ii=get_gsph_byteorder(ibuf(4),ibuf(5))
	  if( ii .eq. 0 )gsph_bswap = .false.
	  if( ii .eq. 1 )gsph_bswap = .true.
	  if( ii .lt. 0 )stop ' Unimplemented byte order type'
	  return
	endif
	if(get_gsph_headtype(ibuf(1)).eq.2) then	! this is a file header
	  CALL gs_list_bytes(ibuf,256,LU1)	
	  CALL gs_list_bytes(ibuf,256,LU2)
	  return
	endif
	if(get_gsph_headtype(ibuf(1)).ne.3) then	! unknown header type
	  write( lu1, '(''  Unknown record type'')')
	  write( lu2, '(''  Unknown record type'')')
	  CALL gs_list_bytes(ibuf,256,LU1)	
	  CALL gs_list_bytes(ibuf,256,LU2)
	  return
	else
	  gsph_bswap = .true.
	  if( (ibuf(1) .eq. 3) .and. (ibuf(3) .eq. 1) ) gsph_bswap = .false.
	endif
	
	if(gsph_bswap)call gs_swap_bytes(ibuf(1),nwords)

	if( firstTime .eq. 0 ) then 
	    ix_ge = -1
	    ix_ub = -1
	    ix_ns = -1
	
	    do ii=0,ntipi
	      if( euromap(ii) .eq. 1 )ix_ge = ii
	      if( euromap(ii) .eq. 2 )ix_ub = ii
	      if( euromap(ii) .eq. 3 )ix_ns = ii  
	    enddo
	    firstTime = 1
	    if( ix_ge .lt. 1 ) then
	      write(*,*)' Gammasphere format error:  Ge detectors not defined, exiting...'
	      call exit(0)
	    endif
	endif
	

	do while (.true.)
	  do ii = 1,120
		gs2det(ii).id=0
		gs2det(ii).gebit=0
		gs2det(ii).bgohit=0
		gs2det(ii).ehi=0
		gs2det(ii).pu=0
		gs2det(ii).over=0
		gs2det(ii).tge=0
		gs2det(ii).tc=0
		gs2det(ii).elo=0
		gs2det(ii).eside=0
		gs2det(ii).tbgo=0
		gs2det(ii).ebgo=0

		ubdet(ii).id = -1
		ubdet(ii).e = 0
		ubdet(ii).t = 0
		ubdet(ii).p = 0
		
		nsdet(ii).id = -1
		nsdet(ii).eh = 0
		nsdet(ii).el = 0
		nsdet(ii).p = 0
		nsdet(ii).tof = 0
	  enddo

	 ge2ext = 0
	 gs2error=gsII_bufp(ibuf,gs2hdr,gs2det, ubdet, nsdet, nw_lecroy )
	 if(gs2error .eq. 1)goto 100
	 if(gs2error .ne. 0)goto 100
	 
	  
	 iio.evcount=iio.evcount+1

	 
	do ii=0,ntipi
	  doff(ii)=0
	  ndet(ii)=0
	enddo

	if(EXISTS(0)) then
	  DOFF(0)=0
	  NDET(0)=1
	  det(0).id=0
	  do ii=0,NDPAR(0)-1
	    if( ii.eq.0 )det(0).ival(0)=gs2hdr.par(10)
	    if( ii.eq.1 )det(0).ival(1)=gs2hdr.par(11)
	    if( ii.eq.2 )det(0).ival(2)=gs2hdr.par(8)
	    if( ii.eq.3 )det(0).ival(3)=gs2hdr.par(9)
	  enddo
	  do ii=NDPAR(0),TDPAR(0)-1
	    det(0).ival(ii)=0
	    det(0).xval(ii)=0.
	  enddo
	  do ii=0,NDPAR(0)-1
	    det(0).xval(ii)=det(0).ival(ii)+rand_real2()
	  enddo
	endif

	NDET(ix_ge) =gs2hdr.par(2)+gs2hdr.par(3)
	
	DOFF(ix_ge) = DOFF(0) + NDET(0)
	if(NDET(ix_ge).lt.FOLDMIN(ix_ge)) goto 30
	kk=0
	nonvalid=0
	do jj=DOFF(ix_ge), DOFF(ix_ge)+NDET(ix_ge)-1
10	  kk=kk+1
	  if(kk .gt. ndet(ix_ge)) goto 20
	  badge = (gs2det(kk).gebit .ne. 1) .or. (gs2det(kk).pu .eq. 1) .or. (gs2det(kk).over .eq. 1)
	  if(badge)then
	    nonvalid=nonvalid+1
	    goto 10
	  endif
	  det(jj).id=gs2det(kk).id-1
	  do ii=0,NDPAR(ix_ge)-1
	    if( ii.eq.0 )det(jj).ival(0)=gs2det(kk).ehi
	    if( ii.eq.1 )det(jj).ival(1)=gs2det(kk).tge
	    if( ii.eq.2 )det(jj).ival(2)=gs2det(kk).elo
	    if( ii.eq.3 )det(jj).ival(3)=gs2det(kk).eside
	    if( ii.eq.4 )det(jj).ival(4)=gs2det(kk).ebgo
	    if( ii.eq.5 )det(jj).ival(5)=gs2det(kk).tbgo
	    if( ii.eq.6 )det(jj).ival(6)=gs2det(kk).gebit
	    if( ii.eq.7 )det(jj).ival(7)=gs2det(kk).bgohit
	  enddo
	  do ii=0,NDPAR(ix_ge)-1
	    det(jj).xval(ii)=det(jj).ival(ii)
	    det(jj).xval(ii)=det(jj).xval(ii)+rand_real2()
	  enddo
	  do ii=NDPAR(ix_ge),TDPAR(ix_ge)-1
	    det(jj).ival(ii)=0
	    det(jj).xval(ii)=0.
	  enddo
	enddo
20	NDET(ix_ge)=NDET(ix_ge)-nonvalid
	STAT.FOLD(NDET(ix_ge),ix_ge,0)=STAT.FOLD(NDET(ix_ge),ix_ge,0)+1

	if( nw_lecroy .le. 0 ) goto 25
	
	do ii = 1,100
	   if( (ubdet(ii).id  .ge. 0) .and. (ix_ub .gt. 0) ) NDET(ix_ub) =  NDET(ix_ub)+1
	   if( (nsdet(ii).id  .ge. 0) .and. (ix_ns .gt. 0) ) NDET(ix_ns) =  NDET(ix_ns)+1
	enddo

	if( ix_ub .gt. 0 ) then
	     DOFF(ix_ub) = DOFF(ix_ge) + NDET(ix_ge)
	     ix = ix_ub
	     jjub = DOFF(ix_ub)
	else
	     ix = ix_ge
	endif
	
	if( ix_ns .gt. 0 )then
	   DOFF(ix_ns) = DOFF(ix) + NDET(ix)
	   jjns = DOFF(ix_ns)
	endif

	
	do kk = 1,100
	   if( ix_ub .gt. 0 ) then
	     if( ubdet(kk).id  .ge. 0 ) then
	       det(jjub).id=ubdet(kk).id
	       do ii=0,NDPAR(ix_ub)-1
	     	 if( ii.eq.0 )det(jjub).ival(0)=ubdet(kk).e
	     	 if( ii.eq.1 )det(jjub).ival(1)=ubdet(kk).t
	     	 if( ii.eq.2 )det(jjub).ival(2)=ubdet(kk).p
	       enddo
	       do ii=0,NDPAR(ix_ub)-1
	     	 det(jjub).xval(ii)=det(jjub).ival(ii)
	     	 det(jjub).xval(ii)=det(jjub).xval(ii)+rand_real2()
	       enddo
	       do ii=NDPAR(ix_ub),TDPAR(ix_ub)-1
	     	 det(jjub).ival(ii)=0
	     	 det(jjub).xval(ii)=0.
	       enddo
	       jjub = jjub + 1
	     endif
	   endif
	       
	   if( ix_ns .gt. 0 ) then
	     if( nsdet(kk).id  .ge. 0 ) then
	       det(jjns).id=nsdet(kk).id
	       do ii=0,NDPAR(ix_ns)-1
	     	 if( ii.eq.0 )det(jjns).ival(0)=nsdet(kk).eh
	     	 if( ii.eq.1 )det(jjns).ival(1)=nsdet(kk).el
	     	 if( ii.eq.2 )det(jjns).ival(2)=nsdet(kk).p
	     	 if( ii.eq.3 )det(jjns).ival(3)=nsdet(kk).tof
	       enddo
	       do ii=0,NDPAR(ix_ns)-1
	     	 det(jjns).xval(ii)=det(jjns).ival(ii)
	     	 det(jjns).xval(ii)=det(jjns).xval(ii)+rand_real2()
	       enddo
	       do ii=NDPAR(ix_ns),TDPAR(ix_ns)-1
	     	 det(jjns).ival(ii)=0
	     	 det(jjns).xval(ii)=0.
	       enddo
	       jjns = jjns + 1
	     endif
	   endif
	enddo
	if( ix_ub .gt. 0 )STAT.FOLD(NDET(ix_ub),ix_ub,0)=STAT.FOLD(NDET(ix_ub),ix_ub,0)+1
	if( ix_ns .gt. 0 )STAT.FOLD(NDET(ix_ns),ix_ns,0)=STAT.FOLD(NDET(ix_ns),ix_ns,0)+1
	
	
25	call EVANA
30	continue
	if(break) return

	enddo
100	return
	end
	

	subroutine SORT_YALE(IBUF,nwords,stat)

!  decodifica l'evento e lo passa alla routine di analisi

#include "gsort.inc"
	record/statdat/stat

	INTEGER*2 IBUF(MAXWORDS)

	INTEGER*2 JDAT(2)
	INTEGER*4 LDAT
	EQUIVALENCE (JDAT,LDAT)

	structure /YALE_DETECTOR/
		integer*2 id
		integer*2 energy
		integer*2 time
		integer*2 dummy
	end structure
	
	structure /YALE_EVENT/
		integer*2  coax_fold
		integer*2  clover_fold
		integer*2  leps_fold
		integer*2  part_fold
		record /YALE_DETECTOR/coax(0:MAXDET-1)
		record /YALE_DETECTOR/clover(0:MAXDET-1)
		record /YALE_DETECTOR/leps(0:MAXDET-1)
		record /YALE_DETECTOR/particle(0:MAXDET-1)
	end structure
	
	record /YALE_EVENT/yev
	
	MACHINE_POINTERSIZE  bufptr, endpntr
	MACHINE_POINTERSIZE  get_yalevent
	external get_yalevent
	integer yev_status
	
	bufptr = %LOC(ibuf(13))
	endpntr = %LOC(ibuf(nwords))
	
	do while ( .TRUE. )
		bufptr = get_yalevent( bufptr, endpntr, yev, yev_status )
		if( yev_status .eq. 1 ) then
		    iio.evcount=iio.evcount+1
		    do ii=0,ntipi
		      doff(ii)=0
		      ndet(ii)=0
		    enddo
		    if( EXISTS(0) ) then
		      DOFF(0)=0
	  	      NDET(0)=1
	  	      det(0).id=0
	  	      do ii=0,TDPAR(0)-1
	  	    	det(0).ival(ii)=0
			det(0).xval(ii)=0
	  	      enddo
	              STAT.FOLD(NDET(0),0,0)=STAT.FOLD(NDET(0),0,0)+1
		     endif
		     
		     if( EXISTS(1) ) then
		     DOFF(1) = DOFF(0)+NDET(0)
		     NDET(1) = yev.coax_fold
		     do ii = 0,yev.coax_fold-1
		        ij = ii + DOFF(1)
			det(ij).id   = yev.coax(ii).id
			det(ij).ival(0) = yev.coax(ii).energy
			det(ij).xval(0) = det(ij).ival(0)
			det(ij).xval(0) = det(ij).xval(0)+rand_real2()
			det(ij).ival(1) = yev.coax(ii).time
			det(ij).xval(1) = det(ij).ival(1)
			det(ij).xval(1) = det(ij).xval(1)+rand_real2()
			do jj=2,TDPAR(1)-1
	  	    	   det(ij).ival(jj)=0
			   det(ij).xval(jj)=0
			enddo
	  	      enddo
	              STAT.FOLD(NDET(1),1,0)=STAT.FOLD(NDET(1),1,0)+1
		      endif
		     
		     if( EXISTS(2) ) then
		     DOFF(2) = DOFF(1)+yev.coax_fold
		     NDET(2) = yev.clover_fold
		     do ii = 0,yev.clover_fold-1
		        ij = ii + DOFF(2)
			det(ij).id   = yev.clover(ii).id
			det(ij).ival(0) = yev.clover(ii).energy
			det(ij).xval(0) = det(ij).ival(0)
			det(ij).xval(0) = det(ij).xval(0)+rand_real2()
			det(ij).ival(1) = yev.clover(ii).time
			det(ij).xval(1) = det(ij).ival(1)
			det(ij).xval(1) = det(ij).xval(1)+rand_real2()
			do jj=2,TDPAR(2)-1
	  	    	   det(ij).ival(jj)=0
			   det(ij).xval(jj)=0
			enddo
	  	      enddo
	              STAT.FOLD(NDET(2),2,0)=STAT.FOLD(NDET(2),2,0)+1
		      endif

		     if( EXISTS(3) ) then
		     DOFF(3) = DOFF(2)+yev.clover_fold
		     NDET(3) = yev.leps_fold
		     do ii = 0,yev.leps_fold-1
		        ij = ii + DOFF(3)
			det(ij).id   = yev.leps(ii).id
			det(ij).ival(0) = yev.leps(ii).energy
			det(ij).xval(0) = det(ij).ival(0)
			det(ij).xval(0) = det(ij).xval(0)+rand_real2()
			det(ij).ival(1) = yev.leps(ii).time
			det(ij).xval(1) = det(ij).ival(1)
			det(ij).xval(1) = det(ij).xval(1)+rand_real2()
			do jj=2,TDPAR(2)-1
	  	    	   det(ij).ival(jj)=0
			   det(ij).xval(jj)=0
			enddo
	  	      enddo
	              STAT.FOLD(NDET(3),3,0)=STAT.FOLD(NDET(3),3,0)+1
		      endif

		     if( EXISTS(4) ) then
		     DOFF(4) = DOFF(3)+yev.leps_fold
		     NDET(4) = yev.part_fold
		     do ii = 0,yev.part_fold-1
		        ij = ii + DOFF(4)
			det(ij).id   = yev.particle(ii).id
			det(ij).ival(0) = yev.particle(ii).energy
			det(ij).xval(0) = det(ij).ival(0)
			det(ij).xval(0) = det(ij).xval(0)+rand_real2()
			det(ij).ival(1) = yev.particle(ii).time
			det(ij).xval(1) = det(ij).ival(1)
			det(ij).xval(1) = det(ij).xval(1)+rand_real2()
			do jj=2,TDPAR(2)-1
	  	    	   det(ij).ival(jj)=0
			   det(ij).xval(jj)=0
			enddo
	  	      enddo
	              STAT.FOLD(NDET(4),4,0)=STAT.FOLD(NDET(4),4,0)+1
		      endif
		      
		      !write(6,*)' GSORT folds : ',yev.coax_fold,yev.clover_fold
		      call EVANA
		      if(break) return
		elseif (yev_status .eq. 0)then
			return
		endif
	enddo
	
	end
	


	subroutine SORT_GSPN(IBUF,nwords,stat)

!  decodifica l'evento e lo passa alla routine di analisi

#include "gsort.inc"
	record/statdat/stat

	INTEGER*2 IBUF(MAXWORDS)
	INTEGER*2 IDAT
	
	INTEGER*2 JDAT(2),evtype,evlength
	INTEGER*4 LDAT, ipuntev, getevtypelength
	integer*4 has_header
	external getevtypelength
	EQUIVALENCE (JDAT,LDAT)

	logical*1 initialize	/.TRUE./
	integer quale(0:31)

	if(initialize) then    ! better to be in other place!!!
	  do ii=0,31
	    quale(ii)=-1
	  enddo
	  do ii=0,NDTYPE
	    if(gaspmap(ii).ne.-1) then
	      quale(gaspmap(ii))=ii
	    endif
	  enddo
	  if( exists(0) )has_header = 1
	  else has_header = 0
	  initialize=.FALSE.
	endif                  ! 

	ipuntev=16				! inizio dati
	ilast=nwords

*	ipuntev = getevtypelength(ibuf,nwords,evtype,evlength)
*	write(6,*)' Event off,type,length : ',ipuntev,evtype,evlength,nwords

30	ipuntev = ipuntev + getevtypelength(ibuf(ipuntev),nwords-ipuntev+1,evtype,evlength)
	IF(IPUNTEV.GT.ILAST) goto 9999		! Finito --> prossimo buffer
	if( evlength .le. 2) then
	   nlost = nlost + 1
	   goto 30
	endif
	ievl = evlength/2
	if(ievl.gt.MAXEVL) stop ' Event too long in SORT_GSPN'
	if (evtype .eq. 0) then
	  ipunt = ipuntev + 2
	  ipuntdet = ipunt
	elseif (evtype .eq. 1) then
	  ipunt = ipuntev + 2
	  ipuntdet = ipunt
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
	elseif (evtype .eq. 2) then
	  ipunt = ipuntev + 3
	  ipuntdet = ipunt
	elseif (evtype .eq. 3) then
	  ipunt = ipuntev + 3
	  ipuntdet = ipunt
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
	else
	  nlost = nlost + 1 
	  goto 30
	endif

	iio.evcount=iio.evcount+1
	ipuntev = ipuntev + ievl

*	if(exists(0)) then
*	  ndet(0)=0
*	  doff(0)=0
*	  do jp=0,TDPAR(0)-1
*	    det(0).ival(jp)=0
*	    det(0).xval(jp)=0.
*	  enddo
*	  jk=0
*	else
*	  ndet(0)=0
*	  jk=-1
*	endif
	
	jk = has_header - 1
	
	DO II=0,NTIPI
	  doff(ii)=0
	  ndet(ii)=0
	enddo

	do while(ipuntdet.lt.ipuntev)
#if defined( _GW_BIG_ENDIAN )
	  jdat(1)=0
	  jdat(2)=ibuf(ipuntdet)
#else
	  jdat(2)=0
	  jdat(1)=ibuf(ipuntdet)
#endif
	  ipp=ipuntdet
	  idetn=iand(ldat,'000001FF'X)
	  itipo=iand(ldat,'00003E00'X)/512
	  iform=iand(ldat,'0000C000'X)/16384

	  if(iform.ne.0) then
	    write(6,*) ' Skipping rest of event because of unknown format',iform
	    goto 30
	  endif

*	  if(itipo.lt.11 .OR. itipo.gt.14) then
*	    write(6,*) ' Undefined detector type',itipo
*	    goto 30
*	  endif

	  IQ=quale(itipo) 
	  if(IQ .LT. 0) then
	    write(6,*) ' Skipping rest of event because of undefined detector type',itipo
	    goto 30
	  endif

	  if(IQ.ne.0) then
	    jk=jk+1
	    if(NDET(IQ).eq.0) DOFF(IQ)=jk
	    NDET(IQ)=NDET(IQ)+1
	    det(jk).id=idetn
	    jkl = jk
	  else
	    if(NDET(0).ne.0) then
	      write(6,*) ' Skipping rest of event because of repeated header'
	      goto 30
	    endif
	    NDET(0)=1
	    det(0).id = 0
	    jkl = 0
	  endif

	  do jj=0,NDPAR(IQ)-1
	    ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	    jdat(1)=0
	    jdat(2)=ibuf(ipp)
#else
	    jdat(2)=0
	    jdat(1)=ibuf(ipp)
#endif
	    det(jkl).ival(jj)=ldat
	    if(ldat.gt.0) then
	      det(jkl).xval(jj)=ldat+rand_real2()
	    else
	      det(jkl).xval(jj)=0.
	    endif
	  end do
	  ipuntdet = ipp+1
	end do

40	itot=0


	DO II=0,NTIPI
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

	if( (has_header .gt. 0) .and. (ndet(0).eq.0) ) then
	  doff(0) = 0
*	  do ii = 1, ntipi
*	    doff(0) = doff(0) + NDET(ii)
*	  enddo
	  
	  ndet(0)=1
	  do jp=0,TDPAR(0)-1
	    det(0).ival(jp)=0
	    det(0).xval(jp)=0.
	  enddo
	endif

	if(itot.gt.0) then
	  call EVANA
	  if(break) return
	endif

	GOTO 30

9999	return

	end



	subroutine SORT_TANDEM(ibuf,nwords,stat)
	
#include "gsort.inc"
	
	integer nwords
	record /statdat/stat
	integer*2 ibuf(1)
	integer*2 jdat(2)
	integer*4 tnd_nadc
	logical*1 hasfix,hasvar,isvalid
	INTEGER*4 LDAT
	EQUIVALENCE (JDAT,LDAT)
*#ifdef PClinux
*	real rbuf(:)
*#else
	real rbuf(16*MAXWORDS)
*#endif
	integer*4 tnd_decode
	external tnd_decode



	tnd_nadc=0
	hasfix=.false.
	hasvar=.false.
	if(exists(0))then
	  tnd_nadc=tnd_nadc+ndpar(0)
	  hasfix=.true.
	endif
	if(ntipi .gt. 0)then
	 hasvar=.true.
	 do ii=1,ntipi
	  tnd_nadc=tnd_nadc+ndpar(ii)*nitems(ii)
	 enddo
	endif
	if(tnd.presorted)nwords=tnd_decode(ibuf(1),nwords,tnd_nadc)
	if(nwords .lt. tnd_nadc)return
*#ifdef PClinux
*	allocate(rbuf(0:nwords))
*#endif

	do ii=0,nwords-1
	  rbuf(ii)=ibuf(ii+1)
	  rbuf(ii)=rbuf(ii)+rand_real2()
	enddo

	    

	do ii=0,ntipi
	  ndet(ii)=0
	  doff(ii)=0
	enddo
	
	do iit=0,nwords-1,tnd_nadc
	   do jj=0,tdpar(0)-1
	     det(0).id=0
	     det(0).ival(jj)=0
	     det(0).xval(jj)=0.0000000000000000000000000000
	   enddo
	   iiadc=0
	   if(hasfix)then
	     NDET(0)=1
	     DOFF(0)=0
	     do jj=0,ndpar(0)-1
	       det(0).id=0
	       det(0).ival(jj)=rbuf(iit+iiadc)
	       det(0).xval(jj)=rbuf(iit+iiadc)
	       iiadc=iiadc+1
	     enddo
	   endif
*****	
	  nm_offset=NDET(0)
          do ji=1,ntipi
	   if(ndpar(ji) .GT. 0) then
	    NDET(ji) = NITEMS(ji)
	    DOFF(ji) = DOFF(ji-1) + NDET(ji-1)
	   else
	    NDET(ji) = 0
	    DOFF(ji) = DOFF(ji-1) + NDET(ji-1)
	   endif
	 enddo



	do ji=1,ntipi
	  if(NDET(ji).gt.0) then
	    nonvalid=0
	    jj=nm_offset !DOFF(ji)
	    DOFF(ji)=nm_offset
	    do ii=0,NDET(ji)-1
	      jg=ii
	      if(jg.ge.0 .and. jg.lt.NITEMS(ji)) then
	        ioldid=det(jj).id
	        det(jj).id=ii
	        isvalid=.true.
		ivpar = 0
	        do kk=0,NDPAR(ji)-1
		 jdat(2)=0
		 jdat(1)=rbuf(iit+iiadc)
		 if(ldat .lt. 0)ldat=0
	         det(jj).ival(kk)=ldat
		 if(ldat .gt. 0)ivpar = ivpar+1  
	         det(jj).xval(kk)=rbuf(iit+iiadc)
	         iiadc=iiadc+1
		enddo
		isvalid = ivpar .gt. 0
		if((.not.isvalid) .and. tnd.presorted)then
		   do kk=0,NDPAR(ji)-1
		      det(jj).id=ioldid
		      det(jj).ival(kk)=0
		      det(jj).xval(kk)=0.00000000000
		   enddo
		   nonvalid=nonvalid+1
****
	          do kk=NDPAR(ji),TDPAR(ji)-1
	            det(jj).ival(kk)=0
	            det(jj).xval(kk)=0.0000000000
	          enddo
		 else
	           do kk=NDPAR(ji),TDPAR(ji)-1
	             det(jj).ival(kk)=0
	             det(jj).xval(kk)=0.0000000000
	           enddo
	           jj=jj+1
		 endif
	      else
	        nonvalid=nonvalid+1
	      endif
	    enddo
	    NDET(ji)=NDET(ji)-nonvalid
	    nm_offset=nm_offset+NDET(ji)
	    STAT.FOLD(NDET(ji),ji,0)=STAT.FOLD(NDET(ji),ji,0)+1
	  endif
	enddo

******
*	   if(hasvar .and. .false.)then
*	     nonvalid=0
*	     do ij=1,ntipi
*	       NDET(ij)=1
*	       DOFF(ij)=DOFF(ij-1)+NDET(ij-1)
*	       det(ij).id=0
*	       do jj=1,ndpar(ij)
*	         det(ij).ival(jj-1)=ibuf(ii+iiadc)
*	         det(ij).xval(jj-1)=float(ibuf(ii+iiadc))+rand_real2()
*	         iiadc=iiadc+1
*	       enddo
*	     enddo
*	    endif
	  iio.evcount=iio.evcount+1
	  call EVANA

	enddo
*#ifdef PClinux
*	deallocate(rbuf)
*#endif
	return
	end





	subroutine SORT_PRISMA(IBUF,nwords,stat)

!  decodifica l'evento e lo passa alla routine di analisi

#include "gsort.inc"
	record/statdat/stat

	INTEGER*2 IBUF(MAXWORDS)
	INTEGER*2 IDAT
	
	INTEGER*2 JDAT(2)
	integer*2 evtype,evlength
	integer*2 evtype_next,evlength_next
	INTEGER*4 LDAT, ipuntev, getevtypelength
	external getevtypelength
	EQUIVALENCE (JDAT,LDAT)

	integer pattern(0:31)
	data pattern/	'00000001'X,'00000002'X,'00000004'X,'00000008'X,'00000010'X,'00000020'X,'00000040'X,'00000080'X,
	1		'00000100'X,'00000200'X,'00000400'X,'00000800'X,'00001000'X,'00002000'X,'00004000'X,'00008000'X,
	2		'00010000'X,'00020000'X,'00040000'X,'00080000'X,'00100000'X,'00200000'X,'00400000'X,'00800000'X,
	3		'01000000'X,'02000000'X,'04000000'X,'08000000'X,'10000000'X,'20000000'X,'40000000'X,'80000000'X/

	logical*1 initialize	/.TRUE./
	integer*4 detvalid      
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
	
	

	ipuntev=12				! inizio dati
*	ipuntev=1				! inizio dati
	ilast=nwords

*	ipuntev = getevtypelength(ibuf,nwords,evtype,evlength)
*	write(6,*)' Event off,type,length : ',ipuntev,evtype,evlength,nwords
*        write( 6, * )'Processing rec# ', iio.rec, iio.evcount
*	call flush(6)
30	ipuntev = ipuntev + getevtypelength(ibuf(ipuntev),nwords-ipuntev+1,evtype,evlength)
	IF(IPUNTEV.GE.ILAST) goto 9999		! Finito --> prossimo buffer
	if( evlength .le. 4) then
	   nlost = nlost + 1
          ipuntev = ipuntev+2
	   goto 30
	endif
	ievl = evlength/2

	IF( (IPUNTEV+IEVL) .GE. ILAST) goto 9999		! incompletto --> prossimo buffer
	ipuntev_next = ipuntev+ievl+getevtypelength(ibuf(ipuntev+ievl),nwords-ipuntev-ievl+1,evtype_next,evlength_next)
	
	ievl_check = ipuntev_next - ipuntev
*	write(6,*)'  Ev # ',iio.evcount+1
*	call flush(6)
	if(ievl.ne.ievl_check .and. ipuntev_next.lt.ilast ) then
*		write(6,*) 'WARNING : inconsistent event length ',ievl,ievl_check, iio.rec, iio.evcount
*	    if( ievl_check .lt. ievl ) then
		ipuntev = ipuntev_next
		nlost = nlost + 1
		evlength = evlength_next
		if( ipuntev .ge. ilast ) goto 9999
		if( evlength .le. 2) then
	   		nlost = nlost + 1
	   		goto 9999
		endif
		ievl = evlength/2
		if( ipuntev+ievl .gt. ilast ) goto 9999
*	    endif
	endif
	if( evlength_next .eq. 0 )goto 9999


	if(ievl.gt.MAXEVL) goto 9999
*	stop ' Event too long in SORT_PRISMA'
	if (evtype .eq. 0) then
	  ipunt = ipuntev + 2
	  ipuntdet = ipunt
	elseif (evtype .eq. 1) then
	  ipunt = ipuntev + 2
	  ipuntdet = ipunt
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
	elseif (evtype .eq. 2) then
	  ipunt = ipuntev + 3
	  ipuntdet = ipunt
	elseif (evtype .eq. 3) then
	  ipunt = ipuntev + 3
	  ipuntdet = ipunt
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
	else
	  nlost = nlost + 1 
	  goto 30
	endif

	iio.evcount=iio.evcount+1
	ipuntev = ipuntev + ievl
*        write( 6, * )'            ev# ', iio.evcount
*	call flush(6)

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
#if defined( _GW_BIG_ENDIAN )
	  jdat(1)=0
	  jdat(2)=ibuf(ipuntdet)
#else
	  jdat(2)=0
	  jdat(1)=ibuf(ipuntdet)
#endif
	  itipo_old = itipo
*	  if( ldat .eq. 0 ) goto 40
	  ipp=ipuntdet
	  idetn=iand(ldat,'000001FF'X)
	  itipo=iand(ldat,'00003E00'X)/512
	  iform=iand(ldat,'0000C000'X)/16384
	  
*	  if( itipo .eq. 0 )then
*	      write(6,*)' previous type: ',itipo_old
*	      stop
*	      write(6,*) ipuntdet, ipuntev, ibuf(ipuntev-ievl+1),ibuf(ipuntev-ievl+2)
*	      goto 40
*	   endif

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
	      if( itipo .gt. 0  ) then
	         write(6,*) ' Skipping rest of event because of undefined detector type',itipo
	         call flush(6)
	      endif
	      nlost = nlost + 1
	      goto 30
	    elseif( ifragl .le. 0 ) then
	      nlost = nlost +1
	      goto 30
	    endif
	    ipuntdet=ipuntdet+ifragl
	  	      

	  elseif(itipo.eq.1) then		        !!!!!!!!!!!!!!!!! CLUSTER
	    idoff=CSEG*idetn
	    if( NCPAR(IQ) .gt. 0 ) then
	      do jj = 0, NCPAR(IQ)-1
	         ipp = ipp + 1
#if defined( _GW_BIG_ENDIAN )
	         jdat(1)=0
	         jdat(2)=ibuf(ipp)
#else
	         jdat(2)=0
	         jdat(1)=ibuf(ipp)
#endif
	     	 jktmp = jk
		 do ii = 0, CSEG-1
		    if(iand(ipat,pattern(ii)).ne.0) then
		       jktmp = jktmp+1
		       det(jktmp).ival(jj) = ldat
	               if(ldat.gt.0) then
	                 det(jktmp).xval(jj)=ldat+rand_real2()
	               else
	                 det(jktmp).xval(jj)=0.
	               endif
		     endif
		 enddo
	       enddo
	     endif
		       
	    do ii=0,CSEG-1
	      if(iand(ipat,pattern(ii)).ne.0) then
	        jk=jk+1
	        if(NDET(IQ).eq.0) DOFF(IQ)=jk
	        NDET(IQ)=NDET(IQ)+1
	        det(jk).id=idoff+ii
	        do jj=NCPAR(IQ),NDPAR(IQ)-1
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
	            det(jk).xval(jj)=ldat+rand_real2()
	          else
	            det(jk).xval(jj)=0.
	          endif
	        enddo
	      endif
            enddo
	    ipuntdet=ipuntdet+ifragl

	  elseif(itipo.eq.2) then			!!!!!!!!!!!!!!!!! CLOVER
	    idoff=QSEG*idetn
	    if( NCPAR(IQ) .gt. 0 ) then
	      do jj = 0, NCPAR(IQ)-1
	         ipp = ipp + 1
#if defined( _GW_BIG_ENDIAN )
	         jdat(1)=0
	         jdat(2)=ibuf(ipp)
#else
	         jdat(2)=0
	         jdat(1)=ibuf(ipp)
#endif
	     	 jktmp = jk
		 do ii = 0, QSEG-1
		    if(iand(ipat,pattern(ii)).ne.0) then
		       jktmp = jktmp+1
		       det(jktmp).ival(jj) = ldat
	               if(ldat.gt.0) then
	                 det(jktmp).xval(jj)=ldat+rand_real2()
	               else
	                 det(jktmp).xval(jj)=0.
	               endif
		     endif
		 enddo
	       enddo
	     endif
	    do ii=0,QSEG-1
	      if(iand(ipat,pattern(ii)).ne.0) then
	        jk=jk+1
	        if(NDET(IQ).eq.0) DOFF(IQ)=jk
	        NDET(IQ)=NDET(IQ)+1
	        det(jk).id=idoff+ii
	        do jj=NCPAR(IQ),NDPAR(IQ)-1
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
	            det(jk).xval(jj)=ldat+rand_real2()
	          else
	            det(jk).xval(jj)=0.
	          endif
	        enddo
	      endif
            enddo
	    ipuntdet=ipuntdet+ifragl

	  elseif(itipo.eq.3) then        		!!!!!!!!!!!!!!!!! TAPERED
	    idoff=TSEG*idetn
	    if( NCPAR(IQ) .gt. 0 ) then
	      do jj = 0, NCPAR(IQ)-1
	         ipp = ipp + 1
#if defined( _GW_BIG_ENDIAN )
	         jdat(1)=0
	         jdat(2)=ibuf(ipp)
#else
	         jdat(2)=0
	         jdat(1)=ibuf(ipp)
#endif
	     	 jktmp = jk
		 do ii = 0, TSEG-1
		    if(iand(ipat,pattern(ii)).ne.0) then
		       jktmp = jktmp+1
		       det(jktmp).ival(jj) = ldat
	               if(ldat.gt.0) then
	                 det(jktmp).xval(jj)=ldat+rand_real2()
	               else
	                 det(jktmp).xval(jj)=0.
	               endif
		     endif
		 enddo
	       enddo
	     endif
	    do ii=0,TSEG-1
	      if(iand(ipat,pattern(ii)).ne.0) then		!!! caso normale
	        jk=jk+1
	        if(NDET(IQ).eq.0) DOFF(IQ)=jk
	        NDET(IQ)=NDET(IQ)+1
	        det(jk).id=idoff+ii
	        do jj=NCPAR(IQ),NDPAR(IQ)-1
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
	            det(jk).xval(jj)=ldat+rand_real2()
	          else
	            det(jk).xval(jj)=0.
	          endif
	        enddo
	      endif
            enddo
	    ipuntdet=ipuntdet+ifragl

**********************************************
	  elseif(itipo.eq.4) then			!!!!!!!!!!!!!!!!! DANTE
	   if( idetn .lt. NDETS(IQ) )then
	    idoff=DANTESEG*idetn
	    if( NCPAR(IQ) .gt. 0 ) then
	      do jj = 0, NCPAR(IQ)-1
	         ipp = ipp + 1
#if defined( _GW_BIG_ENDIAN )
	         jdat(1)=0
	         jdat(2)=ibuf(ipp)
#else
	         jdat(2)=0
	         jdat(1)=ibuf(ipp)
#endif
	     	 jktmp = jk
		 do ii = 0, DANTESEG-1
		    if(iand(ipat,pattern(ii)).ne.0) then
		       jktmp = jktmp+1
		       if( ldat .ge. PARRES(jj,IQ) ) ldat = 0
		       det(jktmp).ival(jj) = ldat
	               if(ldat.gt.0) then
	                 det(jktmp).xval(jj)=ldat+rand_real2()
	               else
	                 det(jktmp).xval(jj)=0.
	               endif
		     endif
		 enddo
	       enddo
	     endif
	    do ii=0,DANTESEG-1
	      if(iand(ipat,pattern(ii)).ne.0) then
	        jk=jk+1
	        if(NDET(IQ).eq.0) DOFF(IQ)=jk
	        NDET(IQ)=NDET(IQ)+1
	        det(jk).id=idoff+ii
		detvalid = 0
	        do jj=NCPAR(IQ),NDPAR(IQ)-1
	          ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	          jdat(1)=0
	          jdat(2)=ibuf(ipp)
#else
	          jdat(2)=0
	          jdat(1)=ibuf(ipp)
#endif
	          if( ldat .ge. PARRES(jj,IQ) ) ldat = 0
		  detvalid = detvalid + ldat

		  det(jk).ival(jj)=ldat
	          if(ldat.gt.0) then
	            det(jk).xval(jj)=ldat+rand_real2()
	          else
	            det(jk).xval(jj)=0.
	          endif
	        enddo
		if( detvalid .LE. 0 )det(jk).id = -1
	      endif
            enddo
	    endif
	    ipuntdet=ipuntdet+ifragl
***************

**********************************************
	  elseif(itipo.eq.13) then			!!!!!!!!!!!!!!!!! PPAC
	   if( idetn .lt. NDETS(IQ) )then
	    idoff=PPACSEG*idetn
	    if( NCPAR(IQ) .gt. 0 ) then
	      do jj = 0, NCPAR(IQ)-1
	         ipp = ipp + 1
#if defined( _GW_BIG_ENDIAN )
	         jdat(1)=0
	         jdat(2)=ibuf(ipp)
#else
	         jdat(2)=0
	         jdat(1)=ibuf(ipp)
#endif
	     	 jktmp = jk
		 do ii = 0, PPACSEG-1
		    if(iand(ipat,pattern(ii)).ne.0) then
		       jktmp = jktmp+1
		       if( ldat .ge. PARRES(jj,IQ) ) ldat = 0
		       det(jktmp).ival(jj) = ldat
	               if(ldat.gt.0) then
	                 det(jktmp).xval(jj)=ldat+rand_real2()
	               else
	                 det(jktmp).xval(jj)=0.
	               endif
		     endif
		 enddo
	       enddo
	     endif
	    do ii=0,PPACSEG-1
	      if(iand(ipat,pattern(ii)).ne.0) then
	        jk=jk+1
	        if(NDET(IQ).eq.0) DOFF(IQ)=jk
	        NDET(IQ)=NDET(IQ)+1
	        det(jk).id=idoff+ii
		detvalid = 0
	        do jj=NCPAR(IQ),NDPAR(IQ)-1
	          ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	          jdat(1)=0
	          jdat(2)=ibuf(ipp)
#else
	          jdat(2)=0
	          jdat(1)=ibuf(ipp)
#endif
	          if( ldat .ge. PARRES(jj,IQ) ) ldat = 0
		  detvalid = detvalid + ldat

		  det(jk).ival(jj)=ldat
	          if(ldat.gt.0) then
	            det(jk).xval(jj)=ldat+rand_real2()
	          else
	            det(jk).xval(jj)=0.
	          endif
	        enddo
		if( detvalid .LE. 0 )det(jk).id = -1
	      endif
            enddo
	    endif
	    ipuntdet=ipuntdet+ifragl
***************

	  elseif(( itipo.GE.5 ) .and. ( itipo .ne. 13 ) ) then	!!!!!!!!!!!!!!!!!SIMPLE DETECTORS
	    jk=jk+1
	    if(NDET(IQ).eq.0) DOFF(IQ)=jk
	    NDET(IQ)=NDET(IQ)+1
	    det(jk).id=idetn
            detvalid = 0
	    do jj=0,NDPAR(IQ)-1
	      ipp=ipp+1
#if defined( _GW_BIG_ENDIAN )
	      jdat(1)=0
	      jdat(2)=ibuf(ipp)
#else
	      jdat(2)=0
	      jdat(1)=ibuf(ipp)
#endif
             if( ldat .ge. PARRES(jj,IQ) ) ldat = 0
             detvalid = detvalid + ldat
             det(jk).ival(jj)=ldat
	      if(ldat.gt.0) then
	        det(jk).xval(jj)=ldat+rand_real2()
	      else
	        det(jk).xval(jj)=0.
	      endif
	    enddo
           if( detvalid .le. 0 ) det(jk).id = -1
	    ipuntdet = ipp+1


	  else
	    write(6,*) ' WARNING - Undefined detector type',itipo, quale( itipo )
	    write(6,*)'                      previous type',itipo_old, quale( itipo_old )
	    ipuntdet=ipuntdet+ifragl
	 
	    nlost = nlost + 1 
	    goto 30
	  endif

	enddo

40	itot=0
	DO II=1,NTIPI
	  call gs_checkdetector(II)
        enddo

	DO II=1,NTIPI
	  nn=NDET(ii)
	  if(nn.lt.FOLDMIN(ii)) goto 30
	  STAT.FOLD(nn,ii,0)=STAT.FOLD(nn,ii,0)+1
	  if(nn.gt.0) then
	    itot=itot+nn
	    do jp=NDPAR(ii),TDPAR(ii)-1
	    do jd = doff(ii), doff(ii)+nn-1
	      det(jd).ival(jp)=0
	      det(jd).xval(jp)=0.00E0
	    enddo
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




	subroutine SORT_GSR(IBUF,nwords,stat)

!  decodifica l'evento e lo passa alla routine di analisi

#include "gsort.inc"
	record/statdat/stat

	INTEGER*2 IBUF(MAXWORDS)

	INTEGER*2 JDAT(2)
	INTEGER*4 LDAT
	EQUIVALENCE (JDAT,LDAT)

	structure /GSR_DETECTOR/
		integer*2 id
		integer*2 energy
		integer*2 time
		integer*2 dummy
	end structure
	
	structure /GSR_EVENT/
		integer*2  coax_fold
		integer*2  spare(3)
		record /GSR_DETECTOR/coax(0:MAXDET-1)
	end structure
	
	record /GSR_EVENT/yev
	
	MACHINE_POINTERSIZE  bufptr, endpntr
	MACHINE_POINTERSIZE  get_gsrevent
	external get_gsrevent
	integer yev_status
	
	bufptr = %LOC(ibuf(1))
	endpntr = %LOC(ibuf(nwords))
	
	do while ( .TRUE. )
		bufptr = get_gsrevent( %VAL(bufptr), %VAL(endpntr), yev, yev_status )
		if( yev_status .eq. 1 ) then
		    iio.evcount=iio.evcount+1
		    do ii=0,ntipi
		      doff(ii)=0
		      ndet(ii)=0
		    enddo
		    if( EXISTS(0) ) then
		      DOFF(0)=0
	  	      NDET(0)=1
	  	      det(0).id=0
	  	      do ii=0,TDPAR(0)-1
	  	    	det(0).ival(ii)=0
			det(0).xval(ii)=0
	  	      enddo
	              STAT.FOLD(NDET(0),0,0)=STAT.FOLD(NDET(0),0,0)+1
		     endif
		     
		     if( EXISTS(1) ) then
		     DOFF(1) = DOFF(0)+NDET(0)
		     NDET(1) = yev.coax_fold
		     do ii = 0,yev.coax_fold-1
		        ij = ii + DOFF(1)
			det(ij).id   = yev.coax(ii).id
			det(ij).ival(0) = yev.coax(ii).energy
			det(ij).xval(0) = det(ij).ival(0)
			det(ij).xval(0) = det(ij).xval(0)+rand_real2()
			det(ij).ival(1) = yev.coax(ii).time
			det(ij).xval(1) = det(ij).ival(1)
			det(ij).xval(1) = det(ij).xval(1)+rand_real2()
			do jj=2,TDPAR(1)-1
	  	    	   det(ij).ival(jj)=0
			   det(ij).xval(jj)=0
			enddo
	  	      enddo
	              STAT.FOLD(NDET(1),1,0)=STAT.FOLD(NDET(1),1,0)+1
		      endif
		     
		      
		      call EVANA
		      if(break) return
		elseif (yev_status .eq. 0)then
			return
		endif
	enddo
	
	end


	subroutine SORT_GANIL(IBUF,nwords,stat)

!  decodifica l'evento e lo passa alla routine di analisi

#include "gsort.inc"
	record/statdat/stat

	INTEGER*2 IBUF(MAXWORDS)
	INTEGER*2 IDAT
	
	integer ganil_isdatablock
	integer ganil_detfold
	integer ganil_getdetector
	MACHINE_POINTERSIZE ganil_getevent	
	external ganil_isdatablock, ganil_detfold, ganil_getdetector, ganil_getevent
	
	MACHINE_POINTERSIZE  bufptr, endpntr
	integer ganil_ev_status, itot, nn, idx,ii, jj,ij

	
	if( ganil_isdatablock( ibuf ) .ne. 1 ) return
	
	
	bufptr  = %LOC(ibuf(33))
	endpntr = %LOC(ibuf(nwords))
	
	do while ( .TRUE. )
30		bufptr = ganil_getevent( %VAL(bufptr), %VAL(endpntr), ganil_ev_status )
		if( ganil_ev_status .gt. 0 ) then
		    iio.evcount=iio.evcount+1
		    do ii=0,ntipi
		      doff(ii)=0
		      ndet(ii)=0
		    enddo
		    if( EXISTS(0) ) then
		      DOFF(0)=0
	  	      NDET(0)=1
	  	      det(0).id=0
	  	      do ii=0,TDPAR(0)-1
	  	    	det(0).ival(ii)=0
			det(0).xval(ii)=0
	  	      enddo
	              STAT.FOLD(NDET(0),0,0)=STAT.FOLD(NDET(0),0,0)+1
		     endif
		     
		     do ii = 1, ntipi
		     	 DOFF(ii) = DOFF(ii-1)+NDET(ii-1)
		     	 NDET(ii) = ganil_detfold( %VAL(euromap(ii)) )
			 idx = DOFF(ii)
		     	 do ij = 0, NDET(ii)-1
		     	    inew = ganil_getdetector( %VAL(euromap(ii)), det(idx).id, %VAL(NDPAR(ii)) )

			    if( inew .eq. 1 ) then
			       do jj=0, NDPAR(ii)-1
			          det(idx).xval(jj)=det(idx).ival(jj)
			          det(idx).xval(jj)=det(idx).xval(jj)+rand_real2()
			       enddo

			       do jj=NDPAR(ii),TDPAR(ii)-1
	  	    	          det(idx).ival(jj)=0
			          det(idx).xval(jj)=0.00E0
			       enddo
			       idx = idx + inew
			    endif
			    
	  	     	  enddo
			  NDET(ii) = idx - DOFF(ii)
		      enddo
		     
		      itot=0
		      DO II=1,NTIPI
		        call gs_checkdetector(II)
        	      enddo

		      DO II=1,NTIPI
		        nn=NDET(ii)
		        if(nn.lt.FOLDMIN(ii)) goto 30
		        STAT.FOLD(nn,ii,0)=STAT.FOLD(nn,ii,0)+1
		        if(nn.gt.0)itot=itot+nn
		      enddo

		      if(itot.gt.0) then
		        call EVANA
		        if(break) return
		      endif
		      
		elseif (ganil_ev_status .eq. 0)then
			return
		endif
	enddo
		
	end
