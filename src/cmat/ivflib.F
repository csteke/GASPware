C+	IVFLIB
C
C	Libreria di gestione di files indicizzati con records lunghi un
C	numero variabile di blocchi di disco
C
C	ivf_new
C	ivf_open
C	ivf_close
C	ivf_write
C	ivf_read
C	ivf_info
C
C-
D	program ivf_test
D
D	character*60 filename
D	logical*1 old,status
D	integer seed	/123456789/
D	integer dati(128*1000)
DCVMS	integer   handle	/-1/
D	integer*8 handle	/-1/
D	logical*1 ivf_new,ivf_open,ivf_close,ivf_info
D	call inp_ch('Filename',filename)
D
D	nseg=1
D	old=ivf_open(filename,0,handle)
D	if(old) then
D	  if(.not.ivf_info(%val(handle),-1,nseg,nlun)) stop 'IVF_INFO  -1 error'
D	  if(.not.ivf_info(%val(handle),-2,nfirst,nlast)) stop 'IVF_INFO  -2 error'
D	  if(.not.ivf_info(%val(handle),-3,nwaste,nfree)) stop 'IVF_INFO  -3 error'
D	  write(6,*) '# segments is',nseg,'   data starts at',nfirst,'   ends at',nlast
D	  write(6,*) '# unused records is',nwaste,'   in',nfree,'  free regions'
D	  do iseg=0,nseg-1
D	    if(.not.ivf_read(%val(handle),dati,iseg,nget))then
D	      write(6,*) 'Bad status reading',iseg
D	      call exit
D	    endif
D	    if(nget.gt.0) then
D	      write(6,'(6i8)') iseg,dati(1),dati(128*nget-1),nget,dati(2),dati(128*nget)
D	    endif
D	  enddo
D	  call inp_i1('# records da riscrivere',nplus)
D	  if(nplus.gt.0) then
D	    ii=ivf_close(%val(handle))
D	    seed=987654321
D	    ii=ivf_open(filename,1,handle)
D	    if(.not.ivf_info(%val(handle),-1,nseg,nlun)) stop 'IVF_INFO  -1 error'
D	    if(.not.ivf_info(%val(handle),-2,nfirst,nlast)) stop 'IVF_INFO  -2 error'
D	    if(.not.ivf_info(%val(handle),-3,nwaste,nfree)) stop 'IVF_INFO  -3 error'
D	    write(6,*) '# segments is',nseg,'   data starts at',nfirst,'   ends at',nlast
D	    write(6,*) '# unused records is ',nwaste,'   in',nfree,'   regions'
D	    write(6,*)
D	    do iiplus=1,nplus
D	      iseg=ran(seed)*nseg
D	      nblo=ran(seed)*10
D	      do jj=1,128*nblo,2
D	        dati(jj)=iseg
D	        dati(jj+1)=nblo
D	      enddo
D	      if(.not.ivf_write(%val(handle),dati,iseg,nblo)) then
D	        write(6,*) 'Bad status writing',iseg
D	        call exit
D	      endif
D	      if(.not.ivf_info(%val(handle),iseg,n1,n2)) then
D	        write(6,*) 'IVF_INFO',iseg,'  error'
D	        call exit
D	      endif
D	      write(6,'(1h+,5i6)') iiplus,iseg,nblo,n1,n2
D	    enddo
D	  endif
D	  if(.not.ivf_info(%val(handle),-2,nfirst,nlast)) stop 'IVF_INFO  -2 error'
D	  if(.not.ivf_info(%val(handle),-3,nwaste,nfree)) stop 'IVF_INFO  -3 error'
D	  write(6,*) '  data starts at',nfirst,'   ends at',nlast
D	  write(6,*) '# unused records is ',nwaste,'   in',nfree,'   regions'
D	else
D	  write(6,*) 'Error opening  ',filename(1:max(lengthc(filename),1))
D	  call inp_i1('# segments for the new file',nseg)
D	  nseg=min(nseg,1000)
D	  if(.not.ivf_new(filename,nseg,1200)) then
D	     write(6,*) 'Error creating file'
D	     call exit
D	  endif
D	  if(.not.ivf_open(filename,1,handle)) stop 'Error Re-opening file'
D	  write(6,*)
D	  do iiplus=1,10*nseg
D	    iseg=ran(seed)*nseg
D	    nblo=ran(seed)*10
D	    do jj=1,128*nblo,2
D	      dati(jj)=iseg
D	      dati(jj+1)=nblo
D	    enddo
D	    if(.not.ivf_write(%val(handle),dati,iseg,nblo)) then
D	      write(6,*) 'Bad status writing',iseg
D	      call exit
D	    endif
D	    if(.not.ivf_info(%val(handle),iseg,n1,n2)) then
D	      write(6,*) 'IVF_INFO',iseg,'  error'
D	      call exit
D	    endif
D	    write(6,'(1h+,5i6)') iiplus,iseg,nblo,n1,n2
D	  enddo
D	  if(.not.ivf_info(%val(handle),-2,nfirst,nlast)) stop 'IVF_INFO  -2 error'
D	  if(.not.ivf_info(%val(handle),-3,nwaste,nfree)) stop 'IVF_INFO  -3 error'
D	  write(6,*) '  data starts at',nfirst,'   ends at',nlast
D	  write(6,*) '# unused records is ',nwaste,'   in',nfree,'   regions'
D	endif
D	call ivf_close(%val(handle))
D
D	call exit
D
D	end
D
	logical*1 function ivf_new(filename,nseg,deflen)

#include "ivflib.par"

	integer nseg,deflen
	character filename*(*)

	integer ii,nn,ld
	integer header(DRECLONG)
	integer mlun /-1/
	integer ndescr,fdescr,pdata
	
	integer filexist
	external filexist

	integer  lengthc
	external lengthc

D	ii=max(1,lengthc(filename))
D	write(6,*) 'IVFLIB: Creating   '//filename(1:ii)

	if( filexist( filename ) .eq. 0 ) then
	     write(6,*)' IVFLIB ERROR: file '//filename(1:max(1,lengthc(filename)))//' already exists'
	     ivf_new=.false.
	     return
	endif
	
	if(mlun.le.0) call lib$get_lun(mlun)

	nn=MINFLEN
	if(deflen.gt.0 ) nn=deflen

	ld=DRECLONG
CVMS	OPEN(UNIT=MLUN,FILE=filename,STATUS='NEW',ACCESS='DIRECT',ORGANIZATION='SEQUENTIAL',RECL=ld,INITIALSIZE=nn,EXTENDSIZE=nn,err=100)
#if defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER)
#if defined(__IFC) || defined(__INTEL_COMPILER)
	OPEN(UNIT=MLUN,FILE=filename,STATUS='NEW',ACCESS='DIRECT',RECL=DRECLONG, err=100, NOSHARED)
#else
	OPEN(UNIT=MLUN,FILE=filename,STATUS='NEW',ACCESS='DIRECT',RECL=DRECLONG, err=100)
#endif
#else
	OPEN(UNIT=MLUN,FILE=filename,STATUS='NEW',ACCESS='DIRECT',RECL=4*DRECLONG, err=100)
#endif

	do ii=1,DRECLONG
	   header(ii)=0
	enddo

	ndescr=(2*nseg+DRECLONG-1)/DRECLONG 	! # of disk records for 2*nseg LW
	fdescr=2				! inizio sezione descrittori
	pdata=fdescr				! inizio dati

#if defined( _GW_BIG_ENDIAN )
	do ii=1,DRECLONG
	  call swap_bytes(header(ii),4)
	enddo
#endif

	do nn=1,ndescr
	  write(mlun,rec=pdata) (header(ii),ii=1,DRECLONG)	!,err=100
	  pdata=pdata+1
	enddo

	header(128)=IVFVERSION		! Versione (qui' per compatibilita')
	header(  1)=IVFVERSION		! Versione
	header(  2)=nseg		! # di segmenti
	header(  3)=DRECBITS		! # bits per disk record
	header(  5)=1			! descrizione consistente
	header( 11)=ndescr		! # records per i descrittori
	header( 12)=fdescr		! inizio descrittori
	header( 20)=pdata		! primo record per dati
	header( 21)=pdata-1		! ultimo record occupato
	header( 22)=0			! # records sprecati
	header( 23)=0			! # regioni libere
#if defined( _GW_BIG_ENDIAN )
	do ii=1,DRECLONG
	  call swap_bytes(header(ii),4)
	enddo
#endif
	write(mlun,rec=1,err=100) (header(ii),ii=1,DRECLONG)

	close(mlun)

	ivf_new=.true.
	return

100	ivf_new=.false.
	return

	end

	logical*1 function ivf_open(filename,mode,ivfbase)

#include "ivflib.par"

	character filename*(*)
	integer mode

	MACHINE_POINTERSIZE ivfbase
	MACHINE_POINTERSIZE basefree

	integer header(DRECLONG)
	integer mlun,nseg,ndescr,nlong,version,freelen,ivflen
CVMS	integer   basefree
	integer ii,nn
	logical file_opened, file_exists
	
	logical*1 readonly

D	integer  lengthc
D	external lengthc

	logical*1  getmem,ivf__open
	external getmem,ivf__open

	integer  lengthc
	external lengthc

	readonly=mode.ne.1

D	ii=max(1,lengthc(filename))
D	if(readonly) then
D	  write(6,*) 'IVFLIB: Opening  (readonly mode)  '//filename(1:ii)
D	else
D	  write(6,*) 'IVFLIB: Opening  (read/write mode)  '//filename(1:ii)
D	endif

	call lib$get_lun(mlun)

	
	if( lengthc( filename ) .le. 4 ) goto 100
	inquire( file=filename, exist=file_exists )
	if( file_exists ) then
	inquire( file=filename, opened=file_opened )
	if( .not. readonly ) then
		if( file_opened ) then
			write( 6,* )'IVFLIB: file '//filename(1:max(1,lengthc(filename)))//' already opened'
			goto 100
		endif
	endif
	else
		goto 100
	endif

#if defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER)
	if(readonly) then
CVMS	  OPEN(UNIT=MLUN,FILE=filename,ACCESS='DIRECT',ORGANIZATION='sequential',STATUS='OLD',RECL=DRECLONG,err=100,READONLY)
	  OPEN(UNIT=MLUN,FILE=filename,STATUS='OLD',ACCESS='DIRECT',RECL=DRECLONG,err=100,READONLY)
	else
CVMS	  OPEN(UNIT=MLUN,FILE=filename,ACCESS='DIRECT',ORGANIZATION='sequential',STATUS='OLD',RECL=DRECLONG,err=100)
#if defined(__IFC) || defined(__INTEL_COMPILER)
	  OPEN(UNIT=MLUN,FILE=filename,STATUS='OLD',ACCESS='DIRECT',RECL=DRECLONG,err=100, NOSHARED)
#else
	  OPEN(UNIT=MLUN,FILE=filename,STATUS='OLD',ACCESS='DIRECT',RECL=DRECLONG,err=100)
#endif
	endif

#elif defined( __sun_c__ ) || defined( __GFORTRAN__ )
	if(readonly) then
	  OPEN(UNIT=MLUN,FILE=filename,STATUS='OLD',ACCESS='DIRECT',RECL=DRECBYTE,err=100,READONLY)
	else
	  OPEN(UNIT=MLUN,FILE=filename,STATUS='OLD',ACCESS='DIRECT',RECL=DRECBYTE,err=100)
	endif

#elif defined(__PGI)
	  OPEN(UNIT=MLUN,FILE=filename,STATUS='OLD',ACCESS='DIRECT',RECL=DRECBYTE,err=100)
#else
#    error "Unknown FORTRAN compiler"
#endif

	read(mlun,rec=1) (header(ii),ii=1,DRECLONG)	!,err=100

#if defined( _GW_BIG_ENDIAN )
	do ii=1,DRECLONG
	  call swap_bytes(header(ii),4)
	enddo
#endif
	version=header(128)
	if(version.ne.IVFVERSION) then
	  write(6,*)'IVFLIB: The present version of the library is',IVFVERSION
	  write(6,*)'        This file uses version               ',version
	  if(version.gt.IVFVERSION .or. version.lt.2 ) goto 99
	  if(.not.readonly) goto 99
	  write(6,*)'        Will try to manage it'
	  nseg=header(125)
	  nlong=header(3)/32
	  ndescr=(2*nseg+nlong-1)/nlong	! # of disk records for 2*nseg LW
	else
	  if(header(5).ne.1) then
	    write(6,*) 'Description section is inconsistent'
	    goto 99
	  endif
	  nseg=header(2)
	  if(nseg.le.0) then
	    write(6,*) 'Number of segments is',nseg
	    goto 99
	  endif
	  nlong=header(3)/32
	  if(nlong.le.0) then
	    write(6,*) 'Inconsistent number of LongWords per disk-block',nlong
	    goto 99
	  endif
	  nn=(2*nseg+nlong-1)/nlong	! # of disk records for 2*nseg LW
	  ndescr=header(11)		!  stessa cosa registrata
	  if(nn.ne.ndescr) then
	    write(6,*) 'Number of blocks of descriptor section is',ndescr,'    Should be ',nn
	    goto 99
	  endif
	endif

	ivflen= 32 + ndescr*DRECLONG	! there is some spare space
	if(.not.getmem(4*ivflen,ivfbase)) then
	  write(6,*) 'Error allocating memory in IVF_OPEN'
	  goto 99
	endif
	call lvect_erase(%val(ivfbase),ivflen)

	if(readonly) then
	  freelen=0
	  basefree=0
	else
	  freelen=2*(nseg+8)		! there is some spare
	  if(.not.getmem(4*freelen,basefree)) then
	    write(6,*) 'Error allocating memory in IVF_OPEN'
	    goto 99
	  endif
	endif
	call lvect_erase(%val(basefree),freelen)

	if(.not.ivf__open(%val(ivfbase),header,mlun,nseg,readonly,basefree,ivflen,freelen)) goto 99

	ivf_open=.true.
	return

 99	close(unit=mlun)
100	call lib$free_lun(mlun)
	ivf_open=.false.
	return

	end

	logical*1 function ivf__open(ivf,header,mlun,nseg,readonly,basefree,ivflen,freelen)

#include "ivflib.def"
	record/ivfheader/ivf
	integer header(DRECLONG)
	logical*1 readonly
	integer mlun,nseg,ivflen,freelen

	MACHINE_POINTERSIZE basefree

	integer ii,nn

	MACHINE_POINTERSIZE datold

	integer bufold(1)
	pointer (bufpntr,bufold)

	logical*1  ivf__getr,ivf__get_old_descr,getmem,freemem
	external ivf__getr,ivf__get_old_descr,getmem,freemem

D	write(6,'(a,i,a,i)') ' IVF__OPEN: ivf at',%loc(ivf)
D	write(6,'(a,6i)')    ' IVF__OPEN: ivf',mlun,basefree,ivflen,freelen,header(128)

	ivf.lun=mlun
	ivf.version=header(128)
	ivf.readonly=readonly
	ivf.modified=.false.
	ivf.basefree=basefree
	ivf.ivflen=ivflen
	ivf.freelen=freelen
D	write(6,'(a,6i)') ' IVF__OPEN: ivf',ivf.lun,ivf.basefree,ivf.ivflen,ivf.freelen

	if(ivf.version.eq.IVFVERSION) then
	  ivf.nseg  =header(2)
	  do ii=1,ivf.nseg
	    ivf.ind(ii).nrec=0	! #blocchi
	    ivf.ind(ii).frec=0	! primo blocco
	  enddo
	  ivf.consistent=header(5).eq.1
	  ivf.ndescr=header(11)	! # records per i descrittori
	  ivf.fdescr=header(12)	! inizio descrittori
	  ivf.nrec=ivf.ndescr
	  ivf.frec=ivf.fdescr
	  if(.not.ivf__getr(ivf,ivf.ind)) goto 100
#if defined( _GW_BIG_ENDIAN )
	  do ii=1,ivf.nseg
	    call swap_bytes(ivf.ind(ii).nrec,4)	! #blocchi
	    call swap_bytes(ivf.ind(ii).frec,4)	! primo blocco
	  enddo
#endif	  
	  ivf.first =header(20)
	  ivf.last  =header(21)
	  ivf.waste =header(22)
	  ivf.nfree =header(23)
	else
	  ivf.nseg   =header(125)
	  ivf.consistent=.true.		! ???????
	  ivf.ndescr=header(127)	! #records per calibrazioni e proiezioni
	  ivf.fdescr=header(124)	! inizio calibrazioni e proiezioni
	  ivf.first =header(122)
	  ivf.last  =header(121)
	  ivf.waste =header(120)
	  ivf.nfree =0
	  do ii=1,ivf.nseg
	    ivf.ind(ii).nrec=0	! #blocchi
	    ivf.ind(ii).frec=0	! primo blocco
	  enddo
	  nn=((ivf.nseg+DRECLONG-1)/DRECLONG)*DRECLONG	  
	  if(.not.getmem(4*nn,datold)) goto 100
	  call lvect_erase(%val(datold),nn)
	  if(.not.ivf__get_old_descr(ivf,%val(datold),1)) goto 100
	  bufpntr=datold
	  do ii=1,ivf.nseg
	    ivf.ind(ii).nrec= bufold(ii)
	  enddo
	  if(.not.ivf__get_old_descr(ivf,%val(datold),2)) goto 100
	  do ii=1,ivf.nseg
	    ivf.ind(ii).frec= bufold(ii)
	  enddo
*#ifdef sun
*	  do ii=1,ivf.nseg
*	    call swap_bytes(ivf.ind(ii).nrec,4)	! #blocchi
*	    call swap_bytes(ivf.ind(ii).frec,4)	! primo blocco
*	  enddo
*#endif	  
	  if(.not.freemem(4*nn,datold)) continue
	endif

	if(.not.readonly) then
	  call ivf__checkf(ivf,%val(ivf.basefree))
	  if(ivf.waste.ne.header(22) .or. ivf.nfree.ne.header(23)) then
	    write(6,*) 'IVF__CHECKF warning: reconstructed WASTE and NFREE',ivf.waste,ivf.nfree
	    write(6,*) '                     recorded values are          ',header(22),header(23)
	  endif
	endif

	ivf__open=.true.
	return

100	ivf__open=.false.
	return

	end

	logical*1 function ivf__get_old_head(ivf,head)

#include "ivflib.def"
	record/ivfheader/ivf
	integer head(DRECLONG)

	logical*1  ivf__getr
	external ivf__getr

	if(ivf.version.ge.5 ) goto 100

	ivf.nrec=1
	ivf.frec=1
	if(.not.ivf__getr(ivf,head)) goto 100

	ivf__get_old_head=.true.
	return

100	ivf__get_old_head=.false.
	return

	end

	logical*1 function ivf__get_old_rec(ivf,dat,nrec,frec)

#include "ivflib.def"
	record/ivfheader/ivf
	integer dat(1),nrec,frec

	logical*1  ivf__getr
	external ivf__getr

	if(ivf.version.ge.5 ) goto 100

	ivf.nrec=nrec
	ivf.frec=frec
	if(.not.ivf__getr(ivf,dat)) goto 100

	ivf__get_old_rec=.true.
	return

100	ivf__get_old_rec=.false.
	return

	end

	logical*1 function ivf__get_old_descr(ivf,drecs,mode)

#include "ivflib.def"
	record/ivfheader/ivf
	integer drecs(1)
	integer mode

	integer header(DRECLONG)
	integer ndescr,lDRECLONG,nnn,nn1,mcmode,ii

	logical*1  ivf__get_old_head,ivf__get_old_rec
	external ivf__get_old_head,ivf__get_old_rec

	if(mode.lt.1 .or. mode.gt.4) goto 100

	if(.not.ivf__get_old_head(ivf,header)) goto 100
#if defined( _GW_BIG_ENDIAN )
	do ii=1,DRECLONG
	  call swap_bytes(header(ii),4)
	enddo
#endif
	if(ivf.version.eq.4) then
	  ndescr=header(126)
	  lDRECLONG=header(3)/32
	  nnn=(ivf.nseg + lDRECLONG-1)/lDRECLONG
	  nn1=header(123)
	  if(ndescr.ne.3*nnn) goto 100
	  if(mode.eq.1) then		!! produce ivf.ind().nrec
	    if(.not.ivf__get_old_rec(ivf,drecs,nnn,nn1)) goto 100
	    mcmode=header(119)+1
	    do ii=1,ivf.nseg
#if defined( _GW_BIG_ENDIAN )
	      call swap_bytes(drecs(ii),4)
#endif
	      drecs(ii)=drecs(ii)/mcmode
	    enddo
	  elseif(mode.eq.2) then	!! produce ivf.ind().frec
	    if(.not.ivf__get_old_rec(ivf,drecs,nnn,nn1+nnn))then
	     goto 100
	    else
#if defined( _GW_BIG_ENDIAN )
	    do ii=1,ivf.nseg
	      call swap_bytes(drecs(ii),4)
	   enddo
#endif
	     continue
	    endif
	  elseif(mode.eq.3) then	!! produce cmode()
	    if(.not.ivf__get_old_rec(ivf,drecs,nnn,nn1)) goto 100
	    mcmode=header(119)+1
	    do ii=1,ivf.nseg
#if defined( _GW_BIG_ENDIAN )
	      call swap_bytes(drecs(ii),4)
#endif
	      drecs(ii)=mod(drecs(ii),mcmode)
	    enddo
	  elseif(mode.eq.4) then	!! produce offset()
	    if(.not.ivf__get_old_rec(ivf,drecs,nnn,nn1+2*nnn)) then
	     goto 100
	    else
#if defined( _GW_BIG_ENDIAN )
	     do ii=1,ivf.nseg
	      call swap_bytes(drecs(ii),4)
	     enddo
#endif
	     continue
	    endif
	  endif
c	elseif(ivf.version.eq.3) then
c	  if(ndescr.ne.2*nn) goto 99
c	  call cmt4__getr(cmt4_DREC1(1),nn1,nn,nread)
c	  if(nread.ne.nn) goto 99
c	  mcmode=cmt4_HEAD(119)+1
c	  do ii=1,nseg			! unpack cmt4_DREC1 and cmt4_CMODE
c	   lw=cmt4_DREC1(ii)
c	   cmt4_CMODE(ii)=mod(lw,mcmode)
c	   cmt4_DREC1(ii)=lw/mcmode
c	   lw=cmt4_cmode(ii)
c	   if(lw.le.LISTOFFS_V3) then
c	      cmt4_DRECS(ii)=(lw*segsize+nbitblo-1)/nbitblo
c	   else
c	      cmt4_DRECS(ii)=lw-LISTOFFS_V3
c	   endif
c	  enddo
c	elseif(ivf.version.eq.2) then
c	  if(ndescr.ne.2*nn) goto 99
c	  call cmt4__getr(cmt4_DREC1(1),nn1,nn,nread)
c	  if(nread.ne.nn) goto 99
c	  mcmode=cmt4_HEAD(119)+1
c	  do ii=1,nseg			! unpack cmt4_DREC1 and cmt4_CMODE
c	   lw=cmt4_DREC1(ii)
c	   cmt4_CMODE(ii)=mod(lw,mcmode)
c	   cmt4_DREC1(ii)=lw/mcmode
c	   lw=cmt4_cmode(ii)
c	   cmt4_DRECS(ii)=(lw*segsize+nbitblo-1)/nbitblo
c	  enddo
	else
	  goto 100
	endif

	ivf__get_old_descr=.true.
	return

100	ivf__get_old_descr=.false.
	return

	end

	logical*1 function ivf_close(ivf)

#include "ivflib.def"
	record/ivfheader/ivf

	integer header(DRECLONG)
	integer ii

	logical*1  freemem,ivf__putr
	external freemem,ivf__putr

	if(.not.ivf.readonly .and. (ivf.modified .or. .not.ivf.consistent)) then
	  read(ivf.lun,rec=1,err=100) (header(ii),ii=1,DRECLONG)
#if defined( _GW_BIG_ENDIAN )
	  do ii=1,DRECLONG
	     call swap_bytes(header(ii),4)
	  enddo
#endif
	  ivf.nrec=header(11)	! # records per i descrittori
	  ivf.frec=header(12)	! inizio descrittori
#if defined( _GW_BIG_ENDIAN )
	  do ii=1,ivf.nseg
	    call swap_bytes(ivf.ind(ii).nrec,4)	! #blocchi
	    call swap_bytes(ivf.ind(ii).frec,4)	! primo blocco
	  enddo
	  if(.not.ivf__putr(ivf,ivf.ind)) then
	  do ii=1,ivf.nseg
	    call swap_bytes(ivf.ind(ii).nrec,4)	! #blocchi
	    call swap_bytes(ivf.ind(ii).frec,4)	! primo blocco
	  enddo
	  goto 100
	  else
	  do ii=1,ivf.nseg
	    call swap_bytes(ivf.ind(ii).nrec,4)	! #blocchi
	    call swap_bytes(ivf.ind(ii).frec,4)	! primo blocco
	  enddo
	  endif	  
#else	  
	  if(.not.ivf__putr(ivf,ivf.ind)) goto 100
#endif
	  header(5)=1
	  header(21)=ivf.last
	  header(22)=ivf.waste
	  HEADER(23)=ivf.nfree
#if defined( _GW_BIG_ENDIAN )
	  do ii=1,DRECLONG
	     call swap_bytes(header(ii),4)
	  enddo
#endif
	  write(ivf.lun,rec=1,err=100) (header(ii),ii=1,DRECLONG)
	endif
	CLOSE(UNIT=ivf.lun)

	call lib$free_lun(ivf.lun)

	if(ivf.freelen.gt.0) then
	  if(.not.freemem(4*ivf.freelen,ivf.basefree)) then
	    write(6,*) 'IVF_CLOSE: Error releasing  memory 1'
	  endif
	endif
	if(.not.freemem(4*ivf.ivflen,%loc(ivf))) then
	  write(6,*) 'IVF_CLOSE: Error releasing  memory 2'
	endif

	ivf_close=.true.
	return

100	ivf_close=.false.
	return

	end

	logical*1 function ivf_write(ivf,buffer,iseg,nrec)

#include "ivflib.def"
	record/ivfheader/ivf
*	integer dati(*)
	integer iseg,nrec

	integer jseg,nrecold

	INTEGER buffer(DRECLONG,1)

	integer header(DRECLONG)
	integer ii,jj,mlun,pwritten

ccc	logical*1 ivf__putr
ccc	external ivf__putr

	if(ivf.readonly .or. iseg.lt.0 .or. iseg.ge.ivf.nseg) goto 100

	jseg=iseg+1
	nrecold=ivf.ind(jseg).nrec
	if(nrec.gt.nrecold) then
	  ivf.nrec=nrecold
	  ivf.frec=ivf.ind(jseg).frec
	  call ivf__oldr(ivf,%val(ivf.basefree))
	  ivf.nrec=nrec
	  call ivf__newr(ivf,%val(ivf.basefree))
	  ivf.ind(jseg).frec=ivf.frec
	elseif(nrec.lt.nrecold) then
	  ivf.nrec=nrecold-nrec
	  ivf.frec=ivf.ind(jseg).frec+nrec
	  call ivf__oldr(ivf,%val(ivf.basefree))
	endif
	ivf.ind(jseg).nrec=nrec

	if(nrec.gt.0) then
	  ivf.nrec=nrec
	  ivf.frec=ivf.ind(jseg).frec
ccc	  if(.not.ivf__putr(ivf,dati)) goto 100  !inlined
*************************************************!inlined ivf__putr
	  mlun=ivf.lun
	  pwritten=ivf.frec
	  do jj=1,ivf.nrec
	    write(mlun,rec=pwritten,err=100) (buffer(ii,jj),ii=1,DRECLONG)
	    pwritten=pwritten+1
	  enddo
	  if(ivf.consistent) then
	    read(mlun,rec=1,err=100) (header(ii),ii=1,DRECLONG)
	    header(5)=-1
#if defined( _GW_BIG_ENDIAN )
	    call swap_bytes(header(5),4)
#endif
	    write(mlun,rec=1,err=100) (header(ii),ii=1,DRECLONG)
	    ivf.consistent=.false.
	  endif
************************************************
	endif
	ivf.modified=.true.

	ivf_write=.true.
	return

100	ivf_write=.false.
	return

	end

	logical*1 function ivf_read(ivf,buffer,iseg,nrec)

#include "ivflib.def"
	record/ivfheader/ivf
c	integer dati(*)
	integer iseg,nrec

	integer jseg

	INTEGER buffer(DRECLONG,1)

	integer ii,jj,mlun,pread

c	logical*1 ivf__getr
c	external ivf__getr

	if(iseg.lt.0 .or. iseg.ge.ivf.NSEG) goto 100

	jseg=iseg+1
	if(ivf.ind(jseg).nrec .gt. 0) then
	   ivf.nrec=ivf.ind(jseg).nrec
	   ivf.frec=ivf.ind(jseg).frec
c	   if(.not.ivf__getr(ivf,dati)) goto 100
******************************************************! inlined ivf__getr
	   mlun=ivf.lun
	   pread=ivf.frec
	   do jj=1,ivf.nrec
	     read(mlun,rec=pread,err=100) (buffer(ii,jj),ii=1,DRECLONG)
	     pread=pread+1
	   enddo
******************************************************
	   nrec=ivf.nrec
	else
	   nrec=0
	endif

	ivf_read=.true.
	return

100	ivf_read=.false.
	return

	end

	logical*1 function ivf_info(ivf,iseg,nn1,nn2)

#include "ivflib.def"
	record/ivfheader/ivf

	integer iseg,nn1,nn2

	integer jseg

	ivf_info=.true.
	if(iseg.ge.0 .and. iseg.lt.ivf.nseg) then
	  jseg=iseg+1
	  nn1=ivf.ind(jseg).nrec
	  nn2=ivf.ind(jseg).frec
	elseif(iseg.eq.-1) then
	  nn1=ivf.nseg
	  nn2=ivf.version
	elseif(iseg.eq.-2) then
	  nn1=ivf.first
	  nn2=ivf.last
	elseif(iseg.eq.-3) then
	  nn1=ivf.waste
	  nn2=ivf.nfree
	elseif(iseg.eq.-4) then
	  nn1=ivf.ivflen
	  nn2=ivf.freelen
	elseif(iseg.eq.-5) then
	  nn1=ivf.ndescr
	  nn2=ivf.fdescr
	else
	  ivf_info=.false.
	endif

	return

	end

	logical*1 function ivf__putr(ivf,buffer)

#include "ivflib.def"
	record/ivfheader/ivf

	INTEGER buffer(DRECLONG,1)

	integer header(DRECLONG)
	integer ii,jj,mlun,pwritten

	if(ivf.nrec.eq.0) then
	  ivf__putr=.true.
	  return
	endif
	if(ivf.nrec.lt.0) then
	  write(6,*) 'IVF__PUTR: Request to write a negative number of records',ivf.nrec
	  goto 100
	endif

	mlun=ivf.lun
	pwritten=ivf.frec
	do jj=1,ivf.nrec
	  write(mlun,rec=pwritten,err=100) (buffer(ii,jj),ii=1,DRECLONG)
	  pwritten=pwritten+1
	enddo
	if(ivf.consistent) then
	  read(mlun,rec=1,err=100) (header(ii),ii=1,DRECLONG)
	  header(5)=-1
#if defined( _GW_BIG_ENDIAN )
	  call swap_bytes(header(5),4)
#endif
	  write(mlun,rec=1,err=100) (header(ii),ii=1,DRECLONG)
	  ivf.consistent=.false.
	endif
	ivf__putr=.true.
	return

100	ivf__putr=.false.
	return

	end

	logical*1 function ivf__getr(ivf,buffer)

#include "ivflib.def"
	record/ivfheader/ivf

	INTEGER buffer(DRECLONG,1)

	integer ii,jj,mlun,pread

	if(ivf.nrec.lt.0) then
	  write(6,*) 'IVF_GETR: Request to read a negative number of records',ivf.nrec
	  goto 100
	endif

	if(ivf.nrec.gt.0) then
	  mlun=ivf.lun
	  pread=ivf.frec
	  do jj=1,ivf.nrec
	    read(mlun,rec=pread,err=100) (buffer(ii,jj),ii=1,DRECLONG)
	    pread=pread+1
	  enddo
	endif

	ivf__getr=.true.
	return

100	ivf__getr=.false.
	return

	end


	subroutine ivf__checkf(ivf,dat)

#include "ivflib.def"
	record/ivfheader/ivf
	record/ivffree/dat

	integer waste,iseg,nrec,first,last,ifree,ifree1,ifree2,jfree

	waste=ivf.last-ivf.first+1
	if(waste.eq.0) then
	  ivf.nfree=0
	  ivf.waste=0
	  return
	endif
	ivf.nfree=1
	dat.free(1,1)=ivf.first
	dat.free(2,1)=ivf.last

	do iseg=1,ivf.nseg
	  nrec=ivf.ind(iseg).nrec
	  if(nrec.gt.0) then
	    first=ivf.ind(iseg).frec
	    last =first+nrec-1
	    do ifree=1,ivf.nfree
	      ifree1=dat.free(1,ifree)
	      ifree2=dat.free(2,ifree)
	      if(first.ge.ifree1 .and. first.le.ifree2) then
	        if(first.eq.ifree1) then
	          if(last.lt.ifree2) then
	            dat.free(1,ifree)=last+1
	          else
	            do jfree=ifree,ivf.nfree-1
	              dat.free(1,jfree)=dat.free(1,jfree+1)
	              dat.free(2,jfree)=dat.free(2,jfree+1)
	            enddo
	            ivf.nfree=ivf.nfree-1
	          endif
	        else
	          if(last.lt.ifree2) then
		    do jfree=ivf.nfree,ifree+1,-1
	              dat.free(1,jfree+1)=dat.free(1,jfree)
	              dat.free(2,jfree+1)=dat.free(2,jfree)
	            enddo
	            dat.free(1,ifree+1)=last+1
	            dat.free(2,ifree+1)=dat.free(2,ifree)
	            dat.free(2,ifree  )=first-1
	            ivf.nfree=ivf.nfree+1
	          else
                    dat.free(2,ifree)=first-1
	          endif
	        endif
	        waste=waste-nrec
	        goto 100
	      endif
	    enddo
D	    stop 'IVF__CHECKF: Dove lo piazzo questo spazio vuoto ?'
	  endif
100	enddo

	ivf.waste=waste
	return

	end

	subroutine ivf__oldr(ivf,dat)

#include "ivflib.def"
	record/ivfheader/ivf
	record/ivffree/dat

	integer jj,ii,first,last

	if(ivf.nrec.eq.0) return

	first=ivf.frec
	last =first+ivf.nrec-1

	if(ivf.nfree.eq.0) then
	  ivf.nfree=1
	  dat.free(1,1)=first
	  dat.free(2,1)=last
	  goto 100
	endif

	do ii=1,ivf.nfree
	  if(first.lt.dat.free(1,ii)) then
	    if(ii.gt.1) then
	      if(first.eq.dat.free(2,ii-1)+1) then
	        if(last.eq.dat.free(1,ii)-1) then
	          dat.free(2,ii-1)=dat.free(2,ii)
	          do jj=ii,ivf.nfree-1
	            dat.free(1,jj)=dat.free(1,jj+1)
	            dat.free(2,jj)=dat.free(2,jj+1)
	          enddo
	          ivf.nfree=ivf.nfree-1
	        else
	          dat.free(2,ii-1)=last
	        endif
	      else
	        if(last.eq.dat.free(1,ii)-1) then
	          dat.free(1,ii)=first
	        else
	          do jj=ivf.nfree,ii,-1
	            dat.free(1,jj+1)=dat.free(1,jj)
	            dat.free(2,jj+1)=dat.free(2,jj)
	          enddo
	          dat.free(1,ii)=first
	          dat.free(2,ii)=last
	          ivf.nfree=ivf.nfree+1
	        endif
	      endif
	    else
	      if(last.eq. dat.free(1,1)-1) then
	        dat.free(1,1)=first
	      else
	        do jj=ivf.nfree,1,-1
	          dat.free(1,jj+1)=dat.free(1,jj)
	          dat.free(2,jj+1)=dat.free(2,jj)
	        enddo
	        dat.free(1,1)=first
	        dat.free(2,1)=last
	        ivf.nfree=ivf.nfree+1
	      endif
	    endif
	    goto 100
	  endif
	enddo
	if(first.eq.dat.free(2,ivf.nfree)+1) then
	  dat.free(2,ivf.nfree)=last
	else
	  ivf.nfree=ivf.nfree+1
	  dat.free(1,ivf.nfree)=first
	  dat.free(2,ivf.nfree)=last
	endif

100	ivf.waste=ivf.waste+ivf.nrec
	if(dat.free(2,ivf.nfree).eq.ivf.last) then
	  ivf.waste=ivf.waste-(dat.free(2,ivf.nfree)-dat.free(1,ivf.nfree)+1)
	  ivf.last=dat.free(1,ivf.nfree)-1
	  ivf.nfree=ivf.nfree-1
	endif

	return

	end

	subroutine ivf__newr(ivf,dat)

#include "ivflib.def"
	record/ivfheader/ivf
	record/ivffree/dat

	integer jj,ii,nn

	if(ivf.nfree.eq.0) goto 100

	do ii=1,ivf.nfree
	  nn=dat.free(2,ii)-dat.free(1,ii)+1
	  if(nn.ge.ivf.nrec) then
	    ivf.frec=dat.free(1,ii)
	    if(nn.eq.ivf.nrec) then
	      do jj=ii,ivf.nfree-1
	        dat.free(1,jj)=dat.free(1,jj+1)
	        dat.free(2,jj)=dat.free(2,jj+1)
	      enddo
	      ivf.nfree=ivf.nfree-1
	    else
	      dat.free(1,ii)=dat.free(1,ii)+ivf.nrec
	    endif
	    ivf.waste=ivf.waste-ivf.nrec
	    return
	  endif
	enddo

100	ivf.frec=ivf.last+1			! piazzalo alla fine
	ivf.last=ivf.last+ivf.nrec

	return

	end
