#define GO_UP_ONE_LINE char(27)//char(91)//char(65)
D	program mlmlib_test
D
D	logical*1 mlm_init,mlm_insert,mlm_flush,mlm_info
CVMSD	integer   mlm,iaddr
D	integer*8 mlm,iaddr
D	logical*1 status
D	integer ibuf(1000000)
D
D1	call inp_i3('nseg,nbytes,megabytes per mlm_init',nseg,nbytes,megabytes)
D	if(.not.mlm_init(mlm,nseg,nbytes,megabytes,nused)) then
D	  write(6,*) nseg,nn
D	  goto 1
D	endif
D	write(6,*) 'Number of bytes used',nused,megabytes*1024*1024
D	status=mlm_info(%val(mlm),-1,n1,n2)
D	write(6,*) 'List length is',n1,n2
D
D10	call inp_i2('iseg,val per mlm_insert',iseg,ival)
D	if(iseg.ge.0) then
D	  status=mlm_insert(%val(mlm),iseg,ival)
D	  status=mlm_info(%val(mlm),iseg,n1,n2)
D	  write(6,*) iseg,ival,n1,n2
D	else
D	  call inp_i3('nval,maxval,icontr',nval,maxval,icontr)
D	  nn=0
D	  
D	  do ii=1,nval
D22	    iseg=ran(iran)*nseg
CD	    if(ran(iran).gt.(float(iseg)/float(nseg))**12)goto 22
D	    ival=ran(iran)*maxval
D	    if(iseg.eq.icontr) then
D	      nn=nn+1
D	      ibuf(nn)=ival
D	    endif
D	    if(.not.mlm_insert(%val(mlm),iseg,ival)) then
D	      status=mlm_info(%val(mlm),iseg,n1,n2)
D	      status=mlm_flush(%val(mlm),iseg,iaddr,nvals)
D	      status=mlm_info(%val(mlm),iseg,m1,m2)
D	      status=mlm_info(%val(mlm),-2,l1,l2)
C	      write(6,'(8i10)') ii,iseg,n1,n2,nvals,m1,l2
D	      if(iseg.eq.icontr) then
D	         write(6,*) 'checking consistency of seg#  ',icontr
D		 call check(%val(iaddr),ibuf,nvals)
D	         nn=0
D	      endif
D	    endif
D	  enddo
D	endif
D
D	call inp_i1('iseg per mlm_info',iseg)
D	status=mlm_info(%val(mlm),iseg,n1,n2)
D	write(6,*) iseg,n1,n2
D
D	call inp_i1('iseg per mlm_flush',iseg)
D	if(mlm_flush(%val(mlm),iseg,iaddr,nvals)) then
D	  write(6,*) iseg,iaddr,nvals
D	  if(nvals.gt.0) call lista(%val(iaddr),nvals)
D	endif
D
D	goto 10
D
D	end
D
D	subroutine lista(buf,nvals)
D
D	integer buf(nvals)
D
D	write(6,'(10i7)') (buf(jj),jj=1,nvals)
D
D	return
D
D	end
D
D	subroutine check(buf1,buf2,nvals)
D
D	integer buf1(nvals),buf2(nvals)
D
D	nd=0
D	do ii=1,nvals
D	  if(buf1(ii).ne.buf2(ii)) then
D	    if(nn.lt.100) write(6,*) ii-1,buf1(ii),buf2(ii)
D	    nn=nn+1
D	  endif
D	enddo
D
D	return
D
D	end



	logical*1 function mlm_init(mlmbase,nseg,nbyt,megabytes,nused)

#include "mlmlib.def"

CVMS	integer   mlmbase
	MACHINE_POINTERSIZE mlmbase
	integer nseg,nbyt,megabytes,nused
	logical*1 getmem,freemem

CVMS	integer   obufbase,recsbase,listbase
	MACHINE_POINTERSIZE obufbase,recsbase,listbase
	integer mlmllen,nrecs,nrmax,listlen
	integer totmem,ii,nn,lsegmin,lsegmed,lsegmax

cp	record/mlmdef/mlm			! portato in subroutine per problema uso pointer
cp	pointer (mlmpnt,mlm)

cp	byte recs(0:1)
cp	pointer (recspnt,recs)
	
	if(nbyt.le.0 .or. nbyt.gt.4) goto 100

	totmem=1024*1024*megabytes

	mlmllen= 22 + 4*nseg		! memoria per i puntatori
	totmem=totmem-mlmllen*4
	if(totmem.le.0) goto 100

	nrecs=nseg*FRACT			! suddivisione fine della memoria
	nrmax=min(nrecs-nseg+1,FRACT*FRACT)	! numero massimo di suddivisioni fini allocabili

	lsegmin=(totmem-nrecs)/(nbyt*nrecs+4*nrmax)	! lunghezza delle nrecs suddivisioni in prima approx.
	lsegmax=nrmax*lsegmin

	totmem=totmem-nrecs-lsegmax*4	! quello che resta tolta la gestione e il buffer di uscita
	if(totmem .LE. 0) goto 100

	lsegmin=totmem/(nbyt*nrecs)	! lungezza delle nrecs suddivisioni in seconda approx.
	lsegmax=nrmax*lsegmin

	listlen=lsegmin*nrecs		! memoria per i buffer di lista

	if(.not.getmem(4*mlmllen,mlmbase)) goto 100
	if(.not.getmem(4*lsegmax,obufbase)) goto 99
	if(.not.getmem(nrecs,recsbase)) goto 98
	if(.not.getmem(listlen*nbyt,listbase)) goto 97

	nused = 4*mlmllen + 4*lsegmax + nrecs + listlen*nbyt

	call mlm__init(%val(mlmbase),%val(recsbase),nrecs,nrmax,nseg,nbyt,mlmllen,lsegmin,lsegmax,
	1	obufbase,listbase,listlen,recsbase)

cp	mlmpnt=mlmbase
cp	mlm.nseg=nseg
cp	mlm.nbyt=nbyt
cp	mlm.mlmllen=mlmllen
cp	mlm.lsegmin=lsegmin
cp	mlm.lsegmax=lsegmax
cp	mlm.obufbase=obufbase
cp	mlm.listbase=listbase
cp	mlm.listlen=listlen
cp	recspnt=recsbase
cp	do ii=0,nseg-1
cp	   recs(ii)=1
cp	   mlm.que(ii).start=ii*lsegmin
cp	   mlm.que(ii).next =mlm.que(ii).start
cp	   mlm.que(ii).end  =mlm.que(ii).start+lsegmin-1
cp	enddo
cp	do ii=nseg,nrecs-1
cp	   recs(ii)=0
cp	enddo
cp
cp	mlm.recsbase=recsbase
cp	mlm.nrecs=nrecs
cp	mlm.nrecsmax=nrmax
cp	mlm.nrecsstep=(nrmax+1)/2
cp	mlm.nrecsfmin=0.05*nrecs
cp	mlm.recsfree=nrecs-nseg

	mlm_init=.true.

	return

97	if(.not.freemem(4*lsegmax,obufbase)) goto 98
98	if(.not.freemem(nrecs,recsbase)) goto 99
99	if(.not.freemem(4*mlmllen,mlmbase)) goto 100
100	mlm_init=.false.
	return

	end

	subroutine mlm__init(mlm,recs,nrecs,nrmax,nseg,nbyt,mlmllen,lsegmin,lsegmax,
	1	obufbase,listbase,listlen,recsbase)

#include "mlmlib.def"

	integer nseg,nbyt,lsegmin,lsegmax,mlmllen,nrecs,nrmax,listlen

CVMS	integer   obufbase,recsbase,listbase
	MACHINE_POINTERSIZE obufbase,recsbase,listbase

	integer ii

	record/mlmdef/mlm
cpp	pointer (mlmpnt,mlm)

	byte recs(0:nrecs-1)
cpp	pointer (recspnt,recs)

	mlm.nseg=nseg
	mlm.nbyt=nbyt
	mlm.mlmllen=mlmllen
	mlm.lsegmin=lsegmin
	mlm.lsegmax=lsegmax
	mlm.obufbase=obufbase
	mlm.listbase=listbase
	mlm.listlen=listlen
	mlm.nflush = 0
cpp	recspnt=recsbase
	do ii=0,nrecs-1
	   recs(ii)=0
	enddo
	do ii=0,nseg-1
	   recs(MLM_GAP*ii)=1
	   mlm.que(ii).start=2*ii*lsegmin
	   mlm.que(ii).next =mlm.que(ii).start
	   mlm.que(ii).end  =mlm.que(ii).start+lsegmin-1
	   mlm.que(ii).nflush = 0
	enddo

	mlm.recsbase=recsbase
	mlm.nrecs=nrecs
	mlm.nrecsmax=nrmax
	mlm.nrecsstep=(nrmax+1)/4  ! old (nrmax+1)/2
	mlm.nrecsfmin=0.05*nrecs
	mlm.recsfree=nrecs-nseg

	return

	end

	logical*1 function mlm_insert(mlm,iseg,lval)

#include "mlmlib.def"
	record/mlmdef/mlm
	integer iseg,lval

	byte      bbuf(0:1)
	pointer   (bpnt,bbuf)

	integer*2 wbuf(0:1)
	pointer   (wpnt,wbuf)

	integer*4 lbuf(0:1)
	pointer   (lpnt,lbuf)

	byte    recs(0:1)
	pointer (recspnt,recs)

*	integer*1 bw(4) /0,0,0,0/
	byte bw(4) /0,0,0,0/
	integer*2 ww(2)	/0,0/
	integer*4 lw	/0/
	equivalence (bw,ww,lw)

	integer iad
	integer nerr /0/

	if(iseg.lt.0 .or. iseg.ge. mlm.nseg) goto 100
	if(mlm.que(iseg).next .gt. mlm.que(iseg).end) goto 100

	if(mlm.nbyt.eq.2) then
	  lw=lval
	  wpnt=mlm.listbase
#if defined( _GW_BIG_ENDIAN )
	  wbuf(mlm.que(iseg).next)=ww(2)
#else
	  wbuf(mlm.que(iseg).next)=ww(1)
#endif

	elseif(mlm.nbyt.eq.3) then
	  lw=lval
	  bpnt=mlm.listbase
	  iad=mlm.que(iseg).next*3
#if defined( _GW_BIG_ENDIAN )
	  bbuf(iad  )=bw(4)
	  bbuf(iad+1)=bw(3)
	  bbuf(iad+2)=bw(2)
#else
	  bbuf(iad  )=bw(1)
	  bbuf(iad+1)=bw(2)
	  bbuf(iad+2)=bw(3)
#endif

	elseif(mlm.nbyt.eq.1) then
	  lw=lval
	  bpnt=mlm.listbase
#if defined( _GW_BIG_ENDIAN )
	  bbuf(mlm.que(iseg).next)=bw(4)
#else
	  bbuf(mlm.que(iseg).next)=bw(1)
#endif

	elseif(mlm.nbyt.eq.4) then
	  lpnt=mlm.listbase
	  lbuf(mlm.que(iseg).next)=lval

	else
	  stop 'MLM_INSERT: should never reach this point'
	endif
	
	mlm.que(iseg).next=mlm.que(iseg).next+1
	if(mlm.que(iseg).next .le. mlm.que(iseg).end) then
	  mlm_insert=.true.
	  return
	elseif(mlm.que(iseg).next .eq. mlm.que(iseg).end+1) then
	  if( mlm.que(iseg).end-mlm.que(iseg).start+1 .lt. mlm.lsegmax) then
	  iad = mlm.que(iseg).end/mlm.lsegmin+1
	  if( iad .ge. mlm.nrecs )goto 100
	  recspnt=mlm.recsbase
	  if( recs(iad) .eq. 0)then
	     recs(iad) = 1
	     mlm.que(iseg).end = mlm.que(iseg).end+mlm.lsegmin
	     mlm.recsfree = mlm.recsfree -1
	     mlm_insert=.true.
	     return
	  endif
	  endif
	endif

100	mlm_insert=.false.
	return

	end

	logical*1 function mlm_flush(mlm,iseg,outaddr,nentries)

#include "mlmlib.def"
	record/mlmdef/mlm
	integer iseg,nentries
CVMS	integer   outaddr
	MACHINE_POINTERSIZE outaddr

	byte      bbuf(0:1)
	pointer   (bpnt,bbuf)

	integer*2 wbuf(0:1)
	pointer   (wpnt,wbuf)

	integer*4 lbuf(0:1)
	pointer   (lpnt,lbuf)

*	integer*1 bw(4) /0,0,0,0/
	byte  bw(4) /0,0,0,0/
	integer*2 ww(2)	/0,0/
	integer*4 lw	/0/
	equivalence (bw,ww,lw)

	integer*4 outbuf(0:1)
	pointer   (outpnt,outbuf)

	byte    recs(0:1)
	pointer (recspnt,recs)

	logical*1  mlm__extend1,mlm__extend2,mlm__extend3,mlm__extend4
	external mlm__extend1,mlm__extend2,mlm__extend3,mlm__extend4

	integer iad,ii,ll,rec1,rec2,reco,recn,nrec1
	
	real AvFlush

	mlm_flush=.false.
	if(iseg.lt.0 .or. iseg.ge.mlm.nseg) return

	nentries=mlm.que(iseg).next-mlm.que(iseg).start
	if(nentries.lt.0) return

	mlm_flush=.true.
	if(nentries.eq.0) return

	outpnt=mlm.obufbase

	if(mlm.nbyt.eq.2) then
	  wpnt=mlm.listbase + 2*mlm.que(iseg).start
	  lw=0
	  do ii=0,nentries-1
#if defined( _GW_BIG_ENDIAN )
	    ww(2)=wbuf(ii)
#else
	    ww(1)=wbuf(ii)
#endif
	    outbuf(ii)=lw
	  enddo

	elseif(mlm.nbyt.eq.3) then
	  bpnt=mlm.listbase + 3*mlm.que(iseg).start
	  iad=0
	  lw=0
	  do ii=0,nentries-1
#if defined( _GW_BIG_ENDIAN )
	    bw(4)=bbuf(iad  )
	    bw(3)=bbuf(iad+1)
	    bw(2)=bbuf(iad+2)
#else
	    bw(1)=bbuf(iad  )
	    bw(2)=bbuf(iad+1)
	    bw(3)=bbuf(iad+2)
#endif
	    outbuf(ii)=lw
	    iad=iad+3
	  enddo

	elseif(mlm.nbyt.eq.1) then
	  bpnt=mlm.listbase + mlm.que(iseg).start
	  lw=0
	  do ii=0,nentries-1
#if defined( _GW_BIG_ENDIAN )
	    bw(4)=bbuf(ii)
#else
	    bw(1)=bbuf(ii)
#endif
	    outbuf(ii)=lw
	  enddo

	elseif(mlm.nbyt.eq.4) then
	  lpnt=mlm.listbase + 4*mlm.que(iseg).start
	  do ii=0,nentries-1
	    outbuf(ii)=lbuf(ii)
	  enddo

	else
	  stop 'MLM_FLUSH: should never reach this point'
	endif

	outaddr=mlm.obufbase
	mlm.que(iseg).next=mlm.que(iseg).start
	mlm.nflush = mlm.nflush + 1
	mlm.que(iseg).nflush=mlm.que(iseg).nflush+1
*	AvFlush = mlm.recsfree
*	AvFlush = AvFlush/mlm.nrecs*100
*	write(6,'('' Swap #'',i8,''  Memory used at '',f5.1)')mlm.nflush,100.0-AvFlush
*	write(6,'(a3,a3)')GO_UP_ONE_LINE,GO_UP_ONE_LINE
*	call flush(6)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      prima di uscire verifica se puo' allungare il buffer          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	if(mlm.recsfree .lt. mlm.nrecsfmin) then	! tutto gia' pieno
D	  write(6,*) 'e ',iseg
	  return
	endif
	ll=mlm.que(iseg).end-mlm.que(iseg).start+1	! lunghezza del segmento
	if(nentries .ne. ll) return			! non era flush da lista piena


	recspnt=mlm.recsbase

	rec1=mlm.que(iseg).start/mlm.lsegmin
	rec2=mlm.que(iseg).end  /mlm.lsegmin
	reco=rec2-rec1+1
       do ii=rec1,rec2  			       ! libera lo spazio vecchio
	 recs(ii)=0
       enddo
       mlm.recsfree=mlm.recsfree+reco
       recn=reco				       ! prova trovare posto piu indietro
						       ! cosi si fa piu spazio continuo in avanti
       AvFlush = mlm.nflush
       AvFlush = AvFlush/mlm.nseg
       if( mlm.que(iseg).nflush .lt. 0.700*AvFlush )recn = max(reco-mlm.nrecsstep,1)	! spazio ristreto
       
       ll=0
       do ii=0,mlm.nrecs-1
	 if(recs(ii).ne.1) then
	   if(ll.eq.0) nrec1=ii
	   ll=ll+1
	   if(ll.eq.recn) then
	     mlm.que(iseg).start=nrec1*mlm.lsegmin
	     mlm.que(iseg).next=mlm.que(iseg).start
	     mlm.que(iseg).end =mlm.que(iseg).start + recn*mlm.lsegmin -1
	     do ll=nrec1,nrec1+recn-1
	       recs(ll)=1
	     enddo
	     mlm.recsfree=mlm.recsfree-recn
D	     write(6,*) 'g ',iseg,recn,nrec1,mlm.recsfree,mlm.nrecsstep
	     if(recn .lt. reco)return
	     rec1=mlm.que(iseg).start/mlm.lsegmin
	     rec2=mlm.que(iseg).end  /mlm.lsegmin
	     goto 10
	   endif
	 else
	   ll=0
	 endif
       enddo

	if( mlm.que(iseg).nflush .lt. 2)return

	ll=mlm.que(iseg).end-mlm.que(iseg).start+1	! lunghezza del segmento
10	if(ll .ge. mlm.lsegmax) then			! buffer gia' lungo il massimo
D	  write(6,*) 'm ',iseg,nentries
*	  mlm.que(iseg).end = mlm.que(iseg).end  - 2*mlm.nrecsstep*mlm.lsegmin
*	  do ii=rec2-2*mlm.nrecsstep+1,rec2
*	    recs(ii)=0
*	  enddo
	  return
	endif

	if(mlm.recsfree .le. reco) then			! tutto gia' pieno
D	  write(6,*) 'f ',iseg,reco
	  return
	endif

	do ii=rec1,rec2					! libera lo spazio vecchio
	  recs(ii)=0
	enddo
	mlm.recsfree=mlm.recsfree+reco

	recn=min(reco+mlm.nrecsstep,mlm.nrecsmax)	! spazio allungato

	ll=0
	do ii=0,mlm.nrecs-1
	  if(recs(ii).ne.1) then
	    if(ll.eq.0) nrec1=ii
	    ll=ll+1
	    if(ll.eq.recn) then
	      mlm.que(iseg).start=nrec1*mlm.lsegmin
	      mlm.que(iseg).next=mlm.que(iseg).start
	      mlm.que(iseg).end =mlm.que(iseg).start + recn*mlm.lsegmin -1
	      do ll=nrec1,nrec1+recn-1
	        recs(ll)=1
	      enddo
	      mlm.recsfree=mlm.recsfree-recn
D	      write(6,*) 'g ',iseg,recn,nrec1,mlm.recsfree,mlm.nrecsstep
	      return
	    endif
	  else
	    ll=0
	  endif
	enddo

	if(mlm.nrecsstep .gt.1)then
	  recn=reco+1					! spazio allungato con 1

	ll=0
	do ii=0,mlm.nrecs-1
	  if(recs(ii).ne.1) then
	    if(ll.eq.0) nrec1=ii
	    ll=ll+1
	    if(ll.eq.recn) then
	      mlm.que(iseg).start=nrec1*mlm.lsegmin
	      mlm.que(iseg).next=mlm.que(iseg).start
	      mlm.que(iseg).end =mlm.que(iseg).start + recn*mlm.lsegmin -1
	      do ll=nrec1,nrec1+recn-1
	        recs(ll)=1
	      enddo
	      mlm.recsfree=mlm.recsfree-recn
D	      write(6,*) 'g ',iseg,recn,nrec1,mlm.recsfree,mlm.nrecsstep
	      return
	    endif
	  else
	    ll=0
	  endif
	enddo
	endif

	mlm.recsfree=mlm.recsfree-reco			!  non avendo trovato spazio rimette tutto come prima
	do ii=rec1,rec2
	  recs(ii)=1
	enddo
	mlm.nrecsstep=max(1,mlm.nrecsstep-1)		! e tenta di fare un passo piu' fine
D	write(6,*) 'b ',iseg,reco,rec1,mlm.recsfree

	return

	end


	logical*1 function mlm_info(mlm,iseg,n1,n2)

#include "mlmlib.def"
	record/mlmdef/mlm
	integer iseg,n1,n2

	if(iseg.GE.0 .AND. iseg.LT.mlm.nseg) then
	   n1=mlm.que(iseg).end  - mlm.que(iseg).start + 1
	   n2=mlm.que(iseg).next - mlm.que(iseg).start
	   mlm_info=.true.
	elseif(iseg.eq.-1) then
	   n1=mlm.lsegmin
	   n2=mlm.lsegmax
	   mlm_info=.false.
	elseif(iseg.eq.-2) then
	   n1=mlm.nrecs
	   n2=mlm.recsfree
	   mlm_info=.false.
	else
	   n1=mlm.nseg
	   n2=mlm.nbyt
	   mlm_info=.false.
	endif

	return

	end
