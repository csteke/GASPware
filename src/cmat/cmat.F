#define GO_UP_ONE_LINE char(27)//char(91)//char(65)
#include "../libr/types.def"

	PROGRAM CMAT

#include "cmat.inc"

	external EXIT_CMAT
	CHARACTER*80 CMD(31)

	CMD( 1)='OPEN             Connect (READONLY)   to a compressed matrix'
	CMD( 2)='RWOPEN           Connect (READ/WRITE)          "'
	CMD( 3)='CLOSE            Close connection to the matrix'
	CMD( 4)='CHANNEL_VALUE    Show value stored in a given channel'   
	CMD( 5)='GET_PROJECTIONS  Extract projections stored in the matrix'
	CMD( 6)='PUT_PROJECTIONS  Store new projections'
	CMD( 7)='DECOMPRESS_2D    Decompress a 2D matrix'
	CMD( 8)='COMPRESS_2D      Compress   a 2D matrix'
	CMD( 9)='ADD              Add 2 matrices with given factors'
	CMD(10)='SHIFT            Reorganize a matrix'
	CMD(11)='TRANSPOSE        Transpose 2 indexes of a matrix'
	CMD(12)='GATE             Get 1D spectrum gating on the other indexes'
	CMD(13)='M2D              Get 2D matrix from a higher dimensional one'
	CMD(14)='DIAGN_2D_SYMM    Get cuts normal   to main diagonal (spectrum of differences)'
	CMD(15)='DIAGP_2D_SYMM    Get cuts parallel to main diagonal (spectrum of sums)'
	CMD(16)='GRID             Search of SD bands using grids of gates'
	CMD(17)='SCATTERPLOT_2D   Scatter plot display of a 2D matrix'
	CMD(18)='MOMENTS          First moments of a 2D matrix along one axis'
	CMD(19)='MDIAGN_2D_SYMM   Get cuts normal to main diagonal (spectrum of differences)'
	CMD(20)='AUTO_BANANA      Find lower and upper banana limits automatically (energy-time)'
	CMD(21)='BANANA           Get 1D spectrum puting a banana the other indexes(3d only)'
	CMD(22)='SET_BACKGROUND   Set the background subtraction parameters (2d symm only)'
	CMD(23)='GET_BACKGROUND   Get the background spectrum (2d symm only)'
	CMD(24)=' '
	CMD(25)=' '
	CMD(26)=' '
	CMD(27)=' '
	CMD(28)='TEST             Reserved for tests'
	CMD(29)='STATISTICS       Information on compression mode ... '
	CMD(30)='EXIT             Quit program'
	CMD(31)='QUIT             Quit program'
	N_COMMANDS=31

CVMS
*	open(6,carriagecontrol='FORTRAN',status='OLD')
	call xinpmode(1)
#if defined(__IFC) || defined(__INTEL_COMPILER)
	call signal(2,EXIT_CMAT,-1)
	call signal(15,EXIT_CMAT,-1)
#endif

10	IF(error) THEN
	  CALL INP_MODE(0)
	  i1dmmy = INP_MSG(' Operation aborted\n')
	  ICOUNT=0			! ON ERROR STOP SERVICE
	ENDIF

	CALL INP_MODE(1)
	if( iargc() .eq. 0) then
		IIC=INP_CMDH('CMAT',CMD,N_COMMANDS,NC)
	else
		IIC=INP_CMD('CMAT',CMD,N_COMMANDS,NC)
	endif
	
	IF(IIC.EQ.-1) NC=30

	error=.true.
	CALL INP_MODE(2)

	IF(NC.EQ.0) THEN
	  ERROR=.FALSE.

	ELSEIF(NC.EQ.1) THEN
	  call MADD_open(1,0,'Compressed matrix filename')

	ELSEIF(NC.EQ.2) THEN
	  call MADD_open(1,1,'Compressed matrix filename')

	ELSEIF(NC.EQ.3) THEN
	  call MADD_close(1)

	ELSEIF(NC.EQ.4) THEN
	  call MADD_channel_value(1)

	ELSEIF(NC.EQ.5) THEN
	  call MADD_getproje(1)

	ELSEIF(NC.EQ.6) THEN
	  call MADD_putproje(1)

	ELSEIF(NC.EQ.7) THEN
	  call MADD_decompress2d(1)

	ELSEIF(NC.EQ.8) THEN
	  call MADD_compress2d(1)

	ELSEIF(NC.EQ.9) THEN
	  call MADD_ADD

	ELSEIF(NC.EQ.10) THEN
	  call MADD_SHIFT(1,2)

	ELSEIF(NC.EQ.11) THEN
	  call MADD_TRANSPOSE(1,2)

	ELSEIF(NC.EQ.12) THEN
	  call MADD_GATE(1)

	ELSEIF(NC.EQ.13) THEN
	  call MADD_M2D(1,2)

	ELSEIF(NC.EQ.14) THEN
	  call MADD_DIAGN(1)

	ELSEIF(NC.EQ.15) THEN
	  call MADD_DIAGP(1)

	ELSEIF(NC.EQ.16) THEN
	  call MADD_GRID(1)

	ELSEIF(NC.EQ.17) THEN
	  call MADD_SCATTERPLOT2D(1)

	ELSEIF(NC.EQ.18) THEN
	  call MADD_MOMENTS(1)

	ELSEIF(NC.EQ.19) THEN
	  call MADD_MDIAGN(1)

	ELSEIF(NC.EQ.20) THEN
	  call MADD_AUTO_BANANA(1)

	ELSEIF(NC.EQ.21) THEN
	  call MADD_BANANA(1)

	ELSEIF(NC.EQ.22) THEN
	  call MADD_SET_AUTOBG

	ELSEIF(NC.EQ.23) THEN
	  call MADD_GET_AUTOBG

	ELSEIF(NC.EQ.28) THEN
	  call MADD_TEST(1)

	ELSEIF(NC.EQ.29) THEN
	  call MADD_STATISTICS(1)

	ELSEIF(NC.EQ.30 .OR. NC.EQ.31) THEN
	  do nmat=1,MAXNMAT
	    if(opened(nmat)) call MADD_close(nmat)
	  end do
	  CALL EXIT(0)

	ELSE
	  error=.true.
	ENDIF

	GOTO 10

	END
	
	
	
	SUBROUTINE EXIT_CMAT

#include "cmat.inc"
	
	  do nmat=1,MAXNMAT
	    if(opened(nmat)) call MADD_close(nmat)
	  end do
	  CALL EXIT(0)
	  
	  RETURN
	  END





	SUBROUTINE MADD_OPEN(nmat,mode,prompt)

#include "cmat.inc"

	integer mode		! 0=readonly 1=r/w
	character*(*) prompt

	logical*1 cmt_open,cmt_close,cmt_new,cmt_info

	if(opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is already opened')
	  return
	endif

	if(prompt.ne.' ') i1dmmy = INP_word(prompt,CMATFILE(nmat))

	if(mode.eq.1) then
	  iomode=1
	else
	 iomode=0
	endif

	if(.not.cmt_open(CMATFILE(nmat),iomode,cmt(nmat))) then
	   i1dmmy = inp_msg('Error opening matrix')
	   return
	endif
	if(.not.cmt_info(%val(cmt(nmat)),-1,ndim(nmat),matmode(nmat))) return
	if(.not.cmt_info(%val(cmt(nmat)),-2,res(1,nmat),step(1,nmat))) return
	if(.not.cmt_info(%val(cmt(nmat)),-3,nseg(nmat),segsize(nmat))) return
	do nn=1,ndim(nmat)
	  mblr(nn,nmat)=(res(nn,nmat)+step(nn,nmat)-1)/step(nn,nmat)
	end do

	call cmt_show(%val(cmt(nmat)),6)

	opened(nmat)=.true.
	readonly(nmat)=iomode.eq.0
	error=.false.

	return

	end

	SUBROUTINE MADD_NEW(nmat,prompt)

#include "cmat.inc"

	character*(*) prompt

	logical*1 cmt_open,cmt_close,cmt_new,cmt_info

	if(ndim(nmat).lt.2 .or. ndim(nmat).gt.MAXMATDIM) then
	   write(6,*)'Wrong matrix dimension'
	   return
	endif

	if(opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is already opened')
	  return
	endif

	if(prompt.ne.' ') i1dmmy = inp_ch(prompt,cmatfile(nmat))

	if(.not.cmt_new(cmatfile(nmat),ndim(nmat),res(1,nmat),step(1,nmat),matmode(nmat))) then
	   i1dmmy = inp_msg('Error creating compressed matrix')
	   return
	endif
	if(.not.CMT_open(CMATFILE(nmat),1,cmt(nmat))) then
	   i1dmmy = inp_msg('Error opening matrix')
	   return
	endif
	if(.not.cmt_info(%val(cmt(nmat)),-1,ndim(nmat),matmode(nmat))) return
	if(.not.cmt_info(%val(cmt(nmat)),-2,res(1,nmat),step(1,nmat))) return
	if(.not.cmt_info(%val(cmt(nmat)),-3,nseg(nmat),segsize(nmat))) return
	do nn=1,ndim(nmat)
	  mblr(nn,nmat)=(res(nn,nmat)+step(nn,nmat)-1)/step(nn,nmat)
	end do

	call cmt_show(%val(cmt(nmat)),6)

	opened(nmat)=.true.
	readonly(nmat)=.false.
	error=.false.

	return

	end

	SUBROUTINE MADD_CLOSE(nmat)

#include "cmat.inc"

	logical*1 cmt_close

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

	call cmt_readspec_reset		 ! reset internal buffers 

	if(.not.cmt_close(%val(cmt(nmat)),cmatfile(nmat))) continue

	opened(nmat)=.false.
	error=.false.

	return

	end

	subroutine MADD_channel_value(nmat)

#include "cmat.inc"

	integer ind0(MAXMATDIM),jnd0(MAXMATDIM)
	integer seg0(MAXMATDIM),iad0(MAXMATDIM)
	character*3 nm_fmt

	character*40 string	/'Ind1, Ind2, Ind3, Ind4, Ind5, Ind6,'/

	logical*1 cmt_readsegment,cmt_getchan

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

	i1dmmy = INP_IA(string(1:6*ndim(nmat)-2)//' ',IND0,ndim(nmat))

	nord=ndim(nmat)
	do ii=1,nord
	  if(ind0(ii).lt.0 .or. ind0(ii).ge.res(ii,nmat)) then
	    write(6,'('' Ind'',I1,'' is out of range'')') ii
	    return
	  endif
	end do

	do ii=1,nord
	  jnd0(ii)=ind0(ii)
	end do
	if(matmode(nmat).eq.1) call ordl(jnd0(1),nord)
	if(matmode(nmat).eq.2) call ordl(jnd0(2),nord-1)

	do ii=1,nord
	  seg0(ii)=     jnd0(ii) / step(ii,nmat)
	  iad0(ii)=mod( jnd0(ii) , step(ii,nmat) )
	end do

	if(matmode(nmat).eq.1) then
	   nblo=iad_sdim0(seg0,nord)
	   ipeso=iad_sfactor(jnd0,nord)
	elseif(matmode(nmat).eq.2) then
	   nblo=iad_sdim0(seg0(2),nord-1)
	   ipeso=iad_sfactor(jnd0(2),nord-1)
	else
	   nblo=iad_ndim0(seg0,mblr(1,nmat),nord)
	   ipeso=1
	endif
	iadd=iad_ndim0(iad0,step(1,nmat),nord)

	if(.not.cmt_readsegment(%val(cmt(nmat)),nblo,bufl(0,nmat),ndrec)) return
        lval=bufl(iadd,nmat)
	lval=lval*ipeso

	write(nm_fmt,'(i3)')nord
	write(6,'(''  Mat('','//nm_fmt//'i6,'' ) ='',i)') (jnd0(ii),ii=1,nord),lval
	write(6,'(''  Seg('','//nm_fmt//'i6,'' ) ='',i)') (seg0(ii),ii=1,nord),nblo
	write(6,'(''  Ind('','//nm_fmt//'i6,'' ) ='',i)') (iad0(ii),ii=1,nord),iadd

cccccc	if(.not.cmt_getchan(%val(cmt(nmat)),jnd0,llval)) return
cccccc	write(6,'(''  cmt_getchan ==> '',i6,'' iok='',i3)') llval

	error=.false.
	return

	END

	subroutine MADD_getproje(nmat)

#include "cmat.inc"

	CHARACTER PROJEFILE*40

	logical*1 cmt_getproje

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

	if(matmode(nmat).gt.0) then
	  do jj=0,MAXRES-1
	    proje(jj,1)=0
	  end do
	  if(.not.cmt_getproje(%val(cmt(nmat)),1,proje(0,1))) return
	  write(projefile,'(''proje#0.dat'')')
	  CALL WRITEDATL(0,projefile,proje(0,1),res(1,nmat),4,KV)
	  lprojefile=lengthc(projefile)
	  if(kv.gt.0) then
            projefile='proje.dat'
	    lprojefile=lengthc(projefile)
	    WRITE(6,'(1X,A,I1)') projefile(1:lprojefile)//'|L:',(res(1,nmat)+1023)/1024
	  endif
	else
	  do ii=1,ndim(nmat)
	    do jj=0,MAXRES-1
	      proje(jj,1)=0
	    end do
	    if(.not.cmt_getproje(%val(cmt(nmat)),ii,proje(0,1))) return
	    write(projefile,'(''proj'',i1.1,''#0.dat'')') ii
	    CALL WRITEDATL(0,projefile,proje(0,1),res(ii,nmat),4,KV)
	    lprojefile=lengthc(projefile)
	    if(kv.gt.0) then
	      projefile=projefile(1:5)//projefile(8:)
	      lprojefile=lengthc(projefile)
	      WRITE(6,'(1X,A,I1)') projefile(1:lprojefile)//'|L:',(res(ii,nmat)+1023)/1024
	    endif
	  end do
	endif

	error=.false.
	return

	END

	subroutine MADD_putproje(nmat)

#include "cmat.inc"

	CHARACTER PROJEFILE*40

	integer ifrm	/4/

	logical*1 cmt_putproje

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

	if(readonly(nmat)) then
	  i1dmmy = inp_msg('Matrix is opened READONLY')
	  i1dmmy = inp_msg('Please open it with RWOPEN')
	  return
	endif

	do ii=1,ndim(nmat)
	  if(res(ii,nmat).GT.MAXRES) then
	    write(6,*) 'Matrix dimension is too big'
	    return
	  endif
	end do

	if(matmode(nmat).gt.0) then
	    do jj=0,MAXRES-1
	      proje(jj,1)=0
	    end do
	    CALL readdatL(1,projefile,proje(0,1),res(1,nmat),ifrm,KV)
	    if(kv.le.0) then
	      return
	    endif
	    ifrm=mod(kv,100)
	    if(kv/100.ne.res(1,nmat)) then
	      i1dmmy = inp_msg('incompatible size')
	      return
	    endif
	  if(inp_yes('You really want to store this as new projection'))then
	    if(.not.cmt_putproje(%val(cmt(nmat)),1,proje(0,1))) return
	    error=.false.
	  endif
	else
	  do ii=1,ndim(nmat)
	    write(6,*) 'projection of parameter #',ii
	    do jj=0,MAXRES-1
	      proje(jj,ii)=0
	    end do
	    CALL readdatL(1,projefile,proje(0,ii),res(ii,nmat),ifrm,KV)
	    if(kv.le.0) then
	      return
	    endif
	    ifrm=mod(kv,100)
	    if(kv/100.ne.res(ii,nmat)) then
	      i1dmmy = inp_msg('incompatible size')
	      return
	    endif
	  end do
	  if(inp_not('You really want to store these as new projections'))then
	    do ii=1,ndim(nmat)
	      if(.not.cmt_putproje(%val(cmt(nmat)),ii,proje(0,ii))) return
	    end do
	    error=.false.
	  endif
	endif
	
	return

	END

	subroutine MADD_STATISTICS(nmat)

!	Determina alcune utili informazioni per un particolare segmento
!	o per tutta la matrice.

#include "cmat.inc"

	PARAMETER (MAXNBITS=32)			! max # of bits/channel
	PARAMETER (MODESTEP=4)			! 4 variants for each mode

	logical*1 frequenze  /.false./

	PARAMETER (MAXFREQ=1024)
	integer*8 fdistr(0:MAXFREQ+1)
	integer*8 nonempty,totnrec,totarea,totzero,totnonz,totchannels
	integer*8 totsegs(0:256),totdrec(0:256)
	integer   maxvero,maxsaved
	real*8    xnorm,prob,totprob

	character*8 cmode

	logical*1 cmt_info,cmt_readsegment

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

	nsegs=nseg(nmat)
	nwords=1
	do ii=1,ndim(nmat)
	  nwords=nwords*step(ii,nmat)
	end do

	i1dmmy = inp_i2('Segment number from, to (-1 if all) ',numseg1,numseg2)
	if(numseg1.lt.0) then
	  numseg1=0
	  numseg2=nsegs-1
	else
	  numseg1=max(0,min(numseg1,nsegs-1))
	  numseg2=max(0,min(numseg2,nsegs-1))
	endif
	if(numseg1.gt.numseg2) call swapl(numseg1,numseg2)
	jblo1=numseg1
	jblo2=numseg2

	i1dmmy = inp_ask('Want to have the frequency distribution of counts',frequenze)
	if(frequenze) then
	  do ii=0,MAXFREQ+1
	    fdistr(ii)=0
	  end do
	endif

	nonempty=0
	totarea=0
	totzero=0
	totnonz=0
	totnrec=0
	do ii=0,256
	  totsegs(ii)=0
	  totdrec(ii)=0
	end do
	maxvero=0
	maxsaved=0

	write(6,*)
	write(6,'(1x,''      #    Mode   #Recs      Minval      Maxval'',''     Nonzero        Area'')')
	do nblo=jblo1,jblo2
	  if(.not.cmt_readsegment(%val(cmt(nmat)),nblo,bufl(0,nmat),ndrec)) return
	  if(ndrec.lt.0) return
	  if(.not.cmt_info(%val(cmt(nmat)),-4,mode,minval)) return
	  totdrec(mode)=totdrec(mode)+ndrec
	  totsegs(mode)=totsegs(mode)+1
	  if(ndrec.gt.0) then
	    nonempty=nonempty+1
	    totnrec=totnrec+ndrec
	    maxval=minval
	    nonzero=0
	    ltot=0
	    do ii=0,nwords-1
	      ldat=bufl(ii,nmat)
	      if(ldat.ne.0) then
	        totnonz=totnonz+1
	        totarea=totarea+abs(ldat)
		maxvero=max(maxvero,abs(ldat))
	      else
	        totzero=totzero+1
	      endif
	      ldat=ldat-minval
	      maxval =max(maxval ,ldat)
	      maxsaved=max(maxsaved,ldat)
	      if(ldat.gt.0) nonzero=nonzero+1
	      ltot=ltot+ldat
	      if(frequenze) then
	        if(ldat.le.MAXFREQ) then
	          fdistr(ldat)=fdistr(ldat)+1
	        else
	          fdistr(MAXFREQ+1)=fdistr(MAXFREQ+1)+1
	        endif
	      endif
	    end do
	    if(mode.ne.0) then
	      if(mode.le.MAXNBITS) then
	        write(cmode,'(i3,'' bits'')') mode
	      elseif(mode.eq.MAXNBITS+0*MODESTEP+1) then
	        cmode='   listW'
	      elseif(mode.eq.MAXNBITS+0*MODESTEP+2) then
	        cmode='   listL'
	      elseif(mode.eq.MAXNBITS+1*MODESTEP+1) then
	        cmode='   token'
	      elseif(mode.eq.MAXNBITS+2*MODESTEP+1) then
	        cmode='     BSM'
	      endif
	      write(6,'(i8,a,i8,5i12)') nblo,cmode,ndrec,minval,maxval,nonzero,ltot
	    endif
	  else
	    totzero=totzero+nwords
	    if(frequenze) fdistr(0)=fdistr(0)+nwords
	  endif
	end do
	write(6,*)
	ntotbits=0
	ntotdrec=0
	do ii=0,256
	  if(totsegs(ii).gt.0) then
	    if(ii.eq.0) then
	      cmode='   empty'
	    elseif(ii.le.MAXNBITS) then
	      write(cmode,'(i3,'' bits'')') ii
	      ntotbits=ntotbits+totsegs(ii)
	      ntotdrec=ntotdrec+totdrec(ii)
	    elseif(ii.eq.MAXNBITS+0*MODESTEP+1) then
	      cmode='   listW'
	    elseif(ii.eq.MAXNBITS+0*MODESTEP+2) then
	      cmode='   listL'
	    elseif(ii.eq.MAXNBITS+1*MODESTEP+1) then
	      cmode='   token'
	    elseif(ii.eq.MAXNBITS+2*MODESTEP+1) then
	      cmode='     BSM'
	    endif
	    write(6,'(5x,a,i8,'' segments'',i10,'' disk_records'')') cmode,totsegs(ii),totdrec(ii)
	  endif
	end do
	if(ntotbits.gt.0) then
	  write(6,'(5x,''sum_bits'',i8,'' segments'',i10,'' disk_records'')') ntotbits,ntotdrec
	endif
	write(6,'(5x,''   TOTAL'',i8,'' segments'',i10,'' disk_records'')') nonempty,totnrec
	totchannels=nwords
	totchannels=totchannels*(jblo2-jblo1+1)
	                        write(6,'(               i21,'' channels         '')') totchannels
	                        write(6,'(               i21,'' counts           '')') totarea
	if(totzero.gt.0)        write(6,'(               i21,'' empty channels   '')') totzero
	                        write(6,'(               i21,'' maximum value    '')') maxvero
	if(maxsaved.ne.maxvero) write(6,'(               i21,'' max. above minval'')') maxsaved
	write(6,*)

	if(frequenze) then
	  write(6,*) ' Frequency distribution of counts (above minval)'
	  write(6,*) ' Value            Counts   Probability     Tot.Prob.'
	  xnorm=nwords
	  xnorm=xnorm*(jblo2-jblo1+1)
	  totprob=0
	  do ii=0,MAXFREQ
	    prob=fdistr(ii)/xnorm
	    totprob=totprob+prob
	    if(fdistr(ii).ne.0) write(6,'(i7,i18,2f14.6)') ii,fdistr(ii),prob,totprob
	  end do
	  if(fdistr(MAXFREQ+1).ne.0) then
	    prob=fdistr(MAXFREQ+1)/xnorm
	    totprob=totprob+prob
	    write(6,'('' >'',i5,i18,2f14.6)') MAXFREQ,fdistr(MAXFREQ+1),prob,totprob
	  endif
	endif

	error=.false.

	return

	end

	subroutine MADD_DECOMPRESS2D(nmat)

! deCOMPRIME una matrice bidimensionale compressa producendo
! una matrice di dimensioni RES(1)*RES(2)  organizzata
! in RES(2) colonne di RES(1) canali di 16 bit (un record per colonna)

#include "cmat.inc"

	CHARACTER MATFILE*40

	INTEGER*4 lspec(0:MAXRES-1)
	INTEGER*2 ispec(0:MAXRES-1)
	byte bspec(0:MAXRES-1)
	INTEGER	  lproje(0:MAXRES-1,2)
	INTEGER	  LUNMAT /0/
	integer	  gvec(2)
	integer ind1	/1/
	integer ind2	/2/
	integer nbch	/2/

	logical*1 different
	logical*1 cmt_readspec,cmt_getproje

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

	if(ndim(nmat).ne.2) then
	  i1dmmy = inp_msg('This command valid only for 2D Matrices')
	  return
	endif

	do ii=1,ndim(nmat)
	  if(res(ii,nmat).GT.MAXRES) then
	    write(6,*) 'Matrix dimension is too big'
	    return
	  endif
	end do

	if(matmode(nmat).eq.0)then
	  i1dmmy = INP_i2('Order of indexes',ind1,ind2)
	  if(ind1.lt.1 .or. ind1.gt.2) return
	  if(ind2.lt.1 .or. ind2.gt.2) return
	  if(ind1.eq.ind2) return
	else
	  ind1=1
	  ind2=2
	endif

	i1dmmy = inp_i1('How many bytes per channel (1|2|4) ',nbch)
	if(nbch.ne.1 .AND. nbch.ne.2 .AND. nbch.ne.4) return

30	i1dmmy = INP_CH('File_name of the deCompressed Matrix',MATFILE)

	ires1=((res(ind1,nmat)+1023)/1024)*1024

	LREC=(ires1/4)*nbch
	MATSIZE=RES(ind2,nmat)*ires1
CVMS	WRITE(6,*) ' The Matrix has  ',RES(ind2,nmat)  ,'  records'
	WRITE(6,*) ' Matrix size is  ',(MATSIZE*nbch)/1024  ,'  kbytes'

CVMS	WRITE(6,*) ' For a total of  ',MATSIZE      ,'  disk blocks'

	WRITE(6,'(A)') '  Its filename is         '//MATFILE

	if(lunmat.le.0) call LIB$GET_LUN(lunmat)
	if(lunmat.le.0) then
	  i1dmmy = inp_msg('Could not get a LUN')
	  return
	endif

CVMS	OPEN(UNIT=LUNMAT,FILE=MATFILE,ACCESS='DIRECT',STATUS='OLD',

CVMS	1    ORGANIZATION='sequential',RECL=LREC,MAXREC=MATSIZE,ERR=40)
	OPEN(UNIT=LUNMAT,FILE=MATFILE,STATUS='OLD',ERR=40)
	i1dmmy = inp_msg('A compatible matrix with this name already exists')
	if(inp_not('Create a new matrix')) then
	  CLOSE(LUNMAT)
	
CVMS	  goto 40
	  goto 30
	else
CVMS	  goto 60
	  return
	endif

40	WRITE(6,*) ' Creating a decompressed matrix file ...'
CVMS	OPEN(UNIT=LUNMAT,FILE=MATFILE,ACCESS='DIRECT',ORGANIZATION='sequential',STATUS='NEW',
CVMS	1    RECL=LREC,MAXREC=MATSIZE,INITIALSIZE=MATSIZE,err=50)
#if defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER)
	OPEN(UNIT=LUNMAT,FILE=MATFILE,ACCESS='DIRECT',STATUS='NEW',RECL=LREC,err=50)
#else
	OPEN(UNIT=LUNMAT,FILE=MATFILE,ACCESS='DIRECT',STATUS='NEW',RECL=4*LREC,err=50)
#endif
	goto 60

50	i1dmmy = inp_msg('Error creating the deCompressed matrix')
	return

60	do ii=0,MAXRES-1
	   lproje(ii,ind1)=0
	   lproje(ii,ind2)=0
	end do
	WRITE(6,*)

	call cmt_readspec_reset		! reset internal buffers
	gvec(ind1)=-1
	do ii2=0,res(ind2,nmat)-1
	  gvec(ind2)=ii2
	  if(.not. cmt_readspec(%val(cmt(nmat)),gvec,lspec)) then
	    write(6,*) 'CMT_READSPEC returned an error condition'
	    CLOSE(LUNMAT)
	    return
	  endif
	  isum=0
	  if(nbch.eq.1) then
	    do ii1=0,ires1-1
	      ldat=lspec(ii1)
	      isum=isum+ldat
	      lproje(ii1,ind1)=lproje(ii1,ind1)+ldat
	      bspec(ii1)=min(127,max(-128,ldat))
	    end do
	    do ii1=ires1,res(ind1,nmat)-1
	      bspec(ii1)=0
	    end do
	    WRITE(LUNMAT,rec=(ii2+1)) (bspec(ii1),ii1=0,ires1-1)
	  elseif(nbch.eq.2) then
	    do ii1=0,ires1-1
	      ldat=lspec(ii1)
	      isum=isum+ldat
	      lproje(ii1,ind1)=lproje(ii1,ind1)+ldat
	      ispec(ii1)=min(32767,max(-32768,ldat))
#if defined( _GW_BIG_ENDIAN )
	      call swap_bytes(ispec(ii1),2)
#endif
	    end do  
	    do ii1=ires1,res(ind1,nmat)-1
	      ispec(ii1)=0
	    end do
	    WRITE(LUNMAT,rec=(ii2+1)) (ispec(ii1),ii1=0,ires1-1)
	  else
	    do ii1=0,ires1-1
	      ldat=lspec(ii1)
	      isum=isum+ldat
	      lproje(ii1,ind1)=lproje(ii1,ind1)+ldat
#if defined( _GW_BIG_ENDIAN )
	      call swap_bytes(lspec(ii1),4)
#endif
	    end do
	    do ii1=ires1,res(ind1,nmat)-1
	      lspec(ii1)=0
	    end do
	    WRITE(LUNMAT,rec=(ii2+1)) (lspec(ii1),ii1=0,ires1-1)
	  endif
	  lproje(ii2,ind2)=isum
#if defined( __GFORTRAN__ )
	  WRITE(6,'(a3,''Rec#'',I8,I16)')GO_UP_ONE_LINE, ii2,isum
#else
	  WRITE(6,'(a3,''Rec#'',I8,I)')GO_UP_ONE_LINE, ii2,isum
#endif
	  call flush(6)
	end do

	CLOSE(LUNMAT)

	if(.not.cmt_getproje(%val(cmt(nmat)),ind1,proje(0,ind1))) return
	if(.not.cmt_getproje(%val(cmt(nmat)),ind2,proje(0,ind2))) return

	if(matmode(nmat).gt.0) then
	  different=.false.
	  DO II=0,RES(ind1,nmat)-1
	    if(lproje(ii,ind1).ne.proje(ii,ind1)) different=.true.
	  end do
	  if(different) then
	    write(6,*) 'Calculated projection is different from stored one'
	    CALL WRITESPECL('PROJE.DAT',lproje(0,ind1),'L',RES(ind1,nmat),KV)
	    WRITE(6,'(1X,A,I1)')'PROJE.DAT|L:',(RES(ind1,nmat)+1023)/1024
	  endif
	else
	  different=.false.
	  DO II=0,RES(ind1,nmat)-1
	    if(lproje(ii,ind1).ne.proje(ii,ind1)) different=.true.
	  end do
	  if(different) then
	    write(6,*) 'Calculated projection1 is different from stored one'
	    CALL WRITESPECL('PROJ1.DAT',lproje(0,ind1),'L',RES(ind1,nmat),KV)
	    WRITE(6,'(1X,A,I1)')'PROJ1.DAT|L:',(RES(ind1,nmat)+1023)/1024
	  endif
	  different=.false.
	  DO II=0,RES(ind2,nmat)-1
	    if(lproje(ii,ind2).ne.proje(ii,ind2)) different=.true.
	  end do
	  if(different) then
	    write(6,*) 'Calculated projection2 is different from stored one'
	    CALL WRITESPECL('PROJ2.DAT',lproje(0,ind2),'L',RES(ind2,nmat),KV)
	    WRITE(6,'(1X,A,I1)')'PROJ2.DAT|L:',(RES(ind2,nmat)+1023)/1024
	  endif
	endif

	error=.false.
	return

	END

	subroutine MADD_COMPRESS2D(nmat)

! COMPRIME  una matrice di dimensioni IRES1*IRES2  organizzata
! in IRES2 colonne di IRES1 canali di 8/16/32 bit (un record per colonna)

#include "cmat.inc"

	CHARACTER MATFILE*40
	logical*1 symmetrized
	integer lunmat	/0/

	MACHINE_POINTERSIZE mats2

	logical*1 getmem,freemem

	i1dmmy = INP_CH('File_name of the deCompressed matrix ',MATFILE)

	i1dmmy = INP_I2('Matrix dimensions',IRES1,IRES2)
	IF(IRES1.GT.MAXRES .OR. IRES1.LT.1) then
	  i1dmmy = inp_msg('Wrong Matrix dimension_1')
	  return
	endif
	IF(IRES2.GT.MAXRES .OR. IRES2.LT.1) then
	  i1dmmy = inp_msg('Wrong Matrix dimension_2')
	  return
	endif

	if(lunmat.le.0) call LIB$GET_LUN(lunmat)
	if(lunmat.le.0) then
	  i1dmmy = inp_msg('Could not get a LUN')
	  return
	endif

	i1dmmy = inp_i1('How many bytes per channel (1|2|4) ',nbch)
	if(nbch.ne.1 .AND. nbch.ne.2 .AND. nbch.ne.4) return
	irecl=(nbch*ires1)/4

CVMS	OPEN(UNIT=LUNMAT,FILE=MATFILE,ACCESS='DIRECT',ORGANIZATION='sequential',STATUS='OLD',RECL=irecl,READONLY,ERR=140)
#if defined(Digital) || defined(__IFC) || defined(__INTEL_COMPILER)
	OPEN(UNIT=LUNMAT,FILE=MATFILE,ACCESS='DIRECT',STATUS='OLD',RECL=irecl,READONLY,ERR=140)

#elif defined(__sun_c__) || defined( __GFORTRAN__ )
	OPEN(UNIT=LUNMAT,FILE=MATFILE,ACCESS='DIRECT',STATUS='OLD',RECL=4*irecl,READONLY,ERR=140)

#elif defined( __PGI )
	OPEN(UNIT=LUNMAT,FILE=MATFILE,ACCESS='DIRECT',STATUS='OLD',RECL=4*irecl,ERR=140)
#else
#   error "Unknown FORTRAN compiler"
#endif


	symmetrized=.false.
	if(ires1.eq.ires2) then
	  i1dmmy = inp_ask('Is this a symmetrized matrix',symmetrized)
	endif

	if(opened(nmat)) call MADD_close(nmat)

	ndim(nmat)=2
	res(1,nmat)=ires1
	res(2,nmat)=ires2
	if(step(1,nmat).le.0) step(1,nmat)=min(res(1,nmat),64)
	if(step(2,nmat).le.0) step(2,nmat)=min(res(2,nmat),64)

	matmode(nmat)=0
	if(symmetrized) then
6	  i1dmmy = inp_i1('STEP of the compressed matrix',step(1,nmat))
	  step(1,nmat)=abs(step(1,nmat))
	  step(2,nmat)=step(1,nmat)
	  if(step(1,nmat)*step(2,nmat).gt. MAXSEGSIZE) then
	    write(6,*) 'Step * Step should not be bigger than',MAXSEGSIZE
	    goto 130
	  endif
	  ntile1=(res(1,nmat)+step(1,nmat)-1)/step(1,nmat)
	  ntile2=ntile1
	  matmode(nmat)=1
	else
	  i1dmmy = inp_i2('STEP_1, STEP_2 of the compressed matrix',step(1,nmat),step(2,nmat))
	  if(step(2,nmat).gt.MAXSTEP2) then
	    write(6,*) 'Step_2 should not be bigger than',MAXSTEP2
	    goto 130
	  endif
	  if(step(1,nmat)*step(2,nmat).gt. MAXSEGSIZE) then
	    write(6,*) 'Step_1 * Step_2 should not be bigger than',MAXSEGSIZE
	    goto 130
	  endif
	  ntile1=(res(1,nmat)+step(1,nmat)-1)/step(1,nmat)
	  ntile2=(res(2,nmat)+step(2,nmat)-1)/step(2,nmat)
	endif

	error=.not.getmem(nbch*res(1,nmat)*step(2,nmat),mats2)
	if(error) return
	error=.true.

	CALL MADD_new(1,'File_name of the compressed matrix')
	if(error) return
	error=.true.

	if(nbch.eq.1) then
	  call MADD__do_compress2d_b(nmat,symmetrized,res(1,nmat),step(1,nmat),step(2,nmat),ntile1,ntile2,lunmat,%val(mats2))
	elseif(nbch.eq.2) then
	  call MADD__do_compress2d_w(nmat,symmetrized,res(1,nmat),step(1,nmat),step(2,nmat),ntile1,ntile2,lunmat,%val(mats2))
	elseif(nbch.eq.4) then
	  call MADD__do_compress2d_l(nmat,symmetrized,res(1,nmat),step(1,nmat),step(2,nmat),ntile1,ntile2,lunmat,%val(mats2))
	endif
	
	if(error) return
	error=.true.

	error=.not.FREEMEM(nbch*res(1,nmat)*step(2,nmat),mats2)
	if(error) return

	error=.false.
	return

130	close(LUNMAT)
	return

140	i1dmmy = inp_MSG('Error opening the deCompressed matrix')
	return

	END

	subroutine MADD__DO_COMPRESS2D_b(nmat,symmetrized,ires1,istep1,istep2,ntile1,ntile2,lunmat,mats2)

#include "cmat.inc"

	logical*1 symmetrized
        byte mats2(0:ires1-1,0:istep2-1)

	logical*1 cmt_writesegment,cmt_putproje

	DO II=0,MAXRES-1
	  PROJE(II,1)=0
	  PROJE(II,2)=0
	end do

	isegw=0
	write(6,*)

	DO n2=0,ntile2-1
	  IR0=n2*istep2
	  IR=IR0
	  DO I2=0,istep2-1			! Legge step2 colonne
	    IR=IR+1
	    READ(lunmat,rec=IR) (mats2(I1,I2),I1=0,ires1-1)
	  end do

	  DO I2=0,istep2-1			! Calcola le Proiezioni
	    IP2=0
	    DO I1=0,iRES1-1
	      PROJE(I1,1)=PROJE(I1,1)+mats2(I1,I2)
	      IP2=IP2+mats2(I1,I2)
	    end do
	    PROJE(IR0+I2,2)=IP2
	  end do

	  DO n1=0,ntile1-1
	    kk=0
	    ioffs1=n1*istep1
	    if(symmetrized .and. n1.eq.n2) then
	      do i2=0,istep2-1
		mats2(i2+ioffs1,i2)=mats2(i2+ioffs1,i2)/2
		do i1=i2+1,istep2-1
		  mats2(i1+ioffs1,i2)=0
		end do
	      end do
	    endif
	    DO i2=0,istep2-1			! Riporta il quadrato
	      DO i1=0,istep1-1			! di step1*step2
		bufl(KK,nmat)=mats2(i1+ioffs1,i2)
		kk=kk+1
	      end do
	    end do
	    if(.not.CMT_WRITESEGMENT(%val(cmt(nmat)),isegw,bufl(0,nmat),ndrec))return
	    WRITE(6,'(a3,I8,1Hr,I8,1Hb)')GO_UP_ONE_LINE, isegw,ndrec
	    isegw=isegw+1
	    if(symmetrized. and. n1.eq.n2) goto 20
	  end do
20	end do
	close(lunmat)

	if(.not.cmt_putproje(%val(cmt(nmat)),1,proje(0,1))) return
	if(.not.cmt_putproje(%val(cmt(nmat)),2,proje(0,2))) return
	call MADD_close(nmat)

	error=.false.
	return

	end

	subroutine MADD__DO_COMPRESS2D_w(nmat,symmetrized,ires1,istep1,istep2,ntile1,ntile2,lunmat,mats2)

#include "cmat.inc"

	logical*1 symmetrized
        INTEGER*2 mats2(0:ires1-1,0:istep2-1)

	logical*1 cmt_writesegment,cmt_putproje

	DO II=0,MAXRES-1
	  PROJE(II,1)=0
	  PROJE(II,2)=0
	end do

	isegw=0
	write(6,*)

	DO n2=0,ntile2-1
	  IR0=n2*istep2
	  IR=IR0
	  DO I2=0,istep2-1			! Legge step2 colonne
	    IR=IR+1
	    READ(lunmat,rec=IR) (mats2(I1,I2),I1=0,ires1-1)
	  end do

	  DO I2=0,istep2-1			! Calcola le Proiezioni
	    IP2=0
	    DO I1=0,iRES1-1
#if defined( _GW_BIG_ENDIAN )
	      call swap_bytes(mats2(i1,i2),2)
#endif
	      PROJE(I1,1)=PROJE(I1,1)+mats2(I1,I2)
	      IP2=IP2+mats2(I1,I2)
	    end do
	    PROJE(IR0+I2,2)=IP2
	  end do

	  DO n1=0,ntile1-1
	    kk=0
	    ioffs1=n1*istep1
	    if(symmetrized .and. n1.eq.n2) then
	      do i2=0,istep2-1
		mats2(i2+ioffs1,i2)=mats2(i2+ioffs1,i2)/2
		do i1=i2+1,istep2-1
		  mats2(i1+ioffs1,i2)=0
		end do
	      end do
	    endif
	    DO i2=0,istep2-1			! Riporta il quadrato
	      DO i1=0,istep1-1			! di step1*step2
		bufl(KK,nmat)=mats2(i1+ioffs1,i2)
		kk=kk+1
	      end do
	    end do
	    if(.not.CMT_WRITESEGMENT(%val(cmt(nmat)),isegw,bufl(0,nmat),ndrec))return
	    WRITE(6,'(a3,I8,1Hr,I8,1Hb)')GO_UP_ONE_LINE, isegw,ndrec
	    call flush(6)
	    isegw=isegw+1
	    if(symmetrized. and. n1.eq.n2) goto 20
	  end do
20	end do
	close(lunmat)

	if(.not.cmt_putproje(%val(cmt(nmat)),1,proje(0,1))) return
	if(.not.cmt_putproje(%val(cmt(nmat)),2,proje(0,2))) return
	call MADD_close(nmat)

	error=.false.
	return

	end

	subroutine MADD__DO_COMPRESS2D_l(nmat,symmetrized,ires1,istep1,istep2,ntile1,ntile2,lunmat,mats2)

#include "cmat.inc"

	logical*1 symmetrized
        INTEGER*4 mats2(0:ires1-1,0:istep2-1)

	logical*1 cmt_writesegment,cmt_putproje

	DO II=0,MAXRES-1
	  PROJE(II,1)=0
	  PROJE(II,2)=0
	end do

	isegw=0
	write(6,*)

	DO n2=0,ntile2-1
	  IR0=n2*istep2
	  IR=IR0
	  DO I2=0,istep2-1			! Legge step2 colonne
	    IR=IR+1
	    READ(lunmat,rec=IR) (mats2(I1,I2),I1=0,ires1-1)
	  end do

	  DO I2=0,istep2-1			! Calcola le Proiezioni
	    IP2=0
	    DO I1=0,iRES1-1
#if defined( _GW_BIG_ENDIAN )
	      call swap_bytes(mats2(i1,i2),4)
#endif
	      PROJE(I1,1)=PROJE(I1,1)+mats2(I1,I2)
	      IP2=IP2+mats2(I1,I2)
	    end do
	    PROJE(IR0+I2,2)=IP2
	  end do

	  DO n1=0,ntile1-1
	    kk=0
	    ioffs1=n1*istep1
	    if(symmetrized .and. n1.eq.n2) then
	      do i2=0,istep2-1
		mats2(i2+ioffs1,i2)=mats2(i2+ioffs1,i2)/2
		do i1=i2+1,istep2-1
		  mats2(i1+ioffs1,i2)=0
		end do
	      end do
	    endif
	    DO i2=0,istep2-1			! Riporta il quadrato
	      DO i1=0,istep1-1			! di step1*step2
		bufl(KK,nmat)=mats2(i1+ioffs1,i2)
		kk=kk+1
	      end do
	    end do
	    if(.not.CMT_WRITESEGMENT(%val(cmt(nmat)),isegw,bufl(0,nmat),ndrec))return
	    WRITE(6,'(a3,I8,1Hr,I8,1Hb)')GO_UP_ONE_LINE, isegw,ndrec
	    call flush(6)
	    isegw=isegw+1
	    if(symmetrized. and. n1.eq.n2) goto 20
	  end do
20	end do
	close(lunmat)

	if(.not.cmt_putproje(%val(cmt(nmat)),1,proje(0,1))) return
	if(.not.cmt_putproje(%val(cmt(nmat)),2,proje(0,2))) return
	call MADD_close(nmat)

	error=.false.
	return

	end

	subroutine MADD_add

! somma due matrici compresse di uguali dimensioni
! Il risultato e' un'altra matrice compressa

#include "cmat.inc"

	real f(MAXNMAT) /MAXNMAT*1./
	integer ndrec(MAXNMAT)
	integer ntosum /2/
	character*40 prompt
	character*2 nm_fmt
	logical*1 cmt_readsegment,cmt_writesegment,cmt_getproje,cmt_putproje

	do ii=1,MAXNMAT
	  if(opened(ii)) call MADD_close(ii)
	end do
	
	i1dmmy = inp_i1('How many matrices to sum',ntosum)
	if(ntosum.lt.1) return
	if(ntosum.ge.MAXNMAT) then
	  write(6,*) 'Maximum number is',MAXNMAT
	  error=.FALSE.
	  return
	endif

	do mm=1,ntosum
	  write(prompt,'(a,i3)') 'File_name of matrix',mm
	  lprompt=lengthc(prompt)
	  call MADD_open(mm,0,prompt(1:lprompt))
	  if(error) return
	  error=.true.
	  do ii=1,ndim(mm)	
	    IF(RES(ii,1).GT.MAXRES) then
	      i1dmmy = inp_msg('Matrix dimension is too big')
	      return
	    endif
	  end do
	  if(mm.ne.1) then
	    if( ndim(mm) .NE. ndim(1) ) then
	      write(6,*) 'Incompatible dimensions'
	      return
	    endif
	    if( matmode(mm) .NE. matmode(1) ) then
	      write(6,*) 'Incompatible organisation'
	      return
	    endif
	    do ii=1,ndim(1)
	      if(res(ii,mm).ne.res(ii,1)) then
	        write(6,*) 'Incompatible axis length'
	        return
	      endif
	    end do
	    do ii=1,ndim(1)
	      if(step(ii,mm).ne.step(ii,1)) then
	        write(6,*) 'Incompatible step size'
	        return
	      endif
	    end do
	  endif
	end do

	mms=ntosum+1
	ndim(mms)=ndim(1)
	matmode(mms)=matmode(1)
	do ii=1,ndim(mms)
	  res (ii,mms)=res (ii,1)
	  step(ii,mms)=step(ii,1)
	end do
	
	CALL MADD_NEW(mms,'File_name  of the summed matrix ')
	i1dmmy = INP_WORD_RESET
	if(error) return
	error=.true.

	nwords=1
	do ii=1,ndim(mms)
	  nwords=nwords*step(ii,mms)
	end do

	write(prompt,'(a,i3,a)')'Factors to multiply the',ntosum,'  matrices'
	lprompt=lengthc(prompt)
	i1dmmy = inp_ra(prompt(1:lprompt),f,ntosum)
	i1dmmy = inp_r1('Factor to multiply the summed matrix ',f(mms))

	write(6,*)					! somma blocco a blocco
	write(nm_fmt,'(i2)')ntosum+1
	DO iw3=0,nseg(mms)-1
	  do ii=0,nwords-1
	    bufl(ii,mms)=0
	  end do
	  do mm=1,ntosum
	    if(.not.cmt_readsegment(%val(cmt(mm)),iw3,bufl(0,1),ndrec(mm))) then
	      write(6,'(a,i7,a,i3)') ' Error reading segment',iw3,'  of matrix',mm
	      return
	    endif
	    do ii=0,nwords-1
	      bufl(ii,mms)=bufl(ii,mms) +  f(mm)*bufl(ii,1)
	    end do
	  end do
	  do ii=0,nwords-1
	    bufl(ii,mms)=f(mms)*bufl(ii,mms)
	  end do
	  if(.not.cmt_writesegment(%val(cmt(mms)),iw3,bufl(0,mms),ndrec(mms))) then
	    write(6,'(a,i7)') ' Error writing segment',iw3
	    return
	  endif
	  if(ndrec(mms).ge.0) WRITE(6,'(a3,i7,'//nm_fmt//'i4)')GO_UP_ONE_LINE, iw3,(ndrec(ii),ii=1,ntosum+1)
	  call flush(6)
	end do

	do ii=1,ndim(mms)				! le proiezioni 
	  do jj=0,res(ii,1)-1
	    proje(jj,1)=0
	  end do
	  do mm=1,ntosum
	    if(.not.cmt_getproje(%val(cmt(mm)),ii,proje(0,2))) then
	      write(6,'(a,i3,a,i3)') ' Error reading projection',ii,'  of matrix',mm
	      return
	    endif
	    do jj=0,res(ii,mms)-1
	      proje(jj,1)=proje(jj,1) + f(mm)*proje(jj,2)
	    end do
	  end do
	  do jj=0,res(ii,mms)-1
	    proje(jj,1)=f(mms)*proje(jj,1)
	  end do
	  if(.not.cmt_putproje(%val(cmt(mms)),ii,proje(0,1))) then
	    write(6,'(a,i3)') ' Error writing projection',ii
	    return
	  endif
	end do

	do mm=1,ntosum+1
	  call madd_close(mm)
	end do

	error=.false.
	return

	END

	SUBROUTINE MADD_SHIFT(m1,m2)

! impacca e ordina un cubo producendo una nuova versione dello stesso

#include "cmat.inc"

	logical*1 reduce	/.FALSE./
	integer npack(MAXMATDIM),nshif(MAXMATDIM)
	integer knn1(MAXMATDIM),knn2(MAXMATDIM)
	integer nwords1,nwords2
	integer newch1i,newch1f,newch2i,newch2f
	integer oldch1i,oldch1f,oldch2i,oldch2f

	logical*1 cmt_readsegment,cmt_writesegment,cmt_getproje,cmt_putproje

	if(.not.opened(m1)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

	ndim(m2)=ndim(m1)
	nndim=ndim(m1)
	matmode(m2)=matmode(m1)

	nwords1=1
	do ii=1,nndim
	  nwords1=nwords1*step(ii,m1)
	end do

	do ii=1,nndim
	  npack(ii)=1
	  nshif(ii)=0
	end do
	reduce=.FALSE.
	if(matmode(m1).gt.0) then
	  i1dmmy = inp_i1('# of channels to pack together        ',npack(1))
	  npack(1)=max(1,npack(1))
	  reduce=reduce .OR. npack(1).gt.1
	  i1dmmy = inp_i1('# of channels to shift the packed data',nshif(1))
	  reduce=reduce .OR. nshif(1).ne.0
	  res(1,m2)=res(1,m1)/npack(1)+nshif(1)
	  i1dmmy = inp_i1('Dimension of the packed matrix',res(1,m2))
	  reduce=reduce .OR. (res(1,m2).ne.res(1,m1))
	  step(1,m2)=step(1,m1)
	  i1dmmy = inp_i1('Step of the packed matrix',step(1,m2))
	  reduce=reduce .OR. (step(1,m2).ne.step(1,m1))
	  nwords2=1
	  do ii=1,nndim
	    npack(ii)=npack(1)
	    nshif(ii)=nshif(1)
	    res(ii,m2)=res(1,m2)
	    step(ii,m2)=step(1,m2)
	    nwords2=nwords2*step(ii,m2)
	  end do
	  if(nwords2.gt.MAXSEGSIZE) then
	    write(6,*) 'Maximum size of segment is',MAXSEGSIZE
	    return
	  endif
	else
	  i1dmmy = inp_ia('# of channels to pack together on each axis ',npack,nndim)
	  do ii=1,nndim
	    npack(ii)=max(1,npack(ii))
	    reduce=reduce .OR. (npack(ii).gt.1)
	  end do
	  i1dmmy = inp_ia('# of channels to shift the packed data on each axis',nshif,nndim)
	  do ii=1,nndim
	    reduce=reduce .OR. (nshif(ii).ne.0)
	    res(ii,m2)=res(ii,m1)/npack(ii)+nshif(ii)
	  end do
	  i1dmmy = inp_ia('Dimension of each axis of the packed matrix',res(1,m2),nndim)
	  do ii=1,nndim
	    reduce=reduce .OR. (res(ii,m2).ne.res(ii,m1))
	    step(ii,m2)=step(ii,m1)
	  end do
	  i1dmmy = inp_ia('Step of each axis of the packed matrix',step(1,m2),nndim)
	  reduce=reduce .OR. (step(1,m2).ne.step(1,m1))
	  nwords2=1
	  do ii=1,nndim
	    reduce=reduce .OR. (res(ii,m2).ne.res(ii,m1))
	    nwords2=nwords2*step(ii,m2)
	  end do
	  if(nwords2.gt.MAXSEGSIZE) then
	      write(6,*) 'Maximum size of segment is',MAXSEGSIZE
	      return
	  endif
	endif

	do ii=1,nndim
	  res(ii,m2)=((res(ii,m2)+step(ii,m2)-1)/step(ii,m2))*step(ii,m2)
	end do
	
	CALL MADD_NEW(m2,'File_name of the packed matrix ')
	if(error) return
	error=.true.

	write(6,*)
	if(.not.reduce) then
	  DO iw2=0,nseg(m2)-1
	    iw1=iw2
	    if(.not.cmt_readsegment(%val(cmt(m1)),iw1,bufl(0,m1),nbits1)) then
	      write(6,*) 'Error reading old_matrix at segment',iw1
	      return
	    endif
	    nbits2=0
	    if(nbits1.gt.0) then
	      do ii=0,nwords2-1
	        bufl(ii,m2)=bufl(ii,m1)
	      end do
	      if(.not.cmt_writesegment(%val(cmt(m2)),iw2,bufl(0,m2),nbits2)) then
	        write(6,*) 'Error writing new_matrix at segment iw2'
	        return
	      endif
	    endif
	    WRITE(6,'(a3,2i8)')GO_UP_ONE_LINE, IW2,NBITS2
	    call flush(6)
	  end do
	else
	  do ii=1,nndim
	    knn2(ii)=0
	  end do
	  DO iw2=0,nseg(m2)-1
	    do ii=0,nwords2-1
	      bufl(ii,m2)=0
	    end do
	    if(nndim.eq.2) then
	      newch1i=knn2(1)*step(1,m2)
	      newch1f=newch1i+step(1,m2)-1
	      newch2i=knn2(2)*step(2,m2)
	      newch2f=newch2i+step(2,m2)-1
	      oldch1i=(newch1i-nshif(1))*npack(1)
	      oldch1f=(newch1f-nshif(1))*npack(1)
	      oldch2i=(newch2i-nshif(2))*npack(2)
	      oldch2f=(newch2f-nshif(2))*npack(2)
	      nn1i=oldch1i/step(1,m1)
	      nn1f=oldch1f/step(1,m1)
	      nn2i=oldch2i/step(2,m1)
	      nn2f=oldch2f/step(2,m1)
	      do nn2=max(0,nn2i),min(nn2f,mblr(2,m1)-1)
		knn1(2)=nn2
		do nn1=max(0,nn1i),min(nn1f,mblr(1,m1)-1)
		  knn1(1)=nn1
		  if(matmode(m1).eq.0) then
		    iw1=iad_ndim0(knn1,mblr(1,m1),nndim)
		  elseif(matmode(m1).eq.1) then
		    if(nn1.gt.nn2) goto 21
		    iw1=iad_sdim0(knn1,nndim)
		  elseif(matmode(m1).eq.2) then
		    iw1=iad_ndim0(knn1,mblr(1,m1),nndim) !???????????????????????????
		  endif
		  if(.not.cmt_readsegment(%val(cmt(m1)),iw1,bufl(0,m1),nbits1)) then
	            write(6,*) 'Error reading old_matrix at segment',iw1
	            return
	          endif
		  call madd__do_pack2(bufl(0,m1),step(1,m1),step(2,m1),knn1,
	1			      bufl(0,m2),step(1,m2),step(2,m2),knn2,npack,nshif)
21		end do
	      end do
	    elseif(nndim.eq.3) then
	      newch1i=knn2(1)*step(1,m2)
	      newch1f=newch1i+step(1,m2)-1
	      newch2i=knn2(2)*step(2,m2)
	      newch2f=newch2i+step(2,m2)-1
	      newch3i=knn2(3)*step(3,m2)
	      newch3f=newch3i+step(3,m2)-1
	      oldch1i=(newch1i-nshif(1))*npack(1)
	      oldch1f=(newch1f-nshif(1))*npack(1)
	      oldch2i=(newch2i-nshif(2))*npack(2)
	      oldch2f=(newch2f-nshif(2))*npack(2)
	      oldch3i=(newch3i-nshif(3))*npack(3)
	      oldch3f=(newch3f-nshif(3))*npack(3)
	      nn1i=oldch1i/step(1,m1)
	      nn1f=oldch1f/step(1,m1)
	      nn2i=oldch2i/step(2,m1)
	      nn2f=oldch2f/step(2,m1)
	      nn3i=oldch3i/step(3,m1)
	      nn3f=oldch3f/step(3,m1)
	      do nn3=max(0,nn3i),min(nn3f,mblr(3,m1)-1)
		knn1(3)=nn3
		do nn2=max(0,nn2i),min(nn2f,mblr(2,m1)-1)
		  knn1(2)=nn2
		  do nn1=max(0,nn1i),min(nn1f,mblr(1,m1)-1)
		    knn1(1)=nn1
		    if(matmode(m1).eq.0) then
		      iw1=iad_ndim0(knn1,mblr(1,m1),nndim)
		    elseif(matmode(m1).eq.1) then
		      if(nn1.gt.nn2) goto 31
		      if(nn2.gt.nn3) goto 32
		      iw1=iad_sdim0(knn1,nndim)
		    elseif(matmode(m1).eq.2) then
		      iw1=iad_ndim0(knn1,mblr(1,m1),nndim) !?????????????????
		    endif
		    if(.not.cmt_readsegment(%val(cmt(m1)),iw1,bufl(0,m1),nbits1)) then
	              write(6,*) 'Error reading old_matrix at segment',iw1
	              return
	            endif
		    call madd__do_pack3(bufl(0,m1),step(1,m1),step(2,m1),step(3,m1),knn1,
	1			        bufl(0,m2),step(1,m2),step(2,m2),step(3,m2),knn2,npack,nshif)
31		  end do
32		end do
	      end do
	    elseif(nndim.eq.4) then
	      newch1i=knn2(1)*step(1,m2)
	      newch1f=newch1i+step(1,m2)-1
	      newch2i=knn2(2)*step(2,m2)
	      newch2f=newch2i+step(2,m2)-1
	      newch3i=knn2(3)*step(3,m2)
	      newch3f=newch3i+step(3,m2)-1
	      newch4i=knn2(4)*step(4,m2)
	      newch4f=newch4i+step(4,m2)-1
	      oldch1i=(newch1i-nshif(1))*npack(1)
	      oldch1f=(newch1f-nshif(1))*npack(1)
	      oldch2i=(newch2i-nshif(2))*npack(2)
	      oldch2f=(newch2f-nshif(2))*npack(2)
	      oldch3i=(newch3i-nshif(3))*npack(3)
	      oldch3f=(newch3f-nshif(3))*npack(3)
	      oldch4i=(newch4i-nshif(4))*npack(4)
	      oldch4f=(newch4f-nshif(4))*npack(4)
	      nn1i=oldch1i/step(1,m1)
	      nn1f=oldch1f/step(1,m1)
	      nn2i=oldch2i/step(2,m1)
	      nn2f=oldch2f/step(2,m1)
	      nn3i=oldch3i/step(3,m1)
	      nn3f=oldch3f/step(3,m1)
	      nn4i=oldch4i/step(4,m1)
	      nn4f=oldch4f/step(4,m1)
	      do nn4=max(0,nn4i),min(nn4f,mblr(4,m1)-1)
		knn1(4)=nn4
	        do nn3=max(0,nn3i),min(nn3f,mblr(3,m1)-1)
		  knn1(3)=nn3
		  do nn2=max(0,nn2i),min(nn2f,mblr(2,m1)-1)
		    knn1(2)=nn2
		    do nn1=max(0,nn1i),min(nn1f,mblr(1,m1)-1)
		      knn1(1)=nn1
		      if(matmode(m1).eq.0) then
		        iw1=iad_ndim0(knn1,mblr(1,m1),nndim)
		      elseif(matmode(m1).eq.1) then
		        if(nn1.gt.nn2) goto 41
		        if(nn2.gt.nn3) goto 42
		        if(nn3.gt.nn4) goto 43
		        iw1=iad_sdim0(knn1,nndim)
		      elseif(matmode(m1).eq.2) then
		        iw1=iad_ndim0(knn1,mblr(1,m1),nndim) !?????????????????
		      endif
		      if(.not.cmt_readsegment(%val(cmt(m1)),iw1,bufl(0,m1),nbits1)) then
	                write(6,*) 'Error reading old_matrix at segment',iw1
	                return
	              endif
		      call madd__do_pack4(bufl(0,m1),step(1,m1),step(2,m1),step(3,m1),step(4,m1),knn1,
	1                                 bufl(0,m2),step(1,m2),step(2,m2),step(3,m2),step(4,m2),knn2,npack,nshif)
41		    end do
42		  end do
43	        end do
	      end do
	    else
	      stop 'Available only for 2D, 3D and 4D'
	    endif
	    if(.not.cmt_writesegment(%val(cmt(m2)),iw2,bufl(0,m2),nbits2))  then
	      write(6,*) 'Error writing new_matrix at segment iw2'
	      return
	    endif
	    WRITE(6,'(a3,2i8)')GO_UP_ONE_LINE, IW2,NBITS2
	    call flush(6)
	    if(matmode(m2).eq.0) then
	      call iad_nincr0(knn2,mblr(1,m2),nndim)
	    elseif(matmode(m2).eq.1) then
	      call iad_sincr0(knn2,nndim)
	    elseif(matmode(m2).eq.2) then
	      call iad_nincr0(knn2,mblr(1,m2),nndim) !????????????????????
	    endif
	  end do
	endif

	do ii=1,nndim
	  if(.not.cmt_getproje(%val(cmt(m1)),ii,proje(0,1)))return
	  if(reduce) then
	    do jj=0,maxres-1
	      proje(jj,2)=0
	    end do
	    do jj=0,res(ii,m1)-1
	      new=jj/npack(ii)+nshif(ii)
	      if(new.ge.0 .and. new.lt.res(ii,m2)) then
	        proje(new,2)=proje(new,2)+proje(jj,1)
	      endif
	    end do
	    if(.not.cmt_putproje(%val(cmt(m2)),ii,proje(0,2))) return
	  else
	    if(.not.cmt_putproje(%val(cmt(m2)),ii,proje(0,1))) return
	  endif
	end do

	call madd_close(m2)

	error=.false.
	return

	END

	subroutine madd__do_pack2(mat1,ir11,ir12,knn1,
	1			  mat2,ir21,ir22,knn2,
	1			  npack,nshif)

	integer mat1(0:ir11-1,0:ir12-1),mat2(0:ir21-1,0:ir22-1)
	integer knn1(2),knn2(2)
	integer npack(2),nshif(2)

	np1=npack(1)
	np2=npack(2)
	ns1=nshif(1)
	ns2=nshif(2)
	kof11=knn1(1)*ir11
	kof12=knn1(2)*ir12
	kof21=knn2(1)*ir21
	kof22=knn2(2)*ir22

	do i2=0,ir12-1
	  j2=(i2+kof12)/np2 + ns2
	  j2=j2-kof22
	  if(j2.ge.0 .AND. j2.lt.ir22) then
	    do i1=0,ir11-1
	      j1=(i1+kof11)/np1 + ns1
	      j1=j1-kof21
	      if(j1.ge.0 .AND. j1.lt.ir21) then
		mat2(j1,j2)=mat2(j1,j2)+mat1(i1,i2)
	      endif
	  end do
	  endif
	end do

	return

	end

	subroutine madd__do_pack3(mat1,ir11,ir12,ir13,knn1,
	1			  mat2,ir21,ir22,ir23,knn2,
	1			  npack,nshif)

	integer mat1(0:ir11-1,0:ir12-1,0:ir13-1),mat2(0:ir21-1,0:ir22-1,0:ir23-1)
	integer knn1(3),knn2(3)
	integer npack(3),nshif(3)

	np1=npack(1)
	np2=npack(2)
	np3=npack(3)
	ns1=nshif(1)
	ns2=nshif(2)
	ns3=nshif(3)
	kof11=knn1(1)*ir11
	kof12=knn1(2)*ir12
	kof13=knn1(3)*ir13
	kof21=knn2(1)*ir21
	kof22=knn2(2)*ir22
	kof23=knn2(3)*ir23

	do i3=0,ir13-1
	  j3=(i3+kof13)/np3 + ns3
	  j3=j3-kof23
	  if(j3.ge.0 .AND. j3.lt.ir23) then
	    do i2=0,ir12-1
	      j2=(i2+kof12)/np2 + ns2
	      j2=j2-kof22
	      if(j2.ge.0 .AND. j2.lt.ir22) then
	        do i1=0,ir11-1
	          j1=(i1+kof11)/np1 + ns1
	          j1=j1-kof21
	          if(j1.ge.0 .AND. j1.lt.ir21) then
		    mat2(j1,j2,j3)=mat2(j1,j2,j3)+mat1(i1,i2,i3)
	          endif
	        end do
	      endif
	    end do
	  endif
	end do

	return

	end

	subroutine madd__do_pack4(mat1,ir11,ir12,ir13,ir14,knn1,
	1			  mat2,ir21,ir22,ir23,ir24,knn2,
	1			  npack,nshif)

	integer mat1(0:ir11-1,0:ir12-1,0:ir13-1,0:ir14-1),mat2(0:ir21-1,0:ir22-1,0:ir23-1,0:ir24-1)
	integer knn1(4),knn2(4)
	integer npack(4),nshif(4)

	np1=npack(1)
	np2=npack(2)
	np3=npack(3)
	np4=npack(4)
	ns1=nshif(1)
	ns2=nshif(2)
	ns3=nshif(3)
	ns4=nshif(4)
	kof11=knn1(1)*ir11
	kof12=knn1(2)*ir12
	kof13=knn1(3)*ir13
	kof14=knn1(4)*ir14
	kof21=knn2(1)*ir21
	kof22=knn2(2)*ir22
	kof23=knn2(3)*ir23
	kof24=knn2(4)*ir24

	do i4=0,ir14-1
	  j4=(i4+kof14)/np4 + ns4
	  j4=j4-kof24
	  if(j4.ge.0 .AND. j4.lt.ir24) then
	    do i3=0,ir13-1
	      j3=(i3+kof13)/np3 + ns3
	      j3=j3-kof23
	      if(j3.ge.0 .AND. j3.lt.ir23) then
	        do i2=0,ir12-1
	          j2=(i2+kof12)/np2 + ns2
	          j2=j2-kof22
	          if(j2.ge.0 .AND. j2.lt.ir22) then
	            do i1=0,ir11-1
	              j1=(i1+kof11)/np1 + ns1
	              j1=j1-kof21
	              if(j1.ge.0 .AND. j1.lt.ir21) then
	                mat2(j1,j2,j3,j4)=mat2(j1,j2,j3,j4)+mat1(i1,i2,i3,i4)
	              endif
	            end do
	          endif
	        end do
	      endif
	    end do
	  endif
	end do

	return

	end

	subroutine MADD_TRANSPOSE(m1,m2)

! Traspone due indici di una matrice. Risultato in una nuova matrice

#include "cmat.inc"

	integer iseg(MAXMATDIM),iadd(MAXMATDIM)
	integer step1(MAXMATDIM),step2(MAXMATDIM)

	logical*1 cmt_readsegment,cmt_writesegment,cmt_getproje,cmt_putproje

	if(.not.opened(m1)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

	if(matmode(m1).gt.0) then
	  i1dmmy = inp_msg('This is meaningless for symmetrized matrices')
	  return
	endif

	if(ndim(m1).eq.2) then
	  ind1=1
	  ind2=2
	else
	  i1dmmy = inp_i2('The two indexes to transpose',ind1,ind2)
	  if(ind1.le.0 .or. ind1.gt.ndim(m1)) return
	  if(ind2.le.0 .or. ind2.gt.ndim(m1)) return
	  if(ind1.eq.ind2) return
	endif

	nord=ndim(m1)
	ndim(m2)=nord
	matmode(m2)=matmode(m1)

	do ii=1,nord
	  res (ii,m2)=res (ii,m1)
	  step(ii,m2)=step(ii,m1)
	end do
	call swapl(res (ind1,m2),res (ind2,m2))
	call swapl(step(ind1,m2),step(ind2,m2))

	if(opened(m2)) call madd_close(m2)

	CALL MADD_NEW(m2,'File_name of the transposed matrix ')
	if(error) return
	error=.true.

	nwords=1
	do ii=1,nord
	  nwords=nwords*step(ii,m1)
	end do

	write(6,*)

	do ii=1,nord
	  iseg(ii)=0
	  step1(ii)=step(ii,m1)
	  step2(ii)=step(ii,m2)
	end do

	do nblo2=0,nseg(m2)-1

	  call swapl(iseg(ind1),iseg(ind2))
	  nblo1=iad_ndim0(iseg,mblr(1,m1),nord)  
	  if(.not.cmt_readsegment(%val(cmt(m1)),nblo1,bufl(0,m1),nbits1)) return

	  nbits2=0
	  if(nbits1.ne.0) then
	    if(nbits1.gt.0) then
	      do ii=0,nwords-1
		bufl(ii,m2)=0
	      end do  
	      do ii=1,nord
		iadd(ii)=0
	      end do
	      do ii=0,nwords-1
		lval=bufl(ii,m1)
		if(lval.ne.0) then
		  itemp=iadd(ind1)
		  iadd(ind1)=iadd(ind2)
		  iadd(ind2)=itemp
		  jj=iad_ndim0(iadd,step2,nord)  
		  bufl(jj,m2)=lval
		  itemp=iadd(ind1)
		  iadd(ind1)=iadd(ind2)
		  iadd(ind2)=itemp
		endif
		call iad_nincr0(iadd,step1,nord)
	      end do  
	      if(.not.cmt_writesegment(%val(cmt(m2)),nblo2,bufl(0,m2),nbits2)) return
	    endif
	  endif
	  WRITE(6,'(a3,4i8)')GO_UP_ONE_LINE, nblo2,nblo1,nbits2,nbits1
	  call flush(6)

	  call swapl(iseg(ind1),iseg(ind2))
	  call iad_nincr0(iseg,mblr(1,m2),nord)

	end do

	do ii=1,nord
	  iseg(ii)=ii
	end do
	call swapl(iseg(ind1),iseg(ind2))
	do ii=1,nord
	  if(.not.cmt_getproje(%val(cmt(m1)),     ii ,proje)) return
	  if(.not.cmt_putproje(%val(cmt(m2)),iseg(ii),proje)) return
	end do

	call madd_close(m2)
	error=.false.

	return

	end

	subroutine MADD_GATE(nmat)

! Estrae lo spettro di un indice gattando sugli altri indici
! prepara il lavoro per i casi specifici

#include "cmat.inc"

	character gatefile*60
	character specfile*60
	integer   spettro(0:MAXRES-1)
	integer   partial(0:MAXRES-1)

	integer ngates(MAXMATDIM)
	integer wl(MAXGATES,MAXMATDIM)
	integer wh(MAXGATES,MAXMATDIM)
	integer ws(MAXGATES,MAXMATDIM)

	integer gate(4),gside
	integer cside	/1/
	integer cres
	integer iform	/4/

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif
	if(ndim(nmat).lt.2 .or. ndim(nmat).gt.4) return

	if(matmode(nmat).gt.0) then
	  cside=1
	  gside=2
10	  write(6,*) 'Input Gates'
	  call madd__inputgates(wl(1,gside),wh(1,gside),ws(1,gside),MAXGATES,res(gside,nmat),ngates(gside),gatefile)
	  if( ngates(gside) .LT. (ndim(nmat)-1) ) return
	  do ii=1,ngates(gside)
	    write(6,'(5i8)') ii,wl(ii,gside),wh(ii,gside),ws(ii,gside)
	  end do
	  if(.not.inp_yes('OK')) goto 10
	else
	  ii=inp_i1('Which index do you want to project',cside)
	  if(ii.lt.0) return
	  if(cside.lt.1 .OR. cside.gt.ndim(nmat)) return
	  do gside=1,ndim(nmat)
	    if(gside.ne.cside) then
20	      write(6,'(A,i2)') 'Input Gates on index',gside
	      call madd__inputgates(wl(1,gside),wh(1,gside),ws(1,gside),MAXGATES,res(gside,nmat),ngates(gside),gatefile)
	      if(ngates(gside).lt.1) return
	      do nn=1,ngates(gside)
		write(6,'(5i8)') nn,wl(nn,gside),wh(nn,gside),ws(nn,gside)
	      end do
	      if(.not.inp_yes('OK')) goto 20
	    endif
	  end do
	endif

	cres=res(cside,nmat)
	DO II=0,MAXRES-1
	  SPETTRO(II)=0
	  PARTIAL(II)=0
	end do

	write(6,*)

	goto(1,2,3,1,5,6,1,8,9) (MAXMATDIM-1)*matmode(nmat)+ndim(nmat)-1
	return

1	continue
	call madd_gate_2d(nmat,cside,spettro,partial,wl,wh,ws,ngates,bufl)
	goto 200

2	continue
	call madd_gate_3dn(nmat,cside,spettro,partial,wl,wh,ws,ngates,bufl)
	goto 200

3	continue
c	call madd_gate_4dn(nmat,cside,spettro,partial,wl,wh,ws,ngates,bufl)
	goto 200

5	continue
	call madd_gate_3ds(nmat,spettro,partial,wl(1,2),wh(1,2),ws(1,2),ngates(2),bufl)
	goto 200

6	continue
	call madd_gate_4ds(nmat,spettro,partial,wl(1,2),wh(1,2),ws(1,2),ngates(2),bufl)
	goto 200

8	continue
c	call madd_gate_3dh()
	goto 200

9	continue
c	call madd_gate_4dh()
	goto 200


200	if(error) return
	do ii=cres,MAXRES-1
	  spettro(ii)=0
	end do
	CALL WRITEDATL(1,SPECFILE,spettro,cres,iform,KV)
	lspecfile=lengthc(specfile)
	nk=(cres+1023)/1024
	lk=1
	if(nk.gt.9) lk=2
	if(lk .eq. 1)WRITE(6,'(1X,A,I1)') SPECFILE(1:lspecfile)//'|L:',nk
	if(lk .eq. 2)WRITE(6,'(1X,A,I2)') SPECFILE(1:lspecfile)//'|L:',nk

	error=.false.
	return

	END

	subroutine madd_gate_2d(nmat,cside,spettro,partial,wl,wh,ws,ngates,gates)

#include "cmat.inc"

	integer cside

	integer   spettro(0:MAXRES-1)
	integer   partial(0:MAXRES-1)

	integer wl(MAXGATES,MAXMATDIM)
	integer wh(MAXGATES,MAXMATDIM)
	integer ws(MAXGATES,MAXMATDIM)
	integer ngates(MAXMATDIM)

	integer gates(2,*)
	integer gside
	integer cres

	logical*1 cmt_readspecs

	gside=mod(cside,2)+1
	cres=res(cside,nmat)
	maxngg=(MAXSEGSIZE*MAXNMAT)/2

	do nn1=1,ngates(gside)
	  iws=ws(nn1,gside)
	  ngg=0
	  do iaddr1=wl(nn1,gside),wh(nn1,gside)
	    if(ngg.ge.maxngg) then
	      write(6,*) nn1,ngg,'   too many spectra in gate'
	      return
	    endif
	    ngg=ngg+1
	    gates(cside,ngg)=-1
	    gates(gside,ngg)=iaddr1
	  end do
	  write(6,'(4i8)') nn1,ngg
	  if(.not.cmt_readspecs(%val(cmt(nmat)),gates,ngg,partial)) return
	  do ii=0,cres-1
	    spettro(ii)=spettro(ii)+partial(ii)*iws
	  end do
	end do
	error=.false.
	return

	end

	subroutine madd_gate_3dn(nmat,cside,spettro,partial,wl,wh,ws,ngates,gates)

#include "cmat.inc"

	integer cside,nmat

	integer   spettro(0:MAXRES-1)
	integer   partial(0:MAXRES-1)

	integer wl(MAXGATES,MAXMATDIM)
	integer wh(MAXGATES,MAXMATDIM)
	integer ws(MAXGATES,MAXMATDIM)
	integer ngates(MAXMATDIM)

	integer gates(3,*)

	integer gside,gside1,gside2
	integer cres

	logical*1 cmt_readspecs

	cres=res(cside,nmat)
	maxngg=(MAXSEGSIZE*MAXNMAT)/3

	if(cside.eq.1) then
	  gside1=2
	  gside2=3
	elseif(cside.eq.2) then
	  gside1=1
	  gside2=3
	else
	  gside1=1
	  gside2=2
	endif

	do nn1=1,ngates(gside1)
	do nn2=1,ngates(gside2)
	  iws=ws(nn1,gside1)*ws(nn2,gside2)
	  ngg=0
	  do iaddr1=wl(nn1,gside1),wh(nn1,gside1)
	  do iaddr2=wl(nn2,gside2),wh(nn2,gside2)
	    if(ngg.ge.maxngg) then
	      write(6,*) nn1,nn2,ngg,'   too many spectra in gate'
	      return
	    endif
	    ngg=ngg+1
	    gates(cside,ngg)=-1
	    gates(gside1,ngg)=iaddr1
	    gates(gside2,ngg)=iaddr2
	  end do
	  end do
	  if(.not.cmt_readspecs(%val(cmt(nmat)),gates,ngg,partial)) return
*	  write(6,'(a3,3i8)') GO_UP_ONE_LINE,nn1,nn2,ngg
	  write(6,'(a3,4i8)') GO_UP_ONE_LINE,wl(nn1,gside1),wh(nn1,gside1),wl(nn2,gside2),wh(nn2,gside2)
	  call flush(6)
	  do ii=0,cres-1
	    spettro(ii)=spettro(ii)+partial(ii)*iws
	    partial(ii)=0
	  end do
	end do
	end do

	error=.false.
	return

	end

	subroutine madd_gate_3ds(nmat,spettro,partial,wl,wh,ws,ngates,gates)

#include "cmat.inc"

	integer   spettro(0:MAXRES-1)
	integer   partial(0:MAXRES-1)

	integer wl(MAXGATES)
	integer wh(MAXGATES)
	integer ws(MAXGATES)
	integer ngates
	integer gates(3,*)

	integer cres
	logical*1 spherical	/.FALSE./
	real    centro(2),asse(2),distn2(2)
	logical*1 putit

	logical*1 cmt_readspecs

	i1dmmy = inp_ask('Are these supposed to be spherical gates',spherical)
c	if(spherical) call inp_r1('Level of acceptance',livello)

	cres=res(1,nmat)
	maxngg=(MAXSEGSIZE*MAXNMAT)/3

	do nn1=1,ngates-1
	do nn2=nn1+1,ngates
	  if(spherical) then
	    centro(1)=(wl(nn1)+0.5 + wh(nn1)+0.5)/2.
	    centro(2)=(wl(nn2)+0.5 + wh(nn2)+0.5)/2.
	    asse  (1)= wh(nn1)+1.0 - centro(1)
	    asse  (2)= wh(nn2)+1.0 - centro(2)
	  endif
	  iws=ws(nn1)*ws(nn2)
	  naa=0
	  ngg=0
	  do iaddr2=wl(nn2),wh(nn2)
	    if(spherical) distn2(2)=((iaddr2+0.5-centro(2))/asse(2))**2 
	    do iaddr1=wl(nn1),wh(nn1)
	      naa=naa+1
	      if(spherical) then
	        distn2(1)=((iaddr1+0.5-centro(1))/asse(1))**2 
	        dist2=distn2(1)+distn2(2)
	        putit=dist2.le.1
	      else
	        putit=.TRUE.
	      endif
	      if(putit) then
	        if(ngg.ge.maxngg) then
	          write(6,*) nn1,nn2,ngg,'   too many spectra in gate'
	          return
	        endif
		ngg=ngg+1
	        gates(1,ngg)=-1
	        gates(2,ngg)=iaddr1
	        gates(3,ngg)=iaddr2
	      endif
	    end do
	  end do
	  write(6,'(8i8)') nn1,nn2,naa,ngg
	  if(.not.cmt_readspecs(%val(cmt(nmat)),gates,ngg,partial)) return
	  do ii=0,cres-1
	    spettro(ii)=spettro(ii)+partial(ii)*iws
	    partial(ii)=0
	  end do
	end do
	end do

	error=.false.
	return

	end

	subroutine madd_gate_4dn(nmat,cside,spettro,partial,wl,wh,ws,ngates,gates)

#include "cmat.inc"

	integer cside

	integer   spettro(0:MAXRES-1)
	integer   partial(0:MAXRES-1)

	integer wl(MAXGATES,MAXMATDIM)
	integer wh(MAXGATES,MAXMATDIM)
	integer ws(MAXGATES,MAXMATDIM)
	integer ngates(MAXMATDIM)

	integer gside,gside1,gside2,gside3
	integer cres

	logical*1 cmt_readspecs

	integer gates(3,*)

	cres=res(cside,nmat)
	maxngg=(MAXSEGSIZE*MAXNMAT)/3

	if(cside.eq.1) then
	  gside1=2
	  gside2=3
	  gside3=4
	elseif(cside.eq.2) then
	  gside1=1
	  gside2=3
	  gside3=4
	elseif(cside.eq.3) then
	  gside1=1
	  gside2=2
	  gside3=4
	else
	  gside1=1
	  gside2=2
	  gside3=3
	endif

	do nn1=1,ngates(gside1)
	do nn2=1,ngates(gside2)
	do nn3=1,ngates(gside3)
	  iws=ws(nn1,gside1)*ws(nn2,gside2)*ws(nn3,gside3)
	  ngg=0
	  do iaddr1=wl(nn1,gside1),wh(nn1,gside1)
	  do iaddr2=wl(nn2,gside2),wh(nn2,gside2)
	  do iaddr3=wl(nn3,gside3),wh(nn3,gside3)
	    if(ngg.ge.maxngg) then
	      write(6,*) nn1,nn2,nn3,ngg,'   too many spectra in gate'
	      return
	    endif
	    ngg=ngg+1
	    gates(cside,ngg)=-1
	    gates(gside1,ngg)=iaddr1
	    gates(gside2,ngg)=iaddr2
	    gates(gside3,ngg)=iaddr3
	  end do
	  end do
	  end do
	  write(6,'(4i8)') nn1,nn2,nn3,ngg
	  if(.not.cmt_readspecs(%val(cmt(nmat)),gates,ngg,partial)) return
	  do ii=0,cres-1
	    spettro(ii)=spettro(ii)+partial(ii)*iws
	  end do
	end do
	end do
	end do

	error=.false.
	return

	end

	subroutine madd_gate_4ds(nmat,spettro,partial,wl,wh,ws,ngates,gates)

#include "cmat.inc"

	integer   spettro(0:MAXRES-1)
	integer   partial(0:MAXRES-1)

	integer wl(MAXGATES)
	integer wh(MAXGATES)
	integer ws(MAXGATES)
	integer ngates

	integer gates(4,*)

	integer cres
	logical*1 spherical	/.FALSE./
	real    centro(3),asse(3),distn2(3)
	logical*1 putit

	logical*1 cmt_readspecs

	i1dmmy = inp_ask('Are these supposed to be spherical gates',spherical)
c	if(spherical) call inp_r1('Level of acceptance',livello)

	cres=res(1,nmat)
	maxngg=(MAXSEGSIZE*MAXNMAT)/4

	do nn1=1,ngates-2
	do nn2=nn1+1,ngates-1
	do nn3=nn2+1,ngates
	  if(spherical) then
	    centro(1)=(wl(nn1)+0.5 + wh(nn1)+0.5)/2.
	    centro(2)=(wl(nn2)+0.5 + wh(nn2)+0.5)/2.
	    centro(3)=(wl(nn3)+0.5 + wh(nn3)+0.5)/2.
	    asse  (1)= wh(nn1)+1.0 - centro(1)
	    asse  (2)= wh(nn2)+1.0 - centro(2)
	    asse  (3)= wh(nn3)+1.0 - centro(3)
	  endif
	  iws=ws(nn1)*ws(nn2)*ws(nn3)
	  naa=0
	  ngg=0
	  do iaddr3=wl(nn3),wh(nn3)
	    if(spherical) distn2(3)=((iaddr3+0.5-centro(3))/asse(3))**2 
	    do iaddr2=wl(nn2),wh(nn2)
	      if(spherical) distn2(2)=((iaddr2+0.5-centro(2))/asse(2))**2 
	      do iaddr1=wl(nn1),wh(nn1)
	        naa=naa+1
	        if(spherical) then
	           distn2(1)=((iaddr1+0.5-centro(1))/asse(1))**2 
	           dist2=distn2(1)+distn2(2)+distn2(3)
	           putit=dist2.le.1
	        else
	          putit=.TRUE.
	        endif
	        if(putit) then
	          if(ngg.ge.maxngg) then
	            write(6,*) nn1,nn2,nn3,ngg,'   too many spectra in gate'
	            return
	          endif
	          ngg=ngg+1
	          gates(1,ngg)=-1
	          gates(2,ngg)=iaddr1
	          gates(3,ngg)=iaddr2
	          gates(4,ngg)=iaddr3
	        endif
	      end do
	    end do
	  end do
	  write(6,'(a3,8i8)')GO_UP_ONE_LINE, nn1,nn2,nn3,naa,ngg
	  call flush(6)
	  if(.not.cmt_readspecs(%val(cmt(nmat)),gates,ngg,partial)) return
	  do ii=0,cres-1
	    spettro(ii)=spettro(ii)+partial(ii)*iws
	  end do
	end do
	end do
	end do


	error=.false.
	return

	end

	subroutine MADD__inputgates(wl,wh,ws,MAXGATES,res,ngates,gatefile)

	INTEGER wl(MAXGATES)
	INTEGER wh(MAXGATES)
	INTEGER ws(MAXGATES)
	integer res,ngates
	character gatefile*(*)
	integer LUNIN /0/
	logical*1 filein

	ngates=0

	i1dmmy = inp_ask('Gates from file',filein)
	if(filein) then
	  i1dmmy = inp_ch('Filename',gatefile)
	  if(LUNIN.le.0) call LIB$GET_LUN(LUNIN)
	  if(LUNIN.le.0) then
	    i1dmmy = inp_msg('Could not get a LUN')
	    return
	  endif
	  call filetype(gatefile,'gates')
	  open(unit=LUNIN,file=gatefile,status='old',err=200)
	  call inp_showlun(lluin,lluout)
	  call inp_setlun(LUNIN,0)		!  NO input prompts
	endif
	ng=1
6	iin=inp_i3('Low, High, Sign(1 or -1)',wl(ng),wh(ng),ws(ng))
	if(iin.gt.0 .and.
	1	wl(ng).ge.0 .and. wl(ng).lt.res .and.
	1	wh(ng).ge.0 .and. wh(ng).lt.res) then
	   if(wl(ng).gt.wh(ng)) call swapl(wl(ng),wh(ng))
	   if(ws(ng).ge.0) then
	      ws(ng)=1
	   else
	      ws(ng)=-1
	   endif
	   ng=ng+1
	   if(ng.le.MAXGATES) goto 6
	else
	   ng=ng-1
	endif
	ngates=ng
	if(filein) then
	  call inp_setlun(lluin,lluout)	
	  close(LUNIN)
	endif

	return

200	write(6,*) 'Error opening ',gatefile
	return

	end

	subroutine MADD_GRID(nmat)

#include "cmat.inc"

	character gatefile*60
	character specfile*60
	integer   spettro(0:MAXRES-1)

	integer ngates
	integer wl(MAXGATES)
	integer wh(MAXGATES)
	integer ws(MAXGATES)

	integer gate(4),gside
	integer cside	/1/
	integer cres
	integer iform	/4/
	integer idelta /1/
	integer ntimes /20/

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif
	if(ndim(nmat).lt.2 .or. ndim(nmat).gt.4) return
	if(matmode(nmat).ne.1) return

	cside=1
	gside=2
10	write(6,*) 'Input Gates'
	call madd__inputgates(wl,wh,ws,MAXGATES,res(1,nmat),ngates,gatefile)
	if( ngates .LT. (ndim(nmat)-1) ) return
	do ii=1,ngates
	  write(6,'(5i8)') ii,wl(ii),wh(ii)
	end do
	if(.not.inp_yes('OK')) goto 10

	i1dmmy = inp_i2('Delta, times',idelta,ntimes)
	cres=res(cside,nmat)
	DO II=0,MAXRES-1
	  SPETTRO(II)=0
	end do

	write(6,*)

	goto(2,3,4) ndim(nmat)-1
	return

2	continue
	do ii=1,ntimes
	  call madd_grid_2ds(nmat,wl,wh,ngates,step(2,nmat),bufl(0,nmat),bufl(0,nmat+1),atot)
	  jj=(wl(1)+wh(1))/2
	  spettro(jj)=atot
	  do jj=1,ngates
	    wl(jj)=wl(jj)+idelta
	    wh(jj)=wh(jj)+idelta
	  end do
	end do
	goto 200

3	continue
	do ii=1,ntimes
	  call madd_grid_3ds(nmat,wl,wh,ngates,step(2,nmat),bufl(0,nmat),bufl(0,nmat+1),atot)
	  jj=(wl(1)+wh(1))/2
	  spettro(jj)=atot
	  do jj=1,ngates
	    wl(jj)=wl(jj)+idelta
	    wh(jj)=wh(jj)+idelta
	  end do
	end do
	goto 200

4	continue
	do ii=1,ntimes
	  call madd_grid_4ds(nmat,wl,wh,ngates,step(2,nmat),bufl(0,nmat),bufl(0,nmat+1),atot)
	  jj=(wl(1)+wh(1))/2
	  spettro(jj)=atot
	  do jj=1,ngates
	    wl(jj)=wl(jj)+idelta
	    wh(jj)=wh(jj)+idelta
	  end do
	end do
	goto 200

200	if(error) return
	do ii=cres,MAXRES-1
	  spettro(ii)=0
	end do
	cres=res(1,nmat)
	CALL WRITEDATL(1,SPECFILE,spettro,cres,iform,KV)
	lspecfile=lengthc(specfile)
	nk=(cres+1023)/1024
	lk=1
	if(nk.gt.9) lk=2
	if(lk .eq. 1)WRITE(6,'(1X,A,I1)') SPECFILE(1:lspecfile)//'|L:',nk
	if(lk .eq. 2)WRITE(6,'(1X,A,I2)') SPECFILE(1:lspecfile)//'|L:',nk

	error=.false.
	return

	END

	subroutine madd_grid_2ds(nmat,wl,wh,ngates,cstep,buff,mat2,atot)

#include "cmat.inc"

	integer wl(MAXGATES)
	integer wh(MAXGATES)
	integer ngates,cstep
	integer buff(0:cstep-1,0:cstep-1)
	integer mat2(0:cstep-1,0:cstep-1)
	real atot

	real    centro(2),asse(2)
	integer ww(2),ax(2),lo(2),hi(2),lb(2)
	integer seg(2),seglo(2),seghi(2)
	logical*1 putit

	logical*1 cmt_readsegment

	cres=res(1,nmat)
	atot=0

	do nn1=    1,ngates-1
	do nn2=nn1+1,ngates
	  ww(1)=nn1
	  ww(2)=nn2
	  do ii=1,2
	    ax(ii)=wh(ww(ii))-wl(ww(ii))+1
	    if(3*ax(ii).gt.cstep) then
	       write(6,'(a,4i5)') ' Gate too wide',ww
	       return
	    endif
	    lo(ii)=wl(ww(ii))-ax(ii)
	    hi(ii)=wh(ww(ii))+ax(ii)
	    seglo(ii)=lo(ii)/cstep
	    seghi(ii)=hi(ii)/cstep
	    centro(ii)=( hi(ii) - lo(ii) )/2.
	    asse  (ii)=  ax(ii)/2.
	  end do
	  do is2=seglo(2),seghi(2)
	  do is1=seglo(1),seghi(1)
	    seg(2)=is2
	    seg(1)=is1
	    ids=iad_sdim0(seg,2)
	    if(.not.cmt_readsegment(%val(cmt(nmat)),ids,buff,nbits)) return
	    lb(2)=is2*cstep
	    lb(1)=is1*cstep
	    do ll2=max(lb(2),lo(2)),min(lb(2)+cstep-1,hi(2))
	    do ll1=max(lb(1),lo(1)),min(lb(1)+cstep-1,hi(1))
	      mat2(ll1-lo(1),ll2-lo(2))= buff(ll1-lb(1),ll2-lb(2))
	    end do
	    end do
	  end do
	  end do
	  itotp=0
	  ntotp=0
	  do ll2=0,hi(2)-lo(2)
	    distn2_2=           ((ll2-centro(2))/asse(2))**2 
	    do ll1=0,hi(1)-lo(1)
	      distn2_1=distn2_2 + ((ll1-centro(1))/asse(1))**2 
	      if(distn2_1 .LE. 1.) then
	        itotp=itotp+mat2(ll1,ll2)
	        ntotp=ntotp+1
	      endif
	    end do
	  end do
	  itotb=0
	  ntotb=0
	  do ll2=0,hi(2)-lo(2)
	  do ll1=0,hi(1)-lo(1)
	    itotb=itotb+mat2(ll1,ll2)
	    ntotb=ntotb+1
	  end do
	  end do
	  backc=float(itotb-itotp)/float(ntotb-ntotp)
	  apart=itotp - ntotp*backc
c	  write(6,'(2i3,8i8)') ww,ntotp,itotp,ntotb,itotb,int(apart)
	  atot=atot+apart
	end do
	end do
	write(6,*) (wl(1)+wh(1))/2,atot
	error=.false.
	return

	end

	subroutine madd_grid_3ds(nmat,wl,wh,ngates,cstep,buff,mat3,atot)

#include "cmat.inc"

	integer wl(MAXGATES)
	integer wh(MAXGATES)
	integer ngates,cstep
	integer buff(0:cstep-1,0:cstep-1,0:cstep-1)
	integer mat3(0:cstep-1,0:cstep-1,0:cstep-1)
	real atot

	real    centro(3),asse(3)
	integer ww(3),ax(3),lo(3),hi(3),lb(3)
	integer seg(3),seglo(3),seghi(3)
	logical*1 putit

	logical*1 cmt_readsegment

	cres=res(1,nmat)
	atot=0

	do nn1=    1,ngates-2
	do nn2=nn1+1,ngates-1
	do nn3=nn2+1,ngates
	  ww(1)=nn1
	  ww(2)=nn2
	  ww(3)=nn3
	  do ii=1,3
	    ax(ii)=wh(ww(ii))-wl(ww(ii))+1
	    if(3*ax(ii).gt.cstep) then
	       write(6,'(a,4i5)') ' Gate too wide',ww
	       return
	    endif
	    lo(ii)=wl(ww(ii))-ax(ii)
	    hi(ii)=wh(ww(ii))+ax(ii)
	    seglo(ii)=lo(ii)/cstep
	    seghi(ii)=hi(ii)/cstep
	    centro(ii)=( hi(ii) - lo(ii) )/2.
	    asse  (ii)=  ax(ii)/2.
	  end do
	  do is3=seglo(3),seghi(3)
	  do is2=seglo(2),seghi(2)
	  do is1=seglo(1),seghi(1)
	    seg(3)=is3
	    seg(2)=is2
	    seg(1)=is1
	    ids=iad_sdim0(seg,3)
	    if(.not.cmt_readsegment(%val(cmt(nmat)),ids,buff,nbits)) return
	    lb(3)=is3*cstep
	    lb(2)=is2*cstep
	    lb(1)=is1*cstep
	    do ll3=max(lb(3),lo(3)),min(lb(3)+cstep-1,hi(3))
	    do ll2=max(lb(2),lo(2)),min(lb(2)+cstep-1,hi(2))
	    do ll1=max(lb(1),lo(1)),min(lb(1)+cstep-1,hi(1))
	      mat3(ll1-lo(1),ll2-lo(2),ll3-lo(3))= buff(ll1-lb(1),ll2-lb(2),ll3-lb(3))
	    end do
	    end do
	    end do
	  end do
	  end do
	  end do
	  itotp=0
	  ntotp=0
	  do ll3=0,hi(3)-lo(3)
	    distn2_3=             ((ll3-centro(3))/asse(3))**2 
	    do ll2=0,hi(2)-lo(2)
	      distn2_2=distn2_3 + ((ll2-centro(2))/asse(2))**2 
	      do ll1=0,hi(1)-lo(1)
	        distn2_1=distn2_2 + ((ll1-centro(1))/asse(1))**2 
	        if(distn2_1 .LE. 1.) then
	          itotp=itotp+mat3(ll1,ll2,ll3)
	          ntotp=ntotp+1
	        endif
	      end do
	    end do
	  end do
	  itotb=0
	  ntotb=0
	  do ll3=0,hi(3)-lo(3)
	  do ll2=0,hi(2)-lo(2)
	  do ll1=0,hi(1)-lo(1)
	    itotb=itotb+mat3(ll1,ll2,ll3)
	    ntotb=ntotb+1
	  end do
	  end do
	  end do
	  backc=float(itotb-itotp)/float(ntotb-ntotp)
	  apart=itotp - ntotp*backc
c	  write(6,'(3i3,8i8)') ww,ntotp,itotp,ntotb,itotb,int(apart)
	  atot=atot+apart
	end do
	end do
	end do
	write(6,*) (wl(1)+wh(1))/2,atot
	error=.false.
	return

	end

	subroutine madd_grid_4ds(nmat,wl,wh,ngates,cstep,buff,mat4,atot)

#include "cmat.inc"

	integer wl(MAXGATES)
	integer wh(MAXGATES)
	integer ngates,cstep
	integer buff(0:cstep-1,0:cstep-1,0:cstep-1,0:cstep-1)
	integer mat4(0:cstep-1,0:cstep-1,0:cstep-1,0:cstep-1)
	real atot

	real    centro(4),asse(4)
	integer ww(4),ax(4),lo(4),hi(4),lb(4)
	integer seg(4),seglo(4),seghi(4)
	logical*1 putit

	logical*1 cmt_readsegment

	cres=res(1,nmat)
	atot=0

	do nn1=    1,ngates-3
	do nn2=nn1+1,ngates-2
	do nn3=nn2+1,ngates-1
	do nn4=nn3+1,ngates
	  ww(1)=nn1
	  ww(2)=nn2
	  ww(3)=nn3
	  ww(4)=nn4
	  do ii=1,4
	    ax(ii)=wh(ww(ii))-wl(ww(ii))+1
	    if(3*ax(ii).gt.cstep) then
	       write(6,'(a,4i5)') ' Gate too wide',ww
	       return
	    endif
	    lo(ii)=wl(ww(ii))-ax(ii)
	    hi(ii)=wh(ww(ii))+ax(ii)
	    seglo(ii)=lo(ii)/cstep
	    seghi(ii)=hi(ii)/cstep
	    centro(ii)=( hi(ii) - lo(ii) )/2.
	    asse  (ii)=  ax(ii)/2.
	  end do
	  do is4=seglo(4),seghi(4)
	  do is3=seglo(3),seghi(3)
	  do is2=seglo(2),seghi(2)
	  do is1=seglo(1),seghi(1)
	    seg(4)=is4
	    seg(3)=is3
	    seg(2)=is2
	    seg(1)=is1
	    ids=iad_sdim0(seg,4)
	    if(.not.cmt_readsegment(%val(cmt(nmat)),ids,buff,nbits)) return
	    lb(4)=is4*cstep
	    lb(3)=is3*cstep
	    lb(2)=is2*cstep
	    lb(1)=is1*cstep
	    do ll4=max(lb(4),lo(4)),min(lb(4)+cstep-1,hi(4))
	    do ll3=max(lb(3),lo(3)),min(lb(3)+cstep-1,hi(3))
	    do ll2=max(lb(2),lo(2)),min(lb(2)+cstep-1,hi(2))
	    do ll1=max(lb(1),lo(1)),min(lb(1)+cstep-1,hi(1))
	      mat4(ll1-lo(1),ll2-lo(2),ll3-lo(3),ll4-lo(4))= buff(ll1-lb(1),ll2-lb(2),ll3-lb(3),ll4-lb(4))
	    end do
	    end do
	    end do
	    end do
	  end do
	  end do
	  end do
	  end do
	  itotp=0
	  ntotp=0
	  do ll4=ax(4),2*ax(4)-1
	    distn2_4=           ((ll4-centro(4))/asse(4))**2 
	    do ll3=0,hi(3)-lo(3)
	      distn2_3=distn2_4 + ((ll3-centro(3))/asse(3))**2 
	      do ll2=0,hi(2)-lo(2)
	        distn2_2=distn2_3 + ((ll2-centro(2))/asse(2))**2 
	        do ll1=0,hi(1)-lo(1)
	          distn2_1=distn2_2 + ((ll1-centro(1))/asse(1))**2 
	          if(distn2_1 .LE. 1.) then
	            itotp=itotp+mat4(ll1,ll2,ll3,ll4)
	            ntotp=ntotp+1
		  endif
	        end do
	      end do
	    end do
	  end do
	  itotb=0
	  ntotb=0
	  do ll4=0,hi(4)-lo(4)
	  do ll3=0,hi(3)-lo(3)
	  do ll2=0,hi(2)-lo(2)
	  do ll1=0,hi(1)-lo(1)
	    itotb=itotb+mat4(ll1,ll2,ll3,ll4)
	    ntotb=ntotb+1
	  end do
	  end do
	  end do
	  end do
	  backc=float(itotb-itotp)/float(ntotb-ntotp)
	  apart=itotp - ntotp*backc
c	  write(6,'(4i3,8i8)') ww,ntotp,itotp,ntotb,itotb,int(apart)
	  atot=atot+apart
	end do
	end do
	end do
	end do
	write(6,*) (wl(1)+wh(1))/2,atot
	error=.false.
	return

	end

	subroutine MADD_M2D(m1,m2)

! Estrae una matrice 2D da una matrice 3D/4D con gate posto sugli altri indici
! prepara il lavoro per i casi specifici

#include "cmat.inc"

	CHARACTER*60 gatefile(2)

	integer indp1	/1/
	integer indp2	/2/
	integer indg1	/3/
	integer indg2	/4/
	integer step1,step2
	integer resg(2),stepg(2),ngates(2)

	integer wl(MAXGATES,2)
	integer wh(MAXGATES,2)
	integer ws(MAXGATES,2)

	integer hash(0:MAXRES-1,2)
	integer mhash(0:MAXRES-1,2)


	MACHINE_POINTERSIZE mat2

*	logical*1 inp_not
	logical*1 getmem,freemem

	if(.not.opened(m1)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

	if(ndim(m1).ne.3 .AND. ndim(m1).ne.4 ) then
	  i1dmmy = inp_msg('This command only for a 3D/4D matrix')
	  return
	endif

	if(matmode(m1).gt.0) then
	  indp1=1
	  indp2=2
	  indg1=3
	  indg2=4
	else
	  ii=inp_i2('Which indexes do you want to project',indp1,indp2)
	  if(ii.lt.0) return
	  if(indp1.lt.1 .or. indp1.gt.ndim(m1)) return
	  if(indp2.lt.1 .or. indp2.gt.ndim(m1)) return
	  if(indp1.eq.indp2) return
	  if(ndim(m1).EQ.3) then
	    indg1=1
	    if(indp1.eq.indg1 .OR. indp2.eq.indg1) indg1=indg1+1
	    if(indp1.eq.indg1 .OR. indp2.eq.indg1) indg1=indg1+1
	    write(6,'(1x,a,i3)') 'Gates on',indg1
	  elseif(ndim(m1).EQ.4) then
	    indg1=1
	    do ii=2,4
	      if(indg1.eq.indp1 .OR. indg1.eq.indp2) indg1=indg1+1
	    end do
	    indg2=1
	    do ii=2,4
	      if(indg2.eq.indp1 .OR. indg2.eq.indp2 .OR. indg2.eq.indg1) indg2=indg2+1
	    end do
	    write(6,'(1x,a,i3,a,i3)') 'Gates on',indg1,'  and',indg2
	  endif
	endif

	ndim(m2)=2

	res(1,m2)=res(indp1,m1)
	res(2,m2)=res(indp2,m1)
	resg(1)=res(indg1,m1)
	resg(2)=res(indg2,m1)

	step(1,m2)=step(indp1,m1)
	step(2,m2)=step(indp2,m1)
	stepg(1)=step(indg1,m1)
	stepg(2)=step(indg2,m1)

	mblr(1,m2)=mblr(indp1,m1)
	mblr(2,m2)=mblr(indp2,m1)

	call inp_mode(0)			!  Legge i gates; NO pipelining

	if(matmode(m1).gt.0) then
	  write(6,*) 'Gates for other index',indg1
40	  call madd__inputgates(wl(1,1),wh(1,1),ws(1,1),MAXGATES,resg(1),ngates(1),gatefile(1))
	  if(ngates(1).lt.1) return
	  do ii=1,ngates(1)
	   write(6,'(4i8)') ii,wl(ii,1),wh(ii,1),ws(ii,1)
	  end do
	  if(.not.inp_yes('OK')) goto 40
	else
	  jjj=indg1
	  do j=1,ndim(m1)-2
	    write(6,*) 'Gates for index#',jjj
50	    call madd__inputgates(wl(1,j),wh(1,j),ws(1,j),MAXGATES,resg(j),ngates(j),gatefile(j))
	    if(ngates(j).lt.1) return
	    do ii=1,ngates(j)
	      write(6,'(4i8)') ii,wl(ii,j),wh(ii,j),ws(ii,j)
	    end do
	    if(.not.inp_yes('OK')) goto 50
	    jjj=indg2
	  end do
	endif

	do j=1,ndim(m1)-2
	  do ii=0,MAXRES-1
	    hash(ii,j)=0
	    mhash(ii,j)=0
	  end do
	  do ii=1,ngates(j)
	    ii1=wl(ii,j)
	    ii2=wh(ii,j)
	    iis=ws(ii,j)
	    do jj=ii1,ii2
	      hash(jj,j)=iis
	    end do
	  end do
	  do ii=0,resg(j)-1
	    if(hash(ii,j).ne.0) mhash(ii/stepg(j),j)=1
	  end do
	end do

	if(opened(m2)) call madd_close(m2)

	matmode(m2)=matmode(m1)
	if((matmode(m1).eq.0) .AND. (res(1,m2).eq.res(2,m2)) ) then
	  if(inp_not('Want to symmetrize the extracted matrix')) matmode(m2)=1
	endif

	CALL MADD_NEW(m2,'File_name of the extracted matrix')
	if(error) return
	error=.true.

	do ii=0,MAXRES-1
	   proje(ii,1)=0
	   proje(ii,2)=0
	end do

	goto(1,2,3,4,5,6,7,8,9) (MAXMATDIM-1)*matmode(m1)+ndim(m1)-1
	return

1	continue
	return

2	continue
	if(matmode(m2).eq.0) then
	  call MADD_M2D_FROM_M3D_N(m1,m2,step(1,m1),step(2,m1),step(3,m1),bufl(0,m1),
	1                                step(1,m2),step(2,m2),bufl(0,m2),indp1,indp2,indg1,hash,mhash)
	elseif(matmode(m2).eq.1) then
	  call MADD_M2D_FROM_M3D_NS(m1,m2,step(1,m1),step(2,m1),step(3,m1),bufl(0,m1),
	1                                 step(1,m2),step(2,m2),bufl(0,m2),indp1,indp2,indg1,hash,mhash)
	else
	  error=.true.
	endif
	return

3	continue
	if(matmode(m2).eq.0) then
	  call MADD_M2D_FROM_M4D_N(m1,m2,step(1,m1),step(2,m1),step(3,m1),step(4,m1),bufl(0,m1),
	1                                step(1,m2),step(2,m2),bufl(0,m2),indp1,indp2,indg1,indg2,hash,mhash)
	else
	  call MADD_M2D_FROM_M4D_NS(m1,m2,step(1,m1),step(2,m1),step(3,m1),step(4,m1),bufl(0,m1),
	1                                 step(1,m2),step(2,m2),bufl(0,m2),indp1,indp2,indg1,indg2,hash,mhash)
	endif
	return

4	continue
	return

5	continue
	memsize = ( (res(1,m2)+1) * res(1,m2) )/2
	if(getmem(4*memsize,mat2)) then		! direttamente in memoria
	  call MADD_M2D_FROM_M3D_SL(m1,m2,step(1,m1),bufl(0,m1),bufl(0,m2),%val(mat2),hash(0,1),mhash(0,1))
	  error=.not.FREEMEM(4*memsize,inbuf)
	else
	  call MADD_M2D_FROM_M3D_S(m1,m2,step(1,m1),bufl(0,m1),bufl(0,m2),hash,mhash)
	endif
	return

6	continue
c	memsize = ( (res(1,m2)+1) * res(1,m2) )/2
c	if(getmem(4*memsize,mat2)) then		! direttamente in memoria
c	  call MADD_M2D_FROM_M4D_SL(m1,m2,step(1,m1),bufl(0,m1),bufl(0,m2),%val(mat2),hash(0,1),mhash(0,1))
	  error=.not.FREEMEM(4*memsize,inbuf)
c	else
c	  call MADD_M2D_FROM_M4D_S(m1,m2,step(1,m1),bufl(0,m1),bufl(0,m2),hash,mhash)
c	endif
	return

7	continue
	return

8	continue
	return

9	continue
	return

	end


	subroutine MADD_M2D_FROM_M3D_N(m1,m2,step11,step12,step13,bufr,
	1                                    step21,step22,bufw,indp1,indp2,indg,hash,mhash)

! Matrice 2D non simmetrizzata da matrice 3D non simmetrizzata
! lavora direttamente sui blocchi su disco

#include "cmat.inc"

	integer step11,step12,step13
	integer*4 bufr(0:step11-1,0:step12-1,0:step13-1)

	integer step21,step22
	integer*4 bufw(0:step21-1,0:step22-1)

	integer indp1,indp2,indg

	integer hash(0:MAXRES-1)
	integer mhash(0:MAXRES-1)

	integer knnr(3),offr(3),ip(3),stpr(3)
	integer knnw(2),offw(2)
	integer isign

	logical*1 cmt_readsegment,cmt_writesegment,cmt_getproje,cmt_putproje

	write(6,*)

	stpr(1)=step11
	stpr(2)=step12
	stpr(3)=step13
	nrr=0
	nrw=0
	irw=0
	do n2=0,mblr(2,m2)-1
	  knnr(indp2)=n2
	  offr(indp2)=n2*stpr(indp2)
	  knnw(2)=n2
	  offw(2)=n2*step22
	  do n1=0,mblr(1,m2)-1
	    knnr(indp1)=n1
	    offr(indp1)=n1*stpr(indp1)
	    knnw(1)=n1
	    offw(1)=n1*step21
	    do i2=0,step22-1
	    do i1=0,step21-1
	      bufw(i1,i2)=0
	    end do
	    end do
	    do ng=0,mblr(indg,m1)-1
	      if(mhash(ng).ne.0) then
	        knnr(indg)=ng
	        offr(indg)=ng*stpr(indg)
	        irr=iad_ndim0(knnr,mblr(1,m1),3)
	        if(.not.cmt_readsegment(%val(cmt(m1)),irr,bufr,nbits) .OR. nbits.lt.0) then
	          write(6,*) 'CMT_READSEGMENT returned an error condition in MADD_M2D_FROM_M3D_N'
		  return
		endif
	        if(nbits.gt.0) then
	          nrr=nrr+1
	          WRITE(6,'(a3,9i8)')GO_UP_ONE_LINE, offr,irr,nrr,irw,nrw
	          call flush(6)
		  do ig=0,step(indg,m1)-1
		    isign=hash(ig+offr(indg))
	            if(isign.ne.0) then
	              ip(indg)=ig
	              do i1=0,step(indp1,m1)-1
	                ip(indp1)=i1
	                do i2=0,step(indp2,m1)-1
	                  ip(indp2)=i2
	                  ival=bufw(i1,i2)
	                  ival=ival+bufr(ip(1),ip(2),ip(3))*isign
	                  bufw(i1,i2)=ival
	                end do
	              end do
	            endif
	          end do
	        endif
	      endif
	    end do
	    do i2=0,step(2,m2)-1
	      j2=i2+offw(2)
	      do i1=0,step(1,m2)-1
	        j1=i1+offw(1)
	        proje(j1,1)=proje(j1,1)+bufw(i1,i2)
	        proje(j2,2)=proje(j2,2)+bufw(i1,i2)
	      end do
	    end do
	    irw=iad_ndim0(knnw,mblr(1,m2),2)
	    if(.not.cmt_writesegment(%val(cmt(m2)),irw,bufw,nbits) .OR. nbits.lt.0) then
	      write(6,*) 'CMT_WRITESEGMENT returned an error condition in MADD_M2D_FROM_M3D_N'
	      call madd_close(m2)
	      return
	    endif
	    if(nbits.gt.0) nrw=nrw+1
	  end do
	end do

	if(.not.cmt_putproje(%val(cmt(m2)),1,proje(0,1))) return
	if(.not.cmt_putproje(%val(cmt(m2)),2,proje(0,2))) return
	call madd_close(m2)

	error=.false.
	return

	END

	subroutine MADD_M2D_FROM_M3D_NS(m1,m2,step11,step12,step13,bufr,
	1                                     step21,step22,bufw,indp1,indp2,indg,hash,mhash)

! Matrice 2D simmetrizzata da matrice 3D non simmetrizzata
! lavora direttamente sui blocchi su disco

#include "cmat.inc"

	integer step11,step12,step13
	integer*4 bufr(0:step11-1,0:step12-1,0:step13-1)

	integer step21,step22
	integer*4 bufw(0:step21-1,0:step22-1)

	integer indp1,indp2,indg

	integer hash(0:MAXRES-1)
	integer mhash(0:MAXRES-1)

	integer knnr(3),offr(3),ip(3),stpr(3)
	integer knnw(2),offw(2)
	integer isign

	logical*1 cmt_readsegment,cmt_writesegment,cmt_getproje,cmt_putproje

	write(6,*)

	stpr(1)=step11
	stpr(2)=step12
	stpr(3)=step13
	nrr=0
	nrw=0
	irw=0
	do n2=0,mblr(2,m2)-1
	  knnr(indp2)=n2
	  offr(indp2)=n2*stpr(indp2)
	  knnw(2)=n2
	  offw(2)=n2*step22
	  do n1=0,n2
	    knnr(indp1)=n1
	    offr(indp1)=n1*stpr(indp1)
	    knnw(1)=n1
	    offw(1)=n1*step21
	    do i2=0,step22-1
	    do i1=0,step21-1
	      bufw(i1,i2)=0
	    end do
	    end do
	  do iss=1,2
	    do ng=0,mblr(indg,m1)-1
	      if(mhash(ng).ne.0) then
	        knnr(indg)=ng
	        offr(indg)=ng*stpr(indg)
	        irr=iad_ndim0(knnr,mblr(1,m1),3)
	        if(.not.cmt_readsegment(%val(cmt(m1)),irr,bufr,nbits) .OR. nbits.lt.0) then
	          write(6,*) 'CMT_READSEGMENT returned an error condition in MADD_M2D_FROM_M3D_NS'
		  return
		endif
	        if(nbits.gt.0) then
	          nrr=nrr+1
	          WRITE(6,'(a3,9i8)')GO_UP_ONE_LINE, offr,irr,nrr,irw,nrw
		  call flush(6)
	          do ig=0,step(indg,m1)-1
		    isign=hash(ig+offr(indg))
	            if(isign.ne.0) then
	              ip(indg)=ig
	              do i1=0,step(indp1,m1)-1
	                ip(indp1)=i1
	                do i2=0,step(indp2,m1)-1
	                  ip(indp2)=i2
	                  ival=bufw(i1,i2)
	                  ival=ival+bufr(ip(1),ip(2),ip(3))*isign
	                  bufw(i1,i2)=ival
	                end do
	              end do
	            endif
	          end do
	        endif
	      endif
	    end do
	    if(n1.EQ.n2) goto 50
	    call swapl(knnr(indp1),knnr(indp2))
	    call swapl(offr(indp1),offr(indp2))
	    call swapl(     indp1 ,     indp2 )
	  end do

50	    if(n1.eq.n2) then
	      do i2=0,step(2,m2)-1
	        do i1=i2+1,step(1,m2)-1
	          bufw(i2,i1)=bufw(i2,i1)+bufw(i1,i2)
	          bufw(i1,i2)=0
	        end do
	      end do
	    endif
	    do i2=0,step(2,m2)-1
	      j2=i2+offw(2)
	      do i1=0,step(1,m2)-1
	        j1=i1+offw(1)
	        proje(j1,1)=proje(j1,1)+bufw(i1,i2)
	        proje(j2,1)=proje(j2,1)+bufw(i1,i2)
	      end do
	    end do
	    irw=iad_sdim0(knnw,2)
	    if(.not.cmt_writesegment(%val(cmt(m2)),irw,bufw,nbits) .OR. nbits.lt.0) then
	      write(6,*) 'CMT_WRITESEGMENT returned an error condition in MADD_M2D_FROM_M3D_NS'
	      call madd_close(m2)
	      return
	    endif
	    if(nbits.gt.0) nrw=nrw+1
	  end do
	end do

	if(.not.cmt_putproje(%val(cmt(m2)),1,proje(0,1))) return
	if(.not.cmt_putproje(%val(cmt(m2)),2,proje(0,1))) return
	call madd_close(m2)

	error=.false.
	return

	END

	subroutine MADD_M2D_FROM_M4D_N(m1,m2,step11,step12,step13,step14,bufr,
	1                step21,step22,bufw,indp1,indp2,indg1,indg2,hash,mhash)

! Matrice 2D non simmetrizzata da matrice 4D non simmetrizzata
! lavora direttamente sui blocchi su disco

#include "cmat.inc"

	integer step11,step12,step13,step14
	integer*4 bufr(0:step11-1,0:step12-1,0:step13-1,0:step14-1)

	integer step21,step22
	integer*4 bufw(0:step21-1,0:step22-1)

	integer indp1,indp2,indg1,indg2

	integer hash(0:MAXRES-1,2)
	integer mhash(0:MAXRES-1,2)

	integer knnr(4),offr(4),ip(4),stpr(4)
	integer knnw(2),offw(2)
	integer isign

	logical*1 cmt_readsegment,cmt_writesegment,cmt_getproje,cmt_putproje

	write(6,*)

	stpr(1)=step11
	stpr(2)=step12
	stpr(3)=step13
	stpr(4)=step14
	nrr=0
	nrw=0
	irw=0
	do n2=0,mblr(2,m2)-1
	  knnr(indp2)=n2
	  offr(indp2)=n2*stpr(indp2)
	  knnw(2)=n2
	  offw(2)=n2*step22
	  do n1=0,mblr(1,m2)-1
	    knnr(indp1)=n1
	    offr(indp1)=n1*stpr(indp1)
	    knnw(1)=n1
	    offw(1)=n1*step21
	    do i2=0,step22-1
	    do i1=0,step21-1
	      bufw(i1,i2)=0
	    end do
	    end do
	    do ng1=0,mblr(indg1,m1)-1
	      if(mhash(ng1,1).ne.0) then
	        knnr(indg1)=ng1
	        offr(indg1)=ng1*stpr(indg1)
	        do ng2=0,mblr(indg2,m1)-1
	          if(mhash(ng2,2).ne.0) then
	            knnr(indg2)=ng2
	            offr(indg2)=ng2*stpr(indg2)
	            irr=iad_ndim0(knnr,mblr(1,m1),4)
	            if(.not.cmt_readsegment(%val(cmt(m1)),irr,bufr,nbits) .OR. nbits.lt.0) then
	              write(6,*) 'CMT_READSEGMENT returned an error condition in MADD_M2D_FROM_M3D_N'
		      return
		    endif
	            if(nbits.gt.0) then
	              nrr=nrr+1
	              WRITE(6,'(a3,9i8)')GO_UP_ONE_LINE, offr,irr,nrr,irw,nrw
		      call flush(6)
	              do ig1=0,step(indg1,m1)-1
		        isign1=hash(ig1+offr(indg1),1)
	                if(isign1.ne.0) then
	                  do ig2=0,step(indg2,m1)-1
		            isign=isign1*hash(ig2+offr(indg2),2)
	                    if(isign.ne.0) then
	                      ip(indg1)=ig1
	                      ip(indg2)=ig2
	                      do i1=0,step(indp1,m1)-1
	                        ip(indp1)=i1
	                        do i2=0,step(indp2,m1)-1
	                          ip(indp2)=i2
	                          ival=bufw(i1,i2)
	                          ival=ival+bufr(ip(1),ip(2),ip(3),ip(4))*isign
	                          bufw(i1,i2)=ival
	                        end do
	                      end do
	                    endif
                          end do
	                endif
	              end do
	            endif
	          endif
	        end do
	      endif
	    end do
	    do i2=0,step(2,m2)-1
	      j2=i2+offw(2)
	      do i1=0,step(1,m2)-1
	        j1=i1+offw(1)
	        proje(j1,1)=proje(j1,1)+bufw(i1,i2)
	        proje(j2,2)=proje(j2,2)+bufw(i1,i2)
	      end do
	    end do
	    irw=iad_ndim0(knnw,mblr(1,m2),2)
	    if(.not.cmt_writesegment(%val(cmt(m2)),irw,bufw,nbits) .OR. nbits.lt.0) then
	      write(6,*) 'CMT_WRITESEGMENT returned an error condition in MADD_M2D_FROM_M3D_N'
	      call madd_close(m2)
	      return
	    endif
	    if(nbits.gt.0) nrw=nrw+1
	  end do
	end do

	if(.not.cmt_putproje(%val(cmt(m2)),1,proje(0,1))) return
	if(.not.cmt_putproje(%val(cmt(m2)),2,proje(0,2))) return
	call madd_close(m2)

	error=.false.
	return

	END

	subroutine MADD_M2D_FROM_M4D_NS(m1,m2,step11,step12,step13,step14,bufr,
	1                step21,step22,bufw,indp1,indp2,indg1,indg2,hash,mhash)

! Matrice 2D simmetrizzata da matrice 4D non simmetrizzata
! lavora direttamente sui blocchi su disco

#include "cmat.inc"

	integer step11,step12,step13,step14
	integer*4 bufr(0:step11-1,0:step12-1,0:step13-1,0:step14-1)

	integer step21,step22
	integer*4 bufw(0:step21-1,0:step22-1)

	integer indp1,indp2,indg1,indg2

	integer hash(0:MAXRES-1,2)
	integer mhash(0:MAXRES-1,2)

	integer knnr(4),offr(4),ip(4),stpr(4)
	integer knnw(2),offw(2)
	integer isign

	logical*1 cmt_readsegment,cmt_writesegment,cmt_getproje,cmt_putproje

	write(6,*)

	stpr(1)=step11
	stpr(2)=step12
	stpr(3)=step13
	stpr(4)=step14
	nrr=0
	nrw=0
	irw=0
	do n2=0,mblr(2,m2)-1
	  knnr(indp2)=n2
	  offr(indp2)=n2*stpr(indp2)
	  knnw(2)=n2
	  offw(2)=n2*step22
	  do n1=0,mblr(1,m2)-1
	    knnr(indp1)=n1
	    offr(indp1)=n1*stpr(indp1)
	    knnw(1)=n1
	    offw(1)=n1*step21
	    do i2=0,step22-1
	    do i1=0,step21-1
	      bufw(i1,i2)=0
	    end do
	    end do
	  do iss=1,2
	    do ng1=0,mblr(indg1,m1)-1
	      if(mhash(ng1,1).ne.0) then
	        knnr(indg1)=ng1
	        offr(indg1)=ng1*stpr(indg1)
	        do ng2=0,mblr(indg2,m1)-1
	          if(mhash(ng2,2).ne.0) then
	            knnr(indg2)=ng2
	            offr(indg2)=ng2*stpr(indg2)
	            irr=iad_ndim0(knnr,mblr(1,m1),4)
	            if(.not.cmt_readsegment(%val(cmt(m1)),irr,bufr,nbits) .OR. nbits.lt.0) then
	              write(6,*) 'CMT_READSEGMENT returned an error condition in MADD_M2D_FROM_M3D_NS'
		      return
		    endif
	            if(nbits.gt.0) then
	              nrr=nrr+1
	              WRITE(6,'(a3,9i8)')GO_UP_ONE_LINE, offr,irr,nrr,irw,nrw
	              call flush(6)
		      do ig1=0,step(indg1,m1)-1
		        isign1=hash(ig1+offr(indg1),1)
	                if(isign1.ne.0) then
	                  do ig2=0,step(indg2,m1)-1
		            isign=isign1*hash(ig2+offr(indg2),2)
	                    if(isign.ne.0) then
	                      ip(indg1)=ig1
	                      ip(indg2)=ig2
	                      do i1=0,step(indp1,m1)-1
	                        ip(indp1)=i1
	                        do i2=0,step(indp2,m1)-1
	                          ip(indp2)=i2
	                          ival=bufw(i1,i2)
	                          ival=ival+bufr(ip(1),ip(2),ip(3),ip(4))*isign
	                          bufw(i1,i2)=ival
	                        end do
	                      end do
	                    endif
                          end do
	                endif
	              end do
	            endif
	          endif
	        end do
	      endif
	    end do
	    if(n1.EQ.n2) goto 50
	    call swapl(knnr(indp1),knnr(indp2))
	    call swapl(offr(indp1),offr(indp2))
	    call swapl(     indp1 ,     indp2 )
	  end do
50	    if(n1.eq.n2) then
	      do i2=0,step(2,m2)-1
	        do i1=i2+1,step(1,m2)-1
	          bufw(i2,i1)=bufw(i2,i1)+bufw(i1,i2)
	          bufw(i1,i2)=0
	        end do
	      end do
	    endif
	    do i2=0,step(2,m2)-1
	      j2=i2+offw(2)
	      do i1=0,step(1,m2)-1
	        j1=i1+offw(1)
	        proje(j1,1)=proje(j1,1)+bufw(i1,i2)
	        proje(j2,1)=proje(j2,1)+bufw(i1,i2)
	      end do
	    end do
	    irw=iad_ndim0(knnw,mblr(1,m2),2)
	    if(.not.cmt_writesegment(%val(cmt(m2)),irw,bufw,nbits) .OR. nbits.lt.0) then
	      write(6,*) 'CMT_WRITESEGMENT returned an error condition in MADD_M2D_FROM_M3D_NS'
	      call madd_close(m2)
	      return
	    endif
	    if(nbits.gt.0) nrw=nrw+1
	  end do
	end do

	if(.not.cmt_putproje(%val(cmt(m2)),1,proje(0,1))) return
	if(.not.cmt_putproje(%val(cmt(m2)),2,proje(0,1))) return
	call madd_close(m2)

	error=.false.
	return

	END

	subroutine MADD_M2D_FROM_M3D_SL(m1,m2,istep,bufr,bufw,smat,hash,mhash)

! Matrice 2D simmetrizzata da matrice 3D simmetrizzata
! lavora su buffer m2 in memoria a LW

#include "cmat.inc"

	integer   m1,m2,istep
	integer*4 bufr(0:istep-1,0:istep-1,0:istep-1)
	integer*4 bufw(0:istep-1,0:istep-1)
	integer*4 smat(0:1)
	integer   hash(0:1)
	integer   mhash(0:1)

	integer ires,mblr1,mblr2,ind,sign
	integer knnr(3),offr(3),ip(3)
	integer knnw(2),offw(2)
	logical*1 readit

	logical*1 cmt_readsegment,cmt_writesegment,cmt_getproje,cmt_putproje

	ires=res(1,m2)

	nrt1=((mblr(1,m1)+2)*(mblr(1,m1)+1)*mblr(1,m1))/6
	nrt2=((mblr(1,m2)+1)*mblr(1,m2))/2

	nn=((ires+1)*ires)/2
	do ii=0,nn-1
	  smat(ii)=0
	end do

	write(6,*) ' Reading ...'
	write(6,*)

	nrr=0
	do ii=1,3
	  knnr(ii)=0
	end do
	do irr=0,nrt1-1
	  readit=.FALSE.
	  do ii=1,3
	    if(mhash(knnr(ii)).ne.0) readit=.TRUE.
	  end do
	  if(readit) then
	    if(.not.cmt_readsegment(%val(cmt(m1)),irr,bufr,nbits) .OR. nbits.lt.0) then
	      write(6,*) 'CMT_READSEGMENT returned an error condition in MADD_M2D_FROM_M3D_SL'
	      return
	    endif
	    if(nbits.gt.0) then
	      nrr=nrr+1
	      write(6,'(a3,6i8)')char(27)//char(91)//char(65), knnr,irr,nrr

	      do ii=1,3
	        offr(ii)=knnr(ii)*istep
	      end do
	      do ind=1,3
	        ind1=mod(ind+0,3)+1
	        ind2=mod(ind+1,3)+1
	        do ig=0,istep-1
	          ip(ind)=ig
	          jg=offr(ind)+ig
	          sign=hash(jg)
	          if(sign.ne.0) then
	            do i1=0,istep-1
	              ip(ind1)=i1
	              j1=offr(ind1)+i1
	              do i2=0,istep-1
	                ip(ind2)=i2
	                j2=offr(ind2)+i2
	                if(j2.ge.j1) then
	                  kk=j1+((j2+1)*j2)/2
	                else
	                  kk=j2+((j1+1)*j1)/2
	                endif
	                ival=smat(kk)
	                ival=ival+bufr(ip(1),ip(2),ip(3))*sign
	                smat(kk)=ival
	              end do
	            end do
	          endif
	        end do
	      end do
	    endif
	  endif
	  call iad_sincr0(knnr,3)
	end do

	write(6,*) ' Writing ...'
	write(6,*)

	do ii=1,2
	  knnw(ii)=0
	end do
	nrw=0	
	do irw=0,nrt2-1
	  do ii=1,2
	    offw(ii)=knnw(ii)*istep
	  end do
	  do i2=0,istep-1
	    j2=offw(2)+i2
	    k2=((j2+1)*j2)/2
	    if(knnw(1).eq.knnw(2)) then
	      i12=i2
	    else
	      i12=istep-1
	    endif
	    do i1=0,i12
	      j1=offw(1)+i1
	      kk=k2+j1
	      ival=smat(kk)
	      proje(j1,1)=proje(j1,1)+ival
	      proje(j2,1)=proje(j2,1)+ival
	      bufw(i1,i2)=ival
	    end do
	  end do
	  if(knnw(2).eq.knnw(1)) then
	    do i2=0,istep-1
	    do i1=i2+1,istep-1
	      bufw(i1,i2)=0
	    end do
	    end do
	  endif
	  if(.not.cmt_writesegment(%val(cmt(m2)),irw,bufw,nbits) .OR. nbits.lt.0) then
	    write(6,*) 'CMT_WRITESEGMENT returned an error condition in MADD_M2D_FROM_M3D_SL'
	    call madd_close(m2)
	    return
	  endif
	  if(nbits.gt.0) then
	    nrw=nrw+1
	    WRITE(6,'(a3,4i8)')char(27)//char(91)//char(65), knnw,irw,nrw
	  endif
	  call iad_sincr0(knnw,2)
	end do

	if(.not.cmt_putproje(%val(cmt(m2)),1,proje(0,1))) return
	if(.not.cmt_putproje(%val(cmt(m2)),2,proje(0,1))) return

	call madd_close(m2)

	error=.false.
	return

	END

	subroutine MADD_M2D_FROM_M3D_S(m1,m2,istep,bufr,bufw,hash,mhash)

! Matrice 2D simmetrizzata da matrice 3D simmetrizzata
! lavora direttamente sui buffer finali di m2 nel caso in cui la memoria non sia sufficente
! per la MADD_M2D_FROM_M3D_SL. E' circa 3 volte piu' lento di MADD_M2D_FROM_M3D_SL

#include "cmat.inc"

	integer m1,m2,istep
	integer*4 bufr(0:istep-1,0:istep-1,0:istep-1)
	integer*4 bufw(0:istep-1,0:istep-1)
	integer hash(0:1)
	integer mhash(0:1)


	integer ires,mblr1,mblr2,ind,sign
	integer knnr(3),offr(3),ip(3)
	integer knnw(2),offw(2)

	logical*1 cmt_readsegment,cmt_writesegment,cmt_getproje,cmt_putproje

	ires=res(1,m1)

	mblr1=mblr(1,m1)
	mblr2=mblr(1,m2)

	write(6,*)

	nrr=0
	nrw=0
	do n2=0,mblr2-1
	  knnw(2)=n2
	  do n1=0,n2
	    knnw(1)=n1
	    irw=iad_sdim0(knnw,2)
	    do i2=0,istep-1
	    do i1=0,istep-1
	      bufw(i1,i2)=0
	    end do
	    end do
	    do ng=0,mblr1-1
	      if(mhash(ng).ne.0) then
	        knnr(1)=n1
	        knnr(2)=n2
	        knnr(3)=ng
	        call ordl(knnr,3)
	        irr=iad_sdim0(knnr,3)
	        if(.not.cmt_readsegment(%val(cmt(m1)),irr,bufr,nbits) .OR. nbits.lt.0) then
	          write(6,*) 'CMT_READSEGMENT returned an error condition in MADD_M2D_FROM_M3D_S'
		  return
	        endif
		if(nbits.gt.0) then
	          nrr=nrr+1
	          WRITE(6,'(a3,9i8)')GO_UP_ONE_LINE, knnr,irr,nrr,knnw,irw,nrw
	          call flush(6)
		  do ii=1,3
	            offr(ii)=knnr(ii)*istep
	          end do
	          do indg=1,3
	            if(knnr(indg).eq.ng) then
	              ind1=mod(indg+0,3)+1
	              ind2=mod(indg+1,3)+1
	              do ig=0,istep-1
	                ip(indg)=ig
	                jg=offr(indg)+ig
	                if(hash(jg).ne.0) then
	                  do i1=0,istep-1
	  	            ip(ind1)=i1  
		            j1=offr(ind1)+i1
		            do i2=0,istep-1
		              ip(ind2)=i2
		              j2=offr(ind2)+i2
		              if(j2.ge.j1) then
		                k1=mod(j1,istep)
		                k2=mod(j2,istep)
		              else
		                k1=mod(j2,istep)
		                k2=mod(j1,istep)
		              endif
		              ival=bufw(k1,k2)
		              ival=ival+bufr(ip(1),ip(2),ip(3))*hash(jg)
		              bufw(k1,k2)=ival
		            end do
	                  end do
	                endif
	              end do
	            endif
	          end do
		endif
	      endif
	    end do
	    offw(1)=knnw(1)*istep
	    offw(2)=knnw(2)*istep
	    do i2=0,istep-1
	      j2=offw(2)+i2  
	      do i1=0,istep-1
	        j1=offw(1)+i1  
	        idat=bufw(i1,i2)
	        proje(j1,1)=proje(j1,1)+idat
	        proje(j2,1)=proje(j2,1)+idat
	      end do
	    end do
	    irw=iad_sdim0(knnw,2)
	    if(.not.cmt_writesegment(%val(cmt(m2)),irw,bufw,nbits) .OR. nbits.lt.0) then
	      write(6,*) 'CMT_WRITESEGMENT returned an error condition in MADD_M2D_FROM_M3D_S'
	      call madd_close(m2)
	      return
	    endif
	    if(nbits.gt.0) then
	      nrw=nrw+1
	      WRITE(6,'(a3,4i8)')GO_UP_ONE_LINE, knnw,irw,nrw
	      call flush(6)
	    endif
	  end do
	end do

	if(.not.cmt_putproje(%val(cmt(m2)),1,proje(0,1))) return
	if(.not.cmt_putproje(%val(cmt(m2)),2,proje(0,1))) return

	call madd_close(m2)

	error=.false.
	return

	END

	subroutine MADD_DIAGN(nmat)

! Da una matrice 2D simmetrizzata
! Estrae uno spettro PERPENDICOLARE alla diagonale principale
! I limiti della zona da sommare iwl-iwh sono dati dal valore della
! proiezione sull'asse x del tratto di diagonale principale considerato.
! Lo spettro viene dato con offset pari dimensione ires della matrice
! Spettro(i=x-y+offset)=Mat(x,y)
! Il nome dello spettro e' iwl_iwh.diagn
! Si puo limitare la lunghezza dello spettro a idmax distanza dalla diagonale
! Lo spettro puo' essere compattato sommando assieme npack canali
! Cuts calcolabili in sequenza dando step,count

#include "cmat.inc"

	PARAMETER (MAXINMEM=1024)

	CHARACTER SPECFILE*60
	INTEGER   SPETTRO_p(-MAXRES:MAXRES-1)
	INTEGER   SPETTRO_t(-MAXRES:MAXRES-1)
	logical*1 write_part /.TRUE./
	logical*1 write_tot/.TRUE./


	MACHINE_POINTERSIZE inbuf


	logical*1 getmem,freemem

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

	if(ndim(nmat).ne.2 .OR. matmode(nmat).ne.1) then
	  i1dmmy = inp_msg('This command valid only for 2D symmetrized  Matrices')
	  return
	endif

	ires=res(1,nmat)
	istep=step(1,nmat)

	i1dmmy = INP_I2('Gate From, To (counting channels on main axis) ',iwl,iwh)
	iwl=abs(iwl)
	iwh=abs(iwh)
	if(iwl.gt.iwh) call swapl(iwl,iwh)
	iwl=max(iwl,0)
	iwh=min(iwh,ires-1)
	if(iwl.eq.0 .and. iwh.eq.0) iwh=ires-1

	i1dmmy = inp_i1('Max. distance from diagonal ( 0 if all )',idmax)
	idmax=min(abs(idmax),ires-1)

	i1dmmy = inp_i1('Number of channels to pack together',npack)
	npack=max(abs(npack),1)
	jres=ires/npack

	i1dmmy = INP_I2('For multiple cuts give Step, Count ',istp,ncicli)
	if(ncicli.lt.1) ncicli=1
	if(istp.le.0) istp=iwh-iwl+1

	isize=istep*istep
	maxm=min( MAXINMEM,2*mblr(1,nmat) )
	memsize=4*isize*maxm
	error=.not.getmem(memsize,inbuf)
	dowhile(error)
	  maxm=maxm/2
	  if(maxm.lt.1) return
	  memsize=4*isize*maxm
	  error=.not.getmem(memsize,inbuf)
	end do

	error=.true.

	if(ncicli.gt.1) then
	  i1dmmy = inp_ask('Write partial spectra',write_part)
	  if(.not.write_part) then
	    write_tot=.TRUE.
	  else
	    i1dmmy = inp_ask('Write total spectrum',write_tot)
	  endif
	else
	  write_part=.true.
	  write_tot=.false.
	endif

	ninmem=0
	DO II=-MAXRES,MAXRES-1
	  SPETTRO_t(II)=0
	end do

	jwl=iwl
	jwh=iwh
	do icicli=1,ncicli
	  DO II=-MAXRES,MAXRES-1
	    SPETTRO_p(II)=0
	  end do
	  do iww=jwl,jwh
	    call madd__do_diagn(nmat,iww,idmax,spettro_p(0),%val(inbuf),isize,maxm,ninmem)
	    if(error) goto 200
	  end do
	  if(npack.gt.1) then
	    nn=0
	    do ii=0,ires-1,npack
	      lval=0
	      do jj=0,npack-1
	        lval=lval+spettro_p(ii+jj)
	      end do
	      spettro_p(nn)=lval
	      nn=nn+1
	    end do
	    do ii=jres,ires-1
	      spettro_p(ii)=0
	    end do
	  endif
	  do ii=-ires,-1
	    SPETTRO_p(ii)=SPETTRO_p(-ii)
	  end do
	  if(write_part) then
	    write(specfile,'(i4.4,1H_,i4.4,''.DIAGN'')') jwl,jwh
	    lspecfile=lengthc(specfile)
	    CALL WRITEDATL(0,SPECFILE,SPETTRO_p(-jres),2*jres,4,KV)
	    lspecfile=lengthc(specfile)
	    WRITE(6,'(a3,70x,a)')GO_UP_ONE_LINE,'      '
	    call flush(6)
	    if(kv.gt.0) then
	      WRITE(6,'(a3,A,I1)')GO_UP_ONE_LINE,SPECFILE(1:lspecfile)//'|L:',
	1			(2*jres+1023)/1024
	      call flush(6)
	    else
	      write(6,*) 'Error writing '//SPECFILE(1:lspecfile),kv
	      goto 200
	    endif
	  else
	    WRITE(6,'(a3,70x,a)')GO_UP_ONE_LINE,'                                  '
#if defined( __GFORTRAN__ )
	    write(6,'(a3,3i8)')GO_UP_ONE_LINE, icicli,jwl,jwh
#else
	    write(6,'(a3,3i)')GO_UP_ONE_LINE, icicli,jwl,jwh
#endif
	    call flush(6)
	  endif
	  write(6,*)
	  if(write_tot) then
	    do ii=-ires,ires-1
	      SPETTRO_t(ii)=SPETTRO_t(ii)+SPETTRO_p(ii)
	    end do
	  endif
	  if(icicli.ne.ncicli) then
	    jwl=jwl+istp
	    jwh=jwh+istp
	    if(jwh.gt.ires) goto 100
	  endif
	end do

100	if(write_tot) then
	  write(specfile,'(i4.4,1H_,i4.4,1H.,i4.4)') iwl,iwh,istp
	  lspecfile=lengthc(specfile)
	  CALL WRITEDATL(1,SPECFILE,SPETTRO_t(-jres),2*jres,4,KV)
	  lspecfile=lengthc(specfile)
	  WRITE(6,'(a3,70x,a)')GO_UP_ONE_LINE,'                            '
	  if(kv.gt.0) then
	    WRITE(6,'(a3,A,I1)')GO_UP_ONE_LINE,SPECFILE(1:lspecfile)//'|L:',
	1			(2*jres+1023)/1024
	    call flush(6)
	  else
	    write(6,*) 'Error writing '//SPECFILE(1:lspecfile),kv
	    goto 200
	  endif
	endif

	error=.not.FREEMEM(memsize,inbuf)
	if(error) return

	error=.false.
	return

200	error=.not.FREEMEM(memsize,inbuf)
	error=.true.
	return
	
	end

	subroutine MADD_MDIAGN(nmat)

! Da una matrice 2D simmetrizzata
! Estrae uno spettro PERPENDICOLARE alla diagonale principale
! I limiti della zona da sommare iwl-iwh sono dati dal valore della
! proiezione sull'asse x del tratto di diagonale principale considerato.
! Lo spettro viene dato con offset pari dimensione ires della matrice
! Spettro(i=x-y+offset)=Mat(x,y)
! Il nome dello spettro e' iwl_iwh.diagn
! Si puo limitare la lunghezza dello spettro a idmax distanza dalla diagonale
! Lo spettro puo' essere compattato sommando assieme npack canali
! Cuts calcolati da un file di gates

#include "cmat.inc"

	PARAMETER (MAXINMEM=1024)

	CHARACTER gatefile*60
	CHARACTER SPECFILE*60
	INTEGER   SPETTRO_p(-MAXRES:MAXRES-1)
	INTEGER   SPETTRO_t(-MAXRES:MAXRES-1)
	logical*1 write_part /.TRUE./
	logical*1 write_tot/.TRUE./
	integer lunin/0/
*	byte lunin/0/


	MACHINE_POINTERSIZE inbuf

	logical*1 getmem,freemem

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

#if defined( __GFORTRAN__ )
	if(ndim(nmat).ne.2 .OR. matmode(nmat).ne.1) then
#else
	if(ndim(nmat).ne.2 .OR. .NOT.matmode(nmat)) then
#endif
	  i1dmmy = inp_msg('This command valid only for 2D symmetrized  Matrices')
	  return
	endif

	ires=res(1,nmat)
	istep=step(1,nmat)

	i1dmmy = inp_ch('Input File with gates',gatefile)
	if(LUNIN.le.0) call LIB$GET_LUN(LUNIN)
	if(LUNIN.le.0) then
	  i1dmmy = inp_msg('Could not get a LUN')
	  return
	endif

	call inp_showlun(lluin,lluout)

	call filetype(gatefile,'gates')
	open(unit=LUNIN,file=gatefile,status='old',err=200)

	i1dmmy = inp_i1('Max. distance from diagonal ( 0 if all )',idmax)
	idmax=min(abs(idmax),ires-1)

	i1dmmy = inp_i1('Number of channels to pack together',npack)
	npack=max(abs(npack),1)
	jres=ires/npack

	i1dmmy = inp_ask('Write partial spectra',write_part)
	if(.not.write_part) then
	  write_tot=.TRUE.
	else
	  i1dmmy = inp_ask('Write total spectrum',write_tot)
	endif

	call inp_setlun(LUNIN,0)		!  NO input prompts

	isize=istep*istep
	maxm=min( MAXINMEM,2*mblr(1,nmat) )
	memsize=4*isize*maxm
	error=.not.getmem(memsize,inbuf)
	dowhile(error)
	  maxm=maxm/2
	  if(maxm.lt.1) return
	  memsize=4*isize*maxm
	  error=.not.getmem(memsize,inbuf)
	end do

	error=.true.

	ninmem=0
	DO II=-MAXRES,MAXRES-1
	  SPETTRO_t(II)=0
	end do
	
	nm_lunin=lunin
	read(unit=nm_lunin,fmt=*,err=200,end=200) i1o,i2o
	read(unit=nm_lunin,fmt=*,err=200,end=200) i1n,i2n
	
10	jwl=(i1o+i1n)/2
	jwh=(i2o+i2n+1)/2
	DO II=-MAXRES,MAXRES-1
	  SPETTRO_p(II)=0
	end do
	do iww=jwl,jwh
	    call madd__do_diagn(nmat,iww,idmax,spettro_p(0),%val(inbuf),isize,maxm,ninmem)
	    if(error) goto 200
	end do
	if(npack.gt.1) then
	  nn=0
	  do ii=0,ires-1,npack
	    lval=0
	    do jj=0,npack-1
	      lval=lval+spettro_p(ii+jj)
	    end do
	    spettro_p(nn)=lval
	    nn=nn+1
	  end do
	  do ii=jres,ires-1
	    spettro_p(ii)=0
	  end do
	endif
	do ii=-ires,-1
	    SPETTRO_p(ii)=SPETTRO_p(-ii)
	end do
	if(write_part) then
	  write(specfile,'(i4.4,1H_,i4.4,''.DIAGN'')') jwl,jwh
	  lspecfile=lengthc(specfile)
	  CALL WRITEDATL(0,SPECFILE,SPETTRO_p(-jres),2*jres,4,KV)
	  lspecfile=lengthc(specfile)
	  WRITE(6,'(a3,70x,a)')GO_UP_ONE_LINE,'                                 '
	  if(kv.gt.0) then
	    WRITE(6,'(a3,A,I1)')GO_UP_ONE_LINE,SPECFILE(1:lspecfile)//'|L:',(2*jres+1023)/1024
	    call flush(6)
	  else
	    write(6,*) 'Error writing '//SPECFILE(1:lspecfile),kv
	    goto 200
	  endif
	else
	  WRITE(6,'(a3,70x,a)')GO_UP_ONE_LINE,'      '
#if defined( __GFORTRAN__ )
	  write(6,'(a3,2i8)')GO_UP_ONE_LINE, jwl,jwh
#else
	  write(6,'(a3,2i)')GO_UP_ONE_LINE, jwl,jwh
#endif
	  call flush(6)
	endif
	write(6,*)
	if(write_tot) then
	  do ii=-ires,ires-1
	    SPETTRO_t(ii)=SPETTRO_t(ii)+SPETTRO_p(ii)
	  end do
	endif
	i1o=i1n
	i2o=i2n
	nm_lunin=lunin
	read(nm_lunin,*,err=100,end=100) i1n,i2n
	goto 10

100	call inp_setlun(lluin,lluout)	
	close(LUNIN)
	if(write_tot) then
	  specfile=gatefile
	  lspecfile=index(specfile,']')
	  if(lspecfile.gt.0) specfile=specfile(lspecfile+1:)	  
	  lspecfile=index(specfile,'.')
	  if(lspecfile.gt.0) specfile=specfile(1:lspecfile-1)	  
	  lspecfile=lengthc(specfile)
	  specfile=specfile(1:lspecfile)//'.diagn'
	  lspecfile=lengthc(specfile)
	  CALL WRITEDATL(1,SPECFILE,SPETTRO_t(-jres),2*jres,4,KV)
	  lspecfile=lengthc(specfile)
	  WRITE(6,'(a3,70x,a)')GO_UP_ONE_LINE,'      '
	  if(kv.gt.0) then
	    WRITE(6,'(a3,A,I1)')GO_UP_ONE_LINE,SPECFILE(1:lspecfile)//'|L:',(2*jres+1023)/1024
	    call flush(6)
	  else
	    write(6,*) 'Error writing '//SPECFILE(1:lspecfile),kv
	    goto 200
	  endif
	endif

	error=.not.FREEMEM(memsize,inbuf)
	if(error) return

	error=.false.
	return

200	error=.not.FREEMEM(memsize,inbuf)
	error=.true.
	call inp_setlun(lluin,lluout)	
	return
	
	end

	subroutine MADD__DO_DIAGN(nmat,ind,idmax,spec,inbuf,isize,maxm,ninmem)

C	indici contati da zero

#include "cmat.inc"

	INTEGER   spec(0:MAXRES)
	integer inbuf(0:isize-1,maxm)

	PARAMETER (MAXINMEM=1024)
	integer inmem(MAXINMEM)

	logical*1 cmt_readsegment,cmt_writesegment,cmt_getproje,cmt_putproje

	ires=res(1,nmat)
	istep=step(1,nmat)
	maxid=idmax
	if(maxid.le.1) maxid=ires

	lastblo=-1
	do id=0,maxid
	    id1=ind-(id+1)/2
	    id2=ind+(id  )/2
	    if(id1.lt.0) goto 50
	    if(id2.ge.ires) goto 50
	    kn1 =     id1/istep
	    ki1 = mod(id1,istep)
	    kn2 =     id2/istep
	    ki2 = mod(id2,istep)
	    nblo = kn1 + (kn2*(kn2+1))/2
	    ivalblo = ki1 + istep * ki2
	    if(nblo.ne.lastblo) then
	      do nblin=1,ninmem
		if(nblo.eq.inmem(nblin)) goto 20
	      end do
	      if(ninmem.lt.maxm) then
		ninmem=ninmem+1
		iinmem=ninmem
	      else
		iinmem=iinmem+1
		if(iinmem.gt.maxm) iinmem=1
	      endif
	      if(.not.cmt_readsegment(%val(cmt(nmat)),nblo,inbuf(0,iinmem),ndrec)) return
	      write(6,'(a3,6i8)')GO_UP_ONE_LINE, nblo,id1,id2,id,ind,ndrec
	      call flush(6)
	      if(ndrec.lt.0) return
	      nblin=iinmem
	      inmem(iinmem)=nblo
	    endif
20	    ival=inbuf(ivalblo,nblin)
	    if(id.eq.0) ival=2*ival
	    spec(id)=spec(id)+ival
	    lastblo=nblo
	end do

50	error=.false.
	return

	end

	subroutine MADD_DIAGP(nmat)

! Da una matrice 2D simmetrizzata
! Estrae uno spettro PARALLELO alla diagonale principale
! I limiti della zona da sommare idmin-idmax sono dati contando la distanza
! dalla diagonale principale |x-y|
! Spettro(i=x+y)=Mat(x,y)
! Il nome dello spettro e' idmin_idmax.diagp

#include "cmat.inc"

	PARAMETER (MAXINMEM=1024)

	CHARACTER SPECFILE*60
	INTEGER   SPETTRO_p(0:2*MAXRES-1)
	INTEGER   SPETTRO_t(0:2*MAXRES-1)
	logical*1 write_part/.TRUE./
	logical*1 write_tot/.TRUE./

	MACHINE_POINTERSIZE inbuf

	logical*1 getmem,freemem

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

	if(ndim(nmat).ne.2 .OR. matmode(nmat).ne.1) then
	  i1dmmy = inp_msg('This command valid only for 2D symmetrized  Matrices')
	  return
	endif

	ires=res(1,nmat)
	istep=step(1,nmat)

	i1dmmy = INP_I2('Distance from diagonal From, To',idl,idh)
	idl=(abs(idl)/2)*2
	idh=(abs(idh)/2)*2
	if(idl.gt.idh) call swapl(idl,idh)
	idl=max(idl,0)
	idh=min(idh,ires-1)
	if(idl.eq.0 .and.idh.eq.0) idh=ires-1

	i1dmmy = inp_i2('Cut limits ',iwl,iwh)
	iwl=(abs(iwl)/2)*2
	iwh=(abs(iwh)/2)*2
	if(iwl.gt.iwh) call swapl(iwl,iwh)
	iwh=min(iwh,2*ires-1)
	if(iwh.le.iwl) iwh=2*ires-1

	i1dmmy = inp_i1('Number of channels to pack together',npack)
	npack=max(npack,1)
	jres=(2*ires)/npack

	i1dmmy = INP_I2('For multiple cuts give Step, Count ',istp,ncicli)
	istp=(istp/2)*2
	if(ncicli.lt.1) ncicli=1
	if(istp.le.0) istp=iwh-iwl+1

	isize=istep*istep
	maxm=min( MAXINMEM,2*mblr(1,nmat) )
	memsize=4*isize*maxm
	error=.not.getmem(memsize,inbuf)
	dowhile(error)
	  maxm=maxm/2
	  if(maxm.lt.1) return
	  memsize=4*isize*maxm
	  error=.not.getmem(memsize,inbuf)
	end do

	error=.true.

	if(ncicli.gt.1) then
	  i1dmmy = inp_ask('Write partial spectra',write_part)
	  if(.not.write_part) then
	    write_tot=.TRUE.
	  else
	    i1dmmy = inp_ask('Write total spectrum',write_tot)
	  endif
	else
	  write_part=.true.
	  write_tot=.false.
	endif

	ninmem=0
	DO II=0,2*MAXRES-1
	  SPETTRO_t(II)=0
	end do

	jdl=idl
	jdh=idh
	do icicli=1,ncicli
	  DO II=0,2*MAXRES-1
	    SPETTRO_p(II)=0
	  end do
	  do idd=jdl,jdh
	    call madd__do_diagp(nmat,idd,iwl,iwh,spettro_p(0),%val(inbuf),isize,maxm,ninmem)
	    if(error) goto 200
	  end do
	  if(npack.gt.1) then
	    nn=0
	    do ii=0,2*ires-1,npack
	      lval=0
	      do jj=0,npack-1
	        lval=lval+spettro_p(ii+jj)
	      end do
	      spettro_p(nn)=lval
	      nn=nn+1
	    end do
	    do ii=jres,2*ires-1
	      spettro_p(ii)=0
	    end do
	  endif
	  if(write_part) then
	    write(specfile,'(i4.4,1H_,i4.4,''.DIAGP'')') jdl,jdh
	    lspecfile=lengthc(specfile)
	    CALL WRITEDATL(0,SPECFILE,SPETTRO_p,jres,4,KV)
	    lspecfile=lengthc(specfile)
	    WRITE(6,'(a3,70x,a)')GO_UP_ONE_LINE,'      '
	    if(kv.gt.0) then
	      WRITE(6,'(a3,A,I1)')GO_UP_ONE_LINE,SPECFILE(1:lspecfile)//'|L:',(jres+1023)/1024
	    else
	      write(6,*) 'Error writing '//SPECFILE(1:lspecfile),kv
	      goto 200
	    endif
	  else
	    WRITE(6,'(a3,70x,a)')GO_UP_ONE_LINE,'      '
#if defined( __GFORTRAN__ )
	    write(6,'(a3,i16,2i8)')GO_UP_ONE_LINE, icicli,jdl,jdh
#else
	    write(6,'(a3,3i)')GO_UP_ONE_LINE, icicli,jdl,jdh
#endif
	  endif
	  call flush(6)
	  if(write_tot) then
	    do ii=0,2*ires-1
	      SPETTRO_t(ii)=SPETTRO_t(ii)+SPETTRO_p(ii)
	    end do
	  endif
	  if(icicli.ne.ncicli) then
	    jdl=jdl+istp
	    jdh=jdh+istp
	    if(jdh.gt.ires) goto 100
	  endif
	end do

100	if(write_tot) then
	  write(specfile,'(i4.4,1H_,i4.4,1H.,i4.4)') idl,idh,istp
	  lspecfile=lengthc(specfile)
	  CALL WRITEDATL(1,SPECFILE,SPETTRO_t,jres,4,KV)
	  lspecfile=lengthc(specfile)
	  WRITE(6,'(a3,70x,a)')GO_UP_ONE_LINE,'      '
	  if(kv.gt.0) then
	    WRITE(6,'(a3,A,I1)')GO_UP_ONE_LINE,SPECFILE(1:lspecfile)//'|L:',(jres+1023)/1024
	  else
	    write(6,*) 'Error writing '//SPECFILE(1:lspecfile),kv
	    goto 200
	  endif
	endif
	call flush(6)

	error=.not.FREEMEM(memsize,inbuf)
	if(error) return

	error=.false.
	return

200	error=.not.FREEMEM(memsize,inbuf)
	error=.true.
	return
	
	end

	subroutine MADD__DO_DIAGP(nmat,ind,mind,maxd,spec,inbuf,isize,maxm,ninmem)

C	indici contati da zero

#include "cmat.inc"

	INTEGER   spec(0:2*MAXRES-1)
	integer inbuf(0:isize-1,maxm)

	PARAMETER (MAXINMEM=1024)
	integer inmem(MAXINMEM)

	logical*1 cmt_readsegment

	ires=res(1,nmat)
	istep=step(1,nmat)

	lastblo=-1
	do id=0,2*ires-1
	    id2=ind+(id+1)/2
	    id1=    (id  )/2
	    if(id2.ge.ires) goto 50
	    if(id1.ge.ires) goto 50
	    iad=id1+id2
	    if(iad.lt.mind) goto 30
	    if(iad.gt.maxd) goto 50
	    kn1 =     id1/istep
	    ki1 = mod(id1,istep)
	    kn2 =     id2/istep
	    ki2 = mod(id2,istep)
	    nblo = kn1 + (kn2*(kn2+1))/2
	    ivalblo = ki1 + istep * ki2
	    if(nblo.ne.lastblo) then
	      do nblin=1,ninmem
		if(nblo.eq.inmem(nblin)) goto 20
	      end do
	      if(ninmem.lt.maxm) then
		ninmem=ninmem+1
		iinmem=ninmem
	      else
		iinmem=iinmem+1
		if(iinmem.gt.maxm) iinmem=1
	      endif
	      if(.not.cmt_readsegment(%val(cmt(nmat)),nblo,inbuf(0,iinmem),ndrec)) return
	      write(6,'(a3,6i8)')GO_UP_ONE_LINE, nblo,id1,id2,id,ind,ndrec
	      call flush(6)
	      if(ndrec.lt.0) return
	      nblin=iinmem
	      inmem(iinmem)=nblo
	    endif
20	    ival=inbuf(ivalblo,nblin)
	    if(ind.eq.0) ival=2*ival
	    spec(iad)=spec(iad)+ival
	    lastblo=nblo
30	end do

50	error=.false.
	return

	end

	subroutine MADD_MOMENTS(nmat)

! estrae i primi momenti (area,pos,sigma) degli spettri lungo un asse
! di una matrice 2D e li salva sotto forma di spettri

#include "cmat.inc"

	DIMENSION AREA(0:MAXRES-1),POSY(0:MAXRES-1),FWHM(0:MAXRES-1)
	INTEGER   SPEC (0:MAXRES-1)
	REAL	  RSPEC(0:MAXRES-1)
	DIMENSION PM(0:5)

	INTEGER gvec(4)
	INTEGER gside /2/
	INTEGER cside /1/
	integer cres,gres
	integer ich1 /1/
	integer ich2 /MAXRES/

	logical*1 cmt_readspec

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif

	if(ndim(nmat).ne.2) then
	  i1dmmy = inp_msg('This command valid only for 2D Matrices')
	  return
	endif

	i1dmmy = inp_i1('Axis to analyze in terms of moments',cside)
	if(cside.eq.1) then
	  gside=2
	elseif(cside.eq.2) then
	  gside=1
	else
	  gside=2
	  cside=1
	endif
	cres=res(cside,nmat)
	gres=res(gside,nmat)
	ich1=max(ich1,0)
	ich2=min(ich2,cres-1)
	i1dmmy = inp_i2('Region to analyze',ich1,ich2)
	ncan=ich2-ich1+1
	i1dmmy = inp_i1('Number of channels (inside this region) for background',nfon)
	nfon=min(nfon,ncan/2)

	do ii=0,maxres-1
	  area(ii)=0
	  posy(ii)=0
	  fwhm(ii)=0
	end do

	call cmt_readspec_reset		! reset internal buffers

	gvec(cside)=-1
	do ind=0,gres-1
	  gvec(gside)=ind
	  if(.not.cmt_readspec(%val(cmt(nmat)),gvec,spec)) return
	  IF(NFON.GT.0) THEN
	    FONDO=0
	    DO II=ICH1,ICH1+NFON-1
	      FONDO=FONDO+SPEC(II)
	    end do
	    DO II=ICH2-NFON+1,ICH2
	      FONDO=FONDO+SPEC(II)
	    end do
	    FONDO=FONDO/(2*NFON)
	    DO II=ich1,ich2
	      RSPEC(II)=MAX(0.,SPEC(II)-FONDO)
	    end do
	  ELSE
	    DO II=ICH1,ICH2
	      RSPEC(II)=SPEC(II)
	    end do
	  ENDIF
	  call momentr(rspec(ich1),ncan,pm,3)
	  P0=PM(0)
	  IF(P0.GT.0) THEN
	    P1=PM(1)+ich1
	    VAR=PM(2)
	    IF(VAR.LT.0) VAR=0
	    XFWHM=SQRT(VAR)*2.355
	    AREA(ind)=P0
	    POSY(ind)=P1
	    FWHM(ind)=XFWHM
	  ELSE
	    P1=0.
	    XFWHM=0.
	  ENDIF
	  WRITE(6,'(a3,i8,3f12.1)')GO_UP_ONE_LINE, IND,P0,P1,XFWHM
	  call flush(6)
	end do

	CALL WRITESPEC('AREA.DAT',AREA,'R',gres,KV)
	IF(KV.GE.0) WRITE(6,*) 'AREA.DAT|R',gres,'   channels'
	CALL WRITESPEC('POSI.DAT',POSY,'R',gres,KV)
	IF(KV.GE.0) WRITE(6,*) 'POSI.DAT|R',gres,'   channels'
	CALL WRITESPEC('FWHM.DAT',FWHM,'R',gres,KV)
	IF(KV.GE.0) WRITE(6,*) 'FWHM.DAT|R',gres,'   channels'

	error=.false.
	return

	END






	subroutine MADD_AUTO_BANANA(NMAT)

!	SCATTER PLOT display di una matrice non simmetrizzata 
!			(estratta da un (iper)cubo eventualmente)

#include "cmat.inc"

	MACHINE_POINTERSIZE matxy		! puntatore alla matrice in memoria virtuale

	integer wl1,wh1,wl2,wh2

	integer pack(4)	/1,1,1,1/
	integer*4 pstep(4),pres(4),knn(4),offs(4)
	integer isize,psize

	logical*1 cmt_readsegment,cmt_writesegment,cmt_getproje,cmt_putproje

	PARAMETER (MAXBPOINTS=2*MAXRES)
	INTEGER*4 BANANA(2,MAXBPOINTS,2)
	INTEGER*4 NBPOINTS(2)	/0,0/
	logical*1   MODIFIED(2)
	character*60 banfile(2)
	integer lunban	/0/
	character*60 line1(2)
	character*60 line2(2)
	character*4 chh	/'1234'/
	integer indx	/1/
	integer indy	/2/
	integer istart, istop

	logical*1 getmem,freemem

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif
	if(ndim(nmat).lt.2 .or. ndim(nmat).gt.4) return
	if(matmode(nmat).gt.0)then
	  i1dmmy = inp_msg('Not inplemented for symmetrized matrices')
	  return
	endif

	isize=1
	do nn=1,ndim(nmat)
	  if(res(nn,nmat).gt.MAXRES) stop 'Dimension too big'
	  isize=isize*step(nn,nmat)
	end do
	if(isize.gt.MAXSEGSIZE) then
	  i1dmmy = inp_msg('Buffer size too large')
	  return
	endif

10	if(ndim(nmat).ge.3) then
	  i1dmmy = inp_i2('The 2 axes you want to process',indx,indy)
	  if(indx.lt.1 .or. indx.gt.ndim(nmat)) return
	  if(indy.lt.1 .or. indy.gt.ndim(nmat)) return
	  if(indx.eq.indy) return
          indg1=1
          if(indg1.eq.indx .OR. indg1.eq.indy ) indg1=indg1+1
          if(indg1.eq.indx .OR. indg1.eq.indy ) indg1=indg1+1
          indg2=1
          if(indg2.eq.indx .OR. indg2.eq.indy .OR. indg2.eq.indg1) indg2=indg2+1
          if(indg2.eq.indx .OR. indg2.eq.indy .OR. indg2.eq.indg1) indg2=indg2+1
          if(indg2.eq.indx .OR. indg2.eq.indy .OR. indg2.eq.indg1) indg2=indg2+1
	else
	  indx=1
	  indy=2
          indg1=3
          indg2=4
	endif
	i1dmmy = inp_i2('# of channels to pack together X, Y',pack(indx),pack(indy))
	pack(indg1)=1
	pack(indg2)=1
	pack(indx)=max(1,min(pack(indx),res(indx,nmat)))
	pack(indy)=max(1,min(pack(indy),res(indy,nmat)))
	if( (mod(step(indx,nmat),pack(indx)).ne.0) .or.
	1   (mod(step(indy,nmat),pack(indy)).ne.0) ) then
	  write(6,*) 'Packing only by integer fraction of step'
	  goto 10
	endif
	if( istop .eq. 0 ) istop = res(indy,nmat)/pack(indy)-1
	i1dmmy = inp_i2('Limits of the analysis region on axis '//chh(indy:indy),istart,istop)
	if(ndim(nmat).ge.3) then
	  i1dmmy = inp_i2('Gate on side '//chh(indg1:indg1),wl1,wh1)
	  if(wl1.lt.0 .or. wl1.ge.res(indg1,nmat))return
	  if(wh1.lt.0 .or. wh1.ge.res(indg1,nmat))return
	  if(wl1.gt.wh1) call swapl(wl1,wh1)
	endif
	if(ndim(nmat).ge.4) then
	  i1dmmy = inp_i2('Gate on side '//chh(indg2:indg2),wl2,wh2)
	  if(wl2.lt.0 .or. wl2.ge.res(indg2,nmat))return
	  if(wh2.lt.0 .or. wh2.ge.res(indg2,nmat))return
	  if(wl2.gt.wh2) call swapl(wl2,wh2)
	endif


	pstep(indx)=step(indx,nmat)/pack(indx)
	pres(indx)=res(indx,nmat)/pack(indx)
	pstep(indy)=step(indy,nmat)/pack(indy)
	pres(indy)=res(indy,nmat)/pack(indy)
	matsize=pres(indx)*pres(indy)
	psize=pstep(indx)*pstep(indy)

	error=.not.getmem(4*matsize,matxy)
	if(error) return
	error=.true.
	call lvect_erase(%val(matxy),matsize)

	write(6,*) 'Reading matrix ...'
	write(6,*)
	if(ndim(nmat).eq.2) then
	  do ibl2=0,mblr(2,nmat)-1
	    knn(2)=ibl2
	    offs(2)=ibl2*step(2,nmat)
	    do ibl1=0,mblr(1,nmat)-1
	      knn(1)=ibl1
	      offs(1)=ibl1*step(1,nmat)
	      nblo=iad_ndim0(knn,mblr(1,nmat),2)
	      if(.not.cmt_readsegment(%val(cmt(nmat)),nblo,bufl(0,nmat),ndrec)) return
	      if(ndrec.ne.0) then
	        write(6,'(a3,3i8)')GO_UP_ONE_LINE, ibl1,ibl2,nblo
		call flush(6)
	        call madd__scat_2d(%val(matxy),pres(indx),pres(indy),pack(indx),pack(indy),
	1	                bufl(0,nmat),step(1,nmat),step(2,nmat),offs(1),offs(2))
	      endif
	    end do
	  end do
	elseif(ndim(nmat).eq.3) then
	  do iblx=0,mblr(indx,nmat)-1
	    knn(indx)=iblx
	    offs(indx)=iblx*step(indx,nmat)
	    do ibly=0,mblr(indy,nmat)-1
	      knn(indy)=ibly
	      offs(indy)=ibly*step(indy,nmat)
	      do iblg1=wl1/step(indg1,nmat),wh1/step(indg1,nmat)
	        knn(indg1)=iblg1
	        offs(indg1)=iblg1*step(indg1,nmat)
	        nblo=iad_ndim0(knn,mblr(1,nmat),3)
	        if(.not.cmt_readsegment(%val(cmt(nmat)),nblo,bufl(0,nmat),ndrec)) return
	        if(ndrec.ne.0) then
	          write(6,'(a3,4i8)')GO_UP_ONE_LINE, (knn(ii),ii=1,3),nblo
	          call flush(6)
		  call madd__scat_3d(%val(matxy),pres(indx),pres(indy),
	1					 pack(indx),pack(indy),
	1	 bufl(0,nmat),step(1,nmat),step(2,nmat),step(3,nmat),
	1	 offs(1),offs(2),offs(3),indx,indy,indg1,wl1,wh1)
	        endif
	      end do
	    end do
	  end do
	elseif(ndim(nmat).eq.4) then
	  do iblx=0,mblr(indx,nmat)-1
	    knn(indx)=iblx
	    offs(indx)=iblx*step(indx,nmat)
	    do ibly=0,mblr(indy,nmat)-1
	      knn(indy)=ibly
	      offs(indy)=ibly*step(indy,nmat)
	      do iblg1=wl1/step(indg1,nmat),wh1/step(indg1,nmat)
	        knn(indg1)=iblg1
	        offs(indg1)=iblg1*step(indg1,nmat)
	        do iblg2=wl2/step(indg2,nmat),wh2/step(indg2,nmat)
	          knn(indg2)=iblg2
	          offs(indg2)=iblg2*step(indg2,nmat)
	          nblo=iad_ndim0(knn,mblr(1,nmat),4)
	          if(.not.cmt_readsegment(%val(cmt(nmat)),nblo,bufl(0,nmat),ndrec)) return
	          if(ndrec.ne.0) then
	            write(6,'(a3,5i8)')GO_UP_ONE_LINE, (knn(ii),ii=1,4),nblo
	            call flush(6)
		    call madd__scat_4d(%val(matxy),pres(indx),pres(indy),
	1					   pack(indx),pack(indy),
	1	   bufl(0,nmat),step(1,nmat),step(2,nmat),step(3,nmat),step(4,nmat),
	1	   offs(1),offs(2),offs(3),offs(4),indx,indy,indg1,wl1,wh1,
	1                                                    indg2,wl2,wh2)
	          endif
	        end do
	      end do
	    end do
	  end do
	endif

	call autobanana(%val(matxy),pres(indx),pres(indy),istart,istop)

	error=.not.FREEMEM(4*matsize,matxy)

	return

	END







        subroutine MADD_AUTO_BANANA_old(nmat)

#include "cmat.inc"

        CHARACTER FILE1*40
        CHARACTER FILE2*40

	integer lspec(0:MAXRES-1)
	integer tmin(0:MAXRES-1),tmax(0:MAXRES-1)

	integer   gvec(2)
	integer   nsum, tmax_abs, tmin_abs

	real      frac_low  /10./
	real      frac_high /10./

        logical*1 different

	logical*1 cmt_readspec,cmt_getproje

        if(.not.opened(nmat)) then
          i1dmmy = inp_msg('Matrix is not opened')
          return
        endif

        if(ndim(nmat).ne.2) then
          i1dmmy = inp_msg('This command valid only for 2D Matrices')
          return
        endif

        do ii=1,ndim(nmat)
          if(res(ii,nmat).GT.MAXRES) then
            write(6,*) 'Matrix dimension is too big'
            return
          endif
        end do

	tmin_abs = res(2,nmat) *0.1
	tmax_abs = res(2,nmat) *0.9
	
	i1dmmy = inp_i2('Region to search on 2nd axis  min, max  ',tmin_abs,tmax_abs)
	i1dmmy = inp_r2('Fraction of intensity (%)     low, high ',frac_low,frac_high)

60      do ii=0,MAXRES-1
          lspec(ii)=0
	  tmin(ii)=0
	  tmax(ii)=0
        end do

        WRITE(6,*)
        call cmt_readspec_reset			! reset internal buffers

        gvec(2)=-1
        do ind1=0,res(1,nmat)-1
          gvec(1)=ind1
          if(.not.cmt_readspec(%val(cmt(nmat)),gvec,lspec)) then
            write(6,*) 'cmt_readspec returned an error condition at',gvec(1)
            return
          endif

	  isum=0
          do ind2=tmin_abs, tmax_abs
            isum=isum + lspec(ind2)
          end do

	  iy=tmin_abs
	  nsum=lspec(iy)
	  ifrac=isum *(frac_high/100.)
	  do while ((nsum.lt.ifrac) .and. (iy.lt.tmax_abs))
	    iy=iy+1
	    nsum=nsum+lspec(iy)
  	  end do
	  tmin(ind1)=iy	

	  iy=tmax_abs
	  nsum=lspec(iy)
	  ifrac=isum *(frac_low/100.)
	  do while ((nsum.lt.ifrac) .and. (iy.gt.tmin_abs))
	    iy=iy-1
	    nsum=nsum+lspec(iy)
  	  end do
	  tmax(ind1)=iy	

	  write(6,'(a3,3i8)')GO_UP_ONE_LINE, ind1,tmin(ind1),tmax(ind1)
	  call flush(6)
	end do

	write(6,*) 'Spectrum of the lower edge'
	CALL WRITEDATL(1,file1,tmin,res(1,nmat),4,KV)
	if(kv .LE. 0) write(6,*) 'error writing'//file1

	write(6,*) 'Spectrum of the upper edge'
	CALL WRITEDATL(1,file2,tmax,res(1,nmat),4,KV)
	if(kv .LE. 0) write(6,*) 'error writing'//file1
        
	error=.false.
	return

	end


	subroutine MADD_SCATTERPLOT2D(NMAT)

!	SCATTER PLOT display di una matrice non simmetrizzata 
!			(estratta da un (iper)cubo eventualmente)

#include "cmat.inc"

	MACHINE_POINTERSIZE matxy		! puntatore alla matrice in memoria virtuale

	integer wl1,wh1,wl2,wh2

	integer pack(4)	/1,1,1,1/
	integer*4 pstep(4),pres(4),knn(4),offs(4)
	integer isize,psize

	logical*1 cmt_readsegment,cmt_writesegment,cmt_getproje,cmt_putproje

	PARAMETER (MAXBPOINTS=2*MAXRES)
	INTEGER*4 BANANA(2,MAXBPOINTS,2)
	INTEGER*4 NBPOINTS(2)	/0,0/
	logical*1   MODIFIED(2)
	character*60 banfile(2)
	integer lunban	/0/
	character*60 line1(2)
	character*60 line2(2)
	character*4 chh	/'1234'/
	integer indx	/1/
	integer indy	/2/

	logical*1 getmem,freemem

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif
	if(ndim(nmat).lt.2 .or. ndim(nmat).gt.4) return
	if(matmode(nmat).gt.0)then
	  i1dmmy = inp_msg('Not inplemented for symmetrized matrices')
	  return
	endif

	isize=1
	do nn=1,ndim(nmat)
	  if(res(nn,nmat).gt.MAXRES) stop 'Dimension too big'
	  isize=isize*step(nn,nmat)
	end do
	if(isize.gt.MAXSEGSIZE) then
	  i1dmmy = inp_msg('Buffer size too large')
	  return
	endif

10	if(ndim(nmat).ge.3) then
	  i1dmmy = inp_i2('The 2 axes you want to present',indx,indy)
	  if(indx.lt.1 .or. indx.gt.ndim(nmat)) return
	  if(indy.lt.1 .or. indy.gt.ndim(nmat)) return
	  if(indx.eq.indy) return
          indg1=1
          if(indg1.eq.indx .OR. indg1.eq.indy ) indg1=indg1+1
          if(indg1.eq.indx .OR. indg1.eq.indy ) indg1=indg1+1
          indg2=1
          if(indg2.eq.indx .OR. indg2.eq.indy .OR. indg2.eq.indg1) indg2=indg2+1
          if(indg2.eq.indx .OR. indg2.eq.indy .OR. indg2.eq.indg1) indg2=indg2+1
          if(indg2.eq.indx .OR. indg2.eq.indy .OR. indg2.eq.indg1) indg2=indg2+1
	else
	  indx=1
	  indy=2
          indg1=3
          indg2=4
	endif
	i1dmmy = inp_i2('# of channels to pack together X, Y',pack(indx),pack(indy))
	pack(indg1)=1
	pack(indg2)=1
	pack(indx)=max(1,min(pack(indx),res(indx,nmat)))
	pack(indy)=max(1,min(pack(indy),res(indy,nmat)))
	if( (mod(step(indx,nmat),pack(indx)).ne.0) .or.
	1   (mod(step(indy,nmat),pack(indy)).ne.0) ) then
	  write(6,*) 'Packing only by integer fraction of step'
	  goto 10
	endif
	if(ndim(nmat).ge.3) then
	  i1dmmy = inp_i2('Gate on side '//chh(indg1:indg1),wl1,wh1)
	  if(wl1.lt.0 .or. wl1.ge.res(indg1,nmat))return
	  if(wh1.lt.0 .or. wh1.ge.res(indg1,nmat))return
	  if(wl1.gt.wh1) call swapl(wl1,wh1)
	endif
	if(ndim(nmat).ge.4) then
	  i1dmmy = inp_i2('Gate on side '//chh(indg2:indg2),wl2,wh2)
	  if(wl2.lt.0 .or. wl2.ge.res(indg2,nmat))return
	  if(wh2.lt.0 .or. wh2.ge.res(indg2,nmat))return
	  if(wl2.gt.wh2) call swapl(wl2,wh2)
	endif

	pstep(indx)=step(indx,nmat)/pack(indx)
	pres(indx)=res(indx,nmat)/pack(indx)
	pstep(indy)=step(indy,nmat)/pack(indy)
	pres(indy)=res(indy,nmat)/pack(indy)
	matsize=pres(indx)*pres(indy)
	psize=pstep(indx)*pstep(indy)

	error=.not.getmem(4*matsize,matxy)
	if(error) return
	error=.true.
	call lvect_erase(%val(matxy),matsize)

	write(6,*) 'Reading matrix ...'
	write(6,*)
	if(ndim(nmat).eq.2) then
	  do ibl2=0,mblr(2,nmat)-1
	    knn(2)=ibl2
	    offs(2)=ibl2*step(2,nmat)
	    do ibl1=0,mblr(1,nmat)-1
	      knn(1)=ibl1
	      offs(1)=ibl1*step(1,nmat)
	      nblo=iad_ndim0(knn,mblr(1,nmat),2)
	      if(.not.cmt_readsegment(%val(cmt(nmat)),nblo,bufl(0,nmat),ndrec)) return
	      if(ndrec.ne.0) then
	        write(6,'(a3,3i8)')GO_UP_ONE_LINE, ibl1,ibl2,nblo
		call flush(6)
	        call madd__scat_2d(%val(matxy),pres(indx),pres(indy),pack(indx),pack(indy),
	1	                bufl(0,nmat),step(1,nmat),step(2,nmat),offs(1),offs(2))
	      endif
	    end do
	  end do
	elseif(ndim(nmat).eq.3) then
	  do iblx=0,mblr(indx,nmat)-1
	    knn(indx)=iblx
	    offs(indx)=iblx*step(indx,nmat)
	    do ibly=0,mblr(indy,nmat)-1
	      knn(indy)=ibly
	      offs(indy)=ibly*step(indy,nmat)
	      do iblg1=wl1/step(indg1,nmat),wh1/step(indg1,nmat)
	        knn(indg1)=iblg1
	        offs(indg1)=iblg1*step(indg1,nmat)
	        nblo=iad_ndim0(knn,mblr(1,nmat),3)
	        if(.not.cmt_readsegment(%val(cmt(nmat)),nblo,bufl(0,nmat),ndrec)) return
	        if(ndrec.ne.0) then
	          write(6,'(a3,4i8)')GO_UP_ONE_LINE, (knn(ii),ii=1,3),nblo
	          call flush(6)
		  call madd__scat_3d(%val(matxy),pres(indx),pres(indy),
	1					 pack(indx),pack(indy),
	1	 bufl(0,nmat),step(1,nmat),step(2,nmat),step(3,nmat),
	1	 offs(1),offs(2),offs(3),indx,indy,indg1,wl1,wh1)
	        endif
	      end do
	    end do
	  end do
	elseif(ndim(nmat).eq.4) then
	  do iblx=0,mblr(indx,nmat)-1
	    knn(indx)=iblx
	    offs(indx)=iblx*step(indx,nmat)
	    do ibly=0,mblr(indy,nmat)-1
	      knn(indy)=ibly
	      offs(indy)=ibly*step(indy,nmat)
	      do iblg1=wl1/step(indg1,nmat),wh1/step(indg1,nmat)
	        knn(indg1)=iblg1
	        offs(indg1)=iblg1*step(indg1,nmat)
	        do iblg2=wl2/step(indg2,nmat),wh2/step(indg2,nmat)
	          knn(indg2)=iblg2
	          offs(indg2)=iblg2*step(indg2,nmat)
	          nblo=iad_ndim0(knn,mblr(1,nmat),4)
	          if(.not.cmt_readsegment(%val(cmt(nmat)),nblo,bufl(0,nmat),ndrec)) return
	          if(ndrec.ne.0) then
	            write(6,'(a3,5i8)')GO_UP_ONE_LINE, (knn(ii),ii=1,4),nblo
	            call flush(6)
		    call madd__scat_4d(%val(matxy),pres(indx),pres(indy),
	1					   pack(indx),pack(indy),
	1	   bufl(0,nmat),step(1,nmat),step(2,nmat),step(3,nmat),step(4,nmat),
	1	   offs(1),offs(2),offs(3),offs(4),indx,indy,indg1,wl1,wh1,
	1                                                    indg2,wl2,wh2)
	          endif
	        end do
	      end do
	    end do
	  end do
	endif

	call contourplot(%val(matxy),pres(indx),pres(indy))

	error=.not.FREEMEM(4*matsize,matxy)

	return

	END

	subroutine madd__scat_2d(matxy,rx,ry,px,py,bufl,s1,s2,of1,of2)

	integer rx,ry,px,py,s1,s2,of1,of2
	integer*4 matxy(0:rx-1,0:ry-1)
	integer*4 bufl(of1:of1+s1-1,of2:of2+s2-1)

	do i2=of2,of2+s2-1
	  j2=i2/py
	  do i1=of1,of1+s1-1
	    j1=i1/px
	    matxy(j1,j2)=matxy(j1,j2)+bufl(i1,i2)
	  end do
	end do

	return

	end

	subroutine madd__scat_3d(matxy,rx,ry,px,py,bufl,s1,s2,s3,of1,of2,of3,
	1	indx,indy,indg,wl,wh)

	integer rx,ry,px,py,s1,s2,s3,of1,of2,of3,indx,indy,indg,wl,wh
	integer*4 matxy(0:rx-1,0:ry-1)
	integer*4 bufl(of1:of1+s1-1,of2:of2+s2-1,of3:of3+s3-1)
	integer off(3),step(3),pack(3),ib(3)

	off(1)=of1
	off(2)=of2
	off(3)=of3
	step(1)=s1
	step(2)=s2
	step(3)=s3
	pack(indx)=px
	pack(indy)=py
	pack(indg)=1
	
	do ig=off(indg),off(indg)+step(indg)-1
	  if(ig.ge.wl .and. ig.le.wh) then
	    ib(indg)=ig
	    do ix=off(indx),off(indx)+step(indx)-1
	      jx=ix/pack(indx)
	      ib(indx)=ix
	      do iy=off(indy),off(indy)+step(indy)-1
	        jy=iy/pack(indy)
	        ib(indy)=iy
	        matxy(jx,jy)=matxy(jx,jy)+bufl(ib(1),ib(2),ib(3))
	    end do
	  end do
	  endif
	end do

	return

	end

	subroutine madd__scat_4d(matxy,rx,ry,px,py,bufl,s1,s2,s3,s4,of1,of2,of3,of4,
	1	indx,indy,indg1,wl1,wh1,indg2,wl2,wh2)

	integer rx,ry,px,py,s1,s2,s3,s4,of1,of2,of3,of4,indx,indy
	integer indg1,wl1,wh1,indg2,wl2,wh2
	integer*4 matxy(0:rx-1,0:ry-1)
	integer*4 bufl(of1:of1+s1-1,of2:of2+s2-1,of3:of3+s3-1,of4:of4+s4-1)
	integer off(4),step(4),pack(4),ib(4)

	off(1)=of1
	off(2)=of2
	off(3)=of3
	off(4)=of4
	step(1)=s1
	step(2)=s2
	step(3)=s3
	step(4)=s4
	pack(indx)=px
	pack(indy)=py
	pack(indg1)=1
	pack(indg2)=1
	
	do ig1=off(indg1),off(indg1)+step(indg1)-1
	  if(ig1.ge.wl1 .and. ig1.le.wh1) then
	    ib(indg1)=ig1
	    do ig2=off(indg2),off(indg2)+step(indg2)-1
	      if(ig2.ge.wl2 .and. ig2.le.wh2) then
	        ib(indg2)=ig2
	        do ix=off(indx),off(indx)+step(indx)-1
	          jx=ix/pack(indx)
	          ib(indx)=ix
	          do iy=off(indy),off(indy)+step(indy)-1
	            jy=iy/pack(indy)
	            ib(indy)=iy
	            matxy(jx,jy)=matxy(jx,jy)+bufl(ib(1),ib(2),ib(3),ib(4))
	          end do
	        end do
	      endif
	    end do
	  endif
	end do

	return

	end

	subroutine scatterplot(mat,res1,res2,iax,iay,banana,nbpoints,modified)

!	SCATTER PLOT display di una matrice compressa 2D
!
!	display normale con
!	indice1 sull'asse delle X (orizzontale)
!	indice2 sull'asse delle Y (verticale)

	integer res1,res2
	INTEGER*4 MAT(0:res1-1,0:res2-1)

	BYTE BMAT(0:511,0:511)

	PARAMETER (MAXRES=8192)
	INTEGER PROX(0:MAXRES+512),PROY(0:MAXRES+512)
	integer ban(2,0:maxres-1),ixx,iyy,ix0,iy0

	PARAMETER (MAXBPOINTS=1024)
	INTEGER*4 BANANA(2,MAXBPOINTS,2)
	INTEGER*4 nbpoints(2)
	logical*1   modified(2)

	real    pX(2),pY(2)
	logical*1 pdefined(2) /.false.,.false./
	real    rX(4),rY(4)
	logical*1 rdefined(4) /.false.,.false.,.false.,.false./

	logical*1 transposed,dispersion
	logical*1 mapped,rotate,ov16

	character*4 labx,laby
	character*1 gch
	
	integer iseed /123456789/

	integer*2 tekcolor,icmap(0:7),ixcolor

	PARAMETER (IX0=300)
	PARAMETER (IX1=811)
	PARAMETER (IY0= 50)
	PARAMETER (IY1=561)
	PARAMETER (ISP=180)

	i1dmmy = inp_ch('Please set the terminal to graphic mode and type <RET>',gch)

	call tekanmode

	modified(1)=.false.
	modified(2)=.false.
	mapped=.false.
	rotate=.false.
	pdefined(1)=.false.
	pdefined(2)=.false.
	ov16=.false.
	ax=0.
	bx=1.
	ay=0.
	by=1.
	matmax=-66000
	ixcolor=-1
	icmap(0)=9
	icmap(1)=8
	icmap(2)=4
	icmap(3)=7
	icmap(4)=5
	icmap(5)=3
	icmap(6)=6
	icmap(7)=1
	isctyp=1
	
	do ii=0,res1-1
	  do jj=0,res2-1
	    if(mat(ii,jj) .gt. matmax)matmax=mat(ii,jj)
	  enddo
	enddo

	mres=max(res1,res2)
	transposed=.false.
	if(transposed) then
	  ix_1=0
	  ix_2=res2-1
	  iy_1=0
	  iy_2=res1-1
	else
	  ix_1=0
	  ix_2=res1-1
	  iy_1=0
	  iy_2=res2-1
	endif
	izoff=0

12	CALL INITT(960)
	call tekvecmode
	tekcolor=1
	call teksetcolor(tekcolor)
	call str_toupper(gch)
	nchx=ix_2 - ix_1 +1
	nchy=iy_2 - iy_1 +1
	SCAX=FLOAT(IX1-IX0+1)/nchx
	SCAY=FLOAT(IY1-IY0+1)/nchy
	dispersion=scax.gt.1 .or. scay.gt.1
	ipixel=max(1.,scax*scay+0.5)

	do ii=0,mres-1
	  prox(ii)=0
	  proy(ii)=0
	end do
	do i2=0,511
	do i1=0,511
	  bmat(i1,i2)=0
	end do
	end do

	if(transposed) then
	  write(labx,'(''('',i1,'')'')') iay
	  write(laby,'(''('',i1,'')'')') iax
	else
	  write(labx,'(''('',i1,'')'')') iax
	  write(laby,'(''('',i1,'')'')') iay
	endif

	if(rotate) then
	  labx=' '
	  laby=' '
	  rot11=cosd(angle)
	  rot22=rot11
	  rot12=-sind(angle)
	  rot21=-rot12
	endif

	CALL MOVABS(IX0 , IY0)
	CALL DRWABS(IX1 , IY0)
	CALL DRWABS(IX1 , IY1)
	CALL DRWABS(IX0 , IY1)
	CALL DRWABS(IX0 , IY0)

	call tkt__flush
	
	tekcolor=6
	call teksetcolor(tekcolor)

	CALL MOVABS(IX0+10,IY0-20)
	CALL ANMODE
CVMS	write(6,'(1h+,i4)') ix_1
	write(6,'(i4)') ix_1
	CALL VECMOD

	CALL MOVABS((IX0+IX1)/2-10,IY0-20)
	CALL ANMODE
CVMS	write(6,'(1h+,A)') LABX
	write(6,'(A)') LABX
	CALL VECMOD

	CALL MOVABS(IX1-60,IY0-20)
	CALL ANMODE
CVMS	write(6,'(1h+,i4)') ix_2
	write(6,'(i4)') ix_2
	CALL VECMOD

	CALL MOVABS(IX0-60,IY0+20)
	CALL ANMODE
CVMS	write(6,'(1h+,i4)') iy_1
	write(6,'(i4)') iy_1
	CALL VECMOD

	CALL MOVABS(IX0-50,(IY0+IY1)/2-10)
	CALL ANMODE
CVMS	write(6,'(1h+,A)') LABY
	write(6,'(A)') LABY
	CALL VECMOD

	CALL MOVABS(IX0-60,IY1-20)
	CALL ANMODE
CVMS	write(6,'(1h+,i4)') iy_2
	write(6,'(i4)') iy_2
	CALL VECMOD
	call tekvecmode
	tekcolor=1
	call teksetcolor(tekcolor)

	if(transposed) then
	  matval=mat(iy_1,ix_1)
	else
	  matval=mat(ix_1,iy_1)
	endif
	iz_1=matmax
	iz_2=matval
	itot=0
	matmax=0

	do ix=ix_1,ix_2
	do iy=iy_1,iy_2
	  if(transposed) then
	    matval=mat(iy,ix)
	  else
	    matval=mat(ix,iy)
	  endif
	  if(ov16)then
	   if(matval .lt. 0)then
	    do while (matval .lt. 32768)
	      matval=matval+65386
	    enddo
	   endif
	  endif
	  if(matval .gt. matmax)matmax=matval
	enddo
	enddo

	do ix=ix_1,ix_2
	do iy=iy_1,iy_2
	  if(transposed) then
	    matval=mat(iy,ix)
	  else
	    matval=mat(ix,iy)
	  endif
	  if(ov16)then
	   if(matval .lt. 0)then
	    do while (matval .lt. 32768)
	      matval=matval+65386
	    enddo
	   endif
	  endif
	  if(mapped) then
	    mix=ax+bx*ix
	    miy=ay+by*iy
	  elseif(rotate) then
	    mix=rot11*(ix-rx0) + rot12*(iy-ry0) + rx0
	    miy=rot21*(ix-rx0) + rot22*(iy-ry0) + ry0
	  else
	    mix=ix
	    miy=iy
	  endif
	  if(mix.ge.ix_1 .AND. mix.le.ix_2 .AND.
	1    miy.ge.iy_1 .AND. miy.le.iy_2 ) then
	    prox(mix+1)=prox(mix+1)+matval
	    proy(miy+1)=proy(miy+1)+matval
	    itot=itot+matval
	    iz_1=min(iz_1,matval)
	    iz_2=max(iz_2,matval)
	    if(matval .gt. izoff) then
	      if(isctyp .eq. 1)iiclr=float(matval-izoff)/(matmax-izoff+0.1)*8.000
	      if(isctyp .eq. 2)iiclr=log10(matval-izoff+1.0)/log10(matmax-izoff+2.0)*8.0
	      if(isctyp .eq. 3)iiclr=sqrt((matval-izoff+0.01)/(matmax-izoff+1.0))*8.0
	      if(ixcolor .ne. iiclr)then
	         ixcolor = iiclr
		 call tkt__flush
		 call teksetcolor(icmap(ixcolor))
	      endif
	      if(dispersion) then
	        npoints=min(ipixel,matval)
	        do ii=1,npoints
	          if(scax.le.1) then
	            ixx=(mix-ix_1)*scax
	          else
	            ixx=(mix-ix_1)*scax+ran(iseed)*scax
	          endif
	          if(scay.le.1) then
	            iyy=(miy-iy_1)*scay
	          else
	            iyy=(miy-iy_1+ran(iseed))*scay
	          endif
	          if(bmat(ixx,iyy).eq.0) then
	            CALL PNTABS(ixx+IX0,iyy+IY0)
	            bmat(ixx,iyy)=1
	          endif
	        end do
	      else
	        ixx=(mix-ix_1)*scax
	        iyy=(miy-iy_1)*scay
	        if(bmat(ixx,iyy).eq.0) then
	          CALL PNTABS(ixx+IX0,iyy+IY0)
	          bmat(ixx,iyy)=1
	        endif
	      endif
	    endif
	  endif
	end do
	end do
	call tkt__flush
	tekcolor=1
	ixcolor=-1
	call teksetcolor(tekcolor)

	CALL HOME
	CALL ANMODE
	CALL VECMOD
	CALL DRAW_LSPEC(PROX(ix_1),nchx,IX0,IX1,IY1,IY1+ISP,0,0.9)
	CALL DRAW_LSPEC(PROY(iy_1),nchy,IX1,IX1+ISP,IY0,IY1,1,0.9)
	CALL HOME
	call textmode
	write(6,'('' Total '',i10,''\n'')') itot
	write(6,'('' Minval'',i10,''\n'')') iz_1
	write(6,'('' Maxval'',i10,''\n'')') iz_2
	write(6,'('' Offset'',i10,''\n'')') izoff
	if(mapped) then
	    write(6,'('' X'',f10.3,f9.4,''\n'')') ax,bx
	    write(6,'('' Y'',f10.3,f9.4,''\n'')') ay,by
	endif
	call tekvecmode
	call HOME
	call tekanmode
	CALL ANMODE

	ix_l=ix_1
	ix_r=ix_2
	iy_u=iy_1
	iy_o=iy_2

	do nban=1,2
	    if(nbpoints(nban).gt.0) then
	      CALL VECMOD
	      tekcolor=2
	      call teksetcolor(tekcolor)
	      ixc=(banana(1,1,nban)-ix_1)*scax+ix0
	      iyc=(banana(2,1,nban)-iy_1)*scay+iy0
	      CALL MOVABS(ixc,iyc)
	      CALL DRWABS(ixc,iyc)
	      do ii=2,nbpoints(nban)
	        ixc=(banana(1,ii,nban)-ix_1)*scax+ix0
	        iyc=(banana(2,ii,nban)-iy_1)*scay+iy0
	        CALL DRWABS(ixc,iyc)
	      end do
	      CALL ANMODE
	    endif
	end do

	do ii=1,4
	    if(rdefined(ii)) then
	      CALL VECMOD
	      ixc=(rx(ii)-ix_1)*scax+ix0
	      iyc=(ry(ii)-iy_1)*scay+iy0
	      CALL MOVABS(ixc-5,iyc  )
	      CALL DRWABS(ixc+5,iyc  )
	      CALL MOVABS(ixc  ,iyc-5)
	      CALL DRWABS(ixc  ,iyc+5)
	      CALL MOVABS(ixc-5,iyc-5)
	      CALL DRWABS(ixc+5,iyc+5)
	      CALL MOVABS(ixc+5,iyc-5)
	      CALL DRWABS(ixc-5,iyc+5)
	      CALL ANMODE
	    endif
	end do

10	call tekvecmode
	CALL DCURSR(IZc,IXc,IYc)
	 !TEKanmode
	gch=char(izc)
	call str_toupper(gch)

11	if(gch.eq.'=') then
	  ax=0.
	  bx=1.
	  ay=0.
	  by=1.
	  mapped=.false.
	  rotate=.false.
	  goto 12

	elseif(gch.eq.' ') then
	  ichx=(ixc-ix0)/scax + ix_1
	  ichy=(iyc-iy0)/scay + iy_1
*	  CALL VECMOD
*	  CALL MOVABS(10,iyc)
*	  CALL tekANMODE
	  call textmode
	  write(6,'('' X = '',i5,''   Y = '',i5,''\n'')') ichx,ichy
	  CALL tekANMODE
	  goto 10

	elseif(gch.eq.'B') then
	  nban=1
	  if(nbpoints(nban).lt.MAXBPOINTS) then
	    ibx=(ixc-ix0)/scax + ix_1
	    iby=(iyc-iy0)/scay + iy_1
	    if(ibx.ge.0 .and. ibx.lt.res1 .and. iby.ge.0 .and. iby.lt.res2) then
	      nbpoints(nban)=nbpoints(nban)+1
	      banana(1,nbpoints(nban),nban)=ibx
	      banana(2,nbpoints(nban),nban)=iby
*	      CALL tekVECMODe
*	      CALL MOVABS(10,iyc)
	      CALL textmode !tekANMODE
	      np=nbpoints(nban)
	      write(6,'('' Banana #'',i1,2i5,''\n'')') nban,banana(1,np,nban),banana(2,np,nban)
	      CALL tekVECMODe
	      ixc=(banana(1,1,nban)-ix_1)*scax+ix0
	      iyc=(banana(2,1,nban)-iy_1)*scay+iy0
	      tekcolor=2
	      call teksetcolor(tekcolor)
	      CALL MOVABS(ixc,iyc)
	      CALL DRWABS(ixc,iyc)
	      do ii=2,nbpoints(nban)
	        ixc=(banana(1,ii,nban)-ix_1)*scax+ix0
	        iyc=(banana(2,ii,nban)-iy_1)*scay+iy0
	        CALL DRWABS(ixc,iyc)
	      end do
	      CALL ANMODE
	      call VECMOD
	    endif
	    modified(nban)=.true.
	  endif
	  goto 10

	elseif(gch.eq.'1' .or. gch.eq.'2') then
	  if(gch.eq.'1') np=1
	  if(gch.eq.'2') np=2
	  ichx=(ixc-ix0)/scax + ix_1
	  ichy=(iyc-iy0)/scay + iy_1
	  CALL VECMOD
	  CALL MOVABS(10,iyc)
	  CALL ANMODE
	  write(6,'(1h+,2i5)') ichx,ichy
	  px(np)=ichx
	  py(np)=ichy
	  pdefined(np)=.true.
	  call tekanmode
	  goto 10

	elseif(gch.eq.'!' .or. gch.eq.'@') then
	  if(gch.eq.'!') np=1
	  if(gch.eq.'@') np=2
	  ichx=(ixc-ix0)/scax + ix_1
	  ichy=(iyc-iy0)/scay + iy_1
	  CALL VECMOD
	  CALL MOVABS(10,iyc)
	  CALL ANMODE
	  write(6,'(1h+,2i5)') ichx,ichy
	  rx(np)=ichx
	  ry(np)=ichy
	  rdefined(np)=.true.
	  call tekanmode
	  goto 10

	elseif(gch.eq.'M') then
	  if(.not.rdefined(1) .or. .not.rdefined(2)) then
	    call ansi_bell(6)
	    goto 10
	  endif
	  if(pdefined(1) .and. pdefined(2)) then
	    if(mapped) then
	      bbx=(rx(2)-rx(1))/(px(2)-px(1))	! nuovi shifts
	      aax=rx(1)-bbx*px(1)
	      bby=(ry(2)-ry(1))/(py(2)-py(1))
	      aay=ry(1)-bby*py(1)
	      ax=aax+bbx*ax			! composti con quelli vecchi
	      bx=bbx*bx
	      ay=aay+bby*ay
	      by=bby*by
	    else
	      bx=(rx(2)-rx(1))/(px(2)-px(1))
	      ax=rx(1)-bx*px(1)
	      by=(ry(2)-ry(1))/(py(2)-py(1))
	      ay=ry(1)-by*py(1)
	    endif
	    pdefined(1)=.false.
	    pdefined(2)=.false.
	  endif
	  mapped=.true.
	  goto 12

	elseif(gch.eq.'X') then
	  if(.not.rdefined(1) .or. .not.rdefined(2)) then
	    call ansi_bell(6)
	    goto 10
	  endif
	  if(pdefined(1) .and. pdefined(2)) then
	    if(mapped) then
	      bbx=(rx(2)-rx(1))/(px(2)-px(1))	! nuovi shifts
	      aax=rx(1)-bbx*px(1)
	      ax=aax+bbx*ax			! composti con quelli vecchi
	      bx=bbx*bx
	    else
	      bx=(rx(2)-rx(1))/(px(2)-px(1))
	      ax=rx(1)-bx*px(1)
	    endif
	    pdefined(1)=.false.
	    pdefined(2)=.false.
	  endif
	  mapped=.true.
	  goto 12

	elseif(gch.eq.'Y') then
	  if(.not.rdefined(1) .or. .not.rdefined(2)) then
	    call ansi_bell(6)
	    goto 10
	  endif
	  if(pdefined(1) .and. pdefined(2)) then
	    if(mapped) then
	      bby=(ry(2)-ry(1))/(py(2)-py(1))	! nuovi shifts
	      aay=ry(1)-bby*py(1)
	      ay=aay+bby*ay			! composti con quelli vecchi
	      by=bby*by
	    else
	      by=(ry(2)-ry(1))/(py(2)-py(1))
	      ay=ry(1)-by*py(1)
	    endif
	    pdefined(1)=.false.
	    pdefined(2)=.false.
	  endif
	  mapped=.true.
	  goto 12

	elseif(gch.eq.'K') then
*	  CALL tekVECMODe
*	  CALL MOVABS(0,500)
	  call textmode
	  write(6,*)
	  i1dmmy = inp_r2('Xa, Xb',ax,bx)
	  i1dmmy = inp_r2('Ya, Yb',ay,by)
	  mapped=.true.
	  CALL tekANMODE
	  goto 12

	elseif(gch.eq.'A') then
*	  CALL tekVECMODe
*	  CALL MOVABS(0,500)
	  call textmode
	  write(6,*)
	  i1dmmy = inp_r1('Angle (degrees)',angle)
	  rotate = angle .NE. 0.
	  if(rotate) i1dmmy = inp_r2('Centered on',rx0,ry0)
	  CALL tekANMODE
	  goto 12

	elseif(gch.eq.'L') then
	  ix_l=(ixc-ix0)/scax + ix_1
	  ix_l=min(max(ix_l,ix_1),ix_2)
	  ixx=ix0+(ix_l-ix_1)*scax
	  CALL VECMOD
	  tekcolor=2
	  call teksetcolor(tekcolor)
	  CALL MOVABS(IXX,IY0)
	  CALL DRWABS(IXX,IY1)
	  CALL tekANMODE
	  goto 10

	elseif(gch.eq.'R') then
	  ix_r=(ixc-ix0)/scax + ix_1
	  ix_r=min(max(ix_r,ix_1),ix_2)
	  ixx=ix0+(ix_r-ix_1)*scax
	  CALL VECMOD
	  tekcolor=2
	  call teksetcolor(tekcolor)
	  CALL MOVABS(ixx,IY0)
	  CALL DRWABS(ixx,IY1)
	  CALL tekANMODE
	  goto 10

	elseif(gch.eq.'O') then
	  iy_o=(iyc-iy0)/scay + iy_1
	  iy_o=min(max(iy_o,iy_1),iy_2)
	  iyy=iy0+(iy_o-iy_1)*scay
	  CALL VECMOD
	  tekcolor=2
	  call teksetcolor(tekcolor)
	  CALL MOVABS(IX0,iyy)
	  CALL DRWABS(IX1,iyy)
	  CALL tekANMODE
	  goto 10

	elseif(gch.eq.'U') then
	  iy_u=(iyc-iy0)/scay + iy_1
	  iy_u=min(max(iy_u,iy_1),iy_2)
	  iyy=iy0+(iy_u-iy_1)*scay
	  CALL tekVECMODe
	  tekcolor=2
	  call teksetcolor(tekcolor)
	  CALL MOVABS(IX0,iyy)
	  CALL DRWABS(IX1,iyy)
	  CALL tekANMODE
	  goto 10

	elseif(gch.eq.'E') then
	  if(ix_l.gt.ix_r) call swapl(ix_r,ix_l)
	  if(iy_u.gt.iy_o) call swapl(iy_o,iy_u)
	  ix_1=ix_l
	  ix_2=ix_r
	  iy_1=iy_u
	  iy_2=iy_o
	  goto 12

	elseif(gch.eq.'I') then
	  if(ix_l.gt.ix_r) call swapl(ix_r,ix_l)
	  if(iy_u.gt.iy_o) call swapl(iy_o,iy_u)
	  iarea=0
	  if(transposed) then
	    do ix=ix_l,ix_r
	      do iy=iy_u,iy_o
	        iarea=iarea+mat(iy,ix)
	      end do
	    end do
	  else
	    do iy=iy_u,iy_o
	      do ix=ix_l,ix_r
	        iarea=iarea+mat(ix,iy)
	      end do
	    end do
	  endif
	  ichx=(ixc-ix0)/scax + ix_1
	  ichy=(iyc-iy0)/scay + iy_1
	  ixc=0
	  iyc=500
	  CALL VECMOD
	  tekcolor=2
	  call teksetcolor(tekcolor)
	  CALL MOVABS(ixc,iyc)
	  iyc=iyc-15
	  CALL ANMODE
	  write(6,'('' X  ='',i5,'' --'',i5)') ix_l,ix_r
	  CALL VECMOD
	  CALL MOVABS(ixc,iyc)
	  iyc=iyc-15
	  CALL ANMODE
	  write(6,'('' Y  ='',i5,'' --'',i5)') iy_u,iy_o
	  CALL VECMOD
	  CALL MOVABS(ixc,iyc)
	  iyc=iyc-15
	  CALL ANMODE
	  write(6,'('' A  = '',i12)') iarea
	  do nban=1,2
	    if(nbpoints(nban).gt.0) then
	      call ban_makebanana(banana(1,1,nban),nbpoints(nban),ban,res1,res2)
	      iarea=0
	      if(transposed) then
	        do ix=ix_l,ix_r
	          if(ban(1,ix).ne.-1) then
	            do iy=max(ban(1,ix),iy_u),min(ban(2,ix),iy_o)
	              iarea=iarea+mat(iy,ix)
	            end do
	          endif
	        end do
	      else
	        do ix=ix_l,ix_r
	          if(ban(1,ix).ne.-1) then
	            do iy=max(ban(1,ix),iy_u),min(ban(2,ix),iy_o)
	              iarea=iarea+mat(ix,iy)
	            end do
	          endif
	        end do
	      endif
	      CALL VECMOD
	      CALL MOVABS(ixc,iyc)
	      iyc=iyc-15
	      CALL ANMODE
	      if(nban.eq.1) write(6,'('' b1 = '',i16 )') iarea
	      if(nban.eq.2) write(6,'('' b2 = '',i16 )') iarea
	      call tekANMODE
	    endif
	  end do
	  goto 10

	elseif(gch.eq.'F') then
	  if(transposed) then
	    ix_1=0
	    ix_2=res2-1
	    iy_1=0
	    iy_2=res1-1
	  else
	    ix_1=0
	    ix_2=res1-1
	    iy_1=0
	    iy_2=res2-1
	  endif
!	  izoff=0
	  goto 12

	elseif(gch.eq.'D') then
	  call textmode
	  i1dmmy = inp_i2('Xmin,Xmax',ix_l,ix_r)
	  i1dmmy = inp_i2('Ymin,Ymax',iy_u,iy_o)
	  ix_l=max(ix_l,0)
	  iy_u=max(iy_u,0)
	  if(transposed) then
	    ix_r=min(ix_r,res2-1)
	    iy_o=min(iy_o,res1-1)
	  else
	    ix_r=min(ix_r,res1-1)
	    iy_o=min(iy_o,res2-1)
	  endif
	  gch='E'
	  call tekanmode
	  goto 11

	elseif(gch.eq.'T') then
	  transposed=.not.transposed
	  call swapl(ix_1,iy_1)
	  call swapl(ix_2,iy_2)
	  goto 12

	elseif(gch.eq.'C') then
	  ov16=.true.
	  do ii=0,res1-1
	    do jj=0,res2-1
	     if(mat(ii,jj).lt.0)then
	      matval=mat(ii,jj)
	      do while(matval .lt. 32768)
	         matval=matval+65536
		 prox(jj)=prox(jj)+65536
		 proy(ii)=proy(ii)+65536
	      enddo
	      if(matval .gt. matmax)matmax=matval
	     endif
	    enddo
	  enddo
	  call tekanmode
	  goto 12

	elseif(gch.eq.'Z') then
	  call textmode
	  i1dmmy = inp_i1('z_offset',izoff)
	  call tekanmode
	  goto 10
	
	elseif(gch .eq. 'S') then
	  call textmode
	  i1dmmy = inp_i1('Color scale --> (1)Lin  (2)Log  (3)Sqrt',isctyp)
	  call tekanmode
	  goto 12

	elseif(gch.eq.'H' .or. gch.eq.'?') then
*	  call initt(960)
*	  call anmode
	  call textmode
	  write(6,*)'=         redraw (without mapping)','\n'
	  write(6,*)'<space>   position','\n'
	  write(6,*)'L         left  marker for E or I','\n'
	  write(6,*)'R         right marker     "','\n'
	  write(6,*)'U         under marker     "','\n'
	  write(6,*)'O         over  marker     "','\n'
	  write(6,*)'E         expansion between L R U O','\n'
	  write(6,*)'I         integral  between L R U O','\n'
	  write(6,*)'F         full display','\n'
	  write(6,*)'D         ask display limits','\n'
	  write(6,*)'B         set next point of banana','\n'
	  write(6,*)'1 2       set 1st and 2nd point  for calibration','\n'
	  write(6,*)'! @       set 1st and 2nd reference  for calibration','\n'
	  write(6,*)'M         calibrate the matrix mapping (1,2)==>(!,@)','\n'
	  write(6,*)'X Y       calibrate the matrix mapping only X Y','\n'
	  write(6,*)'K         calibrate the matrix according to input coefficients','\n'
	  write(6,*)'T         transpose display','\n'
	  write(6,*)'C         correct 16-bit overlow(s) ','\n'
	  write(6,*)'Z         offset for scatter plot','\n'
	  write(6,*)'S         color scaling style for scatter plot','\n'
	  write(6,*)'CTRL_Z    exit program','\n'
	  write(6,*)
*	  write(6,*)'type = to redraw'
*	  write(6,*)
	  call tekanmode
	  goto 10

	elseif((gch.eq.char(3)).or.(gch.eq.char(25)).or.(gch.eq.char(26)))then
*	  CALL INITT(960)
*	  CALL HOME
*	  CALL ANMODE
	  call textmode
	  return
	else
	  call ansi_bell(6)
	  goto 10
	endif

	end


	subroutine MADD_test(nmat)

c!	Determina alcune utili informazioni per un particolare segmento
c!	o per tutta la matrice.
c
c	include "cmat.inc"
c
c	logical*1 cmt_info,cmt_readsegment
c	integer ccomp_compress,ccomp_decompress
c
c	if(.not.opened(nmat)) then
c	  call inp_msg('Matrix is not opened')
c	  return
c	endif
c
c	nsegs=nseg(nmat)
c	nwords=1
c	do ii=1,ndim(nmat)
c	  nwords=nwords*step(ii,nmat)
c	end do
c
c	call inp_i2('Segment number from, to (-1 if all) ',numseg1,numseg2)
c	if(numseg1.lt.0) then
c	  numseg1=0
c	  numseg2=nsegs-1
c	else
c	  numseg1=max(0,min(numseg1,nsegs-1))
c	  numseg2=max(0,min(numseg2,nsegs-1))
c	endif
c	if(numseg1.gt.numseg2) call swapl(numseg1,numseg2)
c	jblo1=numseg1
c	jblo2=numseg2
c
c	do nblo=jblo1,jblo2
c	  if(.not.cmt_readsegment(%val(cmt(nmat)),nblo,bufl(0,nmat),ndrec)) return
c	  if(ndrec.lt.0) return
c	  if(.not.cmt_info(%val(cmt(nmat)),-4,mode,minval)) return
c	  call comp_compress (bufl(0,nmat),nwords,bufl(0,nmat+1),nbytes1,mode1,minval1)
c	  nbytes2 = ccomp_compress(bufl(0,nmat),nwords,bufl(0,nmat+2),mode2,minval2)
c	  if(nbytes2 .ne. nbytes1 .or. mode2.ne.mode1 .or. minval2.ne.minval1) then
c	    write(6,*) '  A',nblo
c	    write(6,*) '  A',nbytes1,mode1,minval1
c	    write(6,*) '  A',nbytes2,mode2,minval2
c	    return
c	  endif
c	  nn=0
c	  do ii = 0, (max(nbytes1,nbytes2)+3)/4-1
c	    if(bufl(ii,nmat+1).ne.bufl(ii,nmat+2)) then
c	      if(nn.lt.20) write(6,*) '  a',ii,bufl(ii,nmat+1),bufl(ii,nmat+2)
c	      nn=nn+1
c	    endif
c	  enddo
c	  if(nn.ne.0) then
c	    write(6,*) '  b',nblo,nn
c	    write(6,*) '  b',nbytes1,mode1,minval1
c	    write(6,*) '  b',nbytes2,mode2,minval2
c	    return
c	  endif
c	  nbytes3 = ccomp_decompress(bufl(0,nmat+3),nwords,bufl(0,nmat+2),mode2,minval2)
c	  if(nbytes3 .ne. nbytes2) then
c	    write(6,*) '  c',nblo,nbytes2,nbytes3,mode2,minval2
c	    return
c	  endif
c	  nn=0
c	  do ii = 0, (max(nbytes3,nbytes2)+3)/4-1
c	    if(bufl(ii,nmat+3).ne.bufl(ii,nmat)) then
c	      if(nn.lt.20) write(6,*) '  d',ii,bufl(ii,nmat),bufl(ii,nmat+3)
c	      nn=nn+1
c	    endif
c	  enddo
c	  if(nn.ne.0) then
c	    write(6,*) '  e',nblo,nn
c	    write(6,*) '  e',nbytes2,nbytes3
c	    return
c	  endif
c	  write(6,*)' ok',nblo,nbytes2,mode,minval
c	end do
c
c	error = .false.
	return

	end



	subroutine MADD_BANANA(nmat)

! Estrae lo spettro di un indice con banana sugli altri indici
! prepara il lavoro per i casi specifici

#include "cmat.inc"

	character gatefile*60
	character specfile*60
	integer   spettro(0:MAXRES-1)
	integer   partial(0:MAXRES-1)

	integer ngates(MAXMATDIM)
	integer wl(MAXGATES,MAXMATDIM)
	integer wh(MAXGATES,MAXMATDIM)
	integer ws(MAXGATES,MAXMATDIM)
	integer tmpbuff(3,MAXGATES)

	integer gate(4)
	integer cside	/1/
	integer cres
	integer iform	/4/
	
	parameter (MAXBANS = 10)
	structure /BANANASTR/
	  character*80 banfname
	  integer limit(2,0:MAXRES-1)
	end structure
	record /BANANASTR/ban(MAXBANS)
	integer NBANS,gside1,gside2
*	save ban,nbans,gside1,gside2

	if(.not.opened(nmat)) then
	  i1dmmy = inp_msg('Matrix is not opened')
	  return
	endif
	if(ndim(nmat).ne.3)then
	 i1dmmy = inp_msg('This command works only for cubes')
	 return
	endif

	NBANS = 0
	

	if(matmode(nmat).gt.0) then
	  cside=1
	  gside1=2
	  gside2=3
	else
	  ii=inp_i2('On which indexes do you defined bananas',gside1,gside2)
	  if(ii.lt.0) return
	  if(gside1.lt.1 .OR. gside1.gt.ndim(nmat)) return
	  if(gside2.lt.1 .OR. gside2.gt.ndim(nmat)) return
	  cside=0
	  do ii=1,ndim(nmat)
	    if( (ii.ne.gside1).and.(ii.ne.gside2) )cside=ii
	  enddo
	  if(cside.lt.1 .OR. cside.gt.ndim(nmat)) return
	endif

	write(6,*) 'Input Banana files (<CR> to end)'
	nbans = madd_getbananas(ban,nmat,gside1,gside2)
	if( nbans .LT. 1 ) return

	cres=res(cside,nmat)
	DO II=0,MAXRES-1
	  SPETTRO(II)=0
	  PARTIAL(II)=0
	end do
	surface=0.000
	write(6,*)
	
	 do ii=0,res(gside1,nmat)-1
	    do jj=1,MAXMATDIM
	      ngates(jj)=0
	    enddo
	    do kk=1,nbans
	       if( (ban(kk).limit(1,ii).ge.0) .and. (ban(kk).limit(2,ii).ge.0) )then
		   ngates(gside2)=ngates(gside2)+1
		   wl(ngates(gside2),gside2)=ban(kk).limit(1,ii)
		   wh(ngates(gside2),gside2)=ban(kk).limit(2,ii)
		   ws(ngates(gside2),gside2) = 1
		   surface = surface+ban(kk).limit(2,ii)-ban(kk).limit(1,ii)+1
		endif
	     enddo
	     if(ngates(gside2) .gt. 0) then
	       ngates(gside1)=1
	       wl(ngates(gside1),gside1)=ii
	       wh(ngates(gside1),gside1)=ii
	       ws(ngates(gside1),gside1) = 1
	       call madd_gate_3dn(nmat,cside,spettro,partial,wl,wh,ws,ngates,tmpbuff)
	       rrr=0
	       do itt=0,cres-1
	          rrr = rrr+spettro(itt)
	       enddo
	       write(6,'(32x,a3,''  sum: '',1p,e12.4)')GO_UP_ONE_LINE,rrr
	       call flush(6)
	     endif
	 enddo
	
	write(6,*)'Total cut surface : ',surface

200	if(error) return
	do ii=cres,MAXRES-1
	  spettro(ii)=0
	end do
	CALL WRITEDATL(1,SPECFILE,spettro,cres,iform,KV)
	lspecfile=lengthc(specfile)
	nk=(cres+1023)/1024
	lk=1
	if(nk.gt.9) lk=2
	if(lk .eq. 1)WRITE(6,'(1X,A,I1)') SPECFILE(1:lspecfile)//'|L:',nk
	if(lk .eq. 2)WRITE(6,'(1X,A,I2)') SPECFILE(1:lspecfile)//'|L:',nk

	error=.false.
	return

	END


	integer function MADD_GETBANANAS(ban,nmat,gside1,gside2)
	
#include "cmat.inc"
	
	integer maxbans,gside1,gside2
	parameter (maxbans=10)
	structure /BANANASTR/
	  character*80 banfname
	  integer limit(2,0:MAXRES-1)
	end structure
	record /BANANASTR/ban(MAXBANS)
	integer luncheck,ii,nbans
	save luncheck,ii,nbans
	integer inp_ch
	external inp_ch

	luncheck = 0
	call LIB$GET_LUN(luncheck)
	if(luncheck.le.0) then
	  i1dmmy = inp_msg('Could not get a LUN')
	  MADD_GETBANANAS=0
	  return
	endif

	nbans = 0
	ii = 1

	do while(.true.)
	    if( ii .gt. maxbans)goto 100
10	    if(inp_ch('Banana Filename',ban(ii).banfname).le.0) goto 100
	    call filetype(ban(ii).banfname,'ban')
	    open(unit=luncheck,file=ban(ii).banfname,status='old',err=11)
	    goto 12
11	    i1dmmy = inp_msg('Cannot open banana file')
            goto 10
12	    close(luncheck)	    
	    call ban_getbanomap(6,ban(ii).banfname,-1,-1,ban(ii).limit(1,0),res(gside1,nmat),res(gside2,nmat),nbpoints)
	    if(nbpoints.le.0)then
	       i1dmmy = inp_msg('Cannot read banana from file')
	       goto 10
	    endif
	    nbans = ii
	    ii = ii + 1
	enddo
	
100	continue
	MADD_GETBANANAS = nbans
	return	
	end
	


	subroutine MADD_SET_AUTOBG
		
#include "cmat.inc"
	real tmp(3)
	
	if( .not. autobg.INITIALISED )then
	    autobg.M           = 2
	    autobg.FSTEP       = 0.1000000
	    autobg.ITMAX       = 2
	    autobg.INITIALISED = .TRUE.
	endif
	
	tmp(1)=autobg.M
	tmp(2)=autobg.FSTEP
	tmp(3)=autobg.ITMAX
	
	i1dmmy = inp_r3('    ---> [ win_size, frac_step, max_iter_no ]',tmp(1), tmp(2),tmp(3))

	autobg.M       = nint(tmp(1))
	autobg.FSTEP   = tmp(2)
	autobg.ITMAX   = nint(tmp(3))
	autobg.CHANGED = .TRUE.
	autobg.YES     = .FALSE.
	
	error = .FALSE.
	end
	

	subroutine AUTOBG_CmatChangeNotify
	
#include "cmat.inc"

	if( CMATFILE(1) .ne. autobg.CmatName ) autobg.CHANGED = .TRUE.
	autobg.YES = .FALSE.
	
	end

	subroutine AUTOBG_CmatAcknowledge
	
#include "cmat.inc"

	autobg.CmatName = CMATFILE(1)
	autobg.CHANGED  = .FALSE.
	autobg.YES      = .TRUE.
	
	end



	subroutine AUTOBG_CmatReset

#include "cmat.inc"
	logical*1 cmt_info, cmt_getproje
	external cmt_info, cmt_getproje
	
	integer mdim, mmode,mres(4),mstep(4)
	integer mproj(0:maxres-1)
	
	call AUTOBG_CmatChangeNotify
	
	if( .not. autobg.INITIALISED )then
		i1dmmy = inp_msg('Automatic background not initialized, call SET_BACKGROUND command')
		error = .TRUE.
		return
	endif
	
	if( .not.opened(1) )then
		i1dmmy = inp_msg('Compressed matrix not opened')
		error = .TRUE.
		return
	endif
	
	if(.not.cmt_info(%val(cmt(1)),-1,mdim,mmode))then
		i1dmmy = inp_msg('Compressed matrix of unknown type')
		error = .TRUE.
		return
	endif
	
	if( mdim .ne. 2 .or. mmode .eq. 0 )then
		i1dmmy = inp_msg('Automatic background implemented only for 2d-symm matrices')
		error = .TRUE.
		return
	endif
	
	if( .not. autobg.CHANGED )then
		autobg.YES = .TRUE.
		error = .FALSE.
		return
	endif
	
	call cmt_readspec_reset		 ! reset internal buffers

	if(.not.cmt_info(%val(cmt(1)),-2,mres,mstep))then
		i1dmmy = inp_msg('Errror reading from compressed matrix')
		error = .TRUE.
		return
	endif
	autobg.RES = mres(1)
	
	if(.not.cmt_getproje(%val(cmt(1)),1,mproj))then
		i1dmmy = inp_msg('Errror reading projection from compressed matrix')
		error = .TRUE.
		return
	endif
	call cmt_readspec_reset		 ! reset internal buffers

	autobg.TOT = 0
	do ii = 0, autobg.RES
		autobg.P(ii) = mproj(ii)
		autobg.TOT = autobg.TOT + mproj(ii)
	enddo
	call autobgmin(autobg.P(0),autobg.BP(0),autobg.RES,1,autobg.RES,autobg.M,autobg.ITMAX,autobg.FSTEP)

	call AUTOBG_CmatAcknowledge
	error  = .FALSE.
	
	end
	

	subroutine AUTOBG_Output

#include "cmat.inc"
	
	CALL WRITEDATR(1,autobg.SPECNAME,autobg.BG,autobg.RES,3,KV)
	
	if( KV .le. 0)error = .TRUE.

	end


	subroutine MADD_GET_AUTOBG
		
#include "cmat.inc"

	integer NMarkers
	integer Marker(2*MAXGATES)

	integer   spettro(0:MAXRES-1)
	integer   partial(0:MAXRES-1)
	real      cut(0:MAXRES-1)
	real      e2cut(0:MAXRES-1)

	integer ngates(MAXMATDIM)
	integer wl(MAXGATES,MAXMATDIM)
	integer wh(MAXGATES,MAXMATDIM)
	integer ws(MAXGATES,MAXMATDIM) 
	
	integer cside    /1/
	integer gside    /2/

	
	call AUTOBG_CmatReset
*	if( .not. autobg.YES ) call AUTOBG_Output
	if( error )return

	NMarkers = (inp_ia('Gates markers [Low_1,High_1, ... ,Low_n,High_n]',Marker,20)/2)*2
*	write(*,*) NMarkers

	
	if( NMarkers .lt. 2 )then
		i1dmmy = inp_msg('At least a couple of valid markers must be given')
		error = .TRUE.
		return
	endif

	if( (Marker(1) .eq. autobg.RES) .and. (Marker(2) .eq. autobg.RES) ) then
	     do ii=0,autobg.RES-1
	     	     autobg.BG(ii) = autobg.BP(ii)
	     enddo
	     write(6,*) autobg.RES, autobg.RES
	     call AUTOBG_Output
	     return
	endif
	
	do ii =0,MAXRES-1
		spettro(ii) = 0
		partial(ii)=0
	enddo
	
	ngates(gside) = 0
	
	do ii = 1,NMarkers,2
	   if( Marker(ii).ge.0.and.Marker(ii+1).ge.0.and.Marker(ii).lt.autobg.RES.and.Marker(ii+1).lt.autobg.RES)then 
		ngates(gside) = ngates(gside)+1
		wl(ngates(gside),gside) = min(Marker(ii),Marker(ii+1))
		wh(ngates(gside),gside) = max(Marker(ii),Marker(ii+1))
		ws(ngates(gside),gside) = 1
	   endif
	enddo

	if( ngates(gside) .lt. 1 )then
		i1dmmy = inp_msg('At least a couple of valid markers must be given')
		error = .TRUE.
		return
	endif
	
	call madd_gate_2d(1,cside,spettro,partial,wl,wh,ws,ngates,bufl)

	autobg.GSC=0.0
	autobg.GBC=0.0
	do ii=0,autobg.RES-1
		autobg.SP(ii) = spettro(ii)
	  	autobg.GSC    = autobg.GSC+spettro(ii)
	enddo
	do nn=1,ngates(gside)
	  do ii=wl(nn,gside),wh(nn,gside)
	    autobg.GBC=autobg.GBC+autobg.BP(ii)
	  enddo
*	  write(*,*) nn, autobg.GBC
	enddo
	call autobgmin(autobg.SP(0),autobg.BG(0),autobg.RES,1,autobg.RES,autobg.M,autobg.ITMAX,autobg.FSTEP)

        autobg.GSC=autobg.GSC/autobg.TOT
        autobg.GBC=autobg.GBC/autobg.TOT
        do ii=0,autobg.RES-1
          autobg.RR=sqrt(4.0*autobg.BG(ii)+1.96)+1.4
          autobg.RR=1-4.0*autobg.BG(ii)/autobg.RR/autobg.RR
          autobg.BG(ii)=autobg.BG(ii)+autobg.GSC*autobg.RR*autobg.BP(ii)
        enddo
        do ii=0,autobg.RES-1
          autobg.BG(ii)=autobg.BG(ii)+(autobg.P(ii)-autobg.BP(ii))*autobg.GBC
	  cut(ii) = spettro(ii)
          e2cut(ii)= abs(cut(ii))+abs(autobg.BG(ii))
          cut(ii) = cut(ii)-autobg.BG(ii)
          autobg.SP(ii)=0.00000
          autobg.MARK(ii)=.FALSE.
          if(cut(ii)+sqrt(e2cut(ii))*(1.00+2.00/(0.10+e2cut(ii))) .le. 0.0000)then
            if(ii.gt.0)then
              if(.not. autobg.MARK(ii-1))autobg.SP(ii-1)=cut(ii-1)+sqrt(e2cut(ii-1))
            endif
            autobg.SP(ii)=-cut(ii)
            autobg.MARK(ii)=.true.
          else
            if(ii.gt.0)then
              if(autobg.MARK(ii-1))autobg.SP(ii)=cut(ii)+sqrt(e2cut(ii))
            endif
          endif
        enddo
        r_sum=0
        do ii=1,autobg.RES-2
          autobg.BG(ii)=(autobg.SP(ii-1)/2.000+autobg.SP(ii)+autobg.SP(ii+1)/2.000)/2.000
          if(autobg.BG(ii) .gt.autobg.SP(ii))autobg.BG(ii)=autobg.BG(ii)*0.25+autobg.SP(ii)*0.75
          if(autobg.bg(ii) .le. 0.000)autobg.BG(ii)=0.000
          if(autobg.MARK(ii))then
             cut(ii)=cut(ii)+autobg.BG(ii)
             e2cut(ii)=e2cut(ii)+autobg.BG(ii)
             if( cut(ii)+sqrt(e2cut(ii))*(1.00+2.00/(0.10+e2cut(ii))) .lt. 0.00000 )then
        	 cut(ii) = cut(ii)+sqrt(e2cut(ii))/2.000
             endif
          endif
        enddo

	do ii=0,autobg.RES-1
		autobg.SP(ii) = spettro(ii)
	  	autobg.BG(ii) = autobg.SP(ii) - cut(ii)
	enddo

	error = .FALSE.	
	call AUTOBG_Output
	
	end
	
	
