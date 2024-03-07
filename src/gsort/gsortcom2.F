CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       GS0_... definizione      C
C       GS1_... lista            C
C       GS2_... inizializzazione C
C       GS3_... esecuzione       C
C       GS4_... finalizzazione   C
C       GS5_... opzionale        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


CCCCCCCCCCCCCCCCCCCCCC Comandi di sort CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine gs0_projections(pro)

#include "gsort.inc"
	record/proje/pro

	logical*1 getmem

	character*16 specname(0:maxpar-1,0:ndtype)
	integer ieq_specname
	equivalence (specname,ieq_specname)
	character*3 nm_fmt

	synt(1)='PROJECTIONS   [ Filename{for_all_defined_parameters} ]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(TDPAR(0).GT.0) then
	  SPECNAME(0,0)=dname(0)
	  iin=lengthn(comline)
	  if(iin.ne.0) call istr_ch(comline,SPECNAME(0,0))
	  lname=lengthc(SPECNAME(0,0))
	  if(lname.LT.1) call gs_syntax('Filename of ''//dname(0)//''  projection expected')
	endif

	do ji=1,NTIPI
	  if(TDPAR(ji).LT.10) then
	    nn=1
	  else
	    nn=2
	  endif
	  write(nm_fmt,'(i1,a1,i1)')nn,'.',nn
	  do ii=0,TDPAR(ji)-1
	    write(SPECNAME(ii,ji),'(a,I'//nm_fmt//')') dname(ji),ii
	    iin=lengthn(comline)
	    if(iin.ne.0) call istr_ch(comline,SPECNAME(ii,ji))
	    lname=lengthc(SPECNAME(ii,ji))
	    if(lname.LT.1) then
	      write(line,'(''Filename of '',a,I'//nm_fmt//',A)') dname(ji),ii,' projection expected'
	      lline=lengthc(line)
	      call gs_syntax(line(1:lline))
	    endif
	  end do
	end do

	iaddr=0

	mfres=0
	if(specname(0,0) .ne. '-') then
	  DO jp=0,TDPAR(0)-1
	    mfres=max(mfres,PARRES(jp,0))
	  end do
	  iaddr=iaddr+mfres*TDPAR(0)
	endif
	pro.mfres=mfres

	do ji=1,NDTYPE
	  DO jp=0,TDPAR(ji)-1
	    if(specname(jp,ji) .ne. '-') iaddr=iaddr+PARRES(jp,ji)*NITEMS(ji)
	  end do
	end do
	pro.len=iaddr

	namelen=16*MAXPAR*(NDTYPE+1)
	iaddrlen=4*MAXDET*MAXPAR*(NDTYPE+1)
	iprojlen=4*iaddr
	ibsize= namelen + iaddrlen + iprojlen
	if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for PROJECTIONS'
	com(icmd).size=ibsize
	call vmovb(%ref(ieq_specname),1,%val(com(icmd).addr),1,namelen)	! riporta i nomi degli spettri

	return

	end

	subroutine gs1_projections(pro,dat)

#include "gsort.inc"
	record/proje/pro
	record/projedat/dat

	if(TDPAR(0).GT.0) then
	  lname=lengthc(dat.name(0,0))
	  write(list(llist:),'(A)') dat.name(0,0)(1:lname)
	  llist=lengthc(list)+3
	endif
	do ji=1,NDTYPE
	  if(TDPAR(ji).GT.0) then
	    do ii=0,TDPAR(ji)-1
	      lname=lengthc(dat.name(ii,ji))
	      write(list(llist:),'(A)') dat.name(ii,ji)(1:lname)
	      llist=lengthc(list)+2
	    end do
	    llist=llist+1
	  endif
	end do

	return

	end

	subroutine gs2_projections(pro,dat)

#include "gsort.inc"
	record/proje/pro
	record/projedat/dat

	IF(init_mode.EQ.0) then
	  iaddr=0
	  DO jp=0,TDPAR(0)-1
	    if(pro.mfres .GT.0) then
	      dat.addr(0,jp,0)=iaddr
	      iaddr=iaddr+pro.mfres
	    else
	      dat.addr(0,jp,0)=-1
	    endif
	  end do
	  do ji=1,NDTYPE
	    DO jp=0,TDPAR(ji)-1
	      if(dat.name(jp,ji) .ne. '-') then
	        nrres=PARRES(jp,ji)
	        DO nriv=0,NITEMS(ji)-1
	          dat.addr(nriv,jp,ji)=iaddr
	          iaddr=iaddr+nrres
	        end do
	      else
	        DO nriv=0,NITEMS(ji)-1
	          dat.addr(nriv,jp,ji)=-1
	        end do
	      endif
	    end do
	  end do
	  if(iaddr .ne. pro.len) stop 'Inconsistency in GS2_PROJECTION'
	endif

	call lvect_erase(dat.spec,pro.len)

	return

	end

	subroutine gs3_projections(pro,dat)

#include "gsort.inc"
	record/proje/pro
	record/projedat/dat

	do ji=0,NTIPI
	  if(NDET(ji).GT.0) then
	    nit=nitems(ji)
	    do jp=0,TDPAR(ji)-1
	      if(dat.addr(0,jp,ji) .GE. 0) then
	        ires=PARRES(jp,ji)
	        do jd=DOFF(ji),DOFF(ji)+NDET(ji)-1
 	          key=det(jd).id
	          if(key.lt.nit) then
	            ICAN=det(jd).ival(jp)
	            if(ican.GT.0 .AND. ican.LT.ires) then
	              iaddr=ican+dat.addr(key,jp,ji)
	              dat.spec(iaddr)=dat.spec(iaddr)+1
	              pro.nincr=pro.nincr+1
	            endif
	          endif
	        end do
	      endif
	    end do
	  endif
	end do
	return

	end

	subroutine gs4_projections(pro,dat)

#include "gsort.inc"
	record/proje/pro
	record/projedat/dat

	CHARACTER FILEPRO*40
	CHARACTER RUN*5
	integer*8 isum,isumtot

	nincr(1)=nincr(1)+pro.nincr
	if(finit_mode.ne.1) return

	RUN=' '
	WRITE(RUN(1:4),'(I4.4)') abs(iio.run)

	if(dat.name(0,0) .ne. '-' ) then
	  lname=lengthc(dat.name(0,0))
	  isumtot=0
	  mfres=pro.mfres
	  DO nriv=0,TDPAR(0)-1
	    iaddr=dat.addr(0,nriv,0)
	    isum=dat.spec(iaddr)
	    DO ICAN=1,mfres-1
	      iaddr=iaddr+1
	      ISUM=ISUM+ABS(dat.spec(iaddr))
	    end do
	    IF(ISUM.GT.0) WRITE(LU2,'(1x,a,''#'',i3.3,I15,'' Counts'')') dat.name(0,0)(1:lname),nriv,isum
	    isumtot=isumtot+isum
	  end do
	  IF(ISUMtot.GT.0) THEN
	    WRITE(FILEPRO,'(A,1H.,A)') dat.name(0,0)(1:lname),RUN
	    lfilepro=lengthc(filepro)
	    iaddr=dat.addr(0,0,0)
	    CALL WRITESPECL(FILEPRO,dat.spec(iaddr),'L',mfres*TDPAR(0),KV)
	    if(kv.GT.0) then
	      WRITE(LU1,'(1X,A,I15,10x,'' Counts'')') filepro(1:lfilepro),isumtot
	      WRITE(LU2,'(1X,A,I15,10x,'' Counts'')') filepro(1:lfilepro),isumtot
	    else
	      WRITE(LU1,*) 'Error writing  '//filepro(1:lfilepro),kv
	      WRITE(LU2,*) 'Error writing  '//filepro(1:lfilepro),kv
	    endif
	  ENDIF
	endif

	do ji=1,NDTYPE
	  DO JP=0,TDPAR(ji)-1
	    if(dat.name(jp,ji) .ne. '-') then
	      lname=lengthc(dat.name(jp,ji))
	      isumtot=0
	      nrres=PARRES(jp,ji)
	      DO nriv=0,NITEMS(ji)-1
	        iaddr=dat.addr(nriv,jp,ji)
	        isum=dat.spec(iaddr)
	        DO ICAN=1,nrres-1
	          iaddr=iaddr+1
	          ISUM=ISUM+ABS(dat.spec(iaddr))
	        end do
	        IF(ISUM.GT.0) WRITE(LU2,'(1X,A,''#'',i3.3,I15,'' Counts'')') dat.name(jp,ji)(1:lname),nriv,isum
	        isumtot=isumtot+isum
	      end do
	      IF(ISUMTOT.GT.0) THEN
	        WRITE(FILEPRO,'(A,1H.,A)') dat.name(jp,ji)(1:lname),RUN
	        lfilepro=lengthc(filepro)
	        iaddr=dat.addr(0,jp,ji)
	        CALL WRITESPECL(FILEPRO,dat.spec(iaddr),'L',nrres*NITEMS(ji),KV)
		if(kv.GT.0) then
	          WRITE(LU1,'(1X,A,I15,10x,'' Counts'')') filepro(1:lfilepro),isumtot
	          WRITE(LU2,'(1X,A,I15,10x,'' Counts'')') filepro(1:lfilepro),isumtot
		else
	          WRITE(LU1,*) 'Error writing  '//filepro(1:lfilepro),kv
	          WRITE(LU2,*) 'Error writing  '//filepro(1:lfilepro),kv
		endif
	      ENDIF
	    endif
	  end do
	end do

	return

	end


	subroutine gs0_sort1d(spec)

#include "gsort.inc"
	record/spectrum/spec

	logical*1 getmem

	synt(1)='SORT1D  Px  spectrumname  [RES R]  [Hn [#times]] [Cubic|Spherical]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	call gs_getindpar(spec.ind,spec.par,spec.res)

	call istr_ch(comline,spec.file)
	if(lengthc(spec.file).LT.1) call gs_syntax('Filename of spectrum expected')

	call gs_resstep(spec.res,istep,1)
	
	call gs_gethash(spec.ihash,spec.nhash,spec.hind,spec.hpar,spec.spherical)
	if(spec.ihash.GE.0) then
	  spec.hsame=.FALSE.
	  if(spec.ind.EQ.spec.hind) spec.hsame=.TRUE.
	endif

	ibsize= 4*spec.res
	if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for SORT1D spectrum'
	com(icmd).size=ibsize

	return

	end

	subroutine gs1_sort1d(spec)

#include "gsort.inc"
	record/spectrum/spec

	call gs_putindpar(spec.ind,spec.par)

	lfile=max(1,lengthc(spec.file))
	write(list(llist+2:),'(1x,a)') spec.file(1:lfile)

	llist=lengthc(list)+3
	call gs_putres(spec.res,1)

	if(spec.nhash.GT.0) then
	  llist=lengthc(list)+3
	  lph=1
	  if(spec.ihash.GE.10) lph=2
	  if(spec.spherical) then
	    write(list(llist:),4201) spec.ihash,' applied',spec.nhash,' time(s)'
	  else
	    write(list(llist:),4202) spec.ihash,' applied',spec.nhash,' time(s)'
	  endif 
	endif	   

#if defined( __GFORTRAN__ )
4201	format('Spherical H',I0,A,I3,A)
4202	format('Cubic     H',I0,A,I3,A)
#else
4201	format('Spherical H',I<lph>,A,I3,A)
4202	format('Cubic     H',I<lph>,A,I3,A)
#endif

	return

	end


	subroutine gs2_sort1d(spec,dat)

#include "gsort.inc"
	record/spectrum/spec
	record/specdat/dat

	spec.nincr=0
	call lvect_erase(dat.spec,spec.res)

	return

	end

	subroutine gs3_sort1d(spec,dat)

#include "gsort.inc"
	record/spectrum/spec
	record/specdat/dat

	logical*1 hsave

	ji=spec.ind
	if(NDET(ji).LT.1) goto 8000	! niente da incrementare
	
	jp=spec.par
	ires=spec.res

	if(spec.nhash.GT.0) goto 2000

1000	j1f=DOFF(ji)
	j1l=DOFF(ji)+NDET(ji)-1
	do j1=j1f,j1l
	  ican=det(j1).ival(jp)
	  if(ican.GT.0 .AND. ican.LT.ires) then
	    dat.spec(ican)=dat.spec(ican)+1
	    spec.nincr=spec.nincr+1
	  endif
	end do
	return

2000	nhash=spec.nhash
	call gs3_hgate(%val(hashaddr(spec.ihash)))
	if(hntrue.LT.nhash) goto 8000	! numero di gates insufficiente

	if(spec.spherical) goto 3000	! questi vanno verificati in modo piu' completo

	if(.NOT.spec.hsame) goto 1000	! puo' incrementarli tutti perche' i gates sono in un altro rivelatore
	if(hntrue.GT.nhash) goto 1000	! anche se si incrementa uno di gate ne rimangono sempre almeno nhash
    
	j1f=DOFF(ji)
	j1l=DOFF(ji)+NDET(ji)-1
	do j1=j1f,j1l
	  ican=det(j1).ival(jp)
	  if(ican.GT.0 .AND. ican.LT.ires .AND. .NOT.hltrue(j1)) then
	    dat.spec(ican)=dat.spec(ican)+1
	    spec.nincr=spec.nincr+1
	  endif
	end do
	return

3000	call gs3_hgate_sph(%val(hashaddr(spec.ihash)))
	if(hstrue.LT.nhash) goto 8000	! numero di gates insufficiente

	if(.NOT.spec.hsame) goto 1000	! puo' incrementarli tutti perche' i gates sono in un altro rivelatore

	lhstrue=hstrue
	j1f=DOFF(ji)
	j1l=DOFF(ji)+NDET(ji)-1
	do j1=j1f,j1l
	  ican=det(j1).ival(jp)
	  if(hltrue(j1)) then
	    hltrue(j1)=.FALSE.
	    hsave=.TRUE.
	    call gs3_hgate_sph(%val(hashaddr(spec.ihash))) ! quanti in gate togliendo questo?
	  else
	    hsave=.FALSE.
	  endif
	  if(hstrue.GE.nhash) then
	    if(ican.GT.0 .AND. ican.LT.ires) then
	      dat.spec(ican)=dat.spec(ican)+1
	      spec.nincr=spec.nincr+1
	    endif
	  endif
	  if(hsave) then		! rimette le cose a posto
	    hltrue(j1)=.TRUE.
	    hstrue=lhstrue
	  endif
	end do

8000	return

	end

	subroutine gs4_sort1d(spec,dat)

#include "gsort.inc"
	record/spectrum/spec
	record/specdat/dat

	CHARACTER FILEPRO*80
	CHARACTER RUN*5
	integer*8 isum,isumtot

	nincr(1)=nincr(1)+spec.nincr
	if(finit_mode.ne.1) return

	WRITE(RUN(1:4),'(I4.4)') abs(iio.run)

	lname=lengthc(spec.file)
	ires=spec.res
	isum=0
	do ii=0,ires-1
	  isum=isum+abs(dat.spec(ii))
	end do
	IF(ISUM.GT.0) then
	  WRITE(FILEPRO,'(A,1H.,A)') spec.file(1:lname),RUN
	  lfilepro=lengthc(filepro)
	  CALL WRITESPECL(FILEPRO,dat.spec,'L',spec.res,KV)
	  if(kv.GT.0) then
	    nk=(spec.res+1023)/1024
	    nn=1
	    if(nk.GE.10) nn=nn+1
	    if(nk.GE.100) nn=nn+1
	    if(nk.GE.1000) nn=nn+1
	    WRITE(LU1,4201) filepro(1:lfilepro)//'|L:',nk,isum
	    WRITE(LU2,4201) filepro(1:lfilepro)//'|L:',nk,isum
	  else
	    WRITE(LU1,*) 'Error writing  '//filepro(1:lfilepro),kv
	    WRITE(LU2,*) 'Error writing  '//filepro(1:lfilepro),kv
	  endif
	ENDIF
#if defined( __GFORTRAN__ )
4201	format(1X,A,I0,I15,' Counts')
#else
4201	format(1X,A,I<nn>,I15,' Counts')
#endif

	return

	end





	subroutine gs0_sort1d_m(spec)

#include "gsort.inc"
	record/spectrum/spec

	logical*1 getmem

	synt(1)='SORT1D_M  Px  spectrumname  [RES R] [MINFOLD m] Hn maxtimes [Cubic|Spherical]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	if(hashgiven.LE.0) call gs_syntax('HGATE not defined')

	call gs_getindpar(spec.ind,spec.par,spec.res)

	call istr_ch(comline,spec.file)
	if(lengthc(spec.file).LT.1) call gs_syntax('Filename of spectrum expected')

	call gs_resstep(spec.res,istep,1)

	spec.minfold=1
	iin=lengthn(comline)
	if(iin.LT.0 .AND. (comline(1:1).EQ.'M' .OR. comline(1:1).EQ.'m')) then
	  call istr_ch(comline,comsort)
	  iin=lengthn(comline)
	  if(iin.LE.0) call gs_syntax('Missing minfold value')
	  ii=istr_i1(comline,spec.minfold)
	endif

	call gs_gethash(spec.ihash,spec.nhash,spec.hind,spec.hpar,spec.spherical)
	if(spec.ihash.LT.0) call gs_syntax('Hn maxtime (with maxtimes>1) extected')
	if(spec.nhash.LE.1) call gs_syntax('Hn maxtime (with maxtimes>1) extected')
	spec.hsame=.FALSE.
	if(spec.ind.EQ.spec.hind) spec.hsame=.TRUE.

	ibsize=4*spec.res*(spec.nhash+1)
	if(.NOT.getmem(ibsize,com(icmd).addr)) Stop 'Error allocating memory for SORT1D_M spectrum'
	com(icmd).size=ibsize

	return

	end

	subroutine gs1_sort1d_m(spec)

#include "gsort.inc"
	record/spectrum/spec

	call gs_putindpar(spec.ind,spec.par)

	lfile=max(1,lengthc(spec.file))
	write(list(llist+2:),'(1x,a)') spec.file(1:lfile)

	llist=lengthc(list)+3
	call gs_putres(spec.res,1)

	write(list(llist+3:),'(''MINFOLD'',i3)') spec.minfold

	llist=lengthc(list)+3
	lph=1
	if(spec.ihash.GE.10) lph=2
	if(spec.spherical) then
	  write(list(llist:),4201) spec.ihash,' applied up to ',spec.nhash,' time(s)'
	else
	  write(list(llist:),4202) spec.ihash,' applied up to ',spec.nhash,' time(s)'
	endif

#if defined( __GFORTRAN__ )
4201	format('Spherical H',I0,A,I3,A)
4202	format('Cubic     H',I0,A,I3,A)
#else
4201	format('Spherical H',I<lph>,A,I3,A)
4202	format('Cubic     H',I<lph>,A,I3,A)
#endif

	return

	end


	subroutine gs2_sort1d_m(spec,dat)

#include "gsort.inc"
	record/spectrum/spec
	record/specdat/dat

	spec.nincr=0
	call lvect_erase(dat.spec,spec.res*max(1,spec.nhash))

	return

	end

	subroutine gs3_sort1d_m(spec,dat)

#include "gsort.inc"
	record/spectrum/spec
	record/specdat/dat

	logical*1 hsave

	ji=spec.ind
	if(NDET(ji).LT.spec.minfold) goto 8000	! niente da incrementare

	jp=spec.par
	ires=spec.res
	j1f=DOFF(spec.ind)
	j1l=DOFF(spec.ind)+NDET(spec.ind)-1

	nhash=spec.nhash
	call gs3_hgate(%val(hashaddr(spec.ihash)))

	if(hntrue.EQ.0) then
	  do j1=j1f,j1l				! no HGATE: vanno sempre tutti nel primo spettro
	    ican=det(j1).ival(jp)
	    if(ican.GT.0 .AND. ican.LT.ires) then
	      dat.spec(ican)=dat.spec(ican)+1
	      spec.nincr=spec.nincr+1
	    endif
	  end do
	  return
	endif

	if(hntrue.EQ.1) then
	  if(.NOT.spec.hsame) then		! 1 HGATE su un altro rivelatore
	    do j1=j1f,j1l
	      ican=det(j1).ival(jp)
	      if(ican.GT.0 .AND. ican.LT.ires) then
	        dat.spec(ican)=dat.spec(ican)+1
	        ican=ican+ires
	        dat.spec(ican)=dat.spec(ican)+1
	        spec.nincr=spec.nincr+2
	      endif
	    end do
	  else					! 1 HGATE sullo stesso rivelatore
	    do j1=j1f,j1l
	      ican=det(j1).ival(jp)
	      if(ican.GT.0 .AND. ican.LT.ires) then
	        if(hltrue(j1)) then
	          dat.spec(ican)=dat.spec(ican)+1
	          spec.nincr=spec.nincr+1
	        else
	          dat.spec(ican)=dat.spec(ican)+1
	          ican=ican+ires
	          dat.spec(ican)=dat.spec(ican)+1
	          spec.nincr=spec.nincr+2
	        endif
	      endif
	    end do
	  endif
	  return
	endif

	if(.NOT.spec.spherical) then
	  if(spec.hsame) then			! >1 HGATE CUBICO sullo stesso rivelatore
	    do j1=j1f,j1l
	      ican=det(j1).ival(jp)
	      if(ican.GT.0 .AND. ican.LT.ires) then
	        if(hltrue(j1)) then
	          nn=min(hntrue-1,nhash)
	        else
	          nn=min(hntrue,nhash)
	        endif
	        do ii=0,nn
	          dat.spec(ican)=dat.spec(ican)+1
	          ican=ican+ires
	        end do
	        spec.nincr=spec.nincr+nn+1
	      endif
	    end do
	  else					! >1 HGATE CUBICO su un altro rivelatore
	    nn=min(hntrue,nhash)
	    do j1=j1f,j1l
	      ican=det(j1).ival(jp)
	      if(ican.GT.0 .AND. ican.LT.ires) then
	        do ii=0,nn
	          dat.spec(ican)=dat.spec(ican)+1
	          ican=ican+ires
	        end do
	        spec.nincr=spec.nincr+nn+1
	      endif
	    end do
	  endif
	  return
	endif

	call gs3_hgate_sph(%val(hashaddr(spec.ihash)))
	if(spec.hsame) then
	  lhstrue=hstrue			! >1 HGATE SFERICO sullo stesso rivelatore
	  do j1=j1f,j1l
	    ican=det(j1).ival(jp)
	    if(hltrue(j1)) then
	      hltrue(j1)=.FALSE.
	      hsave=.TRUE.
	      call gs3_hgate_sph(%val(hashaddr(spec.ihash))) ! quanti in gate togliendo questo?
	    else
	      hsave=.FALSE.
	    endif
	    nn=min(hstrue,nhash)
	    if(ican.GT.0 .AND. ican.LT.ires) then
	      do ii=0,nn
	        dat.spec(ican)=dat.spec(ican)+1
	        ican=ican+ires
	      end do
	      spec.nincr=spec.nincr+nn+1
	    endif
	    if(hsave) then		! rimette le cose a posto
	      hltrue(j1)=.TRUE.
	      hstrue=lhstrue
	    endif
	  end do
	else
	  nn=min(hstrue,nhash)			! >1 HGATE SFERICO su un altro rivelatore
	  do j1=j1f,j1l
	    ican=det(j1).ival(jp)
	    if(ican.GT.0 .AND. ican.LT.ires) then
	      do ii=0,nn
	        dat.spec(ican)=dat.spec(ican)+1
	        ican=ican+ires
	      end do
	      spec.nincr=spec.nincr+nn+1
	    endif
	  end do
	endif

8000	return

	end

	subroutine gs4_sort1d_m(spec,dat)

#include "gsort.inc"
	record/spectrum/spec
	record/specdat/dat

	CHARACTER FILEPRO*80
	CHARACTER RUN*5
	integer*8 isum,isumtot

	nincr(1)=nincr(1)+spec.nincr
	if(finit_mode.ne.1) return

	WRITE(RUN(1:4),'(I4.4)') abs(iio.run)

	lname=lengthc(spec.file)
	ires=spec.res
	nspec=spec.nhash+1
	isumtot=0
	do nn=0,nspec-1
	  isum=0
	  do ii=ires*nn,ires*(nn+1)-1
	    isum=isum+abs(dat.spec(ii))
	  end do
	  IF(ISUM.GT.0 .AND. nspec.GT.1) WRITE(LU2,'(''#HGATEs='',I3,I15,'' Counts'')') nn,isum
	  isumtot=isumtot+isum
	end do
	IF(ISUMtot.GT.0) then
	  WRITE(FILEPRO,'(A,1H.,A)') spec.file(1:lname),RUN
	  lfilepro=lengthc(filepro)
	  CALL WRITESPECL(FILEPRO,dat.spec,'L',spec.res*nspec,KV)
	  if(kv.GT.0) then
	    nk=(spec.res*nspec+1023)/1024
	    nn=1
	    if(nk.GE.10) nn=nn+1
	    if(nk.GE.100) nn=nn+1
	    if(nk.GE.1000) nn=nn+1
	    WRITE(LU1,4201) filepro(1:lfilepro)//'|L:',nk,isumtot
	    WRITE(LU2,4201) filepro(1:lfilepro)//'|L:',nk,isumtot
	  else
	    WRITE(LU1,*) 'Error writing  '//filepro(1:lfilepro),kv
	    WRITE(LU2,*) 'Error writing  '//filepro(1:lfilepro),kv
	  endif
	ENDIF
#if defined( __GFORTRAN__ )
4201	format(1X,A,I0,I15,' Counts')
#else
4201	format(1X,A,I<nn>,I15,' Counts')
#endif
	return

	end





	subroutine gs0_sort2d(mat)

#include "gsort.inc"
	record/matrix/mat
	
        synt(1)='SORT2D  Px Py  matrixname [RES Rx Ry] [STEP Sx Sy] [Hn [#times]]'
CCCC	synt(1)='SORT2D  [({] Px Py [)}]  matrixname [RES Rx Ry] [STEP Sx Sy] [Hn [#times]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=2
CCCC	call gs_getmatmode(mat.matmode,mat.msdim)

	do nn=1,mat.ndim
	  call gs_getindpar(mat.ind(nn),mat.par(nn),mat.res(nn))
	  mat.step(nn)=min(mat.res(nn),DEFSTEP2D)
	end do
	call gs_checkorder(mat.ind,mat.par,mat.ndim,mat.nind)

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res,mat.step,mat.ndim)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	if(mat.ihash.GE.0) then
	  mat.hsame=.FALSE.
	  do ii=1,mat.ndim
	    if(mat.ind(ii).EQ.mat.hind) mat.hsame=.TRUE.
	  end do
	endif

	mat.type=-1

	if(mat.nind.EQ.1) then
	  call gs_countvals(mat.par(1),2,nvals)
	  if(nvals.eq.1) then
	    mat.type=1				! AaAa
	  elseif(nvals.eq.2) then
	    mat.type=2				! AaAb
	  endif

	elseif(mat.nind.EQ.2) then
	  mat.type=3				! AxBy
	endif

	if(mat.type.le.0) call gs_syntax('Do not know how to treat this combination of parameters')

	mat.ind1=mat.ind(1)
	mat.ind2=mat.ind(2)

	mat.par1=mat.par(1)
	mat.par2=mat.par(2)

	mat.hhh1=mat.ind1 .EQ. mat.hind
	mat.hhh2=mat.ind2 .EQ. mat.hind

	mat.matmode=0

	return

	end


	subroutine gs1_sort2d(mat)
	entry      gs1_sort3d(mat)
	entry      gs1_sort4d(mat)

#include "gsort.inc"
	record/matrix/mat

	matndim=mat.ndim
	do nn=1,matndim
	  call gs_putindpar(mat.ind(nn),mat.par(nn))
	  llist=llist+2
	end do

	lfile=max(1,lengthc(mat.file))
	write(list(llist:),'(1x,a)') mat.file(1:lfile)

	llist=lengthc(list)+3
	call gs_putres(mat.res,matndim)

	llist=llist+3
	call gs_putstep(mat.step,matndim)

	llist=llist+3
	call gs_puthash(mat.nhash,mat.ihash)

CCCC	llist=llist+3
CCCC	write(list(llist:),'(''mm'',2i4)') mat.matmode,mat.msdim

	return

	end

	subroutine gs2_sort2d(mat)
	entry      gs2_sort3d(mat)
	entry      gs2_sort4d(mat)
	entry      gs2_sort2d_symm(mat)
	entry      gs2_sort3d_symm(mat)
	entry      gs2_sort4d_symm(mat)
	entry      gs2_sort2d_hsymm(mat)
	entry      gs2_sort3d_hsymm(mat)
	entry      gs2_sort4d_hsymm(mat)

#include "gsort.inc"
	record/matrix/mat

	if(init_mode.EQ.0) then
	  call gs_matopen(mat)
	endif

	return

	end

	subroutine gs3_sort2d(mat)

#include "gsort.inc"
	record/matrix/mat

	integer incr(2)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue.LT.nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	goto (10,20,30) mat.type

10	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l-1					! AaAa
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	    incr(2)=det(j2).ival(mat.par1)
*	    write(*,*)det(j1).id, det(j2).id
*	    if(det(j1).id .gt. det(j2).id)then
*	       incr(1)=det(j2).ival(mat.par1)
*	       incr(2)=det(j1).ival(mat.par1)
*	    endif
	    call cmt_2dincr(%val(mat.cmtbase),incr)
	  end do
	end do
	return

20	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l					! AaAb
	  incr(1)=det(j1).ival(mat.par1)
	    incr(2)=det(j1).ival(mat.par2)
	    call cmt_2dincr(%val(mat.cmtbase),incr)
	end do
	return

30	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBy
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    incr(2)=det(j2).ival(mat.par2)
	    call cmt_2dincr(%val(mat.cmtbase),incr)
	  end do
	end do
	return

1000	goto(1010,1020,1030) mat.type

1010	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l-1					! AaAa
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1011
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1012
	    incr(2)=det(j2).ival(mat.par1)
	    call cmt_2dincr(%val(mat.cmtbase),incr)
1012	  end do
1011	end do
	return

1020	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l					! AaAb
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1021
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  call cmt_2dincr(%val(mat.cmtbase),incr)
1021	end do
	return

1030	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBy
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1031
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1031
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    call cmt_2dincr(%val(mat.cmtbase),incr)
1032	  end do
1031	end do

8000	return

	end

	subroutine gs4_sort2d(mat)
	entry      gs4_sort3d(mat)
	entry      gs4_sort4d(mat)
	entry      gs4_sort2d_symm(mat)
	entry      gs4_sort3d_symm(mat)
	entry      gs4_sort4d_symm(mat)
	entry      gs4_sort2d_hsymm(mat)
	entry      gs4_sort3d_hsymm(mat)
	entry      gs4_sort4d_hsymm(mat)

#include "gsort.inc"
	record/matrix/mat

CVMS	integer   n1,n2
	integer*8 n1,n2

	call cmt_infoincr(%val(mat.cmtbase),n1,n2)
	nincr(mat.ndim)=nincr(mat.ndim) + n1
	flushed=flushed + n2
	  
	if(finit_mode.EQ.2) then
	  call gs_matclose(mat)
	endif

	return

	end




	subroutine gs0_sort3d(mat)

#include "gsort.inc"
	record/matrix/mat
	
	synt(1)='SORT3D  Px Py Pz  matrixname  [RES Rx Ry Rz] [STEP Sx Sy Sz] [Hn [#times]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=3
CCCC	call gs_getmatmode(mat.matmode,mat.msdim)

	do nn=1,mat.ndim
	  call gs_getindpar(mat.ind(nn),mat.par(nn),mat.res(nn))
	  mat.step(nn)=max(1,min(mat.res(nn),DEFSTEP3D))
	end do
	call gs_checkorder(mat.ind,mat.par,mat.ndim,mat.nind)

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res,mat.step,mat.ndim)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	if(mat.ihash.GE.0) then
	  mat.hsame=.FALSE.
	  do ii=1,mat.ndim
	    if(mat.ind(ii).EQ.mat.hind) mat.hsame=.TRUE.
	  end do
	endif

	mind1=mat.ind(1)
	mind2=mat.ind(2)
	mind3=mat.ind(3)
	mat.type=-1

	if(mat.nind .EQ. 1) then
	  call gs_countvals(mat.par(1),3,nvals)
	  if(nvals.eq.1) then
	    mat.type=1					! AaAaAa
	  elseif(nvals.eq.3) then
	    mat.type=2					! AaAbAc
	  endif

	elseif(mat.nind.EQ.2) then
	  if(mind1.EQ.mind2) then
	    call gs_countvals(mat.par(1),2,nvals)
	    if(nvals.eq.1) then
	      mat.type=3				! AaAaBx
	    elseif(nvals.eq.2) then
	      mat.type=4				! AaAbBx
	    endif
	  elseif(mind2.EQ.mind3) then
	    call gs_countvals(mat.par(2),2,nvals)
	    if(nvals.eq.1) then
	      mat.type=5				! AxBaBa
	    elseif(nvals.eq.2) then
	      mat.type=6				! AxBaBb
	    endif
	  endif

	elseif(mat.nind.EQ.3) then
	  mat.type=7					! AxByCz
	endif

	if(mat.type.le.0) call gs_syntax('Do not know how to treat this combination of parameters')

	mat.ind1=mat.ind(1)
	mat.ind2=mat.ind(2)
	mat.ind3=mat.ind(3)

	mat.par1=mat.par(1)
	mat.par2=mat.par(2)
	mat.par3=mat.par(3)

	mat.hhh1=mat.ind1 .EQ. mat.hind
	mat.hhh2=mat.ind2 .EQ. mat.hind
	mat.hhh3=mat.ind3 .EQ. mat.hind
	
	mat.matmode=0

	return

	end

	subroutine gs3_sort3d(mat)

#include "gsort.inc"
	record/matrix/mat

	integer incr(3)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue.LT.nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	goto(10,20,30,40,50,60,70) mat.type

10	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l-2					! AaAaAa
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l-1
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j2+1,j1l
	      incr(3)=det(j3).ival(mat.par1)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    end do
	  end do
	end do
	return

20	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l					! AaAbAc
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  incr(3)=det(j1).ival(mat.par3)
	  call cmt_3dincr(%val(mat.cmtbase),incr)
	end do
	return

30	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l-1					! AaAaBx
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j3f,j3l
	      incr(3)=det(j3).ival(mat.par3)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    end do
	  end do
	end do
	return

40	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AaAbBx
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  do j3=j3f,j3l
	    incr(3)=det(j3).ival(mat.par3)
	    call cmt_3dincr(%val(mat.cmtbase),incr)
	  end do
	end do
	return

50	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBaBa
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l-1
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j2+1,j2l
	      incr(3)=det(j3).ival(mat.par2)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    end do
	  end do
	end do
	return

60	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBaBb
	  incr(1)=det(j1).ival(mat.par1)
	    do j2=j2f,j2l
	      incr(2)=det(j2).ival(mat.par2)
	      incr(3)=det(j2).ival(mat.par3)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    end do
	end do
	return

70	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AxByCz
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    incr(2)=det(j2).ival(mat.par2)
	      do j3=j3f,j3l
	        incr(3)=det(j3).ival(mat.par3)
	        call cmt_3dincr(%val(mat.cmtbase),incr)
	      end do
	  end do
	end do
	return

1000	goto(1010,1020,1030,1040,1050,1060,1070) mat.type

1010	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l-2					! AaAaAa
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1011
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1f-1
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1012
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j2+1,j1l
	        nn3=nn2
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1013
	      incr(3)=det(j3).ival(mat.par1)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
1013	    end do
1012	  end do
1011	end do
	return

1020	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l					! AaAbAc
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1021
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  incr(3)=det(j1).ival(mat.par3)
	  call cmt_3dincr(%val(mat.cmtbase),incr)
1021	end do
	return

1030	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l-1					! AaAaBx
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1031
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	    if(mat.hhh1) then
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1032
	    endif
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j3f,j3l
	      if(mat.hhh3) then
	        nn3=hntrue
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1033
	      endif
	      incr(3)=det(j3).ival(mat.par3)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
1033	    end do
1032	  end do
1031	end do
	return

1040	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AaAbBx
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1041
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  do j3=j3f,j3l
	    if(mat.hhh3) then
	      nn3=hntrue
	      if(hltrue(j3)) nn3=nn3-1
	      if(nn3.LT.nhash) goto 1043
	    endif
	    incr(3)=det(j3).ival(mat.par3)
	    call cmt_3dincr(%val(mat.cmtbase),incr)
1043	  end do
1041	end do
	return

1050	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBaBa
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1051
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l-1
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1052
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j2+1,j2l
	      if(mat.hhh2) then
	        nn3=nn2
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1053
	      endif
	      incr(3)=det(j3).ival(mat.par2)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
1053	    end do
1052	  end do
1051	end do
	return

1060	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBaBb
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1061
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1062
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    incr(3)=det(j2).ival(mat.par3)
	    call cmt_3dincr(%val(mat.cmtbase),incr)
1062	  end do
1061	end do
	return

1070	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AxByCz
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1071
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1072
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j3f,j3l
	      if(mat.hhh3) then
	        nn3=hntrue
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1073
	      endif
	      incr(3)=det(j3).ival(mat.par3)
	      call cmt_3dincr(%val(mat.cmtbase),incr)
1073	     end do
1072	  end do
1071	end do

8000	return

	end




	subroutine gs0_sort4d(mat)

#include "gsort.inc"
	record/matrix/mat
	
	synt(1)='SORT4D  Px Py Pz Pt  matrixname  [RES Rx Ry Rz Rt] [STEP Sx Sy Sz St] [Hn [#times]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=4
CCCC	call gs_getmatmode(mat.matmode,mat.msdim)

	do nn=1,mat.ndim
	  call gs_getindpar(mat.ind(nn),mat.par(nn),mat.res(nn))
	  mat.step(nn)=max(1,min(mat.res(nn),DEFSTEP4D))
	end do
	call gs_checkorder(mat.ind,mat.par,mat.ndim,mat.nind)

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res,mat.step,mat.ndim)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	if(mat.ihash.GE.0) then
	  mat.hsame=.FALSE.
	  do ii=1,mat.ndim
	    if(mat.ind(ii).EQ.mat.hind) mat.hsame=.TRUE.
	  end do
	endif

	mind1=mat.ind(1)
	mind2=mat.ind(2)
	mind3=mat.ind(3)
	mind4=mat.ind(4)
	mat.type=-1

	if(mat.nind .EQ. 1) then
	  call gs_countvals(mat.par(1),4,nvals)
	  if(nvals.eq.1) then
	    mat.type=1					! AaAaAaAa
	  elseif(nvals.eq.2) then
	    mat.type=2					! AaAaAbAb
	  elseif(nvals.eq.4) then
	    mat.type=3					! AaAbAcAd
	  endif

	elseif(mat.nind.EQ.2) then
	  if(mind1.EQ.mind2 .AND. mind1.eq.mind3) then
	    call gs_countvals(mat.par(1),3,nvals)
	    if(nvals.eq.1) then
	      mat.type=4				! AaAaAaBx
	    elseif(nvals.eq.3) then
	      mat.type=5				! AaAbAcBx
	    endif
	  elseif(mind2.EQ.mind3 .AND. mind2.eq.mind4) then
	    call gs_countvals(mat.par(2),3,nvals)
	    if(nvals.eq.1) then
	      mat.type=6				! AxBaBaBa
	    elseif(nvals.eq.3) then
	      mat.type=7				! AxBaBbBc
	    endif
	  elseif(mind1.EQ.mind2 .AND. mind3.eq.mind4) then
	    call gs_countvals(mat.par(1),2,nvala)
	    call gs_countvals(mat.par(3),2,nvalb)
	    if(nvala.eq.1 .AND. nvalb.eq.1) then
	      mat.type=8				! AaAaBbBb
	    elseif(nvala.eq.1 .AND. nvalb.eq.2) then
	      mat.type=9				! AaAaBbBc
	    elseif(nvala.eq.2 .AND. nvalb.eq.1) then
	      mat.type=10				! AaAbBcBc
	    elseif(nvala.eq.2 .AND. nvalb.eq.2) then
	      mat.type=11				! AaAbBcBd
	    endif
	  endif

	elseif(mat.nind.EQ.3) then
	  if(mind1.eq.mind2) then
	    call gs_countvals(mat.par(1),2,nvals)
	    if(nvals.eq.1) then
	      mat.type=12				! AaAaBxCy
	    elseif(nvals.eq.2) then
	      mat.type=13				! AaAbBxCy
	    endif
	  elseif(mind2.eq.mind3) then
	    call gs_countvals(mat.par(2),2,nvals)
	    if(nvals.eq.1) then
	      mat.type=14				! AxBaBaCy
	    elseif(nvals.eq.2) then
	      mat.type=15				! AxBaBbCy
	    endif
	  elseif(mind3.eq.mind4) then
	    call gs_countvals(mat.par(3),2,nvals)
	    if(nvals.eq.1) then
	      mat.type=16				! AxByCaCa
	    elseif(nvals.eq.2) then
	      mat.type=17				! AxByCaCb
	    endif
	  endif

	elseif(mat.nind.EQ.4) then
	  mat.type=18					! AxByCzDt
	endif

	if(mat.type.le.0) call gs_syntax('Do not know how to treat this combination of parameters')
	
	mat.ind1=mat.ind(1)
	mat.ind2=mat.ind(2)
	mat.ind3=mat.ind(3)
	mat.ind4=mat.ind(4)

	mat.par1=mat.par(1)
	mat.par2=mat.par(2)
	mat.par3=mat.par(3)
	mat.par4=mat.par(4)

	mat.hhh1=mat.ind1 .EQ. mat.hind
	mat.hhh2=mat.ind2 .EQ. mat.hind
	mat.hhh3=mat.ind3 .EQ. mat.hind
	mat.hhh4=mat.ind4 .EQ. mat.hind

	mat.matmode=0

	return

	end

	subroutine gs3_sort4d(mat)

#include "gsort.inc"
	record/matrix/mat

	integer incr(4)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue.LT.nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	goto(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180) mat.type

10	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l-3					! AaAaAaAa
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l-2
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j2+1,j1l-1
	      incr(3)=det(j3).ival(mat.par1)
	      do j4=j3+1,j1l
	        incr(4)=det(j4).ival(mat.par1)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
	      end do
	    end do
	  end do
	end do
	return

20	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l-1					! AaAaAbAb
	  incr(1)=det(j1).ival(mat.par1)
	  incr(3)=det(j1).ival(mat.par3)
	  do j2=j1+1,j1l
	    incr(2)=det(j2).ival(mat.par1)
	    incr(4)=det(j2).ival(mat.par3)
	    call cmt_4dincr(%val(mat.cmtbase),incr)
	  end do
	end do
	return

30	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l					! AaAbAcAd
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  incr(3)=det(j1).ival(mat.par3)
	  incr(4)=det(j1).ival(mat.par4)
	  call cmt_4dincr(%val(mat.cmtbase),incr)
	end do
	return

40	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l-2					! AaAaAaBx
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l-1
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j2+1,j1l
	      incr(3)=det(j3).ival(mat.par1)
	      do j4=j4f,j4l
	        incr(4)=det(j4).ival(mat.par4)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
	      end do
	    end do
	  end do
	end do
	return

50	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l					! AaAbAcBx
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  incr(3)=det(j1).ival(mat.par3)
	  do j4=j4f,j4l
	    incr(4)=det(j4).ival(mat.par4)
	    call cmt_4dincr(%val(mat.cmtbase),incr)
	  end do
	end do
	return

60	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBaBaBa
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l-2
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j2+1,j2l-1
	      incr(3)=det(j3).ival(mat.par2)
	      do j4=j3+1,j2l
	        incr(4)=det(j4).ival(mat.par2)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
	      end do
	    end do
	  end do
	end do
	return

70	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBaBbBc
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    incr(2)=det(j2).ival(mat.par2)
	    incr(3)=det(j2).ival(mat.par3)
	    incr(4)=det(j2).ival(mat.par4)
	    call cmt_4dincr(%val(mat.cmtbase),incr)
	  end do
	end do
	return

80	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l-1					! AaAaBbBb
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j3f,j3l-1
	      incr(3)=det(j3).ival(mat.par3)
	      do j4=j3+1,j3l
	        incr(4)=det(j4).ival(mat.par3)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
	      end do
	    end do
	  end do
	end do
	return

90	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l-1					! AaAaBbBc
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j3f,j3l
	      incr(3)=det(j3).ival(mat.par3)
	      incr(4)=det(j3).ival(mat.par4)
	      call cmt_4dincr(%val(mat.cmtbase),incr)
	    end do
	  end do
	end do
	return

100	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AaAbBcBc
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  do j3=j3f,j3l-1
	    incr(3)=det(j3).ival(mat.par3)
	    do j4=j3+1,j3l
	      incr(4)=det(j4).ival(mat.par3)
	      call cmt_4dincr(%val(mat.cmtbase),incr)
	    end do
	  end do
	end do
	return

110	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AaAbBcBd
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  do j3=j3f,j3l
	    incr(3)=det(j3).ival(mat.par3)
	    incr(4)=det(j3).ival(mat.par4)
	    call cmt_4dincr(%val(mat.cmtbase),incr)
	  end do
	end do
	return

120	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l-1					! AaAaBxCy
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j3f,j3l
	      incr(3)=det(j3).ival(mat.par3)
	      do j4=j4f,j4l
	        incr(4)=det(j4).ival(mat.par4)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
	      end do
	    end do
	  end do
	end do
	return

130	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l					! AaAbBxCy
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  do j3=j3f,j3l
	    incr(3)=det(j3).ival(mat.par3)
	    do j4=j4f,j4l
	      incr(4)=det(j4).ival(mat.par4)
	      call cmt_4dincr(%val(mat.cmtbase),incr)
	    end do
	  end do
	end do
	return

140	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l					! AxBaBaCy
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l-1
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j2+1,j2l
	      incr(3)=det(j3).ival(mat.par2)
	      do j4=j4f,j4l
	        incr(4)=det(j4).ival(mat.par4)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
	      end do
	    end do
	  end do
	end do
	return

150	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l					! AxBaBbCy
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    incr(2)=det(j2).ival(mat.par2)
	    incr(3)=det(j2).ival(mat.par3)
	    do j4=j4f,j4l
	      incr(4)=det(j4).ival(mat.par4)
	      call cmt_4dincr(%val(mat.cmtbase),incr)
	    end do
	  end do
	end do
	return

160	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AxByCaCa
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j3f,j3l-1
	      incr(3)=det(j3).ival(mat.par3)
	      do j4=j3+1,j3l
	        incr(4)=det(j4).ival(mat.par3)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
	      end do
	    end do
	  end do
	end do
	return

170	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AxByCaCb
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j3f,j3l
	      incr(3)=det(j3).ival(mat.par3)
	      incr(4)=det(j3).ival(mat.par4)
	      call cmt_4dincr(%val(mat.cmtbase),incr)
	    end do
	  end do
	end do
	return

180	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l					! AxByCzDt
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j3f,j3l
	      incr(3)=det(j3).ival(mat.par3)
	      do j4=j4f,j4l
	        incr(4)=det(j4).ival(mat.par4)
	        call cmt_3dincr(%val(mat.cmtbase),incr)
	      end do
	    end do
	  end do
	end do
	return

1000	goto(1010,1020,1030,1040,1050,1060,1070,1080,1090,1100,1110,1110,1120,1130,1140,1150,1160,1170,1180) mat.type

1010	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l-3					! AaAaAaAa
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1011
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l-2
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1012
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j2+1,j1l-1
	        nn3=nn2
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1013
	      incr(3)=det(j3).ival(mat.par1)
	      do j4=j3+1,j1l
	          nn4=nn3
	          if(hltrue(j4)) nn4=nn4-1
	          if(nn4.LT.nhash) goto 1014
	        incr(4)=det(j4).ival(mat.par1)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
1014	      end do
1013	    end do
1012	  end do
1011	end do
	return

1020	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l-1					! AaAaAbAb
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1021
	  incr(1)=det(j1).ival(mat.par1)
	  incr(3)=det(j1).ival(mat.par3)
	  do j2=j1+1,j1l
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1022
	    incr(2)=det(j2).ival(mat.par1)
	    incr(4)=det(j2).ival(mat.par4)
	    call cmt_4dincr(%val(mat.cmtbase),incr)
1022	  end do
1021	end do
	return

1030	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	do j1=j1f,j1l					! AaAbAcAd
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1031
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  incr(3)=det(j1).ival(mat.par3)
	  incr(4)=det(j1).ival(mat.par4)
	  call cmt_4dincr(%val(mat.cmtbase),incr)
1031	end do
	return

1040	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l-2					! AaAaAaBx
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1041
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l-1
	    if(mat.hhh1) then
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1042
	    endif
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j2+1,j1l
	      if(mat.hhh1) then
	        nn3=nn2
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1043
	      endif
	      incr(3)=det(j3).ival(mat.par1)
	      do j4=j4f,j4l
	        if(mat.hhh4) then
	          nn4=hntrue
	          if(hltrue(j4)) nn4=nn4-1
	          if(nn4.LT.nhash) goto 1044
	        endif
	        incr(4)=det(j4).ival(mat.par4)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
1044	      end do
1043	    end do
1042	  end do
1041	end do
	return

1050	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l					! AaAbAcBx
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1051
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  incr(3)=det(j1).ival(mat.par3)
	  do j4=j4f,j4l
	    if(mat.hhh4) then
	      nn4=hntrue
	      if(hltrue(j4)) nn4=nn4-1
	      if(nn4.LT.nhash) goto 1054
	    endif
	    incr(4)=det(j4).ival(mat.par4)
	    call cmt_4dincr(%val(mat.cmtbase),incr)
1054	  end do
1051	end do
	return

1060	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBaBaBa
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1061
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l-2
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1062
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j2+1,j2l-1
	      if(mat.hhh2) then
	        nn3=nn2
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1063
	      endif
	      incr(3)=det(j3).ival(mat.par2)
	      do j4=j3+1,j2l
	        if(mat.hhh2) then
	          nn4=nn3
	          if(hltrue(j4)) nn4=nn4-1
	          if(nn4.LT.nhash) goto 1064
	        endif
	        incr(4)=det(j4).ival(mat.par2)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
1064	      end do
1063	    end do
1062	  end do
1061	end do
	return

1070	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	do j1=j1f,j1l					! AxBaBbBc
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1071
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1072
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    incr(3)=det(j2).ival(mat.par3)
	    incr(4)=det(j2).ival(mat.par4)
	    call cmt_4dincr(%val(mat.cmtbase),incr)
1072	  end do
1071	end do
	return

1080	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l-1					! AaAaBbBb
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1081
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	    if(mat.hhh1) then
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1082
	    endif
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j3f,j3l-1
	      if(mat.hhh3) then
	        nn3=hntrue
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1083
	      endif
	      incr(3)=det(j3).ival(mat.par3)
	      do j4=j3+1,j3l
	        if(mat.hhh3) then
	          nn4=nn3
	          if(hltrue(j4)) nn4=nn4-1
	          if(nn4.LT.nhash) goto 1084
	        endif
	        incr(4)=det(j4).ival(mat.par3)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
1084	      end do
1083	    end do
1082	  end do
1081	end do
	return

1090	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l-1					! AaAaBbBc
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1091
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	    if(mat.hhh1) then
	      nn2=nn2
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1092
	    endif
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j3f,j3l
	      if(mat.hhh3) then
	        nn3=hntrue
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1093
	      endif
	      incr(3)=det(j3).ival(mat.par3)
	      incr(4)=det(j3).ival(mat.par4)
	      call cmt_4dincr(%val(mat.cmtbase),incr)
1093	    end do
1092	  end do
1091	end do
	return

1100	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AaAbBcBc
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1101
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  do j3=j3f,j3l-1
	    if(mat.hhh3) then
	      nn3=hntrue
	      if(hltrue(j3)) nn3=nn3-1
	      if(nn3.LT.nhash) goto 1103
	    endif
	    incr(3)=det(j3).ival(mat.par3)
	    do j4=j3+1,j3l
	      if(mat.hhh3) then
	        nn4=nn3
	        if(hltrue(j4)) nn4=nn4-1
	        if(nn4.LT.nhash) goto 1104
	      endif
	      incr(4)=det(j4).ival(mat.par3)
	      call cmt_4dincr(%val(mat.cmtbase),incr)
1104	    end do
1103	  end do
1101	end do
	return


1110	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AaAbBcBd
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1111
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  do j3=j3f,j3l
	    if(mat.hhh3) then
	      nn3=hntrue
	      if(hltrue(j3)) nn3=nn3-1
	      if(nn3.LT.nhash) goto 1113
	    endif
	    incr(3)=det(j3).ival(mat.par3)
	    incr(4)=det(j3).ival(mat.par4)
	    call cmt_4dincr(%val(mat.cmtbase),incr)
1113	  end do
1111	end do
	return

1120	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l-1					! AaAaBxCy
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1121
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	    if(mat.hhh1) then
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1122
	    endif
	    incr(2)=det(j2).ival(mat.par1)
	    do j3=j3f,j3l
	      if(mat.hhh3) then
	        nn3=hntrue
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1123
	      endif
	      incr(3)=det(j3).ival(mat.par3)
	      do j4=j4f,j4l
	        if(mat.hhh4) then
	          nn4=hntrue
	          if(hltrue(j4)) nn4=nn4-1
	          if(nn4.LT.nhash) goto 1124
	        endif
	        incr(4)=det(j4).ival(mat.par4)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
1124	      end do
1123	    end do
1122	  end do
1121	end do
	return

1130	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l					! AaAbBxCy
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1131
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  incr(2)=det(j1).ival(mat.par2)
	  do j3=j3f,j3l
	    if(mat.hhh3) then
	      nn3=hntrue
	      if(hltrue(j3)) nn3=nn3-1
	      if(nn3.LT.nhash) goto 1133
	    endif
	    incr(3)=det(j3).ival(mat.par3)
	    do j4=j4f,j4l
	      if(mat.hhh4) then
	        nn4=hntrue
	        if(hltrue(j4)) nn4=nn4-1
	        if(nn4.LT.nhash) goto 1134
	      endif
	      incr(4)=det(j4).ival(mat.par4)
	      call cmt_4dincr(%val(mat.cmtbase),incr)
1134	    end do
1133	  end do
1131	end do
	return

1140	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l					! AxBaBaCy
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1141
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l-1
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn1.LT.nhash) goto 1142
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j2+1,j2l
	      if(mat.hhh2) then
	        nn3=nn2
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1143
	      endif
	      incr(3)=det(j3).ival(mat.par2)
	      do j4=j4f,j4l
	        if(mat.hhh4) then
	          nn4=hntrue
	          if(hltrue(j4)) nn4=nn4-1
	          if(nn4.LT.nhash) goto 1144
	        endif
	        incr(4)=det(j4).ival(mat.par4)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
1144	      end do
1143	    end do
1142	  end do
1141	end do
	return

1150	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l					! AxBaBbCy
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1151
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn1.LT.nhash) goto 1152
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    incr(3)=det(j3).ival(mat.par2)
	    do j4=j4f,j4l
	      if(mat.hhh4) then
	        nn4=hntrue
	        if(hltrue(j4)) nn4=nn4-1
	        if(nn4.LT.nhash) goto 1154
	      endif
	      incr(4)=det(j4).ival(mat.par4)
	      call cmt_4dincr(%val(mat.cmtbase),incr)
1154	    end do
1152	  end do
1151	end do
	return

1160	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j3f=DOFF(mat.ind4)
	j3l=DOFF(mat.ind4)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AxByCaCa
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1161
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn1.LT.nhash) goto 1162
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j3f,j3l-1
	      if(mat.hhh3) then
	        nn3=hntrue
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1163
	      endif
	      incr(3)=det(j3).ival(mat.par3)
	      do j4=j3+1,j3l
	        if(mat.hhh3) then
	          nn4=nn3
	          if(hltrue(j4)) nn4=nn4-1
	          if(nn4.LT.nhash) goto 1164
	        endif
	        incr(4)=det(j4).ival(mat.par3)
	        call cmt_4dincr(%val(mat.cmtbase),incr)
1164	      end do
1163	    end do
1162	  end do
1161	end do
	return

1170	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j3f=DOFF(mat.ind4)
	j3l=DOFF(mat.ind4)+NDET(mat.ind3)-1
	do j1=j1f,j1l					! AxByCaCb
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1171
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn1.LT.nhash) goto 1172
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j3f,j3l-1
	      if(mat.hhh3) then
	        nn3=hntrue
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1173
	      endif
	      incr(3)=det(j3).ival(mat.par3)
	      incr(4)=det(j3).ival(mat.par4)
	      call cmt_4dincr(%val(mat.cmtbase),incr)
1173	    end do
1172	  end do
1171	end do
	return

1180	j1f=DOFF(mat.ind1)
	j1l=DOFF(mat.ind1)+NDET(mat.ind1)-1
	j2f=DOFF(mat.ind2)
	j2l=DOFF(mat.ind2)+NDET(mat.ind2)-1
	j3f=DOFF(mat.ind3)
	j3l=DOFF(mat.ind3)+NDET(mat.ind3)-1
	j4f=DOFF(mat.ind4)
	j4l=DOFF(mat.ind4)+NDET(mat.ind4)-1
	do j1=j1f,j1l					! AxByCzDt
	  if(mat.hhh1) then
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 1181
	  endif
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j2f,j2l
	    if(mat.hhh2) then
	      nn2=hntrue
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 1182
	    endif
	    incr(2)=det(j2).ival(mat.par2)
	    do j3=j3f,j3l
	      if(mat.hhh3) then
	        nn3=hntrue
	        if(hltrue(j3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 1183
	      endif
	      incr(3)=det(j3).ival(mat.par3)
	      do j4=j4f,j4l
	        if(mat.hhh4) then
	          nn4=hntrue
	          if(hltrue(j4)) nn4=nn4-1
	          if(nn4.LT.nhash) goto 1184
	        endif
	        incr(4)=det(j4).ival(mat.par4)
	        call cmt_3dincr(%val(mat.cmtbase),incr)
1184	      end do
1183	    end do
1182	  end do
1181	end do
	return

8000	return

	end



	subroutine gs0_sort2d_symm(mat)

#include "gsort.inc"
	record/matrix/mat

	synt(1)='SORT2D_SYMM Px  matrixname   [RES R] [STEP S] [Hn [#times]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=2

	call gs_getindpar(mat.ind(1),mat.par(1),mat.res(1))
	mat.step(1)=min(mat.res(1),DEFSTEP2DS)

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res,mat.step,1)
	mat.res(2)=mat.res(1)
	mat.step(2)=mat.step(1)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	if(mat.ihash.GE.0) then
	  mat.hsame=.FALSE.
	  if(mat.ind(1).EQ.mat.hind) mat.hsame=.TRUE.
	endif

	mat.matmode=1

	return

	end

	subroutine gs1_sort2d_symm(mat)
	entry      gs1_sort3d_symm(mat)
	entry      gs1_sort4d_symm(mat)

#include "gsort.inc"
	record/matrix/mat

	call gs_putindpar(mat.ind(1),mat.par(1))

	lmatfile=max(1,lengthc(mat.file))
	write(list(llist+2:),'(a)') mat.file(1:lmatfile)

	llist=lengthc(list)+3
	call gs_putres(mat.res,1)

	llist=llist+3
	call gs_putstep(mat.step,1)

	llist=llist+3
	call gs_puthash(mat.nhash,mat.ihash)

	return

	end

	subroutine gs3_sort2d_symm(mat)

#include "gsort.inc"
	record/matrix/mat

	integer incr(2)

	ji=mat.ind(1)
	if(NDET(ji).LT.2) goto 8000
	jp=mat.par(1)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue .LT. nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	jgf=doff(ji)
	jgl=doff(ji)+ndet(ji)-1
	ires=mat.res(1)
	do jg1=jgf,jgl-1
	  ican=det(jg1).ival(jp)
	  if(ican.le.0 .or. ican.ge.ires) goto 10
	  incr(1)=ican
	  do jg2=jg1+1,jgl
	    ican=det(jg2).ival(jp)
	    if(ican.le.0 .or. ican.ge.ires) goto 20
	    incr(2)=ican
	    call cmt_s2dincr(%val(mat.cmtbase),incr)
20	  end do
10	end do
	return

1000	jgf=doff(ji)
	jgl=doff(ji)+ndet(ji)-1
	ires=mat.res(1)
	do jg1=jgf,jgl-1
	  ican=det(jg1).ival(jp)
	  if(ican.le.0 .or. ican.ge.ires) goto 100
	    nn1=hntrue
	    if(hltrue(jg1)) nn1=nn1-1
	    if(nn1.lt.nhash) goto 100
	  incr(1)=ican
	  do jg2=jg1+1,jgl
	    ican=det(jg2).ival(jp)
	    if(ican.le.0 .or. ican.ge.ires) goto 200
	      nn2=nn1
	      if(hltrue(jg2)) nn2=nn2-1
	      if(nn2.lt.nhash) goto 200
	    incr(2)=ican
	    call cmt_s2dincr(%val(mat.cmtbase),incr)
200	  end do
100	end do

8000	return

	end



	subroutine gs0_sort3d_symm(mat)

#include "gsort.inc"
	record/matrix/mat

	synt(1)='SORT3D_SYMM  Px  matrixname  [RES R] [STEP S] [Hn [#times]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=3

	call gs_getindpar(mat.ind(1),mat.par(1),mat.res(1))
	mat.step(1)=min(mat.res(1),DEFSTEP3D)

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res,mat.step,1)
	mat.res(2)=mat.res(1)
	mat.res(3)=mat.res(1)
	mat.step(2)=mat.step(1)
	mat.step(3)=mat.step(1)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	if(mat.ihash.GE.0)then
	  mat.hsame=.FALSE.
	  if(mat.ind(1).EQ.mat.hind) mat.hsame=.TRUE.
	endif

	mat.matmode=1

	return

	end

	subroutine gs3_sort3d_symm(mat)

#include "gsort.inc"
	record/matrix/mat

	integer incr(3)

	ji=mat.ind(1)
	if(NDET(ji).LT.3) goto 8000
	jp=mat.par(1)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue .LT. nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	jgf=DOFF(ji)
	jgl=DOFF(ji)+NDET(ji)-1
	ires=mat.res(1)
	do jg1=jgf,jgl-2
	  ICAN=det(jg1).ival(jp)
	  if(ican.LE.0 .or. ican.GE.ires) goto 10
	  INCR(1)=ican
	  do jg2=jg1+1,jgl-1
	    ICAN=det(jg2).ival(jp)
	    if(ican.LE.0 .or. ican.GE.ires) goto 20
	    INCR(2)=ican
	    do jg3=jg2+1,jgl
	      ICAN=det(jg3).ival(jp)
	      if(ican.LE.0 .or. ican.GE.ires) goto 30
	      INCR(3)=ican
	      CALL cmt_s3dincr(%val(mat.cmtbase),incr)
30	    end do
20	  end do
10	end do
	return

1000	jgf=DOFF(ji)
	jgl=DOFF(ji)+NDET(ji)-1
	ires=mat.res(1)
	do jg1=jgf,jgl-2
	  ICAN=det(jg1).ival(jp)
	  if(ican.LE.0 .or. ican.GE.ires) goto 100
	    nn1=hntrue
	    if(hltrue(jg1)) nn1=nn1-1
	    if(nn1.LT.nhash) goto 100
	  INCR(1)=ican
	  do jg2=jg1+1,jgl-1
	    ICAN=det(jg2).ival(jp)
	    if(ican.LE.0 .or. ican.GE.ires) goto 200
	      nn2=nn1
	      if(hltrue(jg2)) nn2=nn2-1
	      if(nn2.LT.nhash) goto 200
	    INCR(2)=ican
	    do jg3=jg2+1,jgl
	      ICAN=det(jg3).ival(jp)
	      if(ican.LE.0 .or. ican.GE.ires) goto 300
	        nn3=nn2
	        if(hltrue(jg3)) nn3=nn3-1
	        if(nn3.LT.nhash) goto 300
	      INCR(3)=ican
	      CALL cmt_s3dincr(%val(mat.cmtbase),incr)
300	    end do
200	  end do
100	end do

8000	return

	end




	subroutine gs0_sort4d_symm(mat)

#include "gsort.inc"
	record/matrix/mat

	synt(1)='SORT4D_SYMM  Px  matrixname  [RES R] [STEP S] [Hn [#times]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=4

	call gs_getindpar(mat.ind(1),mat.par(1),mat.res(1))
	mat.step(1)=min(mat.res(1),DEFSTEP4D)

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res,mat.step,1)
	mat.res(2)=mat.res(1)
	mat.res(3)=mat.res(1)
	mat.res(4)=mat.res(1)
	mat.step(2)=mat.step(1)
	mat.step(3)=mat.step(1)
	mat.step(4)=mat.step(1)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	if(mat.ihash.GE.0)then
	  mat.hsame=.FALSE.
	  if(mat.ind(1).EQ.mat.hind) mat.hsame=.TRUE.
	endif

	mat.matmode=1

	return

	end

	subroutine gs3_sort4d_symm(mat)

#include "gsort.inc"
	record/matrix/mat

	integer incr(4)

	ji=mat.ind(1)
	if(NDET(ji).LT.4) goto 8000
	jp=mat.par(1)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue .LT. nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	jgf=doff(ji)
	jgl=doff(ji)+ndet(ji)-1
	ires=mat.res(1)
	do jg1=jgf,jgl-3
	  ican=det(jg1).ival(jp)
	  if(ican.le.0 .or. ican.ge.ires) goto 10
	  incr(1)=ican
	  do jg2=jg1+1,jgl-2
	    ican=det(jg2).ival(jp)
	    if(ican.le.0 .or. ican.ge.ires) goto 20
	    incr(2)=ican
	    do jg3=jg2+1,jgl-1
	      ican=det(jg3).ival(jp)
	      if(ican.le.0 .or. ican.ge.ires) goto 30
	      incr(3)=ican
	      do jg4=jg3+1,jgl
	        ican=det(jg4).ival(jp)
	        if(ican.le.0 .or. ican.ge.ires) goto 30
	        incr(4)=ican
	        call cmt_s4dincr(%val(mat.cmtbase),incr)
40	      end do
30	    end do
20	  end do
10	end do
	return

1000	jgf=doff(ji)
	jgl=doff(ji)+ndet(ji)-1
	ires=mat.res(1)
	do jg1=jgf,jgl-3
	  ican=det(jg1).ival(jp)
	  if(ican.le.0 .or. ican.ge.ires) goto 100
	    nn1=hntrue
	    if(hltrue(jg1)) nn1=nn1-1
	    if(nn1.lt.nhash) goto 100
	  incr(1)=ican
	  do jg2=jg1+1,jgl-2
	    ican=det(jg2).ival(jp)
	    if(ican.le.0 .or. ican.ge.ires) goto 200
	      nn2=nn1
	      if(hltrue(jg2)) nn2=nn2-1
	      if(nn2.lt.nhash) goto 200
	    incr(2)=ican
	    do jg3=jg2+1,jgl-1
	      ican=det(jg3).ival(jp)
	      if(ican.le.0 .or. ican.ge.ires) goto 300
	        nn3=nn2
	        if(hltrue(jg3)) nn3=nn3-1
	        if(nn3.lt.nhash) goto 300
	      incr(3)=ican
	      do jg4=jg3+1,jgl
	        ican=det(jg4).ival(jp)
	        if(ican.le.0 .or. ican.ge.ires) goto 400
	          nn4=nn3
	          if(hltrue(jg4)) nn4=nn4-1
	          if(nn4.lt.nhash) goto 300
	        incr(4)=ican
	        call cmt_s4dincr(%val(mat.cmtbase),incr)
400	      end do
300	    end do
200	  end do
100	end do

8000	return

	end




	subroutine gs0_sort2d_hsymm(mat)

#include "gsort.inc"
	record/matrix/mat

	synt(1)='SORT2D_HSYMM  Px  matrixname   [RES R] [STEP S] [Hn [#times]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=2

	call gs_getindpar(mat.ind(1),mat.par(1),mat.res(1))
	mat.step(2)=min(mat.res(1),DEFSTEP2DH)

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res(1),mat.step(2),1)
	mat.step(1)=mat.res(1)
	mat.res(2)=mat.res(1)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	if(mat.ihash.GE.0) then
	  mat.hsame=.FALSE.
	  if(mat.ind(1).EQ.mat.hind) mat.hsame=.TRUE.
	endif

	mat.matmode=2

	return

	end

	subroutine gs1_sort2d_hsymm(mat)
	entry      gs1_sort3d_hsymm(mat)
	entry      gs1_sort4d_hsymm(mat)

#include "gsort.inc"
	record/matrix/mat

	call gs_putindpar(mat.ind(1),mat.par(1))

	lmatfile=max(1,lengthc(mat.file))
	write(list(llist+2:),'(a)') mat.file(1:lmatfile)

	llist=lengthc(list)+3
	call gs_putres(mat.res(2),1)

	llist=llist+3
	call gs_putstep(mat.step(2),1)

	llist=llist+3
	call gs_puthash(mat.nhash,mat.ihash)

	return

	end

	subroutine gs3_sort2d_hsymm(mat)

#include "gsort.inc"
	record/matrix/mat

	integer incr(2)

	ji=mat.ind(1)
	if(ndet(ji).LT.2) goto 8000
	jp=mat.par(1)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue .LT. nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	jgf=doff(ji)
	jgl=doff(ji)+ndet(ji)-1
	ires=mat.res(1)
	do jg1=jgf,jgl-1
	  ican=det(jg1).ival(jp)
	  if(ican.le.0 .or. ican.ge.ires) goto 10
	  incr(1)=ican
	  do jg2=jg1+1,jgl
	    ican=det(jg2).ival(jp)
	    if(ican.le.0 .or. ican.ge.ires) goto 20
	    incr(2)=ican
	    call cmt_hs2dincr(%val(mat.cmtbase),incr)
20	  end do
10	end do
	return

1000	jgf=doff(ji)
	jgl=doff(ji)+ndet(ji)-1
	ires=mat.res(1)
	do jg1=jgf,jgl-1
	  ican=det(jg1).ival(jp)
	  if(ican.le.0 .or. ican.ge.ires) goto 100
	    nn1=hntrue
	    if(hltrue(jg1)) nn1=nn1-1
	    if(nn1.lt.nhash) goto 100
	  incr(1)=ican
	  do jg2=jg1+1,jgl
	    ican=det(jg2).ival(jp)
	    if(ican.le.0 .or. ican.ge.ires) goto 200
	      nn2=nn1
	      if(hltrue(jg2)) nn2=nn2-1
	      if(nn2.lt.nhash) goto 200
	    incr(2)=ican
	    call cmt_hs2dincr(%val(mat.cmtbase),incr)
200	  end do
100	end do

8000	return

	end



	subroutine gs0_sort3d_hsymm(mat)

#include "gsort.inc"
	record/matrix/mat

	synt(1)='SORT3D_HSYMM  Px  matrixname  [RES R] [STEP S] [Hn [#times]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=3

	call gs_getindpar(mat.ind(1),mat.par(1),mat.res(1))
	mat.step(2)=min(mat.res(1),DEFSTEP3DH)

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res(1),mat.step(2),1)
	mat.step(1)=mat.res(1)
	mat.res(2)=mat.res(1)
	mat.res(3)=mat.res(1)
	mat.step(3)=mat.step(2)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	if(mat.ihash.GE.0) then
	  mat.hsame=.FALSE.
	  if(mat.ind(1).EQ.mat.hind) mat.hsame=.TRUE.
	endif
	
	mat.matmode=2

	return

	end

	subroutine gs3_sort3d_hsymm(mat)

#include "gsort.inc"
	record/matrix/mat

	integer incr(3)

	ji=mat.ind(1)
	if(NDET(ji).LT.3) goto 8000
	jp=mat.par(1)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue .LT. nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	jgf=doff(ji)
	jgl=doff(ji)+ndet(ji)-1
	ires=mat.res(1)
	do jg1=jgf,jgl-2
	  ican=det(jg1).ival(jp)
	  if(ican.le.0 .or. ican.ge.ires) goto 10
	  incr(1)=ican
	  do jg2=jg1+1,jgl-1
	    ican=det(jg2).ival(jp)
	    if(ican.le.0 .or. ican.ge.ires) goto 20
	    incr(2)=ican
	    do jg3=jg2+1,jgl
	      ican=det(jg3).ival(jp)
	      if(ican.le.0 .or. ican.ge.ires) goto 30
	      incr(3)=ican
	      call cmt_hs3dincr(%val(mat.cmtbase),incr)
30	    end do
20	  end do
10	end do
	return

1000	jgf=doff(ji)
	jgl=doff(ji)+ndet(ji)-1
	ires=mat.res(1)
	do jg1=jgf,jgl-2
	  ican=det(jg1).ival(jp)
	  if(ican.le.0 .or. ican.ge.ires) goto 100
	    nn1=hntrue
	    if(hltrue(jg1)) nn1=nn1-1
	    if(nn1.lt.nhash) goto 100
	  incr(1)=ican
	  do jg2=jg1+1,jgl-1
	    ican=det(jg2).ival(jp)
	    if(ican.le.0 .or. ican.ge.ires) goto 200
	      nn2=nn1
	      if(hltrue(jg2)) nn2=nn2-1
	      if(nn2.lt.nhash) goto 200
	    incr(2)=ican
	    do jg3=jg2+1,jgl
	      ican=det(jg3).ival(jp)
	      if(ican.le.0 .or. ican.ge.ires) goto 300
	        nn3=nn2
	        if(hltrue(jg3)) nn3=nn3-1
	        if(nn3.lt.nhash) goto 300
	      incr(3)=ican
	      call cmt_hs3dincr(%val(mat.cmtbase),incr)
300	    end do
200	  end do
100	end do

8000	return

	end




	subroutine gs0_sort4d_hsymm(mat)

#include "gsort.inc"
	record/matrix/mat

	synt(1)='SORT4D_HSYMM  Px  matrixname  [RES R] [STEP S] [Hn [#times]]'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=4

	call gs_getindpar(mat.ind(1),mat.par(1),mat.res(1))
	mat.step(2)=min(mat.res(1),DEFSTEP4DH)

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res(1),mat.step(2),1)
	mat.step(1)=mat.res(1)
	mat.res(2)=mat.res(1)
	mat.res(3)=mat.res(1)
	mat.res(4)=mat.res(1)
	mat.step(3)=mat.step(2)
	mat.step(4)=mat.step(2)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	if(mat.ihash.GE.0) then
	  mat.hsame=.FALSE.
	  if(mat.ind(1).EQ.mat.hind) mat.hsame=.TRUE.
	endif
	
	mat.matmode=2

	return

	end

	subroutine gs3_sort4d_hsymm(mat)

#include "gsort.inc"
	record/matrix/mat

	integer incr(4)

	ji=mat.ind(1)
	if(ndet(ji).LT.4) goto 8000
	jp=mat.par(1)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue .LT. nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	jgf=doff(ji)
	jgl=doff(ji)+ndet(ji)-1
	ires=mat.res(1)
	do jg1=jgf,jgl-3
	  ican=det(jg1).ival(jp)
	  if(ican.le.0 .or. ican.ge.ires) goto 10
	  incr(1)=ican
	  do jg2=jg1+1,jgl-2
	    ican=det(jg2).ival(jp)
	    if(ican.le.0 .or. ican.ge.ires) goto 20
	    incr(2)=ican
	    do jg3=jg2+1,jgl-1
	      ican=det(jg3).ival(jp)
	      if(ican.le.0 .or. ican.ge.ires) goto 30
	      incr(3)=ican
	      do jg4=jg3+1,jgl
	        ican=det(jg4).ival(jp)
	        if(ican.le.0 .or. ican.ge.ires) goto 40
	        incr(4)=ican
	        call cmt_hs4dincr(%val(mat.cmtbase),incr)
40	      end do
30	    end do
20	  end do
10	end do
	return

1000	jgf=doff(ji)
	jgl=doff(ji)+ndet(ji)-1
	ires=mat.res(1)
	do jg1=jgf,jgl-3
	  ican=det(jg1).ival(jp)
	  if(ican.le.0 .or. ican.ge.ires) goto 100
	    nn1=hntrue
	    if(hltrue(jg1)) nn1=nn1-1
	    if(nn1.lt.nhash) goto 100
	  incr(1)=ican
	  do jg2=jg1+1,jgl-2
	    ican=det(jg2).ival(jp)
	    if(ican.le.0 .or. ican.ge.ires) goto 200
	      nn2=nn1
	      if(hltrue(jg2)) nn2=nn2-1
	      if(nn2.lt.nhash) goto 200
	    incr(2)=ican
	    do jg3=jg2+1,jgl-1
	      ican=det(jg3).ival(jp)
	      if(ican.le.0 .or. ican.ge.ires) goto 300
	        nn3=nn2
	        if(hltrue(jg3)) nn3=nn3-1
	        if(nn3.lt.nhash) goto 300
	      incr(3)=ican
	      do jg4=jg3+1,jgl
	        ican=det(jg4).ival(jp)
	        if(ican.le.0 .or. ican.ge.ires) goto 400
	          nn4=nn3
	          if(hltrue(jg4)) nn4=nn4-1
	          if(nn4.lt.nhash) goto 400
	        incr(4)=ican
	        call cmt_hs4dincr(%val(mat.cmtbase),incr)
400	      end do
300	    end do
200	  end do
100	end do

8000	return

	end



	subroutine gs0_sort3d_pair(mat)

#include "gsort.inc"
	record/matrix/mat

	synt(1)='SORT3D_PAIR Dn Dn Pi matrixname [RES Rx Rx Rz] [STEP Sx Sx Sz] [Hn [#times]]'
	synt(2)='                 (i ==>  which of the PAIRDEF)'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=3

	call gs_getindpar(mat.ind(1),mat.par(1),mat.res(1))
	mat.step(1)=min(mat.res(1),DEFSTEP3D)

	call gs_getindpar(mat.ind(2),mat.par(2),mat.res(2))
	mat.step(2)=min(mat.res(2),DEFSTEP3D)

	if(mat.ind(1).NE.mat.ind(2) .OR.
	1  mat.par(1).NE.mat.par(2) ) call gs_syntax('Must be same Index and same Parameter')
	if(mat.step(1).LE.0) mat.step(1)=min(mat.res(1),DEFSTEP3D)

	call gs_getpair(mat.par(3),mat.res(3))
	mat.step(3)=1
	mat.ind(3)=-1

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res,mat.step,mat.ndim)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	mat.hsame=.FALSE.
	if(mat.ihash.GE.0) then
	  if(mat.ind(1).EQ.mat.hind) mat.hsame=.TRUE.
	endif

	mat.ind1=mat.ind(1)
	mat.ind2=mat.ind(2)
	mat.ind3=mat.ind(3)
	mat.par1=mat.par(1)
	mat.par2=mat.par(2)
	mat.par3=mat.par(3)

	mat.matmode=0

	com(icmd).addr=pairsaddr(mat.par3)

	return

	end

	subroutine gs1_sort3d_pair(mat)

#include "gsort.inc"
	record/matrix/mat

#if defined( __GFORTRAN__ )
	character*64 dyn_format
#endif

	call gs_putindpar(mat.ind(1),mat.par(1))
	llist=llist+2
	call gs_putindpar(mat.ind(2),mat.par(2))

	lgi=1
	if(mat.par(3).GE.10) lgi=2

#if defined( __GFORTRAN__ )
	write(dyn_format,'(i0)')jpar
	write(list(llist+2:),'(A,'//dyn_format(1:len(dyn_format))//')')'P',mat.par(3)
#else
	write(list(llist+2:),4201)'P',mat.par(3)
4201	format(A,I<lgi>)
#endif

	llist=max(1,lengthc(list))+3
	lfile=max(1,lengthc(mat.file))
	write(list(llist:),'(1x,a)') mat.file(1:lfile)

	llist=lengthc(list)+3
	call gs_putres(mat.res,mat.ndim)

	llist=llist+3
	call gs_putstep(mat.step,mat.ndim)

	llist=llist+3
	call gs_puthash(mat.nhash,mat.ihash)

	return

	end

	subroutine gs2_sort3d_pair(mat)

#include "gsort.inc"
	record/matrix/mat

	if(init_mode.EQ.0) then
	  call gs_matopen(mat)
	endif

	return

	end

	subroutine gs3_sort3d_pair(mat,dat)

#include "gsort.inc"
	record/matrix/mat
	record/pairsdat/dat

	integer incr(3)
	
	
	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue.LT.nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	j1f=doff(mat.ind1)
	j1l=doff(mat.ind1)+ndet(mat.ind1)-1
	do j1=j1f,j1l
	  id1=det(j1).id
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1f,j1l
	   if(j2 .ne. j1) then
	    id2=det(j2).id
	    incr(2)=det(j2).ival(mat.par1)
	    id3=dat.pind(id1,id2)
	    if(id3.ge.0) then
	      incr(3)=id3
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    endif
	    endif
	  end do
	end do
	return

1000	continue

	j1f=doff(mat.ind1)
	j1l=doff(mat.ind1)+ndet(mat.ind1)-1
	do j1=j1f,j1l
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.lt.nhash) goto 110
	  incr(1)=det(j1).ival(mat.par1)
	  id1=det(j1).id
	  do j2=j1f,j1l
	    if(j2 .ne. j1) then
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.lt.nhash) goto 120
	    incr(2)=det(j2).ival(mat.par1)
	    id2=det(j2).id
	    id3=dat.pind(id1,id2)
	    if(id3.ge.0) then
	      incr(3)=id3
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    endif
	    endif
120	  end do
110	end do
	return

8000	return

	end

	subroutine gs4_sort3d_pair(mat)

#include "gsort.inc"
	record/matrix/mat

CVMS	integer   n1,n2
	integer*8 n1,n2

	call cmt_infoincr(%val(mat.cmtbase),n1,n2)
	nincr(mat.ndim)=nincr(mat.ndim) + n1
	flushed=flushed + n2
	  
	if(finit_mode.EQ.2) then
	  call gs_matclose(mat)
	endif

	return

	end




	subroutine gs0_sort2d_ab(mat)

#include "gsort.inc"
	record/matrix/mat

	synt(1)='SORT2D_AB Pn Pm matrixname [RES Rx Ry] [STEP Sx Sy] [Hn [#times]] '

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=2

	call gs_getindpar(mat.ind(1),mat.par(1),mat.res(1))
	mat.step(1)=min(mat.res(1),DEFSTEP2D)

	call gs_getindpar(mat.ind(2),mat.par(2),mat.res(2))
	mat.step(2)=min(mat.res(2),DEFSTEP2D)

	if(mat.ind(1) .NE. mat.ind(2)) call gs_syntax('Must be same Index')
	if(mat.par(1) .EQ. mat.par(2)) call gs_syntax('Must be different Parameter')

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res,mat.step,mat.ndim)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	mat.hsame=.FALSE.
	if(mat.ihash.GE.0) then
	  if(mat.ind(1).EQ.mat.hind) mat.hsame=.TRUE.
	endif

	mat.ind1=mat.ind(1)
	mat.ind2=mat.ind(2)

	mat.par1=mat.par(1)
	mat.par2=mat.par(2)

	mat.matmode=0

	return

	end

	subroutine gs1_sort2d_ab(mat)

#include "gsort.inc"
	record/matrix/mat

	matndim=mat.ndim
	do nn=1,matndim
	  call gs_putindpar(mat.ind(nn),mat.par(nn))
	  llist=llist+2
	end do

	lfile=max(1,lengthc(mat.file))
	write(list(llist:),'(1x,a)') mat.file(1:lfile)

	llist=lengthc(list)+3
	call gs_putres(mat.res,matndim)

	llist=llist+3
	call gs_putstep(mat.step,matndim)

	llist=llist+3
	call gs_puthash(mat.nhash,mat.ihash)

	return

	end

	subroutine gs2_sort2d_ab(mat)

#include "gsort.inc"
	record/matrix/mat

	if(init_mode.EQ.0) then
	  call gs_matopen(mat)
	endif

	return

	end

	subroutine gs3_sort2d_ab(mat)

#include "gsort.inc"
	record/matrix/mat

	integer incr(2)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue.LT.nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	j1f=doff(mat.ind1)
	j1l=doff(mat.ind1)+ndet(mat.ind1)-1
	do j1=j1f,j1l
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1f,j1l
	    if(j2 .NE. j1) then
	      incr(2)=det(j2).ival(mat.par2)
	      call cmt_2dincr(%val(mat.cmtbase),incr)
	    endif
	  end do
	end do
	return

1000	continue

	j1f=doff(mat.ind1)
	j1l=doff(mat.ind1)+ndet(mat.ind1)-1
	do j1=j1f,j1l
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.lt.nhash) goto 110
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1f,j1l
	    if(j2. NE. j1) then
	        nn2=nn1
	        if(hltrue(j2)) nn2=nn2-1
	        if(nn2.lt.nhash) goto 120
	      incr(2)=det(j2).ival(mat.par2)
	      call cmt_2dincr(%val(mat.cmtbase),incr)
	    endif
120	  end do
110	end do
	return

8000	return

	end

	subroutine gs4_sort2d_ab(mat)

#include "gsort.inc"
	record/matrix/mat

CVMS	integer   n1,n2
	integer*8 n1,n2

	call cmt_infoincr(%val(mat.cmtbase),n1,n2)
	nincr(mat.ndim)=nincr(mat.ndim) + n1
	flushed=flushed + n2
	  
	if(finit_mode.EQ.2) then
	  call gs_matclose(mat)
	endif

	return

	end


	subroutine gs0_sort3d_diff(mat)

#include "gsort.inc"
	record/matrix/mat

	synt(1)='SORT3D_DIFF Pn Pn Dm offset matrixname [RES Rx Rx Rz] [STEP Sx Sx Sz] [Hn [#times]] '
	synt(2)='   (for pair ij of detectors, 3rd parameter  will be:  Dm(j) - Dm(i) + offset )'

	if(syntax) then
	  call gs_syntax(' ')
	  return
	endif

	mat.ndim=3

	call gs_getindpar(mat.ind(1),mat.par(1),mat.res(1))
	mat.step(1)=min(mat.res(1),DEFSTEP3D)

	call gs_getindpar(mat.ind(2),mat.par(2),mat.res(2))
	mat.step(2)=min(mat.res(2),DEFSTEP3D)

	if(mat.ind(1).NE.mat.ind(2) .OR.
	1  mat.par(1).NE.mat.par(2) ) call gs_syntax('Must be same Index and same Parameter')

	call gs_getindpar(mat.ind(3),mat.par(3),mat.res(3))
	mat.step(3)=min(mat.res(3),DEFSTEP3D)

	nn=istr_i1(comline,mat.doffset)

	call istr_ch(comline,mat.file)
	if(lengthc(mat.file).LT.1) call gs_syntax('Filename of matrix expected')
	call filetype(mat.file,'cmat')

	call gs_resstep(mat.res,mat.step,mat.ndim)

	call gs_gethash(mat.ihash,mat.nhash,mat.hind,mat.hpar,mat.spherical)
	mat.hsame=.FALSE.
	if(mat.ihash.GE.0) then
	  if(mat.ind(1).EQ.mat.hind) mat.hsame=.TRUE.
	endif

	mat.ind1=mat.ind(1)
	mat.ind2=mat.ind(2)
	mat.ind3=mat.ind(3)

	mat.par1=mat.par(1)
	mat.par2=mat.par(2)
	mat.par3=mat.par(3)

	mat.matmode=0

	return

	end

	subroutine gs1_sort3d_diff(mat)

#include "gsort.inc"
	record/matrix/mat

	matndim=mat.ndim
	do nn=1,matndim
	  call gs_putindpar(mat.ind(nn),mat.par(nn))
	  llist=llist+2
	end do

	llist=max(1,lengthc(list))+2
	write(list(llist:),'(i6)') mat.doffset

	lfile=max(1,lengthc(mat.file))
	write(list(llist:),'(1x,a)') mat.file(1:lfile)

	llist=lengthc(list)+3
	call gs_putres(mat.res,matndim)

	llist=llist+3
	call gs_putstep(mat.step,matndim)

	llist=llist+3
	call gs_puthash(mat.nhash,mat.ihash)

	return

	end

	subroutine gs2_sort3d_diff(mat)

#include "gsort.inc"
	record/matrix/mat

	if(init_mode.EQ.0) then
	  call gs_matopen(mat)
	endif

	return

	end

	subroutine gs3_sort3d_diff(mat)

#include "gsort.inc"
	record/matrix/mat

	integer incr(3)

	if(mat.nhash.GT.0) then
	  nhash=mat.nhash
	  call gs3_hgate(%val(hashaddr(mat.ihash)))
	  if(hntrue.LT.nhash) goto 8000
	  if(mat.hsame) goto 1000
	endif

	j1f=doff(mat.ind1)
	j1l=doff(mat.ind1)+ndet(mat.ind1)-1
	matres3=mat.res(3)
	do j1=j1f,j1l-1
	  do j2=j1+1,j1l
	    id1=det(j1).id
	    incr(1)=det(j1).ival(mat.par1)
	    id2=det(j2).id
	    incr(2)=det(j2).ival(mat.par1)
	    rrtmp=det(j2).xval(mat.par3) - det(j1).xval(mat.par3)
	    id3= rrtmp + mat.doffset
	    if(id3.ge.0 .and. id3.lt.matres3) then
	      incr(3)=id3
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    endif
	    
	    id2=det(j1).id
	    incr(2)=det(j1).ival(mat.par1)
	    id1=det(j2).id
	    incr(1)=det(j2).ival(mat.par1)
	    rrtmp=det(j1).xval(mat.par3) - det(j2).xval(mat.par3)
	    id3= rrtmp + mat.doffset
	    if(id3.ge.0 .and. id3.lt.matres3) then
	      incr(3)=id3
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    endif
	    
	  end do
	end do
	return

1000	continue

	j1f=doff(mat.ind1)
	j1l=doff(mat.ind1)+ndet(mat.ind1)-1
	matres3=mat.res(3)
	do j1=j1f,j1l-1
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.lt.nhash) goto 110
	  incr(1)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.lt.nhash) goto 120
	    incr(2)=det(j2).ival(mat.par1)
	    rrtmp=det(j2).xval(mat.par3) - det(j1).xval(mat.par3)
	    id3=rrtmp + mat.doffset
	    if(id3.ge.0 .and. id3.lt.matres3) then
	      incr(3)=id3
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    endif
120	  end do
110	end do

	j1f=doff(mat.ind1)
	j1l=doff(mat.ind1)+ndet(mat.ind1)-1
	matres3=mat.res(3)
	do j1=j1f,j1l-1
	    nn1=hntrue
	    if(hltrue(j1)) nn1=nn1-1
	    if(nn1.lt.nhash) goto 111
	  incr(2)=det(j1).ival(mat.par1)
	  do j2=j1+1,j1l
	      nn2=nn1
	      if(hltrue(j2)) nn2=nn2-1
	      if(nn2.lt.nhash) goto 121
	    incr(1)=det(j2).ival(mat.par1)
	    rrtmp=det(j1).xval(mat.par3) - det(j2).xval(mat.par3)
	    id3=rrtmp + mat.doffset
	    if(id3.ge.0 .and. id3.lt.matres3) then
	      incr(3)=id3
	      call cmt_3dincr(%val(mat.cmtbase),incr)
	    endif
121	  end do
111	end do

	return

8000	return

	end

	subroutine gs4_sort3d_diff(mat)

#include "gsort.inc"
	record/matrix/mat

CVMS	integer   n1,n2
	integer*8 n1,n2

	call cmt_infoincr(%val(mat.cmtbase),n1,n2)
	nincr(mat.ndim)=nincr(mat.ndim) + n1
	flushed=flushed + n2
	  
	if(finit_mode.EQ.2) then
	  call gs_matclose(mat)
	endif

	return

	end
