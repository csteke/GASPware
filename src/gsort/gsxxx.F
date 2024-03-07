	subroutine gs0_sort2d_diff

	INCLUDE 'gsort.inc'

	synt(1)='SORT2D_DIFF   matrixname  (G|S|T)n  [RES Rx Ry [STEP Sx Sy] ]  Offset'

	cmatfile=' '
	call istr_ch(comline,CMATFILE)
	if(lengthc(CMATFILE).lt.1) call gs_syntax('Filename of matrix expected',0)
	matndim=2
	call gs_getindpar(d21ind,d21par,ires)
	matres(1)=ires
	if(d21ind.eq.0) then
	  call gs_syntax('G#par or S#par or T#par expected',0)
	elseif(d21ind.eq.1) then
	  matres(2)=((NITEMS(G)+1)*NITEMS(G))/2
	elseif(d21ind.eq.2) then
	  matres(2)=((NITEMS(S)+1)*NITEMS(S))/2
	endif
	call gs_resstep(matres,matstep,matndim)
	if(matstep(1).le.0) matstep(1)=min(matres(1),DEFSTEP2D)
	if(matstep(2).le.0) matstep(2)=min(matres(2),DEFSTEP2D)
	matres(2)=((matres(2)+matstep(2)-1)/matstep(2))*matstep(2)
	ii=istr_i1(comline,D21offset)
	if(ii.ne.1) call gs_syntax('Wrong number of parameters',0)
	DOMATRIX=.TRUE.
	SYMMETRIC=.FALSE.
	HSYMMETRIC=.FALSE.

	return

	end

	subroutine gs1_sort2d_diff

	INCLUDE 'gsort.inc'

	lcmatfile=max(1,lengthc(Cmatfile))
	write(list(llist:),'(a,i3)') CMATFILE(1:lcmatfile)
	lgp=1
	if(D21par.ge.10) lgp=2
	llist=max(1,lengthc(list))+3
	write(list(llist:),4201)dname(D21ind),D21par
	imatndim=matndim
	llist=lengthc(list)+4
	write(list(llist:),4202) (matres(ii),ii=1,imatndim)
	llist=lengthc(list)+4
	write(list(llist:),4203) (matstep(ii),ii=1,imatndim)
	llist=max(1,lengthc(list))+3
	write(list(llist:),'(i6)') D21offset

4201	format(a,I<lgp>)
4202	format('Res',<imatndim>i5)
4203	format('Step',<imatndim>i4)

	return

	end

	subroutine gs2_sort2d_diff

	INCLUDE 'gsort.inc'

	if(.not.firstrun) return
	call gs5_init_membuffs	! inizializza i buffers in memoris
	call gs5_matopen		! apre la matrice; va eseguita dopo gs5_init_membuffs per
	return

	end

	subroutine gs3_sort2d_diff

	INCLUDE 'gsort.inc'

C    matrice bidimensionale [differenza,indice] delle differenze di valore
C    di un parametro tra coppie ordinate di rivelatori
C	
C       differenza = par(l2) - par(l1) + offset
C       indice     = descrive la coppia di rivelatori (l1,l2) con l2>l1
C                   gli indici sono ordinati secondo la sequenza
C                   (0,0) (0,1) (0,2) ... (0,ndet-1)
C                         (1,1) (1,2) ... (1,ndet-1)
C                            ......................
C                                         (ndet-1,ndet-1)
C                   ovviamente gli indici (i,i) danno spettri vuoti e servono
C                   come segnaposto.

	if(NDET(D21ind).lt.2) goto 8000
	if(hash) then
	  call gs3_ggate(%val(hbase))
	  if(nhh.lt.nhash) goto 8000
	  goto 1000
	endif

	if(D21ind.eq.1) then
	  do jg1=0,NDET(G)-2
	    l1=LVAL(jg1,-1,G)
	    do jg2=jg1+1,NDET(G)-1
	      l2=LVAL(jg2,-1,G)
	      if(l1.le.l2) then
	        incr(1)=LVAL(jg2,D21par,G)-LVAL(jg1,D21par,G) + D21OFFSET
	        incr(2)=NITEMS(G)*l1-(l1*(l1+1))/2 + l2
	      else
	        incr(1)=LVAL(jg1,D21par,G)-LVAL(jg2,D21par,G) + D21OFFSET
	        incr(2)=NITEMS(G)*l2-(l2*(l2+1))/2 + l1
	      endif
	      call gs5_2dincr
	    enddo
	  enddo
	elseif(D21ind.eq.2) then
	  do j1=0,NDET(S)-2
	    l1=LVAL(j1,-1,S)
	    do j2=j1+1,NDET(S)-1
	      l2=LVAL(j2,-1,S)
	      if(l1.le.l2) then
	        incr(1)=LVAL(j2,D21par,S)-LVAL(j1,D21par,S) + D21OFFSET
	        incr(2)=NITEMS(S)*l1-(l1*(l1+1))/2 + l2
	      else
	        incr(1)=LVAL(j1,D21par,S)-LVAL(j2,D21par,S) + D21OFFSET
	        incr(2)=NITEMS(S)*l2-(l2*(l2+1))/2 + l1
	      endif
	      call gs5_2dincr
	    enddo
	  enddo
	endif

	return

1000	if(D21ind.eq.1) then
	  do jg1=0,NDET(G)-2
	      nn1=nhh
	      if(htrue(jg1)) nn1=nn1-1
	      if(nn1.lt.nhash) goto 110
	      l1=LVAL(jg1,-1,G)
	    do jg2=jg1+1,NDET(G)-1
	        nn2=nn1
	        if(htrue(jg2)) nn2=nn2-1
	        if(nn2.lt.nhash) goto 120
	      l2=LVAL(jg2,-1,G)
	      if(l1.le.l2) then
	        incr(1)=LVAL(jg2,D21par,G)-LVAL(jg1,D21par,G) + D21OFFSET
	        incr(2)=NITEMS(G)*l1-(l1*(l1+1))/2 + l2
	      else
	        incr(1)=LVAL(jg1,D21par,G)-LVAL(jg2,D21par,G) + D21OFFSET
	        incr(2)=NITEMS(G)*l2-(l2*(l2+1))/2 + l1
	      endif
	      call gs5_2dincr
120	    enddo
110	  enddo
	elseif(D21ind.eq.2) then
	  do j1=0,NDET(S)-2
	    l1=LVAL(j1,-1,S)
	    do j2=j1+1,NDET(S)-1
	      l2=LVAL(j2,-1,S)
	      if(l1.le.l2) then
	        incr(1)=LVAL(j2,D21par,S)-LVAL(j1,D21par,S) + D21OFFSET
	        incr(2)=NITEMS(S)*l1-(l1*(l1+1))/2 + l2
	      else
	        incr(1)=LVAL(j1,D21par,S)-LVAL(j2,D21par,S) + D21OFFSET
	        incr(2)=NITEMS(S)*l2-(l2*(l2+1))/2 + l1
	      endif
	      call gs5_2dincr
	    enddo
	  enddo
	endif

8000	return

	end

	subroutine gs4_sort2d_diff

	INCLUDE 'gsort.inc'

	return

	end
