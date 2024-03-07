CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C		PACKAGE DISP_ PER PRESENTAZIONE SU TEK4010
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	SUBROUTINE DISP_YFUN(yfun,xmin,xmax,xdelta,ityp,dwin,twin)

	dimension dwin(4),twin(4)

	RXVAL(XX)=MIN(MAX( ((XX-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(YY)=MIN(MAX( ((YY-DY0)*YSCA+TY0) , TY0),TY1)

	call disp_setscale(dwin,twin,xsca,ysca)
	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

	XX=xmin
	YY=yfun(xx)

	RX=RXVAL(XX)
	RY=RYVAL(YY)
	call disp_vecmod
	CALL disp_movabs(RX,RY)

	GOTO (1,2,3) abs(ityp)

CCCCCCCCCCC HISTOGRAMM
1	IF(XSCA.LT.0.5 .and. ityp.lt.0) GOTO 2
	XX=xmin+xdelta
	RX=RXVAL(XX)
	CALL disp_drwabs(RX,RY)
	DO xx=xmin+xdelta,xmax,xdelta
	   YY=yfun(xx)
	   RX=RXVAL(XX)
	   RY=RYVAL(YY)
	   CALL disp_drwabs(RX,RY)
	   RX=RXVAL(XX+xdelta)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

CCCCCCCCCCC VECTOR
2	IF(XSCA.LT.0.3 .and. ityp.lt.0) GOTO 20
	DO xx=xmin,xmax,xdelta
	   YY=yfun(xx)
	   RX=RXVAL(XX)
	   RY=RYVAL(YY)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

CCCCCCCCCCC VECTOR COMPRESSED
20	CONTINUE
	RYMIN=RY
	RYMAX=RY
	III=RX
	DO xx=xmin,xmax,xdelta
	   YY=yfun(xx)
	   RX=RXVAL(XX)
	   RY=RYVAL(YY)
	   IF(int(RX).EQ.III) THEN
		RYMIN=MIN(RYMIN,RY)
		RYMAX=MAX(RYMAX,RY)
	   ELSE
		CALL disp_movabs(float(III),RYMIN)
		CALL disp_drwabs(float(III),RYMAX)
		CALL disp_drwabs(RX,RY)
		RYMIN=RY
		RYMAX=RY
		III=RX
	   ENDIF
	ENDDO
	CALL disp_movabs(float(III),RYMIN)
	CALL disp_drwabs(float(III),RYMAX)
	call disp_anmode
	RETURN

CCCCCCCCCCC POINTS
3	CONTINUE
	DO xx=xmin,xmax,xdelta
	   YY=yfun(xx)
	   RX=RXVAL(XX)
	   RY=RYVAL(YY)
	   CALL disp_pntabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

	END

	SUBROUTINE DISP_xfun(xfun,ymin,ymax,ydelta,ityp,dwin,twin)

	dimension dwin(4),twin(4)

	RXVAL(XX)=MIN(MAX( ((XX-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(YY)=MIN(MAX( ((YY-DY0)*YSCA+TY0) , TY0),TY1)

	call disp_setscale(dwin,twin,xsca,ysca)
	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

	yy=ymin
	xx=xfun(yy)

	RX=RXVAL(XX)
	RY=RYVAL(YY)

	call disp_vecmod
	CALL disp_movabs(RX,RY)

	GOTO (2,2,3) abs(ityp)

CCCCCCCCCCC VECTOR
2	DO yy=ymin,ymax,ydelta
	   xx=xfun(yy)
	   RX=RXVAL(XX)
	   RY=RYVAL(YY)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

CCCCCCCCCCC POINTS
3	CONTINUE
	DO yy=ymin,ymax,ydelta
	   xx=xfun(yy)
	   RX=RXVAL(XX)
	   RY=RYVAL(YY)
	   CALL disp_pntabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

	END


	SUBROUTINE DISP_pfun(xfun,yfun,pmin,pmax,pdelta,ityp,dwin,twin)

	dimension dwin(4),twin(4)

	RXVAL(XX)=MIN(MAX( ((XX-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(YY)=MIN(MAX( ((YY-DY0)*YSCA+TY0) , TY0),TY1)

	call disp_setscale(dwin,twin,xsca,ysca)
	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

	pp=pmin
	xx=xfun(pp)
	yy=yfun(pp)

	RX=RXVAL(XX)
	RY=RYVAL(YY)

	call disp_vecmod
	CALL disp_movabs(RX,RY)

	GOTO (2,2,3) abs(ityp)

CCCCCCCCCCC VECTOR
2	DO pp=pmin,pmax,pdelta
	   xx=xfun(pp)
	   yy=yfun(pp)
	   RX=RXVAL(XX)
	   RY=RYVAL(YY)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

CCCCCCCCCCC POINTS
3	CONTINUE
	DO pp=pmin,pmax,pdelta
	   xx=xfun(pp)
	   yy=yfun(pp)
	   RX=RXVAL(XX)
	   RY=RYVAL(YY)
	   CALL disp_pntabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

	END

	SUBROUTINE DISP_YFUN_eb(yfun,eyfun,xmin,xmax,xdelta,dwin,twin)

	dimension dwin(4),twin(4)

	RXVAL(XX)=MIN(MAX( ((XX-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(YY)=MIN(MAX( ((YY-DY0)*YSCA+TY0) , TY0),TY1)

	call disp_setscale(dwin,twin,xsca,ysca)
	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

	DO xx=xmin,xmax,xdelta
	   yYY=yfun(xx)
	   eYY=eyfun(xx)
	   yy=yYY-eYY
	   RX=RXVAL(XX)
	   RY=RYVAL(YY)
	   CALL disp_movabs(RX,RY)
	   yy=yYY+eYY
	   RY=RYVAL(YY)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

	END

	SUBROUTINE DISP_xFUN_eb(xfun,exfun,ymin,ymax,ydelta,dwin,twin)

	dimension dwin(4),twin(4)

	RXVAL(XX)=MIN(MAX( ((XX-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(YY)=MIN(MAX( ((YY-DY0)*YSCA+TY0) , TY0),TY1)

	call disp_setscale(dwin,twin,xsca,ysca)
	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

	DO yy=ymin,ymax,ydelta
	   xXX=xfun(yy)
	   eXX=exfun(yy)
	   xx=xXX-eXX
	   RX=RXVAL(XX)
	   RY=RYVAL(YY)
	   CALL disp_movabs(RX,RY)
	   xx=xXX+eXX
	   RX=RXVAL(xx)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

	END

	SUBROUTINE DISP_pFUN_eb(xfun,yfun,exfun,eyfun,pmin,pmax,pdelta
	1	,dwin,twin)

	dimension dwin(4),twin(4)

	RXVAL(XX)=MIN(MAX( ((XX-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(YY)=MIN(MAX( ((YY-DY0)*YSCA+TY0) , TY0),TY1)

	call disp_setscale(dwin,twin,xsca,ysca)
	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

	DO pp=pmin,pmax,pdelta
	   xXX=xfun(pp)
	   eXX=exfun(pp)
	   yYY=yfun(pp)
	   eYY=eyfun(pp)
	   xx=xXX-eXX
	   RX=RXVAL(XX)
	   RY=RYVAL(yYY)
	   CALL disp_movabs(RX,RY)
	   xx=xXX+eXX
	   RX=RXVAL(xx)
	   CALL disp_drwabs(RX,RY)
	   yy=yYY-eYY
	   RX=RXVAL(xXX)
	   RY=RYVAL(YY)
	   CALL disp_movabs(RX,RY)
	   yy=yYY+eYY
	   RY=RYVAL(YY)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

	END

	SUBROUTINE DISP_Yvec(yvec,imin,imax,idelta,ityp,dwin,twin)

	dimension dwin(4),twin(4)
	dimension yvec(1)

	RXVAL(ii)=MIN(MAX( ((ii-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(YY)=MIN(MAX( ((YY-DY0)*YSCA+TY0) , TY0),TY1)

	call disp_setscale(dwin,twin,xsca,ysca)
	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

	ii=imin
	YY=yvec(ii)

	RX=RXVAL(ii)
	RY=RYVAL(YY)

	call disp_vecmod
	CALL disp_movabs(RX,RY)

	GOTO (1,2,3) abs(ityp)

CCCCCCCCCCC HISTOGRAMM
1	IF(XSCA.LT.0.5 .and. ityp.lt.0) GOTO 2
	ii=imin+idelta
	RX=RXVAL(ii)
	CALL disp_drwabs(RX,RY)
	DO ii=imin+idelta,imax,idelta
	   YY=yvec(ii)
	   RX=RXVAL(ii)
	   RY=RYVAL(YY)
	   CALL disp_drwabs(RX,RY)
	   RX=RXVAL(ii+idelta)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

CCCCCCCCCCC VECTOR
2	IF(XSCA.LT.0.3 .and. ityp.lt.0) GOTO 20
	DO ii=imin,imax,idelta
	   YY=yvec(ii)
	   RX=RXVAL(ii)
	   RY=RYVAL(YY)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

CCCCCCCCCCC VECTOR COMPRESSED
20	CONTINUE
	RYMIN=RY
	RYMAX=RY
	III=RX
	DO ii=imin,imax,idelta
	   YY=yvec(ii)
	   RX=RXVAL(ii)
	   RY=RYVAL(YY)
	   IF(int(RX).EQ.III) THEN
		RYMIN=MIN(RYMIN,RY)
		RYMAX=MAX(RYMAX,RY)
	   ELSE
		CALL disp_movabs(float(III),RYMIN)
		CALL disp_drwabs(float(III),RYMAX)
		CALL disp_drwabs(RX,RY)
		RYMIN=RY
		RYMAX=RY
		III=RX
	   ENDIF
	ENDDO
	CALL disp_movabs(float(III),RYMIN)
	CALL disp_drwabs(float(III),RYMAX)
	call disp_anmode
	RETURN

CCCCCCCCCCC POINTS
3	CONTINUE
	DO ii=imin,imax,idelta
	   YY=yvec(ii)
	   RX=RXVAL(ii)
	   RY=RYVAL(YY)
	   CALL disp_pntabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

	END

	SUBROUTINE DISP_xvec(xvec,imin,imax,idelta,ityp,dwin,twin)

	dimension dwin(4),twin(4)
	dimension xvec(1)

	RXVAL(XX)=MIN(MAX( ((XX-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(ii)=MIN(MAX( ((ii-DY0)*YSCA+TY0) , TY0),TY1)

	call disp_setscale(dwin,twin,xsca,ysca)
	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

	ii=imin
	xx=xvec(ii)

	RX=RXVAL(xx)
	RY=RYVAL(ii)

	call disp_vecmod
	CALL disp_movabs(RX,RY)

	GOTO (2,2,3) abs(ityp)

CCCCCCCCCCC VECTOR
2	DO ii=imin,imax,idelta
	   xx=xvec(ii)
	   RX=RXVAL(xx)
	   RY=RYVAL(ii)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

CCCCCCCCCCC POINTS
3	DO ii=imin,imax,idelta
	   xx=xvec(ii)
	   RX=RXVAL(xx)
	   RY=RYVAL(ii)
	   CALL disp_pntabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

	END

	SUBROUTINE DISP_pvec(xvec,yvec,imin,imax,idelta,ityp,dwin,twin)

	dimension dwin(4),twin(4)
	dimension xvec(1)
	dimension yvec(1)

	RXVAL(XX)=MIN(MAX( ((XX-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(YY)=MIN(MAX( ((YY-DY0)*YSCA+TY0) , TY0),TY1)

	call disp_setscale(dwin,twin,xsca,ysca)
	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

	ii=imin
	xx=xvec(ii)
	yy=yvec(ii)

	RX=RXVAL(XX)
	RY=RYVAL(YY)

	call disp_vecmod
	CALL disp_movabs(RX,RY)

	GOTO (2,2,3) abs(ityp)

CCCCCCCCCCC VECTOR
2	DO ii=imin,imax,idelta
	   xx=xvec(ii)
	   yy=yvec(ii)
	   RX=RXVAL(XX)
	   RY=RYVAL(YY)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

CCCCCCCCCCC POINTS
3	CONTINUE
	DO ii=imin,imax,idelta
	   xx=xvec(ii)
	   yy=yvec(ii)
	   RX=RXVAL(XX)
	   RY=RYVAL(YY)
	   CALL disp_pntabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

	END

	SUBROUTINE DISP_Yvec_eb(yvec,eyvec,imin,imax,idelta,dwin,twin)

	dimension dwin(4),twin(4)
	dimension yvec(1),eyvec(1)

	RXVAL(ii)=MIN(MAX( ((ii-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(YY)=MIN(MAX( ((yy-DY0)*YSCA+TY0) , TY0),TY1)

	call disp_setscale(dwin,twin,xsca,ysca)
	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

	DO ii=imin,imax,idelta
	   yYY=yvec(ii)
	   eYY=eyvec(ii)
	   yy=yYY-eYY
	   RX=RXVAL(ii)
	   RY=RYVAL(YY)
	   CALL disp_movabs(RX,RY)
	   yy=yYY+eYY
	   RY=RYVAL(YY)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

	END


	SUBROUTINE DISP_xvec_eb(xvec,exvec,imin,imax,idelta,dwin,twin)

	dimension dwin(4),twin(4)
	dimension xvec(1),exvec(1)

	RXVAL(XX)=MIN(MAX( ((XX-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(ii)=MIN(MAX( ((ii-DY0)*YSCA+TY0) , TY0),TY1)

	call disp_setscale(dwin,twin,xsca,ysca)
	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

	ii=imin
	xx=xvec(ii)

	RX=RXVAL(xx)
	RY=RYVAL(ii)

	call disp_vecmod
	CALL disp_movabs(RX,RY)

	DO ii=imin,imax,idelta
	   xXX=xvec(ii)
	   eXX=exvec(ii)
	   xx=xXX-eXX
	   RX=RXVAL(xx)
	   RY=RYVAL(ii)
	   CALL disp_movabs(RX,RY)
	   xx=xXX+eXX
	   RX=RXVAL(xx)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

	END

	SUBROUTINE DISP_pvec_eb(xvec,exvec,yvec,eyvec,imin,imax,idelta,
	1	dwin,twin)

	dimension dwin(4),twin(4)
	dimension xvec(1),exvec(1)
	dimension yvec(1),eyvec(1)

	RXVAL(XX)=MIN(MAX( ((XX-DX0)*XSCA+TX0) , TX0),TX1)
	RYVAL(YY)=MIN(MAX( ((YY-DY0)*YSCA+TY0) , TY0),TY1)

	call disp_setscale(dwin,twin,xsca,ysca)
	dx0=dwin(1)
	dx1=dwin(2)
	dy0=dwin(3)
	dy1=dwin(4)
	tx0=twin(1)
	tx1=twin(2)
	ty0=twin(3)
	ty1=twin(4)

	DO ii=imin,imax,idelta
	   xXX=xvec(ii)
	   eXX=exvec(ii)
	   yYY=yvec(ii)
	   eYY=eyvec(ii)
	   xx=xXX-eXX
	   RX=RXVAL(XX)
	   RY=RYVAL(yYY)
	   CALL disp_movabs(RX,RY)
	   xx=xXX+eXX
	   RX=RXVAL(xx)
	   CALL disp_drwabs(RX,RY)
	   yy=yYY-eYY
	   RX=RXVAL(xXX)
	   RY=RYVAL(YY)
	   CALL disp_movabs(RX,RY)
	   yy=yYY+eYY
	   RY=RYVAL(YY)
	   CALL disp_drwabs(RX,RY)
	ENDDO
	call disp_anmode
	RETURN

	END

	subroutine disp_setscale(dwin,twin,xsca,ysca)

	dimension dwin(4),twin(4)
	
	dx0=dwin(1)
	dx1=dwin(2)

	dy0=dwin(3)
	dy1=dwin(4)

	tx0=twin(1)
	tx1=twin(2)

	ty0=twin(3)
	ty1=twin(4)

	IF(DY1.Lt.DY0) call swapi(dy0,dy1)
	IF(DY1.eq.DY0) then
	   DY0=DY1-1
	   DY1=DY1+1
	ENDIF
	IF(tY1.Lt.tY0) call swapi(ty0,ty1)
	IF(tY1.eq.tY0) then
	   ty0=max(0.,ty0-1)
	   tY1=tY1+1
	ENDIF
	YSCA=(TY1-TY0)/(DY1-DY0)

	IF(Dx1.Lt.Dx0) call swapi(dx0,dx1)
	IF(Dx1.eq.Dx0) then
	   Dx0=Dx1-1
	   Dx1=Dx1+1
	ENDIF
	IF(tx1.Lt.tx0) call swapi(tx0,tx1)
	IF(tx1.eq.tx0) then
	   tx0=max(0.,tx0-1)
	   tx1=tx1+1
	ENDIF
	xSCA=(Tx1-Tx0)/(Dx1-Dx0)

	dwin(1)=dx0
	dwin(2)=dx1

	dwin(3)=dy0
	dwin(4)=dy1

	twin(1)=tx0
	twin(2)=tx1

	twin(3)=ty0
	twin(4)=ty1

	return

	end

	subroutine disp_map(dx,dy,tx,ty,dwin,twin)

	dimension dwin(4),twin(4)

	call disp_setscale(dwin,twin,xsca,ysca)

	tx=(DX-Dwin(1))*XSCA+Twin(1)
	ty=(DY-Dwin(3))*ySCA+Twin(3)

	return

	end

	subroutine disp_clipw(x,y,win)

	dimension win(4)

	x=max(win(1),min(x,win(2)))
	y=max(win(3),min(y,win(4)))

	return

	end

	subroutine disp_alfa_home

	call disp_vecmod
	call disp_home
	call disp_anmode

	return

	end

	subroutine disp_window(win)

	dimension win(4)

	call disp_vecmod
	call disp_movabs(win(1),win(3))
	call disp_drwabs(win(2),win(3))
	call disp_drwabs(win(2),win(4))
	call disp_drwabs(win(1),win(4))
	call disp_drwabs(win(1),win(3))
	call disp_anmode

	return

	end

	subroutine disp_line(x1,y1,x2,y2)

	call disp_vecmod
	call disp_movabs(x1,y1)
	call disp_drwabs(x2,y2)
	call disp_anmode

	return

	end

	subroutine disp_posabs(rx,ry)

	call DISP_vecmod
	call DISP_movabs(rx,ry)
	call DISP_anmode

	return

	end

	subroutine disp_movabs(rx,ry)

	COMMON /disp_000_mode/ mode_disp

	if(mode_disp.eq.0) then
	   call movabs(int(rx),int(ry))
	elseif(mode_disp.eq.1) then
	   call LAS_move(rx,ry)
	endif

	return

	end

	subroutine disp_drwabs(rx,ry)

	COMMON /disp_000_mode/ mode_disp

	if(mode_disp.eq.0) then
	   call drwabs(int(rx),int(ry))
	elseif(mode_disp.eq.1) then
	   call LAS_draw(rx,ry)
	endif

	return

	end

	subroutine disp_pntabs(rx,ry)

	COMMON /disp_000_mode/ mode_disp

	if(mode_disp.eq.0) then
	   call pntabs(int(rx),int(ry))
	elseif(mode_disp.eq.1) then
	   call LAS_move(rx,ry)
	   call LAS_draw(rx,ry)
	endif

	return

	end

	subroutine disp_movrel(rx,ry)

	COMMON /disp_000_mode/ mode_disp

	if(mode_disp.eq.0) then
	   call movrel(int(rx),int(ry))
	endif

	return

	end

	subroutine disp_drwrel(rx,ry)

	COMMON /disp_000_mode/ mode_disp

	if(mode_disp.eq.0) then
	   call drwrel(int(rx),int(ry))
	endif

	return

	end

	subroutine disp_pntrel(rx,ry)

	COMMON /disp_000_mode/ mode_disp

	if(mode_disp.eq.0) then
	   call pntrel(int(rx),int(ry))
	endif

	return

	end

	subroutine disp_initt

	COMMON /disp_000_mode/ mode_disp

	if(mode_disp.eq.0) then
	   call initt(960)
	endif

	return

	end

	subroutine disp_vecmod

	COMMON /disp_000_mode/ mode_disp

	if(mode_disp.eq.0) then
	   call vecmod
	endif

	return

	end

	subroutine disp_anmode

	COMMON /disp_000_mode/ mode_disp

	if(mode_disp.eq.0) then
	   call anmode
	endif

	return

	end

	subroutine disp_text(x,y,size,ang,string)

	character*(*) string

	COMMON /disp_000_mode/ mode_disp

	if(mode_disp.eq.0) then
	   lstring=lengthc(string)
	   if(lstring.gt.0) then
		call disp_posabs(x,y)
CVMS		write(6,'(A)') '+'//string(1:lstring)
		write(6,'(A)') string(1:lstring)
	   endif
	elseif(mode_disp.eq.1) then
	   call las_text(x,y,size,ang,string)
	endif

	return

	end

	subroutine disp_home

	COMMON /disp_000_mode/ mode_disp

	if(mode_disp.eq.0) then
	   call home
	endif

	return

	end

	subroutine disp_dcursr(ix,iy,iz)

	COMMON /disp_000_mode/ mode_disp

	if(mode_disp.eq.0) then
	   call dcursr(ix,iy,iz)
	endif

	return

	end

	subroutine disp_setmode(mode)

	COMMON /disp_000_mode/ mode_disp

	mode_disp=mode

	return

	end

	subroutine disp_showmode(mode)

	COMMON /disp_000_mode/ mode_disp

	mode=mode_disp

	return

	end

	subroutine disp_tek_mode(xterm,tekmode)

	COMMON /disp_000_mode/ mode_disp
	logical xterm,tekmode

	if(mode_disp.eq.0 .and. xterm) then
	  if(.not.tekmode) then
	   call toutpc(27)	! Xterm VT100==>Tek
	   call toutpc(91)	! <ESC>[?38h
	   call toutpc(63)
	   call toutpc(51)
	   call toutpc(56)
	   call toutpt(104)
	   tekmode=.true.
	  endif
	endif

	return

	end

	subroutine disp_vt_mode(xterm,tekmode)

	COMMON /disp_000_mode/ mode_disp
	logical xterm,tekmode

	if(mode_disp.eq.0 .and. xterm) then
	  if(tekmode) then
	   call toutpc(27)	! Xterm Tek==>VT100
	   call toutpt(3)	! <ESC><EXT>	   	   
	   tekmode=.false.
	  endif
	endif

	return

	end
