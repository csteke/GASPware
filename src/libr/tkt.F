D	program tkt_test
D
D	open(6,carriagecontrol='FORTRAN',status='OLD')
D
D	call initt
D	call movabs(100,100)
D	call drwabs(900,100)
D	call drwabs(900,700)
D	call drwabs(100,700)
D	call drwabs(100,100)
D
D	ll=0
D10	call dcursr(ich,ix,iy)
D
D	call movabs(10,760-ll*35)
D	call anmode
D
D	write(6,*) 'TKT test  '//char(ich),ix,iy
D	ll=ll+1
D	if(ich.ge.32) goto 10
D
D	call exit
D
D	end

	subroutine tkt
	return
	end

	subroutine initt
	IMPLICIT NONE
	call erase
	return
	end

	subroutine home
	IMPLICIT NONE
	integer xmin
	parameter (xmin =0)
	integer yhome
	parameter (yhome=767)
	call movabs(xmin,yhome)
	return
	end

	subroutine erase
	IMPLICIT NONE
	integer gmode
	parameter (gmode=29)
	integer homec
	parameter (homec=12)
	integer pfix
	parameter (pfix =27)
	integer xmin
	parameter (xmin =0)
	integer yhome
	parameter (yhome=767)
	call TKT__alloc
	call tkt__send(gmode)
	call tkt__send(pfix)
	call tkt__send(homec)
	call tkt__set(xmin,yhome)
	call tkt__flush
	return
	end

	subroutine vecmod
	entry      pntmod
	IMPLICIT NONE
	integer gmode
	parameter (gmode=29)
	call TKT__alloc
	call tkt__send(gmode)
	return
	end

	subroutine anmode
	IMPLICIT NONE
	integer amode
	parameter (amode=31)
	call tkt__send(amode)
	call tkt__flush
	return
	end

	subroutine vector
	IMPLICIT NONE
	return
	end

	subroutine alfa
	IMPLICIT NONE
	return
	end

	subroutine movabs(ix,iy)
	IMPLICIT NONE
	integer gmode
	parameter (gmode=29)
	integer ix,iy
	call tkt__set(ix,iy)
	call tkt__send(gmode)
	call tkt__pltcom
	return
	end

	subroutine movrel(ix,iy)
	IMPLICIT NONE
	integer gmode
	parameter (gmode=29)
	integer ix,iy
	call tkt__inc(ix,iy)
	call tkt__send(gmode)
	call tkt__pltcom
	return
	end

	subroutine drwabs(ix,iy)
	IMPLICIT NONE
	integer ix,iy
	call tkt__set(ix,iy)
	call tkt__pltcom
	return
	end

	subroutine drwrel(ix,iy)
	IMPLICIT NONE
	integer ix,iy
	call tkt__inc(ix,iy)
	call tkt__pltcom
	return
	end

	subroutine pntabs(ix,iy)
	include 'tkt.inc'
	integer ix,iy
	call tkt__set(ix,iy)
	ipunt=ipunt+1
	buf(ipunt)=gmode
	call tkt__flush
	call tkt__cnvt
	if(yhig_n.ne.yhig) then
	   yhig=yhig_n
	   call tkt__send(yhig)
	endif
	if(xhig_n.ne.xhig) then
	   ylow=ylow_n
	   call tkt__send(ylow)
	   xhig=xhig_n
	   call tkt__send(xhig)
	elseif(ylow_n.ne.ylow) then
	   ylow=ylow_n
	   call tkt__send(ylow)
	endif
	xlow=xlow_n
	call tkt__send(xlow)
	call tkt__cnvt
	if(yhig_n.ne.yhig) then
	   yhig=yhig_n
	   call tkt__send(yhig)
	endif
	if(xhig_n.ne.xhig) then
	   ylow=ylow_n
	   call tkt__send(ylow)
	   xhig=xhig_n
	   call tkt__send(xhig)
	elseif(ylow_n.ne.ylow) then
	   ylow=ylow_n
	   call tkt__send(ylow)
	endif
	xlow=xlow_n
	call tkt__send(xlow)
	call tkt__flush
	return
	end

	subroutine pntrel(ix,iy)
	IMPLICIT NONE
	integer ix,iy
	call movrel(ix,iy)
	call drwrel(0,0)
	return
	end

	subroutine toutpt(ch)
	IMPLICIT NONE
	character*1 ch
	return
	end

	subroutine toutpc(ch)
	IMPLICIT NONE
	character*1 ch
	return
	end

	subroutine ttstat
	IMPLICIT NONE
	return
	end

	subroutine dcursr(ich,ix,iy)
	IMPLICIT NONE
	integer strtx
	parameter (strtx=26)
	integer pfix
	parameter (pfix =27)
	integer ich,ix,iy
10	call tkt__send(pfix)
	call tkt__send(strtx)
	call tkt__flush
	call tkt__enqcom(ich,ix,iy)
	if(ich.le.0) goto 10
c	if(ich.gt.96 .and. ich. le.122) ich=ich-32
	return
	end

	subroutine tinput(ch)
	IMPLICIT NONE
	integer stat
	parameter (stat = 5)
	integer pfix
	parameter (pfix =27)
	integer ich,ix,iy
	character*1 ch
	call tkt__send(pfix)
	call tkt__send(stat)
	call tkt__flush
	call tkt__enqcom(ich,ix,iy)
	ch=char(ich)
	return
	end

	subroutine line_style
	IMPLICIT NONE
	return
	end

	subroutine new_color
	IMPLICIT NONE
	return
	end

	subroutine tkt__set(x,y)
	include 'tkt.inc'
	integer x,y
	xpos=min(max(x,xmin),xmax)
	ypos=min(max(y,ymin),ymax)
	return
	end

	subroutine tkt__inc(x,y)
	include 'tkt.inc'
	integer x,y
	xpos=min(max(xpos+x,xmin),xmax)
	ypos=min(max(ypos+y,ymin),ymax)
	return
	end

	subroutine tkt__send(val)
	include 'tkt.inc'
	integer val
	ipunt=ipunt+1
	buf(ipunt)=iand(val,127)
	if(ipunt.eq.buflen) call tkt__flush
	return
	end

	subroutine tkt__pltcom
	include 'tkt.inc'
	call tkt__cnvt
	if(yhig_n.ne.yhig) then
	   yhig=yhig_n
	   call tkt__send(yhig)
	endif
	if(xhig_n.ne.xhig) then
	   ylow=ylow_n
	   call tkt__send(ylow)
	   xhig=xhig_n
	   call tkt__send(xhig)
	elseif(ylow_n.ne.ylow) then
	   ylow=ylow_n
	   call tkt__send(ylow)
	endif
	xlow=xlow_n
	call tkt__send(xlow)
	return
	end	

	subroutine tkt__cnvt
	include 'tkt.inc'
	integer val
	val=min(max(ypos,ymin),ymax)
	yhig_n=val/32+32
	ylow_n=iand(val,31)+96
	val=min(max(xpos,xmin),xmax)
	xhig_n=val/32+32
	xlow_n=iand(val,31)+64
	return
	end	

	subroutine tkt__flush
	include 'tkt.inc'
	integer ii
	character*4 nmfmt
	if(ipunt.le.0) then
	   ipunt=0
	   return
	endif
CVMS	write(6,'(1h+,<ipunt>a,$)') (char(buf(ii)),ii=1,ipunt)
	write(nmfmt,'(i4)')ipunt
	write(6,'('//nmfmt//'a,$)') (char(buf(ii)),ii=1,ipunt)
	call flush(6)
	ipunt=0
	return
	end

	subroutine tkt__enqcom(ich,ix,iy)
	include 'tkt.inc'
	character*10 chbuf
	integer ii,ich,ix,iy,ii1,ii2
	integer ichar
	ich=0
#if defined( __GFORTRAN__ )
	read(5,'(a)',err=100,end=100) chbuf
	ii = len(chbuf)
#else
	read(5,'(q,a)',err=100,end=100) ii,chbuf
#endif
	if(ii.ne.5) return
	ii1=ichar(chbuf(1:1))
	ich=iand(ii1,127)
	ii1=ichar(chbuf(2:2))
	ii2=ichar(chbuf(3:3))
	ix =iand(ii1,31)*32+iand(ii2,31)
	ii1=ichar(chbuf(4:4))
	ii2=ichar(chbuf(5:5))
	iy =iand(ii1,31)*32+iand(ii2,31)
100	return
	end
	
	subroutine tkt__alloc
	return
	end
