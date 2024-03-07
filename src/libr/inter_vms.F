CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC						CCC
CCC	Program package to substitute system calls	CCC
CCC    originating from the usage of the VMS system	CCC
CCC						CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC
	logical*1 function lib$lun()
CCC

	logical*1 lib$get_lun
	logical*1 lib$free_lun
	integer	luns(100),lun
	data 	luns	/100*0/
C
	lib$lun=.FALSE.
C
C  Get a free logical*1 unit
C
	entry lib$get_lun(lun)
	lib$get_lun=.FALSE.
	do i=20,100
	  if (luns(i).eq.0) then
	    lun=i
	    luns(i)=1
	    lib$get_lun=.TRUE.
	    return
	  endif
	enddo
	return
C
C  Unlock a logical unit
C
	entry lib$free_lun(lun)
	lib$free_lun=.FALSE.
	if (lun.ge.20.and.lun.le.100) then
	  luns(lun)=0
	  lib$free_lun=.TRUE.
	endif
	return
	end
cC
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cCCC
	subroutine lib$wait(time)
ccCCC
#if !defined( __GFORTRAN__ )
	external 	sleep
#endif
	real 		time
	integer*4	itime
cC
	itime=min(1.,time)
	call sleep(itime)
	return
	end
cC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC
	subroutine lib$spawn(command)
CCC
	character*(*) command

	call system(command)

	return

	end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC
	subroutine str$trim(txt1,txt2,il)
CCC
	character*(*) txt1,txt2
	integer il
C
	txt2=txt1
	il=0
	call str_trim(txt2,0)
	il=lengthc(txt2)

	return

	end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC
	subroutine str$upcase(txt1,txt2)
CCC
	character*(*) txt1,txt2
C
	txt2=txt1
	call str_toupper(txt2)

	return

	end
cC
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cCCC
c	subroutine ux_readspec(bfname,spek,bform,ncan,kv)
c	
c	byte bfname(0:1),bform(0:1)
c	integer spek,ncan,kv
c
c	character fname*256,form*256
c
c	fname= ' '
c	ii=0
c	dowhile(bfname(ii).ne.0)
c	  fname(ii:ii)=char(ii)
c	  ii=ii+1
c	enddo
c
c	form= ' '
c	ii=0
c	dowhile(bform(ii).ne.0)
c	  form(ii:ii)=char(ii)
c	  ii=ii+1
c	enddo
c
c	call readspecr(fname,spek,form,ncan,kv)
c
c	return
c
c	end
