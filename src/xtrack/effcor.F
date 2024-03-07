*  Calculate detection efficiency interactivelly defined
*
	logical function eff_define ()

#include "effcor.inc"
	
	CHARACTER*10 CMD(0:2)	/'NONE','FUNCTION','SPECTRUM'/
	character*9 strk


	write(6,'(''   Possible choices are :'')')
	write(6,'(''         FUNCTION - polynomial coefficients of'')')
	write(6,'(''                    ln(Eff) = F( ln(Energy) )'')')
	write(6,'(''         SPECTRUM - spectrum containing the efficiency value'')')
	write(6,'(''                    for each energy unit (e.g. 1keV/ch)'')')
	write(6,'(''         NO       - do not consider efficiency correction'')')
	write(6,'(''   Current definition is '',a)')cmd(efftype)

	lnch=INP_CMD('(F)unction,(S)pectrum,None ',CMD(0),3,ICMD)
	
	if( lnch .eq. -1 )then
	     eff_define = efftype .gt. 0
	     return
	endif
	
	if( ICMD .eq. 1 )then
	     efftype = 0
	     eff_define = .FALSE.
	     	
	elseif( ICMD .eq. 2 )then
	     do ii=1,MAXCOEF
#if defined( __GFORTRAN__ )
	        write(6,'('' A('',i1,'')   = '',1pg0)') ii-1,coeff(ii)
#else
	        write(6,'('' A('',i1,'')   = '',1pg)') ii-1,coeff(ii)
#endif
	     enddo
	     efftype = 1
	     eff_define = .TRUE.
	     write(6,'('' New efficiency function'')')
	     do ii=1,MAXCOEF
	     	write(strk,'(''A('',i1,'')  '')') ii-1
	     	lnch=INP_R1(strk,coeff(ii))
	     	if(lnch.eq.-1) return
	     end do
	     return
	     
	elseif( ICMD .eq. 3 )then
	     nkap = 16
             call readdat(1,fname,spec(0),nkap*1024,ifrm,kv)
	     lfnamei=max(1,lengthc(fname))
	     if( kv .lt. 0 )then
	        write(6,*) '   ERROR reading ',fname(1:lfnamei)
		eff_define = .FALSE.
		efftype = 0
		return
	     endif
	     nchan = kv/100
	     do ii=0, nchan-1
	        if( spec(ii) .lt. 0.000000001 ) then
		  write(6,'(''  Inconsistent (negative or too close to 0) values in '',a)')fname(1:lfnamei)
		  eff_define = .FALSE.
		  efftype = 0
		  return
		endif
	     enddo
	     efftype = 2
	     eff_define = .TRUE.
	     return
	endif
	
	efftype = 0
	eff_define = .FALSE.
	return
	end
	
	
	real function effvalue ( x )

	real x
	real*8 xx, yy, aa

#include "effcor.inc"	
	
	if( efftype .eq. 0 ) then
	    effvalue  = 1.000000000
	    return
	elseif( efftype .eq. 1 ) then
	    xx = x
	    if( xx .lt. 0.00001 )then
	       write(6,'(''  WARNING - too small value for energy'')')
	       effvalue = 1.0000
	       return
	    endif
	    xx = log(x)
	    yy = coeff(MAXCOEF)
	    do ii = MAXCOEF-1,1,-1
	       aa = coeff(ii)
	       yy = aa + xx*yy
	    enddo
	    yy = exp(yy)
	    effvalue = yy
	    return
	elseif( efftype .eq. 2) then
	    ii = nint( x )
	    if( ii .lt. 0 )then
	       effvalue = 1.000
	       write(6,'(''  WARNING - negative value for energy'')')
	    elseif( ii .ge. nchan )then
	       effvalue = 1.000
	       write(6,'(''  WARNING - too large value for energy'')')
	    else
	       effvalue = spec(ii)
	    endif
	    return
	endif
	
	effvalue = 1.00000
	return
	end
	
