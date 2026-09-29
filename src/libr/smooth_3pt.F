	subroutine smooth_3pt(arr_in, arr_out, n)
	implicit none
	integer n, ii, pass
	real arr_in(0:n-1), arr_out(0:n-1)
	real temp(0:131071)

	if (n .le. 1) then
	   if (n .eq. 1) arr_out(0) = arr_in(0)
	   return
	endif

	do ii = 0, n-1
	   arr_out(ii) = arr_in(ii)
	enddo

	do pass = 1, 2
	   do ii = 0, n-1
	      temp(ii) = arr_out(ii)
	   enddo
	   ! 3-point binomial smoothing [0.25, 0.50, 0.25] with boundary reflection
	   arr_out(0) = 0.75 * temp(0) + 0.25 * temp(1)
	   do ii = 1, n-2
	      arr_out(ii) = 0.25 * temp(ii-1) + 0.50 * temp(ii) + 0.25 * temp(ii+1)
	   enddo
	   arr_out(n-1) = 0.25 * temp(n-2) + 0.75 * temp(n-1)
	enddo

	return
	end
