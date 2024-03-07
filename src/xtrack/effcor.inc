#define MAXCOEF 5
#define MAXCHAN  16384
	integer efftype
	real  coeff(MAXCOEF)
	character*128 fname
	real  spec(0:MAXCHAN-1)
	integer nchan
	common /effic/ efftype,coeff,fname,spec,nchan
