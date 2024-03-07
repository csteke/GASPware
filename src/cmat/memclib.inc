#include "../libr/types.def"

	IMPLICIT NONE

	integer maxnseg,minpercent,maxbuf
	
	PARAMETER (MAXNSEG=16*1024)
	PARAMETER (MINPERCENT=50)

	integer memc_used,memc_nseg,memc_times
	integer memc_first,memc_last
	integer memc_mmc(0:MAXNSEG-1)
	common/memccom0/memc_used,memc_nseg,memc_times,
	1	memc_first,memc_last,memc_mmc

	structure /entry/
	  integer start
	  integer end
	  integer size
	  integer id
	  integer times
	  integer prev
	  integer next
	endstructure
	record /entry/ mmc(0:MAXNSEG-1)
	common/memccom1/mmc

	MACHINE_POINTERSIZE memc_base

	integer memc_size
	common /memccom2/ memc_base,memc_size

	PARAMETER (MAXBUF=64*1024)
	integer memc_buf(0:MAXBUF-1)
	common /memccom3/ memc_buf
