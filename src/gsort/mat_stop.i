# 1 "mat_stop.F"
	PROGRAM MAT_STOP

!  CREA IL FILE STOP_MATRIX   PRT FERMARE L'ESECUZIONE DI MAT2_LNL

	OPEN(2,FILE='STOP_MATRIX',STATUS='UNKNOWN')
	WRITE(2,*) 'STOP_MATRIX'
	CLOSE(2)

	END