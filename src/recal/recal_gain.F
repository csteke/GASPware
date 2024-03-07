	program recal_gain

	PARAMETER (MAXGER=256)
	PARAMETER (MAXORD=10)

	real temp(0:maxord)
	character*60 calfilei
	character*60 calfileo

	call xinpmode(1)
	call inp_str('file with the old recal. coefficients',calfilei)
	luni=31
	open(unit=luni,file=calfilei,form='FORMATTED',status='OLD')

	gain=1.
	offset=0.
	call inp_r1('Please input GAIN [,OFFSET] to be applied to these coefficients  ',gain,offset)

	calfileo=calfilei
	call inp_str('file with the new recal. coefficients',calfileo)
	luno=32
	open(unit=luno,file=calfileo,form='FORMATTED',status='NEW')

20	read(luni,*,END=30,ERR=30) iltap,iladc,nlco,(temp(j),j=0,nlco-1)
	nlord=nlco-1
	if(iladc.GE.0 .AND. iladc.LT.MAXGER .AND.
	1  nlord.GT.0 .AND. nlord.LE.MAXORD) then
	  write(6,   '(I5,I5,I3,F10.3,F10.6,<nlord>G14.6)') iltap,iladc,nlco,(temp(j),j=0,nlord)
	  do ii=0,nlord
	    temp(ii)=temp(ii)*gain
	  enddo
	  temp(0)=offset+temp(0)
	  write(6,   '(5X,5X,3X,F10.3,F10.6,<nlord>G14.6)')                  (temp(j),j=0,nlord)
	  write(luno,'(I5,I5,I3,F10.3,F10.6,<nlord>G14.6)') iltap,iladc,nlco,(temp(j),j=0,nlord)
	endif
	goto 20

30	call exit

	end
