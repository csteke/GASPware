	program recal_gain

	PARAMETER MAXGER=256
	PARAMETER MAXORD=10
	PARAMETER MAXFILES = 10

	real temp(0:maxord)

	character*60 calfilei(MAXFILES)
	integer      ndet    (MAXFILES)

	character*60 calfileo

	character*80 line

	call xinpmode(1)

	luni = 31
	luno = 32

	nf=0
	ndet(1)=105
	ndet(2)=104
	ndet(3)=30
	do ii=1,MAXORD
	  write(line,'(''File'',i3)') ii
	  lline=lengthc(line)
	  nn=inp_str(line(1:lline),calfilei(ii))
	  if(nn.le.0) goto 10
	  nf=nf+1
	  call inp_i1('#of detectors for '//line(1:lline),ndet(nf))
	enddo

10	if(nf.le.1) call exit
	call inp_str('file with the new recal. coefficients',calfileo)
	open(unit=luno,file=calfileo,form='FORMATTED',status='NEW')

	noff=0
	do nn = 1,nf
	  open(unit=luni,file=calfilei(nn),form='FORMATTED',status='OLD')
20	  read(luni,*,END=30,ERR=30) iltap,iladc,nlco,(temp(j),j=0,nlco-1)
          nlord=nlco-1
	  write(6,   '(I5,I5,I3,F10.3,F10.6,<nlord>G14.6)') iltap,iladc+noff,nlco,(temp(j),j=0,nlord)
	  write(luno,'(I5,I5,I3,F10.3,F10.6,<nlord>G14.6)') iltap,iladc+noff,nlco,(temp(j),j=0,nlord)
	  goto 20
30	  noff = noff + ndet(nn)
        enddo

	end
