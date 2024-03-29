# 1 "laslib.F"
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!C
C                                                                       C
C  This package was taken from routines written at M.S.I. in Stockholm  C
C  Routines obtained through NBI software                                                                   C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!C

	subroutine las_open(Filename,LUN)

	character*(*) Filename
	character string*80
	integer LUN

	logical*1 lasopen,lib$get_lun,lib$free_lun,istat
	external lib$get_lun,lib$free_lun
	character*12 symlas
	character*100 uhome
	integer luhome
	integer ncoor
	common /las_000_com/ ncoor,TSLAS,GRLAS,WLLAS,SYMLAS,laslun,lasopen

# 1 "./laslib.inc" 1 
	parameter ( N_INIT_STR = 92 )
	character*80 PS_INIT_STR(N_INIT_STR)
	
	data PS_INIT_STR /
     1 "%!PS-Adobe									 ",   
     1 "statusdict (waittimeout) 10 put 						 ",   
     1 "%General PS definitions.							 ",   
     1 "/l {lineto} def   /m {moveto} def /rl {rlineto} def /rm {rmoveto} def		 ",   
     1 "/sg {setgray} def /r {rotate} def /sc {scale} def   /tr {translate} def 	 ",   
     1 "/gs {gsave} def /gr {grestore} def /st {stroke} def				 ",   
     1 "/s {show} def									 ",   
     1 "/cm {28.3465 mul} def	  %1 cm = 28.3465 units 				 ",   
     1 "/mm {2.83465 mul} def	  %1 mm = 2.83465 units 				 ",   
     1 "/in {72.0000 mul} def	  %1 in = 72.0000 units 				 ",   
     1 "/pi {3.141592654} def								 ",   
     1 "/slw {setlinewidth} def 							 ",   
     1 "/ssf {scalefont setfont} def /trf {/Times-Roman findfont} def			 ",   
     1 "/syf {/Symbol findfont} def 2 setlinejoin					 ",   
     1 "%scale plot.									 ",   
     1 "%0.992 1.008 sc       %Rescale X,Y slightly so that they are equal		 ",   
     1 "%			      %on Apple laserprinter.				 ",   
     1 "0.9955752 1.0027855 sc    %Rescale X,Y slightly so that they are equal  	 ",   
     1 "%			      %on LN03 at NBITAL				 ",   
     1 "%15 mm 5 mm tr        %Origo, Apple Laserprinter, AFI				 ",   
     1 "%5 mm 25 mm tr        %Origo, Digital Laserprinter LN03, NBITAL 		 ",   
     1 "12 mm 7 mm tr	      %Origo, Digital Laserprinter LN03, NBITAL 		 ",   
     1 "%0.0195 mm 0.0195 cm sc   %scale plot so that 1000 units = 19.5 cm.		 ",   
     1 "%			      %i.e. 1 unit = 19.5/1000 cm = 0.195 mm		 ",   
     1 "1 mm 1 mm sc		      %scale plot so that 1 unit = 1 mm.		 ",   
     1 "trf 1 cm ssf		      %/Times Roman size = 10 units			 ",   
     1 "syf 1 cm ssf			%/Symbol size = 10 units			 ",   
     1 "%0.5 slw		%Set default linewidth to be 0.5 units  		 ",   
     1 "%				%(i.e. slightly LT 0.1 mm.			 ",   
     1 "%				%0.5128205 units=10./19.5 units=0.01 cm=0.1 mm   ",   
     1 "0.1 slw 		%Set default linewidth to be 0.1 units  		 ",   
     1 "0 sg				%Set default grayness to be 0, which is black.   ",   
     1 "%Define plotsymbols, box,triangle, etc. 					 ",   
     1 "%/plus for filling								 ",   
     1 "%/cross for filling								 ",   
     1 "%/star for filling								 ",   
     1 "%/pentagon									 ",   
     1 "%/hbar  									 ",   
     1 "/box {newpath 0 0 m -0.5 -0.5 rm 0 1 rl 1 0 rl 0 -1 rl closepath} def		 ",   
     1 "/bf		%Filled box							 ",   
     1 "   {/g exch def /ys exch def /xs exch def /a exch def /y exch def /x exch def	 ",   
     1 "   /cmtx matrix currentmatrix def	 %Store the CTM in the varible ctmx	 ",   
     1 "   gs x y tr a r xs ys sc box cmtx setmatrix g sg fill gr} def  		 ",   
     1 "/bo		%Open box							 ",   
     1 "   {/g exch def /ys exch def /xs exch def /a exch def /y exch def /x exch def	 ",   
     1 "   /cmtx matrix currentmatrix def						 ",   
     1 "   gs x y tr a r xs ys sc box cmtx setmatrix g sg st gr} def			 ",   
     1 "/plus										 ",   
     1 "   {/g exch def /ys exch def /xs exch def /a exch def /y exch def /x exch def	 ",   
     1 "   /cmtx matrix currentmatrix def gs x y tr a r xs ys sc newpath		 ",   
     1 "   -0.5 0 m 0.5 0 l 0 -0.5 m 0 0.5 l cmtx setmatrix g sg st gr} def		 ",   
     1 "/r1 {0.5 2 sqrt div} def							 ",   
     1 "/cross  									 ",   
     1 "   {/g exch def /ys exch def /xs exch def /a exch def /y exch def /x exch def	 ",   
     1 "   /cmtx matrix currentmatrix def gs x y tr a r xs ys sc newpath r1 neg 	 ",   
     1 "   r1 neg m r1 r1 l r1 neg r1 m r1 r1 neg l cmtx setmatrix g sg st gr} def	 ",   
     1 "/star										 ",   
     1 "   {/g exch def /ys exch def /xs exch def /a exch def /y exch def /x exch def	 ",   
     1 "   /cmtx matrix currentmatrix def gs x y tr a r xs ys sc newpath -0.5 0 m	 ",   
     1 "   0.5 0 l 0 -0.5 m 0 0.5 l r1 neg r1 neg m r1 r1 l r1 neg r1 m r1 r1 neg l	 ",   
     1 "   cmtx setmatrix g sg st gr} def						 ",   
     1 "/ef		%Filled ellipse (or circle if xs=ys)				 ",   
     1 "  {/g exch def /endangle exch def /startangle exch def /yrad exch def		 ",   
     1 "   /xrad exch def /angle exch def /y exch def /x exch def			 ",   
     1 "   /cmtx matrix currentmatrix def gs x y tr angle r xrad yrad sc newpath	 ",   
     1 "   0 0 0.5 startangle endangle arc cmtx setmatrix g sg fill gr} def		 ",   
     1 "/eo		%Open ellipse (or circle if xs=ys)				 ",   
     1 "  {/g exch def /endangle exch def /startangle exch def /yrad exch def		 ",   
     1 "   /xrad exch def /angle exch def /y exch def /x exch def			 ",   
     1 "   /cmtx matrix currentmatrix def gs x y tr angle r xrad yrad sc newpath	 ",   
     1 "   0 0 0.5 startangle endangle arc cmtx setmatrix g sg st gr} def		 ",   
     1 "/xt {0.5} def /y1t {xt 3 sqrt div} def /y2t {2 xt mul 3 sqrt div} def		 ",   
     1 "/triangle {newpath xt neg y1t neg m 0 y2t l xt y1t neg l closepath} def 	 ",   
     1 "/tf		%Filled triangle						 ",   
     1 "   {/g exch def /ys exch def /xs exch def /a exch def /y exch def /x exch def	 ",   
     1 "   /cmtx matrix currentmatrix def gs x y tr a r xs ys sc triangle		 ",   
     1 "   cmtx setmatrix g sg fill gr} def						 ",   
     1 "/to		%Open triangle  						 ",   
     1 "   {/g exch def /ys exch def /xs exch def /a exch def /y exch def /x exch def	 ",   
     1 "    /cmtx matrix currentmatrix def gs x y tr a r xs ys sc triangle		 ",   
     1 "    cmtx setmatrix g sg st gr} def						 ",   
     1 "/h1 {0.5 3 sqrt div} def /h2 {h1 2 mul} def /h3 {h1 3 sqrt mul} def		 ",   
     1 "/hexagone {newpath h1 h3 m h2 0 l h1 h3 neg l h1 neg h3 neg l h2 neg 0 l	 ",   
     1 "    h1 neg h3 l closepath} def  						 ",   
     1 "/hf		%Filled hexagone						 ",   
     1 "   {/g exch def /ys exch def /xs exch def /a exch def /y exch def /x exch def	 ",   
     1 "   /cmtx matrix currentmatrix def gs x y tr a r xs ys sc hexagone		 ",   
     1 "   cmtx setmatrix g sg fill gr} def						 ",   
     1 "/ho		%Open hexagone  						 ",   
     1 "   {/g exch def /ys exch def /xs exch def /a exch def /y exch def /x exch def	 ",   
     1 "   /cmtx matrix currentmatrix def gs x y tr a r xs ys sc hexagone		 ",   
     1 "   cmtx setmatrix g sg st gr} def						 " /  
# 22 "laslib.F" 2 

	if(lasopen) then
	   write(6,*) 'Laser file already open'
	   goto 100
	endif

	istat=lib$get_lun(laslun)
	if(.not.istat) then
*	   call segnala(istat)
	   goto 100
	endif
	lun=laslun
CVMS	OPEN(LASLUN,FILE=FILENAME,STATUS='NEW',DEFAULTFILE='.ps',
# 37

	OPEN(LASLUN,FILE=FILENAME,STATUS='UNKNOWN',CARRIAGECONTROL='LIST',ERR=100)

	LASOPEN=.TRUE.
	ncoor=0

*	istat=lib$get_lun(laslunscr)
*	if(.not.istat) then
*	   call segnala(istat)
*	   goto 100
*	endif
*	call getenv('HOME',uhome)
*	luhome=lengthc(uhome)
*#ifdef PClinux
*	OPEN(LASLUNSCR,NAME=uhome(1:luhome)//'/.laslib.init',STATUS='OLD')
*#else
*	OPEN(LASLUNSCR,NAME=uhome(1:luhome)//'/.laslib.init',STATUS='OLD',READONLY,SHARED)
*#endif
*	DO WHILE (.TRUE.)
*	  READ(LASLUNSCR,'(A)',END=50,ERR=50) STRING
* 	  CALL las_000_WRSTRING(STRING)
*	ENDDO
*
*50	CLOSE(LASLUNSCR)
*	istat=lib$free_lun(laslunscr)

       DO ii = 1, N_INIT_STR
	 CALL las_000_WRSTRING(PS_INIT_STR(ii))
       ENDDO


	TSLAS=10.0	! Text size 10 units.
	WRITE(string,'(''trf '',F8.1,'' ssf'')') TSLAS
	CALL LAS_000_WRSTRING(string)

	WLLAS=0.1	! Line width 0.1 unit
	WRITE(string,'(F8.1,'' slw'')') WLLAS
	CALL LAS_000_WRSTRING(string)

	GRLAS=0.0	! Set default grayness to be 0, which is black.
	WRITE(string,'(F8.1,'' sg'')') GRLAS
	CALL LAS_000_WRSTRING(string)

	SYMLAS=' '	! Set default symbol to be : ' '

	return

100	lun=-1
	LASOPEN=.false.
	call ansi_bell(6)
	return

	end

	subroutine las_close(ncopies)

	logical*1 lasopen
	character*12 symlas
	integer ncoor
	common /las_000_com/ ncoor,TSLAS,GRLAS,WLLAS,SYMLAS,laslun,lasopen

	if(.not.lasopen) then
	   write(6,*) 'Laser file not open'
	   call ansi_bell(6)
	   return
	endif

	CALL las_000_wrstring('st')
	nrcopies=ncopies
	IF(NRCOPIES.GT.1) THEN
	   IF(NRCOPIES.GT.10) NRCOPIES=10
	   DO I=1,NRCOPIES-1
	      CALL las_000_wrstring('copypage')
	   ENDDO
	ENDIF
	CALL las_000_WRSTRING('showpage')

	close(UNIT=laslun)
	call lib$free_lun(laslun)
	lasopen=.false.

	return

	end

	subroutine las_move(rx,ry)

	character string*80

	logical*1 lasopen
	character*12 symlas
	integer ncoor
	common /las_000_com/ ncoor,TSLAS,GRLAS,WLLAS,SYMLAS,laslun,lasopen

	if(.not.lasopen) then
	   write(6,*) 'Laser file not open'
	   call ansi_bell(6)
	   return
	endif

	WRITE(string,'(f8.1,1x,f8.1,'' m'')') RX,RY
	CALL las_000_wrstring(string)

	ncoor=ncoor+1
	if(ncoor.gt.200) then
	   WRITE(string,'(''st '',f8.1,1x,f8.1,'' m'')') RX,RY
	   CALL las_000_wrstring(string)
	   ncoor=0
	endif

	return

	end

	subroutine las_draw(rx,ry)

	character string*80

	logical*1 lasopen
	character*12 symlas
	integer ncoor
	common /las_000_com/ ncoor,TSLAS,GRLAS,WLLAS,SYMLAS,laslun,lasopen

	if(.not.lasopen) then
	   write(6,*) 'Laser file not open'
	   call ansi_bell(6)
	   return
	endif

	WRITE(string,'(f8.1,1x,f8.1,'' l'')') RX,RY
	CALL las_000_wrstring(string)

	ncoor=ncoor+1
	if(ncoor.gt.200) then
	   WRITE(string,'(''st '',f8.1,1x,f8.1,'' m'')') RX,RY
	   CALL las_000_wrstring(string)
	   ncoor=0
	endif

	return

	end

	subroutine las_origin(x0,y0)
		
	character string*80

	logical*1 lasopen
	character*12 symlas
	integer ncoor
	common /las_000_com/ ncoor,TSLAS,GRLAS,WLLAS,SYMLAS,laslun,lasopen

	if(.not.lasopen) then
	   write(6,*) 'Laser file not open'
	   call ansi_bell(6)
	   return
	endif

	WRITE(string,'(f8.1,1x,f8.1,'' tr'')') x0,y0
	CALL las_000_wrstring(string)

	return

	end

	subroutine las_landscape
		
	logical*1 lasopen
	character*12 symlas
	integer ncoor
	common /las_000_com/ ncoor,TSLAS,GRLAS,WLLAS,SYMLAS,laslun,lasopen

	if(.not.lasopen) then
	   write(6,*) 'Laser file not open'
	   call ansi_bell(6)
	   return
	endif

	CALL las_000_wrstring('90.0 r ')

	return

	end

	subroutine las_text(x,y,size,ang,txt)

C	This routine searches the string TXT for special symbols like super- or
C	subscripts, greek-letters, h-bar, etc. (enclosed between backslash
C	symbols) and manipulates the special symbols so that they will appear
C	properly in PS.
C	The subroutine WRSTRING, which writes the text into the PS-file is
C	called. If the text between \ \ is not defined the text will be
C	written as it is.
C	Special symbols :
C	Greek lowercase symbol : \alpha\,\beta\,...
C	Greek uppercase symbol : \Alpha\,\bETa\,\GAMMA\,... i.e. if any letter
C	is uppercase result will be uppercase greek symbol.
C	Subscript : \_text\ .
C	Superscript : \^text\ or \**text\ .

C	Example of text with special symbols :
C	TXT='This will give h-bar : \h-bar\'
C	TXT='This will give hbar : \hbar\'
C	TXT='Greek lowercase beta subscript 2 : \beta\\_2\'
C	TXT='Greek uppercase DELTA superscript lowercase beta : \Delta\\^beta\'
C	TXT='Greek uppercase DELTA superscript lowercase beta : \Delta\\**beta\'

	character*(*) txt

	logical*1 lasopen
	character*12 symlas
	integer ncoor
	common /las_000_com/ ncoor,TSLAS,GRLAS,WLLAS,SYMLAS,laslun,lasopen

	CHARACTER XC*8,YC*8,SC*8,CANG*8,SC2*8
	CHARACTER TEXT*200,SC4*8,SC5*8,STR*200,SPEC*200,SUB*8,SUP*8
	CHARACTER FS*8	/'  (F8.2)'/
	INTEGER STR$TRIM,STR$UPCASE
	DATA SIZLAS /1.55/

	BYTE LGC(26)	!LGC=# of characters in each of the Greek symbols.
	DATA LGC /5,4,3,5,7,3, 5,3,4,4,5,6, 2,2,7,2,5,3, 5,3,7,6,5,2, 3,4/
	CHARACTER GC*1,GUC*8
	CHARACTER GREEKLC(26)*8,GREEKUC(26)*8   !Greek UpperCase and LowerCase

	DATA GREEKLC /
     1'alpha   ','beta    ','chi     ','delta   ','epsilon ','phi     ',
     2'gamma   ','eta     ','iota    ','phi1    ','kappa   ','lambda  ',
     3'mu      ','nu      ','omicron ','pi      ','theta   ','rho     ',
     4'sigma   ','tau     ','upsilon ','omega1  ','omega   ','xi      ',
     5'psi     ','zeta    '/
	DATA GREEKUC /
     1'Alpha   ','Beta    ','Chi     ','Delta   ','Epsilon ','Phi     ',
     2'Gamma   ','Eta     ','Iota    ','Phi1    ','Kappa   ','Lambda  ',
     3'Mu      ','Nu      ','Omicron ','Pi      ','Theta   ','Rho     ',
     4'Sigma   ','Tau     ','Upsilon ','Omega1  ','Omega   ','Xi      ',
     5'Psi     ','Zeta    '/

	if(.not.lasopen) then
	   write(6,*) 'Laser file not open'
	   call ansi_bell(6)
	   return
	endif

	SS=SIZLAS*SIZE
	WRITE(SUB,FS)   1./6.*SS	!Subscript   offset
	WRITE(SUP,FS)   2./6.*SS	!superscript offset
	IF(TSLAS.NE.SS) THEN	!We have new text size since last call.
	  WRITE(SC,FS)    SS	!Text size
	  CALL las_000_WRSTRING('trf '//SC//' ssf')
	  TSLAS=SS
	ENDIF
	IF(SIZE.EQ.0.) RETURN
	IS=STR$TRIM(TXT,TXT,IL)
	IF(IL.EQ.0) RETURN
	TEXT=TXT
	XX=X
	YY=Y
	WRITE(XC,'(F8.1)') XX
	WRITE(YC,'(F8.1)') YY
	WRITE(CANG,'(F6.1)') ANG
	CALL las_000_WRSTRING('gs '//XC//' '//YC//' m '//CANG//' r')

	L=1
	J=0
444	J=J+1
	IF(J.GT.IL.OR.J.GT.199) GOTO 400	!All is done or too long string.

C	Search for ( and ) outside \ \ and substitute \( for ( and \) for ) :
	IF(TEXT(J:J).EQ.'('.OR.TEXT(J:J).EQ.')') THEN
	  IF(J.EQ.1) STR='\\'//TEXT(J:IL+1)
	  IF(J.GT.1) STR=TEXT(1:J-1)//'\\'//TEXT(J:IL+1)
	  TEXT=STR
	  J=J+1
	  IL=IL+1
	ENDIF

C	Search for \ \ :
	IF(TEXT(J:J).EQ.'\\') THEN	!J points to the left \ !
	  IF(J-1.GE.L) THEN	!If J-1.GE.L then write text from L up to J-1:
	    CALL las_000_WRSTRING('('//TEXT(L:J-1)//') s')
	  ENDIF
	  L=J
	  DO K=J+1,IL
	    IF(TEXT(K:K).EQ.'\\') GOTO 333 !K points to the right \ !
	  ENDDO
	  GOTO 400
	ENDIF
	GOTO 444
333	CONTINUE
	L=K+1	!L points to the character following the right \ !

	SPEC=TEXT(J+1:K-1)

C	Now we have found a special symbol between \ \ at TEXT(J+1:K-1)
C	and stored this string in SPEC.
C	I.e. TEXT(J:J) = left \ and TEXT(K:K) = right \.

C	Search for ( and ) inside \ \ and substitute ( with \( and ) with \) :
	J1=0
	K1=K-J-1	!K1= number of char. in SPEC.
455	J1=J1+1
	IF(J1.GT.K1) GOTO 456
	IF(SPEC(J1:J1).EQ.'('.OR.SPEC(J1:J1).EQ.')') THEN
	  IF(J1.EQ.1) STR='\\'//SPEC(J1:K1)
	  IF(J1.GT.1) STR=SPEC(1:J1-1)//'\\'//SPEC(J1:K1)
	  SPEC=STR
	  J1=J1+1
	  K1=K1+1
	ENDIF
	GOTO 455
456	CONTINUE

C	Search for super- or subscripts :

	ISS=0
	SSS=SS
	IF(SPEC(1:2).EQ.'**'.OR.SPEC(1:1).EQ.'^'.OR.SPEC(1:1).EQ.'_') THEN
	  SSS=SS*2./3.		!Size of text for sub- or superscript.
	  WRITE(SC,FS)    SSS
	  CALL las_000_WRSTRING('trf '//SC//' ssf') !all text 2/3 smaller
	  IF(SPEC(1:2).EQ.'**') THEN		!Superscript
	    ISS=1
	    CALL las_000_WRSTRING('0 '//SUP//' rm')
	    SPEC=SPEC(3:K1)
	    K1=K1-2
	  ELSEIF(SPEC(1:1).EQ.'^') THEN		!Superscript
	    ISS=1
	    CALL las_000_WRSTRING('0 '//SUP//' rm')
	    SPEC=SPEC(2:K1)
	    K1=K1-1
	  ELSE IF(SPEC(1:1).EQ.'_') THEN		!Subscript
	    ISS=-1
	    CALL las_000_WRSTRING('0 '//SUB//' neg rm')
	    SPEC=SPEC(2:K1)
	    K1=K1-1
	  ENDIF
	ENDIF

C	Search for special symbols :

C	h-bar or hbar :
	STR=SPEC
	I2=STR$UPCASE(STR,STR)
	IF(STR(1:5).EQ.'H-BAR'.OR.STR(1:4).EQ.'HBAR') THEN
C	  WRITE(SC2,FS)   SSS*2./3.
C	  WRITE(SC4,FS)   SSS/4.5
C	  WRITE(SC5,FS)   SSS/3.5
	  WRITE(SC2,FS)   SSS*0.55
	  WRITE(SC4,FS)   SSS/7.	
	  WRITE(SC5,FS)   SSS/3.
	  CALL las_000_WRSTRING('(h) s gs')
	  CALL las_000_WRSTRING(
C	1	SC4//' neg '//SC2//' rm '//
C	2	SC5//' neg '//SC5//' neg rl '//
C	3	SC5//' '//SC5//' rm '//
C	4	SC4//' '//SC2//' neg rm')
     1	SC4//' neg '//SC2//' rm '//
     2	SC5//' neg 0 neg rl '//
     3	SC5//' 0 rm '//
     4	SC4//' '//SC2//' neg rm')
	  CALL las_000_WRSTRING(SC//' 15 div slw stroke gr')
	  GOTO 111
	ENDIF

C	Multiplication sign :
	IF(SPEC(1:1).EQ.'*') THEN
	  CALL las_000_WRSTRING('<B7> s')
	  GOTO 111
	ENDIF

C	Search for Greek symbols :
	DO I=1,26
	  I1=LGC(I)
	  STR=SPEC(1:I1)
	  IF(STR(1:I1).EQ.GREEKLC(I)) THEN	!Lowercase
	    GC=CHAR(I+96)		!ASCII for a,b,.. is 97,98,..
	    CALL las_000_WRSTRING
     1	('syf '//SC//' ssf ('//GC//') s trf '//SC//' ssf')
	    GOTO 111
	  ELSE
	    I2=STR$UPCASE(STR,STR)
	    GUC=GREEKUC(I)
	    I2=STR$UPCASE(GUC,GUC)
	    IF(STR(1:I1).EQ.GUC) THEN
		GC=CHAR(I+64)	!ASCII for A,B,.. is 65,66,..
		CALL las_000_WRSTRING
     1	('syf '//SC//' ssf ('//GC//') s trf '//SC//' ssf')
		GOTO 111
	    ENDIF
	  ENDIF
	ENDDO

C	Could not decode special symbol - write it as it stands :
	CALL las_000_WRSTRING('('//SPEC(1:K1)//') s')

111	IF(ISS.NE.0) THEN !We have done sub- or superscripts. Back to normal.
	  WRITE(SC,FS)    SS
	  CALL las_000_WRSTRING('trf '//SC//' ssf') !all text is normal size.
	  IF(ISS.EQ.1) CALL las_000_WRSTRING('0 '//SUP//' neg rm')
	  IF(ISS.EQ.-1) CALL las_000_WRSTRING('0 '//SUB//' rm')
	ENDIF

	J=K
	GOTO 444
400	CONTINUE

	IF(L.LE.IL) THEN
	  CALL las_000_WRSTRING('('//TEXT(L:IL)//') s')
	ENDIF
	CALL las_000_WRSTRING('gr')

	return

	end

	subroutine las_000_wrstring(string)
	character*(*) string

	logical*1 lasopen
	character*12 symlas
	integer ncoor
	common /las_000_com/ ncoor,TSLAS,GRLAS,WLLAS,SYMLAS,laslun,lasopen

	call killbla(string)
	lstring=lengthc(string)
	if(lstring.le.0) return

	ll=index(string(1:lstring),'  ')
	do while (ll.gt.0)
	   string=string(1:ll)//string(ll+2:lstring)
	   lstring=lengthc(string)
	   if(lstring.le.0) return
	   ll=index(string(1:lstring),'  ')
	enddo
	write(laslun,'(a)') string(1:lstring)

	return

	end
