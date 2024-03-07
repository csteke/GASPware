#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

#if !defined(__APPLE__)
#include <malloc.h>
#endif

#include <math.h>
#include <string.h>


#define DATATYPE int
#define MINCOUNTS  3000

DATATYPE ResX, ResY;
DATATYPE **Data;
DATATYPE *Spec;

DATATYPE *PointYU;
DATATYPE *PointYD;
DATATYPE *PointX;
DATATYPE *PointXP;
double   *PointStat;

DATATYPE Step;
int NPoints;

DATATYPE Sum;
double  Total;
double  Low, High;
int XPeak;
int NP_To_Write;

int XStart, XStop;

int InputString( unsigned char *ExtString );    /* from ../libr/inter_isl.c */
int GetADCNumber( char *String );               /* from ../libr/inter_c.c */




void  autobanana_(DATATYPE *data, DATATYPE *resx, DATATYPE *resy,
		  DATATYPE *start, DATATYPE *stop);
		  
int   AB_Statistics ( DATATYPE * );
void  AB_FindLimits ( DATATYPE * );
void  AB_WriteBanana( void );



void AB_WriteBanana ( void ) {

  FILE *BanFile;
  unsigned char c[128];
  unsigned char *String;
  int l, adc;
  struct stat FileStatus;
  double rr;


    String  = &c[0];
    printf(" WRITE banana in file : ");
    l = InputString( String);
    if( l < 1 )return;

    if( l > 4 ){
      if( ( c[l-4] == '.' )&&
  	  ( c[l-3] == 'b' )&&
  	  ( c[l-2] == 'a' )&&
  	  ( c[l-1] == 'n' ) ) l -= 4;
    }
  
    String[l] = '.'; String[l+1] = 'b'; String[l+2] = 'a'; String[l+3] = 'n';
    String[l+4] = '\0';
    adc  = GetADCNumber( (char *)String );
  
    String += strspn( (char *)String," \t" );
    BanFile = NULL;

    if( adc > -1 )BanFile = fopen((char *)String,"a");
    else {
      if( stat( (char *)String, &FileStatus ) == 0 ){
         printf(" WARNING - File %s already exists, you can choose to save the banana\n",String);
	 printf("           in a different file or to overwrite the existing one\n");
         printf(" WRITE banana in file : ");
         l = InputString( String);
         if( l < 1 )return;
         if( l > 4 ){
            if( ( c[l-4] == '.' )&&
                ( c[l-3] == 'b' )&&
	        ( c[l-2] == 'a' )&&
	        ( c[l-1] == 'n' ) ) l -= 4; }

	 String[l] = '.'; String[l+1] = 'b'; String[l+2] = 'a'; String[l+3] = 'n';
	 String[l+4] = '\0';
	 adc  = GetADCNumber( (char *)String );
	 if( adc > -1 )BanFile = fopen((char *)String,"a");
	 else BanFile = fopen((char *)String,"w");
	 }
       else BanFile = fopen((char *)String,"w");
       }
    
    if( !BanFile ){
      printf(" ERROR - cannot open file : %s\n",String);
      return;
    }

    if( adc > -1 )fprintf(BanFile,"ADC %d\n",adc);

    for( l = 0; l < NP_To_Write; l++ ){
        rr = PointXP[l] - PointYD[l];
    	PointYD[l] = PointXP[l] - (rr*log( PointStat[l] ) + Low*Total )/(log( PointStat[l] ) +  Total );
        rr = PointYU[l] - PointXP[l];
    	PointYU[l] = PointXP[l] + (rr*log( PointStat[l] ) + High*Total )/(log( PointStat[l] ) +  Total );
    }


    for( l = 0; l < NP_To_Write; l++ )
      fprintf(BanFile," %6d  %6d\n",PointX[l],PointYD[l]);
    for( l = NP_To_Write - 1; l >= 0; l-- )
      fprintf(BanFile," %6d  %6d\n",PointX[l],PointYU[l]);

    fclose(BanFile);
}


void  AB_FindLimits ( DATATYPE *d ) {

  DATATYPE Bgr, iw, Bgr0;
  int fromBgr, toBgr;
  double sY, sXY, xx;
  int i, j, x;
  double w;
  
  PointYD[ NP_To_Write ] = XStart;
  PointYU[ NP_To_Write ] = XStop;
  
  xx = XPeak;
  Bgr = d[XStart];
  for( i = XStart + 1; i < XPeak; i++ ) {
        if( Bgr == 0 ) Bgr = d[i];
  	if( d[i] ) Bgr = (d[i] < Bgr)?d[i]:Bgr;
  }

  fromBgr = -1;   toBgr = -1;
  Bgr0 = Bgr + 2*sqrt( (double) (Bgr + 1));
  for( i = XStart ; i < XPeak; i++ ) {
  	if( ( d[i] - sqrt( (double)d[i]) <  Bgr0 ) && (d[i] > 0) ) {
	    if( fromBgr == -1 ) fromBgr = i;
	    else toBgr = i;
	}
  }
  
  for( Bgr = 0, i = fromBgr; i <= toBgr; i++ ) Bgr += d[i];
  Bgr /= (toBgr - fromBgr +1);	

  sY = sXY = 0;
  for( i = XStart + 1; i <= XPeak; i++ ) {
     if( d[i] > Bgr ){
	sY += d[i] - Bgr;
	j = XPeak - i;
	sXY += j*j*( d[i] -Bgr );
     }
  }
  
  if( sY <= 0 ) PointYD[ NP_To_Write ] = XStart;
  else {
  	w = sXY /sY;
	w = sqrt( w ) ;
	iw = 2.50000*w;
	if( XPeak - iw <  XStart ) PointYD[ NP_To_Write ] = XStart;
	else PointYD[ NP_To_Write ] = XPeak - iw;
  }
  
  if( PointYD[ NP_To_Write ] - iw > XStart ){
       sY = sXY = 0;
       x = PointYD[ NP_To_Write ] - iw;
       Bgr = d[XStart];
       for( i = XStart + 1; i < x + iw; i++ ) Bgr += d[i];
       Bgr /= x +iw - XStart + 1;  Bgr++;
       for( i = XStart + 1; i <= x; i++ ) {
            if( d[i] > Bgr ){
	     sY += d[i] - Bgr;
             j = XPeak - i;
             sXY +=  j*j*( d[i] -Bgr );
	    }
       }

       if( sY > 0 ) {
             w = sXY /sY/2.00;
             w = sqrt( w ) ;
	     iw = 2.50000*w;
             if( XPeak - iw >  x ) 
              PointYD[ NP_To_Write ] = XPeak - iw;
       }
  }


  Bgr = d[XStop];
  for( i = XStop - 1; i > XPeak; i-- ) {
        if( Bgr == 0 ) Bgr = d[i];
  	if( d[i] ) Bgr = (d[i] < Bgr)?d[i]:Bgr;
  }

  fromBgr = -1;   toBgr = -1;
  Bgr0 = Bgr + 2*sqrt( (double) (Bgr + 1));
  for( i = XStop ; i > XPeak; i-- ) {
  	if( ( d[i] <  Bgr0 ) && (d[i] > 0) ) {
	    if( fromBgr == -1 ) fromBgr = i;
	    else toBgr = i;
	}
  }
  
  for( Bgr = 0, i = fromBgr; i >= toBgr; i-- ) Bgr += d[i];
  Bgr /= (fromBgr - toBgr +1);	
	    

  sY = sXY = 0;
  for( i = XStop - 1; i >= XPeak; i-- ) {
    if( d[i] > Bgr ){
	sY += d[i] - Bgr;
	j = XPeak - i;
	sXY += j*j*( d[i] -Bgr );
    }
  }
  
  if( sY <= 0 ) PointYU[ NP_To_Write ] = XStop;
  else {
  	w = sXY /sY;
	w = sqrt( w ) ;
	iw = 2.50000000*w;
	if( XPeak + iw >  XStop ) PointYU[ NP_To_Write ] = XStop;
	else PointYU[ NP_To_Write ] = XPeak + iw;
  }

  if( PointYU[ NP_To_Write ] +iw < XStop ){
       sY = sXY = 0;
       x = PointYU[ NP_To_Write ] + iw;
       Bgr = d[XStop];
       for( i = XStop - 1; i >= x -iw ; i-- ) Bgr += d[i];
       Bgr /= XStop -x + iw + 1; Bgr++;
       for( i = x; i >= XPeak; i-- ) {
            if( d[i] > Bgr ){
             sY += d[i] - Bgr;
             j = XPeak - i;
             sXY += j*j*( d[i] -Bgr );
	    }
       }

       if( sY > 0 ) {
             w = sXY /sY;
             w = sqrt( w ) ;
	     iw = 2.50000*w;
             if( XPeak + iw <  x ) 
              PointYU[ NP_To_Write ] = XPeak + iw;
       }
  }
  
}

  
  
  


int AB_Statistics ( DATATYPE *d ) {

  int xm, i, l, r;
  DATATYPE s;
  double sY, sXY, sXXY, dm;
  
  s = 0;
  xm = XStart;
  sXXY = sXY = 0.0000;
  
  for( i = XStart; i <= XStop; i++ ) {
     if( d[xm] < d[i] ) xm = i;
     s += d[i];
     sXY += i*d[i];
  }
  
  
  Sum = s;
  XPeak = xm;
  
  if( s < MINCOUNTS ) return 0;
  if( d[xm] > 400 )return 1;
  
  sY = s;
  dm = sXY/sY;
  xm = dm;

  for( i = XStart; i <= XStop; i++ ) {
     r = XPeak - i;
     sXXY += r*r*d[i];
  }
  xm = sqrt( sXXY/sY )/4.000;
  
  
  sY = sXY = 0.00000;
  
  l = ( (XPeak - xm) > XStart )? XPeak - xm : XStart;
  r = ( (XPeak + xm) < XStop  )? XPeak + xm : XStop;

  for( i = l; i <= r; i++ ) {
     sY += d[i];
     sXY += i*d[i];
  }
  if( sY < 10.0 )return 1;
  XPeak = sXY/sY;  
  
  return 1;
}




void  autobanana_(DATATYPE *data, DATATYPE *resx, DATATYPE *resy,
		  DATATYPE *start, DATATYPE *stop)
{

  DATATYPE y;
  static DATATYPE *Proje;
  DATATYPE SumProje;
  int i, j, jj;
  int LastX, LastStart;

   ResX = *resx;
   ResY = *resy;
   XStart = *start;
   XStop = *stop;
   
   Data = (DATATYPE **)realloc((void *)Data, ResY*sizeof(DATATYPE *));
   for( y = 0; y < ResY; y++)  Data[y] = data+y*ResX ;
   
   Step = 32;
   Total = 0.00000000;
   Low = High = 0.00000000000;


   NPoints = ResX / Step;

   NPoints = ( NPoints <  8 )? ( (  8 > ResX )?ResX: 8 ) : NPoints;
   NPoints = ( NPoints > 48 )? ( ( 48 > ResX )?ResX:48 ) : NPoints;
   Step = ResX / NPoints;
   NPoints = ResX / Step;
   if( ResX - Step*NPoints ) NPoints++;
   NPoints += 2;

   
   PointX    = (DATATYPE *)realloc((void *)PointX,  NPoints*sizeof(DATATYPE));
   PointYU   = (DATATYPE *)realloc((void *)PointYU, NPoints*sizeof(DATATYPE));
   PointYD   = (DATATYPE *)realloc((void *)PointYD, NPoints*sizeof(DATATYPE));
   PointXP   = (DATATYPE *)realloc((void *)PointXP, NPoints*sizeof(DATATYPE));
   PointStat = (double *)  realloc((void *)PointStat, NPoints*sizeof(double));
   Spec      = (DATATYPE *)realloc((void *)Spec,    ResY*sizeof(DATATYPE));
   Proje     = (DATATYPE *)realloc((void *)Proje,   ResX*sizeof(DATATYPE));
   
   for( i = 0; i < ResX; i++ ) {
     Proje[i] = 0;
     for( jj = XStart; jj <= XStop; jj++ ) Proje[i] += Data[jj][i];
     if( Proje[i] ) LastX = i;
   }
   
   SumProje = 0;
   for( i = LastX; (i >= 0) && ( SumProje < MINCOUNTS ); i-- )
      { SumProje += Proje[i]; LastStart = i; } 

   NP_To_Write = 0;
   
   for ( jj = 0; jj < ResY; jj++ )  Spec[jj] = Data[jj][0];

   if ( AB_Statistics( Spec ) ) {
     	     AB_FindLimits ( Spec );
     	     PointX [ NP_To_Write ] = 0;
	     PointStat[ NP_To_Write ] = Sum;
	     PointXP [ NP_To_Write ] = XPeak;
	     Total += Sum;
	     Low += (XPeak - PointYD[ NP_To_Write ])*Sum;
	     High += (PointYU[ NP_To_Write ] - XPeak)*Sum;
   	     NP_To_Write++;
      	     for ( jj = 0; jj < ResY; jj++ )Spec[jj] = 0;
   }
   
   for ( i = 1; i < LastStart; i += Step ){
      for ( jj = 0; jj < ResY; jj++ ) {
        Spec[jj] = 0;
      	for ( j = 0; (j < Step)&&(j+i < LastStart); j++ ) Spec[jj] += Data[jj][i+j];
      }
      
      if ( AB_Statistics( Spec ) ) {
		AB_FindLimits ( Spec );
		PointX [ NP_To_Write ] = (i + Step/2 < LastStart)?(i + Step/2):LastStart ;
		PointStat[ NP_To_Write ] = Sum;
	        PointXP [ NP_To_Write ] = XPeak;
		Total += Sum;
	     	Low += (XPeak - PointYD[ NP_To_Write ])*Sum;
	     	High += (PointYU[ NP_To_Write ] - XPeak)*Sum;
      		NP_To_Write++;
      		for ( jj = 0; jj < ResY; jj++ )Spec[jj] = 0;
      }
   }

   for ( jj = 0; jj < ResY; jj++ ) {
     Spec[jj] = 0;
     for ( j = LastStart; j <= LastX; j++ ) Spec[jj] += Data[jj][j];
   }
   
   if ( AB_Statistics( Spec ) ) {
     	     AB_FindLimits ( Spec );
     	     PointX [ NP_To_Write ] = LastX ;
     	     PointStat[ NP_To_Write ] = Sum;
     	     PointXP [ NP_To_Write ] = XPeak;
     	     Total += Sum;
     	     Low += (XPeak - PointYD[ NP_To_Write ])*Sum;
     	     High += (PointYU[ NP_To_Write ] - XPeak)*Sum;
   	     NP_To_Write++;
   }

   
   Low  /= Total;
   High /= Total;
   Total = log( Total )/2.00000;
   if( NP_To_Write ) AB_WriteBanana ();
}

		
   
