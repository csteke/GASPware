#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define IABS(x) ( (x<0)?-x:x )
#define FABS(x) ( ((x)<0.0000000000000000000e0)?-(x):x )

float cpol_(float *,float *,int *);
void  CPOL_new_coefs(float *old, float *new, int deg);
double CPOL_deriv(double *x, float *a, int deg);
double CPOL_value(double *x, float *a, int deg);
double CPOL_inv_newt(float *y, float *a, int deg, float xmax, int ndiv);
float cpolinv_(float *y, float *a, int *n);


float cpol_(float *x, float *a, int *n){

  int i,deg;
  float y;
  
  if(*n == 0)return 0.00000000;
  deg=IABS(*n)-1;
  y=a[deg];
  for(i=deg-1 ; i>=0 ;i--){
      y*=(*x);
      y+=a[i];
      }
  if(*n > 0) return y;
  if(*x < 0.00000000)return 0.0000000;
  y+=a[deg+1]*sqrtf(*x);
  return y;
 }
 
void CPOL_new_coefs(float *old, float *new, int deg){

  int i;
   
   for(i=0 ; i <= deg ; i++){
     *(new+2*i)=*(old+i);
     *(new+2*i+1)=0.0000000000;
     }
   *(new+1)=*(old+deg+1);
 }
 
double CPOL_deriv(double *x, float *a, int deg){

  int i;
  double y;
  
  if(deg < 1)return 0.00000000000;
  y=a[deg]*deg;
  for(i=deg-1 ; i > 0 ; i--){
    y*=*x;
    y+=a[i]*i;
    }
  return y;
 }
 
 
double CPOL_value(double *x, float *a, int deg){

  int i;
  double y;
  
  if( deg < 0 )return 0.00000000;
  y=a[deg];
  for(i=deg-1 ; i >= 0 ; i--){
    y*=*x;
    y+=a[i];
    }
  return y;
 }
 
 
double CPOL_inv_newt(float *y, float *a, int deg, float xright, int ndiv){

  int i, k;
  double x,x0,xp,xf,xleft,xstep;
  int MAXITER=75;
  
  
  if(deg < 1)return 0.00000000;

  if( deg == 1 )
  {
    if(a[1] != 0.0000000 )return (*y - a[0])/a[1];
    return 0.0000000;
  }

  xleft=0.0000000;
  xstep=xright/ndiv;
  if( (float)CPOL_value(&xleft,a,deg) == *y)return xleft;

  xleft=1.0000000;
  if( (float)CPOL_value(&xleft,a,deg) == *y)return xleft;

  x0=1.0000000;
  while( x0 < xright )
  {
    x0 += xstep;
    x = CPOL_value(&xleft,a,deg)-(*y);
    if(x == 0.0000 )return x;
    x *= (CPOL_value(&x0,a,deg)-(*y));
    if(x <= 0 )break;
    xleft = x0;
  }
    
  xright = x0;
  x0 -= xstep/2.000000;
  if(FABS( CPOL_deriv(&x0,a,deg) ) < 1.000000E-20 ) x0 += 0.01000000000;

  for( k = 0; k < MAXITER; k++ )
    for( i=0 ; i < MAXITER ; i++)
    {
      xp=CPOL_value(&x0,a,deg)-(double)(*y);
      if( FABS(xp) < 1.000000E-10 ) return x0;

      xf = CPOL_deriv(&x0,a,deg);
      x=x0-xp/xf*25.00/(25.00+i+k);

      if( x < xleft ) x = x0-(x0-xleft)*(x0-xleft)*xf/xp;
      if( x > xright ) x = x0-(x0-xright)*(x0-xright)*xf/xp;

      xp = CPOL_value(&x,a,deg)-(double)(*y);
      if( FABS(x - x0)+FABS(xp) < (double)1.000000E-10 ) return x;
      x0=x;
    }
   
   return x;
 }
 
 
 float cpolinv_(float *y, float *a, int *n){
 
   int i , deg , ndiv;
   float *b , x , xmax ;
   double xx;
   
   if( *n == 0 )return 0.00000000;

   if( *n > 0 )
   {
     deg=*n-1;
     xmax=17000.0000;
     ndiv=100;
     return (float)CPOL_inv_newt(y,a,deg,xmax,ndiv);
   }
   
   deg=-1-(*n);
   if(deg == 0)return 0.0000000;

   b=(float *)calloc( (2*deg+3) , sizeof(float) );
   if(b == NULL)return 0.000000000;

   for(i=0; i<2*deg+3 ; i++)b[i]=0.000000000;

   CPOL_new_coefs(a,b,deg);
   deg*=2;
   xmax=200.00000000000000000;
   ndiv=150;
   xx = CPOL_inv_newt(y,b,deg,xmax,ndiv);
   x = (float) xx*xx;

   realloc(b,0);

   return x;
 }
