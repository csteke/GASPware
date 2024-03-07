#if ( defined( __i386__ ) || defined( __x86_64__) ) && !defined(__APPLE__)
#      define _LARGEFILE64_SOURCE
#endif


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/time.h> 

#include "../libr/types.def"

#ifdef ALLOWONLINE
#include "get_vme_hist.c"
#endif


/* Period parameters */  
#define N 624
#define M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */

static unsigned long mt[N]; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */


/*void lib$wait_(float *ftime)
{
  if ( *ftime > 0.)
  {
  unsigned int itime;
    itime=*ftime;
    sleep ( itime );
/*    usleep( (useconds_t) ( (*ftime - floorf(*ftime)) * 1000000.) );
  }
} */



void swap_bytes_(char *dataIn, int *SizeOfData) {                               
                                                                                
  int i;                                                                        
  char *dataOut, buff[64];                                                      
                                                                                
  dataOut=&buff[0];                                                             
  for(i=0; i<*SizeOfData; i++) *(dataOut + i)= *(dataIn + i);                   
  for(i=0; i<*SizeOfData; i++) *(dataIn+i) = *(dataOut+ *SizeOfData-i-1);       
}


int filexist_(char *path){

  struct stat *pstat;
  int error,len,ii;
  char *pstring;
  char *Space = " ";
  
  pstat = (struct stat *)calloc(1,sizeof(struct stat));
  len = strlen(path);
  pstring = (char *)calloc( len+4, sizeof(char) );
  
  
  if( pstat == NULL ){
    printf("  ERROR - cannot allocate memory for the STAT structure in FILEXIST\n");
    return -1;
    }

  if( pstring == NULL ){
    printf("  ERROR - cannot allocate memory for the string copy in FILEXIST\n");
    return -1;
    }
  strcpy(pstring,path);
  for( ii = 0; ii < len; ii++){
     if( (pstring[ii] == Space[0])||(pstring[ii] == '\t')||(pstring[ii] == '\n')||(pstring[ii] == '\r') )
        pstring[ii] = '\0';}
  error = stat(pstring,pstat);
  
  realloc(pstat,0); pstat = NULL;
  realloc(pstring,0); pstring = NULL;
  
  return error;
 }

int fileremove_(char *path){

  int len,ii;
  char *pstring;
  char *Space = " ";

  len = strlen(path);
  pstring = (char *)calloc( len+4, sizeof(char) );


  if( pstring == NULL ){
    printf("  ERROR - cannot allocate memory for the string copy in FILEREMOVE\n");
    return -1;
    }
  strcpy(pstring,path);
  for( ii = 0; ii < len; ii++){
     if( (pstring[ii] == Space[0])||(pstring[ii] == '\t')||(pstring[ii] == '\n') )
        pstring[ii] = '\0';}

  if(filexist_(path) == 0)return remove(pstring);
  else return -1;
}

int fileopenro_(char *path){

  int len,ii;
  char *pstring;
  char *Space = " ";

  len = strlen(path);
  pstring = (char *)calloc( len+4, sizeof(char) );


  if( pstring == NULL ){
    printf("  ERROR - cannot allocate memory for the string copy in FILEOPENRO\n");
    return -1;
    }
  strcpy(pstring,path);
  for( ii = 0; ii < len; ii++){
     if( (pstring[ii] == Space[0])||(pstring[ii] == '\t')||(pstring[ii] == '\n') )
        pstring[ii] = '\0';}
#if defined( _LARGEFILE64_SOURCE )
   return open64(pstring,O_RDONLY);
#else
   return open(pstring,O_RDONLY);
#endif
}


int GetADCNumber( char *String ){

  char *ADCstart, *c;
  int ADCsize, ADC;
  char ADCstring[32];
  
  ADCstart = strchr( String, '#');
  if( !ADCstart )return -1;
  
  ADCstart++; ADCsize = strspn(ADCstart, "0123456789");
  if( !ADCsize )return -1;
  *(ADCstart-1) = '\0';
  
  sscanf(ADCstart,"%[0123456789]",ADCstring);
  sscanf(ADCstring,"%d",&ADC);
  
  c = ADCstart + ADCsize;
  strcat( String, c);
  return ADC;
}

int get_adc_number_( char *string )
{

  char *ADCstart;
  int ADCsize, ADC;
  char ADCstring[32];
  
  ADCstart = strchr( string, '#');
  if( !ADCstart )return -1;
  
  ADCstart++;
  ADCsize = strspn(ADCstart, "0123456789");
  if( !ADCsize )return -1;
  
  sscanf(ADCstart,"%[0123456789]",ADCstring);
  sscanf(ADCstring,"%d",&ADC);
  
  return ADC;
}

int get_tape_number_( char *string )
{

  char *ADCstart;
  int ADCsize, ADC;
  char ADCstring[32];
  
  ADCstart = strrchr( string, '.');
  if( !ADCstart )return -1;
  
  ADCstart++;
  ADCsize = strspn(ADCstart, "0123456789");
  if( !ADCsize )return -1;
  
  sscanf(ADCstart,"%[0123456789]",ADCstring);
  sscanf(ADCstring,"%d",&ADC);
  
  return ADC;
}


float r4_time_( void )
{

struct timeval  TimeVal;
#ifndef sun
struct timezone TimeZone;
#endif
float r;

static int FirstCall = 1;
static long Reference = 0;


#ifdef sun
  gettimeofday( &TimeVal, NULL );
#else
  gettimeofday( &TimeVal, &TimeZone );
#endif
  if( FirstCall )
  {
     Reference = TimeVal.tv_sec;
     FirstCall = 0;
  }
  
  r = TimeVal.tv_usec;
  r /= 1000000.0000000000;
  r += (TimeVal.tv_sec - Reference);
  
  return r;
}


unsigned long iseed_generator_( void )
{

struct timeval  TimeVal;
#ifndef sun
struct timezone TimeZone;
#endif
int dev_random_fd = -1;
unsigned long long  rnd_bytes;
unsigned long rnd_seed;
  
  dev_random_fd = open("/dev/urandom", O_RDONLY);
  
  if( dev_random_fd == -1 )
  {
#ifdef sun
      gettimeofday( &TimeVal, NULL );
#else
      gettimeofday( &TimeVal, &TimeZone );
#endif
      rnd_bytes = (TimeVal.tv_sec & TimeVal.tv_usec) + (TimeVal.tv_sec ^ TimeVal.tv_usec);
      if( !rnd_bytes ) rnd_bytes = TimeVal.tv_sec;
      return rnd_bytes;
  }
  while( read(dev_random_fd, &rnd_bytes, sizeof(unsigned long)) != sizeof(unsigned long) );
  
  close(dev_random_fd);
  
  rnd_bytes &= (unsigned long) 0x0fff;
  rnd_bytes <<= 24; 
  rnd_bytes /= (unsigned long)10000;
  rnd_bytes *= (unsigned long)10000;
  rnd_bytes += (unsigned long)5749;
  rnd_seed = rnd_bytes;
  
  return rnd_seed;
}



void *gw_malloc_( int *Bytes )
{
   return (void *) calloc( (size_t) (*Bytes), 1 );
}

void gw_free_( void **p )
{
   free( *p );
}

/*----------------------------------------------------------------------------------------------
 
   A C-program for MT19937, with initialization improved 2002/1/26.
   Coded by Takuji Nishimura and Makoto Matsumoto.

   Before using, initialize the state by using init_genrand(seed)  
   or init_by_array(init_key, key_length).

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.                          

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote 
        products derived from this software without specific prior written 
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.keio.ac.jp/matumoto/emt.html
   email: matumoto@math.keio.ac.jp
*/






/* initializes mt[N] with a seed */
void init_genrand(unsigned long s)
{
    mt[0]= s & 0xffffffffUL;
    for (mti=1; mti<N; mti++) {
        mt[mti] = 
	    (1812433253UL * (mt[mti-1] ^ (mt[mti-1] >> 30)) + mti); 
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        mt[mti] &= 0xffffffffUL;
        /* for >32 bit machines */
    }
}

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
void init_by_array(init_key, key_length)
unsigned long init_key[], key_length;
{
    int i, j, k;
    init_genrand(19650218UL);
    i=1; j=0;
    k = (N>key_length ? N : key_length);
    for (; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL))
          + init_key[j] + j; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++; j++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
        if (j>=key_length) j=0;
    }
    for (k=N-1; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL))
          - i; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
    }

    mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */ 
}

/* generates a random number on [0,0xffffffff]-interval */
unsigned long genrand_int32(void)
{
    unsigned long y;
    static unsigned long mag01[2]={0x0UL, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if init_genrand() has not been called, */
            init_genrand(5489UL); /* a default initial seed is used */

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];

        mti = 0;
    }
  
    y = mt[mti++];

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return y;
}

/* generates a random number on [0,0x7fffffff]-interval */
long genrand_int31(void)
{
    return (long)(genrand_int32()>>1);
}

/* generates a random number on [0,1]-real-interval */
double genrand_real1(void)
{
    return genrand_int32()*(1.0/4294967295.0); 
    /* divided by 2^32-1 */ 
}

/* generates a random number on [0,1)-real-interval */
double genrand_real2(void)
{
    return genrand_int32()*(1.0/4294967296.0); 
    /* divided by 2^32 */
}

/* generates a random number on (0,1)-real-interval */
double genrand_real3(void)
{
    return (((double)genrand_int32()) + 0.5)*(1.0/4294967296.0); 
    /* divided by 2^32 */
}

/* generates a random number on [0,1) with 53-bit resolution*/
double genrand_res53(void) 
{ 
    unsigned long a=genrand_int32()>>5, b=genrand_int32()>>6; 
    return(a*67108864.0+b)*(1.0/9007199254740992.0); 
} 
/* These real versions are due to Isaku Wada, 2002/01/09 added */

/* generates a random number on [0,1)-real-interval */

void init_frand_( void )
{
	init_genrand( iseed_generator_() );
}
	

float rand_real2_(void)
{
    return (float) (((double)genrand_int32())/((double)4294967296.0E0)) ;
    /* divided by 2^32 */
}


