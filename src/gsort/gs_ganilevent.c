#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../libr/types.def"

#if defined (__linux__) || defined(__APPLE__)
#  if !defined(__USE_XOPEN)
#      define __USE_XOPEN
#      include <unistd.h>
#      undef __USE_XOPEN
#   endif
#else
#   include <unistd.h>
#endif

/***** GSORT interface function and required declarations *****/

#define MAXDET 	256
#define MAXPAR  16
#define MAXTYPES  32

#define FREEDET -1

#define NO_TYPE  -1
#define TRIGGER   1
#define CLOVER    2
#define SEGMENTS  3
#define DIAMANT   4
#define NWALL     5
#define NTAC      6
#define SYNC      7

typedef struct GANIL_Detector
{	
        int Id;
	int Val[MAXPAR];
	struct GANIL_Detector *Next; 
} GANIL_Detector;

typedef struct GANIL_Event
{
	int Fold[MAXTYPES];
	GANIL_Detector *  Det[MAXTYPES];
	GANIL_Detector *  Out[MAXTYPES];
} GANIL_Event;


static GANIL_Event *Ev;
static int GANIL_types[256];
static int GANIL_minor[MAXTYPES];
static char *swapBuffer;
static int swapBufferSize;

int ganil_get_run_no_( char * );
int ganil_isdatablock_( char * );
int ganil_detfold_( int Type );
int ganil_getdetector_(int Type, int *data, int npar );
int ganil_init_( void );
unsigned short int *ganil_getevent_( unsigned short int *, unsigned short int *, int *);

GANIL_Detector * newDetector( void );

/****** End GSORT declarations *********************************/


int ganil_get_run_no_( char *buff )
{
     if( !strncmp(" FILEH  ", buff, 8) ) return -3;
     if(  strncmp(" EVENTH ", buff, 8) ) return -2;
     else
      return atoi(buff+73);
}

int ganil_isdatablock_( char *buff )
{
     int *tmp;
     ssize_t length;
     
     if( strncmp(" EBYEDAT", buff, 8) ) return 0;
     else
     {
        tmp = (int *)(buff+12);
	if( *tmp != 0x22061999 )
	{
	   length  = buff[31]; length>>8;
	   length += buff[30]; length>>8;
	   length += buff[29]; length>>8;
	   length += buff[28];
	   length *= 2;
	   if( swapBufferSize < length ) swapBuffer = realloc( swapBuffer, length+32 );
	   swab( buff+32, swapBuffer, length );
	   memcpy( buff+32, swapBuffer, length );
	}
	return 1;
     }
}


int ganil_detfold_(int Type )
{
  if( Ev )
   { if( Type >=0 && Type < MAXTYPES ) return Ev->Fold[Type]; }
  return 0;
}

int ganil_getdetector_(int Type, int *data, int npar )
{
  int ii;
  
  if( Ev )
  {									     
     if( Type >=0 && Type < MAXTYPES )					     
     {									     
  	 if( !Ev->Out[Type] ) return 0; 				     
  	 if( Ev->Out[Type]->Id == FREEDET ) return 0;
  	 data[0] = Ev->Out[Type]->Id;
  	 for( ii = 0; ii < npar; ii++ ) data[ii+1] = Ev->Out[Type]->Val[ii];
  	 Ev->Out[Type] = Ev->Out[Type]->Next;
  	 return 1;
     }									     
   }									     
   									     
   return 0;								     
}


int ganil_init_()
{
   int ii;

   for( ii =   0; ii < 255; ii++ ) GANIL_types[ii] = NO_TYPE ;
   
   for( ii = 246; ii < 246; ii++ ) GANIL_types[ii] = TRIGGER ;   /*  TRIGGER   */
   for( ii = 160; ii < 175; ii++ ) GANIL_types[ii] = CLOVER  ;   /*  CLOVER    */
   for( ii = 176; ii < 190; ii++ ) GANIL_types[ii] = SEGMENTS;   /*  SEGMENTS  */
   for( ii =   1; ii <  81; ii++ ) GANIL_types[ii] = DIAMANT ;   /*  DIAMANT   */   
   for( ii = 111; ii < 116; ii++ ) GANIL_types[ii] = NWALL   ;   /*  NWALL     */
   for( ii = 245; ii < 245; ii++ ) GANIL_types[ii] = SYNC    ;   /*  SYNC      */
   
   GANIL_minor[ TRIGGER ] = 246;
   GANIL_minor[ CLOVER  ] = 160;
   GANIL_minor[ SEGMENTS] = 176;
   GANIL_minor[ DIAMANT ] =   1;
   GANIL_minor[ NWALL   ] = 111;
   GANIL_minor[ SYNC    ] = 245;
   
   swapBufferSize = 64*1024;
   swapBuffer = ( char * )calloc( swapBufferSize, sizeof( char ) );
   
   Ev = ( GANIL_Event *) calloc( 1, sizeof( GANIL_Event ) );
   if( Ev ) return 1;
   return 0;
}


unsigned short int *ganil_getevent_( unsigned short int *bufptr, unsigned short int *endptr, int *ev_status )
{
   int ii;
   GANIL_Detector *d;
   unsigned short int *data, *next, *sev_lim;
   unsigned short int addr, val;
   int Type, detId, Item, Status;
   
   for( ii = 0; ii < MAXTYPES; ii++ )
   {
      Ev->Fold[ii] = 0;
      Ev->Out[ii] = NULL;
      d = Ev->Det[ii];
      while( d ) { d->Id = FREEDET; memset( d->Val, 0, MAXPAR*sizeof( int ) ); d = d->Next; }
   }
   
   *ev_status = 0;
   if( bufptr >= endptr )
   {
# if defined(_GW_32_BYTES)
       printf(" Pointer mismatch  %d  %d\n", bufptr, endptr);
#elif defined(_GW_64_BYTES)
       printf(" Pointer mismatch  %ld  %ld\n", bufptr, endptr);
#endif
       return endptr;
   }
   
   data = bufptr;

SEARCH:
   while( data < endptr ) 
   {
     if( ((*data)&0xFF00) == 0xFF00 ) break;
     data++;
   }
   
   
   if( *data == 0xFF00 ) return data;   /* end-of-block mark */
   if( data > endptr-2 ) return data;   /* end of buffer, no more evets */
   
   if( *data == 0xFF60 )   /* format type 0 */
   {
       
       if( (*( data + *(data+1) )&0xFF00) != 0xFF00 )  /* check event consistency */
       { 
          data += 5; 
	  if( data < endptr )goto SEARCH;
	  else {*ev_status = 0; return data;} 
       }
       
       next = data + *(data+1);                               /* save a pointer to next event */
       if( *(data+1) < 8 ) { data += 5; goto SEARCH; }        /* event too short */
       data += 5;                                             /* skip event header */
       if( ((*data)&0x000F) != 1 ){ data = next; goto SEARCH; }   /* unknown sub-event type, skip event */

       if( ((*data)>>10) == 0 )  /*  EXOGAM */
       {
           sev_lim = data + *(data+1);
	   data += 2;
	   
	   while( data < sev_lim )
	   {
	       addr = *data; data++;
	       val  = *data; data++;
	       
	       Type   = GANIL_types[ addr&0x00FF ];
	       if( Type > NO_TYPE )
	       {
	          detId  = (addr&0x00FF) - GANIL_minor[Type];
	          Item   = (addr>>8)&0x003F;
	          Status = addr>>14;
	       }
	       
	       switch( Type )
	       {
	         case TRIGGER:
		 {
		    if( !Ev->Det[TRIGGER] ) Ev->Det[TRIGGER] = newDetector();
		    d = Ev->Det[TRIGGER];
		    while( d )
		    {
		       if( d->Id == detId   )break;
		       if( d->Id == FREEDET )break;
		       d = d->Next;
		    }
		    
		    if( !d ) 
		    {
		       d = Ev->Det[Type];
		       while( d->Next ) d = d->Next;
		       d->Next = newDetector();
		       d = d->Next;
		    }
		    
		    if( d->Id == FREEDET ) (Ev->Fold[TRIGGER])++;
		    d->Id = detId;
		    d->Val[Item] = val&0x7FFF;   /* keep only 15-bit data, 32768 ch */
		    break;
	          }
		  
	         case CLOVER:
		 {
		    if( Status )break;
		    detId = detId*4 + Item/3;       /* assume 4 segments and 3 items/seg.  */
		    Item = Item - ((int)3)*( Item/((int)3) );
		    
		    if( !Ev->Det[CLOVER] ) Ev->Det[CLOVER] = newDetector();
		    d = Ev->Det[CLOVER];
		    while( d )
		    {
		       if( d->Id == detId   )break;
		       if( d->Id == FREEDET )break;
		       d = d->Next;
		    }

		    if( !d ) 
		    {
		       d = Ev->Det[Type];
		       while( d->Next ) d = d->Next;
		       d->Next = newDetector();
		       d = d->Next;
		    }

		    if( d->Id == FREEDET ) (Ev->Fold[CLOVER])++;
		    d->Id = detId;
		    d->Val[Item] = val&0x7FFF;     /*  15-bit data, 32768 ch */
/*		    d->Val[Item] = val&0x3FFF;       14-bit data, 16384 ch */
		    break;
	          }

	         case SEGMENTS:
		 {
		    detId = detId*4 + Item>>3;       /* assume 4 segments and 8 items/seg.  */
		    Item  = Item - ((int)8)*( Item>>3);

		    if( !Ev->Det[SEGMENTS] ) Ev->Det[SEGMENTS] = newDetector();
		    d = Ev->Det[SEGMENTS];
		    while( d )
		    {
		       if( d->Id == detId   )break;
		       if( d->Id == FREEDET )break;
		       d = d->Next;
		    }

		    if( !d ) 
		    {
		       d = Ev->Det[Type];
		       while( d->Next ) d = d->Next;
		       d->Next = newDetector();
		       d = d->Next;
		    }

		    if( d->Id == FREEDET ) (Ev->Fold[SEGMENTS])++;
		    d->Id = detId;       
		    d->Val[Item] = val&0x3FFF;      /* 14-bit data, 16384 ch */
		    break;
	          }

	         case DIAMANT:
		 {
		    if( Status != 1 )break;         /*  Pile-up  */
		    if( !Ev->Det[DIAMANT] ) Ev->Det[DIAMANT] = newDetector();
		    d = Ev->Det[DIAMANT];
		    while( d )
		    {
		       if( d->Id == detId   )break;
		       if( d->Id == FREEDET )break;
		       d = d->Next;
		    }

		    if( !d ) 
		    {
		       d = Ev->Det[Type];
		       while( d->Next ) d = d->Next;
		       d->Next = newDetector();
		       d = d->Next;
		    }

		    if( d->Id == FREEDET ) (Ev->Fold[DIAMANT])++;
		    d->Id = detId;   
		    d->Val[Item] = val&0x1FFF;      /* 13-bit data, 8192 ch */
		    break;
	          }

	         case NWALL:
		 {
		    detId = detId*32 + Item;      
		    Item  = detId/48;
		    detId = detId - Item*48;
		    
		    if( Item == 3 )
		    {
		       Item = detId;
		       detId = 0;
		       Type = NTAC;
		    }
		    
		    if( !Ev->Det[Type] ) Ev->Det[Type] = newDetector();
		    d = Ev->Det[Type];
		    while( d )
		    {
		       if( d->Id == detId   )break;
		       if( d->Id == FREEDET )break;
		       d = d->Next;
		    }

		    if( !d ) 
		    {
		       d = Ev->Det[Type];
		       while( d->Next ) d = d->Next;
		       d->Next = newDetector();
		       d = d->Next;
		    }

		    if( d->Id == FREEDET ) (Ev->Fold[Type])++;
		    d->Id = detId;
		    d->Val[Item] = val&0x3FFF;      /* 14-bit data, 16384 ch */
		    break;
	          }
		  
		  default:
		    break;
	       }
		 
	    }	       	       
     }

   for( ii = 0; ii < MAXTYPES; ii++ )
   {
      if( Ev->Fold[ii] )
      {
       /*   printf("  fold[%d] : %d\n", ii, Ev->Fold[ii]); */
	  Ev->Out[ii] = Ev->Det[ii];
          *ev_status += Ev->Fold[ii];
      }
   }
   return data;
   
  }
  return data;
}

GANIL_Detector * newDetector( )
{
   GANIL_Detector *tmp;
   
   tmp       = (GANIL_Detector *) calloc( 1, sizeof( GANIL_Detector ) );
   tmp->Id   = FREEDET;
   tmp->Next = NULL;
   
   return tmp;
}

