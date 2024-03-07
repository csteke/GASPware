#include <stdio.h>
#include <stdlib.h>
#include "../libr/types.def"

#define MAXCOUNTS 255

static int *GenList, GenHistElem;
static unsigned char *GenHistCounts;

void mlm_compress_init_(int *ExtBufLength) {

  GenList = (int *)calloc( *ExtBufLength, sizeof(int));
  GenHistElem = (int *)calloc( *ExtBufLength, sizeof(int));
  GenHistCounts = (unsigned char *)calloc( *ExtBufLength, sizeof(unsigned char));
 }


/********************  COMPRESSION section  ******************************/


int mlm_compress_1_( unsigned char **ExtBufPointer, int *ExtHistLength, int *ExtBufLength) {

  short int *List, *ListP, *ToRestore ;
  unsigned char *HistElem, *HistCounts, *HistEP, *HistCP;
  int HistLength, ListLength, ListNofEl;
  unsigned char *Buf, *tmp;
  int ii, jj;
  
  List = (short int *)calloc( *ExtBufLength, sizeof(short int));
  HistElem = (unsigned char *)calloc( *ExtBufLength, sizeof(unsigned char));
  HistCounts = (unsigned char *)calloc( *ExtBufLength, sizeof(unsigned char));
  
  for ( ii = 0; ii < *ExtBufLength; ii++){
    *(HistElem + ii) = 0;
    *(HistCounts + ii) = 0;
    }
  
  if( (List == NULL ) || (HistElem == NULL) || (HistCounts == NULL))return 0;
  
  Buf = *ExtBufPointer;
  
  HistLength = (*ExtHistLength)/2;                             /* decode histogram */
  for ( jj = 0; jj < HistLength;  jj++ ){
      *(HistElem   + jj ) = *Buf++;
      *(HistCounts + jj ) = *Buf++;
      }
  
  ListLength = *ExtBufLength - *ExtHistLength;                 /* decode list */
  for ( jj = 0; jj < ListLength;  jj++ ) *(List + jj ) = *Buf++;
  ListNofEl = ListLength;
   
  if ( HistLength > 0 ) {                 /* there is data already histogrammed */
     
     for ( HistEP = HistElem, HistCP = HistCounts ;
           HistEP < HistElem + HistLength; HistEP++, HistCP++ ){
	   
        for( ListP = List; ( *HistCP < MAXCOUNTS ) && (ListP < List+ListLength); ListP++ ) {
	  if ( *ListP == *HistEP ){ (*HistCP)++; *ListP = -1; ListNofEl--; }
	  }

        }
      }

  if( ListNofEl > 2 ){                    /* try to histogram listed data */
   for( ListP = List; ListP < List+ListLength; ListP++ ){
         if ( *ListP >= 0 ) {
	    jj = 0;
	    for ( ii = 1; ListP+ii < List+ListLength; ii++){
		if ( *(ListP + ii) == *ListP ) { 
		   jj++;
		   ToRestore = ListP+ii;
		   *(ListP+ii) = -1;
		   ListNofEl--;
		   if ( jj == MAXCOUNTS ) {
		       *(HistElem   + HistLength) = *ListP;
		       *(HistCounts + HistLength) = MAXCOUNTS;
		       HistLength ++;
		       jj = 0;
		       }
		   }
		 }
	     if ( jj == 1) { *ToRestore = *ListP; ListNofEl++; }
	     if ( jj > 1 ) {
		*(HistElem   + HistLength) = *ListP;
		*(HistCounts + HistLength) = jj+1;
		HistLength ++;
		*ListP = -1;
		ListNofEl--;
		}
	     }
	 }
    }
 
  ii = ListNofEl + 2*HistLength;
  if ( ii >= *ExtBufLength ){            /* no compresion obtained, give up */
    free ( List );
    free ( HistElem );
    free ( HistCounts);
    return 0;
    }
  
  for ( jj = 0, Buf = *ExtBufPointer; jj < HistLength;  jj++ ){    /* encode histogram */
      *Buf++ = *(HistElem   + jj );
      *Buf++ = *(HistCounts + jj );
      }
  *ExtHistLength = 2*HistLength;
  
  if( ListNofEl )                                                  /* encode remained list */
     for( ListP = List; ListP < List+ListLength; ListP++ ){
        if ( *ListP > 0 ) *Buf++ = *ListP;
        }
	
  *ExtBufLength = ii;
    free ( List );
    free ( HistElem );
    free ( HistCounts);
  return 1;
  }
 


int mlm_compress_2_( unsigned char **ExtBufPointer, int *ExtHistLength, int *ExtBufLength) {

  short int *List, *ListP;
  short int *HistElem, *HistEP;
  unsigned char *HistCounts, *HistCP;
  int HistLength, ListLength, ListNofEl;
  unsigned char *Buf;
  int ii, jj;
  
  List = (short int *)calloc( *ExtBufLength, sizeof(short int));
  HistElem = (short int *)calloc( *ExtBufLength, sizeof(short int));
  HistCounts = (unsigned char *)calloc( *ExtBufLength, sizeof(unsigned char));
  
  for ( ii = 0; ii < *ExtBufLength; ii++){
    *(HistElem + ii) = 0;
    *(HistCounts + ii) = 0;
    }
  
  if( (List == NULL ) || (HistElem == NULL) || (HistCounts == NULL))return 0;
  
  Buf = *ExtBufPointer;
  
  HistLength = (2*(*ExtHistLength))/3;                             /* decode histogram */
  for ( jj = 0; jj < HistLength;  jj++ ){
#if defined( _GW_BIG_ENDIAN )
      *(HistElem + jj ) = *Buf++;
      *(HistElem + jj ) <<= 8;
      *(HistElem + jj ) += *Buf++;
#else
      *(HistElem + jj ) = *(Buf+1);
      *(HistElem + jj ) <<= 8;
      *(HistElem + jj ) += *Buf;
      Buf += 2;
#endif
      *(HistCounts + jj ) = *Buf++;
      }

  Buf = (*ExtBufPointer) + 2*(*ExtHistLength);
  ListLength = *ExtBufLength - *ExtHistLength;                 /* decode list */
  for ( jj = 0; jj < ListLength;  jj++ ){
#if defined( _GW_BIG_ENDIAN )
      *(List + jj ) = *Buf++;
      *(List + jj ) <<= 8;
      *(List + jj ) += *Buf++;
#else
      *(List + jj ) = *(Buf+1);
      *(List + jj ) <<= 8;
      *(List + jj ) += *Buf;
      Buf += 2;
#endif
      }
  ListNofEl = ListLength;
   
  if ( HistLength > 0 ) {                 /* there is data already histogrammed */
     
     for ( HistEP = HistElem, HistCP = HistCounts ;
           HistEP < HistElem + HistLength; HistEP++, HistCP++ ){
	   
        for( ListP = List; ( *HistCP < MAXCOUNTS ) && (ListP < List+ListLength); ListP++ ) {
	  if ( *ListP == *HistEP ){ (*HistCP)++; *ListP = -1; ListNofEl--; }
	  }

        }
      }

  if( ListNofEl > 2 ){                    /* try to histogram listed data */
   for( ListP = List; ListP < List+ListLength; ListP++ ){
         if ( *ListP >= 0 ) {
	    jj = 0;
	    for ( ii = 1; ListP+ii < List+ListLength; ii++){
		if ( *(ListP + ii) == *ListP ) { 
		   jj++;
		   *(ListP+ii) = -1;
		   ListNofEl--;
		   if ( jj == MAXCOUNTS ) {
		       *(HistElem   + HistLength) = *ListP;
		       *(HistCounts + HistLength) = MAXCOUNTS;
		       HistLength ++;
		       jj = 0;
		       }
		   }
		 }
	     if ( jj > 0 ) {
		*(HistElem   + HistLength) = *ListP;
		*(HistCounts + HistLength) = jj+1;
		HistLength ++;
		*ListP = -1;
		ListNofEl--;
		}
	     }
	 }
    }
  
  jj = - HistLength/2;
  jj *= 2; jj += HistLength;
  if( jj ) jj = 3*HistLength/2 + 1;
  else jj = 3*HistLength/2;
  
  ii = ListNofEl + jj;
  if ( ii >= *ExtBufLength ){            /* no compresion obtained, give up */
    free ( List );
    free ( HistElem );
    free ( HistCounts);
    return 0;
    }

  *ExtHistLength = jj;
  for ( jj = 0, Buf = *ExtBufPointer; jj < HistLength;  jj++ ){    /* encode histogram */
#if defined( _GW_BIG_ENDIAN )
      *Buf++ = *(HistElem   + jj )>>8;
      *Buf++ = (*(HistElem   + jj )) & 0x00ff;
#else
      *Buf++ = (*(HistElem   + jj )) & 0x00ff;
      *Buf++ = *(HistElem   + jj )>>8;
#endif
      *Buf++ = *(HistCounts + jj );
      }
  
  Buf = *ExtBufPointer + 2*(*ExtHistLength);
  if( ListNofEl )                                                  /* encode remained list */
     for( ListP = List; ListP < List+ListLength; ListP++ ){
        if ( *ListP > 0 ) {
#if defined( _GW_BIG_ENDIAN )
          *Buf++ = (*ListP)>>8;
          *Buf++ = (*ListP) & 0x00ff;
#else
          *Buf++ = (*ListP) & 0x00ff;
          *Buf++ = (*ListP )>>8;
#endif
          }
        }
	
  *ExtBufLength = ii;
    free ( List );
    free ( HistElem );
    free ( HistCounts);
  return 1;
  }
 
 


int mlm_compress_3_( unsigned char **ExtBufPointer, int *ExtHistLength, int *ExtBufLength) {

  int *List, *ListP;
  int *HistElem, *HistEP;
  unsigned char *HistCounts, *HistCP;
  int HistLength, ListLength, ListNofEl;
  unsigned char *Buf;
  int ii, jj;
  
  List = (int *)GenList;
  HistElem = (int *)GenHistElem;
  HistCounts = (unsigned char *)GenHistCounts;
  
/*  for ( ii = 0; ii < *ExtBufLength; ii++){
    *(List + ii) = 0;
    *(HistElem + ii) = 0;
    *(HistCounts + ii) = 0;
    }
  
  if( (List == NULL ) || (HistElem == NULL) || (HistCounts == NULL))return 0;*/
  
  Buf = *ExtBufPointer;
/*
  printf(" IN1 ==> Hist in C : %d  All in C : %d\n",*ExtHistLength,*ExtBufLength); fflush(stdout);
*/  
  HistLength = (3*(*ExtHistLength))/4;                             /* decode histogram */
  for ( jj = 0; jj < HistLength;  jj++ ){
#if defined( _GW_BIG_ENDIAN )
      *(HistElem + jj ) = *Buf++;
      *(HistElem + jj ) <<= 8;
      *(HistElem + jj ) += *Buf++;
      *(HistElem + jj ) <<= 8;
      *(HistElem + jj ) += *Buf++;
#else
      *(HistElem + jj ) = *(Buf+2);
      *(HistElem + jj ) <<= 8;
      *(HistElem + jj ) += *(Buf+1);
      *(HistElem + jj ) <<= 8;
      *(HistElem + jj ) += *Buf;
      Buf += 3;
#endif
      *(HistCounts + jj ) = *Buf++;
      }

  Buf = (*ExtBufPointer) + 3*(*ExtHistLength);
  ListLength = *ExtBufLength - *ExtHistLength;                 /* decode list */
  for ( jj = 0; jj < ListLength;  jj++ ){
#if defined( _GW_BIG_ENDIAN )
      *(List + jj ) = *Buf++;
      *(List + jj ) <<= 8;
      *(List + jj ) += *Buf++;
      *(List + jj ) <<= 8;
      *(List + jj ) += *Buf++;
#else
      *(List + jj ) = *(Buf+2);
      *(List + jj ) <<= 8;
      *(List + jj ) += *(Buf+1);
      *(List + jj ) <<= 8;
      *(List + jj ) += *Buf;
      Buf += 3;
#endif
      }
  ListNofEl = ListLength;

  printf(" IN2 ==> Hist in C : %d  List in C : %d\n",HistLength,ListNofEl); fflush(stdout);
   
  if ( HistLength > 0 ) {                 /* there is data already histogrammed */
     
     for ( HistEP = HistElem, HistCP = HistCounts ;
           HistEP < HistElem + HistLength; HistEP++, HistCP++ ){
	   
        for( ListP = List; ( *HistCP < MAXCOUNTS ) && (ListP < List+ListLength); ListP++ ) {
	  if ( *ListP == *HistEP ){ (*HistCP)++; *ListP = -1; ListNofEl--; }
	  }

        }
      }

  if( ListNofEl > 2 ){                    /* try to histogram listed data */
   for( ListP = List; ListP < List+ListLength; ListP++ ){
         if ( *ListP >= 0 ) {
	    jj = 0;
	    for ( ii = 1; ListP+ii < List+ListLength; ii++){
		if ( *(ListP + ii) == *ListP ) { 
		   jj++;
		   *(ListP+ii) = -1;
		   ListNofEl--;
		   if ( jj == MAXCOUNTS ) {
		       *(HistElem   + HistLength) = *ListP;
		       *(HistCounts + HistLength) = MAXCOUNTS;
		       HistLength ++;
		       jj = 0;
		       }
		   }
		 }
	     if ( jj > 0 ) {
		*(HistElem   + HistLength) = *ListP;
		*(HistCounts + HistLength) = jj+1;
		HistLength ++;
		*ListP = -1;
		ListNofEl--;
		}
	     }
	 }
    }
  
  
  jj = - HistLength/3;
  jj *= 3; jj += HistLength;
  if( jj ) jj = 4*HistLength/3 + 1;
  else jj = 4*HistLength/3;
  
  ii = ListNofEl + jj;
  if ( ii > 4*(*ExtBufLength)/5 ){            /* no compresion obtained, give up */
    return 0;
    }

  *ExtHistLength = jj;

  printf(" OUT ==> Hist in C : %d  List in C : %d\n",HistLength,ListNofEl); fflush(stdout);

  for ( jj = 0, Buf = *ExtBufPointer; jj < HistLength;  jj++ ){    /* encode histogram */
#if defined( _GW_BIG_ENDIAN )
      *Buf++ = (*(HistElem   + jj )>>16)& 0x00ff;
      *Buf++ = (*(HistElem   + jj )>>8)& 0x00ff;
      *Buf++ = (*(HistElem   + jj )) & 0x00ff;
#else
      *Buf++ = (*(HistElem   + jj )) & 0x00ff;
      *Buf++ = (*(HistElem   + jj )>> 8)& 0x00ff;
      *Buf++ = (*(HistElem   + jj )>>16)& 0x00ff;
#endif
      *Buf++ = *(HistCounts + jj );
      }

  Buf = *ExtBufPointer + 3*(*ExtHistLength);
  if( ListNofEl )                                                  /* encode remained list */
     for( ListP = List; ListP < List+ListLength; ListP++ ){
        if ( *ListP >= 0 ) {
#if defined( _GW_BIG_ENDIAN )
          *Buf++ = ((*ListP)>>16)& 0x00ff;;
          *Buf++ = ((*ListP)>>8)& 0x00ff;;
          *Buf++ = (*ListP) & 0x00ff;
#else
          *Buf++ = (*ListP) & 0x00ff;
          *Buf++ = ((*ListP)>> 8)& 0x00ff;
          *Buf++ = ((*ListP)>>16)& 0x00ff;
#endif
          }
        }
	
  *ExtBufLength = ii;
  return 1;
  }
 
  
int mlm_compress_4_( unsigned char **ExtBufPointer, int *ExtHistLength, int *ExtBufLength) {

  int *List, *ListP;
  int *HistElem, *HistEP;
  unsigned char *HistCounts, *HistCP;
  int HistLength, ListLength, ListNofEl;
  unsigned char *Buf;
  int ii, jj;
  
  List = (int *)calloc( *ExtBufLength, sizeof(int));
  HistElem = (int *)calloc( *ExtBufLength, sizeof(int));
  HistCounts = (unsigned char *)calloc( *ExtBufLength, sizeof(unsigned char));
  
  for ( ii = 0; ii < *ExtBufLength; ii++){
    *(HistElem + ii) = 0;
    *(HistCounts + ii) = 0;
    }
  
  if( (List == NULL ) || (HistElem == NULL) || (HistCounts == NULL))return 0;
  
  Buf = *ExtBufPointer;
  
  HistLength = (4*(*ExtHistLength))/5;                             /* decode histogram */
  for ( jj = 0; jj < HistLength;  jj++ ){
#if defined( _GW_BIG_ENDIAN )
      *(HistElem + jj ) = *Buf++;
      *(HistElem + jj ) <<= 8;
      *(HistElem + jj ) += *Buf++;
      *(HistElem + jj ) <<= 8;
      *(HistElem + jj ) += *Buf++;
      *(HistElem + jj ) <<= 8;
      *(HistElem + jj ) += *Buf++;
#else
      *(HistElem + jj ) = *(Buf+3);
      *(HistElem + jj ) <<= 8;
      *(HistElem + jj ) += *(Buf+2);
      *(HistElem + jj ) <<= 8;
      *(HistElem + jj ) += *(Buf+1);
      *(HistElem + jj ) <<= 8;
      *(HistElem + jj ) += *Buf;
      Buf += 4;
#endif
      *(HistCounts + jj ) = *Buf++;
      }

  Buf = (*ExtBufPointer) + 3*(*ExtHistLength);
  ListLength = *ExtBufLength - *ExtHistLength;                 /* decode list */
  for ( jj = 0; jj < ListLength;  jj++ ){
#if defined( _GW_BIG_ENDIAN )
      *(List + jj ) = *Buf++;
      *(List + jj ) <<= 8;
      *(List + jj ) += *Buf++;
      *(List + jj ) <<= 8;
      *(List + jj ) += *Buf++;
      *(List + jj ) <<= 8;
      *(List + jj ) += *Buf++;
#else
      *(List + jj ) = *(Buf+3);
      *(List + jj ) <<= 8;
      *(List + jj ) += *(Buf+2);
      *(List + jj ) <<= 8;
      *(List + jj ) += *(Buf+1);
      *(List + jj ) <<= 8;
      *(List + jj ) += *Buf;
      Buf += 4;
#endif
      }
  ListNofEl = ListLength;
   
  if ( HistLength > 0 ) {                 /* there is data already histogrammed */
     
     for ( HistEP = HistElem, HistCP = HistCounts ;
           HistEP < HistElem + HistLength; HistEP++, HistCP++ ){
	   
        for( ListP = List; ( *HistCP < MAXCOUNTS ) && (ListP < List+ListLength); ListP++ ) {
	  if ( *ListP == *HistEP ){ (*HistCP)++; *ListP = -1; ListNofEl--; }
	  }

        }
      }

  if( ListNofEl > 2 ){                    /* try to histogram listed data */
   for( ListP = List; ListP < List+ListLength; ListP++ ){
         if ( *ListP >= 0 ) {
	    jj = 0;
	    for ( ii = 1; ListP+ii < List+ListLength; ii++){
		if ( *(ListP + ii) == *ListP ) { 
		   jj++;
		   *(ListP+ii) = -1;
		   ListNofEl--;
		   if ( jj == MAXCOUNTS ) {
		       *(HistElem   + HistLength) = *ListP;
		       *(HistCounts + HistLength) = MAXCOUNTS;
		       HistLength ++;
		       jj = 0;
		       }
		   }
		 }
	     if ( jj > 0 ) {
		*(HistElem   + HistLength) = *ListP;
		*(HistCounts + HistLength) = jj+1;
		HistLength ++;
		*ListP = -1;
		ListNofEl--;
		}
	     }
	 }
    }
  
  jj = - HistLength/4;
  jj *= 4; jj += HistLength;
  if( jj ) jj = 5*HistLength/4 + 1;
  else jj = 5*HistLength/4;
  
  ii = ListNofEl + jj;
  if ( ii >= *ExtBufLength ){            /* no compresion obtained, give up */
    free ( List );
    free ( HistElem );
    free ( HistCounts);
    return 0;
    }

  *ExtHistLength = jj;
  for ( jj = 0, Buf = *ExtBufPointer; jj < HistLength;  jj++ ){    /* encode histogram */
#if defined( _GW_BIG_ENDIAN )
      *Buf++ = (*(HistElem   + jj )>>24)& 0x00ff;;
      *Buf++ = (*(HistElem   + jj )>>16)& 0x00ff;
      *Buf++ = (*(HistElem   + jj )>>8 )& 0x00ff;
      *Buf++ = (*(HistElem   + jj )) & 0x00ff;
#else
      *Buf++ = (*(HistElem   + jj )) & 0x00ff;
      *Buf++ = (*(HistElem   + jj )>>8 )& 0x00ff;
      *Buf++ = (*(HistElem   + jj )>>16)& 0x00ff;
      *Buf++ = (*(HistElem   + jj )>>24)& 0x00ff;
#endif
      *Buf++ = *(HistCounts + jj );
      }
  
  Buf = *ExtBufPointer + 4*(*ExtHistLength);
  if( ListNofEl )                                                  /* encode remained list */
     for( ListP = List; ListP < List+ListLength; ListP++ ){
        if ( *ListP > 0 ) {
#if defined( _GW_BIG_ENDIAN )
          *Buf++ = ((*ListP)>>24)& 0x00ff;
          *Buf++ = ((*ListP)>>16)& 0x00ff;
          *Buf++ = ((*ListP)>> 8)& 0x00ff;
          *Buf++ = (*ListP) & 0x00ff;
#else
          *Buf++ = (*ListP) & 0x00ff;
          *Buf++ = ((*ListP)>> 8)& 0x00ff;
          *Buf++ = ((*ListP)>>16)& 0x00ff;
          *Buf++ = ((*ListP)>>24)& 0x00ff;
#endif
          }
        }
	
  *ExtBufLength = ii;
    free ( List );
    free ( HistElem );
    free ( HistCounts);
  return 1;
  }
 
  
/******************  DECOMPRESSION section  ******************************/

void mlm_decompress_1_( int **ExtOutBuf, unsigned char **ExtBufPointer, int *ExtHistLength,
                       int *ExtBufLength, int *NEntries)        {

  int ListLength;
  unsigned char *Buf, *tmp;
  int jj, *OutBuf;
  
  
  
  Buf = *ExtBufPointer;
  OutBuf = *ExtOutBuf;
  
  for ( jj = 0; jj < *ExtHistLength;  jj++ ){
      *OutBuf++ = *Buf++;
      }
  
  ListLength = *ExtBufLength - *ExtHistLength;                 /* decode list */
  for ( jj = 0; jj < ListLength;  jj++ ) {
       *OutBuf++ = *Buf++;
       *OutBuf++ = 1;
       }
 *NEntries = (*ExtHistLength)/2 + ListLength;
 }


void mlm_decompress_2_( int **ExtOutBuf, unsigned char **ExtBufPointer, int *ExtHistLength,
                       int *ExtBufLength, int *NEntries) {

  int HistLength, ListLength;
  unsigned char *Buf;
  int jj, *OutBuf;
  
  
  Buf = *ExtBufPointer;
  OutBuf = *ExtOutBuf;
    
  HistLength = (2*(*ExtHistLength))/3;                             /* decode histogram */
  for ( jj = 0; jj < HistLength;  jj++ ){
#if defined( _GW_BIG_ENDIAN )
      *OutBuf = *Buf++;
      *OutBuf <<= 8;
      *OutBuf += *Buf++;
#else
      *OutBuf = *(Buf+1);
      *OutBuf <<= 8;
      *OutBuf += *Buf;
      Buf += 2;
#endif
      OutBuf++;
      *OutBuf++ = *Buf++;
      }

  Buf = (*ExtBufPointer) + 2*(*ExtHistLength);
  ListLength = *ExtBufLength - *ExtHistLength;                 /* decode list */
  for ( jj = 0; jj < ListLength;  jj++ ){
#if defined( _GW_BIG_ENDIAN )
      *OutBuf = *Buf++;
      *OutBuf <<= 8;
      *OutBuf += *Buf++;
#else
      *OutBuf = *(Buf+1);
      *OutBuf <<= 8;
      *OutBuf += *Buf;
      Buf += 2;
#endif
      *(++OutBuf) = 1; OutBuf++;
      }
 *NEntries = HistLength + ListLength;

 }
 

void mlm_decompress_3_( int **ExtOutBuf, unsigned char **ExtBufPointer, int *ExtHistLength,
                       int *ExtBufLength, int *NEntries) {

  int HistLength, ListLength;
  unsigned char *Buf;
  int jj, *OutBuf;
  
  
  Buf = *ExtBufPointer;
  OutBuf = *ExtOutBuf;
    
  HistLength = (3*(*ExtHistLength))/4;                             /* decode histogram */
  for ( jj = 0; jj < HistLength;  jj++ ){
#if defined( _GW_BIG_ENDIAN )
      *OutBuf = *Buf++;
/*	printf(" 1: %d\n",*OutBuf);fflush(stdout);*/
      *OutBuf <<= 8;
/*	printf(" 1: %d\n",*OutBuf);fflush(stdout);*/
      *OutBuf += *Buf++;
/*	printf(" 2: %d   %d\n",*OutBuf,*(Buf-1));fflush(stdout);*/
      *OutBuf <<= 8;
/*	printf(" 2: %d\n",*OutBuf);fflush(stdout);*/
      *OutBuf += *Buf++;
/*	printf(" 3: %d\n",*OutBuf);fflush(stdout);*/
#else
      *OutBuf = *(Buf+2);
      *OutBuf <<= 8;
      *OutBuf += *(Buf+1);
      *OutBuf <<= 8;
      *OutBuf += *Buf;
      Buf += 3;
#endif
      OutBuf++;
      *OutBuf++ = *Buf++;
      }

  Buf = (*ExtBufPointer) + 3*(*ExtHistLength);
  ListLength = *ExtBufLength - *ExtHistLength;                 /* decode list */
  for ( jj = 0; jj < ListLength;  jj++ ){
#if defined( _GW_BIG_ENDIAN )
      *OutBuf = *Buf++;
      *OutBuf <<= 8;
      *OutBuf += *Buf++;
      *OutBuf <<= 8;
      *OutBuf += *Buf++;
#else
      *OutBuf = *(Buf+2);
      *OutBuf <<= 8;
      *OutBuf += *(Buf+1);
      *OutBuf <<= 8;
      *OutBuf += *Buf;
      Buf += 3;
#endif
/*	printf(" ==> %d\n",*OutBuf);fflush(stdout);*/
      *(++OutBuf) = 1; OutBuf++;
      }
 *NEntries = HistLength + ListLength;

 }
 
 
void mlm_decompress_4_( int **ExtOutBuf, unsigned char **ExtBufPointer, int *ExtHistLength,
                       int *ExtBufLength, int *NEntries) {

  int HistLength, ListLength;
  unsigned char *Buf;
  int jj, *OutBuf;
  
  
  Buf = *ExtBufPointer;
  OutBuf = *ExtOutBuf;
    
  HistLength = (4*(*ExtHistLength))/5;                             /* decode histogram */
  for ( jj = 0; jj < HistLength;  jj++ ){
#if defined( _GW_BIG_ENDIAN )
      *OutBuf = *Buf++;
      *OutBuf <<= 8;
      *OutBuf += *Buf++;
      *OutBuf <<= 8;
      *OutBuf += *Buf++;
      *OutBuf <<= 8;
      *OutBuf += *Buf++;
#else
      *OutBuf = *(Buf+3);
      *OutBuf <<= 8;
      *OutBuf += *(Buf+2);
      *OutBuf <<= 8;
      *OutBuf += *(Buf+1);
      *OutBuf <<= 8;
      *OutBuf += *Buf;
      Buf += 4;
#endif
      OutBuf++;
      *OutBuf++ = *Buf++;
      }

  Buf = (*ExtBufPointer) + 4*(*ExtHistLength);
  ListLength = *ExtBufLength - *ExtHistLength;                 /* decode list */
  for ( jj = 0; jj < ListLength;  jj++ ){
#if defined( _GW_BIG_ENDIAN )
      *OutBuf = *Buf++;
      *OutBuf <<= 8;
      *OutBuf += *Buf++;
      *OutBuf <<= 8;
      *OutBuf += *Buf++;
      *OutBuf <<= 8;
      *OutBuf += *Buf++;
#else
      *OutBuf = *(Buf+3);
      *OutBuf <<= 8;
      *OutBuf += *(Buf+2);
      *OutBuf <<= 8;
      *OutBuf += *(Buf+1);
      *OutBuf <<= 8;
      *OutBuf += *Buf;
      Buf += 4;
#endif
      *(++OutBuf) = 1; OutBuf++;
      }
 *NEntries = HistLength + ListLength;

 }
 
