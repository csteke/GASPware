#include <stdlib.h>
#include "types.def"

#define FRACT 8
#define MLM_GAP 2

#define BOOL  char
#define FALSE 0
#define TRUE  1


typedef struct mlmque
{
    int start;
    int next;
    int end;
    int nflush;
} mlmque;

typedef struct mlmdef
{
    int nseg;
    int nbyt;
    int lsegmin;
    int lsegmax;
    int *obuf;
    char *listbase;
    long int listlen;
    char *recDB;
    int nrecs;
    int nrecsmax;
    int nrecsstep;
    int nrecsfmin;
    int recsfree;
    long int nflush;
    struct mlmque *que;
} mlmdef;

#define MLMQUE struct mlmque
#define MLM  struct mlmdef

BOOL mlm_init_( MLM **, int *, int *, int *, int *);
BOOL mlm_info_(MLM *, int *, int *, int *);		
BOOL mlm_insert_( MLM *, int *, int *);
BOOL mlm_flush_( MLM *, int *, int **, int *);




BOOL mlm_init_( MLM **mlmbase, int *nseg, int *nbyt,
                int *megabytes, int *nused )
{  
  MLM *mlm;
  
  int TotalMem, ListMem, ii;
  
  if( ( *nbyt < 1 ) || ( *nbyt > 4 ) )return FALSE;
  
  TotalMem = (*megabytes)*1024*1024;
  ListMem  = TotalMem - sizeof( MLM ) - (*nseg)*sizeof( MLMQUE );
  if( TotalMem < 32768 ) return FALSE;
  
  mlm = NULL;
  mlm = (MLM *) calloc( 1, sizeof( MLM ) );
  if( mlm == NULL ) return FALSE;
  
  mlm->que = NULL;
  mlm->que = (MLMQUE *) calloc( *nseg, sizeof( MLMQUE ) );
  if( mlm->que == NULL )
  {
      free( mlm );
      mlm = NULL;
      return FALSE;
  }
  
  mlm->nbyt = *nbyt;
  mlm->nflush = 0;
  mlm->nseg = (*nseg);
  mlm->nrecs = mlm->nseg * FRACT;

  mlm->nrecsmax = ((mlm->nrecs - mlm->nseg - 1)>(FRACT*FRACT))?
                                                 (FRACT*FRACT):
		                  (mlm->nrecs - mlm->nseg - 1);
				  
  mlm->nrecsstep = (mlm->nrecsmax+1)/4;
  mlm->nrecsfmin = 0.05*( (float)mlm->nrecs );

  mlm->lsegmin = (ListMem - mlm->nrecs)/(mlm->nbyt*mlm->nrecs 
                                       + mlm->nrecsmax*sizeof(int));
  mlm->lsegmax = mlm->nrecsmax * mlm->lsegmin;
  ListMem -= mlm->nrecs + mlm->lsegmax*sizeof(int);
  mlm->lsegmin = ListMem/mlm->nbyt/mlm->nrecs;
  mlm->lsegmax = mlm->nrecsmax * mlm->lsegmin;
  
  mlm->listlen = mlm->lsegmin * mlm->nrecs;
  
  mlm->obuf = (int *) calloc( mlm->lsegmax, sizeof( int ));
  if( mlm->obuf == NULL )
  {
       free( mlm->que );
       free( mlm );
       return FALSE;
  }
  
  mlm->recDB = (char *) calloc( mlm->nrecs, sizeof(char) );
  if( mlm->recDB == NULL )
  {
       free( mlm->obuf );
       free( mlm->que );
       free( mlm );
       return FALSE;
  }
  for( ii = 0; ii < mlm->nseg; ii++ )
  {
       mlm->recDB[MLM_GAP*ii] = 1;
       mlm->que[ii].next= mlm->que[ii].start = MLM_GAP*ii*mlm->lsegmin;
       mlm->que[ii].end = mlm->que[ii].start + mlm->lsegmin -1;
       mlm->que[ii].nflush = 0;
  }
  mlm->recsfree = mlm->nrecs - mlm->nseg;
  

  mlm->listbase = NULL;
  mlm->listbase = (char *) realloc( mlm->listbase, mlm->nbyt*mlm->listlen );
  if( mlm->listbase == NULL )
  {
       free( mlm->recDB );
       free( mlm->obuf );
       free( mlm->que );
       free( mlm );
       return FALSE;
  }

  *mlmbase = mlm;
  *nused = sizeof( MLM ) + (*nseg)*sizeof( MLMQUE ) + mlm->lsegmax*sizeof(int)+
           mlm->nrecs*sizeof(char) + mlm->nbyt*mlm->listlen;

  return TRUE;
}

 
BOOL mlm_info_(MLM *mlm, int *iseg, int *n1, int *n2)
{

   if( *iseg == -1 )
   {
       *n1 = mlm->lsegmin;
       *n2 = mlm->lsegmax;
       return FALSE;
   }
   
   if( *iseg == -2 )
   {
       *n1 = mlm->nrecs;
       *n2 = mlm->recsfree;
       return FALSE;
   }

   if( (*iseg > -1) && (*iseg < mlm->nseg) )
   {
       *n1 = mlm->que[*iseg].end - mlm->que[*iseg].start +1;
       *n2 = mlm->que[*iseg].next- mlm->que[*iseg].start;
       return TRUE;
   }

   *n1 = mlm->nseg;
   *n2 = mlm->nbyt;
   return FALSE;
}

BOOL mlm_insert_( MLM *mlm, int *Seg, int *Val)
{
   int iseg;
   int ii;
   char *buf, *bval;
   
   iseg = *Seg;
   bval = (char *) Val;
   
   if( (iseg < 0) || (iseg >= mlm->nseg ) )return FALSE;
   if( mlm->que[iseg].next > mlm->que[iseg].end ) return FALSE;
   
   buf = mlm->listbase + mlm->nbyt*mlm->que[iseg].next;
#if defined(sun) && defined(__BIG_ENDIAN__)
   for( ii = 0; ii < mlm->nbyt; ii++, buf++ ) *buf = bval[3-ii];
#else
   for( ii = 0; ii < mlm->nbyt; ii++, buf++ ) *buf = bval[ii];
#endif
   
   if ( ++mlm->que[iseg].next <= mlm->que[iseg].end ) return TRUE;
   
   if ( mlm->que[iseg].next == mlm->que[iseg].end+1 )
   {
        if ( mlm->que[iseg].end-mlm->que[iseg].start+1 < mlm->lsegmax )
	{
	    ii = mlm->que[iseg].end/mlm->lsegmin + 1;
	    if( ii >= mlm->nrecs )return TRUE;
	    if( !mlm->recDB[ii] )
	    {
	        mlm->recDB[ii] = 1;
		mlm->que[iseg].end += mlm->lsegmin;
		mlm->recsfree--;
		return TRUE;
	     }
	 }
   }
   
   return FALSE;
}


BOOL mlm_flush_( MLM *mlm, int *Seg, int **Out, int *NofEntries)
{

  int ii,jj, iseg;
  int nentries;
  double AvFlush;
  
  char *obuf, *buf;
  int rec1, rec2, RecOld, RecNew;
  
  int ContStart;
  int ContLength;

/*
#include <stdio.h>  
  
  printf(" Flush to do\n"); fflush(stdout);
*/
  iseg = *Seg;
  
  if( (iseg < 0) || (iseg >= mlm->nseg ) )return FALSE;
  
  *NofEntries = nentries = mlm->que[iseg].next - mlm->que[iseg].start;
  
  if( nentries < 0 )return FALSE;
  
  if( nentries == 0 )return TRUE;
  
  obuf = (char *)mlm->obuf;
  buf  = mlm->listbase + mlm->nbyt*mlm->que[iseg].start;
  
  for( ii = 0; ii < nentries; ii++, obuf+=4 )
  {
      for( jj = 0; jj < mlm->nbyt; jj++, buf++ )
#if defined( _GW_BIG_ENDIAN)
         obuf[3-jj] = *buf;
#else
	 obuf[jj] = *buf;
#endif
   }
  
   *Out = mlm->obuf;
   
   mlm->que[iseg].next = mlm->que[iseg].start;
   mlm->nflush++;
   mlm->que[iseg].nflush++;




/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      prima di uscire verifica se puo' allungare il buffer          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/
   
   if( mlm->recsfree < mlm->nrecsfmin ) return TRUE;
                                               /* tutto gia' pieno */
   
   if( nentries < mlm->que[iseg].end - mlm->que[iseg].start + 1) return TRUE;
                                               /* non era flush da lista piena */
   
   
   rec1 = mlm->que[iseg].start/mlm->lsegmin;
   rec2 = mlm->que[iseg].end/mlm->lsegmin;

   RecNew = RecOld = rec2 - rec1 +1;

   for( ii = rec1; ii <= rec2; ii++ ) mlm->recDB[ii] = 0;

   mlm->recsfree += RecOld;
   
   AvFlush = ( (double)mlm->nflush )/( (double) mlm->nseg );
   
   if( mlm->que[iseg].nflush < 0.70000*AvFlush )
   {
        RecNew = RecOld - mlm->nrecsstep;
        if( RecNew < 1 ) RecNew = 1;
   } else
   {
        if ( mlm->que[iseg].nflush > 2 ) RecNew += mlm->nrecsstep;
	if ( RecNew > mlm->nrecsmax ) RecNew = mlm->nrecsmax;
	if ( mlm->recsfree - RecNew < mlm->nrecsfmin ) RecNew = RecOld;
   }
   
   ContStart = 0;
   ContLength = 0;
   
   for( ii = 0, jj = 0; ii < mlm->nrecs;  )
   {
      while( mlm->recDB[ii+jj] == 0 ) 
	  if( ii+(++jj) >= mlm->nrecs )break;

      if( ContLength < jj )
      {
         ContStart = ii;
	 ContLength= jj;
	 if( ContLength >= RecNew ) goto FOUND;
      }
      ii += jj;  jj =0;
      while( mlm->recDB[ii] == 1 )
      {
        if( ii >= mlm->nrecs ) break;
        ii++;
      }
    }  

FOUND:

    if( RecNew > ContLength ) RecNew = ContLength;
    for( ii = ContStart; ii < ContStart+RecNew; ii++ ) mlm->recDB[ii] = 1;
    mlm->que[iseg].start = mlm->que[iseg].next = ContStart*mlm->lsegmin;
    mlm->que[iseg].end   = mlm->que[iseg].start + RecNew*mlm->lsegmin -1;
    mlm->recsfree -= RecNew;
    
    return TRUE;
}
