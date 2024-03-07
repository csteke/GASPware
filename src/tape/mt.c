/*      J.R.B. 1/90
- --------------------
SOURCE FILE:  /usr/users/beene/Djblibs/mt_fort.c
LIBRARY:      /usr/users/beene/lib/jblibc1.a
- -----------------------------------------------------------------------
   A group of Fortran callable magtape handling routines
   constructed using standard c calls and hence (I hope) portable.
   In the calls below "tape" is the name of the tape (e.g. /dev/nrmt0h),
   nr is the repeat count for operations (integer), and ierr is an
   error return (integer).
*****************IMPORTANT USAGE NOTES*******************************
These routines are designed for ALL arguments to be passed by reference.
This means that arguments defined as char in the various routines
should be called with numeric variables as corresponding FORTRAN actual
arguments. In general when a CHARACTER variable is more convenient, I do
this by equivalencing a numeric variable to it and then using the
numeric variable as the argument. (Note that for optimization reasons
it's better to avoid floating point numeric types for this purpose).
You can also use the %REF descriptor, but portability issues are then
involved. For example:
        CHARACTER*100 CNAME
        INTEGER NAME
        INTEGER TERMINATE_STRING
        EQUIVALENCE(NAME,CNAME)
        ...
        CNAME='/dev/nrmt0h'
        JERR = TERMINATE_STRING(CNAME)
        CALL MT_OPENRO(NAME,LU)
        ...
The function TERMINATE_STRING is found in the library utility01. It
puts a NULL after the last non-blank character in its argument.

Use INTEGER*4 for ALL arguments and you can't go wrong.
**********************************************************************

**********************************************************************
*GENERAL TAPE HANDLING ROUTINES*
**********************************************************************
   IN ALL FOLLOWING ROUTINES:
   ierr   is an error return (==0 means OK)
   tlu    is an io descriptor ( a tape unit is assumed).
   buf    is an io buffer.
   nb_rd  is a byte count (no. of bytes read)
   nb_wt  is a byte count (no. of bytes written)
   nb_req is a byte count (no. of bytes requested in in IO operation.)
   tape   is a string giving the path to the tape unit being opened.
          IN THE CALLING PROGRAM THE ACTUAL ARGUMENT CORRESPONDING TO
          TAPE MUST BE A NUMERIC TYPE. Make it an integer.
***********************************************************************
    MT_OPENRO(tape,tlu)                              Open tape(name), Read Only.
    MT_OPENRW(tape,tlu)                              Open tape(name), Read & Write.
    MT_CLOSE(tlu)                                    Close tape (tlu).
    MT_CSE(tlu,ierr)                                 Clear "serious exception"
    MT_REW(tlu,ierr)                                 Rewind tape.
    MT_REWUL(tlu,ierr)                               Rewind & unload tape.
    MT_FR(tlu,nr,ierr)                               Skip nr records forward.
    MT_BR(tlu,nr,ierr)                               "    "   "     backward.
    MT_FF(tlu,nr,ierr)                               "    "  files forward.
    MT_BF(tlu,nr,ierr)                               "    "    "   backward.
    MT_READ(tlu,buf,nby_req,nby_rd,ierr)             Read.
    MT_WRITE(tlu,buf,nby_req,nby_wt,ierr)            Write.
    MT_GETSTATUS(tlu,type,stat_r,err_r,resid,ierr)
    DEV_GETSTATUS(tlu,type,stat_r,err_r,resid,ierr)
    MT_WHERE(tlu,pos)                                pos = bytes from BOT.
    MT_RESETPOS(tlu,pos)

***********************************************************************
ROUTINES FOR ULTRIX n-buffered (n-b) IO and/or non-blocking (n-d) IO
See manual pages nbuf(4) for more information on what these terms mean.
***********************************************************************
    MT_NB_OFF(tlu,ierr)                    Turn off n-b io and wait for completion of any
                                           outstanding requests.
    MT_NB_ON(tlu,count,ierr)               Turn on n-buffered (n-b) io. count is the number
                                           of pending IO's to allow on this descriptor .
    MT_ND_ON(tlu,ierr)                     Turn on non-blocking (n-d) io .
    MT_NBND_ON(tlu,count,ierr)             Turn on both n-b & n-d io .
    MT_WAIT(tlu,buf,nb_rd,ierr)            Wait for pending n-b IO.
    MT_WAITND(tlu,buf,nb_rd,limit,ierr)    Wait for pending n-b & n-d IO.  limit is timeout in milliseconds.
    MT_HOLDNB(tlu,limit,ierr)              Uses the select system call to wait for an IO to be possible on
                                           descriptor tlu. If only n-b reading is on, and MT_NB_ON was called
                                           with count=1, then this amounts to a wait for io completion.
                                           ierr = 998 or 997 if the hold times out.
                                           ierr = 0 if io can be done on this descriptor.
                                           Note that the MT_HOLDNB routines do NOT actually complete the io.
                                           Their main function for me is to help me detect hangups.
                                           The MT_WAIT routine should be called after a MT_HOLD type routine
                                           returns to force io completion.
    MT_R_HOLDNB(tlu,limit,ierr)            Same as MT_HOLDNB but only checks on read operations.
    MT_W_HOLDNB(tlu,limit,ierr)            Same as MT_HOLDNB but only checks on write operations.
    MT_READW(tlu,buf,nby_req,nby_rd,ierr)  Read(wait). Turns off n-b io first.
    MT_WRITEW(tlu,buf,nby_req,nby_wt,ierr) Write(wait).  Turns off n-b io first.

***********************************************************************
Utility routines: fortran callable
***********************************************************************
    CSWAB(data,bytes)      Swap bytes in data (not io)
    FCASEFIX(str)          Convert str to lower case and terminate it properly.
                           MAX_LEN = 80 characters.
***********************************************************************
***********************************************************************
*/

#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>

#if defined(__APPLE__)
#include <Apple/mtio.h>
#else
#include <sys/mtio.h>
#endif
#ifdef Digital
#include <sys/devio.h>
#else
#define MTCSE 0x0a
#endif
#ifdef sun
#include <stropts.h>
#endif
#include <sys/file.h>
#include <string.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>

#ifdef Digital
#ifdef USE_AIO
#include <aio.h>
  static struct aiocb AioPars;
  const struct aiocb *AioPointer;
#endif
#endif

#ifdef sun
#ifdef USE_AIO
#include <aio.h>
  static struct aiocb AioPars;
  const struct aiocb *AioPointer;
#endif
#endif

#if defined (__linux__) || defined(__APPLE__)
#ifdef USE_AIO

#if defined(__linux__)
#  define __USE_GNU
#endif

#include <pthread.h> 

typedef struct mt_aio_type
{
   int Tape;
   char *buf;
   int nby_req;
   int nby_read;
   int error;
} mt_aio_type;

static struct mt_aio_type mt_aio;
static int mt_aio_init=0;

pthread_t mthr;

#if defined(__linux__)
pthread_mutex_t mthr_mtx_read = PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP;
pthread_mutex_t mthr_mtx_sync = PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP;
#else
pthread_mutex_t mthr_mtx_read;
pthread_mutex_t mthr_mtx_sync;
#endif

pthread_cond_t  mthr_cond_read;
pthread_cond_t  mthr_cond_sync;


void  mthr_init( void );
void *mthr_run ( void * );
void  mthr_read ( int *tlu, char *buf, int *nby_req );
void  mthr_sync( int *nb_rd, int *ierr );
#endif
#endif

int errno;
int casefix(char *);


#define WTM_EOF 999
#define IO_TIMEOUT 998
#define IO_WTIMEOUT 997
#define LOOPS_MS 20





/***************************************
   Fortran call sequence:
   CALL MT_OPENRO(tape,tlu)
   open tape for READ only.
*/
void mt_openro_(char *tape, int *tlu)
{

#if !(defined (__linux__) )
#ifdef sun
    int on = 1;
#endif
    if(casefix(tape) > 0){
      *tlu=open(tape,O_RDONLY,0666);
#ifdef sun
      ioctl(*tlu, MTIOCPERSISTENT, &on);
#endif
      }
    else
      *tlu = -1;
#else
    struct mtop temp1;
    struct mtget temp2;
    int i;

    for( i = 0; i < 30; i++){
       if(casefix(tape) > 0) {
         *tlu=open(tape,O_RDONLY,0666);
	 if( *tlu == -1 )return;
       }
       else {
         *tlu = -1;
	 return;
       }
       temp1.mt_count = 1;
       temp1.mt_op = MTNOP;
       ioctl(*tlu,MTIOCTOP,&temp1);
       ioctl(*tlu,MTIOCGET,&temp2);
       if( GMT_ONLINE( temp2.mt_gstat) ){
         if( i )printf("                                                        \r");
	 fflush(stdout);
	 return;
       }
       else {
          close( *tlu );
	  *tlu = -1;
	  printf(" >>> Trying to mount the tape ... %2.2d\r", i+1);
	  fflush(stdout);
	  if( i < 29 )sleep( 10 );
       }
     }
#endif
}

/*****************************************
   Fortran call sequence:
   CALL MT_OPENRW(tape,tlu)
   open tape for READ & WRITE.
*/
void mt_openrw_(char *tape, int *tlu)
{
#if !(defined (__linux__) )
#ifdef sun
    int on = 1;
#endif
    if(casefix(tape) > 0){
      *tlu=open(tape,O_RDWR,0666);
#ifdef sun
      ioctl(*tlu, MTIOCPERSISTENT, &on);
#endif
      }
    else
      *tlu = -1;
#else
    struct mtop temp1;
    struct mtget temp2;
    int i;

    for( i = 0; i < 30; i++){
       if(casefix(tape) > 0) {
         *tlu=open(tape,O_RDWR,0666);
	 if( errno == EROFS ){ *tlu = -1; return; }
       }
       else {
         *tlu = -1;
	 return;
       }
       temp1.mt_count = 1;
       temp1.mt_op = MTNOP;
       ioctl(*tlu,MTIOCTOP,&temp1);
       ioctl(*tlu,MTIOCGET,&temp2);
       if( GMT_WR_PROT( temp2.mt_gstat ) ) {
         if( *tlu > -1 )close( *tlu );
	 *tlu = -1;
	 return;
       }
       if( GMT_ONLINE( temp2.mt_gstat) ){
         if( i )printf("                                                        \r");
	 fflush(stdout);
	 return;
       }
       else {
          close( *tlu );
	  *tlu = -1;
	  printf(" >>> Trying to mount the tape ... %2.2d\r", i+1);
	  fflush(stdout);
	  if( i < 29 )sleep( 10 );
       }
     }
#endif
}

/*
{
#ifdef sun
    int on = 1;
#endif
    if(casefix(tape) > 0){
      *tlu=open(tape,O_RDWR,0666);
#ifdef sun
      ioctl(*tlu, MTIOCPERSISTENT, &on);
#endif
      }
    else
      *tlu = -1;
}
*/
/*****************************************
   Fortran call sequence:
   CALL MT_CLOSE(tlu)
   close tape
*/
void mt_close_(int *tlu)
{
    close(*tlu);
}

/*****************************************
   Fortran call sequence:
   CALL MT_REW(tlu,ierr)
   rewind tape.
*/
void mt_rew_( int *tlu, int *ierr)
{
    struct mtop temp;
    temp.mt_op = MTREW;
    temp.mt_count = 1;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
#ifndef __linux__
    lseek(*tlu,0,0);
#endif
}

/*****************************************
   Fortran call sequence:
   CALL MT_REW(tlu,ierr)
   go to EOT
*/
void mt_eod_( int *tlu, int *ierr)
{
    struct mtop temp;
#ifdef Digital
    temp.mt_op = MTSEOD;
#else
    temp.mt_op = MTEOM;
#endif
    temp.mt_count = 1;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
}

/*****************************************
   Fortran call sequence:
   CALL MT_WHERE(tlu,pos)
   determine tape position. (Offset in bytes from pt where tape opened).
*/
void mt_where_( int *tlu, int *pos)
{
#if defined(__linux__)|| defined(__APPLE__)
    *pos=lseek(*tlu,0,SEEK_CUR);
#else
    *pos=tell(*tlu);
#endif
}

/*****************************************
   Fortran call sequence:
   CALL MT_RESETPOS(tlu,pos)
   determine tape position. (Offset in bytes from pt where tape opened).
*/
void mt_resetpos_( int *tlu, int *pos)
{
    *pos=lseek(*tlu,*pos,0);
}

/*****************************************
   Fortran call sequence:
   CALL MT_REWUL(tlu,ierr)
   rewind tape.
*/
void mt_rewul_( int *tlu, int *ierr)
{
    struct mtop temp;
    int i;

#ifdef __linux__
    temp.mt_op = MTREW;
    temp.mt_count = 1;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
    for ( i = 0; i < 12; i++ ){
      temp.mt_op = MTUNLOAD;
      temp.mt_count = 1;
      sleep( 10 );
      *ierr=ioctl(*tlu,MTIOCTOP,&temp);
      if( *ierr == 0 )return;
    }
#else
    temp.mt_op = MTOFFL;
    temp.mt_count = 1;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
    lseek(*tlu,0,0);
#endif
}

/*****************************************
   Fortran call sequence:
   CALL MT_CSE(tlu,ierr)
   clear "serious" exception. (Stangely enough an
   eof is a serious exception in nbuffered mode!!)
*/
void mt_cse_( int *tlu, int *ierr)
{
#ifdef Digital
    struct mtop temp;
    temp.mt_op = MTCSE;
    temp.mt_count = 1;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
#endif
#ifdef __APPLE__
    struct mtop temp;
    temp.mt_op = MTNOP;
    temp.mt_count = 1;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
#endif
#if defined (__linux__) 
    struct mtop temp;
    temp.mt_op = MTRESET;
    temp.mt_count = 1;
/*    *ierr=ioctl(*tlu,MTIOCTOP,&temp);*/
#endif
#ifdef sun
    *ierr=ioctl(*tlu,MTIOCLRERR);
#endif
}
#if defined (__linux__) || defined(__APPLE__)
/*****************************************
   Fortran call sequence:
   CALL MT_SETBLK(tlu,isize,ierr)
   sets tape block size
*/
void mt_setblk_( int *tlu, int *isize,int *ierr)
{
    struct mtop temp;
#if defined(__linux__) 
    temp.mt_op = MTSETBLK;
#elif defined(__APPLE__)
    temp.mt_op = MTSETBSIZ;
#endif
    temp.mt_count = *isize;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
}
#endif
#ifdef sun
/*****************************************
   Fortran call sequence:
   CALL MT_SETBLK(tlu,isize,ierr)
   clear "serious" exception. (Stangely enough an
   eof is a serious exception in nbuffered mode!!)
*/
void mt_setblk_( int *tlu, int *isize,int *ierr)
{
    struct mtop temp;

    temp.mt_op = MTGRSZ;
    temp.mt_count = *isize;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
    if(temp.mt_count != *isize){
      if(temp.mt_count)printf("    Record size changed from %d bytes to %d bytes\n",temp.mt_count,*isize);
      else printf("    Record size changed from variable length to %d bytes\n",*isize);
      temp.mt_op = MTSRSZ;
      temp.mt_count = *isize;
      *ierr=ioctl(*tlu,MTIOCTOP,&temp);
      }
}
#endif
/*****************************************
   Fortran call sequence:
   CALL MT_FR(tlu,nr,ierr)
   skip *nr records forward
*/
void mt_fr_( int *tlu, int *nr, int *ierr)
{
    struct mtop temp;

    temp.mt_count = 1;
    temp.mt_op = MTNOP;
    ioctl(*tlu,MTIOCTOP,&temp);

    temp.mt_op = MTFSR;
    temp.mt_count = *nr;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
/*
    temp.mt_count = 1;
    temp.mt_op = MTNOP;
    ioctl(*tlu,MTIOCTOP,&temp);
*/
}

/*****************************************
   Fortran call sequence:
   CALL MT_BR(tlu,nr,ierr)
   skip *nr records backward
*/
void mt_br_( int *tlu, int *nr, int *ierr)
{
    struct mtop temp;

    temp.mt_count = 1;
    temp.mt_op = MTNOP;
    ioctl(*tlu,MTIOCTOP,&temp);

    temp.mt_op = MTBSR;
    temp.mt_count = *nr;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
/*
    temp.mt_count = 1;
    temp.mt_op = MTNOP;
    ioctl(*tlu,MTIOCTOP,&temp);
*/
}

/*****************************************
   Fortran call sequence:
   CALL MT_FF(tlu,nf,ierr)
   skip *nf files forward
*/
void mt_ff_( int *tlu, int *nf, int *ierr)
{
    struct mtop temp;

    temp.mt_count = 1;
    temp.mt_op = MTNOP;
    ioctl(*tlu,MTIOCTOP,&temp);

    temp.mt_op = MTFSF;
    temp.mt_count = *nf;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
/*
    temp.mt_count = 1;
    temp.mt_op = MTNOP;
    ioctl(*tlu,MTIOCTOP,&temp);
*/
}

/*****************************************
   Fortran call sequence:
   CALL MT_BF(tlu,nf,ierr)
   skip *nf files backward
*/
void mt_bf_( int *tlu, int *nf, int *ierr)
{
    struct mtop temp;

    temp.mt_count = 1;
    temp.mt_op = MTNOP;
    ioctl(*tlu,MTIOCTOP,&temp);

    temp.mt_op = MTBSF;
    temp.mt_count = *nf;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
/*
    temp.mt_count = 1;
    temp.mt_op = MTNOP;
    ioctl(*tlu,MTIOCTOP,&temp);
*/
}
/*****************************************
   Fortran call sequence:
   CALL MT_WEOF(tlu,ierr)
   write end of file
*/
void mt_weof_( int *tlu, int *ierr)
{
    struct mtop temp;
    temp.mt_op = MTWEOF;
    temp.mt_count = 1;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp);
}

/*****************************************
   Fortran call sequence:
   CALL MT_GETSTATUS(tlu,type,stat_r,err_r,resid,ierr)
   get SCSI stautus registers.
*/
void mt_getstatus_( int *tlu, int *t, int *s, int *e, int *r, int *ierr )
{
    struct mtget temp2;
    *ierr=ioctl(*tlu,MTIOCGET,&temp2);
    *t = temp2.mt_type;
    *s = temp2.mt_dsreg;
    *e = temp2.mt_erreg;
    *r = temp2.mt_resid;
}

/*****************************************
   Fortran call sequence:
   CALL DEV_GETSTATUS(tlu,soft_count,hard_count,stat_mask,ierr)
   get status from controlling device driver.
   See devio(4)
*/
void dev_getstatus_( int *tlu, unsigned *s_e, unsigned *h_e, int *st, int *ierr)
{
#ifdef Digital
    struct devget temp2;
    *ierr=ioctl(*tlu,DEVIOCGET,&temp2);
    *s_e = temp2.soft_count;
    *h_e = temp2.hard_count;
    *st  = temp2.stat;
#else
    struct mtget temp2;
#if defined(__linux__) || defined(__APPLE__)
    struct mtop temp3;
    temp3.mt_count = 1;
    temp3.mt_op = MTNOP;
    *ierr=ioctl(*tlu,MTIOCTOP,&temp3);
#endif
    *ierr=ioctl(*tlu,MTIOCGET,&temp2);
    *s_e = temp2.mt_fileno;
    *h_e = temp2.mt_blkno;
    *st = temp2.mt_dsreg;
#endif
#ifdef __linux__
    if( (!GMT_BOT(temp2.mt_gstat)) && (!GMT_WR_PROT(temp2.mt_gstat)) )*st = 0;
    if( GMT_BOT(temp2.mt_gstat) && (!GMT_WR_PROT(temp2.mt_gstat)) )*st = 1;
    if( GMT_BOT(temp2.mt_gstat) && GMT_WR_PROT(temp2.mt_gstat) )*st = 9;
    if( (!GMT_BOT(temp2.mt_gstat)) && GMT_WR_PROT(temp2.mt_gstat) )*st = 8;
#endif
}

/*****************************************
   Fortran call sequence:
   CALL MT_READ(tlu,buf,nby_req,nb_rd,ierr)
   Read on discriptor tlu.
   nby_req = bytes requested (input)
   nby_rd = bytes read (returned). =0 if eof. =-1 if error.
   ierr = 0 if successful, = errno otherwise.
*/
void mt_read_(int *tlu, char *buf, int *nby_req, int *nb_rd, int *ierr)
{
    int tmperr;
    *ierr = 0;
    *nb_rd =(int) read(*tlu,buf,*nby_req);
    if (*nb_rd < 0) {
       *ierr = errno;
       mt_cse_( tlu, (int *)&tmperr);
    }
    if (*nb_rd == 0) {
       *ierr = WTM_EOF;
       mt_cse_( tlu, (int *)&tmperr);
    }
}
/*****************************************
   Fortran call sequence:
   CALL MT_WRITE(tlu,buf,nby_req,nb_w,ierr)
   Read on discriptor tlu.
   nby_req = bytes requested (input)
   nby_w = bytes written (returned). =0 if eof. =-1 if error.
   ierr = 0 if successful, = errno otherwise.
*/
void mt_write_(int *tlu, char *buf, int *nby_req, int *nb_w, int *ierr)
{
    int tmperr;
    *ierr = 0;
    *nb_w = write(*tlu, buf, *nby_req);
    if (*nb_w < 0) {
       *ierr = errno;
       printf(" errno: %d\n",*ierr);
       mt_cse_( tlu, (int *)&tmperr);
    }
    else if (*nb_w == 0){
       *ierr = WTM_EOF;
       mt_cse_( tlu, (int *)&tmperr);
    }
}

/*****************************************
   Fortran call sequence:
   CALL DEV_STATUS(tlu,serr_r,herr_r,stat_r,cstat_r,ierr)
   get status from controlling device driver.
   See devio(4)
*/
#ifdef Digital
void dev_status_( int *tlu, unsigned *s_e, unsigned *h_e,
         int *st, int *cst, int *ierr )
{
    struct devget temp2;
    *ierr=ioctl(*tlu,DEVIOCGET,&temp2);
    *s_e =  temp2.soft_count;
    *h_e =  temp2.hard_count;
    *st  =  temp2.stat;
    *cst =  temp2.category_stat;
}
#endif
/*****************************************
   Fortran call sequence:
   CALL MT_ERRMES(IERR)
   get error message.
*/
void mt_errmes_(int *ierr)
{
   *ierr=errno;
   perror(" ");
}

/**********************************************/
/*************UTILITY ROUTINES*****************/
/**********************************************/

/*****************************************
    FORTRAN:  SUBROUTINE CSWAB(data, nbytes)
*/
void cswab_(char *data, int *bytes)
{

#if !( defined(__APPLE__) || defined(__sun) )
    void swab(char *, char *, int);
#endif

    int nbyte;
    
    nbyte = *bytes;
    swab(data,data,nbyte);
}

/*****************************************
    casefix
*/
int casefix( char *text)
{
   int i,j;
   int c = '\040';
   static char sp[] =" ";
   j = strlen(text);
   i = 0;
   if( index(text,c) == NULL){
         if( j > 80){
            text[80] = '\0';
            j = 79;
         }
         while(i<=j){
            text[i] = tolower(text[i]);
            i++;
         }
         return j;
    }
    else{
        while( (memcmp(&text[i],sp,1) != 0)){
           text[i] = tolower(text[i]);
           i++;
        }
        text[i] = '\0';
        return i;
    }
}

/*****************************************
/*   FORTRAN:  SUBROUTINE FCASEFIX(istring)
*/
int fcasefix_(char *s)
{
    int i;
    i=casefix(s);
    return i;
}



/*****************************************
   Fortran call sequence:
   CALL MT_READ(tlu,buf,nby_req,nb_rd,ierr)
   Read on discriptor tlu.
   nby_req = bytes requested (input)
   nby_rd = bytes read (returned). =0 if eof. =-1 if error.
   ierr = 0 if successful, = errno otherwise.
*/
#ifdef ALTsun
void mt_aioread_(int *tlu, char *buf, int *nby_req, int *nb_rd, int *ierr){


    *ierr = 0;
    *nb_rd = aioread(*tlu,buf,*nby_req,0,SEEK_CUR,&AioResult);
}

void mt_aiocomplete_(int *tlu, int *nb_rd, int *ierr){

   static struct timeval timeout;
   aio_result_t *AioResultP;
   int tmperr;

   timeout.tv_sec  = 10;
   timeout.tv_usec =  0;

   AioResultP = aiowait(&timeout);
   if( ((int )AioResultP == -1) || (AioResultP == (aio_result_t *)&AioResult) ){
     *nb_rd  = AioResult.aio_return;
     if (*nb_rd < 0) {
       *ierr = AioResult.aio_errno;
       mt_cse_( tlu, (int *)&tmperr);
       }
     if (*nb_rd == 0) {
       *ierr = WTM_EOF;
       mt_cse_( tlu, (int *)&tmperr);
       }
     }
   else {
     *nb_rd = 0;
     *ierr = WTM_EOF;
       mt_cse_( tlu, (int *)&tmperr);
     }
 }

#endif


#ifdef USE_AIO
#if !(defined (__linux__) || defined(__APPLE__))
void mt_aioread_(int *tlu, char *buf, int *nby_req, int *nb_rd, int *ierr)
{

    AioPars.aio_fildes = *tlu;
    AioPars.aio_offset = 0;
    AioPars.aio_buf = buf;
    AioPars.aio_nbytes = *nby_req;
    *ierr = 0;
    *nb_rd = aio_read(&AioPars);
}

void mt_aiocomplete_(int *tlu, int *nb_rd, int *ierr)
{

   AioPointer = (const struct aiocb *)&AioPars;
   aio_suspend(&AioPointer,1,NULL);
   *ierr = aio_error(&AioPars);
   *nb_rd = aio_return(&AioPars);
   if (*nb_rd == 0) *ierr = WTM_EOF;
}
#endif

#if defined (__linux__) || defined(__APPLE__)

void mt_aioread_(int *tlu, char *buf, int *nby_req, int *nb_rd, int *ierr)
{
   *ierr = 0;
   mthr_read( tlu, buf, nby_req );
}

void mt_aiocomplete_(int *tlu, int *nb_rd, int *ierr)
{
   mthr_sync( nb_rd, ierr );
}


void mthr_init( void )
{

   pthread_attr_t mthr_attr;

#if defined(__APPLE__)
   pthread_mutexattr_t mtx_attr;
   
   pthread_mutexattr_init( &mtx_attr );
   pthread_mutexattr_settype( &mtx_attr, PTHREAD_MUTEX_ERRORCHECK );

   pthread_mutex_init( &mthr_mtx_read, &mtx_attr ); 
   pthread_mutex_init( &mthr_mtx_sync, &mtx_attr ); 
#endif

   pthread_cond_init( &mthr_cond_read, NULL );
   pthread_cond_init( &mthr_cond_sync, NULL );

   pthread_attr_init( &mthr_attr );
   pthread_attr_setdetachstate( &mthr_attr, PTHREAD_CREATE_DETACHED );
   pthread_create( &mthr, &mthr_attr, mthr_run, (void *) &mt_aio );
   pthread_attr_destroy( &mthr_attr );
#if defined(__APPLE__)
   pthread_mutexattr_destroy( &mtx_attr );
#endif
   usleep(200000);
}


void mthr_read( int *tlu, char *buf, int *nby_req )
{
    if( !mt_aio_init ){ mthr_init(); mt_aio_init = 1; }
/*
    while( pthread_mutex_trylock( &mthr_mtx_read ) );
*/
    pthread_mutex_lock( &mthr_mtx_read );
    
    mt_aio.Tape = *tlu;
    mt_aio.buf = buf;
    mt_aio.nby_req = *nby_req;

    pthread_cond_signal( &mthr_cond_read );
    pthread_mutex_unlock( &mthr_mtx_read );
}

void mthr_sync( int *nb_rd, int *ierr )
{
    if( !mt_aio_init )
    { 
        perror(" MT_ERROR: tape thread not started, exiting ...");
	exit(0);
    }
/*
    while( pthread_mutex_trylock( &mthr_mtx_sync ) );
*/
    pthread_mutex_lock( &mthr_mtx_sync );
    *nb_rd = mt_aio.nby_read;
    *ierr  = mt_aio.error;

    pthread_cond_signal( &mthr_cond_sync );
    pthread_mutex_unlock( &mthr_mtx_sync );
    
}

void *mthr_run( void *mt_aio_arg )
{
    struct mt_aio_type *a;
    int tmperr;

    
    a = mt_aio_arg;

    pthread_mutex_lock( &mthr_mtx_read );
    pthread_mutex_lock( &mthr_mtx_sync );
    
    while( 1 ) 
    {
	pthread_cond_wait( &mthr_cond_read, &mthr_mtx_read );
	a->error = 0;
     	a->nby_read =(int) read(a->Tape,a->buf, a->nby_req);
    	if (a->nby_read < 0) {
    	   a->error = errno;
    	   mt_cse_( &(a->Tape), (int *)&tmperr);
    	}
    	if ( a->nby_read == 0) {
    	   a->error = WTM_EOF;
    	   mt_cse_( &(a->Tape), (int *)&tmperr);
    	}
	pthread_cond_wait( &mthr_cond_sync, &mthr_mtx_sync );
    }
    return NULL;
}


#endif
#endif
