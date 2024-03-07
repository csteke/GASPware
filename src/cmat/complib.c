#include  <sys/types.h>
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  <unistd.h>


#include "../libr/types.def"

#define NBITSINT  32
#define SIZEINT    4
#define SIZESHORT  2
#define MODESTEP   4
#define LBIN      16
#define MAXPVAL   1024*1024*1024

#define  TBITS     8
#define  TVAL    128
#define  BMASK   255
#define  WMASK 65535

int comp_compress_  (int *, const int *, unsigned char *, int *, int *, int *);
int comp_decompress_(int *, int *, unsigned char *, int *, int *, int *);

int ccomp__2_check(const int *, int);

int ccomp__0_compress    (const int *, int, unsigned char *, int);
int ccomp__1_compressW   (const int *, int, short     int *, int);
int ccomp__1_compressLW  (const int *, int,           int *, int);
int ccomp__2_compress    (const int *, int, unsigned char *     );
int ccomp__3_compress    (const int *, int, unsigned char *     );

int ccomp__0_decompress  (int *, int, unsigned char *, int);
int ccomp__1_decompressW (int *, int, short     int *     );
int ccomp__1_decompressLW(int *, int,           int *     );
int ccomp__2_decompress  (int *, int, unsigned char *     );
int ccomp__3_decompress  (int *, int, unsigned char *     );

void ccomp__2_findvals(const int *, int *, int *);
int  ccomp__2_puthead(unsigned char *, int  , int  , int  , int  );
int  ccomp__2_gethead(unsigned char *, int *, int *, int *, int *);
int  ccomp__2_comp(const int *, unsigned char *, int, int, int);
int  ccomp__2_deco(int *, unsigned char *, int, int, int);


#if defined( _GW_BIG_ENDIAN )

 void swap_bytes(char *dataIn, int SizeOfData);
#endif


static int newbits[] = { 0, 1, 2, 3, 4, 5, 6, 8, 8, 9,10,11,12,13,14,16,
                        16,17,18,19,20,21,22,24,24,25,26,27,28,29,30,32,32};

static unsigned int nmask[]   = {0x00000000,
                        0x00000001, 0x00000003, 0x00000007, 0x0000000F, 
                        0x0000001F, 0x0000003F, 0x0000007F, 0x000000FF, 
                        0x000001FF, 0x000003FF, 0x000007FF, 0x00000FFF, 
                        0x00001FFF, 0x00003FFF, 0x00007FFF, 0x0000FFFF, 
                        0x0001FFFF, 0x0003FFFF, 0x0007FFFF, 0x000FFFFF, 
                        0x001FFFFF, 0x003FFFFF, 0x007FFFFF, 0x00FFFFFF, 
                        0x01FFFFFF, 0x03FFFFFF, 0x07FFFFFF, 0x0FFFFFFF, 
                        0x1FFFFFFF, 0x3FFFFFFF, 0x7FFFFFFF, 0xFFFFFFFF };

static int ntag[]    = { 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                         3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3};

int
comp_compress_(int *lwdata, const int *nch, unsigned char *packed, int *dnbytes, int *mode, int *minval) {

  int nbytes2z, tipo, nbits;
  int gt_zero, gt_one, totsum, maxval, maxBSM;
  int nbytes, nbytes0, nbytes1, nbytes2, nbytes3, nbycomp;
  int ii, ll;
  int *ptr;
  int  *ptr1;

  nbytes2z = ccomp__2_check(lwdata, *nch);

  if(nbytes2z < SIZEINT * *nch) {nbytes = nbytes2z;       tipo =  2;}
  else                          {nbytes = SIZEINT * *nch; tipo = -1;}

  for(ii = 1, ptr = lwdata, *minval = *ptr++ ; ii < *nch; ii++) {
    if(*ptr < *minval) *minval = *ptr;
    ptr++;
  }
  
  if(*minval != 0) for(ii = 0, ptr = lwdata; ii < *nch; ii++) *ptr++ -= *minval;

  gt_zero = 0; gt_one = 0;
  totsum = 0; maxval = 0;
  maxBSM = (8 * SIZEINT -1) * *nch;
  for(ii = 0, ptr = lwdata; ii < *nch; ii++) {
    ll = *ptr++;
    if(ll > 0) gt_zero++;
    if(ll > 1) gt_one++;
    if(ll > maxval) maxval = ll;
    if(totsum < maxBSM) totsum += ll;
  }
  if(maxval == 0) {
    *mode = 0;
    for(ii = 0, ptr = lwdata; ii < *nch; ii++) *ptr++ += *minval;
    return(0);
  }

/**** ccomp__0_compress tutti i canali con lo stesso numero di bits ****/

  nbits = 0; ll = maxval;
  if(ll > MAXPVAL) {nbits = 32;}
  else { while(ll) {nbits++; ll >>= 1;} }
  nbits = newbits[nbits];
  nbytes0 = (nbits * *nch + 7) / 8;
  if(nbytes0 < nbytes) {nbytes = nbytes0; tipo = 0;}

/**** ccomp__1_compress lista i canali diversi da zero ****/

  ll  = 1;                 /* la parola di testa          */
  ll += 2 * gt_one;        /* 2 parole per i canali  > 1  */
  ll += gt_zero - gt_one;  /* 1 parola per i canali  == 1 */
  ll++;                    /* la parola vuota di coda     */
  if(*nch < 32768  &&  maxval < 32768) nbytes1 = SIZESHORT * ll;
  else                                 nbytes1 = SIZEINT   * ll;
  if(nbytes1 < nbytes) {nbytes = nbytes1; tipo = 1;}

/**** ccomp__2_compress token ****/

  if(*minval == 0) nbytes2 = nbytes2z;
  else             nbytes2 = ccomp__2_check(lwdata, *nch);
  if(nbytes2 < nbytes) {nbytes = nbytes2; tipo = 2;}

/**** ccomp__3_compress BSM ****/

  ll = *nch + totsum;
  nbytes3 = (ll + 7) / 8;
  if(nbytes3 < nbytes) {nbytes = nbytes3; tipo = 3;}


/****************************************************************/
/****************************************************************/
/****************************************************************/

  switch (tipo) {

    case -1:
      nbits   = NBITSINT;
      nbytes0 = SIZEINT * *nch;
    case 0:
      *mode = nbits;
      nbycomp = ccomp__0_compress(lwdata, *nch, packed, *mode);
      if(nbycomp != nbytes0) return(-1);
      nbytes = nbycomp;
      break;

   case 1:
     if(*nch < 32768   &&   maxval < 32768) {
       *mode = NBITSINT + 0 * MODESTEP + 1;
       nbycomp = ccomp__1_compressW (lwdata, *nch, (short int *)packed, gt_zero); }
     else {
       *mode = NBITSINT + 0 * MODESTEP + 2;
       nbycomp = ccomp__1_compressLW(lwdata, *nch, (      int *)packed, gt_zero); }
     if(nbycomp != nbytes1) return(-1);
     nbytes = nbycomp;
     break;

    case 2:
      if(nbytes2z < nbytes2) {
        for(ii = 0, ptr = lwdata; ii < *nch; ii++) *ptr++ += *minval;
        *minval = 0;
        nbytes2 = nbytes2z;
      }
      *mode = NBITSINT + 1 * MODESTEP + 1;
      nbycomp = ccomp__2_compress(lwdata, *nch, packed);
      if(nbycomp != nbytes2) return(-1);
      nbytes = nbycomp;
      break;

    case 3:
      *mode = NBITSINT + 2 * MODESTEP + 1;
      nbycomp = ccomp__3_compress(lwdata, *nch, packed);
      if(nbycomp != nbytes3) return(-1);
      nbytes = nbycomp;
  }

  if(*minval != 0) for(ii = 0, ptr = lwdata; ii < *nch; ii++) *ptr++ += *minval;
  *dnbytes=nbytes;
  return(nbytes);

}

int
comp_decompress_(int *lwdata, int *nch, unsigned char *packed, int* dnbytes, int *mode, int *minval) {

  int * ptr;
  int nbytes, ii;

/* printf(" mode = %d, minval = %d\n",*mode,*minval);*/

 if(*mode >= 0   &&   *mode <= NBITSINT)
     nbytes = ccomp__0_decompress(lwdata, *nch, packed, *mode);
  else if(*mode == (NBITSINT + 0 * MODESTEP + 1))
     nbytes = ccomp__1_decompressW (lwdata, *nch, (short int *)packed);
  else if(*mode == (NBITSINT + 0 * MODESTEP + 2))
     nbytes = ccomp__1_decompressLW(lwdata, *nch, (      int *)packed);
  else if(*mode == (NBITSINT + 1 * MODESTEP + 1))
     nbytes = ccomp__2_decompress(lwdata, *nch, packed);
  else if(*mode == (NBITSINT + 2 * MODESTEP + 1))
     nbytes = ccomp__3_decompress(lwdata, *nch, packed);
  else
     return(-1);

  if(*minval != 0) for(ii = 0, ptr = lwdata; ii < *nch; ii++) *ptr++ += *minval;
  *dnbytes=nbytes;
  return(nbytes);
}

int
ccomp__0_compress(const int *data, int nch, unsigned char *pack, int nbits) {


  unsigned int   * idata, * ipack;
  unsigned short * wdata, * wpack;
  unsigned char  * cdata, * cpack;

  int mask;
  int nbitsr, half, npacked, ii;
  unsigned int dd;

  if(nbits <= 0) {
    return(0);
  }
  else if(nbits >= 32) {
    idata = (unsigned int *) data;
    ipack = (unsigned int *) pack;
    for(ii = 0; ii < nch; ii++) {
       *ipack++ = *idata++;
#if defined( _GW_BIG_ENDIAN )
       swap_bytes( (char *)(ipack-1), sizeof(unsigned int));
#endif
       }
    return(SIZEINT * nch);
  }

  nbitsr = nbits;
  wpack = (unsigned short *) pack;
  half = 0;

  if(nbitsr >= 16) {
    idata = (unsigned int *) data;
    for(ii = 0; ii < nch; ii++) *wpack++ = *idata++ & WMASK ;
#if defined( _GW_BIG_ENDIAN )
       swap_bytes( (char *)(wpack-1), sizeof(unsigned short));
#endif
    if(nbits == 16) return(SIZESHORT * nch);
    nbitsr -= 16;
    half = 1;
  }
#if defined( _GW_BIG_ENDIAN )
  if( half ) half=0;
  else half=1;
#endif

  if(nbitsr == 8) {
    cdata = (unsigned char *) data;
    cdata += 2 * half;
#if defined( _GW_BIG_ENDIAN )
    if( half ) cdata++;
    else cdata--;
#endif
    cpack = (unsigned char *) wpack;
    for(ii = 0; ii < nch; ii++, cdata += SIZEINT) *cpack++ = *cdata & BMASK ;
    return(cpack - pack);
  }

  mask = nmask[nbitsr];
  dd = 0; npacked = 0;
  wdata = (unsigned short *) data;
  wdata += half;
  for( ii = 0; ii < nch; ii++, wdata += SIZESHORT) {
    dd = (dd << nbitsr) | (*wdata & mask);
    npacked += nbitsr;
    if(npacked >= 16) {
      *wpack++ = ( dd >> (npacked-16) ) & WMASK;
      npacked -= 16;
#if defined( _GW_BIG_ENDIAN )
       swap_bytes( (char *)(wpack-1), sizeof(unsigned short));
#endif
    }
  }

  if (npacked >0) {
      *wpack++ = (dd & nmask[npacked]) << (16-npacked);
#if defined( _GW_BIG_ENDIAN )
       swap_bytes( (char *)(wpack-1), sizeof(unsigned short));
#endif
       }
  return ( (unsigned char *)wpack - pack);
 
}

int
ccomp__0_decompress(int *data, int nch, unsigned char *pack, int nbits) {

  unsigned int   * idata, * ipack;
  unsigned short * wdata, * wpack;
  unsigned char  * cdata, * cpack;

  int mask;
  int nbitsr, half, npacked, ii;
  unsigned int dd;
  unsigned short wdd;

  if(nbits <= 0) {
    
    for(ii = 0, idata = (unsigned int *) data; ii < nch; ii++) *idata++ = 0; 
    return(0);
  }
  else if(nbits >= 32) {
    idata = (unsigned int *) data;
    ipack = (unsigned int *) pack;
    for(ii = 0; ii < nch; ii++) {
       *idata++ = *ipack++ ;
#if defined( _GW_BIG_ENDIAN )
       swap_bytes( (char *)(idata-1), sizeof(int));
#endif
       }
    return(SIZEINT * nch);
  }

  nbitsr = nbits;
  wpack = (unsigned short *) pack;
  half = 0;

  if(nbitsr >= 16) {
    idata = (unsigned int *) data;
    for(ii = 0; ii < nch; ii++) *idata++ = *wpack++;
#if defined( _GW_BIG_ENDIAN )
    cpack = (unsigned char *) idata;
    cpack -= 2;
    swap_bytes( (char *)cpack , sizeof(short));
#endif
    if(nbits == 16) {
       return(SIZESHORT * nch);
       }
    nbitsr -= 16;
    half = 1;
  }
  else {
    idata = (unsigned int *) data;
    for(ii = 0; ii < nch; ii++) *idata++ = 0;
  }
#if defined( _GW_BIG_ENDIAN )
  if( half ) half=0;
  else half=1;
#endif

  if(nbitsr == 8) {
    cdata = (unsigned char *) data;
    cdata += 2 * half;
#if defined( _GW_BIG_ENDIAN )
    if (half) cdata ++ ;
    else cdata --;
#endif
    cpack = (unsigned char *) wpack;
    for(ii = 0; ii < nch; ii++, cdata += SIZEINT) *cdata = *cpack++;
    return(cpack - pack);
  }


  mask = nmask[nbitsr];
  dd = 0; npacked = 0;
  wdata = (unsigned short *) data;
  wdata += half;
  ii = nch;
  while(ii) {
    wdd = *wpack++;
#if defined( _GW_BIG_ENDIAN )
    swap_bytes( (char *)&wdd , sizeof(short));
#endif
    dd |= wdd;
    npacked += 16;
    while(npacked > nbitsr) {
      *wdata = ( dd >> (npacked - nbitsr) )  & mask;
      wdata += SIZESHORT; ii--; npacked -= nbitsr;
      if(ii == 0) break;
    }
    dd <<= 16;
  }
  return((unsigned char *) wpack - pack - 2);
}

int
ccomp__1_compressW(const int *data, int nch, short int *wpack,int nonzero) {

  int ii, ll;
  short int *wptr;

  wptr = wpack;
  *wptr++ = nonzero;
#if defined( _GW_BIG_ENDIAN )
  swap_bytes( (char *)(wptr-1), 2);
#endif
  for(ii = 0; ii < nch; ii++) {
    ll = *data++;
    if(ll > 0) {
      *wptr++ = ii;
#if defined( _GW_BIG_ENDIAN )
      swap_bytes( (char *)(wptr-1), 2);
#endif
      if(ll > 1){
       *wptr++ = - --ll;
#if defined( _GW_BIG_ENDIAN )
       swap_bytes( (char *)(wptr-1), 2);
#endif
       }
    }
  }
  *wptr++ = 0;

  return(SIZESHORT * (wptr -wpack));

}

int
ccomp__1_compressLW(const int *data, int nch,int *pack, int nonzero) {

  int ii, ll;
  int * iptr;

  iptr = pack;
  *iptr++ = nonzero;
#if defined( _GW_BIG_ENDIAN )
  swap_bytes( (char *)(iptr-1), sizeof(int));
#endif
  for(ii = 0; ii < nch; ii++) {
    ll = *data++;
    if(ll > 0) {
      *iptr++ = ii;
#if defined( _GW_BIG_ENDIAN )
      swap_bytes( (char *)(iptr-1), sizeof(int));
#endif
      if(ll > 1) {
        *iptr++ = - --ll;
#if defined( _GW_BIG_ENDIAN )
        swap_bytes( (char *)(iptr-1), sizeof(int));
#endif
        }
    }
  }
  *iptr++ = 0;

  return(SIZEINT * (iptr -pack));

}

int
ccomp__1_decompressW(int *data, int nch, short int *wpack) {

  short int ii, nonzero, indpack, inddata, nexdata;
  int *dptr;
  short int *wptr;
#if defined( _GW_BIG_ENDIAN )
  char *swp_ptr;
#endif

  wptr = wpack;
  nonzero = *wptr++;
#if defined( _GW_BIG_ENDIAN )
  swp_ptr=(char *)&nonzero;
  swap_bytes(swp_ptr,2);
#endif
  if(nonzero <= 0) return(-1);

  for(ii = 0, dptr = data; ii < nch; ii++) *dptr++ = 0;

#if defined( _GW_BIG_ENDIAN )
  inddata = *wptr++;
  swp_ptr=(char *)&inddata;
  swap_bytes(swp_ptr,2);
  if(inddata < 0 ) return -1;
#else
  if( (inddata = *wptr++) < 0 ) return(-1);
#endif

  for(ii = 0; ii < nonzero; ii++) {
    *(data + inddata) += 1;
    nexdata = *wptr++;
#if defined( _GW_BIG_ENDIAN )
  swp_ptr=(char *)&nexdata;
  swap_bytes(swp_ptr,2);
#endif
    if(nexdata < 0) {
      *(data + inddata) -= nexdata;
#if defined( _GW_BIG_ENDIAN )
      inddata = *wptr++;
      swp_ptr=(char *)&inddata;
      swap_bytes(swp_ptr,2);
      if(inddata < 0 ) return -1;
#else
      if( (inddata = *wptr++) < 0 ) return(-1);
#endif
    }
    else inddata = nexdata;
  }

  return(SIZESHORT * (wptr - wpack));

}

int
ccomp__1_decompressLW(int *data, int nch, int *pack) {

  int ii, nonzero, indpack, inddata, nexdata;
  int *dptr, *pptr;
#if defined( _GW_BIG_ENDIAN )
  char *swp_ptr;
#endif

  pptr = pack;
  nonzero = *pptr++;
#if defined( _GW_BIG_ENDIAN )
  swp_ptr=(char *)&nonzero;
  swap_bytes(swp_ptr,sizeof(int));
#endif
  if(nonzero <= 0) return(-1);

  for(ii = 0, dptr = data; ii < nch; ii++) *dptr++ = 0;

#if defined( _GW_BIG_ENDIAN )
  inddata = *pptr++;
  swp_ptr=(char *)&inddata;
  swap_bytes(swp_ptr,sizeof(int));
  if(inddata < 0 ) return -1;
#else
  if( (inddata = *pptr++) < 0 ) return(-1);
#endif
  for(ii = 0; ii < nonzero; ii++) {
    *(data + inddata) = 1;
    nexdata = *pptr++;
#if defined( _GW_BIG_ENDIAN )
    swp_ptr=(char *)&nexdata;
    swap_bytes(swp_ptr,sizeof(int));
#endif
    if(nexdata < 0) {
      *(data + inddata) -= nexdata;
#if defined( _GW_BIG_ENDIAN )
      inddata = *pptr++;
      swp_ptr=(char *)&inddata;
      swap_bytes(swp_ptr,sizeof(int));
      if(inddata < 0 ) return -1;
#else
      if( (inddata = *pptr++) < 0 ) return(-1);
#endif
    }
    else inddata = nexdata;
  }

  return(SIZEINT * (pptr - pack));

}

int
ccomp__2_check(const int *dati, int isize) {

  int compress;
  int ibpnt, nbits, minval, itag, icount, ii;
  int nbitsn, minvaln, nch, nbused, iminval, nextra;

  ibpnt = 1;        /* primo byte contiene LBIN */

  ccomp__2_findvals(dati, &nbits, &minval);
  itag = ntag[nbits];
  icount = 0;

  for(ii = LBIN; ii < isize; ii +=LBIN ) {
    compress = 1;
    ccomp__2_findvals(dati + ii, &nbitsn, &minvaln);
    if(nbitsn == nbits   &&   minvaln == minval) {
      if( (itag == 0   &&   icount < 31) ||
          (itag == 1   &&   icount < 15) ||
          (itag == 2   &&   icount <  7) ) { icount++; compress = 0;}
    }
    if(compress) {
      nch = LBIN * (icount + 1);
      if(minval == 0) {
         nbused = 1;
      }
      else {
        iminval = abs(minval)/16;
        if(iminval == 0)               nextra = 1;
        else if(iminval <= 0x000000FF) nextra = 2;
        else if(iminval <= 0x0000FFFF) nextra = 3;
        else if(iminval <= 0x00FFFFFF) nextra = 4;
        else                           nextra = 5;
        nbused = 1 + nextra;
      }
      ibpnt += nbused;
      nbused = (nch * nbits + 7) / 8;
      ibpnt += nbused;
      nbits = nbitsn;
      itag = ntag[nbits];
      minval = minvaln;
      icount = 0;
    }
  }
  nch = LBIN * (icount + 1);
  if(minval == 0) {
    nbused = 1;
  }
  else {
    iminval = abs(minval) >> 4;
    if(iminval == 0)               nextra = 1;
    else if(iminval <= 0x000000FF) nextra = 2;
    else if(iminval <= 0x0000FFFF) nextra = 3;
    else if(iminval <= 0x00FFFFFF) nextra = 4;
    else                           nextra = 5;
    nbused = 1 + nextra;
  }
  ibpnt += nbused;
  nbused = (nch * nbits + 7) / 8;
  ibpnt += nbused;

  return(ibpnt);

}

int
ccomp__2_compress(const int *dati, int isize, unsigned char *bdat) {

  int compress;
  int idpnt, nbits, minval, itag, icount, ii;
  int nbitsn, minvaln, nch, nbused, iminval, nextra;
  unsigned int dd;
  unsigned char * cptr;

  idpnt = 0;
  cptr = bdat;
  *cptr++ = LBIN;

  ccomp__2_findvals(dati, &nbits, &minval);
  itag = ntag[nbits];
  icount = 0;

  for(ii = LBIN; ii < isize; ii += LBIN) {
    compress = 1;
    ccomp__2_findvals(dati + ii, &nbitsn, &minvaln);
    if(nbitsn == nbits   &&   minvaln == minval) {
      if( (itag == 0   &&   icount < 31) ||
          (itag == 1   &&   icount < 15) ||
          (itag == 2   &&   icount <  7) ) { icount++; compress = 0;}
    }
    if(compress) {
      nch = LBIN * (icount + 1);
      nbused = ccomp__2_puthead(cptr, nbits, itag, icount, minval);
      cptr += nbused;
      nbused = ccomp__2_comp(dati + idpnt, cptr, nch, nbits, minval);
      cptr += nbused;
      idpnt += nch;
      nbits = nbitsn;
      itag = ntag[nbits];
      minval = minvaln;
      icount = 0;
    }
  }
  nch = LBIN * (icount + 1);
  nbused = ccomp__2_puthead(cptr, nbits, itag, icount, minval);
  cptr += nbused;
  nbused = ccomp__2_comp(dati + idpnt, cptr, nch, nbits, minval);
  cptr  += nbused;

  return(cptr - bdat);

}

int
ccomp__2_decompress(int *dati, int isize, unsigned char *bdat) {

  unsigned int dd;
  int idpnt, nbits, minval, itag, icount, ii;
  int nhead, nch, nbused;
  int lbin;
  unsigned char * cptr;

  cptr = bdat;
  idpnt = 0;

  lbin = *cptr++;

  while (idpnt < isize) {
    nhead = ccomp__2_gethead(cptr, &nbits, &itag, &icount, &minval);
    if(nhead < 1) return(-1);
    cptr += nhead;
    nch = lbin * (icount + 1);
    nbused = ccomp__2_deco(dati + idpnt, cptr, nch, nbits, minval);
    cptr  += nbused;
    idpnt += nch;
  }
  
  return(cptr - bdat);

}

void
ccomp__2_findvals(const int *dati, int *nbits, int *minval) {

  int ii, maxval;

  maxval = *minval = *dati++;
  for(ii = 1; ii < LBIN; ii++, dati++) {
    if(*dati < *minval) *minval = *dati;
    if(*dati >  maxval)  maxval = *dati;
  }
  maxval -= *minval;

  *nbits = 0;
  while(maxval > 0) {(*nbits)++; maxval >>= 1;}
  return;
}

int
ccomp__2_puthead(unsigned char *bdat, int nbits, int itag, int icount, int minval) {

  int ii, iminval, nextra, nhead;
  unsigned int dd;

  switch (itag) {
    case 0: dd =   icount * 4; break;
    case 1: dd = 1 + icount * 4 + (nbits - 1) * 64; break;
    case 2: dd = 2 + icount * 4 + (nbits - 3) * 32; break;
    case 3: dd = 3 +              (nbits - 1) *  4;
  }

  if(minval == 0) {
    *bdat = dd & BMASK;
    return(1);
  }

  dd |= 0x80;
  *bdat++ = dd & BMASK;

  if(minval > 0) {
    iminval = minval;
    dd =   (iminval & 0xF) << 3;
  }
  else {
    iminval = - minval;
    dd = ( (iminval & 0xF) << 3) | 0x80 ;
  }
  iminval >>= 4;

       if(iminval ==          0) {nextra = 1;}
  else if(iminval <= 0x000000FF) {nextra = 2;}
  else if(iminval <= 0x0000FFFF) {nextra = 3;}
  else if(iminval <= 0x00FFFFFF) {nextra = 4;}
  else                           {nextra = 5;}

  dd += nextra;
  *bdat++ = dd & BMASK;

  for(ii = 1; ii < nextra; ii++) {
    *bdat++ = iminval & BMASK;
    iminval >>= 8;
  }

  return(nextra + 1);

}

int
ccomp__2_gethead(unsigned char *bdat, int *nbits, int *itag, int *icount,int *minval) {

  int nhead;
  unsigned int dd;
  int nextra, isign, ii;

  dd = *bdat++;
  *itag = dd & 3;

  switch (*itag) {
    case 0: *icount = (dd & 0x7C) >> 2; *nbits = 0; break;
    case 1: *icount = (dd & 0x3C) >> 2; *nbits = 1 + ((dd & 0x7F) >> 6); break;
    case 2: *icount = (dd & 0x1C) >> 2; *nbits = 3 + ((dd & 0x7F) >> 5); break;
    case 3: *icount =       0         ; *nbits = 1 + ((dd & 0x7F) >> 2);
  }

  if((dd & 0x80) == 0) {*minval = 0; return(1);}

  dd = *bdat++ & BMASK;
  nextra =  dd       & 0x07;
  *minval = (dd >> 3) & 0x0F;
  isign = dd & 0x80;
  for(ii = 1; ii < nextra; ii++) {
    dd = (*bdat++) & BMASK;
    *minval += dd << (8 * ii - 4);
  }

  if(isign != 0) *minval = -*minval;

  return(nextra + 1);

}

int
ccomp__2_comp(const int *dati, unsigned char *bdat, int nch, int nbits, int minval) {

  unsigned int dd;
  int  ll, mask;
  int ii,  npacked, ntopack, lspace;
  unsigned char * cptr;

  if(nbits <= 0) return(0);

  if(nbits >= 32) {
    for(ii = 0; ii < nch; ii++) {
      dd = *dati++;
      *bdat++ = (dd >> 24) & BMASK;
      *bdat++ = (dd >> 16) & BMASK;
      *bdat++ = (dd >>  8) & BMASK;
      *bdat++ =  dd        & BMASK;
    }
    return(4 * nch);
  }

  mask    = nmask[nbits];
  cptr    = bdat;
  dd      = 0;
  npacked = 0;
  for(ii = 0; ii < nch; ii++) {
    ll = (*dati++ - minval) & mask;
    ntopack = nbits;
    while(ntopack > 0) {
      lspace = 32-npacked;
      if(lspace >= ntopack) {
        dd = (dd << ntopack) | ll;
        npacked += ntopack;
        ntopack = 0;
      }
      else {
        dd = (dd << lspace) | (ll >> (ntopack - lspace) );
        npacked = 32;
        ntopack = nbits-lspace;
        ll = ll & nmask[ntopack];
      }
      if(npacked == 32) {
        *cptr++ = (dd >> 24) & BMASK;
        *cptr++ = (dd >> 16) & BMASK;
        *cptr++ = (dd >>  8) & BMASK;
        *cptr++ =  dd        & BMASK;
        dd = 0;
        npacked = 0;
      }
    }
  }
  if(npacked > 0) {
    lspace = 32 - npacked;
    dd = dd << lspace;
    while(npacked > 0) {
      *cptr++ = (dd >> 24) & BMASK;
       dd <<= 8;
       npacked -= 8;
    }
  }

  return(cptr - bdat);
  
}



int
ccomp__2_deco(int *dati, unsigned char *bdat, int nch, int nbits, int minval) {

  unsigned int dd, ee;
  int ii, mask, npacked, nextra;
  unsigned char * cptr;
  
 
/* printf(" NBits: %d   Minval: %d\n",nbits,minval);*/
  if(nbits <= 0) {
    for(ii = 0; ii < nch; ii++) *dati++ = minval;
    return(0);
  }

  if(nbits >= 32) {
    for(ii = 0; ii < nch; ii++) {
      dd = *bdat++;
      dd = (dd << 8) | *bdat++;
      dd = (dd << 8) | *bdat++;
      dd = (dd << 8) | *bdat++;
      *dati++ = dd;
    }
    return(SIZEINT * nch);
  }

  cptr = bdat;
  npacked = 0;
  mask = nmask[nbits];
  dd = 0;
  ee = 0;
  if(nbits <= 26) {
    for(ii = 0; ii < nch; ii++) {
      while (npacked < nbits) {
        dd = (dd << 8) | *cptr++;
        npacked += 8;
      }
      *dati++ = ( (dd >> (npacked - nbits)) & mask) + minval;
      npacked -= nbits;
    }
  }
  else {
    for(ii = 0; ii < nch; ii++) {
      while (npacked < nbits) {
        npacked += 8;
        if(npacked > 32) ee = dd ; 
        dd = (dd << 8) | *cptr++;
      }
      if(npacked > 32) {
        nextra = nbits + 8 - npacked ;
/*        ee <<= ee << nextra;
        *dati++ = ee | ((dd >> (8 - nextra)) & nmask[nextra]); */
	ee <<= nextra;
	*dati++ = ((ee | ((dd >> (8-nextra)) & nmask[nextra])) & mask)+ minval;
      }
      else {
        *dati++ = ( (dd >> (npacked - nbits)) &mask ) + minval;
      }
      npacked -= nbits;
    }
  }

  return(cptr - bdat);
  
}


int
ccomp__3_compress(const int *data, int nch, unsigned char *pack) {

  unsigned char * chptr;
  const int  * ptr;
  unsigned int dd;
  int ii, ll, nbits;


    ptr = data;
  chptr = pack;
  
  dd = 0; nbits = 0;
  for(ii = 0; ii < nch; ii++, ptr++) {
    ll = *ptr;
    if(ll) {
      if(ll <= (TBITS - nbits) ) {
        dd = (dd << ll) | nmask[ll];
        nbits += ll;
        if(nbits == TBITS) {
          *chptr++ = dd;
          nbits = 0;
        }
      }
      else {
        dd = (dd << TBITS-nbits) | nmask[TBITS-nbits];
        *chptr++ = dd;
        ll -= (TBITS-nbits);
        nbits = 0;
        while(ll >= TBITS) {
          *chptr++ = BMASK;
          ll -= TBITS;
        }
        if(ll) {dd = nmask[ll]; nbits = ll;}
      }
    }
    dd <<=  1;
    nbits++;
    if(nbits == TBITS) {
      *chptr++ = dd;
      nbits = 0;
    }
  }

  if(nbits){
     dd = (dd << (TBITS-nbits)) & ((nmask[TBITS-nbits]) ^ BMASK);
     *chptr++ = dd;
   }

  return(chptr-pack);

}


int
ccomp__3_decompress(int *data, int nch, unsigned char *pack) {

  int  * ptr;
  unsigned char * chptr;
  unsigned int dd;
  int ii, nn, ll, nbits;

  ptr = data; chptr = pack;
  nbits = 0; ii = 0; ll = 0;

  while (1) {
    dd = (*chptr++);
    nbits  = TBITS;
    for(nn = 0; nn < TBITS; nn++) {
      if(dd & TVAL) ll++;
      else {
        *ptr++ = ll;
        if(++ii >= nch) {
	  return(chptr - pack);
	}
        ll = 0;
      }
      dd <<= 1;
    }
  }
}

#if defined( _GW_BIG_ENDIAN )

void swap_bytes(char *dataIn, int SizeOfData) {

  int i;
  char *dataOut, buff[64];
  
  dataOut=&buff[0];
  for(i=0; i<SizeOfData; i++) *(dataOut + i)= *(dataIn + i);
  for(i=0; i<SizeOfData; i++) *(dataIn+i) = *(dataOut+ SizeOfData-i-1);
}

#endif
