#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <fcntl.h>
#include <sys/types.h>

#define  NBGO   15      /* no bgo detectors per ge +1     */
#define  NGE    111     /* no ge detectors                */
#define  MAXID  NGE-1   /* max id                         */

#define DATASIZE 16384
#define NETSIZE 16390
#define MAXVALIDHIE 16127
#define TAPEHEADER 112
#define FILEHEADER  90
#define EOFSIZE      0
#define ERRORSIZE   -1
#define FILEMARK 3

#define SPLEN   8192
#define MAXSANE 30		/* max multiplicity before error return */
#define MAX_MATS 10
#define MAX_SEG  7
#define NBADMAX  0
#define M12 0x0fff

/************************************************************************
       DEFINE TAPE HEADER RECORD
*************************************************************************/
typedef struct tape_header
    {
    unsigned short int RecordType;	/* tape header type= 1             */
    unsigned short int RecordLength;	/* number of bytes in this record  */
    unsigned short int RecordVer;	/* record version or subtype       */
    unsigned short int  ByteOrder_1;	/* to determine byte ordering      */
    unsigned short int  ByteOrder_2;	/* to determine byte ordering      */
    char    exp_title1[40];	        /* first part of exp title         */
    char    exp_title2[40];	        /* 2nd  part of exp title          */
    char    time[9];		        /* hh:mm:ss			   */
    char    date[9];		        /* yy/mm/dd			   */
    unsigned short int TapeNum;		/* tape number in this experiment  */
    unsigned short int TapeUnit;	/* specifies which unit wrote this tape */
}       TAPE_HEADER;

/************************************************************************
       DEFINE FILE HEADER RECORD
*************************************************************************/
typedef struct file_header
    {
    unsigned short int RecordType;	/* file header type= 2             */
    unsigned short int RecordLength;	/* number of bytes in this record  */
    unsigned short int RecordVer;	/* record version or subtype       */
    unsigned short int RunNumber;	/* run number in this experiment   */
    unsigned short int FileNumber;	/* file number in this experiment  */
    char    run_title1[40];		/* first part of run title         */
    char    run_title2[40];		/* 2nd  part of run title          */
}       FILE_HEADER;

/************************************************************************
       DEFINE EVENT DATA RECORD
*************************************************************************/
#define EB_CKSUM_NONE  0	/* no checksum                          */
#define EB_CKSUM_SUM16 1	/* 16 but simple checksum               */
#define EB_CKSUM_CRC16 2	/* crc16 checksum                       */
#define EB_HEADER_LEN  22       /* number of bytes in event header      */
#define EB_SIZE  (8192-11)	/* number of data words in event buffer */
typedef struct event_buffer
    {
    unsigned short int RecordType;		/* 1 event data type= 3              */
    unsigned short int RecordLength;		/* 2 number of bytes in this record  */
    unsigned short int RecordVer;		/* 3 record version or subtype       */
    unsigned short int HeaderBytes;		/* 4 number of bytes in header       */
    unsigned short int EffNumber;		/* 5 eff processor number            */
    unsigned short int StreamID;		/* 6 event stream ID                 */
    unsigned short int EffSequence;		/* 7 eff sequence number             */
    unsigned short int ModeFlags;		/* 8 event format flags              */
    unsigned short int DataLength;		/* 9 number of i*2 data words        */
    unsigned short int ChecksumType;		/* 10 type of checksum               */
    unsigned short int Checksum;		/* 11 checksum value                 */
    unsigned short int EventData[EB_SIZE];	/* event data area                   */
}       EVENT_BUFFER;

/************************************************************************
       DEFINE LOG DATA RECORD
*************************************************************************/
typedef struct log_record
    {
    unsigned short int RecordType;		/* 1 event data type= 4              */
    unsigned short int RecordLength;		/* 2 number of bytes in this record  */
    unsigned short int RecordVer;		/* 3 record version or subtype       */
    int     LineCount;				/* Number of text lines following    */
    char    Line1[80];
    char    Line2[80];
    char    Line3[80];
    char    LineN[80];
}       LOG_RECORD;

/************************************************************************
       DEFINE SCALER DATA RECORD
*************************************************************************/
typedef struct scaler_record
    {
    unsigned short int RecordType;		/* 1 event data type= 5              */
    unsigned short int RecordLength;		/* 2 number of bytes in this record  */
    unsigned short int RecordVer;		/* 3 record version or subtype       */
    int    ScalerCount;				/* Number of scalers to follow     */
    int    Scaler1_ID;				/* Scaler ID                       */
    int    Scaler1;				/* Scaler value                    */
    int    Scaler2_ID;				/* Scaler ID                       */
    int    Scaler2;				/* Scaler value                    */
}       SCALER_RECORD;


/************************************************************************
       DEFINE CALIBRATION RECORD
*************************************************************************/
typedef struct calibration_record
    {
    unsigned short int RecordType;		/* 1 event data type= 6              */
    unsigned short int RecordLength;		/* 2 number of bytes in this record  */
    unsigned short int RecordVer;		/* 3 record version or subtype       */
    /* to be determined */
}       CALIBRATION_RECORD;


/************************************************************************
       DEFINE SOURCE CODE RECORD
*************************************************************************/
typedef struct source_record
    {
    unsigned short int RecordType;		/* 1 event data type= 7              */
    unsigned short int RecordLength;		/* 2 number of bytes in this record  */
    unsigned short int RecordVer;		/* 3 record version or subtype       */
    int    LineCount;				/* Number of text lines following  */
    char    Line1[80];
    char    Line2[80];
    char    Line3[80];
    char    LineN[80];
}       SOURCE_RECORD;

/************************************************************************
       DEFINE FRONT END HISTOGRAM RECORD
*************************************************************************/
        typedef struct histogram_record
    {
    unsigned short int RecordType;		/* 1 event data type= 8              */
    unsigned short int RecordLength;		/* 2 number of bytes in this record  */
    unsigned short int RecordVer;		/* 3 record version or subtype       */
    /*to be determined ..if we use this at all*/
}       HISTOGRAM_RECORD;
/*----------------------------------------------------------------*/


struct EVENT
{
  unsigned short int id;	/* detector id */
  unsigned short int gebit;     /* ge hit bit */
  unsigned short int bgohit;	/* bgo hitpattern */
  unsigned short int ehi;	/* high resolution ge energy */
  unsigned short int pu;	/* pileup bit */
  unsigned short int over;	/* overrange bit */
  unsigned short int tge;	/* germanium time */
  unsigned short int tc;	/* trap corrector */
  unsigned short int elo;	/* low resolution ge energy */
  unsigned short int eside;	/* side chan. energy for seg ge */
  unsigned short int tbgo;	/* bgo time */
  unsigned short int ebgo;	/* bgo energy */
};

struct EVHDR
{

  unsigned short int len;	/* total length of event */
  unsigned short int len_clean;	/* no of clean ge */
  unsigned short int len_dirty;	/* no of dirty ge */
  unsigned short int len_bgo;	/* no of bgo-only */
  unsigned short int ttH;	/* usec-time high bits  */
  unsigned short int ttM;	/* usec-time medium bits*/
  unsigned short int ttL;	/* usec-time low bits   */
  unsigned short int tac1;	/* tac1 */
  unsigned short int tac2;	/* tac2 (RF tac) */
  unsigned short int sumge;	/* ge summed energy */
  unsigned short int sumbgo;	/* bgo summed energy */
};


typedef struct MICROBALL
{
	short int id;
	short int e;
	short int t;
	short int p;
} MICROBALL;
	
typedef struct NSHELL
{
	short int id;
	short int eh;
	short int el;
	short int p;
	short int tof;
} NSHELL;

int print_buffer_header( EVENT_BUFFER *, int * );
int print_buffer_data( EVENT_BUFFER *, int, int );

int get_ev( EVENT_BUFFER*, int*, int, int, struct EVHDR*, struct EVENT*, struct MICROBALL *, struct NSHELL *, int * );
int get_lecroy_( unsigned short int *, struct MICROBALL *, struct NSHELL * );



/*  GSORT interface functions */

int get_gsph_headtype_(unsigned short int *data) {

  int type;
  
  type = -1;
  if ( (((*data)&0xff00) != 0)&&(((*data)&0x00ff) == 0) )type=(*data>>8);
  if ( (((*data)&0xff00) == 0)&&(((*data)&0x00ff) != 0) )type=*data;
  return type;
 }
 
int get_gsph_byteorder_(unsigned short int *data1, unsigned short int *data2) {
 
   int byteorder;
   union {
      unsigned short int s[2];
      unsigned int       i;
      } tmp;

    tmp.i = 0;
    tmp.s[0] = *data1;
    tmp.s[1] = *data2;
    
    byteorder = -1;
    if( tmp.i == 0x01020304 )byteorder = 0;
    if( tmp.i == 0x04030201 )byteorder = 1;
    return byteorder;
 }
 




/************** From gsII_bufp.c *****************/

/* this function is called whenever we have */
/* a buffer to process                      */

int
gsii_bufp_(eb, hdr, ev, ub, ns, ext)

  EVENT_BUFFER   *eb;
  struct EVENT    ev[];
  struct EVHDR   *hdr;
  struct MICROBALL *ub;
  struct NSHELL *ns;
  int *ext;

{

  /* declarations */

  int             i, sta;
  static int      first_time = 1;
  int             i0, i1, i2, i3;
  static int      dl;
  static int      bitset[16];
  static int      maxword;
  int             bgo_segment;
  static int      st = 1;
  static int      evno = 0;

  static int      bincode[15] = {1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384};

#if(0)
  printf("data>\n");
  for (i = 0; i < 30; i++)
    printf("%2i -> 0x%4.4x\n", i, eb->EventData[i]);
  if (1 == 1)
    exit(0);
#endif


  if (st != 0)
  {

    /*---------------------------*/
    /* new buffer so read header */
    /*---------------------------*/

    evno = 0;

    /* extract data structure information from the */
    /* buffer header. We need this information to  */
    /* read the data correctly in get_ev           */

    for (i = 0; i < 16; i++)
      if ((eb->ModeFlags & bincode[i]) == bincode[i])
	bitset[i] = 1;
      else
	bitset[i] = 0;

    /* find max # words we have */

    maxword = eb->DataLength;
    if (maxword >= EB_SIZE)
      return (11);

    /*-------------------*/
    /* find event length */
    /*-------------------*/

    /* __ always hpid, ge_high and ge_side */

    dl = 3;

    /* __ ge_time */

    if (bitset[4] || bitset[5])
      dl += 1;

    /* __ ge_trap and ge_low */

    if (bitset[5])
      dl += 2;

    /* __ bgo_time and bgo_low */

    if (bitset[6])
      dl += 2;

    /* debug info */

    if (first_time == 1)
    {
      first_time = 0;
      print_buffer_header(eb, bitset);
      fflush(stdout);
    };

    /* extract events from the buffer                        */
    /* note: negative st is true error whereas st=1          */
    /* signals that all data in present buffer has been read */

  };

  /* get the next event */

  st = get_ev(eb, bitset, maxword, dl,
	      hdr, ev, ub, ns, ext);

  /* done */

  return (st);

}

/*--------------------------------------------------------------------*/

int
get_ev(eb, bitset, maxword, dl,
       hdr, ev, ub, ns, ext)

/* get the next event in the data buffer and return it */
/* in the simplified ev event array (a structure)      */
/* the return structure will fill in zero in where it  */
/* does not have any information. In that way the      */
/* the caller function need not know about the event   */
/* structure                                           */

/* input to function */

  EVENT_BUFFER   *eb;		/* data buffer                */
  int             bitset[16];	/* modeflag bits decomposed   */
  int             maxword;	/* maximum # word that can    */
  int             dl;		/* each detector word length  */

  struct EVENT    ev[];		/* simplified universal event */
  struct EVHDR   *hdr;		/* event header               */
  struct MICROBALL *ub;
  struct NSHELL *ns;
  int *ext;	/* external detectors         */

{

  /* declarations */

  static unsigned int      pos = 0;	/* current buffer index */
  int             i;
  unsigned short int len_words;	/* total # words in event */
  static int      nbad = 0;
  int             next, ok, cpos;

#if(0)

  /* debug print the data */

  print_buffer_data(eb, pos, pos + 100);

#endif

  /* gs119, have to skip something just after the buffer header */
  /* don't know at this point what it is; but we skip it!!!     */


  /* search for first valid event */

  if (pos == 0)
  {

    if( eb->DataLength < 1024 ) return 1;
    ok = 0;

    while (!ok)
    {

      /* printf("search from pos %i\n",pos); */

      /* find a candidate */

      while ( ((eb->EventData[pos] & 0xc000) != 0x8000 ) && (pos < eb->DataLength) )
	pos++;
      /* printf("candidate at pos %i, value:
       * 0x%4.4x\n",pos,eb->EventData[pos]); */

      /* extract the event length */

      len_words = eb->EventData[pos] & 0x0fff;
      /* printf("len: %i\n",len_words); */
      len_words--;

      /* check that we have an eoe there                   */
      /* (make sure we do not read too far in the process) */

      cpos = pos + len_words;
      if (cpos < 300)
      {
	if (eb->EventData[cpos] == 0xffff)
	  ok = 1;
	else
	  /* seek forward */

	  pos++;

      } else
      {

#if(0)
	printf("check position too long: %i\n", cpos);
	fflush(stdout);
	printf("pos: %i\n", pos);
	printf("len_words: %i\n", len_words);
	for (i = 0; i < 300; i++)
	  printf("%3i: 0x%4.4x\n", i, eb->EventData[i]);
	fflush(stdout);
#endif

	/* seek forward */

	pos++;

      };

#if(0)
      if (ok)
	printf("seems to be ok\n");
      else
      {
	printf("not ok, found 0x%4.4x", eb->EventData[pos + len_words]);
	printf(" at pos: %i\n", pos + len_words);
      };
#endif

    };

  };

  /* printf("pos now: %i\n",pos); */

  /*-------------------------------------------------*/
  /* check we have 80nn type of word or return error */
  /*-------------------------------------------------*/

  if ((eb->EventData[pos] & 0xc000) != 0x8000)
  {

#if(0)
    printf("%4.4x at pos: %i, error 6, not start of event\n",
	   eb->EventData[pos], pos);
#endif
    pos = 0;
    return (6);
  };
  /* printf("pos: %4i, ",pos); */

  /* check we have enough space to read the header */

  if ((pos + 7) > EB_SIZE)
  {
    pos = 0;
    return (7);
  };

  /*-------------------------------*/
  /* read the 7 event header words */
  /*-------------------------------*/

  /* 1_:get total length of the event (in words) */

  len_words = eb->EventData[pos++] & 0x0fff;

  /* 2_:get clean number of germaniums */

  hdr->len_clean = eb->EventData[pos++] & 0x00ff;

  /* 3_:get clean number of ge in event */

  hdr->len_dirty = (unsigned short int) (eb->EventData[pos] & 0xff00) >> 8;

  /* 3_:number of bgo only in event */

  hdr->len_bgo = eb->EventData[pos++] & 0x00ff;

  /* other header info  */

  hdr->ttH = eb->EventData[pos++];	        /* 4 */
  hdr->ttM = eb->EventData[pos++];	        /* 5 */
  hdr->ttL = eb->EventData[pos++];	        /* 6 */

  hdr->tac1 = eb->EventData[pos++] & M12;	/* 7 */
  hdr->tac2 = eb->EventData[pos++] & M12;	/* 8 */

  hdr->sumge = eb->EventData[pos++];	        /* 9 */
  hdr->sumbgo = eb->EventData[pos++];	        /* 10 */

  /*---------------------*/
  /* done reading header */
  /*---------------------*/

  /* determine # of detectors to read in the events (len) */

  /* ---- we always have the clean detectors */

  hdr->len = hdr->len_clean;

  /* ---- if the dirty ones are read out also... */

  if (bitset[7])
    hdr->len += hdr->len_dirty;

  /* ---- and if the bgos with no ges are read out too... */

  if (bitset[8])
    hdr->len += hdr->len_bgo;

  if (hdr->len > MAXSANE)
  {
    pos = 0;
    /* printf("MAXSANE condition\n"); printf("hdr->len_clean=%i\n",
     * hdr->len_clean); printf("hdr->len_dirty=%i\n", hdr->len_dirty);
     * printf("hdr->len_bgo  =%i\n", hdr->len_bgo); printf("hdr->len =%i\n",
     * hdr->len); */
    hdr->len = 0;
    return (8);
  };

  /* fill the ev structure array */

  for (i = 0; i < (int) hdr->len; i++)
  {

    /* check we have enough buffer left to read event */

    if ((pos + dl) > EB_SIZE)
    {
      pos = 0;
      return (9);
    };

    /* printf("pos at loop start: %i, i= %i\n",pos,i); */

    /* read germanium data (allways there) */

    ev[i].id = eb->EventData[pos] & 0x00ff;
    ev[i].bgohit = (unsigned short int) (eb->EventData[pos] & 0xfe00) >> 9;
    ev[i].gebit = (unsigned short int) (eb->EventData[pos++] & 0x0100) >> 8;

    /* make sure the ge id is from 0...110 only */

    if ((int) ev[i].id > MAXID)
      ev[i].id = 0;

    ev[i].ehi = eb->EventData[pos] & 0x3fff;
    ev[i].pu = (unsigned short int) (eb->EventData[pos] & 0x8000) >> 15;
    ev[i].over = (unsigned short int) (eb->EventData[pos++] & 0x4000) >> 14;
    ev[i].eside = eb->EventData[pos++] & M12;

    /* germanium time */

    if (bitset[4] || bitset[5])
      ev[i].tge = eb->EventData[pos++] & 0x1fff;
    else
      ev[i].tge = 0;

    /* full ge data */

    if (bitset[5])
    {

      ev[i].tc = eb->EventData[pos++] & M12;
      ev[i].elo = eb->EventData[pos++] & M12;
      /* printf("tc,elo,eside:
       * %4i,%4i,%4i\n",ev[i].tc,ev[i].elo,ev[i].eside); */
    } else
    {
      ev[i].tc = 0;
      ev[i].elo = 0;
    };

    /* bgo data */

    if (bitset[6])
    {

      ev[i].tbgo = eb->EventData[pos++] & M12;
      ev[i].ebgo = eb->EventData[pos++] & M12;
      /* printf("tbgo,ebgo: %i4,%i4\n",ev[i].tbgo,ev[i].ebgo); */
    } else
    {
      ev[i].tbgo = 0;
      ev[i].ebgo = 0;
    }

  };

  /* now check that we have a 0xff00 or 0xfff */
  /* here or hunt for one or the other        */

  while (!(eb->EventData[pos] == 0xffff || eb->EventData[pos] == 0xff00)
	 && pos < maxword)
    pos++;

  /* end of buffer ? */

  if (pos >= maxword)
  {

    /* reset buffer index and send error code back */
    /* requesting new buffer read                  */

    pos = 0;
    return (1);
  };

  /* check whether we have external detectors at this point */

  /* printf("pos: %i, val: 0x%4.4x\n",pos,eb->EventData[pos]);
   * fflush(stdout); */

  if (eb->EventData[pos] == 0xffff)
  {

    /* no external data */

    /* printf("no extrenal data\n"); fflush(stdout); */
    *ext = 0;

  } else if ((eb->EventData[pos] & 0xff00) == 0xff00)
  {

    /* we have external detector data        */
    /* keep reading it until we hit an       */
    /* 0xffff end-of-event marker            */
    /* (no interpretation in this routine)   */

    /* printf("have external data..."); fflush(stdout); */

    /* put number of external events in first slot */

    pos++;
    next = 0;
    *ext = get_lecroy_(&(eb->EventData[pos]), ub, ns );
    while (eb->EventData[pos] != 0xffff && pos < maxword) pos++;

    /* then copy the external data 
    ext[next++] = eb->EventData[pos++];

    while (eb->EventData[pos] != 0xffff && pos < maxword)
      ext[next++] = eb->EventData[pos++];

  } else
  {


    printf("ooooops, pos: %i\n", pos);
    print_buffer_data(eb, pos - 30, pos + 30);
    exit(0);

 */
 };
  /* end of buffer ? */

  if (pos >= maxword)
  {

    /* reset buffer index and send error code back */
    /* requesting new buffer read                  */

    pos = 0;
    return (1);
  };

  /*-----------------------------------------------------*/
  /* check next entry is an end-of-event as it should be */
  /*-----------------------------------------------------*/

  /* printf("\npos at check: %i\n",pos); */
  if (eb->EventData[pos] == 0xffff)
  {

    /* position pointer at the start of the next event */

    pos++;

    /* end of buffer ? (or that we are not too far to continue) */

    if (pos >= maxword)
    {
      pos = 0;
      return (1);
    };

    return (0);			/* declare victory */

  } else
  {

    /* return error                                       */
    /* this position should have been an end of the event */
    /* and it was not! - so we have a problem             */
    /* we now write lots of stuff so we can figure out    */
    /* what happened                                      */

    nbad++;

    if (nbad < NBADMAX)
    {
      fflush(stdout);
      printf("\n* no end_of_event - found: %i", eb->EventData[pos]);
      printf(" at pos: %i\n", pos);
      printf("event len: %i\n", hdr->len);
      printf("returning error +2\n");
      print_buffer_data(eb, pos - 40, pos + 20);
      printf("hdr->len_clean=%i\n", hdr->len_clean);
      printf("hdr->len_dirty=%i\n", hdr->len_dirty);
      printf("hdr->len_bgo  =%i\n", hdr->len_bgo);
      printf("hdr->len      =%i\n", hdr->len);
      printf("done dumping from gsII_buffp.c, return 10\n");
      fflush(stdout);
    };

    /* reset index and return error */

    pos = 0;
    hdr->len = 0;
    return (10);

  };

}


/*----------------------------------------------------------------*/

int
print_buffer_header(eb, bitset)

  EVENT_BUFFER   *eb;
  int             bitset[16];

{
  EVENT_BUFFER    p;
  int             i;

  /* for (i=0;i<10;i++) { printf("%i: %i\n",i,eb+i); } */

  /* print all information */

  printf("--------------------------------------\n");
  printf("RecordType..: %5i, ", eb->RecordType);
  printf("RecordLength: %5i, ", eb->RecordLength);
  printf("RecordVer...: 0x%4.4x\n", eb->RecordVer);
  printf("HeaderBytes.: %5i, ", eb->HeaderBytes);
  printf("EffNumber...: %5i, ", eb->EffNumber);
  printf("StreamID....: %5i\n", eb->StreamID);
  printf("EffSequence.: %5i, ", eb->EffSequence);
  printf("ModeFlags...:0x%4.4x, ", eb->ModeFlags);
  printf("DataLength..: %5i\n", eb->DataLength);
  printf("ChecksumType: %5i, ", eb->ChecksumType);
  printf("Checksum....: 0x%4.4x\n", eb->Checksum);

  printf("--->");
  if (bitset[1])
    printf(" [gain]");
  if (bitset[2])
    printf(" [time_veto]");
  if (bitset[3])
    printf(" [hon_veto]");
  printf("\n");

  printf("--->");
  if (bitset[4])
    printf(" [wrt_ge_t]");
  if (bitset[5])
    printf(" [wrt_ge_full]");
  if (bitset[6])
    printf(" [wrt_bgo_data]");
  if (bitset[7])
    printf(" [wrt_all_ge]");
  if (bitset[8])
    printf(" [wrt_bgo_det]");
  printf("\n");

  printf("--->");
  if (bitset[9])
    printf(" [isomer_tag]");
  if (bitset[10])
    printf(" [rf_sub]");
  printf("\n");

  printf("--------------------------------------\n\n\n");

  /* done */

  return (0);
}

/*----------------------------------------------*/

int
print_buffer_data(eb, lo, hi)

  EVENT_BUFFER   *eb;
  int             lo, hi;

{

  /* declarations */

  int             i;
  unsigned short int val;

  /* print in simple format */

  if (lo < 0)
    lo = 0;
  if (hi > eb->DataLength / sizeof(u_short))
    hi = eb->DataLength / sizeof(u_short);
  printf("\nbuffer print: from %i to %i\n", lo, hi);

  for (i = lo; i < hi; i++)
  {
    printf("%4i: ", i);

    printf("%5.5i ", eb->EventData[i]);

    printf("[0x%4.4x]", eb->EventData[i]);

    val = eb->EventData[i] & 0xff00;
    val = val >> 8;
    printf("{0x%4.4x/%3.3i,", val, val);
    val = eb->EventData[i] & 0x00ff;
    printf("0x%4.4x/%3.3i}", val, val);


    printf("____________________\n");

    if (eb->EventData[i] == 0xffff)
      printf("=========================\n");
  };

  /* done */

  if (1 == 1)
    exit(0);
  return (0);
}


/* ------------------------------------- */

int
print_event(hdr, ev, nn)

  struct EVHDR    hdr;
  struct EVENT    ev;
  int             nn;

{

  /* declarations */

  int             i, j;

  /* print the header only if first event */

  if (nn == 0)
  {
    printf("EV len");
    printf(" cln");
    printf(" dty");
    printf(" bgo");
    printf("  ttH ");
    printf(" ttM ");
    printf(" ttL ");
    printf(" tac1");
    printf(" tac2");
    printf(" sge ");
    printf(" sbgo\n");
    printf("**  %2.2i", hdr.len);
    printf("  %2.2i", hdr.len_clean);
    printf("  %2.2i", hdr.len_dirty);
    printf("  %2.2i", hdr.len_bgo);
    printf(" %4.4i ", hdr.ttH);
    printf("%4.4i ", hdr.ttM);
    printf("%4.4i ", hdr.ttL);
    printf("%4.4i ", hdr.tac1);
    printf("%4.4i ", hdr.tac2);
    printf("%4.4i ", hdr.sumge);
    printf(" %4.4i\n", hdr.sumbgo);
  }
  if (nn == 0)
  {
    printf(" ev# ");
    printf(" id ");
    printf("  ehi ");
    printf("g ");
    printf("p ");
    printf("o ");
    printf(" tge ");
    printf("ebgo ");
    printf("tbgo ");
    printf(" elo ");
    printf("esid ");
    printf("  tc ");
    printf("bgohitp ");

    printf("\n");
    fflush(stdout);
  };

  printf("%4i ", nn);
  printf("%3.3i ", ev.id);
  printf("%5.5i ", ev.ehi);
  printf("%1.1i ", ev.gebit);
  printf("%1.1i ", ev.pu);
  printf("%1.1i ", ev.over);
  printf("%4.4i ", ev.tge);
  printf("%4.4i ", ev.ebgo);
  printf("%4.4i ", ev.tbgo);
  printf("%4.4i ", ev.elo);
  printf("%4.4i ", ev.eside);
  printf("%4.4i ", ev.tc);

  j = 1;
  for (i = 0; i < 7; i++)
  {
    if ((ev.bgohit & j) == j)
      printf("1");
    else
      printf("0");
    j = j * 2;
  };

  printf("\n");
  fflush(stdout);

  return (0);

}

/*----------------------------------------------------------------------*/

int print_fera(extdata)

  unsigned short int extdata[];

{

  /* declarations */

  int             i, indx, k, vsn, wds, chan, data, last, len;

  /* hello world */

  i = 1;
  len = extdata[0];
  last = i + len;
  printf("has external data - length: %i words\n", len);

#if(0)
  for (i = 0; i < 20; i++)
    printf("%2i --> 0x%4.4x\n", i, extdata[i]);

  if (1 == 1)
    exit(0);
#endif

  /* parse and print fera data */

  while (i < last)
  {

    /* first word better be a fera header */

    if (extdata[i] & 0x8000 != 0x8000)
    {
      printf("first fera word not a header");
      printf(" [%4.4x]\n", extdata[i]);
      return (0);
    };

    /* header */

    printf("-------------------------------------\n");
    wds = (extdata[i] & 0x7800) >> 11;
    if (wds == 0)
      wds = 16;
    vsn = extdata[i] & 0x07ff;
    printf("[0x%4.4x] ", extdata[i]);
    printf("FERA header--> VSN: %4i/0x%4.4x, words: %4i\n", vsn, vsn, wds);
    i++;
    printf("------------\n");


    /* data */

    for (k = 0; k < wds; k++)
    {
      chan = (extdata[i] & 0x7800) >> 11;
      data = extdata[i] & 0x07ff;
      printf("chan: %4i - data: %6i", chan, data);
      printf(" [0x%4.4x]", extdata[i]);

      /* check highest bit isn't set */

      if ((extdata[i] & 0x8000) == 0x8000)
	printf(" ... ooops, high bit set");

      printf("\n");
      i++;

    };

  };

  /* done */

  printf("\n");
  return (0);

}


int get_lecroy_( unsigned short int *data, struct MICROBALL *mb, struct NSHELL *ns )
{
    unsigned short int *d;
    unsigned short int *last;
    int dw;
    
    int nwFERA;
    int ii, nn;
    int id, vsn;
    
    int nconv;
    
    
    nwFERA = *data;
    nconv = 0;
    
    if( nwFERA < 2 ) return nconv;
    d = data+1;
    last = d + nwFERA;
    
    while( d < last )
    {
        if( (*d)&0x8000 )
	{
	    vsn = (*d)&0x00ff;
	     nn = ( (*d)&0x7800 ) >>11;
	     if( nn == 0 ) nn = 16;
	}
	else return nconv;
	
	   switch( vsn )
	   {
	      case 81:
	      {
		  for( ii = 0; ii < nn; ii++ )
		  {
		     dw = *(++d);
		     if( !( dw&0x8000 ) )
		     {
		     	id = ((dw>>11)&0x000F);
		     	ns[id].id = id;
		     	ns[id].eh  = dw&0x07ff;
		     	nconv++;
		     }
		  }
		  break;
	      }
	      case 85:
	      {
		  for( ii = 0; ii < nn; ii++ )
		  {
		     dw = *(++d);
		     if( !( dw&0x8000 ) )
		     {
		     	id = 16 + ((dw>>11)&0x000F);
		     	ns[id].id = id;
		     	ns[id].eh  = dw&0x07ff;
		     	nconv++;
		     }
		  }
		  break;
	      }

	      case 82:
	      {
		  for( ii = 0; ii < nn; ii++ )
		  {
		     dw = *(++d);
		     if( !( dw&0x8000 ) )
		     {
		     	id = ((dw>>11)&0x000F);
		     	ns[id].id = id;
		     	ns[id].el  = dw&0x07ff;
		     	nconv++;
		     }
		  }
		  break;
	      }
	      case 86:
	      {
		  for( ii = 0; ii < nn; ii++ )
		  {
		     dw = *(++d);
		     if( !( dw&0x8000 ) )
		     {
		     	id = 16 + ((dw>>11)&0x000F);
		     	ns[id].id = id;
		     	ns[id].el  = dw&0x07ff;
		     	nconv++;
		     }
		  }
		  break;
	      }

	      case 83:
	      {
		  for( ii = 0; ii < nn; ii++ )
		  {
		     dw = *(++d);
		     if( !( dw&0x8000 ) )
		     {
		     	id = ((dw>>11)&0x000F);
		     	ns[id].id = id;
		     	ns[id].p  = dw&0x07ff;
		     	nconv++;
		     }
		  }
		  break;
	      }
	      case 87:
	      {
		  for( ii = 0; ii < nn; ii++ )
		  {
		     dw = *(++d);
		     if( !( dw&0x8000 ) )
		     {
		     	id = 16 + ((dw>>11)&0x000F);
		     	ns[id].id = id;
		     	ns[id].p  = dw&0x07ff;
		     	nconv++;
		     }
		  }
		  break;
	      }

	      case 84:
	      {
		  for( ii = 0; ii < nn; ii++ )
		  {
		     dw = *(++d);
		     if( !( dw&0x8000 ) )
		     {
		     	id = ((dw>>11)&0x000F);
		     	ns[id].id = id;
		     	ns[id].tof  = dw&0x07ff;
		     	nconv++;
		     }
		  }
		  break;
	      }
	      case 88:
	      {
		  for( ii = 0; ii < nn; ii++ )
		  {
		     dw = *(++d);
		     if( !( dw&0x8000 ) )
		     {
		     	id = 16 + ((dw>>11)&0x000F);
		     	ns[id].id = id;
		     	ns[id].tof  = dw&0x07ff;
		     	nconv++;
		     }
		  }
		  break;
	      }
	      
	      case  97:
	      case  98:
	      case  99:
	      case 100:
	      case 101:
	      case 102:
	      {
	          vsn = (vsn-97)*16;
		  for( ii = 0; ii < nn; ii++ )
		  {
		     dw = *(++d);
		     if( !( dw&0x8000 ) )
		     {
		     	id = vsn + ((dw>>11)&0x000F);
		     	mb[id].id = id;
		     	mb[id].e  = dw&0x07ff;
		     	nconv++;
		     }
		  }
		  break;
	      }
	      
	      case 113:
	      case 114:
	      case 115:
	      case 116:
	      case 117:
	      case 118:
	      {
	          vsn = (vsn-113)*16;
		  for( ii = 0; ii < nn; ii++ )
		  {
		     dw = *(++d);
		     if( !( dw&0x8000 ) )
		     {
		     	id = vsn + ((dw>>11)&0x000F);
		     	mb[id].id = id;
		     	mb[id].t  = dw&0x07ff;
		     	nconv++;
		     }
		  }
		  break;
	      }
	      
	      case 129:
	      case 130:
	      case 131:
	      case 132:
	      case 133:
	      case 134:
	      {
	          vsn = (vsn-129)*16;
		  for( ii = 0; ii < nn; ii++ )
		  {
		     dw = *(++d);
		     if( !( dw&0x8000 ) )
		     {
		        id = vsn + ((dw>>11)&0x000F);
		        mb[id].id = id;
		        mb[id].p  = dw&0x07ff;
		        nconv++;
		     }
		  }
		  break;
	      }
	      
	      default:
	      {   
		  d++;
		  break;
	      }   
	  }
	  d++;
    }
    return nconv;
}   
	      
	   
   
    
    
    
