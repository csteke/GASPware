#include <stdio.h>
#include <stdlib.h>

#define EB_SIMPLE     0         /* no DataFragmetLength and no HitPattern */
#define EB_DFL_ONLY   1         /* has DataFragmetLength but no HitPattern */
#define EB_DFL_SHP    2         /* has DataFragmetLength and 2 bytes - HitPattern  */
#define EB_DFL_LHP    3         /* has DataFragmetLength and 4 bytes - HitPattern */

typedef struct Detector {
     unsigned short int Format;
     unsigned short int Code;
     unsigned short int Family;
     unsigned short int MaxId;
     unsigned short int MaxLength;
     struct Detector *Next;
     } Detector;
     
struct Detector *BaseDet = NULL;
     

int iseventblock_( char *);
int isprismablock_( char *);
int getevtypelength_(unsigned short int *Data, int *NWords, short int *EvType, short int *EvLength);
int geteventstructure_(unsigned short int *Data, int *NWords);
int GetDetectorType( unsigned short int *Data, unsigned short int *FormatCode,
                     unsigned short int *DetCode, unsigned short int *DetFamily,
		     unsigned short int *DetId);
struct Detector *CreateDetector( void );
int ExistDetector ( unsigned short int );
struct Detector *WhichDetector ( unsigned short int Family);
void CheckMaxId ( unsigned short int Family, unsigned short int Id );
void CheckMaxLength ( unsigned short int Family, unsigned short int Length );
void PutDetectorType(  struct Detector *Det, unsigned short int Format, unsigned short int Code,
                       unsigned short int Family, unsigned short int MaxId, unsigned short int MaxLength  );
void showtypes_(void);

 int iseventblock_(char *s){

   if( 'E' != *s++ )return 0;
   if( 'B' != *s++ )return 0;
   if( 'E' != *s++ )return 0;
   if( 'V' != *s++ )return 0;
   if( 'E' != *s++ )return 0;
   if( 'N' != *s++ )return 0;
   if( 'T' != *s++ )return 0;
   if( 'D' != *s   )return 0;
   return 1;
 } 

 int isprismablock_(char *s){

   if( 'E' != *s++ )return 0;
   if( 'B' != *s++ )return 0;
   if( 'Y' != *s++ )return 0;
   if( 'E' != *s++ )return 0;
   if( 'D' != *s++ )return 0;
   if( 'A' != *s++ )return 0;
   if( 'T' != *s++ )return 0;
   if( 'A' != *s   )return 0;
   return 1;
 } 
  
  
 int getevtypelength_(unsigned short int *Data, int *NWords, short int *EvType, short int *EvLength) {
 
  unsigned int i;
  
  i=0;
  
  while (  ((*(Data+i))&0xffff)>>4 != (unsigned short int) 0x0fff ) 
          if ( (++i) >= *NWords ){*EvLength=0; return *NWords;}
  
  *EvType = *(Data+i) & (unsigned short int)0x000f;
  *EvLength = *(Data+i+1);
  return i;
 }
 
 
 int geteventstructure_(unsigned short int *Data, int *NWords){
 
  unsigned short int *Ptr, *EndPtr, *NextEventPtr, *SearchPtr;
  short int EventType, EventLength;
  unsigned short int FormatCode, DetCode, DetFamily, DetId;
  int NofEvents, LengthRest;
  unsigned short int LengthOfDet ;
 
   if(!iseventblock_( (char *)Data) )return 0;
   
   Ptr = Data + 16;
   SearchPtr = Ptr;
   EndPtr = Data + *NWords - 1;
   NextEventPtr = NULL;
   NofEvents = 0;
   while ( Ptr < EndPtr ) {
       EventLength = 0;
       while(EventLength == 0){
         LengthRest = ((int)(EndPtr-SearchPtr))/2;
         SearchPtr += getevtypelength_(SearchPtr+1 , &LengthRest, &EventType, &EventLength ) +1 ;
	 if( EventType > 3 ) return 0;
	 }
       Ptr = SearchPtr;
       NextEventPtr = Ptr + EventLength/2 ;
       if ( EndPtr >= NextEventPtr-1 ){
          switch ( EventType ) {
	     case 0 : { Ptr += 2; NofEvents++; break; }
	     case 1 : { Ptr += 4; NofEvents++; break; }
	     case 2 : { Ptr += 3; NofEvents++; break; }
	     case 3 : { Ptr += 5; NofEvents++; break; }
	     default: { printf ( " ERROR - unknown event type %d\n", EventType); return NofEvents;}
	     }
	  while ( Ptr < NextEventPtr-1 ) {
	      LengthOfDet = GetDetectorType(Ptr,&FormatCode,&DetCode,&DetFamily,&DetId)/2;
	      if( ExistDetector(DetFamily) ) { CheckMaxId ( DetFamily, DetId ); CheckMaxLength ( DetFamily, LengthOfDet ); }
	      else PutDetectorType( CreateDetector(),FormatCode, DetCode, DetFamily, DetId, LengthOfDet);
	      if( LengthOfDet ) Ptr += LengthOfDet;
	      else Ptr = NextEventPtr-1;
	     }
	         
	  }
       }
   return NofEvents;
}


 int GetDetectorType( unsigned short int *Data, unsigned short int *FormatCode,
                      unsigned short int *DetCode, unsigned short int *DetFamily, unsigned short int *DetId){
   
   unsigned short int dd;
   
   dd = *Data;
   *FormatCode = dd>>14;
   *DetFamily  = dd>>9;
   *DetCode    = (( dd<<2 )&0xffff)>>11;
   *DetId      = (( dd<<7 )&0xffff)>>7 ;
   if( *FormatCode ) return (int) *(Data+1);
   else   return 0;
 }


 struct Detector *CreateDetector( void ){

    struct Detector *Det;
 
    if( BaseDet ) {
       Det=BaseDet;
       while( Det->Next ) Det = Det->Next;
       Det->Next = (struct Detector *)calloc(1,sizeof(struct Detector));
       return Det->Next;
       }
    else {
      BaseDet = (struct Detector *)calloc(1,sizeof(struct Detector));
      return BaseDet;
      }
 }
 

 int ExistDetector ( unsigned short int Family){
 
   struct Detector *Det;
 
   if( BaseDet ) {
       Det = BaseDet;
       while( Det ) {
           if ( Det->Family == Family )return 1;
	   Det = Det->Next;
	   }
       return 0;
       }
   else return 0;
 }
 
 struct Detector *WhichDetector ( unsigned short int Family){
 
   struct Detector *Det;
 
   if( BaseDet ) {
       Det = BaseDet;
       while( Det ) {
           if ( Det->Family == Family )return Det;
	   Det = Det->Next;
	   }
       return NULL;
       }
   else return NULL;
 }


 void CheckMaxId ( unsigned short int Family, unsigned short int Id ) {

   struct Detector *Det;
 
   if( WhichDetector(Family)->MaxId < Id ) WhichDetector(Family)->MaxId = Id;
   
 }

 void CheckMaxLength ( unsigned short int Family, unsigned short int Length ) {

   struct Detector *Det;
 
   if( WhichDetector(Family)->MaxLength < Length ) WhichDetector(Family)->MaxLength = Length;
   
 }

      
 void PutDetectorType(  struct Detector *Det, unsigned short int Format, unsigned short int Code,
                       unsigned short int Family, unsigned short int MaxId, unsigned short int MaxLength ) {
 
    if( Det ){
         Det->Format = Format;
	 Det->Code   = Code;
	 Det->Family = Family;
	 Det->MaxId  = MaxId;
	 Det->MaxLength  = MaxLength;
	 }
 }
 

  void showtypes_(void) {
  
    struct Detector *Det;
    
    if( BaseDet ) {
       Det = BaseDet;
       while( Det ) {
          printf("\n found detector family %d format %d code %d   max Id %d max length %d\n", Det->Family,
	                                            Det->Format, Det->Code, Det->MaxId, Det->MaxLength);
	  Det = Det->Next;
	  }
       }
  }


/*      if(NextEventPtr) Ptr = NextEventPtr; */
/*	    printf(" Seen Det# %d  format %d code %d \n", DetId,FormatCode,DetCode);*/
