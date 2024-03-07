#include <stdio.h>
#include <termios.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#define RAW 1
#define RESTORE 0
#define MAXCHARS 5
#define MAXLENGTH 512
#define PRINTABLE 1
#define LEFTARROW 2
#define RIGHTARROW 3

#define aKEY 97
#define bKEY 98
#define cKEY 99
#define dKEY 100
#define eKEY 101
#define fKEY 102
#define gKEY 103
#define hKEY 104
#define iKEY 105
#define jKEY 106
#define kKEY 107
#define lKEY 108
#define mKEY 109
#define nKEY 110
#define oKEY 111
#define pKEY 112
#define qKEY 113
#define rKEY 114
#define sKEY 115
#define tKEY 116
#define uKEY 117
#define vKEY 118
#define wKEY 119
#define xKEY 120
#define yKEY 121
#define zKEY 122

#define AKEY 65
#define BKEY 66
#define CKEY 67
#define DKEY 68
#define EKEY 69
#define FKEY 70
#define GKEY 71
#define HKEY 72
#define IKEY 73
#define JKEY 74
#define KKEY 75
#define LKEY 76
#define MKEY 77
#define NKEY 78
#define OKEY 79
#define PKEY 80
#define QKEY 81
#define RKEY 82
#define SKEY 83
#define TKEY 84
#define UKEY 85
#define VKEY 86
#define WKEY 87
#define XKEY 88
#define YKEY 89
#define ZKEY 90

#define LSQBR 91
#define RSQBR 93
#define BSLASH 92
#define SLASH 47
#define DIEZ 35
#define EXCL 33
#define CPRGHT 64
#define DOLLAR 36
#define PERCT 37
#define HAT 94
#define APRSND 38
#define STAR 42
#define LPAR 40
#define RPAR 41
#define MINUS 45
#define EQUAL 61
#define USCORE 95
#define PLUS 43
#define BAR 124
#define AGRAVE 96
#define TILDA 126
#define AACUTE 39
#define DAACUTE 34
#define PCEDIL 59
#define TWOP 58
#define QSIGN 63
#define POINT 46
#define CEDIL 44
#define LTHAN 60
#define GTHAN 62
#define SPACE 32

#define ZEROKEY 48
#define ONEKEY 49
#define TWOKEY 50
#define THREEKEY 51
#define FOURKEY 52
#define FIVEKEY 53
#define SIXKEY 54
#define SEVENKEY 55
#define EIGTHKEY 56
#define NINEKEY 57

#define ESCKEY 27
#define BACKSP 8
#define DELKEY 127
#define TABKEY 9
#define EOFKEY 4
#define CRKEY 10


typedef struct CharStruct {
      int NofChars;
      unsigned char c[MAXCHARS];
      } CharStruct;
      
typedef struct StringStruct {
      int length;
      unsigned char *cbase;
      unsigned char *crt;
      } StringStruct;
      

static int RawState;
struct CharStruct *InputGot;
static struct CharStruct inp;
static struct StringStruct StrBuff;


int ISLSetTerminal(int mode);
struct CharStruct *ISLRead ( void );
int ISLGetInput ( void );
int ISLPutInput ( unsigned char *c, int n);
int ISLWhich ( void );
int ISLGetString ( unsigned char *c, int *n);


 int ISLSetTerminal(int mode){

    static struct termios  cooked, raw;
 
    if( RawState == 0) {
     if (mode == RAW) {
       tcgetattr(0,&cooked);
       memcpy(&raw, &cooked, sizeof(struct termios));
       raw.c_lflag &= ~(ICANON | ECHO ) ;
       raw.c_cc[VMIN] = 0;
       raw.c_cc[VTIME] = 0;
       RawState = 1;
       if(tcsetattr(0, TCSANOW, &raw) != 0)return 0;
       else return 1;
       }
     else return 1;
     }
   else {
     if (mode == RESTORE) {
       RawState = 0;
       if(tcsetattr(0, TCSANOW, &cooked)!= 0)return 0;
       else return 1;
       }
     else return 1;
     }
 }
 
 struct CharStruct *ISLRead ( void ) {
   
  if( RawState ) {
   inp.NofChars = read(0,&inp.c[0],MAXCHARS);
   
   if ( inp.NofChars ) return &inp;
   else  return NULL;
   }
  else {
    printf(" ISLRead$ERROR - call ISLSetTerminal(RAW) first\n");
    return NULL;
    }
 }
 
 
 int ISLGetInput ( void ) {
 
   InputGot = ISLRead();
   
   if( InputGot ) return 1;
   else  return 0;
 }
 
 
 int ISLPutInput ( unsigned char *c, int n) {
 
  int ii;
  unsigned char *cptr;
  
   if( c ) {
      InputGot = &inp;
      InputGot->NofChars = n;
      for ( ii = 0; ii < n; ii++) InputGot->c[ii] = *(c+ii);
      }
      
   if( InputGot ) {
     if ( StrBuff.cbase == NULL ) {
          StrBuff.cbase = (unsigned char *)calloc(MAXLENGTH, sizeof(unsigned char));
	  StrBuff.crt = StrBuff.cbase;
	  StrBuff.length = 0;
	  }
     if ( StrBuff.crt == NULL ) {
	  StrBuff.crt = StrBuff.cbase;
	  StrBuff.length = 0;
	  }
     }
   else return 0;
   
   switch ( ISLWhich() ) {
              case NULL : break;
	      case LEFTARROW : { if ( StrBuff.crt > StrBuff.cbase ) {
	                          printf("\b");
	                          StrBuff.crt--;
				  }
				 fflush(stdout);
				 break;
				 }
	      case RIGHTARROW : { if ( StrBuff.crt < (StrBuff.cbase+StrBuff.length) ) {
	                          for ( ii=0; ii<InputGot->NofChars; ii++)printf("%c",InputGot->c[ii]);
	                          StrBuff.crt++;
				  }
				 fflush(stdout);
				 break;
				 }
	      case PRINTABLE :  { for ( ii = 0; ii < InputGot->NofChars; ii++){
	                            if ( (InputGot->c[ii] > 31) && (InputGot->c[ii] < 127)) {
				        if ( StrBuff.crt < (StrBuff.cbase+StrBuff.length) ) {
					      for ( cptr = (StrBuff.cbase+StrBuff.length); cptr > StrBuff.crt; cptr--)
					           *cptr = *(cptr-1);
					      *(StrBuff.crt) = InputGot->c[ii];
					      }
					 else *(StrBuff.crt) = InputGot->c[ii];
					 StrBuff.length++;
					 for ( cptr = StrBuff.crt; cptr < (StrBuff.cbase+StrBuff.length); cptr++ )printf("%c",*cptr);
					 StrBuff.crt++;
					 for ( cptr = (StrBuff.cbase+StrBuff.length); cptr > StrBuff.crt;cptr--)printf("\b");
					 }
				         
	                            if ( (InputGot->c[ii] == BACKSP) || (InputGot->c[ii] == DELKEY)) {
				      if ( StrBuff.crt > StrBuff.cbase ){
				        if ( StrBuff.crt < (StrBuff.cbase+StrBuff.length) ) {
					      printf("\b");
					      for ( cptr = StrBuff.crt-1; cptr < (StrBuff.cbase+StrBuff.length); cptr++) {
					           printf(" ");
					           *cptr = *(cptr+1);
						   }
					      }
					 else printf("\b ");
					 StrBuff.crt--;
					 for ( cptr = (StrBuff.cbase+StrBuff.length); cptr > StrBuff.crt;cptr--)printf("\b");
					 StrBuff.length--;
					 for ( cptr = StrBuff.crt; cptr < (StrBuff.cbase+StrBuff.length); cptr++ )printf("%c",*cptr);
					 for ( cptr = (StrBuff.cbase+StrBuff.length); cptr > StrBuff.crt;cptr--)printf("\b");
					 }
				       }
				       
	                            if ( InputGot->c[ii] == CRKEY) {
				         StrBuff.crt = NULL;
					 printf("\n");
					 return 1;
					 }
	          
	                            if ( InputGot->c[ii] == EOFKEY) {
				         StrBuff.crt = NULL;
					 StrBuff.length = -1;
					 return -1;
					 }
                                     }
				    fflush(stdout);
				    break;
				    } 
	      default :  { break; }
	      }
    return 0;

 }
   
 int ISLWhich ( void ) {
 
   int iESC, ii;
   
     iESC = -1;
     
     for ( ii = 0; ii < InputGot->NofChars; ii++ ) if ( InputGot->c[ii] == ESCKEY ) iESC = ii;
     
     if( iESC > -1 ) {
        if( (InputGot->NofChars - iESC) == 3 ){
	    if ( InputGot->c[iESC+1] == 91 ){
	          switch ( InputGot->c[iESC+2] ){
		      case 68 : return LEFTARROW;
		      case 67 : return RIGHTARROW;
		      }
		  }
            }
	 return (int) NULL;
	 }
      else return PRINTABLE;
      
 }
 


 int ISLGetString ( unsigned char *c, int *n) {
 
   for( StrBuff.crt = StrBuff.cbase; StrBuff.crt < (StrBuff.cbase + StrBuff.length ); StrBuff.crt++) {
      *c = *(StrBuff.crt);
      c++;
      }
    *n = StrBuff.length;
    StrBuff.crt = NULL;
    return StrBuff.length;
 } 
		  
/* 
 int main ( void ) {
 
    printf(" Enter string : ");
    fflush(stdout);
    ISLSetTerminal( RAW );
    while ( ISLPutInput ( NULL, 0 ) == 0 ) ISLGetInput();
    printf(" String : ");
    for( StrBuff.crt = StrBuff.cbase; StrBuff.crt < (StrBuff.cbase + StrBuff.length ); StrBuff.crt++)
       printf("%c",*(StrBuff.crt));
    printf("\n");
    ISLSetTerminal( RESTORE );
 }  
*/
