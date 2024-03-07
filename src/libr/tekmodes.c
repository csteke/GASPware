#include <stdio.h>
#include <stdlib.h>

#define TEXT 1
#define TEKAN 2
#define TEKVEC 3

static int TermMode;
static int XGterm;

void SetCrtMode(int);

void textmode_(void);
void tekanmode_(void);
void tekvecmode_(void);
void teksetcolor_(short int *);

void TextToTekAn(void);
void TextToTekVec(void);
void TekToText(void);
void TekAnToVec(void);
void TekVecToAn(void);

/* FORTRAN callable functions */

void textmode_(void){

  SetCrtMode(TEXT);
  fflush(stdout);
 }
 
void tekanmode_(void){

  SetCrtMode(TEKAN);
  fflush(stdout);
 }

void tekvecmode_(void){
 
  SetCrtMode(TEKVEC);
  fflush(stdout);
 }

void teksetcolor_(short int *i){

 if(XGterm){
  if(*i >= 10)(*i)-=10*(*i/10);
  if(*i <= 0)*i=1;
  if( TermMode == TEXT )return;
  printf("\033/%dc",*i);
  fflush(stdout);
  }
 }


/* C */

void SetCrtMode(int Mode){

  
  if(TermMode == 0){
     if( getenv("XGTERM") )XGterm=1;
     else XGterm=0;
     switch( Mode ){
      case TEXT   :
        TekToText();    break;
      case TEKAN  :
        TextToTekAn();  break;
      case TEKVEC :
        TextToTekVec(); break;
      }
     TermMode=Mode;
     return;
     }
     
   if(TermMode == TEXT){
     switch( Mode ){
      case TEXT   :
        break;
      case TEKAN  :
        TextToTekAn();  break;
      case TEKVEC :
        TextToTekVec(); break;
      }
     TermMode=Mode;
     return;
     }

   if(TermMode == TEKAN){
     switch( Mode ){
      case TEXT   :
        TekToText();    break;
      case TEKAN  :
        break;
      case TEKVEC :
        TekAnToVec();   break;
      }
     TermMode=Mode;
     return;
     }

   if(TermMode == TEKVEC){
     switch( Mode ){
      case TEXT   :
        TekToText();    break;
      case TEKAN  :
        TekVecToAn();   break;
      case TEKVEC :
        break;
      }
     TermMode=Mode;
     return;
     }
 }
 
 
void TextToTekAn(void){

 if(!XGterm)printf("\033[?38h");
 printf("\037");
 printf("\n");
 }

void TextToTekVec(void){

 if(!XGterm)printf("\033[?38h");
 printf("\037");
 printf("\n");
 printf("\035");
 }

void TekToText(void){

 if(XGterm)printf("\030");
 else{
  printf("\033");
  printf("\035");
  printf("\030");
  printf("\033");
  printf("\003");
  }
 printf("=================================================\n");
 }

void TekAnToVec(void){

 printf("\035");
 }

void TekVecToAn(void){

 printf("\037");
 }


