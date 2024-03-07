#include <stdio.h>
#include <sys/types.h>

#define RAW 1
#define RESTORE 0

extern int ISLSetTerminal(int mode);
extern int ISLGetInput ( void );
extern int ISLPutInput ( unsigned char *c, int n);
extern int ISLGetString ( unsigned char *c, int *n);


void inpx_( unsigned char *InterString, int32_t *InterLength ){

  unsigned char c, c3[3];
  int imore;
  
  fflush(stdin);
  fflush(stdout);
  ISLSetTerminal( RAW );
  imore = 0;
  while ( imore == 0 ) {
    ISLGetInput();
    imore = ISLPutInput( NULL, 0);
    }
  
  ISLGetString( InterString, InterLength);
  ISLSetTerminal(RESTORE);
 }
  
