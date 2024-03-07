#include <stdio.h>
#include <signal.h>



static char *StopFlag = NULL;

void gs_setStop( int );

 
void gs_install_sig_( char *flag )
{
    struct sigaction act;

    
    if( flag ) act.sa_handler = gs_setStop;
    else act.sa_handler = SIG_IGN;

    sigaction( 2, &act, NULL);
    sigaction(15, &act, NULL);
    
    StopFlag = flag;
}


void gs_setStop( int dummy )
{

   if( StopFlag ) *StopFlag = 1;
   
}



