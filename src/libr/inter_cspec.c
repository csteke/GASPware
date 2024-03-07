#include <stdio.h>
#include <sys/types.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef sun
# include <dirent.h>
# include <sys/dirent.h>
#endif

#ifdef Digital
# include <dirent.h>
/*# include <sys/dirent.h>*/
#endif

#ifdef __linux__
# include <unistd.h>
# include <dirent.h>
# include <linux/unistd.h> 
#endif

#include <string.h>

char *OldDirName;
struct dirent *DirEntries;
struct dirent *CrtEntry;
static int DirEntryLength;

void makecstring_(char *s){
   while( (*s != ' ') && (*s != '\0') )s++;
   *s='\0';
   }

/*
int ebgetfname_ ( char *DirName , char *Tag , char *FileName ){

 int DirDesc ;
 char *c , *d ;
 
  if ( DirEntries == NULL ) {
       DirEntries = (struct dirent *)calloc(1024, sizeof(struct dirent));
       if ( DirEntries == NULL ) return -1;
       }
  if ( OldDirName == NULL ) OldDirName = (char *)calloc(255, sizeof(char));   
  if ( strcmp( DirName, OldDirName ) != 0) {
       DirDesc = open( DirName, O_RDONLY);
       if( DirDesc == -1 ) return DirDesc;
       DirEntryLength = getdents(DirDesc, DirEntries, 1024*sizeof(struct dirent))-1 ;
       if ( DirEntryLength < 0 )return -1;
       close(DirDesc);
       CrtEntry = DirEntries;
       d = OldDirName;
       for ( c = DirName ; *c != '\0' ; c++) { *d = *c; d++;} 
       *d = '\0';
       }
  while ( strcmp( CrtEntry->d_name, ".") == 0 ) CrtEntry  = (struct dirent *)(((char *)CrtEntry) + CrtEntry->d_reclen) ;
  while ( strcmp( CrtEntry->d_name, "..") == 0 )CrtEntry  = (struct dirent *)(((char *)CrtEntry) + CrtEntry->d_reclen) ;

  while (  strstr( CrtEntry->d_name, Tag) != CrtEntry->d_name ){
    if ( CrtEntry->d_reclen == 0 ) { 
       DirEntries = (struct dirent *)realloc(DirEntries, 0);
       *OldDirName = '\0';
       return 0;
       }
    CrtEntry  = (struct dirent *)(((char *)CrtEntry) + CrtEntry->d_reclen) ;
    }
   for ( c = DirName; *c != '\0' ; c++) { *FileName = *c; FileName++;} 
   for ( c = CrtEntry->d_name; *c != '\0' ; c++) { *FileName = *c; FileName++;} 
   *(FileName) = '\0';
   CrtEntry  = (struct dirent *)(((char *)CrtEntry) + CrtEntry->d_reclen) ; 
   return 1;
   
}
*/



#if defined( __GW_LITTLE_ENDIAN )

void swap_bytes(char *dataIn, int SizeOfData) {

  int i;
  char *dataOut, buff[64];
  
  dataOut=&buff[0];
  for(i=0; i<SizeOfData; i++) *(dataOut + i)= *(dataIn + i);
  for(i=0; i<SizeOfData; i++) *(dataIn+i) = *(dataOut+ SizeOfData-i-1);
}

#endif


int ebh_read_( char *name, int *spec ) {

  FILE *handle;
  int Nread, ii;
  
  
  handle = NULL;
  handle =  fopen (name,"rb");
  if( handle == NULL )return -1;
  
  if( handle ) {
       fseek ( handle, 500, SEEK_SET );
       Nread =  fread ( spec , sizeof(int), 8192, handle);
       fclose(handle);

#if defined( __GW_LITTLE_ENDIAN )
       for( ii = 0; ii < Nread; ii++, spec++) 
          swap_bytes( (char *)spec, sizeof(int));      
#endif
       return Nread;
       }
  else {
       fclose(handle);
       return -1;
       }
  }
