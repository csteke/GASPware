#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>

int32_t FIRSTFILE;
unsigned short int VALID_CHANNEL[16]={1,2,4,8,16,32,64,128,256,512,1024,
                                      2048,4096,8192,16384,32768};


int32_t tnd_getnf_(char *path);
int32_t tnd_read_(char *path, int32_t *current, char *buff);
int32_t tnd_decode_(unsigned short int *buff, int32_t *nwords,  int32_t *npar);


int32_t tnd_getnf_(char *path){

 struct stat filestatus;
 int32_t i,count,nfiles,isfirst=0;
 char *stri;


   stri=(char *)calloc(128,sizeof(char));
   i=0;
   while(*(path+i) != '\0' && *(path+i) != ' ' && *(path+i) != '\n'){
     *(stri+i)=*(path+i);
     i++;
   }
   *(stri+i)='.';
   
   count=0;
   nfiles=0;
   while(++count<1000){
     sprintf((stri+i+1),"%3.3d",count);
     *(stri+i+4)='\0';
     if(stat(stri,&filestatus)==0){
       nfiles++;
       if(isfirst == 0){
          FIRSTFILE=count-1;
	  isfirst=1; }
       }
   }
   printf(" %4.1d files were found \n",nfiles);
   free(stri);
   return nfiles;
}




int32_t tnd_read_(char *path,int32_t *current, char *buff){

 struct stat filestatus;
 int32_t i,nbytes,maxbytes=524288;
 char *stri,aa;
 FILE *file;
 
    
   stri=(char *)calloc(128,sizeof(char));
   i=0;
   while(*(path+i) != '\0' && *(path+i) != ' ' && *(path+i) != '\n'){
     *(stri+i)=*(path+i);
     i++;
   }
   *(stri+i)='.';
   
   
   nbytes=0;
   sprintf((stri+i+1),"%3.3d",(*current+FIRSTFILE));
   *(stri+i+4)='\0';
   if(stat(stri,&filestatus)==0){
     file=(FILE *)calloc(1,sizeof(FILE));
     file=fopen(stri,"rb");
     nbytes=fread(buff,1,maxbytes,file);
     fclose(file);
/*     free(file);*/
   }
/*   printf(" %s     %d  \n",stri,nbytes);*/
    free(stri);
  return nbytes;
}

    
   
int32_t tnd_decode_(unsigned short int *buff, int32_t *nwords,  int32_t *npar){

   
 int32_t maxwords=262145, outwords ;
 int i ;
 unsigned short int *out, *outnext;
 unsigned short int *next , *endbuff , *head ;
 
 out=(unsigned short int *)calloc(maxwords,sizeof(unsigned short int));
 if( out == NULL ) return -1;
 outnext=out;
 
 head=buff;
 endbuff=buff+((*nwords)-1);
 outwords=0;
 
 while(head < endbuff){
/*   printf(" %d\n",*head); */
   next=head+1;
   for( i=0 ; i<(*npar) ; i++ ){
       if((*head)&VALID_CHANNEL[i]){
          *outnext=*next;
/*	  printf(" ADC%d=%d",i,*next);*/
	  next++;
	  }
       else{
          *outnext=0;
	  }
       outnext++;
       outwords++;
       }
   head=next;
/*   printf("\n");*/
   fflush(stdout);
   }
 memcpy(buff,out,2*outwords);
 free(out);
 return outwords;
} 
       
  
