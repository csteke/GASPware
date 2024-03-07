#include <stdio.h>
#include <stdlib.h>

typedef struct InfoDB {
	int index;
	int Ge_no;
	int dd;
	int mm;
	int yy;
	float Res;
	struct InfoDB *Next;
	} InfoDB;
	

static struct InfoDB *BaseInfoDB = NULL;

int readinfo_( void );
int getinfo_( int *Pos, int *GeNo, char *LabData);

int readinfo_( void ){

   struct InfoDB *p;
   FILE *DBFile;
   int ii, nn, zz,ll,aa;
   float res;
   
   DBFile = fopen("Ge_Info.DB","rt");
   if( DBFile == NULL ){
     printf("  Warning : file Ge_Info.DB does not exist\n");
     return 0;
     }
     
   
   while( fscanf(DBFile,"%d %d %d/%d/%d %f",&ii, &nn, &zz, &ll, &aa, &res) == 6){
      if( BaseInfoDB ){
         p->Next = (struct InfoDB *) calloc(1,sizeof(struct InfoDB));
	 p = p->Next;
	 }
      else {
         BaseInfoDB = (struct InfoDB *) calloc(1,sizeof(struct InfoDB));
	 p = BaseInfoDB;
	 }
      p->Next = NULL;
      p->index = ii;
      p->Ge_no = nn;
      p->dd = zz;
      p->mm = ll;
      p->yy = aa;
      p->Res = res;
      }
   fclose(DBFile);
   return 1;

}

int getinfo_( int *Pos, int *GeNo, char *LabData){

  struct InfoDB *p;
  
   if( BaseInfoDB == NULL )return 0;
  
   p = BaseInfoDB;
   while( p ){
      if( p->index == *Pos) {
        *GeNo = p->Ge_no;
	sprintf(LabData,"%2.2d/%2.2d/%2.2d %5.2f\0",
		 p->dd,
		 p->mm,
		 p->yy,
		 p->Res);
        return 1;
	}
      p = p->Next;
      }
   return 0;
}

/*   Main for debugging
int main(void){
  
  int ii,nn;
  char aa[32];
  
  readinfo_();

  for( ii = 1; ii < 45; ii++){
     if(getinfo_(&ii,&nn,&(aa[0])))
     printf(" %3d %3d %s\n",ii,nn,aa);
     }

  }
*/  
