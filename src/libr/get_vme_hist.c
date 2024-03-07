#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>

#define SERVVME "192.84.148.55"

int get_vme_hist_(histogram,address,length)
    unsigned char *histogram;
    int address,length;
{
  int ii;
  int *pippo;
  int sd,rerr,writerr,sumbytes,lunmessage;
  struct sockaddr_in serv;
  struct sockaddr *pserv;
  char message[34];
  char * function;

  sd = socket(AF_INET,SOCK_STREAM,0);
  if (sd < 0) 
    {
      perror("errore nella creazione del socket");
      return(2); 
    }

  serv.sin_family = AF_INET;
  serv.sin_port = htons(3333);
  serv.sin_addr.s_addr = inet_addr(SERVVME);

  pserv=(struct sockaddr *)&serv;

  if (connect(sd,pserv,sizeof(serv))<0)
    {
      close(sd);
      perror("errore nella connect");
      return(2); 
    }
       

  function="memorydump";
  sprintf(message,"%sX%uX%uZ",function,address,length);
  lunmessage=sizeof(message);

  writerr = write (sd,message,lunmessage);
  if (writerr != lunmessage)
    {
      if (writerr < 0)
        perror("errore nella write");
      else
        printf("messaggio inviato errato");
      close(sd);
      return(2);
    }

  sumbytes=0;
  do
    {
      rerr = read (sd,histogram+sumbytes,length);
      if (rerr <= 0) 
        { 
	  close(sd);
          perror("errore nella read");
          return(2);
        }
      sumbytes=sumbytes+rerr;
    } while (sumbytes != length);
  close(sd);
  return(0);
}
