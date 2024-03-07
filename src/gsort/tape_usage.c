#include <unistd.h>
#include <stdio.h> 
#include <fcntl.h>
#include <time.h>
#include <sys/types.h>
#include <sys/shm.h>

#define READ_TAPE 1
#define REWIND -1
#define UNLOAD -2
#define SPACE_FILES -3
#define SPACE_RECORDS -4
#define GET_STATUS -5

#define E_EOF 0
#define E_EOI -1
#define E_EOT -2
#define E_PARITY -3
#define E_PIPE -4
#define E_MANUAL -5	/* intervento manuale durante lettura */
#define E_MTOP -6	/* errore durante controllo del nastro */
#define E_CALL -7	/* controllo del nastro con nastro in lettura */
#define E_FORK -8	
#define E_UNIT -9	

static pid_t	child_pid=0;
static int	tape_command;
static int	tape_data;
static int	shm_id;
static short	*buff[2];
static long	record_length;
static int	tape_fd=0;
static int	tape_is_reading;
static int	ping_pong_index;

int Init_tape(tape_dev,max_buffl)
char	*tape_dev;
int	max_buffl;
{
  int	pipe_out[2];		/* [0] lettura  [1] scrittura */
  int	pipe_in[2];
  pid_t	child_pid;
  char	fd_command[32];
  char	fd_data[32];
  char	buffl[32];
  char	shm[32];
  
  if ( (tape_fd=open(tape_dev, O_RDONLY)) != -1 )
    close(tape_fd);
  else
    return (E_UNIT);
  pipe(pipe_out);
  pipe(pipe_in);
  shm_id=shmget(IPC_PRIVATE,2*max_buffl,IPC_CREAT | 0600);
  buff[0]=(short *)shmat(shm_id,NULL,0);
  buff[1]=buff[0]+max_buffl/2;
  switch ( child_pid=fork() )
    {
      case -1:			/* error */
        return (E_FORK);
      case 0:			/* questo e' il figlio */
        {
          close(pipe_out[1]);	/* resta aperto in lettura per comandi */
          close(pipe_in[0]);	/* resta aperto in scrittura per sinc e recl */
          sprintf(fd_command,"%i",pipe_out[0]);
          sprintf(fd_data,"%i",pipe_in[1]);
          sprintf(buffl,"%i",max_buffl);
          sprintf(shm,"%i",shm_id);          
          execlp ("tape_reader","tape_reader",
                  tape_dev,fd_command,fd_data,buffl,shm,(char *)0);
        }
    }				/* questo e' il padre */
  close(pipe_out[0]);		/* resta aperto in scrittura per comandi */
  close(pipe_in[1]);		/* resta aperto in lettura per sinc e recl */
  tape_command=pipe_out[1];	/* per comodita' */
  tape_data=pipe_in[0];		/* per comodita' */
  tape_is_reading=0;		/* nastro considerato fermo */
  return (0);
}

int Space_files(nfiles)
int	nfiles;			/* >0 avanti <0 indietro =0 errore */

{
  int	status;
  int	command[2];
  
  if ( tape_is_reading )
    return (E_CALL);
  else
    {
      command[0]=SPACE_FILES;
      command[1]=nfiles;  
      if ( write(tape_command,command,8) == 8 )
        if ( read(tape_data,&status,4) != 4 )
          return (E_PIPE);
        else
          return (status);
      else
        return (E_PIPE);
    }  
}

int Space_records(nrecords)
int	nrecords;		/* >0 avanti <0 indietro =0 errore */

{
  int	status;
  int	command[2];
  
  if ( tape_is_reading )
    return (E_CALL);
  else
    {
      command[0]=SPACE_RECORDS;
      command[1]=nrecords;  
      if ( write(tape_command,command,8) == 8 )
        if ( read(tape_data,&status,4) != 4 )
          return (E_PIPE);
        else
          return (status);
      else
        return (E_PIPE);  
    }  
}

int Rewind()

{
  int	status;
  int	command[2];
  
  if ( tape_is_reading )
    return (E_CALL);
  else
    {
      command[0]=REWIND;
      command[1]=1;  
      if ( write(tape_command,command,8) == 8 )
        if ( read(tape_data,&status,4) != 4 )
          return (E_PIPE);
        else
          return (status);
      else
        return (E_PIPE);  
    }  
}

int Unload()

{
  int	status;
  int	command[2];
  
  if ( tape_is_reading )
    return (E_CALL);
  else
    {
      command[0]=UNLOAD;
      command[1]=1;  
      if ( write(tape_command,command,8) == 8 )
        if ( read(tape_data,&status,4) != 4 )
          return (E_PIPE);
        else
          return (status);
      else
        return (E_PIPE);
    }  
}

void Set_record_length(nbytes)
int	nbytes;

{
  record_length=nbytes;
}

void *Get_record(nbytes,read_more,ierror)
int	*nbytes;
int	read_more;
int	*ierror;

{
  long	command[2];
  int	length;
  int	index;
  int nm_ii;
  
  if ( tape_is_reading )
    if ( read(tape_data,&length,4) != 4 )
      {
        *nbytes=0;
        *ierror=E_PIPE;
        return (NULL);
      }
    else
      if ( length > 0 )
        {
          index=ping_pong_index;
          if ( read_more )
            {
              nm_ii=++ping_pong_index%2;
	      ping_pong_index=nm_ii;
              command[0]=READ_TAPE+ping_pong_index;
              command[1]=record_length;
              if ( write(tape_command,command,8) != 8 )
                {
                  *nbytes=0;
                  *ierror=E_PIPE;
                  return (NULL);
                }
            }
          else
            tape_is_reading=0;
          *nbytes=length;
          *ierror=0;
          return ((void *)buff[index]);
        }
      else
        {
          *nbytes=0;
          *ierror=length;
          return (NULL);
        }       
  else
    {
      ping_pong_index=0;
      command[0]=READ_TAPE+ping_pong_index;
      command[1]=record_length;
      if ( write(tape_command,command,8) != 8 )
        {
          *nbytes=0;
          *ierror=E_PIPE;
          return (NULL);
         }
      if ( read(tape_data,&length,4) != 4 )
        {
          *nbytes=0;
          *ierror=E_PIPE;
          return (NULL);
        }
      index=0;
      ping_pong_index=1;
      command[0]=READ_TAPE+ping_pong_index;
      command[1]=record_length;
      if ( write(tape_command,command,8) != 8 )
        {
          *nbytes=0;
          *ierror=E_PIPE;
          return (NULL);
        }
      tape_is_reading=1;
      *nbytes=length;
      *ierror=0;
      return ((void *)buff[index]);
    }
}

int Get_status()

{
  int	status;
  int	command[2];
  
  command[0]=GET_STATUS;
  command[1]=1;  
  if ( write(tape_command,command,8) == 8 )
    if ( read(tape_data,&status,4) != 4 )
      return (E_PIPE);
    else
      return (status);
  else
    return (E_PIPE);  
}


main()

{
  char	*tape="/dev/rmt/2mbn";
  int	max_buffl=32*1024;
  int	time0;
  int	time1;
  int	timetot;
  int	i;
  int	len;
  int	totlen;
  double	speed;
  int	read_more;
  short	*buff;
  int	ierror;
  
  if ( i=Init_tape(tape,max_buffl) )
    switch (i)
      {
        case E_FORK:
          printf("FORK ERROR !!!!\n");
          exit(1);
        case E_UNIT:
          printf("CAN'T OPEN UNIT !!!!\n");
          exit(2);
      }
  Set_record_length(max_buffl);
  read_more=1;
  totlen=0;
  time0=time(NULL);
  for (i=0; i<500; i++)
    {
      read_more= i < 500 ? 1 : 0 ;
      buff=Get_record(&len,read_more,&ierror);
      totlen+=len;
    }
  time1=time(NULL);
  timetot=time1-time0;
  speed=(double)totlen/(double)timetot;
  printf("risultati %d %d %f\n",timetot,totlen,speed);
  Rewind();
}
