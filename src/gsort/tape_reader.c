#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/shm.h>
#include <sys/mtio.h>

#define SWAB

#define READ_TAPE_B0 1
#define READ_TAPE_B1 2
#define REWIND -1
#define UNLOAD -2
#define SPACE_FILES -3
#define SPACE_RECORDS -4

#define E_EOF 0
#define E_EOI -1
#define E_EOT -2
#define E_PARITY -3
#define E_PIPE -4
#define E_MANUAL -5
#define E_MTOP -6

main (int argc,char **argv)
/*char	*argv[];
int	argc; */

{
  int		shm_id;
  int		command_fd;
  int		data_fd;
  int		tape_fd;
  char		*tape;
  int		max_buffl;
  short		*Buffer[2];
  long		status;
  long		command[2];
  int		len;
  int		eof_buf;
  struct mtget	mtget;
  struct mtop	mtop;
  
  tape=argv[1];
  sscanf(argv[2],"%i",&command_fd);
  sscanf(argv[3],"%i",&data_fd);
  sscanf(argv[4],"%i",&max_buffl);
  sscanf(argv[5],"%i",&shm_id);
  printf("son shm_id%d\n", shm_id);
  Buffer[0]=(short *)shmat(shm_id,NULL,0);
  Buffer[1]=Buffer[0]+max_buffl/2;
  printf("son buffers %d %d \n",Buffer[0],Buffer[1]);
  tape_fd=open(tape, O_RDONLY);
  while ( read(command_fd,command,8) == 8 )
    switch ( command[0] )
      {
        case READ_TAPE_B0:
          {
            if ( (status=read(tape_fd,Buffer[0],command[1])) <= 0 )
              {
                if ( status == 0 )
                  {
                    status=E_EOF;
                    write(data_fd,&status,4);
                  }
                else
                  {
                    status=E_PARITY;
                    write(data_fd,&status,4);
                  }
              }
            else
              {
                status=command[1];
#ifdef SWAB
                swab(Buffer[0],Buffer[0],status/2);
#endif
                write(data_fd,&status,4);
              }
            break;
          }
        case READ_TAPE_B1:
          {
            if ( (status=read(tape_fd,Buffer[1],command[1])) <= 0 )
              {
                if ( status == 0 )
                  {
                    status=E_EOF;
                    write(data_fd,&status,4);
                  }
                else
                  {
                    status=E_PARITY;
                    write(data_fd,&status,4);
                  }
              }
            else
              {
                status=command[1];
#ifdef SWAB
                swab(Buffer[1],Buffer[1],status/2);
#endif
                write(data_fd,&status,4);
              }
            break;
          }
        case REWIND:
          {
            mtop.mt_op = MTREW;
            mtop.mt_count = 1;
            status=ioctl(tape_fd, MTIOCTOP, &mtop);
            if ( status )
              status=E_MTOP;
            write(data_fd,&status,4);
            break;
          }
        case UNLOAD:
          {
            mtop.mt_op = MTOFFL;
            mtop.mt_count = 1;
            status=ioctl(tape_fd, MTIOCTOP, &mtop);
            if ( status )
              status=E_MTOP;
            write(data_fd,&status,4);
            break;
          }
        case SPACE_FILES:	/* >0 avanti <0 indietro =0 errore */
          {
            if ( command[1] )
              if ( command[1] > 0 )
                {
                  mtop.mt_op = MTFSF;
                  mtop.mt_count = command[1];
                  status=ioctl(tape_fd, MTIOCTOP, &mtop);
                  if ( status )
                    {
                      status=E_MTOP;
                      write(data_fd,&status,4);
                    }
                  else
                    write(data_fd,&status,4);
                }
              else
                {
                  mtop.mt_op = MTBSF;
                  mtop.mt_count=1-command[1];	/* indietro e' <0 */
                  status=ioctl(tape_fd, MTIOCTOP, &mtop);
                  if ( status )
                    {
                      status=E_MTOP;
                      write(data_fd,&status,4);
                    }
                  else
                    {
                      if ( status=read(tape_fd,eof_buf,sizeof(int)) == 0)                 
                        write(data_fd,&status,4); /* ha skippato EOF avanti */
                      else
                        {
                          mtop.mt_op = MTREW;  /* si trova al BOT e si fa REW */
                          mtop.mt_count=1;
                          status=ioctl(tape_fd, MTIOCTOP, &mtop);
                          if ( status )
                            {
                              status=E_MTOP;
                              write(data_fd,&status,4);
                            }
                          else
                            write(data_fd,&status,4);
                        }
                    }
                }
            else
              {
                status=E_MTOP;
                write(data_fd,&status,4);
              }
            break;
          }
        case SPACE_RECORDS:	/* >0 avanti <0 indietro =0 errore */
          {
            if ( command[1] )
              if ( command[1] > 0 )
                {
                  mtop.mt_op = MTFSR;
                  mtop.mt_count=command[1];
                  status=ioctl(tape_fd, MTIOCTOP, &mtop);
                  if ( status )
                    {
                      status=E_MTOP;
                      write(data_fd,&status,4);
                    }
                  else
                    write(data_fd,&status,4);
                }
              else
                {
                  mtop.mt_op = MTBSR;
                  mtop.mt_count=-command[1];
                  status=ioctl(tape_fd, MTIOCTOP, &mtop);
                  if ( status )
                    {
                      status=E_MTOP;
                      write(data_fd,&status,4);
                    }
                  else
                    write(data_fd,&status,4);
                }
            else
              {
                status=E_MTOP;
                write(data_fd,&status,4);
              }
            break;
          }
        }
  close(tape_fd);  
  close(command_fd);
  close(data_fd);  
}
