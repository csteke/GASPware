/*
#ifndef SORT_H
#define SORT_H
*/

#include <netinet/in.h>
#include <sys/time.h>
#include <stdio.h>

#define CHANNEL_MIN  16
#define CHANNEL_MAX  4000
#define condition(x)  ( x >= CHANNEL_MIN && x < CHANNEL_MAX )

struct RawEvent {

	struct timeval time_stamp;
	short ic[4][10];
	short E[5];
	short T[5];
	short tof[10];

	short Xl[10];
	short Xr[10];
	short X[10];

	short Ya;
	short Yb;
	short Y;
};

extern void alf_init_( int * );
extern void alf_close_();

extern void alf_write_event_(struct RawEvent *event );

/*
#endif 

#include <netinet/in.h>
#include <sys/time.h>
#include <stdio.h>

#include "sort.h"  
*/

static FILE *file = NULL;

/*
int condition(int x )
{
	const int _min = 16;
	const int _max = 4000;
	
	return x >= _min && x < _max;
}
*/


void alf_init_( int *run )
{
  char FileName[128];

	if( *run < 1 )
	{
	    if( file ) fclose( file );
	    file = NULL;
	    return;
	}
	
	sprintf( FileName, "PRISMA-DATA.RUN%4.4d\0", *run);

	if( file ) fclose( file );
	file = NULL;

	file = fopen(FileName, "w");
	
	if( file ) printf( " OUTPUT file %s created\n", FileName );
	else printf( " ERROR opening file %s \n", FileName );
	fflush( stdout );
}

void alf_close_()
{
	if( file ) fclose(file);
	file = NULL;
}

void alf_write_event_(struct RawEvent *event )
{
	int header 	= 0xF00DF00D;
	int version 	= 1;
	int revision	= 0;
	
	unsigned short count = 0;
	
	short i;
	
	if( !file ) return;
	
	fwrite(&header, 	 sizeof(int), 1, file);
	fwrite(&version,	 sizeof(int), 1, file);
	fwrite(&revision,	 sizeof(int), 1, file);
	
	fwrite(&event->time_stamp,	sizeof(struct timeval), 1, file);
	
	fwrite(&event->Ya,		sizeof(short), 1, file);
	fwrite(&event->Yb,		sizeof(short), 1, file);
	
	for (i = 0; i < 10; i++)	if (condition(event->Xl[i]))	count++;
		
	fwrite(&count, sizeof(unsigned short), 1, file);
		
	for (i = 0; i < 10; i++)	if (condition(event->Xl[i]))
	{
		fwrite(&i, sizeof(short), 1, file);
		fwrite(&event->Xl[i], sizeof(short), 1, file);
	} 
			
	/*   Xright  */
		
	count = 0;
		
	for (i = 0; i < 10; i++)	if (condition(event->Xr[i]))	count++;
		
	fwrite(&count, sizeof(unsigned short), 1, file);
		
	for (i = 0; i < 10; i++)
		if (condition(event->Xr[i]))
		{	
			fwrite(&i, sizeof(short), 1, file);
			fwrite(&event->Xr[i], sizeof(short), 1, file); 
		}
	
	/*   time of flight  */
		
	count = 0;
		
	for (i = 0; i < 10; i++)	if (condition(event->tof[i]))	count++;
		
	fwrite(&count, sizeof(unsigned short), 1, file);
		
	for (i = 0; i < 10; i++)
		if (condition(event->tof[i]))
		{
			fwrite(&i, sizeof(short), 1, file);
			fwrite(&event->tof[i], sizeof(short), 1, file);
		}
		
	/* ionization chambers  */
		
	count = 0;

	for (i = 0; i < 4; i++)
	{
		short j;

		for (j = 0 ; j < 10; j++)
		{
			if (condition(event->ic[i][j]))	count++;
		}
	}
		
	fwrite(&count, sizeof(unsigned short), 1, file);
		
	for (i = 0; i < 4; i++)
	{
		short j;

		for (j = 0 ; j < 10; j++)
		{
			if (condition(event->ic[i][j])) 
			{
				fwrite(&i, sizeof(short), 1, file);
				fwrite(&j, sizeof(short), 1, file);
				fwrite(&event->ic[i][j],  sizeof(short), 1, file);
			}
		}
	}

	/* foreign signals (E) */
		
	count = 0;
		
	for (i = 0; i < 5; i++)	if (condition(event->E[i]))	count++;
		
	fwrite(&count, sizeof(unsigned short), 1, file);
		
	for (i = 0; i < 5; i++)
		if (condition(event->E[i]))
		{
			fwrite(&i, sizeof(short), 1, file);
			fwrite(&event->E[i], sizeof(short), 1, file);
		}
		
	/* foreign signals (T)  */
		
	count = 0;
		
	for (i = 0; i < 5; i++)	if (condition(event->T[i]))	count++;
		
	fwrite(&count, sizeof(unsigned short), 1, file);
		
	for (i = 0; i < 5; i++)
		if (condition(event->T[i]))
		{
			fwrite(&i, sizeof(short), 1, file);
			fwrite(&event->T[i], sizeof(short), 1, file);
		}
}


 
