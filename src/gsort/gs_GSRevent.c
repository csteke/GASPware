#include <stdio.h>


/***** GSORT interface function and required declarations *****/

#define MAXDET 	256
#define MAXFOLD 30
#define I2 short int

/*static short int det_id[MAXDET],det_energy[MAXDET],det_time[MAXDET]; */


typedef struct {
	short int Id;
	short int Energy;
	short int Time;
	short int dummy;
  }GSR_Detector;

typedef struct {
	short int Coax_fold;
	short int Spare[3];
	GSR_Detector   Coax[MAXDET];
   } GSR_Event;



/*int        format_event(int evlen, I2 *ibuf, I2 *end);*/
unsigned short int *get_gsrevent_( unsigned short int *, unsigned short int *, GSR_Event *, int *);


/****** End GSORT declarations *********************************/

/******  GSORT data decoding interface *************************/



/*-----------------------------------inp-------------------inp------out---out*/
 

unsigned short int *get_gsrevent_( unsigned short int *buf_data, unsigned short int *buf_end, GSR_Event *ev, int *EvStatus){

   int i,ii;
   int fold,det_no,e,time;

   

 	        /* get event fold ...*/
		
NEW_TRY:        fold = (*buf_data & (unsigned short) 0xfe00)/512;

		/*FOLD o.k. ? ...                                      */
		if (fold <1 ||fold >30 ) {
		  while ( *buf_data != 0xffff){
		    buf_data++;
		    if (buf_data >=buf_end) {
		      *EvStatus = 0;
		      return buf_end;
		    } 
		  }
		  buf_data++;
		  goto NEW_TRY;
		}

		buf_data++;
		ii=0;

	/*!!	ev_buff.RUN.ev_in[fold]++;		*/

		/* GET DET NUMBER, ENERGY & TIME ..		       	*/
		for(i=0;i<fold;i++) {
		  det_no = (*buf_data & 0xfe00)/512;
		  time   =  *buf_data & 0x01ff + (*(buf_data+1) & 0xc000)/32;
		  buf_data++;
		  e      = (*buf_data++ & 0x3fff);

		  /* PARAMETER RANGE o.k. ? ...                         */
		  if ( det_no <0 || det_no>111 ) {
	/*!!	    ev_buff.RUN.r_det_no++;		*/
		    continue;
		  }

		  if ( time <0 || time>2047 ) {
	/*!!	    ev_buff.RUN.r_time++;		*/
		    continue;
		  }

		  if ( e <0 || e> 16383 ) {
	/*!!	    ev_buff.RUN.r_det_off++;		*/
		    continue;
		  }

		  ii++;

 		  /*INCREMENT UNCAL SPECTRA*/
           	  ev->Coax[ii].Id     = det_no;
		  ev->Coax[ii].Energy = e;
		  ev->Coax[ii].Time   = time;


	        }
		/* SET THRU FOLD ..				*/
		ev->Coax_fold       = ii;


		/*EVENT END o.k. ? ...                                  */
		if(*buf_data++ != 0xffff) {
	/*!!	  ev_buff.RUN.bad_event++;		*/

		  while ( *buf_data==0xffff){
		    buf_data++;
		    if (buf_data >=buf_end) {
		      *EvStatus = 0;
		      return buf_end;
		    } 
		  }
		  buf_data++;
		  goto NEW_TRY;
		}
		else  *EvStatus = 1;    
      
    return buf_data;
}


/*=========================================================================*/

