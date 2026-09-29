#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#if !defined(__APPLE__)
#include <malloc.h>
#endif

#include <math.h>
#include <string.h>
#include <sys/time.h>

#include <X11/Xlib.h>
#include <X11/cursorfont.h>

#include <X11/Ygl.h>
/*
#define GLW_MINXSIZE 774
#define GLW_MINYSIZE 574
#define GLW_FRAMEWIDTH 84  */

Int32 GLW_MINXSIZE = 800;
Int32 GLW_MINYSIZE = 560;
Int32 GLW_FRAMEWIDTH = 80;
float RATIO_W = 5.0 / 8.0;

/* #define GLW_FRAMEWIDTH 80 */
#define GLW_FRAMECOLOR 8
#define GLW_FRAMECOLOR_DARK 9
#define GLW_FRAMECOLOR_LIGHT 10
#define GLW_LABELCOLOR_1 11
#define GLW_LABELCOLOR_2 12
#define GLW_DRAWBG BLACK

#define GLW_FONTID14 4711
#define GLW_FONTID12 4712


typedef struct LabelInFrame {
	float x1,y1,x2,y2;
	int bgcolor;
	int fgcolor;
	char *Text;
	Coord TextX,TextY;
	}LabelInFrame;

typedef struct ButtonInFrame {
	float x1,y1,x2,y2;
	int bgcolor;
	int fgcolor;
	int pushcolor;
	int Push;
	char *Text;
	Coord TextX,TextY;
	}ButtonInFrame;


typedef struct DisplayWindow {
        Int32 x1,y1,x2,y2;
	Int32 ID;
	struct DisplayWindow *Next;
	}DisplayWindow;


typedef struct PeakLabel {
        float Channel;
	struct PeakLabel *Next;
	}PeakLabel;

typedef struct NumPeakLabel {
        float Channel;
	int   N;
	Int32  LabelColor;
	struct NumPeakLabel *Next;
	}NumPeakLabel;


typedef struct TrackPlot {
        float *data[7];
        float *err[7];
	float *logdata[7];
	float *sqrtdata[7];
	Int32 Np[7];
	Int32 LastData;
	Int32 ActiveData;
	Int32 ClearBeforeDraw;
	Int32 LogScale;
	Int32 Color[7];
	Int32 Draw[7];
	float x1,y1,x2,y2;
	float Xmin[7],Xmax[7];
	Int32 Imin[7],Imax[7];
	float Ymin,Ymax;
	char *Comment;
	char *LastFile;
	int FileFormat;
	int FileSpecLength;
	Int32 Row;
	Int32 Col;
	struct PeakLabel *Peak;
	struct NumPeakLabel *NumPeak;
	struct TrackPlot *Next;
	}TrackPlot;
	
typedef struct DataValues {
	Int32 Channel;
	float Energy;
	float Counts;
	float Y;
	struct TrackPlot *Plot;
	}DataValues;
	
typedef struct TrackData {
        float *spek;
	float *err;
	Int32 *ika;
	Int32 *ikl;
	float *ecal;
	float *peaks;
	Int32 *npeaks;
	Int32 *nmin;
	Int32 *nmax;
	float *ymin;
	float *ymax;
	Int32 *ifunct;
	} TrackData;
	

typedef struct IOTerminal {
	Display *disp;
	Window win;
	int revert;
	}IOTerminal;
	
static struct IOTerminal ioTerm, ioFocus;
static struct TrackData Track;
struct DisplayWindow *GLWindow;
static struct LabelInFrame TrackXMinValue, TrackChannelValue,
                           TrackXMaxValue, TrackEnergyValue,
			   TrackYMinValue, TrackCountsValue,
			   TrackYMaxValue, TrackCursorYValue,
			   TrackPath, TrackDisplayed,TrackOutFile;

static struct ButtonInFrame TrackNewSpec,TrackWriteSpec, TrackIncSpec,TrackDecSpec,
                            TrackOpenCM, TrackGateCM,
                            TrackLeft, TrackRight,
                            TrackSameX, TrackSameY,
                            TrackAutoX, TrackAutoY,
                            TrackAutoXY, TrackLinLog, TrackRefresh,
                            TrackDoPkS, TrackDoFit, TrackDoInt,
			    TrackCal2P, TrackAutoTrace, TrackEnCal;

struct TrackPlot *Plot, *CrtPlot, *OldPlot;
static Int32 NofPlots;
static Int32 TrackMenu;

static Int32 _global_LastW = -1, _global_LastH = -1;
static double _global_px = 1.0, _global_py = 1.0;
static int _in_redraw_window = 0;
static unsigned long _global_FramePixel = 0;

/* Functions: ---> some of them TRACKN specific */
void ClosestColor ( Int16 *r, Int16 *g, Int16 *b);
void MapGLWcolors(void);
void LoadGLWfont(void);
void MapDisplayWindow(struct DisplayWindow *dw);
void SetMouseShape(unsigned int shape);
int MouseInDisplayWindow(struct DisplayWindow *dw);
void DrawLabel(LabelInFrame L);
void DrawButton(ButtonInFrame L);
void WriteInLabel(LabelInFrame L);
void DrawFrame(void);
void DrawTrackFrame(Int32 win);
Int32 GLWTrackInit(void);
struct TrackPlot* TrackPlotMap(Int32 Row, Int32 Col);
void KillPlots(struct TrackPlot*);
void KillAllPeakLabels( struct TrackPlot*);
void KillPeakLabels( struct TrackPlot*);
void AddPeakLabel ( struct TrackPlot*, float );
void AddNumPeakLabel ( struct TrackPlot* , float, int, Int32);
void DrawPlotFrame(struct TrackPlot *);
void DrawActivePlotFrame(struct TrackPlot *Pl);
void SetPlotData(Int32 Index, Int32 *Np,
                 Int32 *Imin, Int32 *Imax,
                 float *Xmin, float *Xmax,
		 float *Ymin, float *Ymax,
		 float *data, float *err,
		 struct TrackPlot *);
void DrawPlot(struct TrackPlot *Pl);
void DrawSubPlot(struct TrackPlot *Pl, Int32 Index);
struct TrackPlot *MouseInWhichPlot(struct DisplayWindow *dw);
int MouseInLabel( struct LabelInFrame l);
int MouseInButton( struct ButtonInFrame l);
struct DataValues *Values(struct DisplayWindow *dw);
void CleanPlot(struct TrackPlot *);
void DrawPeakLabel(struct TrackPlot *Pl, float *Channel);
void DrawNumberedPeakLabel(struct TrackPlot *Pl, float *Channel, Int32 N, Int32 LabelColor);
void DrawMarker(struct TrackPlot *, float *Channel, Int32 MarkerColor);
void DrawDoubleMarker(struct TrackPlot *, float *Channel1, float *Channel2, Int32 MarkerColor);
void CopyComment( struct TrackPlot *from_p, struct TrackPlot *to_p );
void TermFocus(void);
void GraphFocus(void);
void PutTrackData(struct TrackPlot *);
float Nintf(float );
void ReshapeWindow ( void );
void RedrawWindow ( int force_reshape );

/****************** XTP - FORTRAN callable functions for TRACK *******************/
void xtpinit_(void);
void xtptermfocus_(void);
void xtpmap_(Int32 *Row, Int32 *Col);
void xtpmaptmp_(Int32 *Row, Int32 *Col);
void xtpsetv_(float *spek, float *err,
              Int32 *ika,  Int32 *ikl,
              float *ecal, float *peaks, Int32 *npeaks,
	      Int32 *nmin, Int32 *nmax,
	      float *ymin, float *ymax, Int32 *ifunct);
void xtplotnew_(void);
void xtplotreset_(void);
void xtplotadd_(void);
void xtpoverlay_(float func_(float *), float *Xmin, float *Xmax, float *step);
void xtpnext_(void);
void xtpshowpeak_(float *);
void xtpshowpeaks_(void);
void xtpshownumpeak_(float *Channel, Int32 *Index, Int32 *MarkerColor); 
void xtpmarker_(float *Channel, Int32 *MarkerColor);
void xtpdoublemarker_(float *Channel1, float *Channel2, Int32 *MarkerColor);
void xtpget_(float *x, float *y, Int32 *c);
void xtpcomment_(char *,Int32 *, Int32 *, Int32 *);
void xtpgetcomment_(char *,Int32 *, Int32 *, Int32 *);
void xtpsamex_(void);
void xtpsamey_(void);
void xtpbell_(void);
void xtpoutfile_(char *Name, Int32 *length);
extern void put_comment_(void); 

/************************************* input library (inter_isl.c) ********/

#define RAW 1
#define RESTORE 0

extern int ISLSetTerminal(int mode);
extern int ISLGetInput ( void );
extern int ISLPutInput ( unsigned char *c, int n);
extern int ISLGetString ( unsigned char *c, int *n);


/************************************************************************/
void xtpinit_(void){

  if(GLWindow)return;
  TermFocus();
  GLWindow=(struct DisplayWindow *)calloc(1,sizeof(struct DisplayWindow));
  if(GLWindow == NULL){
     printf(" TRACK ERROR - cannot allocate memory for display\n");
     fflush(stdout); exit(0);
     }
  GLWindow->ID=GLWTrackInit();
  if(TrackPlotMap(1,1) == NULL){
     printf(" TRACK ERROR - cannot allocate memory for display\n");
     fflush(stdout); exit(0);
     }
  CrtPlot=Plot;
  CrtPlot->Draw[0]=0;
 } 

void xtptermfocus_(void){

   TermFocus();
}

void xtpmap_(Int32 *Row, Int32 *Col){

  Plot=TrackPlotMap(*Row, *Col);
}

void xtpmaptmp_(Int32 *Row, Int32 *Col){

  if( OldPlot == NULL )
  {
    OldPlot = Plot;
    Plot = NULL;
    NofPlots = 0;
  }
      
  Plot = TrackPlotMap(*Row, *Col);
  CrtPlot = Plot;
  qenter(REDRAW,(Int16)GLWindow->ID);
}

void xtpsetv_(float *spek, float *err,
              Int32 *ika,  Int32 *ikl,
              float *ecal, float *peaks, Int32 *npeaks,
	      Int32 *nmin, Int32 *nmax,
	      float *ymin, float *ymax, Int32 *ifunct){
	      

   Track.spek=spek; Track.err=err;
   Track.ika =ika;  Track.ikl=ikl;
   Track.ecal=ecal; 
   Track.peaks=peaks; Track.npeaks=npeaks; 
   Track.nmin=nmin; Track.nmax=nmax;
   Track.ymin=ymin; Track.ymax=ymax;
   Track.ifunct=ifunct;
   
 }

void xtplotnew_(void){

 Int32 Np;
 Int32 nmin, nmax;
 float xmin,xmax;

 if(CrtPlot == NULL)return;
 CleanPlot(CrtPlot);
 Np=*(Track.ikl)-*(Track.ika);
 CrtPlot->Np[0]=Np;
 nmin=*(Track.nmin);
 nmax=*(Track.nmax)-1;
 xmin=*(Track.nmin);
 xmax=*(Track.nmax)-1;
 SetPlotData(0,&Np, &nmin, &nmax, &xmin,&xmax,
             Track.ymin,Track.ymax, Track.spek, Track.err,CrtPlot);
 CrtPlot->ClearBeforeDraw=True;
 CrtPlot->LogScale=*(Track.ifunct)-1;
 if(CrtPlot->LogScale==1){
    CrtPlot->Ymin = (CrtPlot->Ymin > 0.00)?log10(CrtPlot->Ymin):0.00 ; 
    CrtPlot->Ymax = (CrtPlot->Ymax > 0.00)?log10(CrtPlot->Ymax):1.00 ; 
    }
  if(CrtPlot->LogScale==2){
     CrtPlot->Ymin = (CrtPlot->Ymin > 0.00)?pow(CrtPlot->Ymin,0.5000):0.00 ; 
     CrtPlot->Ymax = (CrtPlot->Ymax > 0.00)?pow(CrtPlot->Ymax,0.5000):1.00 ; 
     }
  RedrawWindow(0);
}

void xtplotreset_(void)
{

   if(CrtPlot == NULL)return;
   CleanPlot(CrtPlot);
   CrtPlot->Draw[0] = False;
   RedrawWindow(0);
}

void xtplotsame_(void){

 if(CrtPlot == NULL)return;
 CrtPlot->Xmin[CrtPlot->ActiveData]=*(Track.nmin);
 CrtPlot->Xmax[CrtPlot->ActiveData]=*(Track.nmax)-1;
 CrtPlot->Imin[CrtPlot->ActiveData]=*(Track.nmin);
 CrtPlot->Imax[CrtPlot->ActiveData]=*(Track.nmax)-1;
 CrtPlot->Ymin=*(Track.ymin);
 CrtPlot->Ymax=*(Track.ymax);
 CrtPlot->LogScale=*(Track.ifunct)-1;
 if(CrtPlot->LogScale==1){
    CrtPlot->Ymin = (CrtPlot->Ymin > 0.00)?log10(CrtPlot->Ymin):0.00 ; 
    CrtPlot->Ymax = (CrtPlot->Ymax > 0.00)?log10(CrtPlot->Ymax):1.00 ; 
    }
 if(CrtPlot->LogScale==2){
    CrtPlot->Ymin = (CrtPlot->Ymin > 0.00)?pow(CrtPlot->Ymin,0.5000):0.00 ; 
    CrtPlot->Ymax = (CrtPlot->Ymax > 0.00)?pow(CrtPlot->Ymax,0.5000):1.00 ; 
    }
 CrtPlot->ClearBeforeDraw=True;
 KillPeakLabels( CrtPlot );
 RedrawWindow(0);
}


void xtplotadd_(void){

 Int32 Np;
 Int32 Index;
 Int32 nmin, nmax;
 float ymin,ymax,xmin,xmax;

 if(CrtPlot == NULL)return;

 for(Index=0; (Index<7)&&CrtPlot->Draw[Index] ; Index++);
 if( Index==7 ){
     Index=CrtPlot->LastData+1;
     Index%=7;
     }
 Np=*(Track.ikl)-*(Track.ika);
 ymin=CrtPlot->Ymin;
 ymax=CrtPlot->Ymax;
 nmin=*(Track.nmin);
 nmax=*(Track.nmax)-1;
 xmin=*(Track.nmin);
 xmax=*(Track.nmax)-1;
 CrtPlot->Xmin[CrtPlot->ActiveData]=0;
 CrtPlot->Xmax[CrtPlot->ActiveData]=CrtPlot->Np[CrtPlot->ActiveData]-1;
 CrtPlot->Imin[CrtPlot->ActiveData]=0;
  CrtPlot->Imax[CrtPlot->ActiveData]=CrtPlot->Np[CrtPlot->ActiveData]-1;
  SetPlotData(Index,&Np, &nmin,&nmax, &xmin,&xmax,
              &ymin,&ymax, Track.spek, Track.err,CrtPlot);
  CrtPlot->ActiveData=Index;
  KillAllPeakLabels( CrtPlot );
  RedrawWindow(0);
}


void xtpoverlay_(float func_(float *), float *Xmin, float *Xmax, float *step){

 Int32 Imin,Imax,Np;
 float xmin,xmax;
 Int32 Index,i;
 float *data,x,ymin,ymax,rstep;

 if(CrtPlot == NULL)return;
  
 Np=(*Xmax-*Xmin)/(*step); Np+=1;
 if(Np <= 3)return;
 data=(float *)calloc(Np+1,sizeof(float));
 if(data == NULL)return;
 i=0;
 xmin=*Xmin; xmax=*Xmax;
 rstep=*step;
 xmax=xmin+rstep*Np;
 for(x=xmin; (x<=xmax)&&(i<=Np); x+=rstep/2.000){ x+=rstep/2.0000; *(data+i)=func_(&x);i++;}
 xmax=xmin+rstep*(Np-1);
 for(Index=0; (Index<7)&&CrtPlot->Draw[Index] ; Index++);
 Index%=7;
 if( (OldPlot == NULL)&&(Index==CrtPlot->ActiveData) ){
    Index++; Index%=7;
    if(Index==CrtPlot->LastData){Index++; Index%=7;}
    }
 Imin=0; Imax=Np-1;
 ymin=CrtPlot->Ymin;
 ymax=CrtPlot->Ymax;
 SetPlotData(Index,&Np, &Imin,&Imax,&xmin,&xmax,
             &ymin,&ymax, data, data,CrtPlot);
 free(data);
 RedrawWindow(0);
}


void xtpnext_( void )
{

 if( Plot == NULL ) return;
 if( CrtPlot == NULL ){ CrtPlot=Plot; return;}
  
 CrtPlot=CrtPlot->Next;
  
 if( CrtPlot == NULL )CrtPlot=Plot;

}


void xtpoutfile_(char *Name, Int32 *length){

  size_t L;
  
  L = *length;
  if( L <= 0 ){
     L = 16;
     TrackOutFile.Text = (char *)realloc(TrackOutFile.Text,L);
     sprintf(TrackOutFile.Text,"-> <none>");
     WriteInLabel(TrackOutFile);
     return;
     }
  TrackOutFile.Text = (char *)realloc(TrackOutFile.Text,L+4);
  sprintf(TrackOutFile.Text,"-> ");
  memcpy( (TrackOutFile.Text+3),Name,L);
  *(TrackOutFile.Text+L+3) = '\0';
  WriteInLabel(TrackOutFile);
  return;
}


void xtpcomment_(char *Comment, Int32 *length, Int32 *SpecFormat, Int32 *SpecLength){

 size_t L;

 if(CrtPlot == NULL)return;
 L=*length; 
 if(L<=0){
      CrtPlot->Comment=(char *)realloc(CrtPlot->Comment,7);
      sprintf(CrtPlot->Comment,"<none>");
      return;
      }
      
 CrtPlot->Comment=(char *)realloc(CrtPlot->Comment,L+1);
 memcpy(CrtPlot->Comment,Comment,L);
 *(CrtPlot->Comment+L)='\0';
  
 if( *SpecFormat )
 {
    CrtPlot->LastFile=(char *)realloc(CrtPlot->LastFile,L+1);
    memcpy(CrtPlot->LastFile,CrtPlot->Comment,L+1);
    CrtPlot->FileFormat = *SpecFormat;
    CrtPlot->FileSpecLength = *SpecLength;
 }
}

void xtpgetcomment_(char *Comment, Int32 *length, Int32 *SpecFormat, Int32 *SpecLength){

 size_t L;

 if(CrtPlot == NULL)return;
 *length=strlen(CrtPlot->Comment);
 L=*length;
 *SpecFormat = 0;
 if( L <= 0 )return;
 memcpy(Comment,CrtPlot->Comment,L);

 if(CrtPlot->LastFile) 
 {
    *SpecFormat = CrtPlot->FileFormat;
    *SpecLength = CrtPlot->FileSpecLength;
 }

}



void xtpsamex_(void){

  float *tmpdata;
  struct TrackPlot *p;
  
  if(CrtPlot == NULL)return;
    
  p=Plot;
  while(p){
    if(p->Np[p->ActiveData] < CrtPlot->Np[CrtPlot->ActiveData]){
       tmpdata=p->data[p->ActiveData];
       p->data[p->ActiveData]=(float *)calloc(CrtPlot->Np[CrtPlot->ActiveData],sizeof(float));
       if(tmpdata){
         memcpy(p->data[p->ActiveData],tmpdata,p->Np[p->ActiveData]*sizeof(float));
         free(tmpdata);
       }
       tmpdata=p->logdata[p->ActiveData];
       p->logdata[p->ActiveData]=(float *)calloc(CrtPlot->Np[CrtPlot->ActiveData],sizeof(float));
       if(tmpdata){
         memcpy(p->logdata[p->ActiveData],tmpdata,p->Np[p->ActiveData]*sizeof(float));
         free(tmpdata);
       }
       tmpdata=p->sqrtdata[p->ActiveData];
       p->sqrtdata[p->ActiveData]=(float *)calloc(CrtPlot->Np[CrtPlot->ActiveData],sizeof(float));
       if(tmpdata){
         memcpy(p->sqrtdata[p->ActiveData],tmpdata,p->Np[p->ActiveData]*sizeof(float));
         free(tmpdata);
       }
       tmpdata=p->err[p->ActiveData];
       p->err[p->ActiveData]=(float *)calloc(CrtPlot->Np[CrtPlot->ActiveData],sizeof(float));
       if(tmpdata){
         memcpy(p->err[p->ActiveData],tmpdata,p->Np[p->ActiveData]*sizeof(float));
         free(tmpdata);
       }
       p->Np[p->ActiveData]=CrtPlot->Np[CrtPlot->ActiveData];
       }
       p->Xmin[p->ActiveData]=CrtPlot->Xmin[CrtPlot->ActiveData];
       p->Xmax[p->ActiveData]=CrtPlot->Xmax[CrtPlot->ActiveData];
       p->Imin[p->ActiveData]=CrtPlot->Imin[CrtPlot->ActiveData];
       p->Imax[p->ActiveData]=CrtPlot->Imax[CrtPlot->ActiveData];
       p=p->Next;
    }
  RedrawWindow(0);
}

void xtpsamey_(void){

  struct TrackPlot *p;
  
  if(CrtPlot == NULL)return;
  
  p=Plot;
  while(p){
       p->Ymin=CrtPlot->Ymin;
       p->Ymax=CrtPlot->Ymax;
       p->LogScale=CrtPlot->LogScale;
       p=p->Next;
  } 
  RedrawWindow(0);
}

void xtpshowpeak_(float *Channel){

  if(CrtPlot == NULL || Channel == NULL) return;
  DrawPeakLabel(CrtPlot, Channel);
  AddPeakLabel( CrtPlot, *Channel );
}
 
void xtpshowpeaks_(void){

  Int32 i;
  
  if(CrtPlot == NULL || Track.npeaks == NULL || Track.peaks == NULL) return;
  KillPeakLabels( CrtPlot );
  for(i=0; i < *(Track.npeaks); i++){
    DrawPeakLabel(CrtPlot, (Track.peaks+i));
    AddPeakLabel( CrtPlot, *(Track.peaks+i) );
  }
}


void xtpshownumpeak_(float *Channel, Int32 *Index, Int32 *MarkerColor){

  Int32 N, Color;
  
  if(CrtPlot == NULL || Channel == NULL || Index == NULL || MarkerColor == NULL)return;
  N=*Index; Color=CrtPlot->Color[*MarkerColor];
  DrawNumberedPeakLabel(CrtPlot, Channel, N, Color);
  AddNumPeakLabel(CrtPlot, *Channel, N, Color);
}


void xtpmarker_(float *Channel, Int32 *MarkerColor){

  if(CrtPlot == NULL || Channel == NULL || MarkerColor == NULL)return;
  DrawMarker(CrtPlot,Channel,CrtPlot->Color[*MarkerColor]);
}

void xtpdoublemarker_(float *Channel1, float *Channel2, Int32 *MarkerColor){

  if(CrtPlot == NULL || Channel1 == NULL || Channel2 == NULL || MarkerColor == NULL)return;
  DrawDoubleMarker(CrtPlot,Channel1,Channel2,CrtPlot->Color[*MarkerColor]);
}


void xtpbell_(void){

  printf("\007");
  fflush(stdout);
}

static void UpdateCursorDisplay(struct DataValues *v) {
  float en;
  int i;
  static float Xmin, Xmax, Ymin, Ymax;
  if (!v || !v->Plot) return;

  sprintf(TrackChannelValue.Text, "Channel: %d", v->Channel);
  if (Track.ecal != NULL) {
    en = *(Track.ecal + 5);
    for (i = 4; i >= 0; i--) en = en * (v->Energy) + *(Track.ecal + i);
    en += pow(v->Energy, 0.5000000) * (*(Track.ecal + 6));
  } else {
    en = v->Energy;
  }
  sprintf(TrackEnergyValue.Text, "Energy: %8.2f", en);
  sprintf(TrackCountsValue.Text, "Counts: %12.2f", v->Counts);
  sprintf(TrackCursorYValue.Text, "Y: %12.2f", v->Y);
  WriteInLabel(TrackChannelValue);
  WriteInLabel(TrackEnergyValue);
  WriteInLabel(TrackCountsValue);
  WriteInLabel(TrackCursorYValue);
  if (TrackDisplayed.Text != v->Plot->Comment) {
    TrackDisplayed.Text = v->Plot->Comment;
    WriteInLabel(TrackDisplayed);
  }
  if (v->Plot->Xmin[v->Plot->ActiveData] != Xmin) {
    Xmin = v->Plot->Xmin[v->Plot->ActiveData];
    sprintf(TrackXMinValue.Text, "X Min: %6.1f", Xmin);
    WriteInLabel(TrackXMinValue);
  }
  if (v->Plot->Xmax[v->Plot->ActiveData] != Xmax) {
    Xmax = v->Plot->Xmax[v->Plot->ActiveData];
    sprintf(TrackXMaxValue.Text, "X Max: %6.1f", Xmax);
    WriteInLabel(TrackXMaxValue);
  }
  if (v->Plot->Ymin != Ymin) {
    Ymin = v->Plot->Ymin;
    sprintf(TrackYMinValue.Text, "Y Min: %12.2f", Ymin);
    WriteInLabel(TrackYMinValue);
  }
  if (v->Plot->Ymax != Ymax) {
    Ymax = v->Plot->Ymax;
    sprintf(TrackYMaxValue.Text, "Y Max: %12.2f", Ymax);
    WriteInLabel(TrackYMaxValue);
  }
  if (v->Plot != CrtPlot) {
    DrawPlotFrame(CrtPlot);
    DrawActivePlotFrame(v->Plot);
  }
  CrtPlot = v->Plot;
  PutTrackData(CrtPlot);
}

void RedrawWindow(int force_reshape) {
  Int32 curW, curH;
  struct TrackPlot *p;
  Int16 val;
  Display *dpy = (Display *)getXdpy();
  Window win = (Window)getXwid();
  XEvent ev;

  if (GLWindow == NULL) return;

  /* 1. Flush pending requests and compress queued resize/expose events */
  if (dpy && win) {
    XSync(dpy, False);
    while (XCheckTypedWindowEvent(dpy, win, ConfigureNotify, &ev)) {}
    while (XCheckTypedWindowEvent(dpy, win, Expose, &ev)) {}
  }

  /* 2. Drain any REDRAW events already in Ygl queue */
  while (qtest() == REDRAW) {
    qread(&val);
  }

  getsize(&curW, &curH);
  if (curW <= 0 || curH <= 0) return;

  /* 3. Re-layout only if dimensions changed, or re-viewport if forced */
  if (curW != _global_LastW || curH != _global_LastH) {
    ReshapeWindow();
    _global_LastW = curW;
    _global_LastH = curH;
  } else if (force_reshape) {
    reshapeviewport();
  }

  _in_redraw_window = 1;
  backbuffer(1);

  color(GLW_FRAMECOLOR);
  clear();

  DrawTrackFrame(GLWindow->ID);

  if (CrtPlot && CrtPlot->Comment) {
    TrackDisplayed.Text = CrtPlot->Comment;
  }
  WriteInLabel(TrackDisplayed);

  if (OldPlot == NULL && CrtPlot != NULL) {
    sprintf(TrackXMinValue.Text, "X Min: %6.1f", CrtPlot->Xmin[CrtPlot->ActiveData]);
    WriteInLabel(TrackXMinValue);
    sprintf(TrackXMaxValue.Text, "X Max: %6.1f", CrtPlot->Xmax[CrtPlot->ActiveData]);
    WriteInLabel(TrackXMaxValue);
    sprintf(TrackYMinValue.Text, "Y Min: %12.2f", CrtPlot->Ymin);
    WriteInLabel(TrackYMinValue);
    sprintf(TrackYMaxValue.Text, "Y Max: %12.2f", CrtPlot->Ymax);
    WriteInLabel(TrackYMaxValue);
    sprintf(TrackChannelValue.Text, "Channel ");
    sprintf(TrackEnergyValue.Text, "Energy ");
    sprintf(TrackCountsValue.Text, "Counts ");
    sprintf(TrackCursorYValue.Text, "Y ");
    WriteInLabel(TrackChannelValue);
    WriteInLabel(TrackEnergyValue);
    WriteInLabel(TrackCountsValue);
    WriteInLabel(TrackCursorYValue);
  }

  p = Plot;
  while (p) {
    DrawPlot(p);
    p = p->Next;
  }

  if (CrtPlot) {
    DrawActivePlotFrame(CrtPlot);
  }

  swapbuffers();
  gflush();
  _in_redraw_window = 0;
}

void inpx_( unsigned char *InterString, int *InterLength ){

  Int32 dev, imore;
  Int16 val;
  struct DataValues *v;
  unsigned char c, c3[3];

  fflush(stdin);
  fflush(stdout);
  ISLSetTerminal( RAW );
  imore = 0;
  while ( imore == 0 ) {
    if( qtest () ){
       dev = qread(&val);
   
    switch (dev) {
     case LEFTMOUSE: {
       if ( OldPlot == NULL ){ 
         if( (v=Values(GLWindow))!=NULL ){
           UpdateCursorDisplay(v);
         }
       }
       break;
     }

     case MOUSEX:
     case MOUSEY: {
       if (getbutton(LEFTMOUSE)!=1) break;
       if ( OldPlot == NULL ){ 
         if( (v=Values(GLWindow))!=NULL ){
           UpdateCursorDisplay(v);
         }
       }
       break;
     }

     case REDRAW: {
       RedrawWindow(1);
       break;
     }
		      
     case KEYBD: { c = (unsigned char )val;
                   if ( c == 13 ) c = 10;
                   imore = ISLPutInput( &c, 1);
		   break; }

      case LEFTMOUSEWHEEL:
      case LEFTARROWKEY: { 
        if (val == 0) break;
        if (dev == LEFTMOUSEWHEEL) {
          while (qtest() == dev) { Int16 qv = 0; qread(&qv); }
        }
        c3[0] = 27; c3[1] = 91; c3[2] = 68;
        imore = ISLPutInput( &c3[0], 3);
        break; 
      }

      case RIGHTMOUSEWHEEL:
      case RIGHTARROWKEY: { 
        if (val == 0) break;
        if (dev == RIGHTMOUSEWHEEL) {
          while (qtest() == dev) { Int16 qv = 0; qread(&qv); }
        }
        c3[0] = 27; c3[1] = 91; c3[2] = 67;
        imore = ISLPutInput( &c3[0], 3);
        break; 
      }
     
     default : break;
     }
    }
    else {
    ISLGetInput();
    imore = ISLPutInput( NULL, 0);
    }
  }
  
  ISLGetString( InterString, InterLength);
  ISLSetTerminal(RESTORE);
 }

static float GetWheelAcceleration(Int32 dev, int queued) {
  static struct timeval last_tv = {0, 0};
  static Int32 last_dev = 0;
  static float accel = 1.0f;

  struct timeval now;
  gettimeofday(&now, NULL);

  if (last_tv.tv_sec != 0 && dev == last_dev) {
    long dt_ms = (now.tv_sec - last_tv.tv_sec) * 1000 +
                 (now.tv_usec - last_tv.tv_usec) / 1000;
    if (dt_ms < 0) dt_ms = 0;

    if (dt_ms > 200) {
      /* Pause between gestures: reset to normal base speed */
      accel = 1.0f;
    } else if (dt_ms < 30) {
      /* Very fast swipe */
      accel += 0.35f;
    } else if (dt_ms < 60) {
      /* Moderate fast swipe */
      accel += 0.20f;
    } else if (dt_ms < 100) {
      /* Brisk scroll */
      accel += 0.10f;
    } else {
      /* Gentle scroll */
      accel -= 0.15f;
    }
  } else {
    /* Direction changed or first event */
    accel = 1.0f;
  }

  last_tv = now;
  last_dev = dev;

  /* Bound acceleration to strict saturation limits */
  if (accel < 1.0f) accel = 1.0f;
  if (accel > 2.2f) accel = 2.2f; /* Very limited base acceleration */

  /* Coalesce effect of queued events, capped to avoid runaway */
  if (queued > 2) queued = 2;
  float total_mult = accel * (1.0f + 0.35f * (float)queued);
  if (total_mult > 2.8f) total_mult = 2.8f; /* Strict saturation ceiling */

  return total_mult;
}

void xtpget_(float *x, float *y, Int32 *c){

  Int32 dev;
  Int16 val;
  float ShiftFactor;
  struct DataValues *v;
  struct TrackPlot *p;

  GraphFocus();

  if(OldPlot != NULL){
    KillPlots(Plot);
    Plot=OldPlot;
    OldPlot=NULL;
    RedrawWindow(0);
  }

  if (CrtPlot) {
    DrawActivePlotFrame(CrtPlot);
    sprintf(TrackXMinValue.Text,"X Min: %6.1f",CrtPlot->Xmin[CrtPlot->ActiveData]);
    WriteInLabel(TrackXMinValue);
    sprintf(TrackXMaxValue.Text,"X Max: %6.1f",CrtPlot->Xmax[CrtPlot->ActiveData]);
    WriteInLabel(TrackXMaxValue);
    sprintf(TrackYMinValue.Text,"Y Min: %12.2f",CrtPlot->Ymin);
    WriteInLabel(TrackYMinValue);
    sprintf(TrackYMaxValue.Text,"Y Max: %12.2f",CrtPlot->Ymax);
    WriteInLabel(TrackYMaxValue);
    TrackDisplayed.Text = CrtPlot->Comment;
    WriteInLabel(TrackDisplayed);
  }
        
  fflush(stdin);
  
  while ((dev = qread(&val))) {
   
    switch (dev) {
     case UPMOUSEWHEEL: { 
       if (val == 0) break;
       int queued = 0;
       while (qtest() == dev) {
         Int16 qv = 0;
         qread(&qv);
         if (qv != 0) queued++;
       }
       struct TrackPlot *targetPlot = NULL;
       if ((v = Values(GLWindow)) != NULL) targetPlot = v->Plot;
       else if (CrtPlot != NULL && CrtPlot->data[CrtPlot->ActiveData] != NULL) targetPlot = CrtPlot;
       if (targetPlot != NULL && CrtPlot != targetPlot) {
         DrawPlotFrame(CrtPlot);
         CrtPlot = targetPlot;
         DrawActivePlotFrame(CrtPlot);
       }
       if (CrtPlot) {
         float mult = GetWheelAcceleration(dev, queued);
         ShiftFactor = 0.025000f * mult;
         CrtPlot->Ymax += ShiftFactor * (CrtPlot->Ymax - CrtPlot->Ymin);
         PutTrackData(CrtPlot);
         sprintf(TrackYMaxValue.Text, "Y Max: %12.2f", CrtPlot->Ymax);
         WriteInLabel(TrackYMaxValue);
         RedrawWindow(0);
       }
       break;
     }
     case DOWNMOUSEWHEEL: { 
       if (val == 0) break;
       int queued = 0;
       while (qtest() == dev) {
         Int16 qv = 0;
         qread(&qv);
         if (qv != 0) queued++;
       }
       struct TrackPlot *targetPlot = NULL;
       if ((v = Values(GLWindow)) != NULL) targetPlot = v->Plot;
       else if (CrtPlot != NULL && CrtPlot->data[CrtPlot->ActiveData] != NULL) targetPlot = CrtPlot;
       if (targetPlot != NULL && CrtPlot != targetPlot) {
         DrawPlotFrame(CrtPlot);
         CrtPlot = targetPlot;
         DrawActivePlotFrame(CrtPlot);
       }
       if (CrtPlot) {
         float mult = GetWheelAcceleration(dev, queued);
         ShiftFactor = 0.025000f * mult;
         float range = CrtPlot->Ymax - CrtPlot->Ymin;
         if (range > 0.001f) {
           float new_ymax = CrtPlot->Ymax - ShiftFactor * range;
           if (new_ymax > CrtPlot->Ymin + 0.01f) {
             CrtPlot->Ymax = new_ymax;
           }
         }
         PutTrackData(CrtPlot);
         sprintf(TrackYMaxValue.Text, "Y Max: %12.2f", CrtPlot->Ymax);
         WriteInLabel(TrackYMaxValue);
         RedrawWindow(0);
       }
       break;
     }
     case LEFTMOUSE: { if( (v=Values(GLWindow))!=NULL ){
                          UpdateCursorDisplay(v);
			  if( (getbutton(LEFTCTRLKEY)==1) || (getbutton(RIGHTCTRLKEY)==1) ){
			       while(getbutton(LEFTMOUSE)==1) usleep(10000);
		               *x=v->Energy;
		               *y=v->Y;
			       *c = 'A'; (*c)<<=8; *c+='G';
			       qreset();
			       return;
			       }
		      }
		    else {
		       if( (getbutton(LEFTCTRLKEY)==1) || (getbutton(RIGHTCTRLKEY)==1) )ShiftFactor = 0.02500;
		       else ShiftFactor = 0.200;
		       if(MouseInLabel(TrackXMinValue) && (val == 1)){
		          if(CrtPlot) {
			    CrtPlot->Xmin[CrtPlot->ActiveData]+=Nintf(ShiftFactor*(CrtPlot->Xmax[CrtPlot->ActiveData]
			  				  -CrtPlot->Xmin[CrtPlot->ActiveData]))+1;
			    CrtPlot->Imin[CrtPlot->ActiveData]=CrtPlot->Xmin[CrtPlot->ActiveData];
			    if(CrtPlot->Imin[CrtPlot->ActiveData] >= CrtPlot->Imax[CrtPlot->ActiveData]){
			      CrtPlot->Imin[CrtPlot->ActiveData] = CrtPlot->Imax[CrtPlot->ActiveData]-1;
			      CrtPlot->Xmin[CrtPlot->ActiveData]=CrtPlot->Imin[CrtPlot->ActiveData];
			    }
		            PutTrackData(CrtPlot);
                            RedrawWindow(0);
			  }
		       }
		       if(MouseInLabel(TrackXMaxValue) && (val == 1) ){
		          if(CrtPlot) {
			    CrtPlot->Xmax[CrtPlot->ActiveData]+=Nintf(ShiftFactor*(CrtPlot->Xmax[CrtPlot->ActiveData]
			  				  -CrtPlot->Xmin[CrtPlot->ActiveData]))+1;
			    CrtPlot->Imax[CrtPlot->ActiveData]=CrtPlot->Xmax[CrtPlot->ActiveData];
			    if(CrtPlot->Imax[CrtPlot->ActiveData] >= CrtPlot->Np[CrtPlot->ActiveData]){
			      CrtPlot->Imax[CrtPlot->ActiveData] = CrtPlot->Np[CrtPlot->ActiveData]-1;
			      CrtPlot->Xmax[CrtPlot->ActiveData]=CrtPlot->Imax[CrtPlot->ActiveData];
			    }
		            PutTrackData(CrtPlot);
                            RedrawWindow(0);
			  }
		       }
		       if(MouseInLabel(TrackYMinValue) && (val == 1) ){
		          if(CrtPlot) {
			    CrtPlot->Ymin+=ShiftFactor*(CrtPlot->Ymax-CrtPlot->Ymin);
		            PutTrackData(CrtPlot);
                            RedrawWindow(0);
			  }
		       }
		       if(MouseInLabel(TrackYMaxValue) && (val == 1) ){
		          if(CrtPlot) {
			    CrtPlot->Ymax+=ShiftFactor*(CrtPlot->Ymax-CrtPlot->Ymin);
		            PutTrackData(CrtPlot);
                            RedrawWindow(0);
			  }
		       }
		       if(MouseInButton(TrackNewSpec) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'N'; *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackWriteSpec) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'O'; (*c)<<=8; *c+='S';
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackIncSpec) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
			  if( (getbutton(LEFTCTRLKEY)==1) || (getbutton(RIGHTCTRLKEY)==1) ){
		             *c = '1' ; *x=1 ; *y=1 ; return; 
			     } 
		             *c = '*' ; (*c)<<=8 ; *c+='1' ;
			     *x=1 ; *y=1 ; return;
			  }
		       if(MouseInButton(TrackDecSpec) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
			  if( (getbutton(LEFTCTRLKEY)==1) || (getbutton(RIGHTCTRLKEY)==1) ){
		             *c = '2' ; *x=1 ; *y=1 ; return; 
			     } 
		             *c = '*' ; (*c)<<=8 ; *c+='2' ;
			     *x=1 ; *y=1 ; return;
			  }
		       if(MouseInButton(TrackOpenCM) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'D' ; (*c)<<=8 ; *c+='Q' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackGateCM) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'C' ; (*c)<<=8 ; *c+='W' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackLeft) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = '<' ; *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackRight) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = '>' ; *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackSameX) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'S' ; (*c)<<=8 ; *c+='X' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackSameY) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'S' ; (*c)<<=8 ; *c+='Y' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackAutoX) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'F' ; (*c)<<=8 ; *c+='X' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackAutoY) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'F' ; (*c)<<=8 ; *c+='Y' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackAutoXY) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'F' ; (*c)<<=8 ; *c+='F' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackLinLog) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'L' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackRefresh) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = '=' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackDoPkS) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'C' ; (*c)<<=8 ; *c+='P' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackDoFit) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'C' ; (*c)<<=8 ; *c+='V' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackDoInt) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = 'C' ; (*c)<<=8 ; *c+='J' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackCal2P) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = '*' ; (*c)<<=8 ; *c+='K' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackAutoTrace) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
			  if( (getbutton(LEFTCTRLKEY)==1) || (getbutton(RIGHTCTRLKEY)==1) ){
			      *c = '*' ; (*c)<<=8 ; *c+='T' ;
			      }
			  else {
		              *c = 'D' ; (*c)<<=8 ; *c+='T' ;
			      }
			  *x=1 ; *y=1 ; return; 
			  }
		       if(MouseInButton(TrackEnCal) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
		          *c = '*' ; (*c)<<=8 ; *c+='E' ;
			  *x=1 ; *y=1 ; return; 
			  }
		       
		       }		      
		       break;
		     }
     case MOUSEX:
     case MOUSEY: {
       if (getbutton(LEFTMOUSE) != 1) break;
       if ((v = Values(GLWindow)) != NULL) {
         UpdateCursorDisplay(v);
       }
       break;
     }
		    
     case MIDDLEMOUSE: {
       if ((v = Values(GLWindow)) != NULL) {
         UpdateCursorDisplay(v);
       }
       while (getbutton(MIDDLEMOUSE)) {
         usleep(10000);
         if (qtest()) {
           dev = qread(&val);
           if ((dev == MOUSEX || dev == MOUSEY) && (v = Values(GLWindow)) != NULL) {
             UpdateCursorDisplay(v);
           }
         }
       }
       if ((v = Values(GLWindow)) != NULL) {
         UpdateCursorDisplay(v);
         *c = 'X';
         *x = v->Energy;
         *y = v->Y;
         return;
       }
       break;
     }	



     case MENUBUTTON:  { if( (val==1)&&( (v=Values(GLWindow))!=NULL ) ){
                          switch(dopup(TrackMenu)) {
			     case 4: { Plot->Col--;
			               if(Plot->Col < 1)Plot->Col=1;
			               TrackPlotMap(Plot->Row,Plot->Col);
				       CrtPlot=Plot;
				       break;}
			     
			     case 2: { Plot->Col++;
			               if(Plot->Col < 1)Plot->Col=1;
			               TrackPlotMap(Plot->Row,Plot->Col);
			               p=Plot;
			               while(p){
			                if(p->data[p->ActiveData] == NULL){
			                  OldPlot=CrtPlot;
				          CrtPlot=p;
				          CrtPlot->LogScale=OldPlot->LogScale;
				          xtplotnew_();
				          CopyComment( OldPlot, CrtPlot);
				          CrtPlot=OldPlot;
				          OldPlot=NULL; 
				          }
		                        p=p->Next;
		                        }
			                break;}

			     case 3: { Plot->Row--;
			               if(Plot->Row < 1)Plot->Row=1;
			               TrackPlotMap(Plot->Row,Plot->Col);
				       CrtPlot=Plot;
				       break;}
			     case 1: { Plot->Row++;
			               if(Plot->Row < 1)Plot->Row=1;
			               TrackPlotMap(Plot->Row,Plot->Col);
			               p=Plot;
			               while(p){
			                if(p->data[p->ActiveData] == NULL){
			                  OldPlot=CrtPlot;
				          CrtPlot=p;
				          CrtPlot->LogScale=OldPlot->LogScale;
				          xtplotnew_();
				          CopyComment( OldPlot, CrtPlot);
				          CrtPlot=OldPlot;
				          OldPlot=NULL; 
				          }
		                        p=p->Next;
		                        }
			                break;}
			     case 5: { break;}
			      }
			     RedrawWindow(0);
			     break;
			  }
		    else {
		       if( (getbutton(LEFTCTRLKEY)==1) || (getbutton(RIGHTCTRLKEY)==1) )ShiftFactor = 0.02500;
		       else ShiftFactor = 0.200;
		       if(MouseInLabel(TrackXMinValue) && (val == 1)){
		          if(CrtPlot) {
			    CrtPlot->Xmin[CrtPlot->ActiveData]-=Nintf(ShiftFactor*(CrtPlot->Xmax[CrtPlot->ActiveData]
			  				  -CrtPlot->Xmin[CrtPlot->ActiveData]))+1;
			    CrtPlot->Imin[CrtPlot->ActiveData]=CrtPlot->Xmin[CrtPlot->ActiveData];
			    if(CrtPlot->Imin[CrtPlot->ActiveData] < 0){
			      CrtPlot->Imin[CrtPlot->ActiveData] = 0;
			      CrtPlot->Xmin[CrtPlot->ActiveData]=CrtPlot->Imin[CrtPlot->ActiveData];
			    }
		            PutTrackData(CrtPlot);
                            RedrawWindow(0);
			  }
		       }
		       if(MouseInLabel(TrackXMaxValue) && (val == 1)){
		          if(CrtPlot) {
			    CrtPlot->Xmax[CrtPlot->ActiveData]-=Nintf(ShiftFactor*(CrtPlot->Xmax[CrtPlot->ActiveData]
			  				  -CrtPlot->Xmin[CrtPlot->ActiveData]))+1;
			    CrtPlot->Imax[CrtPlot->ActiveData]=CrtPlot->Xmax[CrtPlot->ActiveData];
			    if(CrtPlot->Imax[CrtPlot->ActiveData] <= CrtPlot->Imin[CrtPlot->ActiveData]){
			      CrtPlot->Imax[CrtPlot->ActiveData] = CrtPlot->Imin[CrtPlot->ActiveData]+1;
			      CrtPlot->Xmax[CrtPlot->ActiveData]=CrtPlot->Imax[CrtPlot->ActiveData];
			    }
		            PutTrackData(CrtPlot);
                            RedrawWindow(0);
			  }
		       }
		       if(MouseInLabel(TrackYMinValue) && (val == 1)){
		          if(CrtPlot) {
			    CrtPlot->Ymin-=ShiftFactor*(CrtPlot->Ymax-CrtPlot->Ymin);
		            PutTrackData(CrtPlot);
                            RedrawWindow(0);
			  }
		       }
		       if(MouseInLabel(TrackYMaxValue) && (val == 1)){
		          if(CrtPlot) {
			    CrtPlot->Ymax-=ShiftFactor*(CrtPlot->Ymax-CrtPlot->Ymin);
		            PutTrackData(CrtPlot);
                            RedrawWindow(0);
			  }
		       }
		       if(MouseInButton(TrackIncSpec) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
			  if( (getbutton(LEFTCTRLKEY)==1) || (getbutton(RIGHTCTRLKEY)==1) ){
		             *c = '3' ; *x=1 ; *y=1 ; return; 
			     } 
		             *c = '*' ; (*c)<<=8 ; *c+='3' ;
			     *x=1 ; *y=1 ; return;
			  }
		       if(MouseInButton(TrackDecSpec) && (val == 1) ) { 
		          PutTrackData(CrtPlot);
			  if( (getbutton(LEFTCTRLKEY)==1) || (getbutton(RIGHTCTRLKEY)==1) ){
		             *c = '4' ; *x=1 ; *y=1 ; return; 
			     } 
		             *c = '*' ; (*c)<<=8 ; *c+='4' ;
			     *x=1 ; *y=1 ; return;
			  }
		       }
		       break;
			}
     
     case LEFTMOUSEWHEEL: {
       if (val == 0) break;
       int queued = 0;
       while (qtest() == dev) {
         Int16 qv = 0;
         qread(&qv);
         if (qv != 0) queued++;
       }
       struct TrackPlot *targetPlot = NULL;
       if ((v = Values(GLWindow)) != NULL) targetPlot = v->Plot;
       else if (CrtPlot != NULL && CrtPlot->data[CrtPlot->ActiveData] != NULL) targetPlot = CrtPlot;
       if (targetPlot != NULL) {
         if (CrtPlot != targetPlot) {
           DrawPlotFrame(CrtPlot);
           CrtPlot = targetPlot;
           DrawActivePlotFrame(CrtPlot);
         }
         float mult = GetWheelAcceleration(dev, queued);
         float width = CrtPlot->Xmax[CrtPlot->ActiveData] - CrtPlot->Xmin[CrtPlot->ActiveData];
         float base_delta = (width + 1.0f) * 0.05f;
         if (base_delta < 1.0f) base_delta = 1.0f;
         float delta = Nintf(base_delta * mult);
         if (delta < 1.0f) delta = 1.0f;

         Int32 min_chan = (Track.ika != NULL) ? *(Track.ika) : 0;
         Int32 max_chan = (Track.ikl != NULL) ? (*(Track.ikl) - 1) : (Int32)(CrtPlot->Np[CrtPlot->ActiveData] - 1);

         float new_xmin = CrtPlot->Xmin[CrtPlot->ActiveData] - delta;
         if (new_xmin < (float)min_chan) new_xmin = (float)min_chan;
         float new_xmax = new_xmin + width;
         if (new_xmax > (float)max_chan) {
           new_xmax = (float)max_chan;
           new_xmin = new_xmax - width;
           if (new_xmin < (float)min_chan) new_xmin = (float)min_chan;
         }
         CrtPlot->Xmin[CrtPlot->ActiveData] = new_xmin;
         CrtPlot->Xmax[CrtPlot->ActiveData] = new_xmax;
         CrtPlot->Imin[CrtPlot->ActiveData] = new_xmin;
         CrtPlot->Imax[CrtPlot->ActiveData] = new_xmax;

         PutTrackData(CrtPlot);
         sprintf(TrackXMinValue.Text,"X Min: %6.1f",CrtPlot->Xmin[CrtPlot->ActiveData]);
         WriteInLabel(TrackXMinValue);
         sprintf(TrackXMaxValue.Text,"X Max: %6.1f",CrtPlot->Xmax[CrtPlot->ActiveData]);
         WriteInLabel(TrackXMaxValue);

         if (CrtPlot->Peak == NULL && Track.npeaks != NULL && *(Track.npeaks) > 0 && Track.peaks != NULL) {
           int pi;
           for (pi = 0; pi < *(Track.npeaks); pi++) {
             AddPeakLabel(CrtPlot, *(Track.peaks + pi));
           }
         }
         CrtPlot->ClearBeforeDraw = True;
         RedrawWindow(0);
       }
       break;
     }

     case LEFTARROWKEY: {
       if (val == 0) break;
       struct TrackPlot *targetPlot = NULL;
       if ((v = Values(GLWindow)) != NULL) targetPlot = v->Plot;
       else if (CrtPlot != NULL && CrtPlot->data[CrtPlot->ActiveData] != NULL) targetPlot = CrtPlot;
       if (targetPlot != NULL) {
         if ((getbutton(LEFTCTRLKEY) != 1) && (getbutton(RIGHTCTRLKEY) != 1)) {
           if (CrtPlot != targetPlot) {
             DrawPlotFrame(CrtPlot);
             CrtPlot = targetPlot;
             DrawActivePlotFrame(CrtPlot);
           }
           PutTrackData(CrtPlot);
           *c = '<';
           *x = 1;
           *y = 1;
           return;
         }
         Plot->Col--;
         if (Plot->Col < 1) Plot->Col = 1;
         TrackPlotMap(Plot->Row, Plot->Col);
         CrtPlot = Plot;
         RedrawWindow(0);
       }
       break;
     }

     case RIGHTMOUSEWHEEL: {
       if (val == 0) break;
       int queued = 0;
       while (qtest() == dev) {
         Int16 qv = 0;
         qread(&qv);
         if (qv != 0) queued++;
       }
       struct TrackPlot *targetPlot = NULL;
       if ((v = Values(GLWindow)) != NULL) targetPlot = v->Plot;
       else if (CrtPlot != NULL && CrtPlot->data[CrtPlot->ActiveData] != NULL) targetPlot = CrtPlot;
       if (targetPlot != NULL) {
         if (CrtPlot != targetPlot) {
           DrawPlotFrame(CrtPlot);
           CrtPlot = targetPlot;
           DrawActivePlotFrame(CrtPlot);
         }
         float mult = GetWheelAcceleration(dev, queued);
         float width = CrtPlot->Xmax[CrtPlot->ActiveData] - CrtPlot->Xmin[CrtPlot->ActiveData];
         float base_delta = (width + 1.0f) * 0.05f;
         if (base_delta < 1.0f) base_delta = 1.0f;
         float delta = Nintf(base_delta * mult);
         if (delta < 1.0f) delta = 1.0f;

         Int32 min_chan = (Track.ika != NULL) ? *(Track.ika) : 0;
         Int32 max_chan = (Track.ikl != NULL) ? (*(Track.ikl) - 1) : (Int32)(CrtPlot->Np[CrtPlot->ActiveData] - 1);

         float new_xmax = CrtPlot->Xmax[CrtPlot->ActiveData] + delta;
         if (new_xmax > (float)max_chan) new_xmax = (float)max_chan;
         float new_xmin = new_xmax - width;
         if (new_xmin < (float)min_chan) {
           new_xmin = (float)min_chan;
           new_xmax = new_xmin + width;
           if (new_xmax > (float)max_chan) new_xmax = (float)max_chan;
         }
         CrtPlot->Xmin[CrtPlot->ActiveData] = new_xmin;
         CrtPlot->Xmax[CrtPlot->ActiveData] = new_xmax;
         CrtPlot->Imin[CrtPlot->ActiveData] = new_xmin;
         CrtPlot->Imax[CrtPlot->ActiveData] = new_xmax;

         PutTrackData(CrtPlot);
         sprintf(TrackXMinValue.Text,"X Min: %6.1f",CrtPlot->Xmin[CrtPlot->ActiveData]);
         WriteInLabel(TrackXMinValue);
         sprintf(TrackXMaxValue.Text,"X Max: %6.1f",CrtPlot->Xmax[CrtPlot->ActiveData]);
         WriteInLabel(TrackXMaxValue);

         if (CrtPlot->Peak == NULL && Track.npeaks != NULL && *(Track.npeaks) > 0 && Track.peaks != NULL) {
           int pi;
           for (pi = 0; pi < *(Track.npeaks); pi++) {
             AddPeakLabel(CrtPlot, *(Track.peaks + pi));
           }
         }
         CrtPlot->ClearBeforeDraw = True;
         RedrawWindow(0);
       }
       break;
     }

     case RIGHTARROWKEY: {
       if (val == 0) break;
       struct TrackPlot *targetPlot = NULL;
       if ((v = Values(GLWindow)) != NULL) targetPlot = v->Plot;
       else if (CrtPlot != NULL && CrtPlot->data[CrtPlot->ActiveData] != NULL) targetPlot = CrtPlot;
       if (targetPlot != NULL) {
         if ((getbutton(LEFTCTRLKEY) != 1) && (getbutton(RIGHTCTRLKEY) != 1)) {
           if (CrtPlot != targetPlot) {
             DrawPlotFrame(CrtPlot);
             CrtPlot = targetPlot;
             DrawActivePlotFrame(CrtPlot);
           }
           PutTrackData(CrtPlot);
           *c = '>';
           *x = 1;
           *y = 1;
           return;
         }
         Plot->Col++;
         if (Plot->Col < 1) Plot->Col = 1;
         TrackPlotMap(Plot->Row, Plot->Col);
         p = Plot;
         while (p) {
           if (p->data[p->ActiveData] == NULL) {
             OldPlot = CrtPlot;
             CrtPlot = p;
             CrtPlot->LogScale = OldPlot->LogScale;
             xtplotnew_();
             CopyComment(OldPlot, CrtPlot);
             CrtPlot = OldPlot;
             OldPlot = NULL;
           }
           p = p->Next;
         }
         RedrawWindow(0);
       }
       break;
     }

     case DOWNARROWKEY: {
       if (val == 0) break;
       if( (v=Values(GLWindow))!=NULL ){
	 if( (getbutton(LEFTCTRLKEY)!=1) && (getbutton(RIGHTCTRLKEY)!=1) )break;
         Plot->Row--;
	 if(Plot->Row < 1)Plot->Row=1;
	 TrackPlotMap(Plot->Row,Plot->Col);
	 CrtPlot=Plot;
         RedrawWindow(0);
       }
       break;
     }

     case UPARROWKEY: {
       if (val == 0) break;
       if( (v=Values(GLWindow))!=NULL ){
	 if( (getbutton(LEFTCTRLKEY)!=1) && (getbutton(RIGHTCTRLKEY)!=1) ){
	   if( CrtPlot != v->Plot ){
	     DrawPlotFrame(CrtPlot);
	     CrtPlot = v->Plot;
	     DrawActivePlotFrame(CrtPlot);
	   }
	   PutTrackData(CrtPlot);
           *c = 'F'; (*c) <<=8; *c += 'Y'; *x=1 ; *y=1 ; return;
	 }
         Plot->Row++;
	 if(Plot->Row < 1)Plot->Row=1;
	 TrackPlotMap(Plot->Row,Plot->Col);
	 p=Plot;
	 while(p){
	   if(p->data[p->ActiveData] == NULL){
	     OldPlot=CrtPlot;
	     CrtPlot=p;
	     CrtPlot->LogScale=OldPlot->LogScale;
	     xtplotnew_();
	     CopyComment( OldPlot, CrtPlot);
	     CrtPlot=OldPlot;
	     OldPlot=NULL; 
	   }
           p=p->Next;
         }
         RedrawWindow(0);
       }
       break;
     }

     case KEYBD: {
       if ((v = Values(GLWindow)) != NULL) {
         UpdateCursorDisplay(v);
         *x = v->Energy;
         *y = v->Y;
         *c = val;
         return;
       }
       break;
     }

     case REDRAW: { 
       RedrawWindow(1);
       break;
     }

     case WINQUIT: {
       gexit();
       exit(0);
       break;
     } 
     default: break;
     }
    } 
}
 
 
/********************************* END FORTRAN *****************************/


void PutTrackData(struct TrackPlot *p){

  size_t Np;
  
  Np=p->Np[p->ActiveData];
  memcpy(Track.spek,p->data[p->ActiveData],Np*sizeof(float));
  memcpy(Track.err,p->err[p->ActiveData],Np*sizeof(float));
  *(Track.ika)=0; *(Track.ikl)=Np;
  *(Track.nmin)=p->Xmin[p->ActiveData];
  *(Track.nmax)=p->Xmax[p->ActiveData]+1;
  *(Track.ymin)=p->Ymin; 
  if(p->LogScale==1){*(Track.ymin)=pow(10.0000,*(Track.ymin));}
  if(p->LogScale==2){*(Track.ymin)=pow(*(Track.ymin),2.000);}
  *(Track.ymax)=p->Ymax; 
  if(p->LogScale==1){*(Track.ymax)=pow(10.0000,*(Track.ymax));}
  if(p->LogScale==2){*(Track.ymax)=pow(*(Track.ymax),2.000);}
  *(Track.ifunct)=p->LogScale+1;
 }

void TermFocus(void){

 if(ioTerm.disp == NULL){
   ioTerm.disp=XOpenDisplay(NULL);
   if(ioTerm.disp == NULL){
      printf(" Cannot open DISPLAY\n");
      exit(0);
      }
   XGetInputFocus(ioTerm.disp,&(ioTerm.win),&(ioTerm.revert));
   ioFocus.disp=ioTerm.disp;
   ioFocus.win=ioTerm.win;
   return;
   }
 XSync(ioTerm.disp,True);
 }

void GraphFocus(void){

 XWindowAttributes ioAttrib;
 Display *d = ioTerm.disp ? ioTerm.disp : (Display *)getXdpy();
 Window w = getXwid();

  if (d == NULL || w == 0) return;
  XSync(d, True);
  XGetWindowAttributes(d, w, &ioAttrib);
  if(ioAttrib.map_state != IsViewable){
   XMapWindow(d, w);
   XRaiseWindow(d, w);
   while(ioAttrib.map_state != IsViewable) {
     usleep(10000);
     XGetWindowAttributes(d, w, &ioAttrib);
   }
  }
  XSetInputFocus(d, w, ioTerm.revert, CurrentTime);
  XSync(d, True);
 }
 
 
void CleanPlot(struct TrackPlot *p){

  int i;
  float *b;
  
  if(p->ActiveData){
     b=p->data[0];
     p->data[0]=p->data[p->ActiveData];
     p->data[p->ActiveData]=b;
     b=p->logdata[0];
     p->logdata[0]=p->logdata[p->ActiveData];
     p->logdata[p->ActiveData]=b;
     b=p->sqrtdata[0];
     p->sqrtdata[0]=p->sqrtdata[p->ActiveData];
     p->sqrtdata[p->ActiveData]=b;
     b=p->err[0];
     p->err[0]=p->err[p->ActiveData];
     p->err[p->ActiveData]=b;
     p->Np[0]=p->Np[p->ActiveData];
     p->Imin[0]=p->Imin[p->ActiveData];
     p->Imax[0]=p->Imax[p->ActiveData];
     p->Xmin[0]=p->Xmin[p->ActiveData];
     p->Xmax[0]=p->Xmax[p->ActiveData];
     } 
  p->ActiveData=0;    
  for(i=1; i<7; i++)p->Draw[i]=0;
  KillAllPeakLabels( p );
  
 }


void DrawMarker(struct TrackPlot *Pl, float *Channel, Int32 MarkerColor){

 float x,y,pixelsizeY;

 if(Pl == NULL)return;
 if(Pl->Draw[Pl->ActiveData] == 0)return;
 if(*Channel <= Pl->Xmin[Pl->ActiveData])return;
 if(*Channel >= Pl->Xmax[Pl->ActiveData]+1.00E0)return;
 
 x=(float)(*Channel-Pl->Xmin[Pl->ActiveData])/
   (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1);
 x+=1.000-(float)(1.000-*Channel+Pl->Xmax[Pl->ActiveData])/
   (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1);
 x/=2.0000;
 x=Pl->x1+x*(Pl->x2-Pl->x1);
  
 pixelsizeY = _global_py;

 if(Pl->y2-Pl->y1 < 10.0*pixelsizeY)return;
 
 color(MarkerColor);
 y=Pl->y1+4.0*pixelsizeY;
 move2(x,y);
 y=Pl->y2-4.0*pixelsizeY;
 draw2(x,y);
 if (!_in_redraw_window) {
   frontbuffer(1);
   color(MarkerColor);
   move2(x,Pl->y1+4.0*pixelsizeY);
   draw2(x,Pl->y2-4.0*pixelsizeY);
   backbuffer(1);
   gflush();
 }
 }


void DrawDoubleMarker(struct TrackPlot *Pl, float *Channel1, float *Channel2, Int32 MarkerColor){

 float x,y,pixelsizeY;

 if(Pl == NULL)return;
 if(Pl->Draw[Pl->ActiveData] == 0)return;
 if(*Channel1 <= Pl->Xmin[Pl->ActiveData])return;
 if(*Channel1 >= Pl->Xmax[Pl->ActiveData])return;
 if(*Channel2 <= Pl->Xmin[Pl->ActiveData])return;
 if(*Channel2 >= Pl->Xmax[Pl->ActiveData])return;
 
 x=(float)(*Channel1-Pl->Xmin[Pl->ActiveData])/
   (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1);
 x+=1.000-(float)(1.000-*Channel1+Pl->Xmax[Pl->ActiveData])/
   (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1);
 x/=2.0000;
 x=Pl->x1+x*(Pl->x2-Pl->x1);
  
 pixelsizeY = _global_py;

 if(Pl->y2-Pl->y1 < 10.0*pixelsizeY)return;
 
 color(MarkerColor);
 y=Pl->y2-4.0*pixelsizeY;
 move2(x,y);
 y=Pl->y1+4.0*pixelsizeY;
 draw2(x,y);
 x=(float)(*Channel2-Pl->Xmin[Pl->ActiveData])/
   (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1);
 x+=1.000-(float)(1.000-*Channel2+Pl->Xmax[Pl->ActiveData])/
   (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1);
 x/=2.0000;
 x=Pl->x1+x*(Pl->x2-Pl->x1);
 draw2(x,y);
 y=Pl->y2-4.0*pixelsizeY;
 draw2(x,y);
 if (!_in_redraw_window) {
   float x_orig;
   x_orig=(float)(*Channel1-Pl->Xmin[Pl->ActiveData])/
     (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1);
   x_orig+=1.000-(float)(1.000-*Channel1+Pl->Xmax[Pl->ActiveData])/
     (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1);
   x_orig/=2.0000;
   x_orig=Pl->x1+x_orig*(Pl->x2-Pl->x1);

   frontbuffer(1);
   color(MarkerColor);
   move2(x_orig, Pl->y2-4.0*pixelsizeY);
   draw2(x_orig, Pl->y1+4.0*pixelsizeY);
   draw2(x, Pl->y1+4.0*pixelsizeY);
   draw2(x, Pl->y2-4.0*pixelsizeY);
   backbuffer(1);
   gflush();
 }
 }

void CopyComment( struct TrackPlot *from_p, struct TrackPlot *to_p )
{
       if( !from_p || !to_p || from_p == to_p ) return;
       if( to_p->Comment ) {
           free( to_p->Comment );
           to_p->Comment = NULL;
       }
       if( from_p->Comment )
           to_p->Comment = strdup( from_p->Comment );
}


void DrawPeakLabel(struct TrackPlot *Pl, float *Channel){

 Int32 FontHeight,yy,stick,i,j;
 char PeakLabel[20];
 float x,y,r,pixelsizeX,pixelsizeY;
 float en;
 float *data = NULL;

 if(Pl == NULL)return;
 if(Pl->Draw[Pl->ActiveData] == 0)return;
 if(*Channel <= Pl->Xmin[Pl->ActiveData])return;
 if(*Channel >= Pl->Xmax[Pl->ActiveData])return;


 switch(Pl->LogScale){
   case 0: {data=Pl->data[Pl->ActiveData];     break;}
   case 1: {data=Pl->logdata[Pl->ActiveData];  break;}
   case 2: {data=Pl->sqrtdata[Pl->ActiveData]; break;}
   }
 if (data == NULL) return;

 
 x=(float)(*Channel-Pl->Xmin[Pl->ActiveData])/
   (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1);
 x+=1.000-(float)(1.000-*Channel+Pl->Xmax[Pl->ActiveData])/
   (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1);
 x/=2.0000;
 i=Pl->Imin[Pl->ActiveData]+(x*(float)(Pl->Imax[Pl->ActiveData]-Pl->Imin[Pl->ActiveData]+1));
 if (i < 0) i = 0;
 if (i >= Pl->Np[Pl->ActiveData]) i = Pl->Np[Pl->ActiveData] - 1;
 if (i < 0) return;
 x=Pl->x1+x*(Pl->x2-Pl->x1);
  
 font(GLW_FONTID12);
 FontHeight=getheight(); FontHeight+=FontHeight%2;
 font(GLW_FONTID14);
 pixelsizeX = _global_px;
 pixelsizeY = _global_py;

 if(Track.ecal != NULL){
   r=*Channel;
   en=*(Track.ecal+5);
   for(j=4; j>=0; j--)en=en*r+*(Track.ecal+j);
   en+=pow(r,0.5000000)*(*(Track.ecal+6));
   }
 else en=*Channel;
 
 
 sprintf(PeakLabel,"%-.1f",en);
 yy=(Pl->y2-Pl->y1)/pixelsizeY;
 
 if( (yy-FontHeight)<=6 )return;
 stick=0;
 if( (yy-FontHeight)<=16)stick=yy-FontHeight-6; 
 else stick=10;
 stick-=stick%2;

 y=*(data+i);
 y=(y-Pl->Ymin > 0)?y:Pl->Ymin;   y=(y-Pl->Ymax < 0)?y:Pl->Ymax;
 y=Pl->y1+(y-Pl->Ymin)/(Pl->Ymax-Pl->Ymin)*(Pl->y2-Pl->y1);


 r=pixelsizeY*(float)(stick/2+FontHeight+6);
 if(Pl->y2 < y+r )y=Pl->y2-r;
 r=pixelsizeY*(float)(stick/2+2);
 if(Pl->y1 > y-r)y=Pl->y1+r;
 
 float stick_x = x;
 float stick_y1 = y - pixelsizeY*(float)(stick/2);
 float stick_y2 = stick_y1 + pixelsizeY*stick;

 float text_y = y + pixelsizeY*(float)(FontHeight/2+2);
 float text_r = pixelsizeX*(float)(strwidth(PeakLabel)/2+1);
 if(2*text_r > (Pl->x2-Pl->x1) )return;
 float text_x = x;
 if(Pl->x1 > text_x-text_r) text_x = Pl->x1+text_r;
 if(Pl->x2 < text_x+text_r) text_x = Pl->x2-text_r; 
 text_x -= text_r;

 color(YELLOW);
 move2(stick_x, stick_y1); 
 draw2(stick_x, stick_y2);
 cmov2(text_x, text_y);
 font(GLW_FONTID12);
 charstr(PeakLabel);
 font(GLW_FONTID14);

 if (!_in_redraw_window) {
   frontbuffer(1);
   color(YELLOW);
   move2(stick_x, stick_y1); 
   draw2(stick_x, stick_y2);
   cmov2(text_x, text_y);
   font(GLW_FONTID12);
   charstr(PeakLabel);
   font(GLW_FONTID14);
   backbuffer(1);
   gflush();
 }
 }
  

void DrawNumberedPeakLabel(struct TrackPlot *Pl, float *Channel, Int32 N, Int32 LabelColor){

 Int32 FontHeight,yy,stick,i,j;
 char PeakLabel[20];
 float x,y,r,pixelsizeX,pixelsizeY,en;
 float *data = NULL;

 if(Pl == NULL)return;
 if(Pl->Draw[Pl->ActiveData] == 0)return;
 if(*Channel <= Pl->Xmin[Pl->ActiveData])return;
 if(*Channel >= Pl->Xmax[Pl->ActiveData])return;


 switch(Pl->LogScale){
   case 0: {data=Pl->data[Pl->ActiveData];     break;}
   case 1: {data=Pl->logdata[Pl->ActiveData];  break;}
   case 2: {data=Pl->sqrtdata[Pl->ActiveData]; break;}
   }
 if (data == NULL) return;

 
 x=(float)(*Channel-Pl->Xmin[Pl->ActiveData])/
   (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1);
 x+=1.000-(float)(1.000-*Channel+Pl->Xmax[Pl->ActiveData])/
   (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1);
 x/=2.0000;
 i=Pl->Imin[Pl->ActiveData]+(x*(float)(Pl->Imax[Pl->ActiveData]-Pl->Imin[Pl->ActiveData]+1));
 if (i < 0) i = 0;
 if (i >= Pl->Np[Pl->ActiveData]) i = Pl->Np[Pl->ActiveData] - 1;
 if (i < 0) return;
 x=Pl->x1+x*(Pl->x2-Pl->x1);
  
 font(GLW_FONTID12);
 FontHeight=getheight(); FontHeight+=FontHeight%2;
 font(GLW_FONTID14);
 pixelsizeX = _global_px;
 pixelsizeY = _global_py;

 if(Track.ecal != NULL){
   r=*Channel;
   en=*(Track.ecal+5);
   for(j=4; j>=0; j--)en=en*r+*(Track.ecal+j);
   en+=pow(r,0.5000000)*(*(Track.ecal+6));
   }
 else en=*Channel;
 sprintf(PeakLabel,"[%d]%-.1f",N,en);
 yy=(Pl->y2-Pl->y1)/pixelsizeY;
 
 if( (yy-FontHeight)<=6 )return;
 stick=0;
 if( (yy-FontHeight)<=106)stick=yy-FontHeight-6; 
 else stick=100;
 stick-=stick%2;

 y=*(data+i);
 y=(y-Pl->Ymin > 0)?y:Pl->Ymin; y=(y-Pl->Ymax < 0)?y:Pl->Ymax;
 y=Pl->y1+(y-Pl->Ymin)/(Pl->Ymax-Pl->Ymin)*(Pl->y2-Pl->y1);
 r=pixelsizeY*(float)(stick/2+FontHeight+6);
 if(Pl->y2 < y+r )y=Pl->y2-r;
 r=pixelsizeY*(float)(stick/2+2);
 if(Pl->y1 > y-r)y=Pl->y1+r;
 
 float stick_x = x;
 float stick_y1 = y - pixelsizeY*(float)(stick/2);
 float stick_y2 = stick_y1 + pixelsizeY*stick;

 float text_y = y + pixelsizeY*(float)(FontHeight/2+2);
 float text_r = pixelsizeX*(float)(strwidth(PeakLabel)/2+1);
 if(2*text_r > (Pl->x2-Pl->x1) )return;
 float text_x = x;
 if(Pl->x1 > text_x-text_r) text_x = Pl->x1+text_r;
 if(Pl->x2 < text_x+text_r) text_x = Pl->x2-text_r; 
 text_x -= text_r;

 color(LabelColor);
 move2(stick_x, stick_y1); 
 draw2(stick_x, stick_y2);
 cmov2(text_x, text_y);
 font(GLW_FONTID12);
 charstr(PeakLabel);
 font(GLW_FONTID14);

 if (!_in_redraw_window) {
   frontbuffer(1);
   color(LabelColor);
   move2(stick_x, stick_y1); 
   draw2(stick_x, stick_y2);
   cmov2(text_x, text_y);
   font(GLW_FONTID12);
   charstr(PeakLabel);
   font(GLW_FONTID14);
   backbuffer(1);
   gflush();
 }
 }
  

void DrawPlot(struct TrackPlot *Pl){

 Int32 i;

 if(Pl == NULL)return;

 if(Pl->ClearBeforeDraw || _in_redraw_window){
   color(GLW_DRAWBG);
   rectf(Pl->x1, Pl->y1,
	 Pl->x2, Pl->y2);
 }
 
 for(i=0; i<7; i++)DrawSubPlot(Pl,i);

 if( Pl->Peak ) {
    PeakLabel *pk;
    pk = Pl->Peak;
    while( pk ){
       DrawPeakLabel ( Pl, &pk->Channel );
       pk = pk->Next;
    }
 }

 if( Pl->NumPeak ) {
    NumPeakLabel *npk;
    npk = Pl->NumPeak;
    while( npk ){
       DrawNumberedPeakLabel ( Pl, &npk->Channel, npk->N, npk->LabelColor );
       npk = npk->Next;
    }
 }

 if( Pl == CrtPlot )DrawActivePlotFrame(Pl);
 else DrawPlotFrame(Pl);
}
 

void DrawSubPlot(struct TrackPlot *Pl, Int32 Index){

  float stepX,stepY,r1,r2,x1,x2,step;
  Coord x,y;
  int i,i1,i2, nn, ii, ii2;
  float *data;

  if(Pl->data[Index] == NULL)return;
  if(Pl->logdata[Index] == NULL)return;
  if(Pl->Draw[Index]==0)return;
  if(Pl->Xmin[Index] >= Pl->Xmax[Pl->ActiveData])return;
  if(Pl->Xmax[Index] <= Pl->Xmin[Pl->ActiveData])return;


 switch(Pl->LogScale){
   case 0: {data=Pl->data[Index];     break;}
   case 1: {data=Pl->logdata[Index];  break;}
   case 2: {data=Pl->sqrtdata[Index]; break;}
   }

  color(Pl->Color[Index]);
  
  r1=( Pl->Xmin[Index]-Pl->Xmin[Pl->ActiveData] > 0 )?Pl->Xmin[Index]:Pl->Xmin[Pl->ActiveData];
  i1=r1;
  r2=( Pl->Xmax[Index]-Pl->Xmax[Pl->ActiveData] < 0 )?Pl->Xmax[Index]:Pl->Xmax[Pl->ActiveData];
  i2=r2;
  x1=Pl->x1+(r1-(float)Pl->Xmin[Pl->ActiveData])/
            (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1)*
            (Pl->x2-Pl->x1);
									      
  x2=Pl->x2+(r2-(float)Pl->Xmax[Pl->ActiveData]-1)/
            (float)(Pl->Xmax[Pl->ActiveData]-Pl->Xmin[Pl->ActiveData]+1)*
            (Pl->x2-Pl->x1);
  step=(float)(Pl->Xmax[Index] - Pl->Xmin[Index])/(float)(Pl->Imax[Index] - Pl->Imin[Index]+1);
  r1=(float)Pl->Imin[Index]+(r1-(float)Pl->Xmin[Index])/(step+(float)(Pl->Xmax[Index] - Pl->Xmin[Index]))*
                     (float)(Pl->Imax[Index] - Pl->Imin[Index]+1);
		     
  r2=(float)Pl->Imax[Index]+(r2-(float)Pl->Xmax[Index])/(step+(float)(Pl->Xmax[Index] - Pl->Xmin[Index]))*
                     (float)(Pl->Imax[Index] - Pl->Imin[Index]+1);
		     
  i1=Nintf(r1); i2=Nintf(r2);
  stepX=(x2 - x1)/( i2-i1);
  stepY=(Pl->y2 - Pl->y1)/(Pl->Ymax - Pl->Ymin);
  
  i=i1; /*Pl->Imin[Index];*/
  x=x1; /*+0.500*stepX;*/
  y=Pl->y1+1+stepY*(data[i]-Pl->Ymin);
  if(y < Pl->y1)y=Pl->y1;
  if(y > Pl->y2)y=Pl->y2;
  move2(x,y);
  nn = _global_px/stepX ;
  if( nn < 2 )nn = 1;
  if( nn < 6 ) nn/=2;


  if( nn > 1 ){
  stepX *= nn;
  x1 = x2 = y;
  for(i=i1; i<=i2 ; i+=nn){
       r1 = data[i];
       r2 = data[i];
       ii2 = (i+nn < i2)?(i+nn):i2+1;
       
       for( ii = i; ii < ii2; ii++ ) {
         if(data[ii] > r1) r1 = data[ii];
         if(data[ii] < r2) r2 = data[ii];
       }
/*       
       if( kk1 < kk2 ) { r1 = data[kk1]; r2 = data[kk2]; }
       else { r1 = data[kk2]; r2 = data[kk1]; }
*/
       y=Pl->y1+stepY*(r2-Pl->Ymin) ;
       if( x1 < y ) y = x1;
       if(y < Pl->y1)y=Pl->y1;
       if(y > Pl->y2)y=Pl->y2;
       if(x < Pl->x1)x=Pl->x1;
       if(x > Pl->x2)x=Pl->x2;
       move2(x,y);

       y=Pl->y1+stepY*(r1-Pl->Ymin) ;
       if( x2 > y ) y = x2;
       if(y < Pl->y1)y=Pl->y1;
       if(y > Pl->y2)y=Pl->y2;
       if(x < Pl->x1)x=Pl->x1;
       if(x > Pl->x2)x=Pl->x2;
       draw2(x,y);
       x1 = Pl->y1+stepY*(r1-Pl->Ymin);
       x2 = Pl->y1+stepY*(r2-Pl->Ymin);
       x+= stepX;
       }
  }
  
  else {
  x-=stepX; 
  for(i=i1; i<=i2 ; i++){
       x+=stepX;
       y=Pl->y1+stepY*(data[i]-Pl->Ymin) ;
       if(y < Pl->y1)y=Pl->y1;
       if(y > Pl->y2)y=Pl->y2;
       if(x < Pl->x1)x=Pl->x1;
       if(x > Pl->x2)x=Pl->x2;
       draw2(x,y);
        
       x+= stepX;
       if(x < Pl->x1)x=Pl->x1;
       if(x > Pl->x2)x=Pl->x2;
       draw2(x,y);
       x-= stepX;
       }
  }
 }

void SetPlotData(Int32 Index, Int32 *Np,
                 Int32 *Imin, Int32 *Imax,
                 float *Xmin, float *Xmax,
		 float *Ymin, float *Ymax,
		 float *data, float *err, struct TrackPlot *Pl)  {

 int i;
 size_t np;
  
  if( (*Imin < 0) || (*Imax >= *Np) ){
    Pl->Draw[Index]=0;
    return;
    }
  np=*Np;
  if ( (Pl->data[Index]=(float *)realloc(Pl->data[Index],np*sizeof(float))) == NULL){
      Pl->Draw[Index]=0;
      return;
      }
  if ( (Pl->logdata[Index]=(float *)realloc(Pl->logdata[Index],np*sizeof(float))) == NULL){
      Pl->Draw[Index]=0;
      return;
      }
  if ( (Pl->sqrtdata[Index]=(float *)realloc(Pl->sqrtdata[Index],np*sizeof(float))) == NULL){
      Pl->Draw[Index]=0;
      return;
      }
  if ( (Pl->err[Index]=(float *)realloc(Pl->err[Index],np*sizeof(float))) == NULL){
      Pl->Draw[Index]=0;
      return;
      }
  Pl->Imin[Index]=*Imin;
  Pl->Imax[Index]=*Imax;
  Pl->Xmin[Index]=*Xmin;
  Pl->Xmax[Index]=*Xmax;
  Pl->Ymin=*Ymin;
  Pl->Ymax=*Ymax;
  Pl->Draw[Index]=True;
  Pl->Np[Index]=*Np;
  for(i=0; i<(*Np); i++){
    Pl->data[Index][i]=*(data+i);
    if(*(data+i) > 0.0000000000000) Pl->logdata[Index][i]=log10(*(data+i));
    else Pl->logdata[Index][i]=0.0000000000000;
    if(*(data+i) > 0.0000000000000) Pl->sqrtdata[Index][i]=pow(*(data+i),0.5000);
    else Pl->sqrtdata[Index][i]=0.0000000000000;
    Pl->err[Index][i] =*(err+i);
    }
  Pl->LastData=Index;
 }    

void DrawPlotFrame(struct TrackPlot *Pl){

  if (Pl == NULL) return;
  backbuffer(1);
  color(GLW_LABELCOLOR_2);
  rect(Pl->x1,Pl->y1,Pl->x2,Pl->y2);
  if (!_in_redraw_window) {
    frontbuffer(1);
    color(GLW_LABELCOLOR_2);
    rect(Pl->x1,Pl->y1,Pl->x2,Pl->y2);
    backbuffer(1);
    gflush();
  }
 }

void DrawActivePlotFrame(struct TrackPlot *Pl){

  if (Pl == NULL) return;
  backbuffer(1);
  color(WHITE);
  rect(Pl->x1,Pl->y1,Pl->x2,Pl->y2);
  if (!_in_redraw_window) {
    frontbuffer(1);
    color(WHITE);
    rect(Pl->x1,Pl->y1,Pl->x2,Pl->y2);
    backbuffer(1);
    gflush();
  }
 }


struct TrackPlot* TrackPlotMap(Int32 Row, Int32 Col){

 Int32 Nplots, i,j;
 float pixelsizeX,pixelsizeY,tX0,tX1,tY0,tY1,stepX,stepY;
 struct TrackPlot *p,*Plot0;
  
 if( (Row <= 0) || (Col <= 0) )return NULL;

 Nplots=Row*Col;
   
 pixelsizeX = _global_px;
 pixelsizeY = _global_py;

 tX0=(5.0+GLW_FRAMEWIDTH)*pixelsizeX;
 tX1=(float)GLW_MINXSIZE-tX0;
 tY0=(5.0+GLW_FRAMEWIDTH)*pixelsizeY;
 tY1=(float)GLW_MINYSIZE-tY0;
 
 stepX=(tX1-tX0)/(float)Col;  stepY=(tY1-tY0)/(float)Row;

 p=Plot;
 if(Nplots == NofPlots){
    for(i=0; i<Row; i++){
      for(j=0; j<Col; j++){
          (*p).x1=tX0+stepX*j; p->x2 = tX0+stepX*(j+1);
	  (*p).y2=tY1-stepY*i; p->y1 = tY1-stepY*(i+1);
	  p=(*p).Next;
	  }
      }
     Plot->Row=Row;
     Plot->Col=Col;
     return Plot;
     }

 
 if( (Nplots < NofPlots)&&(Plot != NULL) ){
    p=Plot;
    for(i=1; (i<Nplots)&&(p->Next != NULL); i++) p=p->Next;
    Plot0=p->Next;
    p->Next=NULL;
    if (Plot0) KillPlots(Plot0);
    }
    
 if(Plot == NULL){   
  if( ( Plot=(struct TrackPlot *)calloc(1,sizeof(struct TrackPlot)) ) == NULL)return Plot;
  NofPlots=1;
  }

 if( (Nplots > NofPlots)&&(Plot != NULL) ){
   p=Plot;
   while(p->Next != NULL)p=p->Next;
   for(i=0; i<(Nplots-NofPlots); i++){
     p->Next=(struct TrackPlot *)calloc(1,sizeof(struct TrackPlot));
     p=p->Next; p->Next=NULL;
     }
   }
   
 
 p=Plot; 
 for(i=0; i < Nplots ; i++){
    if( p == NULL ){
       NofPlots=i+1;
       return Plot;
       }
    j=i%Col;
    (*p).x1=tX0+stepX*j; p->x2 = tX0+stepX*(j+1);
    j=i/Col;
    (*p).y2=tY1-stepY*j; p->y1 = tY1-stepY*(j+1);
    (*p).ClearBeforeDraw=True;
    p->Color[0]=WHITE;
    p->Color[1]=RED;
    p->Color[2]=GREEN;
    p->Color[3]=YELLOW;
    p->Color[4]=BLUE;
    p->Color[5]=CYAN;
    p->Color[6]=MAGENTA;
    if(p->Comment == NULL){
       p->Comment=(char *)realloc(p->Comment,7);
       sprintf(p->Comment,"<none>");
       p->FileFormat = -1;
       }
    p=p->Next;
    }
  Plot->Row=Row;
  Plot->Col=Col;
  NofPlots=i;
  return Plot;
 }    



void KillPlots(struct TrackPlot* Plot0){

  struct TrackPlot* p = Plot0;
  while (p != NULL) {
    struct TrackPlot *next = p->Next;
    int i;
    KillAllPeakLabels(p);
    for (i = 0; i < 7; i++) {
      if (p->data[i]) { free(p->data[i]); p->data[i] = NULL; }
      if (p->logdata[i]) { free(p->logdata[i]); p->logdata[i] = NULL; }
      if (p->sqrtdata[i]) { free(p->sqrtdata[i]); p->sqrtdata[i] = NULL; }
      if (p->err[i]) { free(p->err[i]); p->err[i] = NULL; }
    }
    if (p->Comment) { free(p->Comment); p->Comment = NULL; }
    if (p->LastFile) { free(p->LastFile); p->LastFile = NULL; }
    free(p);
    p = next;
  }
 }

void KillAllPeakLabels( struct TrackPlot* p ){

   if( p ){
     while ( p->Peak ){   
       struct PeakLabel *next = p->Peak->Next;
       free( p->Peak );
       p->Peak = next;
     }

     while ( p->NumPeak ){   
       struct NumPeakLabel *next = p->NumPeak->Next;
       free( p->NumPeak );
       p->NumPeak = next;
     }
   }    
 }  

void KillPeakLabels( struct TrackPlot* p ){

   if( p ){
     while ( p->Peak ){   
       struct PeakLabel *next = p->Peak->Next;
       free( p->Peak );
       p->Peak = next;
     }
   }
}

void AddPeakLabel ( struct TrackPlot* p , float Channel ){

  struct PeakLabel* pk;
  
  if ( p->Peak ){
      pk = p->Peak;
      while( pk->Next ) pk = pk->Next;
      pk->Next = (struct PeakLabel *) calloc( 1, sizeof(struct PeakLabel) );
      pk->Next->Channel = Channel;
  }
  else {
      p->Peak = (struct PeakLabel *) calloc( 1, sizeof(struct PeakLabel) );
      p->Peak->Channel = Channel;
  } 
 }    

void AddNumPeakLabel ( struct TrackPlot* p , float Channel, int N, Int32 LabelColor ){

  struct NumPeakLabel* pk;
  
  if ( p->NumPeak ){
      pk = p->NumPeak;
      while( pk->Next ) pk = pk->Next;
      pk->Next = (struct NumPeakLabel *) calloc( 1, sizeof(struct NumPeakLabel) );
      pk->Next->Channel = Channel;
      pk->Next->N = N;
      pk->Next->LabelColor = LabelColor;
  }
  else {
      p->NumPeak = (struct NumPeakLabel *) calloc( 1, sizeof(struct NumPeakLabel) );
      p->NumPeak->Channel = Channel;
      p->NumPeak->N = N;
      p->NumPeak->LabelColor = LabelColor;
  } 
 }    
  

/*void MapGLWcolors(void){

 Int16 k1,k2,k3;
 Colorindex i;
 
 mapcolor(GLW_FRAMECOLOR ,112,128,144);
 mapcolor(GLW_FRAMECOLOR_DARK,47,79,79);
 mapcolor(GLW_FRAMECOLOR_LIGHT,119,140,157);
 mapcolor(GLW_LABELCOLOR_1,119,144,173);
 mapcolor(GLW_LABELCOLOR_2,119,164,193); 
 }
*/
void ClosestColor ( Int16 *r, Int16 *g, Int16 *b){

 Int16 rd,gd,bd;
 Int16 RedB=0,GreenB=0,BlueB=255;
 XColor c[256];
 int chq,Schq;
 int ii,jj,kk;

 if( usePCMAP() ) return;

#define DD (Display *) getXdpy()
#define DoRGB               (DoRed|DoGreen|DoBlue)
 if(DefaultDepth(DD,DefaultScreen(DD)) < 8)return; 
 if(DefaultDepth(DD,DefaultScreen(DD)) > 16)return; 
 c[0].red = (*r)<<8; c[0].green=(*g)<<8; c[0].blue=(*b)<<8; c[0].flags = DoRGB;
 if(XAllocColor(DD,DefaultColormap(DD,DefaultScreen(DD)),&c[0]))return;

 kk = 1; kk <<=DefaultDepth(DD,DefaultScreen(DD));
 kk /=256;
 
 
 Schq = 200000;
 for( jj = 0; jj<kk; jj++){
 for( ii = 0; ii < 256; ii++){
    c[ii].pixel = ii+jj*256;
    c[ii].flags = 0;
    }
 XQueryColors(DD,DefaultColormap(DD,DefaultScreen(DD)),&c[0],256);
 for( ii = 0; ii < 256; ii++){
    if( c[ii].flags == DoRGB ){
          rd = (c[ii].red>>8)  &0x00ff; rd = ( rd < *r )?(*r - rd):(rd - *r);
	  gd = (c[ii].green>>8)&0x00ff; gd = ( gd < *g )?(*g - gd):(gd - *g);
	  bd = (c[ii].blue>>8) &0x00ff; bd = ( bd < *b )?(*b - bd):(bd - *b);
	  chq = (rd  > gd)? rd:gd;
	  chq = (chq > bd)?chq:bd;
	  if( Schq > chq ){
	   if(XAllocColor(DD,DefaultColormap(DD,DefaultScreen(DD)),&c[ii])) {
	    RedB   = (c[ii].red>>8)  &0x00ff;
	    GreenB = (c[ii].green>>8)&0x00ff;
	    BlueB  = (c[ii].blue>>8) &0x00ff;
	    Schq = chq;
	    }
	   }
	  }
       }}
  *r = RedB; *g = GreenB; *b = BlueB; 
#undef DD
#undef DoRGB
} 

static unsigned long GetFramePixel(Display *dpy, Window win, Int16 rr, Int16 gg, Int16 bb) {
  XWindowAttributes wa;
  if (dpy && win && XGetWindowAttributes(dpy, win, &wa)) {
    XColor xc;
    xc.red   = (unsigned short)(rr << 8);
    xc.green = (unsigned short)(gg << 8);
    xc.blue  = (unsigned short)(bb << 8);
    xc.flags = DoRed | DoGreen | DoBlue;
    if (XAllocColor(dpy, wa.colormap, &xc)) {
      return xc.pixel;
    }
  }
  return (dpy ? BlackPixel(dpy, DefaultScreen(dpy)) : 0);
}

void MapGLWcolors(void){

 Int16 rr,gg,bb;
 Display *dpy = (Display *)getXdpy();
 Window win = (Window)getXwid();
 
 rr=112; gg=128; bb=144;
 ClosestColor( &rr, &gg, &bb);  
 mapcolor(GLW_FRAMECOLOR ,rr, gg, bb);

 if (dpy && win) {
   _global_FramePixel = getXpixel(GLW_FRAMECOLOR);
   if (_global_FramePixel == 0) {
     _global_FramePixel = GetFramePixel(dpy, win, rr, gg, bb);
   }
   XSetWindowAttributes xwa;
   xwa.bit_gravity = NorthWestGravity;
   xwa.background_pixel = _global_FramePixel;
   XChangeWindowAttributes(dpy, win, CWBitGravity | CWBackPixel, &xwa);
   XSetWindowBackground(dpy, win, _global_FramePixel);
 }

 rr=47; gg=79; bb=79;
 ClosestColor( &rr, &gg, &bb);  
 mapcolor(GLW_FRAMECOLOR_DARK,rr,gg,bb);

 rr=119; gg=140; bb=157;
 ClosestColor( &rr, &gg, &bb);  
 mapcolor(GLW_FRAMECOLOR_LIGHT,rr,gg,bb);

 rr=119; gg=144; bb=173;
 ClosestColor( &rr, &gg, &bb);  
 mapcolor(GLW_LABELCOLOR_1,rr,gg,bb);
 
 rr=119; gg=164; bb=193;
 ClosestColor( &rr, &gg, &bb);  
 mapcolor(GLW_LABELCOLOR_2,rr,gg,bb); 
 }


void LoadGLWfont(void){

  Int32 startH, FontSize;
  char FontName[80];
  
  startH = XDisplayHeight( (Display *)getXdpy(), DefaultScreen((Display *)getXdpy()) ); 
  
   GLW_FRAMEWIDTH = 0.020 *(float) startH + 62.00;
   GLW_FRAMEWIDTH += GLW_FRAMEWIDTH%2;
   FontSize = (float)startH * 0.0577 + 45.0;
   FontSize -= FontSize%20 -20;
   if( FontSize > 180 ) FontSize = 180;
   if( FontSize == 160 ) FontSize = 140;
  
   sprintf( FontName, "-*-charter-medium-r-*-*-0-%.2d-0-0-p-0-iso8859-1", FontSize-20); 
   loadXfont(GLW_FONTID12, FontName );

   sprintf( FontName, "-*-charter-medium-r-*-*-0-%.2d-0-0-p-0-iso8859-1", FontSize); 
   loadXfont(GLW_FONTID14, FontName );

/*
  if ( startW >= 3840 && startW < 3*startH ) { 
	GLW_FRAMEWIDTH = 90;
	loadXfont(GLW_FONTID12, "-*-times-medium-r-*-*-24-120-*-*-*-*-iso8859-1");
    loadXfont(GLW_FONTID14, "-*-times-medium-r-*-*-24-140-*-*-*-*-iso8859-1");
  }

  else {
	loadXfont(GLW_FONTID12, "-*-times-medium-r-*-*-12-120-*-*-*-*-iso8859-1");
	loadXfont(GLW_FONTID14, "-*-times-medium-r-*-*-14-140-*-*-*-*-iso8859-1");
  }
*/

/*
  loadXfont(GLW_FONTID12, "-*-helvetica-medium-r-*-*-10-100-*-*-*-*-iso8859-1");
  loadXfont(GLW_FONTID14, "-*-helvetica-bold-r-*-*-12-120-*-*-*-*-iso8859-1");
*/

  font(GLW_FONTID14);
 }
 

void MapDisplayWindow(struct DisplayWindow *dw){

 float x,y;
 Int32 xWp,yWp;

 getsize(&xWp,&yWp);
 
 x=GLW_FRAMEWIDTH+5; y=GLW_FRAMEWIDTH+5;
 
 (*dw).x1=x; (*dw).x2=(float)xWp-x;
 (*dw).y1=y; (*dw).y2=(float)yWp-y;
  
 }

void SetMouseShape(unsigned int shape){

  static Cursor cursor_left = None;
  static Cursor cursor_cross = None;
  static Cursor cursor_arrow = None;
  static unsigned int CrtShape = 0;
  Display *dpy = (Display *)getXdpy();
  Window win = (Window)getXwid();
  Cursor target = None;

  if (dpy == NULL || win == 0) return;
  if (shape == CrtShape) return;

  if (shape == XC_left_ptr) {
    if (cursor_left == None) {
      XColor bg = {0, 0, 0, 0, 0, 0};
      XColor fg = {0, 0xcfff, 0xdfff, 0xffff, 0, 0};
      cursor_left = XCreateFontCursor(dpy, XC_left_ptr);
      XRecolorCursor(dpy, cursor_left, &fg, &bg);
    }
    target = cursor_left;
  } else if (shape == XC_crosshair) {
    if (cursor_cross == None) {
      XColor bg = {0, 0, 0, 0, 0, 0};
      XColor fg = {0, 0xcfff, 0xdfff, 0xffff, 0, 0};
      cursor_cross = XCreateFontCursor(dpy, XC_crosshair);
      XRecolorCursor(dpy, cursor_cross, &fg, &bg);
    }
    target = cursor_cross;
  } else if (shape == XC_double_arrow) {
    if (cursor_arrow == None) {
      XColor bg = {0, 0, 0, 0, 0, 0};
      XColor fg = {0, 0xcfff, 0xdfff, 0xffff, 0, 0};
      cursor_arrow = XCreateFontCursor(dpy, XC_double_arrow);
      XRecolorCursor(dpy, cursor_arrow, &fg, &bg);
    }
    target = cursor_arrow;
  } else {
    target = XCreateFontCursor(dpy, shape);
  }

  if (target != None) {
    XDefineCursor(dpy, win, target);
    CrtShape = shape;
  }
}
  

int MouseInDisplayWindow(struct DisplayWindow *dw){

 Int32 x,y;
 
  MapDisplayWindow(dw);
  getorigin(&x,&y);
  x=getvaluator(MOUSEX)-x;
  y=getvaluator(MOUSEY)-y;

 
  if( (x>=(*dw).x1)&&(x<=(*dw).x2)&&(y>=(*dw).y1)&&(y<=(*dw).y2) ){
    if(getbutton(MIDDLEMOUSE))SetMouseShape(XC_double_arrow);
    else SetMouseShape(XC_crosshair);
    return 1;
    }
  else {
    SetMouseShape(XC_left_ptr);
    return 0;
    }
 }



struct TrackPlot *MouseInWhichPlot(struct DisplayWindow *dw){

  struct TrackPlot *p;
  float x,y;
  Int32 xWp,yWp, xW0,yW0;
  
  
  if(!MouseInDisplayWindow(dw))return NULL;
  if( (p=Plot) == NULL)return p;
  
  getsize(&xWp,&yWp);
  getorigin(&xW0,&yW0);
  
  x=GLW_MINXSIZE*(getvaluator(MOUSEX)-xW0); x/=xWp;
  y=GLW_MINYSIZE*(getvaluator(MOUSEY)-yW0); y/=yWp;
  
  while( (x<p->x1) || (x>p->x2) || (y<p->y1) || (y>p->y2) ){
     p=p->Next; if(p==NULL)return p;
     }
  return p;
 }
  


int MouseInLabel( struct LabelInFrame l){

  float x,y;
  Int32 xWp,yWp, xW0,yW0;
  
  
  
  getsize(&xWp,&yWp);
  getorigin(&xW0,&yW0);
  
  x=GLW_MINXSIZE*(getvaluator(MOUSEX)-xW0); x/=xWp;
  y=GLW_MINYSIZE*(getvaluator(MOUSEY)-yW0); y/=yWp;
  
  if( (x>l.x1) && (x<l.x2) && (y>l.y1) && (y<l.y2) ) return 1;
  else return 0;
 }


int MouseInButton( struct ButtonInFrame l){

  float x,y;
  Int32 xWp,yWp, xW0,yW0;
  
  
  
  getsize(&xWp,&yWp);
  getorigin(&xW0,&yW0);
  
  x=GLW_MINXSIZE*(getvaluator(MOUSEX)-xW0); x/=xWp;
  y=GLW_MINYSIZE*(getvaluator(MOUSEY)-yW0); y/=yWp;
  
  if( (x>l.x1) && (x<l.x2) && (y>l.y1) && (y<l.y2) ){
   l.Push=1; DrawButton(l); sleep(0);
   return 1;
   }
  else return 0;
 }



struct DataValues *Values(struct DisplayWindow *dw){
  
  struct TrackPlot *p;
  float x,y;
  Int32 xWp,yWp, xW0,yW0;
  int ch, max_ch;
  
  static struct DataValues v;
  
  if( (p=MouseInWhichPlot(dw)) == NULL )return NULL;
  if( p->data[p->ActiveData] == NULL)return NULL;
  getsize(&xWp,&yWp);
  getorigin(&xW0,&yW0);
  
  x=GLW_MINXSIZE*(getvaluator(MOUSEX)-xW0); x/=xWp;
  y=GLW_MINYSIZE*(getvaluator(MOUSEY)-yW0); y/=yWp;


  x-=p->x1; x/=(p->x2-p->x1);
  x*=p->Xmax[p->ActiveData]-p->Xmin[p->ActiveData]+1.000;
  x+=p->Xmin[p->ActiveData];


  y-=p->y1; y/=(p->y2-p->y1);
  y*=p->Ymax-p->Ymin; y+=p->Ymin;

  ch = (int)x;
  max_ch = p->Np[p->ActiveData] - 1;
  if (ch < 0) ch = 0;
  if (max_ch >= 0 && ch > max_ch) ch = max_ch;

  v.Channel = ch;
  v.Energy = x;
  v.Counts = (max_ch >= 0) ? p->data[p->ActiveData][ch] : 0.0f;
  v.Y = y;
  v.Plot = p;
  return &v;
 }



void DrawLabel(LabelInFrame L){

 float xf1,xf2,yf1,yf2,pixelsizeX,pixelsizeY;
 Coord LabelBorder[4][2];

 color(L.bgcolor);
 pixelsizeX=2.0000000000000*_global_px;
 pixelsizeY=2.0000000000000*_global_py;

 xf1 = L.x1+pixelsizeX;  xf2 = L.x2-pixelsizeX;
 yf1 = L.y1+pixelsizeY;  yf2 = L.y2-pixelsizeY;
 rectf(xf1,yf1,xf2,yf2);

 xf1=L.x1; xf2=L.x2; yf1=L.y1; yf2=L.y2;
 
 color(GLW_FRAMECOLOR_LIGHT);

 LabelBorder[0][0]=xf1;             LabelBorder[0][1]=yf1;
 LabelBorder[1][0]=xf1+pixelsizeX;   LabelBorder[1][1]=yf1+pixelsizeY;
 LabelBorder[2][0]=xf2-pixelsizeX;   LabelBorder[2][1]=yf1+pixelsizeY;
 LabelBorder[3][0]=xf2;             LabelBorder[3][1]=yf1;
 polf2(4,LabelBorder); 

 LabelBorder[0][0]=xf2;             LabelBorder[0][1]=yf1;
 LabelBorder[1][0]=xf2-pixelsizeX;   LabelBorder[1][1]=yf1+pixelsizeY;
 LabelBorder[2][0]=xf2-pixelsizeX;   LabelBorder[2][1]=yf2-pixelsizeY;
 LabelBorder[3][0]=xf2;             LabelBorder[3][1]=yf2;
 polf2(4,LabelBorder); 

 color(GLW_FRAMECOLOR_DARK);

 LabelBorder[0][0]=xf1;             LabelBorder[0][1]=yf2;
 LabelBorder[1][0]=xf1+pixelsizeX;   LabelBorder[1][1]=yf2-pixelsizeY;
 LabelBorder[2][0]=xf2-pixelsizeX;   LabelBorder[2][1]=yf2-pixelsizeY;
 LabelBorder[3][0]=xf2;             LabelBorder[3][1]=yf2;
 polf2(4,LabelBorder); 

 LabelBorder[0][0]=xf1;             LabelBorder[0][1]=yf1;
 LabelBorder[1][0]=xf1+pixelsizeX;   LabelBorder[1][1]=yf1+pixelsizeY;
 LabelBorder[2][0]=xf1+pixelsizeX;   LabelBorder[2][1]=yf2-pixelsizeY;
 LabelBorder[3][0]=xf1;             LabelBorder[3][1]=yf2;
 polf2(4,LabelBorder); 
 
 }
 
 
 
void WriteInLabel(LabelInFrame L){

 float xf1,xf2,yf1,yf2,pixelsizeX,pixelsizeY,height,width;

 if (L.Text == NULL) return;

 pixelsizeX = _global_px;
 pixelsizeY = _global_py;

 xf1 = L.x1+3.0*pixelsizeX;  xf2 = L.x2-3.0*pixelsizeX;
 yf1 = L.y1+3.0*pixelsizeY;  yf2 = L.y2-3.0*pixelsizeY;

 height=(yf2-yf1-(getheight()-getdescender()-2)*pixelsizeY)/2.0;
 width=(xf2-xf1-strwidth(L.Text)*pixelsizeX)/2.0;

 backbuffer(1);
 color(L.bgcolor);
 rectf(xf1,yf1,xf2,yf2);
 color(L.fgcolor);
 cmov2(xf1+width, yf1+height);
 charstr(L.Text);

 if (!_in_redraw_window) {
   frontbuffer(1);
   color(L.bgcolor);
   rectf(xf1,yf1,xf2,yf2);
   color(L.fgcolor);
   cmov2(xf1+width, yf1+height);
   charstr(L.Text);
   backbuffer(1);
   gflush();
 }
 }
 

void DrawButton(ButtonInFrame L){

 float xf1,xf2,yf1,yf2,pixelsizeX,pixelsizeY,height,width;
 Int32 n;
 Coord LabelBorder[4][2];

 if (L.Text == NULL) return;

 color(L.bgcolor);
 pixelsizeX = _global_px;
 pixelsizeY = _global_py;

 n=L.x1/pixelsizeX+1; L.x1= pixelsizeX*n;
 n=L.x2/pixelsizeX; L.x2= pixelsizeX*n;
 n=L.y1/pixelsizeY; L.y1= pixelsizeY*n;
 n=L.y2/pixelsizeY+1; L.y2= pixelsizeY*n;

 pixelsizeX*=2.0000; pixelsizeY*=2.000000;

 xf1 = L.x1+pixelsizeX;  xf2 = L.x2-pixelsizeX;
 yf1 = L.y1+pixelsizeY;  yf2 = L.y2-pixelsizeY;
 rectf(xf1,yf1,xf2,yf2);

 xf1=L.x1; xf2=L.x2; yf1=L.y1; yf2=L.y2;
 
 color(GLW_FRAMECOLOR_DARK);

 LabelBorder[0][0]=xf1;             LabelBorder[0][1]=yf1;
 LabelBorder[1][0]=xf1+pixelsizeX;   LabelBorder[1][1]=yf1+pixelsizeY;
 LabelBorder[2][0]=xf2-pixelsizeX;   LabelBorder[2][1]=yf1+pixelsizeY;
 LabelBorder[3][0]=xf2;             LabelBorder[3][1]=yf1;
 polf2(4,LabelBorder); 

 LabelBorder[0][0]=xf2;             LabelBorder[0][1]=yf1;
 LabelBorder[1][0]=xf2-pixelsizeX;   LabelBorder[1][1]=yf1+pixelsizeY;
 LabelBorder[2][0]=xf2-pixelsizeX;   LabelBorder[2][1]=yf2-pixelsizeY;
 LabelBorder[3][0]=xf2;             LabelBorder[3][1]=yf2;
 polf2(4,LabelBorder); 

 color(GLW_LABELCOLOR_2);

 LabelBorder[0][0]=xf1;             LabelBorder[0][1]=yf2;
 LabelBorder[1][0]=xf1+pixelsizeX;   LabelBorder[1][1]=yf2-pixelsizeY;
 LabelBorder[2][0]=xf2-pixelsizeX;   LabelBorder[2][1]=yf2-pixelsizeY;
 LabelBorder[3][0]=xf2;             LabelBorder[3][1]=yf2;
 polf2(4,LabelBorder); 

 LabelBorder[0][0]=xf1;             LabelBorder[0][1]=yf1;
 LabelBorder[1][0]=xf1+pixelsizeX;   LabelBorder[1][1]=yf1+pixelsizeY;
 LabelBorder[2][0]=xf1+pixelsizeX;   LabelBorder[2][1]=yf2-pixelsizeY;
 LabelBorder[3][0]=xf1;             LabelBorder[3][1]=yf2;
 polf2(4,LabelBorder); 
 
 pixelsizeX = _global_px;
 pixelsizeY = _global_py;

 xf1 = L.x1+2.0*pixelsizeX;  xf2 = L.x2-2.0*pixelsizeX;
 yf1 = L.y1+2.0*pixelsizeY;  yf2 = L.y2-2.0*pixelsizeY;
 height=(yf2-yf1-(getheight()-getdescender()-2)*pixelsizeY)/2.0;
 width=(xf2-xf1-strwidth(L.Text)*pixelsizeX)/2.0;

 color(GLW_FRAMECOLOR_DARK);
 move2(L.x1,L.y1); draw2(L.x1,L.y2);


 if (L.Push){
    color(L.pushcolor);

    rectf(xf1,yf1,xf2,yf2);

    color(GLW_FRAMECOLOR_DARK);
    move2(L.x1,L.y2); draw2(L.x2,L.y2);

    color(L.fgcolor);

    xf1 += width; 
    yf1 += height; 
 
    cmov2(xf1,yf1);
    charstr(L.Text);

    xf1 -= width; 
    yf1 -= height; 
    L.Push=0;
    while( getbutton(LEFTMOUSE) || getbutton(MENUBUTTON) ) usleep(10000);
   }
 color(L.bgcolor);

 rectf(xf1,yf1,xf2,yf2);

 color(GLW_FRAMECOLOR_LIGHT);
 move2(L.x1,L.y2); draw2(L.x2,L.y2);
 
 color(L.fgcolor);

 xf1 += width; 
 yf1 += height; 
 
 cmov2(xf1,yf1);
 charstr(L.Text);
 

 }





void DrawFrame(void){

 Coord FrameBorder[4][2];
 Int32 x1Wp, x2Wp, y1Wp, y2Wp;
 float x1,x2,y1,y2,pixelsizeX,pixelsizeY;
 

 pixelsizeX = _global_px;
 pixelsizeY = _global_py;

 x1Wp=0; y1Wp=0;
 x2Wp=GLW_MINXSIZE; y2Wp=GLW_MINYSIZE;
 x1=GLW_FRAMEWIDTH*pixelsizeX; y1=GLW_FRAMEWIDTH*pixelsizeY;
 x2=(GLW_FRAMEWIDTH-2)*pixelsizeX; y2=(GLW_FRAMEWIDTH-2)*pixelsizeY;

 color(GLW_DRAWBG);
 rectf((Coord)x1Wp+x2,(Coord)y1Wp+y2,(Coord)x2Wp-x2,(Coord)y1Wp+y1+5);
 rectf((Coord)x1Wp+x2,(Coord)y2Wp-y2,(Coord)x2Wp-x2,(Coord)y2Wp-y1-5);
 rectf((Coord)x1Wp+x2,(Coord)y1Wp+y1,(Coord)x1Wp+x1+5,(Coord)y2Wp-y2);
 rectf((Coord)x2Wp-x2,(Coord)y1Wp+y1,(Coord)x2Wp-x1-5,(Coord)y2Wp-y2);

 
 color(GLW_FRAMECOLOR);
 rectf((Coord)x1Wp-1,(Coord)y1Wp-1,(Coord)x1Wp+x1-1,(Coord)y2Wp+1);
 rectf((Coord)x2Wp+1,(Coord)y1Wp-1,(Coord)x2Wp-x1+1,(Coord)y2Wp+1);
 rectf((Coord)x1Wp-1,(Coord)y1Wp-1,(Coord)x2Wp+1,(Coord)y1Wp+y1-1);
 rectf((Coord)x1Wp-1,(Coord)y2Wp+1,(Coord)x2Wp+1,(Coord)y2Wp-y1+1);
 /*sleep(0); XSync((Display *)getXdpy(),False);*/
 

 color(GLW_LABELCOLOR_1);
 FrameBorder[0][0]=(float)x1Wp+x2; FrameBorder[0][1]=(float)y1Wp+y2;
 FrameBorder[1][0]=(float)x1Wp+x1;  FrameBorder[1][1]=(float)y1Wp+y1;
 FrameBorder[2][0]=(float)x2Wp-x1;  FrameBorder[2][1]=(float)y1Wp+y1;
 FrameBorder[3][0]=(float)x2Wp-x2; FrameBorder[3][1]=(float)y1Wp+y2;
 polf2(4,FrameBorder);
 FrameBorder[0][0]=(float)x2Wp-x2; FrameBorder[0][1]=(float)y1Wp+y2;
 FrameBorder[1][0]=(float)x2Wp-x1;  FrameBorder[1][1]=(float)y1Wp+y1;
 FrameBorder[2][0]=(float)x2Wp-x1;  FrameBorder[2][1]=(float)y2Wp-y1;
 FrameBorder[3][0]=(float)x2Wp-x2; FrameBorder[3][1]=(float)y2Wp-y2;
 polf2(4,FrameBorder);
 
 color(GLW_FRAMECOLOR_DARK);
 FrameBorder[0][0]=(float)x1Wp+x2; FrameBorder[0][1]=(float)y2Wp-y2;
 FrameBorder[1][0]=(float)x1Wp+x1;  FrameBorder[1][1]=(float)y2Wp-y1;
 FrameBorder[2][0]=(float)x2Wp-x1;  FrameBorder[2][1]=(float)y2Wp-y1;
 FrameBorder[3][0]=(float)x2Wp-x2; FrameBorder[3][1]=(float)y2Wp-y2;
 polf2(4,FrameBorder);
 FrameBorder[0][0]=(float)x1Wp+x2; FrameBorder[0][1]=(float)y1Wp+y2;
 FrameBorder[1][0]=(float)x1Wp+x1;  FrameBorder[1][1]=(float)y1Wp+y1;
 FrameBorder[2][0]=(float)x1Wp+x1;  FrameBorder[2][1]=(float)y2Wp-y1;
 FrameBorder[3][0]=(float)x1Wp+x2; FrameBorder[3][1]=(float)y2Wp-y2;
 polf2(4,FrameBorder);
 
 MapDisplayWindow(GLWindow);
 	
  }


void DrawTrackFrame(Int32 win){

 Int32 x1Wp, x2Wp, y1Wp, y2Wp;
 float x,y1,y2,pixelsizeX,pixelsizeY;
 
 winset(win);
 getsize(&x1Wp,&y1Wp);
 if (x1Wp > 0 && y1Wp > 0) {
   _global_px = (double)GLW_MINXSIZE / (double)x1Wp;
   _global_py = (double)GLW_MINYSIZE / (double)y1Wp;
 }

 pixelsizeX = _global_px;
 pixelsizeY = _global_py;
 
                     
 x1Wp=0; y1Wp=0;
 x2Wp=GLW_MINXSIZE; y2Wp=GLW_MINYSIZE;
 x=x2Wp;
 
 y1=GLW_FRAMEWIDTH*pixelsizeY/8.0;
 y2=4*y1;
  

 DrawFrame();
 /*sleep(0); XSync((Display *)getXdpy(),False);*/

 TrackXMinValue.x1=x1Wp+x/18.0;
 TrackXMinValue.y1=y2Wp-y2;
 TrackXMinValue.x2=x1Wp+x/18.0*5.0;
 TrackXMinValue.y2=y2Wp-y1;
 TrackXMinValue.bgcolor=GLW_LABELCOLOR_1;
 TrackXMinValue.fgcolor=BLACK;
 if(TrackXMinValue.Text == NULL)TrackXMinValue.Text=(char *)calloc(20,sizeof(char));
 DrawLabel(TrackXMinValue);
 
 TrackXMaxValue.x1=x1Wp+5.0*x/18.0;
 TrackXMaxValue.y1=y2Wp-y2;
 TrackXMaxValue.x2=x1Wp+x/18.0*9.0;
 TrackXMaxValue.y2=y2Wp-y1;
 TrackXMaxValue.bgcolor=GLW_LABELCOLOR_1;
 TrackXMaxValue.fgcolor=BLACK;
 if(TrackXMaxValue.Text == NULL)TrackXMaxValue.Text=(char *)calloc(20,sizeof(char));
 DrawLabel(TrackXMaxValue); 

 TrackYMinValue.x1=x1Wp+x/18.0*9.0;
 TrackYMinValue.y1=y2Wp-y2;
 TrackYMinValue.x2=x1Wp+x/18.0*13.0;
 TrackYMinValue.y2=y2Wp-y1;
 TrackYMinValue.bgcolor=GLW_LABELCOLOR_1;
 TrackYMinValue.fgcolor=BLACK;
 if(TrackYMinValue.Text == NULL)TrackYMinValue.Text=(char *)calloc(20,sizeof(char));
 DrawLabel(TrackYMinValue);

 TrackYMaxValue.x1=x1Wp+x/18.0*13.0;
 TrackYMaxValue.y1=y2Wp-y2;
 TrackYMaxValue.x2=x1Wp+x/18.0*17.0;
 TrackYMaxValue.y2=y2Wp-y1;
 TrackYMaxValue.bgcolor=GLW_LABELCOLOR_1;
 TrackYMaxValue.fgcolor=BLACK;
 if(TrackYMaxValue.Text == NULL)TrackYMaxValue.Text=(char *)calloc(20,sizeof(char));
 DrawLabel(TrackYMaxValue); 
 /*XSync((Display *)getXdpy(),False);*/


 y2=7*y1 ; y1*=4;

 TrackChannelValue.x1=x1Wp+x/18.0;
 TrackChannelValue.y1=y2Wp-y2;
 TrackChannelValue.x2=x1Wp+x/18.0*5.0;
 TrackChannelValue.y2=y2Wp-y1;
 TrackChannelValue.bgcolor=GLW_LABELCOLOR_1;
 TrackChannelValue.fgcolor=BLACK;
 if(TrackChannelValue.Text == NULL)TrackChannelValue.Text=(char *)calloc(20,sizeof(char));
 DrawLabel(TrackChannelValue); 

 TrackEnergyValue.x1=x1Wp+5.0*x/18.0;
 TrackEnergyValue.y1=y2Wp-y2;
 TrackEnergyValue.x2=x1Wp+x/18.0*9.0;
 TrackEnergyValue.y2=y2Wp-y1;
 TrackEnergyValue.bgcolor=GLW_LABELCOLOR_1;
 TrackEnergyValue.fgcolor=BLACK;
 if(TrackEnergyValue.Text == NULL)TrackEnergyValue.Text=(char *)calloc(20,sizeof(char));
 DrawLabel(TrackEnergyValue);
 
 TrackCountsValue.x1=x1Wp+x/18.0*9.0;
 TrackCountsValue.y1=y2Wp-y2;
 TrackCountsValue.x2=x1Wp+x/18.0*13.0;
 TrackCountsValue.y2=y2Wp-y1;
 TrackCountsValue.bgcolor=GLW_LABELCOLOR_1;
 TrackCountsValue.fgcolor=BLACK;
 if(TrackCountsValue.Text == NULL)TrackCountsValue.Text=(char *)calloc(20,sizeof(char));
 DrawLabel(TrackCountsValue); 

 TrackCursorYValue.x1=x1Wp+x/18.0*13.0;
 TrackCursorYValue.y1=y2Wp-y2;
 TrackCursorYValue.x2=x1Wp+x/18.0*17.0;
 TrackCursorYValue.y2=y2Wp-y1;
 TrackCursorYValue.bgcolor=GLW_LABELCOLOR_1;
 TrackCursorYValue.fgcolor=BLACK;
 if(TrackCursorYValue.Text == NULL)TrackCursorYValue.Text=(char *)calloc(20,sizeof(char));
 DrawLabel(TrackCursorYValue);

  sprintf(TrackChannelValue.Text,"Channel ");
  sprintf(TrackEnergyValue.Text,"Energy ");
  sprintf(TrackCountsValue.Text,"Counts ");
  sprintf(TrackCursorYValue.Text,"Y ");
  WriteInLabel(TrackChannelValue);
  WriteInLabel(TrackEnergyValue);
  WriteInLabel(TrackCountsValue); 
  WriteInLabel(TrackCursorYValue); 


 y1=(float)GLW_FRAMEWIDTH*pixelsizeY/2.0;
 y2=7.0*y1/4.0;

 TrackDisplayed.x1=x1Wp+x/18.0*5.0+pixelsizeX;
 TrackDisplayed.y1=y1Wp+y1;
 TrackDisplayed.x2=x1Wp+x/18.0*17.0;
 TrackDisplayed.y2=y1Wp+y2;
 TrackDisplayed.bgcolor=GLW_LABELCOLOR_1;
 TrackDisplayed.fgcolor=BLACK;
 if(TrackDisplayed.Text == NULL) TrackDisplayed.Text="<none>";
 DrawLabel(TrackDisplayed); WriteInLabel(TrackDisplayed);

 TrackNewSpec.x1=x1Wp+x/18.0;
 TrackNewSpec.y1=y1Wp+y1;
 TrackNewSpec.x2=x1Wp+x/9.0;
 TrackNewSpec.y2=y1Wp+y2;
 TrackNewSpec.bgcolor=GLW_LABELCOLOR_1;
 TrackNewSpec.pushcolor=GLW_FRAMECOLOR;
 TrackNewSpec.Push=0;
 TrackNewSpec.fgcolor=CYAN;
 TrackNewSpec.Text="R";
 DrawButton(TrackNewSpec);

 TrackWriteSpec.x1=x1Wp+x/9.0;
 TrackWriteSpec.y1=y1Wp+y1;
 TrackWriteSpec.x2=x1Wp+x/6.0;
 TrackWriteSpec.y2=y1Wp+y2;
 TrackWriteSpec.bgcolor=GLW_LABELCOLOR_1;
 TrackWriteSpec.pushcolor=GLW_FRAMECOLOR;
 TrackWriteSpec.Push=0;
 TrackWriteSpec.fgcolor=CYAN;
 TrackWriteSpec.Text="W";
 DrawButton(TrackWriteSpec);


 TrackDecSpec.x1=x1Wp+x/6.0;
 TrackDecSpec.y1=y1Wp+y1;
 TrackDecSpec.x2=x1Wp+x/4.50;
 TrackDecSpec.y2=y1Wp+y2;
 TrackDecSpec.bgcolor=GLW_LABELCOLOR_1;
 TrackDecSpec.pushcolor=GLW_FRAMECOLOR;
 TrackDecSpec.Push=0;
 TrackDecSpec.fgcolor=CYAN;
 TrackDecSpec.Text="# -";
 DrawButton(TrackDecSpec);

 TrackIncSpec.x1=x1Wp+x/4.50;
 TrackIncSpec.y1=y1Wp+y1;
 TrackIncSpec.x2=x1Wp+x/18.0*5.0;
 TrackIncSpec.y2=y1Wp+y2;
 TrackIncSpec.bgcolor=GLW_LABELCOLOR_1;
 TrackIncSpec.pushcolor=GLW_FRAMECOLOR;
 TrackIncSpec.Push=0;
 TrackIncSpec.fgcolor=CYAN;
 TrackIncSpec.Text="# +";
 DrawButton(TrackIncSpec);


 y1=(float)GLW_FRAMEWIDTH*pixelsizeY/8.0;
 y2=4.0*y1;

 TrackPath.x1=x1Wp+x/18.0*5.0+pixelsizeX;
 TrackPath.y1=y1Wp+y1;
 TrackPath.x2=x1Wp+x/18.0*13.0;
 TrackPath.y2=y1Wp+y2;
 TrackPath.bgcolor=GLW_LABELCOLOR_1;
 TrackPath.fgcolor=CYAN;
 if(TrackPath.Text == NULL){
    TrackPath.Text=(char *)calloc(200,sizeof(char));    
    getcwd(TrackPath.Text,200);
    }
 DrawLabel(TrackPath); WriteInLabel(TrackPath);

 TrackOutFile.x1=x1Wp+x/18.0*13.0;
 TrackOutFile.y1=y1Wp+y1;
 TrackOutFile.x2=x1Wp+x/18.0*17.0;
 TrackOutFile.y2=y1Wp+y2;
 TrackOutFile.bgcolor=GLW_LABELCOLOR_1;
 TrackOutFile.fgcolor=BLACK;
 if(TrackOutFile.Text == NULL){
    TrackOutFile.Text=(char *)calloc(16,sizeof(char));
    sprintf(TrackOutFile.Text,"-> <none>");
    }
 DrawLabel(TrackOutFile); WriteInLabel(TrackOutFile);


 TrackOpenCM.x1=x1Wp+x/18.0;
 TrackOpenCM.y1=y1Wp+y1;
 TrackOpenCM.x2=x1Wp+x/6.0;
 TrackOpenCM.y2=y1Wp+y2;
 TrackOpenCM.bgcolor=GLW_LABELCOLOR_1;
 TrackOpenCM.pushcolor=GLW_FRAMECOLOR;
 TrackOpenCM.Push=0;
 TrackOpenCM.fgcolor=CYAN;
 TrackOpenCM.Text="Open CM";
 DrawButton(TrackOpenCM);

 TrackGateCM.x1=x1Wp+x/6.0;
 TrackGateCM.y1=y1Wp+y1;
 TrackGateCM.x2=x1Wp+x/18.0*5.0;
 TrackGateCM.y2=y1Wp+y2;
 TrackGateCM.bgcolor=GLW_LABELCOLOR_1;
 TrackGateCM.pushcolor=GLW_FRAMECOLOR;
 TrackGateCM.Push=0;
 TrackGateCM.fgcolor=CYAN;
 TrackGateCM.Text="Gate CM";
 DrawButton(TrackGateCM);

/* Left side buttons */
 x= (float) GLW_FRAMEWIDTH*pixelsizeX/12.000 ;
 y1= (float)(GLW_FRAMEWIDTH+5)*pixelsizeY ;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;

 TrackCal2P.x1=x1Wp+x;
 TrackCal2P.y1=y1Wp+y1;
 TrackCal2P.x2=x1Wp+11.00*x;
 TrackCal2P.y2=y1Wp+y2;
 TrackCal2P.bgcolor=GLW_LABELCOLOR_1;
 TrackCal2P.pushcolor=GLW_FRAMECOLOR;
 TrackCal2P.Push=0;
 TrackCal2P.fgcolor=CYAN;
 TrackCal2P.Text="Cal2P";
 DrawButton(TrackCal2P);

 y1=y2+ 2*pixelsizeY;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 TrackAutoTrace.x1=x1Wp+x;
 TrackAutoTrace.y1=y1Wp+y1;
 TrackAutoTrace.x2=x1Wp+11.00*x;
 TrackAutoTrace.y2=y1Wp+y2;
 TrackAutoTrace.bgcolor=GLW_LABELCOLOR_1;
 TrackAutoTrace.pushcolor=GLW_FRAMECOLOR;
 TrackAutoTrace.Push=0;
 TrackAutoTrace.fgcolor=CYAN;
 TrackAutoTrace.Text="DT";
 DrawButton(TrackAutoTrace);

 y1=y2+ 2*pixelsizeY;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 TrackEnCal.x1=x1Wp+x;
 TrackEnCal.y1=y1Wp+y1;
 TrackEnCal.x2=x1Wp+11.00*x;
 TrackEnCal.y2=y1Wp+y2;
 TrackEnCal.bgcolor=GLW_LABELCOLOR_1;
 TrackEnCal.pushcolor=GLW_FRAMECOLOR;
 TrackEnCal.Push=0;
 TrackEnCal.fgcolor=CYAN;
 TrackEnCal.Text="EnCal";
 DrawButton(TrackEnCal);


/* Right side buttons */
 x= (float) GLW_FRAMEWIDTH*pixelsizeX/8.000 ;
 y1= (float)(GLW_FRAMEWIDTH+5)*pixelsizeY ;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 
 TrackLeft.x1=x2Wp-7.0*x;
 TrackLeft.y1=y1Wp+y1;
 TrackLeft.x2=x2Wp-x*4.0;
 TrackLeft.y2=y1Wp+y2;
 TrackLeft.bgcolor=GLW_LABELCOLOR_1;
 TrackLeft.pushcolor=GLW_FRAMECOLOR;
 TrackLeft.Push=0;
 TrackLeft.fgcolor=CYAN;
 TrackLeft.Text="<";
 DrawButton(TrackLeft);

 TrackRight.x1=x2Wp-4.0*x;
 TrackRight.y1=y1Wp+y1;
 TrackRight.x2=x2Wp-x;
 TrackRight.y2=y1Wp+y2;
 TrackRight.bgcolor=GLW_LABELCOLOR_1;
 TrackRight.pushcolor=GLW_FRAMECOLOR;
 TrackRight.Push=0;
 TrackRight.fgcolor=CYAN;
 TrackRight.Text=">";
 DrawButton(TrackRight);

 y1=y2+ 2*pixelsizeY;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 TrackSameY.x1=x2Wp-7.0*x;
 TrackSameY.y1=y1Wp+y1;
 TrackSameY.x2=x2Wp-x;
 TrackSameY.y2=y1Wp+y2;
 TrackSameY.bgcolor=GLW_LABELCOLOR_1;
 TrackSameY.pushcolor=GLW_FRAMECOLOR;
 TrackSameY.Push=0;
 TrackSameY.fgcolor=CYAN;
 TrackSameY.Text="SY";
 DrawButton(TrackSameY);
 
 y1=y2+ 2*pixelsizeY;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 TrackSameX.x1=x2Wp-7.0*x;
 TrackSameX.y1=y1Wp+y1;
 TrackSameX.x2=x2Wp-x;
 TrackSameX.y2=y1Wp+y2;
 TrackSameX.bgcolor=GLW_LABELCOLOR_1;
 TrackSameX.pushcolor=GLW_FRAMECOLOR;
 TrackSameX.Push=0;
 TrackSameX.fgcolor=CYAN;
 TrackSameX.Text="SX";
 DrawButton(TrackSameX);
 
 y1=y2+ 2*pixelsizeY;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 TrackAutoY.x1=x2Wp-7.0*x;
 TrackAutoY.y1=y1Wp+y1;
 TrackAutoY.x2=x2Wp-x;
 TrackAutoY.y2=y1Wp+y2;
 TrackAutoY.bgcolor=GLW_LABELCOLOR_1;
 TrackAutoY.pushcolor=GLW_FRAMECOLOR;
 TrackAutoY.Push=0;
 TrackAutoY.fgcolor=CYAN;
 TrackAutoY.Text="FY";
 DrawButton(TrackAutoY);

 y1=y2+ 2*pixelsizeY;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 TrackAutoX.x1=x2Wp-7.0*x;
 TrackAutoX.y1=y1Wp+y1;
 TrackAutoX.x2=x2Wp-x;
 TrackAutoX.y2=y1Wp+y2;
 TrackAutoX.bgcolor=GLW_LABELCOLOR_1;
 TrackAutoX.pushcolor=GLW_FRAMECOLOR;
 TrackAutoX.Push=0;
 TrackAutoX.fgcolor=CYAN;
 TrackAutoX.Text="FX";
 DrawButton(TrackAutoX);

 y1=y2+ 2*pixelsizeY;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 TrackAutoXY.x1=x2Wp-7.0*x;
 TrackAutoXY.y1=y1Wp+y1;
 TrackAutoXY.x2=x2Wp-x;
 TrackAutoXY.y2=y1Wp+y2;
 TrackAutoXY.bgcolor=GLW_LABELCOLOR_1;
 TrackAutoXY.pushcolor=GLW_FRAMECOLOR;
 TrackAutoXY.Push=0;
 TrackAutoXY.fgcolor=CYAN;
 TrackAutoXY.Text="FF";
 DrawButton(TrackAutoXY);

 y1=y2+ 2*pixelsizeY;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 TrackLinLog.x1=x2Wp-7.0*x;
 TrackLinLog.y1=y1Wp+y1;
 TrackLinLog.x2=x2Wp-x;
 TrackLinLog.y2=y1Wp+y2;
 TrackLinLog.bgcolor=GLW_LABELCOLOR_1;
 TrackLinLog.pushcolor=GLW_FRAMECOLOR;
 TrackLinLog.Push=0;
 TrackLinLog.fgcolor=CYAN;
 TrackLinLog.Text="L";
 DrawButton(TrackLinLog);

 y1=y2+ 2*pixelsizeY;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 TrackRefresh.x1=x2Wp-7.0*x;
 TrackRefresh.y1=y1Wp+y1;
 TrackRefresh.x2=x2Wp-x;
 TrackRefresh.y2=y1Wp+y2;
 TrackRefresh.bgcolor=GLW_LABELCOLOR_1;
 TrackRefresh.pushcolor=GLW_FRAMECOLOR;
 TrackRefresh.Push=0;
 TrackRefresh.fgcolor=CYAN;
 TrackRefresh.Text="=";
 DrawButton(TrackRefresh);


 y1=y2+ 2*pixelsizeY;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 TrackDoPkS.x1=x2Wp-7.0*x;
 TrackDoPkS.y1=y1Wp+y1;
 TrackDoPkS.x2=x2Wp-x;
 TrackDoPkS.y2=y1Wp+y2;
 TrackDoPkS.bgcolor=GLW_LABELCOLOR_1;
 TrackDoPkS.pushcolor=GLW_FRAMECOLOR;
 TrackDoPkS.Push=0;
 TrackDoPkS.fgcolor=CYAN;
 TrackDoPkS.Text="PkS";
 DrawButton(TrackDoPkS);


 y1=y2+ 2*pixelsizeY;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 TrackDoFit.x1=x2Wp-7.0*x;
 TrackDoFit.y1=y1Wp+y1;
 TrackDoFit.x2=x2Wp-x;
 TrackDoFit.y2=y1Wp+y2;
 TrackDoFit.bgcolor=GLW_LABELCOLOR_1;
 TrackDoFit.pushcolor=GLW_FRAMECOLOR;
 TrackDoFit.Push=0;
 TrackDoFit.fgcolor=CYAN;
 TrackDoFit.Text="Fit";
 DrawButton(TrackDoFit);

 y1=y2+ 2*pixelsizeY;
 y2= y1+ (float)GLW_FRAMEWIDTH*pixelsizeY/3.0;
 TrackDoInt.x1=x2Wp-7.0*x;
 TrackDoInt.y1=y1Wp+y1;
 TrackDoInt.x2=x2Wp-x;
 TrackDoInt.y2=y1Wp+y2;
 TrackDoInt.bgcolor=GLW_LABELCOLOR_1;
 TrackDoInt.pushcolor=GLW_FRAMECOLOR;
 TrackDoInt.Push=0;
 TrackDoInt.fgcolor=CYAN;
 TrackDoInt.Text="Int";
 DrawButton(TrackDoInt);


}

Int32 GLWTrackInit(void){

  Int32 dev, startW, startH;


/*
  minsize(GLW_MINXSIZE,GLW_MINYSIZE);
*/

  ginit();
  
  startH = XDisplayHeight( (Display *)getXdpy(), DefaultScreen((Display *)getXdpy()) ); 
  startW = XDisplayWidth ( (Display *)getXdpy(), DefaultScreen((Display *)getXdpy()) ); 

  if( startW > 3*startH ) startW /= 2;   /* Most likely two monitors used to extend the screen */
  
  GLW_MINXSIZE = startW*RATIO_W;
  GLW_MINYSIZE = startH*RATIO_W;
  if( GLW_MINYSIZE < 560 ) GLW_MINYSIZE = 560;
  minsize(GLW_MINXSIZE,GLW_MINYSIZE);

  stepunit(20,20);
  dev=winopen("GASPware-->Track");
  SetMouseShape(XC_left_ptr);
  viewport(0,GLW_MINXSIZE-1,0,GLW_MINYSIZE-1);
  MapGLWcolors();
  LoadGLWfont();
  doublebuffer();
  gconfig();
  backbuffer(1);
  _global_LastW = GLW_MINXSIZE;
  _global_LastH = GLW_MINYSIZE;
  _global_px = 1.0;
  _global_py = 1.0;

  _in_redraw_window = 1;
  color(GLW_FRAMECOLOR);
  clear();
  DrawTrackFrame(dev);
  swapbuffers();
  gflush();
  _in_redraw_window = 0;

  TrackMenu=defpup("  WINDOWS  %t|Add Line|Add Column|Delete Line|Delete Column|Refresh Display");
  
  qdevice(MOUSEX);
  qdevice(MOUSEY); 
  qdevice(LEFTMOUSE);
  qdevice(MIDDLEMOUSE);
  qdevice(MENUBUTTON);
  qdevice(UPMOUSEWHEEL);
  qdevice(DOWNMOUSEWHEEL);
  qdevice(LEFTMOUSEWHEEL);
  qdevice(RIGHTMOUSEWHEEL);
  qdevice(LEFTARROWKEY);
  qdevice(RIGHTARROWKEY);
  qdevice(UPARROWKEY);
  qdevice(DOWNARROWKEY);
  qdevice(KEYBD);
  qdevice(REDRAW);
  qdevice(WINQUIT);
  qdevice(LEFTCTRLKEY);
  qdevice(RIGHTCTRLKEY);
  unqdevice(INPUTCHANGE);
  qreset();

  return dev;
 }

float Nintf( float x){
   return roundf(x);
}

void ReshapeWindow ( void ){
   Int32 x_wp, y_wp;
   reshapeviewport();
   getsize(&x_wp, &y_wp);
   if (x_wp > 0 && y_wp > 0) {
     _global_px = (double)GLW_MINXSIZE / (double)x_wp;
     _global_py = (double)GLW_MINYSIZE / (double)y_wp;
     if (Plot) {
       TrackPlotMap(Plot->Row, Plot->Col);
     }
   }
}
