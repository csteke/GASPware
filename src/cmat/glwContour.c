#define MXYSIZE 2048

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

#if !defined(__APPLE__)
#include <malloc.h>
#endif

#include <math.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/cursorfont.h>

#include <X11/Ygl.h>
#include <glwidget.h>

#define GLW_XSIZE 640
#define GLW_YSIZE 480
#define GLW_FRAMEWIDTH_LB 56
#define GLW_FRAMEWIDTH_RT 12
#define GLW_FRAMECOLOR 8
#define GLW_FRAMECOLOR_DARK 9
#define GLW_FRAMECOLOR_LIGHT 10
#define GLW_LABELCOLOR_1 11
#define GLW_LABELCOLOR_2 12
#define GLW_VISIBLECOLOR 29
#define GLW_DRAWBG BLACK
#define GLW_MARKERCOLOR GLW_LABELCOLOR_2
#define GLW_UBANANACOLOR CYAN
#define GLW_SBANANACOLOR GLW_VISIBLECOLOR

#define GLW_FONTID14 4711
#define GLW_FONTID12 4712

#define ZLIN 1
#define ZSQRT 2
#define ZCBRT 3
#define ZLOG 4
#define ZMIX 5
#define ZATAN 6
#define mixf(x) (3.00*atan(x)+log10(x))
/*#define FlushDrawings  qreset();sleep(0); XSync((Display *)getXdpy(),False) */
#define FlushDrawings  sleep(0); XSync((Display *)getXdpy(),False)

#define MOVE_UP 0
#define MOVE_DOWN 1
#define MOVE_LEFT 2
#define MOVE_RIGHT 3
#define MOVE_CENTER 4

typedef struct LabelInFrame {
	float x1,y1,x2,y2;
	int bgcolor;
	int fgcolor;
	Int32 *Reverse;
	char *Text;
	Coord TextX,TextY;
	}LabelInFrame;

typedef struct PlotFrame {
        Icoord x1,y1,x2,y2;
	} PlotFrame;
#define LineStruct struct PlotFrame
	
typedef struct PlotStruct {
    Int32 Xmin,Xmax;
	Int32 Ymin,Ymax;
	float Zmin,Zmax;
	Int32 LocalMinVal, LocalMaxVal;
	Int32 ProXMinVal, ProXMaxVal;
	Int32 ProYMinVal, ProYMaxVal;
	float *ProX;
	float *ProY;
	Int32 **data;
	Int32 **OriginalData;
	Int32 **MappedData;
	Int32 ZScaleType;
	Int32 Reverse; 
	Uint8 *Image;
	} PlotStruct;


typedef struct MarkerStruct {
        char Kindl;
	char KindL;
	char On;
	Int16 PixelPos;
	Int16 ChannelPos;
	Int16 Color;
	struct MarkerStruct *Next;
	} MarkerStruct;
#define LMARKERL 'L'
#define RMARKERL 'R'
#define UMARKERL 'U'
#define OMARKERL 'O'
#define LMARKERl 'l'
#define RMARKERl 'r'
#define UMARKERl 'u'
#define OMARKERl 'o'
#define MARKER_ON  1
#define MARKER_OFF 0

typedef struct CoordStruct {
        Int32 x;
	Int32 y;
	float z;
	} CoordStruct;


typedef struct BananaPoint {
        Int32 x;
	Int32 y;
	Int32 Visible;
	struct BananaPoint *Previous;
	struct BananaPoint *Next;
	} BananaPoint;
#define BPoint struct BananaPoint *

typedef struct BananaStruct {
        Int32 Active;
		Int32 Visible;
        Int32 Xl;
        Int32 Xr;
        Int32 Yb;
        Int32 Yt;
        double Surface;
        double Integral;
        double SX;
        double SY;
        double SX2;
        double SY2;
        double SXY;
	BPoint p;
	BPoint s;
	struct BananaStruct *Next;
	} BananaStruct;
#define BStruct struct BananaStruct *
	
static int _global_BS;
static Int32 DoQuit;
static Int32 DataXmax, DataYmax;
static Int32 XWp, YWp;
static struct LabelInFrame CursorXValue, CursorYValue, CursorZValue;
static struct LabelInFrame XMinLabel,  XMaxLabel, YMinLabel,  YMaxLabel, ZMinLabel,  ZMaxLabel;
static struct PlotFrame MatPlot, XpPlot, YpPlot;
static struct PlotStruct Plot;
static struct CoordStruct XYZ;
static Int32 Menu;
struct MarkerStruct *Marker = NULL;
float XChPix, YChPix;
struct BananaStruct *Banana = NULL, *Selected = NULL;
gl_slider *ZMinSlider;
gl_slider *ZMaxSlider;
static Int32 ZScaleChange;
double ZMinFactor, ZMaxFactor;

Colorindex ContColor[17]={ BLACK, 13,14,15,16,  17,18,19,20,  21,22,23,24,  25,26,27,28};

Int16 Red[16]  ={ 40, 63, 71, 63,   31, 15,  0,  0,    0,  0, 15, 63,   95,143,191,255};
Int16 Green[16]={ 40, 63, 73, 63,   31, 31, 63,127,  175,191,207,199,  215,239,239,255};
Int16 Blue[16] ={ 55, 80,107,127,  151,175,191,191,  127, 95, 95, 47,    0,  0,  0,  0};

Int32 Win;

/*************************************************************************/

static char _DB_Off = 0;
#define DOUBLEBUFF_ON  {backbuffer(1); _DB_Off = 0;}
#define DOUBLEBUFF_OFF {swapbuffers();frontbuffer(1); _DB_Off = 1;}


/************************************************************************/

void  contourplot_(Int32 *data, Int32 *resx, Int32 *resy);
void PrintHelp ( void );
/*  Functions */
void MapGLWcolors(void);
void LoadGLWfont(void);
void SetMouseShape(unsigned int shape);
void DrawLabel(LabelInFrame L);
void WriteInLabel(LabelInFrame L);
void ShowPosition ( void );
void ClearDrawArea ( void );
void DrawFrame ( void );
Int32 GLWContourInit(void);
void ReshapeWindow ( void );
void MapPlots( void );
void DrawPlotFrame ( struct PlotFrame P );
void ClosestColor ( Int16 *r, Int16 *g, Int16 *b);
void DrawColorScale( void );
struct MarkerStruct *CreateMarker ( char Kindl, char KindL, char Color );
void DrawMarker ( char Kind );
void RedrawAllMarkers ( void );
Int32 GetXYZ ( Int32 x, Int32 y);
void MovePlotRegion ( Int32 Where );
void ZScaleChangeCbk ( gl_slider *sl, double value);

/* Banana Processing funtions */
struct BananaStruct *BP_CreateBanana ( void );
void BP_DestroyBanana( void );
void BP_WriteBanana ( void );
BStruct BP_ReadBanana ( void );
struct CoordStruct *BP_PixelPosition ( Int32 x, Int32 y);
LineStruct *BP_LineBetween ( BPoint p1, BPoint p2 );
BPoint BP_ClosestPoint ( BStruct b );
BStruct BP_ClosestBanana ( void );
void BP_DrawPoint ( BPoint p );
void BP_DrawSBPoint ( BPoint p );
void BP_SelectPoint ( BPoint p );
void BP_UnselectPoint ( BPoint p );
Int32 BP_IsVisible( BStruct b);
void BP_DrawBanana( BStruct b );
void BP_DrawSBanana( BStruct b );
void BP_SelectBanana( BStruct b );
void BP_UnselectBanana( BStruct b );
void BP_AddPoint( void );
void BP_NewBanana( void );
void BP_MovePoint ( void );
void BP_DeletePoint ( void );
void BP_TypeBanana ( void );

double BP_Num_P ( BStruct b );
double BP_Sum_X ( BStruct b );
double BP_Sum_Y ( BStruct b );
double BP_Sum_X2( BStruct b );
double BP_Sum_XY( BStruct b );
int BP_Limits( BStruct b );
int BP_Sums  ( BStruct b );

Int32 BP_IsPointInside( BStruct b, Int32 x, Int32 y );

void CommonIntercept( void );

int GetString( unsigned char *String);
int GetADCNumber( char *String );               /* from ../libr/inter_c.c */
/************************************* input library (inter_isl.c) ********/

#define RAW 1
#define RESTORE 0

extern int ISLSetTerminal(int mode);
extern int ISLGetInput ( void );
extern int ISLPutInput ( unsigned char *c, int n);
extern int ISLGetString ( unsigned char *c, int *n);


/*************************************************************************/


#define DrawXORFrame(x1,y1,x2,y2) if((x1!=x2)||(y1!=x2)){logicop( LO_XOR );color( GLW_MARKERCOLOR );recti( x1, y1, x2, y2);logicop( LO_SRC );}

  


/*   Banana Processing Funtions                                    */

struct BananaStruct *BP_CreateBanana ( void ){

  struct BananaStruct *b;
  
  b = NULL;
  b = (struct BananaStruct *)calloc(1,sizeof(struct BananaStruct));
  if( b ) {
     b->Visible = True;
     b->Active = True;
     b->p = NULL;
     b->s = NULL;
     b->Next = NULL;
     }
  return b;
 }

void BP_DestroyBanana( void ){

  BStruct b;
  BPoint p;
  
  if( Selected == NULL )return;

  b = Banana;
  while( (b->Next != Selected) && b->Next ) b = b->Next;
  if( Selected == Banana )Banana = Selected->Next;
  else b->Next = Selected->Next;  

  p = Selected->p;
  if( p == NULL ){
    Selected = (BStruct)realloc(Selected,0);
    Selected = NULL;
    return;
    }
  while( p->Next ) p = p->Next;
  while( p->Previous ){
       p = p->Previous;
       p->Next =  (BPoint)realloc(p->Next,0);
       p->Next = NULL;
       }
  
  p = (BPoint)realloc(p,0);


  Selected = (BStruct)realloc(Selected,0);
  Selected = NULL;
}


void BP_WriteBanana ( void ) {

  BPoint p;
  FILE *BanFile;
  unsigned char c[128];
  unsigned char *String;
  int l, adc;
  struct stat FileStatus;


  if( Selected ){
    String  = &c[0];
    printf(" WRITE banana in file : ");
    l = GetString( String);
    while( *String == ' ' )
    {
        String++;
	l--;
    }
    if( l < 1 )return;

  if( l > 4 ){
    if( ( c[l-4] == '.' )&&
        ( c[l-3] == 'b' )&&
	( c[l-2] == 'a' )&&
	( c[l-1] == 'n' ) ) l -= 4; }

    String[l] = '.'; String[l+1] = 'b'; String[l+2] = 'a'; String[l+3] = 'n';
    String[l+4] = '\0';
    adc  = GetADCNumber( (char *)String );
  
    String += strspn( (char *)String," \t" );
    BanFile = NULL;

    if( adc > -1 )BanFile = fopen((char *)String,"a");
    else {
      if( stat( (char *)String, &FileStatus ) == 0 ){
         char *OldFileName;
	 OldFileName = ( char * ) calloc( 128, sizeof(char) );

CHECK:	 strncpy( OldFileName, (char *)String, 128 );
	 
	 printf(" WARNING - File %s already exists, you can choose to save the banana\n",String);
	 printf("           in a different file or to overwrite the existing one\n");
         printf(" WRITE banana in file : ");
         String = c;
	 l = GetString( String);
         while( *String == ' ' )
         {
            String++;
	    l--;
         }
        
	if( l < 1 )return;
         if( l > 4 ){
            if( ( c[l-4] == '.' )&&
                ( c[l-3] == 'b' )&&
	        ( c[l-2] == 'a' )&&
	        ( c[l-1] == 'n' ) ) l -= 4; }

	 String[l] = '.'; String[l+1] = 'b'; String[l+2] = 'a'; String[l+3] = 'n';
	 String[l+4] = '\0';
	 adc  = GetADCNumber( (char *)String );
	 if( adc < 0 ) if( strcmp( OldFileName, (char *)String ) ) goto CHECK;
	 
	 if( adc > -1 )BanFile = fopen((char *)String,"a");
	 else BanFile = fopen((char *)String,"w");
	 }
       else BanFile = fopen((char *)String,"w");
       }
    
    if( !BanFile ){
      printf(" ERROR - cannot open file : %s\n",String);
      return;
      }

    if( adc > -1 )fprintf(BanFile,"ADC %d\n",adc);
    p = Selected->p;
    while( p ){
      fprintf(BanFile," %6d  %6d\n",p->x,p->y);
      p = p->Next;
      }
    fclose(BanFile);
    }
  else printf("  No banana currently selected\n");
}


BStruct BP_ReadBanana ( void ) {

  unsigned char c[128];
  unsigned char *String;
  int l;
  BPoint p = NULL;
  BPoint rp;
  BStruct b;
  FILE *BanFile;
  float x,y;
  int adc, iadc;
  
  String  = &c[0];
  printf(" READ banana from file : ");
  l = GetString( String);

  if( l < 1 )return NULL;

  if( l > 4 ){
    if( ( c[l-4] == '.' )&&
        ( c[l-3] == 'b' )&&
	( c[l-2] == 'a' )&&
	( c[l-1] == 'n' ) ) l -= 4; }

  String[l] = '.'; String[l+1] = 'b'; String[l+2] = 'a'; String[l+3] = 'n';
  String[l+4] = '\0';
  adc  = GetADCNumber( (char *)String );
  
  BanFile = NULL;
  String += strspn( (char *)String," \t" );
  BanFile = fopen((char *)String,"r");
  if( !BanFile ){
    printf(" ERROR - cannot open file : %s\n",String);
    return NULL;
    }
  
  if( !p )p = ( BPoint )calloc(1,sizeof(struct BananaPoint));
  rp = p;
  rp->Previous = NULL;
  rp->Next = NULL;
  
  iadc = -1;
  while(  iadc != adc  ){
   if( ( fscanf(BanFile,"%s",String) == EOF) ){
    printf(" ERROR - ADC %d not found \n",adc);
    fclose(BanFile);
    return NULL;
    }
   if( strspn((char *)String,"ADC") )fscanf(BanFile,"%d",&iadc); 
   }
   
  
  do {
    l = fscanf(BanFile,"%g %g", &x, &y);
    if( l == 2 ){
      rp->x = x;
      rp->y = y;
      rp->Next = ( BPoint )calloc(1,sizeof(struct BananaPoint));
      rp->Next->Next = NULL;
      rp->Next->Previous = rp;
      rp = rp->Next;
      }
    else {
      if( rp->Previous )rp->Previous->Next = NULL;
      else p = NULL;
      rp = ( BPoint )realloc(rp, 0);
      }
    } while( l == 2);
  
  fclose(BanFile);  
  if( p ) {
     b = BP_CreateBanana();
     b->p = p;
     return b;
     }
  else return NULL;

}  
  
struct CoordStruct *BP_PixelPosition ( Int32 x, Int32 y){

   static CoordStruct Pos;
   float r;
   
   if( ( x < Plot.Xmin )||( x > Plot.Xmax )||( y < Plot.Ymin )||( y > Plot.Ymax ) )return NULL;
   r = x - Plot.Xmin;
   r += 0.5000;
   r /= XChPix;
   Pos.x = r + MatPlot.x1 +1;
   r = y - Plot.Ymin;
   r += 0.5000;
   r /= YChPix;
   Pos.y = r + MatPlot.y1 +1;
   return (struct CoordStruct *) (&Pos);  
}

LineStruct *BP_LineBetween ( BPoint p1, BPoint p2 ){

  struct CoordStruct *c1, *c2, sc1, sc2;
  static LineStruct Line;
  float A0, A1;
  Int32 k;

  if( p1 && p2 ){
     c1 = BP_PixelPosition( p1->x, p1->y );
     if( c1 ){ sc1.x = c1->x; sc1.y = c1->y; c1 = &sc1;}
     c2 = BP_PixelPosition( p2->x, p2->y );
     if( c2 ){ sc2.x = c2->x; sc2.y = c2->y; c2 = &sc2;}
     if( (c1==NULL) && (c2==NULL) )return NULL;

     if( c1 ){ Line.x1 = c1->x; Line.y1 = c1->y; }
     else {
       if( p1->x == p2->x ){
        Line.x1 = c2->x;
	if( p1->y < Plot.Ymin ) Line.y1 = MatPlot.y1+1;
	if( p1->y > Plot.Ymax ) Line.y1 = MatPlot.y2-1;
	goto C2;
	}
       else {
	A1 = ((float)( p1->y - p2->y ))/((float)( p1->x - p2->x ));
	A0 = ((float) p1->y) - A1*((float) p1->x);
        if( p1->x < Plot.Xmin ){
	    k = A0+A1*Plot.Xmin;
	    c1 = BP_PixelPosition( Plot.Xmin, k );
	    if( c1 ){ Line.x1 = c1->x; Line.y1 = c1->y; }
	    }
        if( p1->x > Plot.Xmax ){
	    k = A0+A1*Plot.Xmax;
	    c1 = BP_PixelPosition( Plot.Xmax, k );
	    if( c1 ){ Line.x1 = c1->x; Line.y1 = c1->y; }
	    }
	}
       if( p1->y == p2->y ){
        Line.y1 = c2->y;
	if( p1->x < Plot.Xmin ) Line.x1 = MatPlot.x1+1;
	if( p1->x > Plot.Xmax ) Line.x1 = MatPlot.x2-1;
	goto C2;
	}
       else {
        A1 = ((float)( p1->x - p2->x ))/((float)( p1->y - p2->y ));
	A0 = ((float) p1->x) - A1*((float) p1->y);
        if( p1->y < Plot.Ymin ){
	    k = A0+A1*Plot.Ymin;
	    c1 = BP_PixelPosition( k, Plot.Ymin  );
	    if( c1 ){ Line.x1 = c1->x; Line.y1 = c1->y; }
	    }
        if( p1->y > Plot.Ymax ){
	    k = A0+A1*Plot.Ymax;
	    c1 = BP_PixelPosition( k, Plot.Ymax  );
	    if( c1 ){ Line.x1 = c1->x; Line.y1 = c1->y; }
	    }
	}
	}   
C2:
     if( c2 ){ Line.x2 = c2->x; Line.y2 = c2->y; }
     else {
       if( p1->x == p2->x ){
        Line.x2 = c1->x;
	if( p2->y < Plot.Ymin ) Line.y2 = MatPlot.y1+1;
	if( p2->y > Plot.Ymax ) Line.y2 = MatPlot.y2-1;
	goto OK;
	}
       else {
        A1 = ((float)( p1->y - p2->y ))/((float)( p1->x - p2->x ));
	A0 = ((float) p2->y) - A1*((float) p2->x);
        if( p2->x < Plot.Xmin ){
	    k = A0+A1*Plot.Xmin;
	    c2 = BP_PixelPosition( Plot.Xmin, k );
	    if( c2 ){ Line.x2 = c2->x; Line.y2 = c2->y; }
	    }
        if( p2->x > Plot.Xmax ){
	    k = A0+A1*Plot.Xmax;
	    c2 = BP_PixelPosition( Plot.Xmax, k );
	    if( c2 ){ Line.x2 = c2->x; Line.y2 = c2->y; }
	    }
	}
       if( p1->y == p2->y ){
        Line.y2 = c2->y;
	if( p2->x < Plot.Xmin ) Line.x2 = MatPlot.x1+1;
	if( p2->x > Plot.Xmax ) Line.x2 = MatPlot.x2-1;
	goto OK;
	}
       else {
        A1 = ((float)( p1->x - p2->x ))/((float)( p1->y - p2->y ));
	A0 = ((float) p2->x) - A1*((float) p2->y);
        if( p2->y < Plot.Ymin ){
	    k = A0+A1*Plot.Ymin;
	    c2 = BP_PixelPosition( k, Plot.Ymin  );
	    if( c2 ){ Line.x2 = c2->x; Line.y2 = c2->y; }
	    }
        if( p2->y > Plot.Ymax ){
	    k = A0+A1*Plot.Ymax;
	    c2 = BP_PixelPosition( k, Plot.Ymax  );
	    if( c2 ){ Line.x2 = c2->x; Line.y2 = c2->y; }
	    }
	}
	}  

OK:
  if( (Line.x1 > MatPlot.x1 ) && (Line.x1 < MatPlot.x2 ) &&
      (Line.x2 > MatPlot.x1 ) && (Line.x2 < MatPlot.x2 ) &&
      (Line.y1 > MatPlot.y1 ) && (Line.y1 < MatPlot.y2 ) &&
      (Line.y2 > MatPlot.y1 ) && (Line.y2 < MatPlot.y2 ) ) return ( LineStruct *) (&Line);
  else return NULL; }

  else return NULL;
  
}
   

BPoint BP_ClosestPoint ( BStruct b ){

  Int32 distance, d;
  BPoint p;
  BPoint closest;

  if( b == NULL )return NULL;  
  if( !b->Visible )return NULL;
  if( b->p == NULL )return NULL;
  
  closest = NULL;
  p = b->p;
  distance = Plot.Xmax + Plot.Ymax +1;
  while( p ){
   if( p->Visible ){
     d = abs(p->x - XYZ.x) + abs(p->y - XYZ.y);
     if( d < distance ) {
        closest = p;
	distance = d;
	}
      }
    p = p->Next;
    }
  return closest;
}


BStruct BP_ClosestBanana ( void ){

  Int32 distance, d;
  BStruct b;
  BStruct closest;
  BPoint p;

  if( Banana == NULL )return NULL;
  
  closest = NULL;
  b = Banana;
  distance = Plot.Xmax + Plot.Ymax +1;
  while( b ){
   p = BP_ClosestPoint( b );
   if( p ){
     d = abs(p->x - XYZ.x) + abs(p->y - XYZ.y);
     if( d < distance ) {
        closest = b;
	distance = d;
	}
      }
   b = b->Next;
   }
  return closest;
}


void BP_DrawPoint ( BPoint p ){

  struct CoordStruct *c;

  c = BP_PixelPosition( p->x, p->y);
  if( c ){
    logicop(LO_XOR);
    color(GLW_UBANANACOLOR);
    circi( c->x, c->y, 4);
    p->Visible = True;
    logicop(LO_SRC);
    }
  else  p->Visible = False;
}  

void BP_DrawSBPoint ( BPoint p ){

  struct CoordStruct *c;

  c = BP_PixelPosition( p->x, p->y);
  if( c ){
    logicop(LO_XOR);
    color(GLW_SBANANACOLOR);
    circi( c->x, c->y, 4);
    logicop(LO_SRC);
    p->Visible = True;
    }
  else  p->Visible = False;
}  

void BP_SelectPoint ( BPoint p ){

  struct CoordStruct *c;

  c = BP_PixelPosition( p->x, p->y);
  if( c ){
    logicop(LO_XOR);
    color(GLW_SBANANACOLOR);
    circi( c->x, c->y, 4);
    circfi( c->x, c->y, 4);
    p->Visible = True;
    logicop(LO_SRC);
    }
  else  p->Visible = False;
}  

void BP_UnselectPoint ( BPoint p ){

  struct CoordStruct *c;

  c = BP_PixelPosition( p->x, p->y);
  if( c ){
    logicop(LO_XOR);
    color(GLW_SBANANACOLOR);
    circfi( c->x, c->y, 4);
    circi( c->x, c->y, 4);
    p->Visible = True;
    logicop(LO_SRC);
    }
  else  p->Visible = False;
}  

Int32 BP_IsVisible( BStruct b){

  BPoint p;
  
  if( b == NULL )return False;
  if( b->p == NULL) return False;
  if( !b->Active )return False;
  
  p = b->p;
  while( p ){
   if( BP_PixelPosition( p->x, p->y) ){ b->Visible = True; return True;}
   p = p->Next;
   }
  b->Visible = False;
  return False;
}

void BP_DrawBanana( BStruct b ){

  LineStruct *l;
  BPoint p;
  
  if( !BP_IsVisible( b ) )return;
  
  p = b->p;
  while( p ){
    BP_DrawPoint( p );
    l = BP_LineBetween( p, p->Next);
    if( l ){
      logicop(LO_XOR);
      color(GLW_UBANANACOLOR);
      move2i( l->x1, l->y1);
      draw2i( l->x2, l->y2);
      logicop(LO_SRC);
      }
    p = p->Next;
    }
}

void BP_DrawSBanana( BStruct b ){

  LineStruct *l;
  BPoint p;
  
  if( !BP_IsVisible( b ) )return;
  
  p = b->p;
  while( p ){
    BP_DrawSBPoint( p );
    l = BP_LineBetween( p, p->Next);
    if( l ){
      logicop(LO_XOR);
      color(GLW_SBANANACOLOR);
      move2i( l->x1, l->y1);
      draw2i( l->x2, l->y2);
      logicop(LO_SRC);
      }
    p = p->Next;
    }
}

void BP_SelectBanana( BStruct b ){

  BP_DrawBanana( b );
  BP_DrawSBanana( b );
}

void BP_UnselectBanana( BStruct b ){

  if( b->s )BP_UnselectPoint( b->s );
  BP_DrawSBanana( b );
  BP_DrawBanana( b );
}

void BP_AddPoint( void ){

  BPoint p;
  LineStruct *l;
  BStruct b;

  if( Banana == NULL ){
     Banana = BP_CreateBanana();
     Selected = Banana;
     Selected->Visible = True;
     Selected->Active = True;
     Selected->p = ( BPoint )calloc(1,sizeof(struct BananaPoint));
     Selected->p->x = XYZ.x;
     Selected->p->y = XYZ.y;
     Selected->p->Visible = True;
     Selected->p->Previous = NULL;
     Selected->p->Next = NULL;
     BP_DrawSBPoint( Selected->p );
     BP_SelectPoint( Selected->p );
     Selected->s = Selected->p;
     FlushDrawings;
     return;
     }
  if( Selected == NULL ){
     b = Banana;
     while( b->Next ) b = b->Next;
     b->Next = BP_CreateBanana();
     Selected = b->Next;
     Selected->Visible = True;
     Selected->Active = True;
     Selected->p = ( BPoint )calloc(1,sizeof(struct BananaPoint));
     Selected->p->x = XYZ.x;
     Selected->p->y = XYZ.y;
     Selected->p->Visible = True;
     Selected->p->Previous = NULL;
     Selected->p->Next = NULL;
     BP_DrawSBPoint( Selected->p );
     BP_SelectPoint( Selected->p );
     Selected->s = Selected->p;
     FlushDrawings;
     return;
     }
  if( Selected->s == NULL )return;

  if( ( abs(Selected->s->x - XYZ.x)+abs(Selected->s->y - XYZ.y) ) < 1 )return;
  p = ( BPoint )calloc(1,sizeof(struct BananaPoint));
  p->x = XYZ.x;
  p->y = XYZ.y;
  p->Visible = True;
  p->Previous = Selected->s;
  logicop(LO_XOR);
  color(GLW_SBANANACOLOR);
  if( Selected->s->Next == NULL ){
    l = BP_LineBetween( Selected->s, p);
    move2i( l->x1, l->y1 );
    draw2i( l->x2, l->y2 );
    }
  else {
    l = BP_LineBetween( Selected->s->Next, Selected->s );
    if( l ){
      move2i( l->x1, l->y1 );
      draw2i( l->x2, l->y2 );
      }
    l = BP_LineBetween( Selected->s, p );
    draw2i( l->x2, l->y2 );
    l = BP_LineBetween( p, Selected->s->Next);
    if( l )draw2i( l->x2, l->y2 );
    }
  BP_UnselectPoint( Selected->s );
  BP_DrawSBPoint( p );
  BP_SelectPoint( p );
  p->Next = Selected->s->Next;
  Selected->s->Next = p;
  if(p->Next)p->Next->Previous = p;
  Selected->s = p;
  logicop(LO_SRC);
}

void BP_MovePoint ( void ){

  LineStruct *l;
  

  if( Selected == NULL )return;
  if( Selected->s == NULL )return;

  logicop(LO_XOR);
  color(GLW_SBANANACOLOR);

  if( Selected->s->Previous != NULL ){
    l = BP_LineBetween( Selected->s->Previous, Selected->s );
    if( l ){
      move2i( l->x1, l->y1 );
      draw2i( l->x2, l->y2 );
      }}

  if( Selected->s->Next != NULL ){
    l = BP_LineBetween( Selected->s, Selected->s->Next );
    if( l ){
      move2i( l->x1, l->y1 );
      draw2i( l->x2, l->y2 );
      }}

  BP_UnselectPoint( Selected->s );
  BP_DrawSBPoint( Selected->s );
  logicop(LO_XOR);
  color(GLW_SBANANACOLOR);
  
  Selected->s->x = XYZ.x;
  Selected->s->y = XYZ.y;

  if( Selected->s->Previous != NULL ){
    l = BP_LineBetween( Selected->s->Previous, Selected->s );
    if( l ){
      move2i( l->x1, l->y1 );
      draw2i( l->x2, l->y2 );
      }}

  if( Selected->s->Next != NULL ){
    l = BP_LineBetween( Selected->s, Selected->s->Next );
    if( l ){
     move2i( l->x1, l->y1 );
     draw2i( l->x2, l->y2 );
    }}
  BP_DrawSBPoint( Selected->s );
  BP_SelectPoint( Selected->s );
  logicop(LO_SRC);
}

void BP_DeletePoint ( void ){

  LineStruct *l;
  BPoint p;
  

  if( Selected == NULL )return;
  if( Selected->s == NULL )return;

  logicop(LO_XOR);
  color(GLW_SBANANACOLOR);

  if( Selected->s->Previous != NULL ){
    Selected->s->Previous->Next = Selected->s->Next;
    l = BP_LineBetween( Selected->s->Previous, Selected->s );
    if( l ){
      move2i( l->x1, l->y1 );
      draw2i( l->x2, l->y2 );
      }}

  if( Selected->s->Next != NULL ){
    Selected->s->Next->Previous = Selected->s->Previous;
    l = BP_LineBetween( Selected->s, Selected->s->Next );
    if( l ){
      move2i( l->x1, l->y1 );
      draw2i( l->x2, l->y2 );
      }}

  BP_UnselectPoint( Selected->s );
  BP_DrawSBPoint( Selected->s );
  logicop(LO_XOR);
  color(GLW_SBANANACOLOR);
  
  Selected->s->x = XYZ.x;
  Selected->s->y = XYZ.y;

  if( (Selected->s->Previous != NULL) && ( Selected->s->Next != NULL ) ){
    l = BP_LineBetween( Selected->s->Previous, Selected->s->Next );
    if( l ){
      move2i( l->x1, l->y1 );
      draw2i( l->x2, l->y2 );
      }}

  if( Selected->p == Selected->s )Selected->p = Selected->s->Next;
  p = Selected->s;
  Selected->s = NULL;
  if( p->Previous ) if( p->Previous->Visible ){ Selected->s = p->Previous; BP_SelectPoint(Selected->s);}
  if( !Selected->s ){
    if( p->Next ) if( p->Next->Visible ){ Selected->s = p->Next; BP_SelectPoint(Selected->s);}
    }

  p = (BPoint) realloc( p, 0);
  if( Selected->p == NULL )BP_DestroyBanana();
  logicop(LO_SRC);
}
  
    
void BP_NewBanana( void ){

  BPoint p;
  LineStruct *l;
  BStruct b;

  if( Selected )BP_UnselectBanana( Selected );
  b = BP_CreateBanana();
  Selected = b;

  if( Banana == NULL ) Banana = b;
  else {
     b = Banana;
     while( b->Next )b = b->Next;
     b->Next = Selected;
     }
     
  Selected->Visible = True;
  Selected->Active = True;
  Selected->p = ( BPoint )calloc(1,sizeof(struct BananaPoint));
  Selected->p->x = XYZ.x;
  Selected->p->y = XYZ.y;
  Selected->p->Visible = True;
  Selected->p->Previous = NULL;
  Selected->p->Next = NULL;
  BP_DrawSBPoint( Selected->p );
  BP_SelectPoint( Selected->p );
  Selected->s = Selected->p;
  FlushDrawings;
  return;
}

void BP_TypeBanana ( void ){

  BPoint p;
  
  if( Selected ){
    printf("----------------\n");
    printf("     X  |    Y |\n");
    printf("----------------\n");
    p = Selected->p;
    while( p ){
      printf(" %6d  %6d\n",p->x,p->y);
      p = p->Next;
      }
    }
}

double BP_Num_P ( BStruct b )
{
	BPoint p;
	double Sum;
	
	Sum = 0.000000000E0;
	
	if( b )
	{
		p = b->p;
		while( p ) { Sum += (double)1.000E0; p = p->Next;}
	}
	return Sum;
}

double BP_Sum_X ( BStruct b )
{
	BPoint p;
	double Sum;
	
	Sum = 0.000000000E0;
	
	if( b )
	{
		p = b->p;
		while( p ) { Sum += (double)p->x; p = p->Next;}
	}
	return Sum;
}

double BP_Sum_Y ( BStruct b )
{
	BPoint p;
	double Sum;
	
	Sum = 0.000000000E0;
	
	if( b )
	{
		p = b->p;
		while( p ) { Sum += (double)p->y; p = p->Next;}
	}
	return Sum;
}

double BP_Sum_X2 ( BStruct b )
{
	BPoint p;
	double Sum;
	
	Sum = 0.000000000E0;
	
	if( b )
	{
		p = b->p;
		while( p ) { Sum += ( (double)p->x )*( (double)p->x ); p = p->Next;}
	}
	return Sum;
}

double BP_Sum_XY ( BStruct b )
{
	BPoint p;
	double Sum;
	
	Sum = 0.000000000E0;
	
	if( b )
	{
		p = b->p;
		while( p ) { Sum += ( (double)p->x )*( (double)p->y ); p = p->Next;}
	}
	return Sum;
}

int BP_Limits ( BStruct b )
{
	BPoint p;
	
	
	if( b )
	{
		p = b->p;
        b->Xl = b->Xr = p->x;
        b->Yb = b->Yt = p->y;
		while( p ) 
        {
          if( p->x < b->Xl ) b->Xl = p->x;
          if( p->x > b->Xr ) b->Xr = p->x;
          if( p->y < b->Yb ) b->Yb = p->y;
          if( p->y > b->Yt ) b->Yt = p->y;
          p = p->Next;
        }
		
		if( b->Xl < 0 ) return False;
		if( b->Xr >= DataXmax ) return False;

		if( b->Yb < 0 ) return False;
		if( b->Yt >= DataYmax ) return False;
		
		return True;
		
	}
	else
	    return False;
}

int BP_Sums ( BStruct b )
{
	BPoint p;
	Int32 ii, jj;
	
	if( b )
	{
		if( !BP_Limits( b ) ) return False;
        b->Integral = b->Surface = b->SX = b->SY = b->SX2 = b->SY2 = b->SXY = 0.0000000E0;
        for( ii = b->Xl; ii <= b->Xr; ii++ )
          for( jj = b->Yb; jj <= b->Yt; jj++ )
             if( BP_IsPointInside( b, ii, jj) )
             {
                b->Integral += Plot.data[jj][ii];
                b->Surface  += 1.0000000E0;
                b->SX += (double)ii*(double)Plot.data[jj][ii];
                b->SX2+= (double)ii*(double)ii*(double)Plot.data[jj][ii];
                b->SY += (double)jj*(double)Plot.data[jj][ii];
                b->SY2+= (double)jj*(double)jj*(double)Plot.data[jj][ii];
                b->SXY+= (double)ii*(double)jj*(double)Plot.data[jj][ii];
             }
		return True;
	}
	else return False;
}

Int32 BP_IsPointInside( BStruct b, Int32 x, Int32 y )
{
       Int32 Inside;
       Int32 LineBelow;
       Int32 yf;
       BPoint p;
       BPoint n;
       
       Inside = 0;
       
       if( b )
       {
          p = b->p;
          while( p ) { Inside++; p = p->Next; }
          if( Inside < 3 ) return 0;
          Inside = 0;
          
          p = b->p;
          LineBelow = 0;
          while( p )
          {
            if( p->Next ) n = p->Next;
            else n = b->p;
            
            if( (abs(x - p->x) + abs(x - n->x)) == abs(p->x - n->x) )
            {
               if( (x!=p->x) && (x!=n->x) )
               {
                  yf = (float)(p->y) + ((float)(x)-(float)(p->x))*((float)(n->y)-(float)(p->y))/((float)(n->x)-(float)(p->x));
                  if( y < yf ) Inside++;
                  else LineBelow++;
               }
               
               if( x == p->x )
               {
                  if( y < p->y ) Inside++;
                  else LineBelow++;
               }
/*               
               if( x == n->x )
               {
                  if( y < n->y ) Inside++;
                  else LineBelow++;
               }
*/
            }
            p = p->Next;              
          }
          Inside = (Inside%2)&( LineBelow?1:0 );
       }
       return Inside;
}


void CommonIntercept( )
{
	BStruct b;
	double nn, r1, r2, tmp1, tmp2;
        double Slope, LinCorr;
	
	nn = r1 = r2 = 0.0000E0;
	
	if( Banana )
	{
		b = Banana;
		while( b )
		{
			if( BP_Sums( b ) )
			{
               Slope   = (b->Integral*b->SXY - b->SX*b->SY)/(b->Integral*b->SX2 - b->SX*b->SX);
               LinCorr = (b->Integral*b->SXY - b->SX*b->SY)/
                		 sqrt((b->Integral*b->SX2 - b->SX*b->SX)*(b->Integral*b->SY2 - b->SY*b->SY));
               printf("   Slope : %.5lf 		 Linear corr. coeff. : %lf\n",Slope, LinCorr);
               nn  += b->Integral;
			   tmp1 = b->SX;
			   tmp2 = b->SX2;
			   r1 += tmp1*tmp1/tmp2;
			   r2 += b->SY - b->SXY*tmp1/tmp2;
			}
			b = b->Next;
		}
		r2 /= nn - r1;
		printf(" Common intercept : %.3lf\n", r2);
	}
}

/*----------------------------------------------------------------------------*/
Int32 GetXYZ ( Int32 x, Int32 y){

  Int32 x1,x2, y1,y2, ix,iy;

   if( (x < MatPlot.x1)||(x > MatPlot.x2)||(y < MatPlot.y1)||(y > MatPlot.y2) )return 0;
   
   XYZ.x = XYZ.y = -1;
   
   if( x == MatPlot.x1 )XYZ.x = Plot.Xmin;
   if( x == MatPlot.x2 )XYZ.x = Plot.Xmax;
   if( y == MatPlot.y1 )XYZ.y = Plot.Ymin;
   if( y == MatPlot.y2 )XYZ.y = Plot.Ymax;
   if( (XYZ.x != -1) && (XYZ.y != -1) ){ XYZ.z = Plot.data[XYZ.y][XYZ.x]; return 1;}
   
   x -= MatPlot.x1;  y -= MatPlot.y1;
   
   if( XYZ.x == -1){
     x1 = Plot.Xmin + XChPix*x; x1 = ( x1 < Plot.Xmax )?x1:Plot.Xmax;
     x2 = Plot.Xmin + XChPix*(x+1)+1; x2 = ( x2 < Plot.Xmax )?x2:Plot.Xmax;
     }
   else { x1 = XYZ.x; x2 = x1 + 1;}
   
   if( XYZ.y == -1){
     y1 = Plot.Ymin + YChPix*y; y1 = ( y1 < Plot.Ymax )?y1:Plot.Ymax;
     y2 = Plot.Ymin + YChPix*(y+1)+1; y2 = ( y2 < Plot.Ymax )?y2:Plot.Ymax;
     }
   else { y1 = XYZ.y; y2 = y1 + 1;}
   
   XYZ.z = Plot.data[y1][x1];
   XYZ.x = x1; XYZ.y = y1;
   for( ix = x1; ix < x2; ix++ )
     for( iy = y1; iy < y2; iy++ ){
        if( XYZ.z < Plot.data[iy][ix] ){
	   XYZ.x = ix; XYZ.y = iy;
	   XYZ.z = Plot.data[iy][ix];
	   }
	}
   return 1;
}   



struct MarkerStruct *CreateMarker ( char Kindl, char KindL, char Color ){

  struct MarkerStruct *p;
  
  p = NULL;
  p = (struct MarkerStruct *)calloc(1,sizeof(struct MarkerStruct));
  if( p ){
     p->Kindl = Kindl;
     p->KindL = KindL;
     p->On = MARKER_OFF;
     p->Color = Color;
     p->Next = NULL;
     }
  return p;
}

void DrawMarker ( char Kind ){

  struct MarkerStruct *p, *pp;
  Int32 x,y;
  
  p = Marker;
  
  getorigin(&x,&y);
  x=getvaluator(MOUSEX)-x;
  y=getvaluator(MOUSEY)-y;
  if( !GetXYZ(x,y) )return;
  ShowPosition();
  while ( p ){
     if( (p->Kindl == Kind) || (p->KindL == Kind) ){
        logicop( LO_XOR); color( (Colorindex) p->Color);
	switch ( p->KindL ){
	   case LMARKERL:{
	        if( p->On ){ move2i(p->PixelPos, MatPlot.y1+1); draw2i(p->PixelPos, XpPlot.y2-1);}
		p->ChannelPos = XYZ.x;
		x = (float)( XYZ.x - Plot.Xmin )/XChPix; x += MatPlot.x1;
		x = ( x > MatPlot.x1 )?x:(MatPlot.x1+1);
		p->PixelPos = x;
		move2i(p->PixelPos, MatPlot.y1+1); draw2i(p->PixelPos, XpPlot.y2-1);
		p->On = MARKER_ON;
		pp = Marker; while( pp ){ if( (pp->KindL == RMARKERL)&& pp->On )
		                          if( pp->ChannelPos < p->ChannelPos ){ 
					    x = p->ChannelPos; p->ChannelPos = pp->ChannelPos; pp->ChannelPos = x;
					    x = p->PixelPos; p->PixelPos = pp->PixelPos; pp->PixelPos = x;}
					  else if( pp->ChannelPos == p->ChannelPos ){
					    p->On = MARKER_OFF; pp->On = p->On = MARKER_OFF;}
					   pp = pp->Next;}
		break;}
	   case RMARKERL:{
	        if( p->On ){ move2i(p->PixelPos, MatPlot.y1+1); draw2i(p->PixelPos, XpPlot.y2-1);}
		p->ChannelPos = XYZ.x;
		x = (float)( XYZ.x - Plot.Xmin )/XChPix ; x += MatPlot.x1;
		x = ( x < MatPlot.x2 )?x:(MatPlot.x2-1);
		p->PixelPos = x;
		move2i(p->PixelPos, MatPlot.y1+1); draw2i(p->PixelPos, XpPlot.y2-1);
		p->On = MARKER_ON;
		pp = Marker; while( pp ){ if( (pp->KindL == LMARKERL)&& pp->On )
		                          if( pp->ChannelPos > p->ChannelPos ){ 
					    x = p->ChannelPos; p->ChannelPos = pp->ChannelPos; pp->ChannelPos = x;
					    x = p->PixelPos; p->PixelPos = pp->PixelPos; pp->PixelPos = x;}
					  else if( pp->ChannelPos == p->ChannelPos ){
					    p->On = MARKER_OFF; pp->On = p->On = MARKER_OFF;}
					   pp = pp->Next;}
		break;}
	   case UMARKERL:{
	        if( p->On ){ move2i(MatPlot.x1+1, p->PixelPos); draw2i(YpPlot.x2-1,p->PixelPos);}
		p->ChannelPos = XYZ.y;
		y = (float)( XYZ.y - Plot.Ymin )/YChPix; y += MatPlot.y1;
		y = ( y > MatPlot.y1 )?y:(MatPlot.y1+1);
		p->PixelPos = y;
		move2i(MatPlot.x1+1, p->PixelPos); draw2i(YpPlot.x2-1,p->PixelPos);
		p->On = MARKER_ON;
		pp = Marker; while( pp ){ if( (pp->KindL == OMARKERL)&& pp->On )
		                          if( pp->ChannelPos < p->ChannelPos ){ 
					    x = p->ChannelPos; p->ChannelPos = pp->ChannelPos; pp->ChannelPos = x;
					    x = p->PixelPos; p->PixelPos = pp->PixelPos; pp->PixelPos = x;}
					  else if( pp->ChannelPos == p->ChannelPos ){
					    p->On = MARKER_OFF; pp->On = p->On = MARKER_OFF;}
					   pp = pp->Next;}
		break;}
	   case OMARKERL:{
	        if( p->On ){ move2i(MatPlot.x1+1, p->PixelPos); draw2i(YpPlot.x2-1,p->PixelPos);}
		p->ChannelPos = XYZ.y;
		y = (float)( XYZ.y - Plot.Ymin )/YChPix ; y += MatPlot.y1;
		y = ( y < MatPlot.y2 )?y:(MatPlot.y2-1);
		p->PixelPos = y;
		move2i(MatPlot.x1+1, p->PixelPos); draw2i(YpPlot.x2-1,p->PixelPos);
		p->On = MARKER_ON;
		pp = Marker; while( pp ){ if( (pp->KindL == UMARKERL)&& pp->On )
		                          if( pp->ChannelPos > p->ChannelPos ){ 
					    x = p->ChannelPos; p->ChannelPos = pp->ChannelPos; pp->ChannelPos = x;
					    x = p->PixelPos; p->PixelPos = pp->PixelPos; pp->PixelPos = x;}
					  else if( pp->ChannelPos == p->ChannelPos ){
					    p->On = MARKER_OFF; pp->On = p->On = MARKER_OFF;}
					   pp = pp->Next;}
		break;}
	   }
        logicop(LO_SRC);
	return;
	}
     p = p->Next;
     }
 }

void RedrawAllMarkers ( void ){

  struct MarkerStruct *p;
  Int32 x,y;

  p = Marker;
  
  while ( p ){
        logicop( LO_XOR); color( (Colorindex) p->Color);
	switch ( p->KindL ){
	   case LMARKERL:
	      if( p->On ){ 
		x = (float)( p->ChannelPos - Plot.Xmin )/XChPix; x += MatPlot.x1;
		x = ( x > MatPlot.x1 )?x:(MatPlot.x1+1);
		p->PixelPos = x;
		move2i(p->PixelPos, MatPlot.y1+1); draw2i(p->PixelPos, XpPlot.y2-1);
		break;}
	   case RMARKERL:
	      if( p->On ){
		x = (float)( p->ChannelPos - Plot.Xmin )/XChPix ; x += MatPlot.x1;
		x = ( x < MatPlot.x2 )?x:(MatPlot.x2-1);
		p->PixelPos = x;
		move2i(p->PixelPos, MatPlot.y1+1); draw2i(p->PixelPos, XpPlot.y2-1);
		break;}
	   case UMARKERL:
	      if( p->On ){ 
		y = (float)( p->ChannelPos - Plot.Ymin )/YChPix; y += MatPlot.y1;
		y = ( y > MatPlot.y1 )?y:(MatPlot.y1+1);
		p->PixelPos = y;
		move2i(MatPlot.x1+1, p->PixelPos); draw2i(YpPlot.x2-1,p->PixelPos);
		break;}
	   case OMARKERL:
	      if( p->On ){
		y = (float)( p->ChannelPos - Plot.Ymin )/YChPix ; y += MatPlot.y1;
		y = ( y < MatPlot.y2 )?y:(MatPlot.y2-1);
		p->PixelPos = y;
		move2i(MatPlot.x1+1, p->PixelPos); draw2i(YpPlot.x2-1,p->PixelPos);
		break;}
	   }
        logicop(LO_SRC);
        p = p->Next;
	}
 }



void FillIt ( void ){

 Scoord ii,jj;
 Int32 NPoints, ColStep;
 
 NPoints = (MatPlot.x2 - MatPlot.x1 - 1)*(MatPlot.y2 - MatPlot.y1 - 1);
 ColStep = NPoints/17+1;
 
 NPoints = 0; 
 for( ii = MatPlot.x1+1; ii < MatPlot.x2; ii++){
   for( jj = MatPlot.y1+1; jj < MatPlot.y2; jj++){
	NPoints++; color(ContColor[NPoints/ColStep]);
	pnt2s(ii,jj);
	}
    }
    
}


void MakeData ( Int32 *data ){

  Int32 x,y;
  static Int32 OldXmax, OldYmax;
  
  if( (DataXmax != OldXmax) || (DataYmax != OldYmax) ){
    Plot.Xmin = Plot.Ymin = 0;
    Plot.Xmax =DataXmax - 1; Plot.Ymax = DataYmax-1;
    OldXmax = DataXmax;
    OldYmax = DataYmax;
    }
  
  Plot.data = (Int32 **)realloc((void *)Plot.data, DataYmax*sizeof(Int32 *));
  Plot.ProX = (float *)realloc((void *)Plot.ProX, DataXmax*sizeof(float));
  Plot.ProY = (float *)realloc((void *)Plot.ProY, DataYmax*sizeof(float));
  
  for( y = 0; y < DataYmax; y++){
    Plot.data[y] = data+y*DataXmax ;
    }
    
 }



void DrawPlot ( void ){

 Scoord x,xb,y,yb,Xpixels,Ypixels;
 Coord Xx, Yy;
 Int32 x1,x2, y1,y2,ix,iy,m,i,ixb;
 float ColorStep, CellValue, ProStep, ProMax;
 double ZRange;
 Int16 val;

  SetMouseShape(XC_watch);  

  logicop(Plot.Reverse); /*(LO_SRC);*/
  color(GLW_DRAWBG);
  rectfi(XpPlot.x1,XpPlot.y1,XpPlot.x2,XpPlot.y2);
  rectfi(YpPlot.x1,YpPlot.y1,YpPlot.x2,YpPlot.y2);

  rectfi(MatPlot.x1-4,MatPlot.y1, MatPlot.x1,MatPlot.y2);
  rectfi(MatPlot.x2,MatPlot.y1, MatPlot.x2+4,MatPlot.y2);
  rectfi(MatPlot.x1,MatPlot.y1-4, MatPlot.x2,MatPlot.y1);
  rectfi(MatPlot.x1,MatPlot.y2, MatPlot.x2,MatPlot.y2+4);
  DrawPlotFrame(MatPlot);
  DrawPlotFrame(XpPlot); DrawPlotFrame(YpPlot); FlushDrawings;

/*        Find min. and max. values, calculate projections    */

  Plot.LocalMinVal = Plot.LocalMaxVal = Plot.data[Plot.Ymin][Plot.Xmin];
  for( y = Plot.Ymin; y <= Plot.Ymax; y++) Plot.ProY[y] = 0.00;
  for( x = Plot.Xmin; x <= Plot.Xmax; x++){
     Plot.ProX[x] = 0.00;
     for( y = Plot.Ymin; y <= Plot.Ymax; y++){
         Plot.LocalMinVal = ( Plot.LocalMinVal > Plot.data[y][x] )?Plot.data[y][x]:Plot.LocalMinVal;
	 Plot.LocalMaxVal = ( Plot.LocalMaxVal < Plot.data[y][x] )?Plot.data[y][x]:Plot.LocalMaxVal;
	 Plot.ProX[x] += Plot.data[y][x];
	 Plot.ProY[y] += Plot.data[y][x];
	 }
      if( qtest() )
        if( qread( &val ) == REDRAW ) {
            qenter(REDRAW,(Int16)Win);
            return;
            }
     }
  if( Plot.LocalMaxVal <= Plot.LocalMinVal )Plot.LocalMaxVal = Plot.LocalMinVal + 1;
  
  logicop(Plot.Reverse);
/*     Prepare data for display and draw it    */
  Xpixels = MatPlot.x2 - MatPlot.x1-1;
  Ypixels = MatPlot.y2 - MatPlot.y1-1;
  XChPix = ((double)(Plot.Xmax - Plot.Xmin+1))/((double)Xpixels);
  YChPix = ((double)(Plot.Ymax - Plot.Ymin+1))/((double)Ypixels);
  ZRange = Plot.LocalMaxVal - Plot.LocalMinVal;
  ZRange = log10( ZRange );
  /*Plot.Zmax = Plot.LocalMaxVal;*/
  Plot.Zmax = Plot.LocalMinVal + pow(10.00, ZMaxFactor*ZRange/100.00);
  Plot.Zmin = Plot.LocalMinVal + pow(10.00, ZMinFactor*ZRange/100.00)-1.000;
  if ( Plot.Zmin >= Plot.Zmax ) {
    Plot.Zmax = Plot.LocalMaxVal;
    ZMaxFactor = 100.00;
    ZMaxSlider->value = 100.00;
    redraw_widgets();
    }

  switch ( Plot.ZScaleType ) {
     case ZLIN: {ColorStep = ((float)( Plot.Zmax - Plot.Zmin +1 ))/16.00; break;}
     case ZLOG: {ColorStep = log10((double)( Plot.Zmax - Plot.Zmin +1))/16.00; break;}
     case ZSQRT:{ColorStep = sqrt((double)( Plot.Zmax - Plot.Zmin +1))/16.00; break;}
     case ZCBRT:{ColorStep = cbrt((double)( Plot.Zmax - Plot.Zmin +1))/16.00; break;}
     case ZATAN:{ColorStep = atan((double)( Plot.Zmax - Plot.Zmin +1))/16.00; break;}
     case ZMIX: {ColorStep = mixf((double)( Plot.Zmax - Plot.Zmin +1))/16.00; break;}
     }

  Plot.Image = ( Uint8 * )realloc( (void *)Plot.Image, Ypixels*sizeof(Uint8));
  for( x = 0, xb = Xpixels-1 ; x <= xb; x++, xb--){

      x1 = Plot.Xmin + XChPix*x;
      x2 = Plot.Xmin + XChPix*(x+1)+1; x2 = ( x2 < Plot.Xmax+1 )?x2:Plot.Xmax+1;
      for( y = 0, yb = Ypixels-1 ; y <= yb; y++, yb--){
        y1 = Plot.Ymin + YChPix*y;
	y2 = Plot.Ymin + YChPix*(y+1)+1; y2 = ( y2 < Plot.Ymax+1 )?y2:Plot.Ymax+1;
	CellValue = Plot.data[y1][x1];
	for( ix = x1; ix < x2; ix++ )
	  for( iy = y1; iy < y2; iy++ ) CellValue = ( CellValue > Plot.data[iy][ix] )?CellValue:Plot.data[iy][ix];
        if( CellValue <= Plot.Zmin ) Plot.Image[y] = ContColor[0];
	else if( CellValue >= Plot.Zmax ) Plot.Image[y] = ContColor[16];
	else {CellValue -=Plot.Zmin;
	      switch ( Plot.ZScaleType ) {
	         case ZLOG: { if( CellValue < 1.00 ) CellValue += 1.00;
		              CellValue = log10((double)(CellValue +0.01) ); break;}
		 case ZSQRT:{ CellValue = sqrt( (double)CellValue ); break;}
		 case ZCBRT:{ CellValue = cbrt( (double)CellValue ); break;}
		 case ZATAN:{ CellValue = atan( (double)CellValue ); break;}
		 case ZMIX: { if( CellValue < 1.00 ) CellValue += 1.00;
		              CellValue = mixf( (double)CellValue +0.01); break;}
		 }
	      Plot.Image[y] = CellValue/ColorStep + 1;
	      Plot.Image[y] = (Plot.Image[y] > 16)?ContColor[16]:ContColor[Plot.Image[y]];}

        y1 = Plot.Ymin + YChPix*yb;
	y2 = Plot.Ymin + YChPix*(yb+1)+1; y2 = ( y2 < Plot.Ymax+1 )?y2:Plot.Ymax+1;
	CellValue = Plot.data[y1][x1];
	for( ix = x1; ix < x2; ix++ )
	  for( iy = y1; iy < y2; iy++ ) CellValue = ( CellValue > Plot.data[iy][ix] )?CellValue:Plot.data[iy][ix];
        if( CellValue <= Plot.Zmin ) Plot.Image[yb] = ContColor[0];
	else if( CellValue >= Plot.Zmax ) Plot.Image[yb] = ContColor[16];
	else {CellValue -=Plot.Zmin;
	      switch ( Plot.ZScaleType ) {
	         case ZLOG: { if( CellValue < 1.00 ) CellValue += 1.00;
		              CellValue = log10((double)(CellValue +0.01) ); break;}
		 case ZSQRT:{ CellValue = sqrt( (double)CellValue ); break;}
		 case ZCBRT:{ CellValue = cbrt( (double)CellValue ); break;}
		 case ZATAN:{ CellValue = atan( (double)CellValue ); break;}
		 case ZMIX: { if( CellValue < 1.00 ) CellValue += 1.00;
		              CellValue = mixf( (double)CellValue +0.01); break;}
		 }
	      Plot.Image[yb] = CellValue/ColorStep + 1;
	      Plot.Image[yb] = (Plot.Image[yb] > 16)?ContColor[16]:ContColor[Plot.Image[yb]];}
	}
      crectwrite((Screencoord) (MatPlot.x1+x+1),(Screencoord) (MatPlot.y1+1),
                (Screencoord) (MatPlot.x1+x+1),(Screencoord) (MatPlot.y2-1),
		Plot.Image);
      
      x1 = Plot.Xmin + XChPix*xb;
      x2 = Plot.Xmin + XChPix*(xb+1)+1; x2 = ( x2 < Plot.Xmax+1 )?x2:Plot.Xmax+1;

      for( y = 0, yb = Ypixels-1 ; y <= yb; y++, yb--){
        y1 = Plot.Ymin + YChPix*y;
	y2 = Plot.Ymin + YChPix*(y+1)+1; y2 = ( y2 < Plot.Ymax+1 )?y2:Plot.Ymax+1;
	CellValue = Plot.data[y1][x1];
	for( ix = x1; ix < x2; ix++ )
	  for( iy = y1; iy < y2; iy++ ) CellValue = ( CellValue > Plot.data[iy][ix] )?CellValue:Plot.data[iy][ix];
        if( CellValue <= Plot.Zmin ) Plot.Image[y] = ContColor[0];
	else if( CellValue >= Plot.Zmax ) Plot.Image[y] = ContColor[16];
	else {CellValue -=Plot.Zmin;
	      switch ( Plot.ZScaleType ) {
	         case ZLOG: { if( CellValue < 1.00 ) CellValue += 1.00;
		              CellValue = log10((double)(CellValue +0.01) ); break;}
		 case ZSQRT:{ CellValue = sqrt( (double)CellValue ); break;}
		 case ZCBRT:{ CellValue = cbrt( (double)CellValue ); break;}
		 case ZATAN:{ CellValue = atan( (double)CellValue ); break;}
		 case ZMIX: { if( CellValue < 1.00 ) CellValue += 1.00;
		              CellValue = mixf( (double)CellValue +0.01); break;}
		 }
	      Plot.Image[y] = CellValue/ColorStep + 1;
	      Plot.Image[y] = (Plot.Image[y] > 16)?ContColor[16]:ContColor[Plot.Image[y]];}

        y1 = Plot.Ymin + YChPix*yb;
	y2 = Plot.Ymin + YChPix*(yb+1)+1; y2 = ( y2 < Plot.Ymax+1 )?y2:Plot.Ymax+1;
	CellValue = Plot.data[y1][x1];
	for( ix = x1; ix < x2; ix++ )
	  for( iy = y1; iy < y2; iy++ ) CellValue = ( CellValue > Plot.data[iy][ix] )?CellValue:Plot.data[iy][ix];
        if( CellValue <= Plot.Zmin ) Plot.Image[yb] = ContColor[0];
	else if( CellValue >= Plot.Zmax ) Plot.Image[yb] = ContColor[16];
	else {CellValue -=Plot.Zmin;
	      switch ( Plot.ZScaleType ) {
	         case ZLOG: { if( CellValue < 1.00 ) CellValue += 1.00;
		              CellValue = log10((double)(CellValue +0.01) ); break;}
		 case ZSQRT:{ CellValue = sqrt( (double)CellValue ); break;}
		 case ZCBRT:{ CellValue = cbrt( (double)CellValue ); break;}
		 case ZATAN:{ CellValue = atan( (double)CellValue ); break;}
		 case ZMIX: { if( CellValue < 1.00 ) CellValue += 1.00;
		              CellValue = mixf( (double)CellValue +0.01); break;}
		 }
	      Plot.Image[yb] = CellValue/ColorStep + 1;
	      Plot.Image[yb] = (Plot.Image[yb] > 16)?ContColor[16]:ContColor[Plot.Image[yb]];}
	}

      crectwrite((Screencoord) (MatPlot.x1+xb+1),(Screencoord) (MatPlot.y1+1),
                (Screencoord) (MatPlot.x1+xb+1),(Screencoord) (MatPlot.y2-1),
		Plot.Image);
      if( x%100 == 0) {
         FlushDrawings;
	 if( qtest() )
	   if( qread( &val ) == REDRAW ) {
	       qenter(REDRAW,(Int16)Win);
	       return;
	       }
         }

      }
    
  FlushDrawings; /*logicop(LO_SRC);*/ SetMouseShape(XC_left_ptr);

/*  Now draw the projections */

  color(WHITE);
  ProMax = Plot.LocalMinVal;
  for( i = Plot.Xmin; i <= Plot.Xmax; i++ ) ProMax = ( ProMax > Plot.ProX[i] )?ProMax:Plot.ProX[i];
  ProMax *= 1.05000;
  ProStep = ((float)(XpPlot.y2 - XpPlot.y1-1))/(ProMax - Plot.LocalMinVal+0.5);
  Xx = (float)(XpPlot.x1 + 1);
  Yy = ( Plot.ProX[Plot.Xmin] - Plot.LocalMinVal )*ProStep + (float)(XpPlot.y1+1);
  move2( Xx, Yy );
  CellValue = 1.00/XChPix;
  for( i = Plot.Xmin; i < Plot.Xmax; i++){
      Xx += CellValue;
      draw2(Xx,Yy);
      Yy = ( Plot.ProX[i+1] - Plot.LocalMinVal )*ProStep + (float)(XpPlot.y1+1);
      draw2(Xx,Yy);
      }
   Xx = XpPlot.x2 - 1;
   draw2(Xx,Yy);
   
  ProMax = Plot.LocalMinVal;
  for( i = Plot.Ymin; i <= Plot.Ymax; i++ ) ProMax = ( ProMax > Plot.ProY[i] )?ProMax:Plot.ProY[i];
  ProMax *= 1.05000;
  ProStep = ((float)(YpPlot.x2 - YpPlot.x1-1))/(ProMax - Plot.LocalMinVal+0.5);
  Yy = (float)(YpPlot.y1 + 1);
  Xx = ( Plot.ProY[Plot.Ymin] - Plot.LocalMinVal )*ProStep + (float)(YpPlot.x1+1);
  move2( Xx, Yy );
  CellValue = 1.00/YChPix;
  for( i = Plot.Ymin; i < Plot.Ymax; i++){
      Yy += CellValue;
      draw2(Xx,Yy);
      Xx = ( Plot.ProY[i+1] - Plot.LocalMinVal )*ProStep + (float)(YpPlot.x1+1);
      draw2(Xx,Yy);
      }
   Yy = YpPlot.y2 - 1;
   draw2(Xx,Yy);
   
   

/*  Display the limits and the color scale*/
  sprintf( XMinLabel.Text,"%d\0",Plot.Xmin); WriteInLabel(XMinLabel);
  sprintf( XMaxLabel.Text,"%d\0",Plot.Xmax); WriteInLabel(XMaxLabel);
  sprintf( YMinLabel.Text,"%d\0",Plot.Ymin); WriteInLabel(YMinLabel);
  sprintf( YMaxLabel.Text,"%d\0",Plot.Ymax); WriteInLabel(YMaxLabel);

  DrawColorScale();
  sprintf( ZMinLabel.Text,"%.3g\0",Plot.Zmin); WriteInLabel(ZMinLabel);
  sprintf( ZMaxLabel.Text,"%.3g\0",Plot.Zmax); WriteInLabel(ZMaxLabel);
  FlushDrawings;
  logicop(LO_SRC);
 }
  
	

void MapPlots( void ){

 float FontHeight,LeftX,RightX,BottY,TopY,SizeX,SizeY;

#define STRINGSIZE strwidth("00000000\0")
 FontHeight=2*getheight();
 TopY = 2*FontHeight+4; BottY = FontHeight+4; 
 LeftX = STRINGSIZE+4; RightX = 4;
 
 SizeY = (float)(YWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT) - BottY - TopY;
 SizeX = SizeY* (float)(XWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT)/
                (float)(YWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT);
 SizeX = ( SizeX < ((float)(XWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT) - LeftX - RightX))?SizeX:
         ((float)(XWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT) - LeftX - RightX);
	 
 SizeY =  SizeX* 
         (float)(YWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT)/
	 (float)(XWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT);

 MatPlot.x1 = LeftX + (float)GLW_FRAMEWIDTH_LB;
 MatPlot.x2 = MatPlot.x1 + 5.00/6.00*SizeX;
 
 MatPlot.y1 = BottY + (float)GLW_FRAMEWIDTH_LB;
 MatPlot.y2 = MatPlot.y1 + 5.00/6.00*SizeY;
 
 XpPlot.x1 = MatPlot.x1; XpPlot.x2 = MatPlot.x2;
 XpPlot.y1 = MatPlot.y2; XpPlot.y2 = MatPlot.y2 + SizeY/6.00;
 YpPlot.x1 = MatPlot.x2; YpPlot.x2 = MatPlot.x2 + SizeX/6.00;
 YpPlot.y1 = MatPlot.y1; YpPlot.y2 = MatPlot.y2;

 XMinLabel.x1 = MatPlot.x1; XMinLabel.x2 = XMinLabel.x1 + STRINGSIZE;
 XMinLabel.y1 = MatPlot.y1 - FontHeight; XMinLabel.y2 = MatPlot.y1;
 XMinLabel.bgcolor = BLACK; XMinLabel.fgcolor = YELLOW;
 if( XMinLabel.Text == NULL) XMinLabel.Text =(char *)calloc(10,sizeof(char));
 
 XMaxLabel.x1 = MatPlot.x2 - STRINGSIZE; XMaxLabel.x2 = MatPlot.x2;
 XMaxLabel.y1 = MatPlot.y1 - FontHeight; XMaxLabel.y2 = MatPlot.y1;
 XMaxLabel.bgcolor = BLACK; XMaxLabel.fgcolor = YELLOW;
 if( XMaxLabel.Text == NULL) XMaxLabel.Text =(char *)calloc(10,sizeof(char));

 YMinLabel.x1 = GLW_FRAMEWIDTH_LB+4; YMinLabel.x2 = YMinLabel.x1 + STRINGSIZE;
 YMinLabel.y1 = MatPlot.y1; YMinLabel.y2 = YMinLabel.y1 + FontHeight;
 YMinLabel.bgcolor = BLACK; YMinLabel.fgcolor = YELLOW;
 if( YMinLabel.Text == NULL) YMinLabel.Text =(char *)calloc(10,sizeof(char));
 
 YMaxLabel.x1 = GLW_FRAMEWIDTH_LB+4; YMaxLabel.x2 = YMaxLabel.x1 + STRINGSIZE;
 YMaxLabel.y2 = MatPlot.y2; YMaxLabel.y1 = YMaxLabel.y2 - FontHeight;
 YMaxLabel.bgcolor = BLACK; YMaxLabel.fgcolor = YELLOW;
 if( YMaxLabel.Text == NULL) YMaxLabel.Text =(char *)calloc(10,sizeof(char));
#undef STRINGSIZE 
}


void DrawColorScale( void ){

   int Height, x0, y0, Step,i;

#define STRINGSIZE strwidth("00000\0")
   logicop(Plot.Reverse);
   Height = 2*getheight();
   y0 = YWp - GLW_FRAMEWIDTH_RT - Height -4;
   Height /= 1.500;
   x0 =  MatPlot.x1 + STRINGSIZE + 4;
   Step = ( MatPlot.x2 -x0 - STRINGSIZE - 4)/18;
   if( Step < 4)return;
   for( i = 0; i < 17; i++){
      color( ContColor[i]);
      rectfi(x0+Step*i, y0, x0+Step*(i+1), y0+Height);
      }
   color(GLW_FRAMECOLOR_LIGHT); recti(x0,y0, x0+17*Step,y0+Height);
   logicop(LO_SRC); FlushDrawings;

   ZMinLabel.x1 = x0 - STRINGSIZE;  ZMinLabel.x2 = x0 + STRINGSIZE;
   ZMinLabel.y1 = y0 - Height; ZMinLabel.y2 = y0;
   ZMinLabel.bgcolor = BLACK; ZMinLabel.fgcolor = YELLOW;
   if( ZMinLabel.Text == NULL) ZMinLabel.Text =(char *)calloc(10,sizeof(char));
   
   ZMaxLabel.x1 = x0 - STRINGSIZE + 17*Step;  ZMaxLabel.x2 = x0 + STRINGSIZE + 17*Step;
   ZMaxLabel.y1 = y0 - Height; ZMaxLabel.y2 = y0;
   ZMaxLabel.bgcolor = BLACK; ZMaxLabel.fgcolor = YELLOW;
   if( ZMaxLabel.Text == NULL) ZMaxLabel.Text =(char *)calloc(10,sizeof(char));
#undef STRINGSIZE 
 }


void DrawPlotFrame ( struct PlotFrame P ) {

  color(GLW_FRAMECOLOR_LIGHT);  
  recti( P.x1,P.y1 , P.x2,P.y2);
}

void ClosestColor ( Int16 *r, Int16 *g, Int16 *b){

 Int16 rr,gg,bb;
 Int16 rd,gd,bd;
 Int16 RedB=0,GreenB=0,BlueB=255;
 XColor c[256];
 unsigned long pixels[2];
 int chq,Schq;
 int Depth,ii,jj,kk;


 if( usePCMAP() ) return;
 
#define DD (Display *) getXdpy()
#define DoRGB               (DoRed|DoGreen|DoBlue)
 if(DefaultDepth(DD,DefaultScreen(DD)) < 8)return; 
 if(DefaultDepth(DD,DefaultScreen(DD)) > 16)return; 
 c[0].red = (*r)<<8; c[0].green=(*g)<<8; c[0].blue=(*b)<<8; c[0].flags = DoRGB;
 if(XAllocColor(DD,DefaultColormap(DD,DefaultScreen(DD)),&c[0]))return;

 kk = 1; kk <<=DefaultDepth(DD,DefaultScreen(DD));
/* printf(" Depth : %d\n",DefaultDepth(DD,DefaultScreen(DD))); */
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

void MapGLWcolors(void){

 Int16 k1,k2,k3;
 Colorindex i;
 char ColorString[20];
 char *ColorName = &ColorString[0];
 Int16 rr,gg,bb;
 
 rr=255; gg=255; bb=85;
 ClosestColor( &rr, &gg, &bb);  
 mapcolor(GLW_VISIBLECOLOR,rr,gg,bb); 

 rr=112; gg=128; bb=144;
 ClosestColor( &rr, &gg, &bb);  
 mapcolor(GLW_FRAMECOLOR ,rr, gg, bb);

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

 
 for( i = 4; i > 0; i--){

   ClosestColor( &Red[i+11], &Green[i+11], &Blue[i+11]);  
   mapcolor(ContColor[i+12],Red[i+11],Green[i+11],Blue[i+11]);
   
   ClosestColor( &Red[i+7], &Green[i+7], &Blue[i+7]);  
   mapcolor(ContColor[i+8],Red[i+7],Green[i+7],Blue[i+7]);

   ClosestColor( &Red[i+3], &Green[i+3], &Blue[i+3]);  
   mapcolor(ContColor[i+4],Red[i+3],Green[i+3],Blue[i+3]);

   ClosestColor( &Red[i-1], &Green[i-1], &Blue[i-1]);  
   mapcolor(ContColor[i],Red[i-1],Green[i-1],Blue[i-1]);
   }

 }

void LoadGLWfont(void){

  loadXfont(GLW_FONTID12, "-*-times-medium-r-*-*-12-120-*-*-*-*-iso8859-1");
  loadXfont(GLW_FONTID14, "-*-times-medium-r-*-*-14-140-*-*-*-*-iso8859-1");
/*
  loadXfont(GLW_FONTID12, "-*-helvetica-medium-r-*-*-10-100-*-*-*-*-iso8859-1");
  loadXfont(GLW_FONTID14, "-*-helvetica-bold-r-*-*-12-120-*-*-*-*-iso8859-1");
*/

  font(GLW_FONTID14);
 }

void SetMouseShape(unsigned int shape){

 static Cursor MouseShape;
 static XColor bg,fg;
 static unsigned int CrtShape;

  if(shape == CrtShape)return;
  
  bg.red = 0 ; bg.green = 0     ; bg.blue  = 0;
  fg.red = 0xcfff; fg.green = 0xdfff; fg.blue  = 0xffff     ;

  MouseShape=XCreateFontCursor( (Display *)getXdpy(),shape); 
  XRecolorCursor((Display *)getXdpy(),MouseShape,&fg,&bg);
  XDefineCursor((Display *)getXdpy(),getXwid(),MouseShape);
  CrtShape=shape;
 }


/*  Label Stuff */

void DrawLabel(LabelInFrame L){

 float xf1,xf2,yf1,yf2,pixelsizeX,pixelsizeY;
 Coord LabelBorder[4][2];

 logicop( *(L.Reverse) );
 color(L.bgcolor);
 pixelsizeX=2.0;
 pixelsizeY=2.0;

 xf1 = L.x1+pixelsizeX;  xf2 = L.x2-pixelsizeX;
 yf1 = L.y1+pixelsizeY;  yf2 = L.y2-pixelsizeY;
 rectf(xf1,yf1,xf2,yf2);
 logicop( LO_SRC );

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

 float xf1,xf2,yf1,yf2,height,width;

 logicop( *(L.Reverse) );
 color(L.bgcolor);

 xf1 = L.x1+2.0;  xf2 = L.x2-2.0;
 yf1 = L.y1+2.0;  yf2 = L.y2-2.0;
 rectf(xf1,yf1,xf2,yf2);

 height=(yf2-yf1-(getheight()-getdescender()-2))/2.0;
 width=(xf2-xf1-strwidth(L.Text))/2.0;

 color(L.fgcolor);

 xf1 += width; 
 yf1 += height; 
 
 cmov2(xf1,yf1);
 charstr(L.Text);
 logicop( LO_SRC );
 }

/*          END Label Stuff */

void ShowPosition ( void ){

  sprintf(CursorXValue.Text," X : %d\0",XYZ.x);
  sprintf(CursorYValue.Text," Y : %d\0",XYZ.y);
  sprintf(CursorZValue.Text," Z : %.3g\0",XYZ.z);
  WriteInLabel(CursorXValue);
  WriteInLabel(CursorYValue);
  WriteInLabel(CursorZValue);

}

void ClearDrawArea ( void ) {

 Coord FrameBorder[4][2];
 Int32 x1Wp, x2Wp, y1Wp, y2Wp;
 float x1LB,x2LB,y1LB,y2LB;
 float x1RT,x2RT,y1RT,y2RT;
 float FontHeight;
 float MinSizeXY, SizeX, SizeY;
  

 x1Wp=0; y1Wp=0;
 x2Wp=XWp-1; y2Wp=YWp-1;
 x1LB=GLW_FRAMEWIDTH_LB; y1LB=GLW_FRAMEWIDTH_LB;
 x1RT=GLW_FRAMEWIDTH_RT; y1RT=GLW_FRAMEWIDTH_RT;

 logicop(Plot.Reverse); 
 color(BLACK); rectf((Coord)(x1Wp+x1LB),(Coord)(y1Wp+y1LB),(Coord)(x2Wp-x1RT),(Coord)(y2Wp-y1RT));
 logicop(LO_SRC);
 
}

void DrawFrame ( void ) {

 Coord FrameBorder[4][2];
 Int32 x1Wp, x2Wp, y1Wp, y2Wp;
 float x1LB,x2LB,y1LB,y2LB;
 float x1RT,x2RT,y1RT,y2RT;
 float FontHeight;
 float MinSizeXY, SizeX, SizeY;
  

 x1Wp=0; y1Wp=0;
 x2Wp=XWp-1; y2Wp=YWp-1;
 x1LB=GLW_FRAMEWIDTH_LB; y1LB=GLW_FRAMEWIDTH_LB;
 x1RT=GLW_FRAMEWIDTH_RT; y1RT=GLW_FRAMEWIDTH_RT;
 x2LB=(x1LB-2); y2LB=(y1LB-2);
 x2RT=(x1RT-2); y2RT=(y1RT-2);
/* 
 color(BLACK); rectf((Coord)x1Wp,(Coord)y1Wp,(Coord)x2Wp,(Coord)y2Wp);
*/
 ClearDrawArea();
  
 color(GLW_FRAMECOLOR);
 rectf((Coord)x1Wp-1,(Coord)y1Wp,(Coord)x1Wp+x2LB,(Coord)y2Wp+1);
 rectf((Coord)x2Wp+1,(Coord)y1Wp-1,(Coord)x2Wp-x2RT,(Coord)y2Wp+1);
 rectf((Coord)x1Wp-1,(Coord)y1Wp-1,(Coord)x2Wp+1,(Coord)y1Wp+y2LB);
 rectf((Coord)x1Wp-1,(Coord)y2Wp+1,(Coord)x2Wp+1,(Coord)y2Wp-y2RT);
 
 color(GLW_LABELCOLOR_1);
 FrameBorder[0][0]=(float)x1Wp+x2LB; FrameBorder[0][1]=(float)y1Wp+y2LB;
 FrameBorder[1][0]=(float)x1Wp+x1LB;  FrameBorder[1][1]=(float)y1Wp+y1LB;
 FrameBorder[2][0]=(float)x2Wp-x1RT;  FrameBorder[2][1]=(float)y1Wp+y1LB;
 FrameBorder[3][0]=(float)x2Wp-x2RT; FrameBorder[3][1]=(float)y1Wp+y2LB;
 polf2(4,FrameBorder);
 FrameBorder[0][0]=(float)x2Wp-x2RT; FrameBorder[0][1]=(float)y1Wp+y2LB;
 FrameBorder[1][0]=(float)x2Wp-x1RT;  FrameBorder[1][1]=(float)y1Wp+y1LB;
 FrameBorder[2][0]=(float)x2Wp-x1RT;  FrameBorder[2][1]=(float)y2Wp-y1RT;
 FrameBorder[3][0]=(float)x2Wp-x2RT; FrameBorder[3][1]=(float)y2Wp-y2RT;
 polf2(4,FrameBorder);
 
 color(GLW_FRAMECOLOR_DARK);
 FrameBorder[0][0]=(float)x1Wp+x2LB; FrameBorder[0][1]=(float)y2Wp-y2RT;
 FrameBorder[1][0]=(float)x1Wp+x1LB;  FrameBorder[1][1]=(float)y2Wp-y1RT;
 FrameBorder[2][0]=(float)x2Wp-x1RT;  FrameBorder[2][1]=(float)y2Wp-y1RT;
 FrameBorder[3][0]=(float)x2Wp-x2RT; FrameBorder[3][1]=(float)y2Wp-y2RT;
 polf2(4,FrameBorder);
 FrameBorder[0][0]=(float)x1Wp+x2LB; FrameBorder[0][1]=(float)y1Wp+y2LB;
 FrameBorder[1][0]=(float)x1Wp+x1LB;  FrameBorder[1][1]=(float)y1Wp+y1LB;
 FrameBorder[2][0]=(float)x1Wp+x1LB;  FrameBorder[2][1]=(float)y2Wp-y1RT;
 FrameBorder[3][0]=(float)x1Wp+x2LB; FrameBorder[3][1]=(float)y2Wp-y2RT;
 polf2(4,FrameBorder);
 sleep(0); XSync((Display *)getXdpy(),False);

 FontHeight=2*getheight();
#define MAXTXTW strwidth("000000000000\0")
 MinSizeXY = 3*FontHeight+6; MinSizeXY = (MinSizeXY > MAXTXTW)?MinSizeXY:MAXTXTW;
#undef  MAXTXTW
 SizeX = MinSizeXY* (float)(XWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT)/
                    (float)(YWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT);
 SizeY = (MinSizeXY < SizeX)?MinSizeXY:(MinSizeXY* 
         (float)(YWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT)/
	 (float)(XWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT));
 SizeX = SizeY* (float)(XWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT)/
                    (float)(YWp-GLW_FRAMEWIDTH_LB-GLW_FRAMEWIDTH_RT)+GLW_FRAMEWIDTH_RT;
 SizeY -= 6; SizeY /= 3;


 CursorXValue.x1 = XWp -SizeX;
 CursorXValue.y1 = YWp - GLW_FRAMEWIDTH_RT-4 - SizeY;
 CursorXValue.x2 = XWp - GLW_FRAMEWIDTH_RT-4;
 CursorXValue.y2 = YWp - GLW_FRAMEWIDTH_RT-4;
 CursorXValue.bgcolor = GLW_LABELCOLOR_1;
 CursorXValue.bgcolor = BLACK;
 CursorXValue.fgcolor = WHITE;
 CursorXValue.Reverse = &(Plot.Reverse);
 if(CursorXValue.Text == NULL)CursorXValue.Text=(char *)calloc(20,sizeof(char));
 DrawLabel(CursorXValue);
 
 CursorYValue.x1 = CursorXValue.x1;
 CursorYValue.y1 = CursorXValue.y1 -1 - SizeY;
 CursorYValue.x2 = XWp - GLW_FRAMEWIDTH_RT-4;
 CursorYValue.y2 = CursorXValue.y1 -1;
 CursorYValue.bgcolor = BLACK;
 CursorYValue.fgcolor = WHITE;
 CursorYValue.Reverse = &(Plot.Reverse);
 if(CursorYValue.Text == NULL)CursorYValue.Text=(char *)calloc(20,sizeof(char));
 DrawLabel(CursorYValue);

 CursorZValue.x1 = CursorYValue.x1;
 CursorZValue.y1 = CursorYValue.y1 -1 - SizeY;
 CursorZValue.x2 = XWp - GLW_FRAMEWIDTH_RT-4;
 CursorZValue.y2 = CursorYValue.y1 -1;
 CursorZValue.bgcolor = BLACK;
 CursorZValue.fgcolor = WHITE;
 CursorZValue.Reverse = &(Plot.Reverse);
 if(CursorZValue.Text == NULL)CursorZValue.Text=(char *)calloc(20,sizeof(char));
 DrawLabel(CursorZValue);

 if( ZMinSlider )redraw_widgets();
 else {
    ZMinFactor = 0.00;
    ZMinSlider = create_gl_slider( GLW_FRAMEWIDTH_LB, GLW_FRAMEWIDTH_LB*3.00/5.00,
                                   400.0, 12.0, 20.0,
				   GLW_FRAMECOLOR, BLACK, GLW_FRAMECOLOR_DARK, GLW_LABELCOLOR_1,
				   BLACK, GLW_FRAMECOLOR,
				   0.0, 100.0, 1.0, 0.0,
				   NULL, " Min Z (log10): %g%%", ZScaleChangeCbk);
    }

 if( ZMaxSlider == NULL ) {
    ZMaxFactor = 100.00;
    ZMaxSlider = create_gl_slider( GLW_FRAMEWIDTH_LB, GLW_FRAMEWIDTH_LB/5.00,
                                   400.0, 12.0, 20.0,
				   GLW_FRAMECOLOR, BLACK, GLW_FRAMECOLOR_DARK, GLW_LABELCOLOR_1,
				   BLACK, GLW_FRAMECOLOR,
				   0.0, 100.0, 1.0, 100.0,
				   NULL, " Max Z (log10): %g%%", ZScaleChangeCbk);
    }
 FlushDrawings; 
}

void ZScaleChangeCbk ( gl_slider *sl, double value) {

  if ( sl == ZMinSlider ) { ZMinFactor = value; ZScaleChange = 1; }
  if ( sl == ZMaxSlider ) { ZMaxFactor = value; ZScaleChange = 1; }
}

Int32 GLWContourInit(void){

  Int32 dev;

  minsize(GLW_XSIZE,GLW_YSIZE);
  dev=winopen("GASPware-->CMAT plot");
  SetMouseShape(XC_left_ptr);
  viewport(0,GLW_XSIZE-1,0,GLW_YSIZE-1);
  MapGLWcolors();
  LoadGLWfont();
  winset(dev);
  reshapeviewport();
  getsize(&XWp, &YWp);
  Menu=defpup("  Color Scale  %t|Linear|Square root|Cubic root|Log10|Mixed|Atan|Reverse Colors");
  if(Plot.ZScaleType == 0 )Plot.ZScaleType = ZLIN;
  setpup(Menu,Plot.ZScaleType,PUP_GREY); Plot.Reverse = LO_SRC;

  doublebuffer();
  gconfig();
  
  Marker = CreateMarker(LMARKERl,LMARKERL,GLW_MARKERCOLOR);
  if( Marker ){ Marker->Next = CreateMarker(RMARKERl,RMARKERL,GLW_MARKERCOLOR);
  if( Marker->Next ){Marker->Next->Next = CreateMarker(UMARKERl,UMARKERL,GLW_MARKERCOLOR);
  if( Marker->Next->Next )Marker->Next->Next->Next = CreateMarker(OMARKERl,OMARKERL,GLW_MARKERCOLOR);}}   
     
  
  qdevice(MOUSEX);
  qdevice(MOUSEY); 
  qdevice(LEFTMOUSE);
  qdevice(MIDDLEMOUSE);
  qdevice(MENUBUTTON);
  qdevice(LEFTARROWKEY);
  qdevice(RIGHTARROWKEY);
  qdevice(UPARROWKEY);
  qdevice(DOWNARROWKEY);
  qdevice(KEYBD);
  qdevice(REDRAW);
  qdevice(WINQUIT);
  qdevice(LEFTCTRLKEY);
  qdevice(RIGHTCTRLKEY);
  qdevice(LEFTSHIFTKEY);
  qdevice(RIGHTSHIFTKEY);
  qdevice(ESCKEY);
  qdevice(DELKEY);
  qdevice(BACKSPACEKEY);
  unqdevice(INPUTCHANGE);
  qreset();
  _global_BS = DoesBackingStore( DefaultScreenOfDisplay((Display *)getXdpy()) ) == Always;
  return dev;
 }

void ReshapeWindow ( void ){

   reshapeviewport();
   getsize(&XWp, &YWp);
   viewport( (Screencoord) 0,(Screencoord) (XWp-1),(Screencoord) 0,(Screencoord) (YWp-1));
   ortho2( (Coord) 0,(Coord) (XWp-1),(Coord) 0,(Coord) (YWp-1) );
}


void MovePlotRegion ( Int32 Where ){

  struct MarkerStruct *mk;
  Int32 m, w;

  switch ( Where ){
       
       case MOVE_UP:{ m = (Plot.Ymax - Plot.Ymin + 1)/2;
                      m = ( m > 0)?m:1;
		      w = Plot.Ymax - Plot.Ymin;
		      Plot.Ymax = ( (Plot.Ymax + m) > (DataYmax-1) )?(DataYmax-1):(Plot.Ymax + m);
		      Plot.Ymin = Plot.Ymax - w;
		      break; }

       case MOVE_DOWN:{ m = (Plot.Ymax - Plot.Ymin + 1)/2; 
                      m = ( m > 0)?m:1;
		      w = Plot.Ymax - Plot.Ymin;
		      Plot.Ymin = ( (Plot.Ymin - m) < 0 )?0:(Plot.Ymin - m);
		      Plot.Ymax = Plot.Ymin + w;
		      break; }

       case MOVE_RIGHT:{ m = (Plot.Xmax - Plot.Xmin + 1)/2; 
                      m = ( m > 0)?m:1;
		      w = Plot.Xmax - Plot.Xmin;
                      Plot.Xmax = ( (Plot.Xmax + m) > (DataXmax-1) )?(DataXmax-1):(Plot.Xmax + m);
		      Plot.Xmin = Plot.Xmax - w;
		      break; }

       case MOVE_LEFT:{ m = (Plot.Xmax - Plot.Xmin + 1)/2; 
                      m = ( m > 0)?m:1;
		      w = Plot.Xmax - Plot.Xmin;
                      Plot.Xmin = ( (Plot.Xmin - m) < 0 )?0:(Plot.Xmin - m);
		      Plot.Xmax = Plot.Xmin + w;
		      break; }
       case MOVE_CENTER:{ m = (Plot.Xmax - Plot.Xmin + 1)/2; 
		      w = Plot.Xmax - Plot.Xmin;
                      Plot.Xmin = ( (XYZ.x - m) < 0 )?0:(XYZ.x - m);
		      Plot.Xmax = Plot.Xmin + w;
		      Plot.Xmax = ( Plot.Xmax  > (DataXmax-1) )?(DataXmax-1):Plot.Xmax ;
		      Plot.Xmin = Plot.Xmax - w;
		        m = (Plot.Ymax - Plot.Ymin + 1)/2; 
		      w = Plot.Ymax - Plot.Ymin;
                      Plot.Ymin = ( (XYZ.y - m) < 0 )?0:(XYZ.y - m);
		      Plot.Ymax = Plot.Ymin + w;
		      Plot.Ymax = ( Plot.Ymax  > (DataYmax-1) )?(DataYmax-1):Plot.Ymax ;
		      Plot.Ymin = Plot.Ymax - w;			
		      break; }
       default: return;
       }
   RedrawAllMarkers();
   mk = Marker;
   while( mk ){ mk->On = MARKER_OFF; mk = mk->Next; }
   DrawPlot();
 }


#define REDRAW_BANANAS 	 Selected = NULL; b = Banana; while( b ){ BP_DrawBanana( b ); b = b->Next; }
#define REDRAW_BANANAS_SelKeep 	 b = Banana; while( b ){ BP_DrawBanana( b ); b = b->Next;} if(Selected){BP_SelectBanana(Selected);BP_SelectPoint(Selected->s);}


void HandleKey( char key ){

  struct MarkerStruct *m;
  char DoExpand;
  Int32 x,y;
  BStruct b;

  switch ( key ){

      case 'E': case 'e':{ m = Marker; DoExpand = False; RedrawAllMarkers();
                           while( m ){ if( m->On ){ switch ( m->KindL ){
			                               case LMARKERL:{ Plot.Xmin = m->ChannelPos; break; }
						       case RMARKERL:{ Plot.Xmax = m->ChannelPos; break; }
						       case UMARKERL:{ Plot.Ymin = m->ChannelPos; break; }
						       case OMARKERL:{ Plot.Ymax = m->ChannelPos; break; }
						       }
						     DoExpand = True;}
			                m->On = MARKER_OFF;
					m = m->Next;}
					if(DoExpand){ DOUBLEBUFF_ON DrawPlot(); REDRAW_BANANAS; DOUBLEBUFF_OFF}
			    break;}
      case 'F': case 'f':{ DOUBLEBUFF_ON
                           RedrawAllMarkers();
                           m = Marker; while( m ){ m->On = MARKER_OFF; m = m->Next;}
                           Plot.Xmin = Plot.Ymin = 0;
			   Plot.Xmax = DataXmax-1; Plot.Ymax = DataYmax-1;
			   DrawPlot();
	                   Selected = NULL;
	                   b = Banana;
	                   while( b ){ BP_DrawBanana( b ); b = b->Next; }
			   DOUBLEBUFF_OFF
			   break;}
			   
      case 'Q': case 'q':{  DoQuit = True; break;}
      
      case 'A': case 'a':{ 
                   getorigin(&x,&y);
                   x=getvaluator(MOUSEX)-x;
                   y=getvaluator(MOUSEY)-y;
	           if( GetXYZ(x,y) ){ ShowPosition(); BP_AddPoint(); qreset ();}
		   break;}
      case 'D': case 'd':{ 
                   getorigin(&x,&y);
                   x=getvaluator(MOUSEX)-x;
                   y=getvaluator(MOUSEY)-y;
	           if( GetXYZ(x,y) ){ ShowPosition(); BP_DeletePoint(); }
		   break;}
      case 'H': case 'h': case '?':{ PrintHelp(); break;}
      case 'K': case 'k':{ 
                   getorigin(&x,&y);
                   x=getvaluator(MOUSEX)-x;
                   y=getvaluator(MOUSEY)-y;
	           if( GetXYZ(x,y) ){
		     ShowPosition();
		     if( Selected ){
		       BP_UnselectBanana( Selected );
		       BP_DrawBanana( Selected );
		       BP_DestroyBanana();}
		     }
		   break;}
      case 'M': case 'm':{ 
                   getorigin(&x,&y);
                   x=getvaluator(MOUSEX)-x;
                   y=getvaluator(MOUSEY)-y;
	           if( GetXYZ(x,y) ){ ShowPosition(); BP_MovePoint(); }
		   break;}
      case 'N': case 'n':{ 
                   getorigin(&x,&y);
                   x=getvaluator(MOUSEX)-x;
                   y=getvaluator(MOUSEY)-y;
	           if( GetXYZ(x,y) ){ ShowPosition(); BP_NewBanana(); }
		   break;}
/*      case 'H': case 'h':{ 
                   getorigin(&x,&y);
                   x=getvaluator(MOUSEX)-x;
                   y=getvaluator(MOUSEY)-y;
	           if( GetXYZ(x,y) ){
		     ShowPosition();
		     if( Selected ){
		       BP_UnselectBanana( Selected );
		       BP_DrawBanana( Selected );
		       Selected->Active = False;
		       Selected = NULL;}
		     }
		   break;}*/
      case 'T': case 't':{ BP_TypeBanana (); break;}
      case 'R': case 'r':{ 
                 if( Banana ){
                     b = Banana;
		     while ( b->Next ) b = b->Next;
		     b->Next = BP_ReadBanana();
		     if( b->Next )BP_DrawBanana( b->Next );
		     }
		  else {
		     Banana = BP_ReadBanana();
		     if( Banana ) BP_DrawBanana( Banana );
		     }
                 break;}
      case 'W': case 'w':{ BP_WriteBanana (); break;}
      case 'Y': case 'y':{ CommonIntercept (); break;}
      case 'S': case 's':
                        {
                           if( Selected )
                           {
                              fprintf(stderr,"------------------------------------------------------------------\n");
                              fprintf(stderr," Calculating statistics for currently selected banana ....\n"); 
                              BP_Sums( Selected );
                              fprintf(stderr,"   Surface : %.1lf    Integral : %.3lf\n", Selected->Surface, Selected->Integral );
                              fprintf(stderr,"------------------------------------------------------------------\n");
                           } 
                           break;
                         }

      }
}

void  contourplot_(Int32 *data, Int32 *resx, Int32 *resy){

  Int32 dev;
  Int16 val;
  Int32 x,y;
  char testdev;
  BStruct b;
  struct MarkerStruct *m;
  Int32 x1Rex, y1Rex, xRex, yRex;


  ZMinSlider = NULL;
  ZMaxSlider = NULL;
  DataXmax = *resx; DataYmax = *resy;
  DoQuit = False;
  Win = GLWContourInit();
  DOUBLEBUFF_ON
  DrawFrame(); 
  MapPlots();
  XMinLabel.Reverse = &(Plot.Reverse);
  XMaxLabel.Reverse = &(Plot.Reverse);
  YMinLabel.Reverse = &(Plot.Reverse);
  YMaxLabel.Reverse = &(Plot.Reverse);
  ZMinLabel.Reverse = &(Plot.Reverse);
  ZMaxLabel.Reverse = &(Plot.Reverse);
  DrawPlotFrame(MatPlot); DrawPlotFrame(XpPlot); DrawPlotFrame(YpPlot);
  MakeData(data);
  DrawPlot();
  REDRAW_BANANAS;
  fflush(stdin);
  ZScaleChange = 0;

  swapbuffers();

  DrawFrame(); 
  MapPlots();
  XMinLabel.Reverse = &(Plot.Reverse);
  XMaxLabel.Reverse = &(Plot.Reverse);
  YMinLabel.Reverse = &(Plot.Reverse);
  YMaxLabel.Reverse = &(Plot.Reverse);
  ZMinLabel.Reverse = &(Plot.Reverse);
  ZMaxLabel.Reverse = &(Plot.Reverse);
  DrawPlotFrame(MatPlot); DrawPlotFrame(XpPlot); DrawPlotFrame(YpPlot);
  MakeData(data);
  DrawPlot();
  REDRAW_BANANAS;
  fflush(stdin);
  DOUBLEBUFF_OFF
  
  while ( dev = qread(&val) ) {

    testdev=0;
    while ( qtest()&&(testdev<3) ){
     testdev++;
     switch(dev) {
       case KEYBD: { testdev=5; break;}
       case REDRAW: { testdev=5; break;}
       case LEFTMOUSE: { testdev=5; break;}
       case MOUSEX: { dev=qread(&val); break;}
       case MOUSEY: { dev=qread(&val); break;}
       }
     }
    switch( dev ) {
      case KEYBD: {
        HandleKey( val );
	if( DoQuit ){ 
	  if( ZMaxSlider )destroy_gl_slider(ZMaxSlider);
	  if( ZMinSlider )destroy_gl_slider(ZMinSlider);
	  winclose(Win); return;
	  }
        break;}

      case REDRAW: {

	DOUBLEBUFF_ON
redraw:	ReshapeWindow();
	if( qtest() )
	{
	    dev = qread( &val );
	    if( dev == REDRAW )  {  usleep(10000); goto redraw; }
	}

	DrawFrame();
	
	swapbuffers();

	if( qtest() )
	{
	    dev = qread( &val );
	    if( dev == REDRAW )  {  usleep(10000); goto redraw; }
	}
	
	DrawFrame();
  	MapPlots();
	DrawPlotFrame(MatPlot); DrawPlotFrame(XpPlot); DrawPlotFrame(YpPlot);


	swapbuffers(); 
	
	if( qtest() )
	{
	    dev = qread( &val );
	    if( dev == REDRAW ) {  usleep(10000); goto redraw; }
	}


	DrawFrame();
	DrawPlotFrame(MatPlot); DrawPlotFrame(XpPlot); DrawPlotFrame(YpPlot);
	swapbuffers();
	DrawPlot();
	REDRAW_BANANAS_SelKeep;
	RedrawAllMarkers();
	DOUBLEBUFF_OFF

	FlushDrawings;
	break;}

     case ESCKEY: { 
	if( Selected )BP_UnselectBanana( Selected );
	Selected = NULL;
	break; }

     case LEFTMOUSE: { 
#define CTRL_LEFTMouse ( getbutton(LEFTMOUSE) && ( getbutton(LEFTCTRLKEY) || getbutton(RIGHTCTRLKEY) ) )
      if(CTRL_LEFTMouse){
        getorigin(&x,&y);
        x=getvaluator(MOUSEX)-x;
        y=getvaluator(MOUSEY)-y;
	if( GetXYZ(x,y) ) {
	   x1Rex = xRex= x; y1Rex = yRex = y;
	   do {
	     getorigin(&x,&y);
	     x=getvaluator(MOUSEX)-x;
	     y=getvaluator(MOUSEY)-y;
	     if( GetXYZ (x,y) ){ 
	        DrawXORFrame( x1Rex, y1Rex, xRex, yRex );
		xRex= x; yRex = y;
		DrawXORFrame( x1Rex, y1Rex, xRex, yRex );
		ShowPosition();
		}
	   } while ( CTRL_LEFTMouse && GetXYZ (x,y) );
	   DrawXORFrame( x1Rex, y1Rex, xRex, yRex );
	   if( (x1Rex != xRex) && (y1Rex != yRex) ){

	       GetXYZ ( x1Rex, y1Rex );  x1Rex = XYZ.x; y1Rex = XYZ.y;
	       GetXYZ ( xRex, yRex );  xRex = XYZ.x; yRex = XYZ.y;
	       
               m = Marker; while( m ){ m->On = MARKER_OFF; m = m->Next;}
	       
	       Plot.Xmin = ( x1Rex < xRex )?x1Rex:xRex;
	       Plot.Xmax = ( x1Rex > xRex )?x1Rex:xRex;
	       Plot.Ymin = ( y1Rex < yRex )?y1Rex:yRex;
	       Plot.Ymax = ( y1Rex > yRex )?y1Rex:yRex;
	       DOUBLEBUFF_ON
	       DrawPlot(); REDRAW_BANANAS; FlushDrawings;
	       DOUBLEBUFF_OFF
	       }
	    }
	  break; }
#undef CTRL_LEFTMouse	      
       else {
        update_widgets(val); FlushDrawings;
  	if( ZScaleChange ){
DOUBLEBUFF_ON
  	    MapPlots();
  	    DrawPlotFrame(MatPlot); DrawPlotFrame(XpPlot); DrawPlotFrame(YpPlot);
  	    DrawPlot();
  	    REDRAW_BANANAS_SelKeep;
  	    RedrawAllMarkers();
  	    FlushDrawings;
  	    ZScaleChange = 0;
	    update_widgets(val); FlushDrawings;
DOUBLEBUFF_OFF
  	    }
        getorigin(&x,&y);
        x=getvaluator(MOUSEX)-x;
        y=getvaluator(MOUSEY)-y;
	if( GetXYZ(x,y) )ShowPosition();        
	if( getbutton(LEFTSHIFTKEY) || getbutton(RIGHTSHIFTKEY) ){
	    while( ( getbutton(LEFTSHIFTKEY) || getbutton(RIGHTSHIFTKEY) ) && getbutton(LEFTMOUSE) );
	    if( Selected )BP_UnselectBanana( Selected );
	    Selected = BP_ClosestBanana();
	    if(Selected){
	      BP_SelectBanana( Selected );
	      Selected->s = BP_ClosestPoint( Selected );
	      BP_SelectPoint( Selected->s );
	      }
	    }
	if( Selected ){
	   if( Selected->s )BP_UnselectPoint( Selected->s );
	   Selected->s = BP_ClosestPoint( Selected );
	   BP_SelectPoint( Selected->s );
	   }
/*                if( Selected )
                {
                  if( BP_IsPointInside( Selected, XYZ.x, XYZ.y ) ) printf("  point (%d %d) inside selected banana\n", XYZ.x, XYZ.y);
                }
*/
	break;}
	}
	break;
	
     case MIDDLEMOUSE: { 
        getorigin(&x,&y);
        x=getvaluator(MOUSEX)-x;
        y=getvaluator(MOUSEY)-y;
        if( (getbutton(LEFTCTRLKEY) || getbutton(RIGHTCTRLKEY)) && GetXYZ(x,y) ){
	  SetMouseShape(XC_diamond_cross);
	  ShowPosition();
	  while ( (getbutton(LEFTCTRLKEY) || getbutton(RIGHTCTRLKEY)) && 
	                            GetXYZ(x,y) && getbutton(MIDDLEMOUSE) ){
                getorigin(&x,&y);
                x=getvaluator(MOUSEX)-x;
                y=getvaluator(MOUSEY)-y;
		ShowPosition(); 
		}
	   DOUBLEBUFF_ON
           MovePlotRegion ( MOVE_CENTER   ); REDRAW_BANANAS;
	   DOUBLEBUFF_OFF
	   }
	else {
	if( getbutton(LEFTSHIFTKEY) || getbutton(RIGHTSHIFTKEY) ){
	  while( getbutton(MIDDLEMOUSE) && ( getbutton(LEFTSHIFTKEY) || getbutton(RIGHTSHIFTKEY) ) ){
                getorigin(&x,&y);
                x=getvaluator(MOUSEX)-x;
                y=getvaluator(MOUSEY)-y;
	        if( GetXYZ(x,y) )ShowPosition();}
	  val = 'a'; HandleKey( val );
	  }
	else {
	 do {
	  SetMouseShape(XC_hand1);
	  val = 'm'; HandleKey( val );
	  }while( getbutton(MIDDLEMOUSE) );
         }
	 SetMouseShape(XC_left_ptr); 
	}
	break;}
	
     case MOUSEX:
     case MOUSEY: { 
        if(getbutton(LEFTMOUSE)!=1)break;
        getorigin(&x,&y);
        x=getvaluator(MOUSEX)-x;
        y=getvaluator(MOUSEY)-y;
	if( GetXYZ(x,y) )ShowPosition();
	break;}

     case MENUBUTTON:  { 
                        DOUBLEBUFF_ON
			switch(dopup(Menu)) {
                              case 1: {Plot.ZScaleType = ZLIN; 
			               setpup(Menu,1,PUP_GREY);
				       setpup(Menu,2,PUP_NONE);
				       setpup(Menu,3,PUP_NONE);
			               setpup(Menu,4,PUP_NONE);
			               setpup(Menu,5,PUP_NONE);
			               setpup(Menu,6,PUP_NONE);
				       if( _global_BS ) {
				       RedrawAllMarkers();DrawPlot();
				       REDRAW_BANANAS_SelKeep;
				       RedrawAllMarkers();}break;}
			      case 2: {Plot.ZScaleType = ZSQRT; 
			               setpup(Menu,1,PUP_NONE);
				       setpup(Menu,2,PUP_GREY);
				       setpup(Menu,3,PUP_NONE);
			               setpup(Menu,4,PUP_NONE);
			               setpup(Menu,5,PUP_NONE);
			               setpup(Menu,6,PUP_NONE);
				       if( _global_BS ) {
				       RedrawAllMarkers();DrawPlot();
				       REDRAW_BANANAS_SelKeep;
				       RedrawAllMarkers();}break;}
			      case 3: {Plot.ZScaleType = ZCBRT;
			               setpup(Menu,1,PUP_NONE);
				       setpup(Menu,2,PUP_NONE);
				       setpup(Menu,3,PUP_GREY);
			               setpup(Menu,4,PUP_NONE);
			               setpup(Menu,5,PUP_NONE);
			               setpup(Menu,6,PUP_NONE);
				       if( _global_BS ) {
				       RedrawAllMarkers();DrawPlot();
				       REDRAW_BANANAS_SelKeep;
				       RedrawAllMarkers();}break;}
			      case 4: {Plot.ZScaleType = ZLOG;
			               setpup(Menu,1,PUP_NONE);
				       setpup(Menu,2,PUP_NONE);
				       setpup(Menu,3,PUP_NONE);
				       setpup(Menu,4,PUP_GREY);
			               setpup(Menu,5,PUP_NONE);
			               setpup(Menu,6,PUP_NONE);
				       if( _global_BS ) {
				       RedrawAllMarkers();DrawPlot();
				       REDRAW_BANANAS_SelKeep;
				       RedrawAllMarkers();}break;}
			      case 5: {Plot.ZScaleType = ZMIX;
			               setpup(Menu,1,PUP_NONE);
				       setpup(Menu,2,PUP_NONE);
				       setpup(Menu,3,PUP_NONE);
				       setpup(Menu,4,PUP_NONE);
			               setpup(Menu,5,PUP_GREY);
			               setpup(Menu,6,PUP_NONE);
				       if( _global_BS ) {
				       RedrawAllMarkers();DrawPlot();
				       REDRAW_BANANAS_SelKeep;
				       RedrawAllMarkers();}break;}
			      case 6: {Plot.ZScaleType = ZATAN;
			               setpup(Menu,1,PUP_NONE);
				       setpup(Menu,2,PUP_NONE);
				       setpup(Menu,3,PUP_NONE);
				       setpup(Menu,4,PUP_NONE);
				       setpup(Menu,5,PUP_NONE);
			               setpup(Menu,6,PUP_GREY);
				       if( _global_BS ) {
				       RedrawAllMarkers();DrawPlot();
				       REDRAW_BANANAS_SelKeep;
				       RedrawAllMarkers();}break;}
			      case 7: {Plot.Reverse = ( Plot.Reverse == LO_SRC )?LO_NSRC:LO_SRC;
			               ClearDrawArea();
				       if ( _global_BS ) {
				       RedrawAllMarkers();
				       DrawColorScale();DrawPlot();
				       REDRAW_BANANAS_SelKeep;
				       RedrawAllMarkers();}break;}
			      }
                         DOUBLEBUFF_OFF
			 break;}
     case UPARROWKEY:    { 
 	if( getbutton(LEFTCTRLKEY) || getbutton(RIGHTCTRLKEY) ){
	      DOUBLEBUFF_ON
	      MovePlotRegion ( MOVE_UP   ); REDRAW_BANANAS;
	      DOUBLEBUFF_OFF
	      }
	else DrawMarker('O');
        break;}
     case DOWNARROWKEY:  { 
 	if( getbutton(LEFTCTRLKEY) || getbutton(RIGHTCTRLKEY) ){
	      DOUBLEBUFF_ON
	      MovePlotRegion ( MOVE_DOWN   ); REDRAW_BANANAS;
	      DOUBLEBUFF_OFF
	      }
	else DrawMarker('U');
        break;}
     case RIGHTARROWKEY: { 
 	if( getbutton(LEFTCTRLKEY) || getbutton(RIGHTCTRLKEY) ){
	      DOUBLEBUFF_ON
	      MovePlotRegion ( MOVE_RIGHT   ); REDRAW_BANANAS;
	      DOUBLEBUFF_OFF
	      }
	else DrawMarker('R');
        break;}
     case LEFTARROWKEY:  { 
 	if( getbutton(LEFTCTRLKEY) || getbutton(RIGHTCTRLKEY) ){
	      DOUBLEBUFF_ON
	      MovePlotRegion ( MOVE_LEFT   ); REDRAW_BANANAS;
	      DOUBLEBUFF_OFF
	      }
	else DrawMarker('L');
        break;}
     
     case DELKEY: 
     case BACKSPACEKEY:{ while( ( getbutton( BACKSPACEKEY ))||( getbutton(DELKEY )) );
                 val = 'd'; HandleKey( val ); qreset(); break; }
     default: break;
      }
    }  
      
}

int GetString( unsigned char *String ) {


  Int32 dev, imore;
  Int16 val;
  char testdev;
  unsigned char c, c3[3];
  BStruct b;
  int Length;
  
  
  fflush(stdin);
  fflush(stdout);
  ISLSetTerminal( RAW );


  imore = 0;
  while ( imore == 0 ) {
   if( qtest () ){
    dev = qread(&val);
    switch( dev ) {
      case KEYBD: { c = (unsigned char )val;
                   if ( c == 13 ) c = 10;
                   imore = ISLPutInput( &c, 1);
		   break; }

      case LEFTARROWKEY: { c3[0] = 27; c3[1] =91; c3[2] = 68;
                   dev = qread(&val);
		   imore = ISLPutInput( &c3[0], 3);
		   break; }

      case RIGHTARROWKEY: { c3[0] = 27; c3[1] =91; c3[2] = 67;
                   dev = qread(&val);
		   imore = ISLPutInput( &c3[0], 3);
		   break; }

      case REDRAW: {
        DOUBLEBUFF_ON
	ReshapeWindow();
	DrawFrame();
  	MapPlots();
	DrawPlotFrame(MatPlot); DrawPlotFrame(XpPlot); DrawPlotFrame(YpPlot);
	DrawPlot();
	REDRAW_BANANAS_SelKeep;
	RedrawAllMarkers();
	FlushDrawings;
        DOUBLEBUFF_OFF
	break;}
     }
    }
    else {
    ISLGetInput();
    imore = ISLPutInput( NULL, 0);
    }
  }
  
  ISLGetString( String, &Length);
  ISLSetTerminal(RESTORE);
  return Length;
 }


/*  HELP  HELP  HELP  HELP  HELP  HELP  HELP  HELP  HELP  */

void PrintHelp ( void ) {


printf("(1) Setting display limits and properties\n");
printf("     ARROWS            - set the Left, Right, Up, Down markers for expansion\n");
printf("     E                 - expand using the limits set by markers\n");
printf("     F                 - full display of the matrix\n");
printf("     CTRL+LEFTMOUSE    - defines and expand inside a rectangular area\n");
printf("     CTRL+MIDDLEMOUSE  - center the display on the selected point preserving XY size\n");
printf("     CTRL+ARROWS       - shift the display to Left, Right, Up, Down preserving XY size\n");
printf("     RIGHTMOUSE        - set Z color scale\n");
printf("\n");
printf("(2) Banana commands\n");
printf("     SHIFT+LEFTMOUSE   - select banana\n");
printf("     ESC               - unselect banana\n");
printf("     LEFTMOUSE         - select < The Current Point> in the selected banana\n");
printf("  M, MIDDLEMOUSE       - move < The Current Point>\n");
printf("  A, SHIFT+MIDDLEMOUSE - add a banana point AFTER < The Current Point>\n");
printf("                         If ther is no banana selected, create a new one\n");
printf("  N                    - create a new banana and add the first point\n");
printf("  D,DEL, BACKSPACE     - delete < The Current Point>\n");
printf("  K                    - kill selected banana\n");
printf("  R                    - read banana from file\n");
printf("  W                    - write the selected banana to file\n");
printf("  T                    - type on terminal the (x,y) values of the selected banana\n");
printf("\n");
printf("(3) Other commands\n");
printf("  H,?                  - print this help\n");
printf("  Q                    - close graphics display and return to CMAT prompt\n");
printf("\n");
}
