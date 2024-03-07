/*
 *    Ygl: Run GL programs with standard X11 routines.
 *    (C) Fred Hucht 1993-96
 *    EMail: fred@thp.Uni-Duisburg.DE
 */

static char vcid[] = "$Id: font.c,v 4.2 1997-07-07 11:09:39+02 fred Exp $";

#include "header.h"

void loadXfont(Int32 id, Char8 *name) {
  int i;
  XFontStruct *fs;
  const char * MyName = "loadXfont";
  I(MyName);
  if(NULL == (fs = XLoadQueryFont(D, name))) {
    Yprintf(MyName, "can't find font '%s'.\n", name);
    return;
  }

  if(Ygl.Fonts == NULL) { /* initialize */
    i = Ygl.LastFont = 0;
    Ygl.Fonts = (YglFont*)malloc(sizeof(YglFont));
  } else {
    for(i = Ygl.LastFont; i >= 0 && Ygl.Fonts[i].id != id; i--);
    if(i < 0) { /* not found */
      i = ++Ygl.LastFont;
      Ygl.Fonts = (YglFont*)realloc(Ygl.Fonts,
				    (Ygl.LastFont + 1) * sizeof(YglFont));
    }
  }
  
  if(Ygl.Fonts == NULL) {
    Yprintf(MyName, "can't allocate memory for font '%s'.\n", name);
    exit(-1);
  }
  
  Ygl.Fonts[i].fs = fs;
  Ygl.Fonts[i].id = id;
#ifdef DEBUG
  fprintf(stderr, 
	  "loadXfont: name = '%s', fs = 0x%x, id = %d.\n", 
	  name, fs, id);
#endif
}

void font(Int16 id) {
  int i = Ygl.LastFont;
  const char * MyName = "font";
  I(MyName);
  while(i > 0 && Ygl.Fonts[i].id != id) i--;
  W->font = i;
#ifdef DEBUG
  fprintf(stderr, "font: id = %d, W->font = %d, fid = 0x%x.\n",
	  id, i, Ygl.Fonts[i].fs->fid);
#endif
  XSetFont(D, W->chargc, Ygl.Fonts[i].fs->fid);
  
  /* if(YglFontStruct != NULL) XFreeFont(D,YglFontStruct);
   * YglFontStruct = XQueryFont(D, YglFonts[i].font);
   */
}

Int32 getfont(void) {
  const char * MyName = "getfont";
  I(MyName);
  return Ygl.Fonts[W->font].id;
}

void getfontencoding(char *r) {
  XFontStruct *fs;
  XFontProp *fp;
  int i;
  Atom fontatom;
  char *name, *np = NULL, *rp = r;
  
  const char * MyName = "getfontencoding";
  I(MyName);
  fs = Ygl.Fonts[W->font].fs;
  fontatom = XInternAtom(D, "FONT", False);
  
  for (i = 0, fp = fs->properties; i < fs->n_properties; i++, fp++) {
    if (fp->name == fontatom) {
      np = name = XGetAtomName(D, fp->card32);
      i = 0;
      while(i < 13 && *np != 0) if(*np++ == '-') i++;
      do {
	if(*np != '-') *rp++ = *np;
      }
      while(*np++ != 0);
      XFree(name);
    }
  }
  if(np == NULL) {
    Yprintf(MyName, "can't determine fontencoding.\n");
    *r = '\0';
  }
}

Int32 getheight(void) {
  const char * MyName = "getheight";
  I(MyName);
  return(Ygl.Fonts[W->font].fs->ascent +
	 Ygl.Fonts[W->font].fs->descent);
}

Int32 getdescender(void) {
  const char * MyName = "getdescender";
  I(MyName);
  return Ygl.Fonts[W->font].fs->descent;
}

Int32 strwidth(Char8 *string) {
  const char * MyName = "strwidth";
  I(MyName);
  return XTextWidth(Ygl.Fonts[W->font].fs, string, strlen(string));
}

void charstr(Char8 *Text) {
  const char * MyName = "charstr";
  I(MyName);
  if(!(W->rgb || Ygl.GC)) { /* set text color to active color */
    XSetForeground(D, W->chargc, YGL_COLORS(W->color));
  }
  XDrawString(D, W->draw, W->chargc, X(W->xc), Y(W->yc), Text, strlen(Text));
  W->xc += strwidth(Text) / W->xf;
  F;
}
