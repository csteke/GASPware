/*
 *    Ygl: Run GL programs with standard X11 routines.
 *    (C) Fred Hucht 1993-96
 *    EMail: fred@thp.Uni-Duisburg.DE
 */

#if defined(__GNUC__) || defined(__clang__)
#define YGL_UNUSED __attribute__((unused))
#else
#define YGL_UNUSED
#endif

static char vcid[] YGL_UNUSED = "$Id: font.c,v 4.2 1997-07-07 11:09:39+02 fred Exp $";

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
    } else {
      if(Ygl.Fonts[i].fs != NULL) {
        XFreeFont(D, Ygl.Fonts[i].fs);
      }
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
  char *name, *rp = r;
  
  const char * MyName = "getfontencoding";
  I(MyName);
  *r = '\0';
  fs = Ygl.Fonts[W->font].fs;
  if(fs == NULL) return;
  fontatom = XInternAtom(D, "FONT", False);
  
  for (i = 0, fp = fs->properties; i < fs->n_properties; i++, fp++) {
    if (fp->name == fontatom) {
      name = XGetAtomName(D, fp->card32);
      if(name != NULL) {
        int dashes = 0;
        char *p = name;
        while(*p != '\0' && dashes < 13) {
          if(*p++ == '-') dashes++;
        }
        if(dashes >= 13) {
          while(*p != '\0') {
            if(*p != '-') *rp++ = *p;
            p++;
          }
          *rp = '\0';
        }
        XFree(name);
      }
    }
  }
  if(*r == '\0') {
    Yprintf(MyName, "can't determine fontencoding.\n");
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
