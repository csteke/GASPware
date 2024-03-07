/*****************************************************************************
NAME
	lineget.c
AUTOR
	B. Stanzel
FUNKTION
	Routinen fu"r Terminalinput, die ein Editieren der Zeile erlauben.
DATUM
	07 Mar 1996
VERSION
	1.3
BESCHREIBUNG
	lineget
	Liest vom Terminal bis zum RETURN oder einem Sonderzeichen

	linectrl
	Kontrollfunktionen
	
MODULE
	linecntrl	Kontrollfunktionen fuer lineget
	linecntrl_	f77 - Version 
	lineget		Liest eine Zeile von stdin
	lineget_	f77 - Version
	flineget	Liest eine Zeile von einer beliebigen File/Device
INCLUDE

STICHWORTE

SONSTIGES

*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#if defined(Digital)
#   include <termios.h>
#else
#   include <termios.h>
#endif

#include <unistd.h>
#include <ctype.h>
#include <signal.h>

#ifdef __linux__
#  include <sys/ioctl.h>
#endif

#ifdef Digital
#  include <stropts.h>
#endif

#ifdef sun
#  include <stropts.h>
#endif


#undef putchar

#define histbuflen 2048
#define maxbuflen 256
#define filnamlen 80

#define ctrl(x)	(x & 0x1f)
#define ALT_INT 256
#define ESCAPE 128
#define esc(x)	(x | ESCAPE)

#define  MEMORY


int lineget(unsigned char *, int);
int flineget(FILE *, unsigned char *, int);




static void delete(int), deleteline(), insert(), prichar(), linectrl_cf();
static int countword(), testchar(), movecur();
static void histclose(), histopen(), linetohistory(), histsearch(), sputchar();
static int gethistline();

static char histbuf[histbuflen], buffer[maxbuflen], searchbuf[40];
static char alt_buffer[maxbuflen];
static char histfilenam[filnamlen] = "\0";
static char ctrlarray[] = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
static int histcur = 0, histlen = -1, search = 0;
static int cur, buflen = 0, lastget;
static FILE *fp = NULL, *ifp;

static int getc_default(FILE *fp, char *buf);
static int (*user_getc) () = getc_default;

int getc_local();
  

/*BIBLIOTHEKSFUNKTION*********************************************************
NAME
	linectrl, linctrl_
AUTOR
	B.Stanzel
FUNKTION
	Kontrollfunktionen fuer lineget
AUFRUF
.	void linectrl(flag, buffer)
ARGUMENTE
	int flag
	char * buffer
RETURNWERT
	--
FEHLER

BESCHREIBUNG
	Fuehrt Kontrollfunktionen fuer lineget aus:
	flag	buffer		Funktion
	-2	Kontrollzeichen	wenn ein, in buffer definiertes kontrollzeichen
				getippt wird, so wird diese kontrollzeichen
				in den zeilenbuffer von linget geschrieben
				und linget abgebrochen.
	-1	String		vor dem aufruf von lineget wird der buffer in
				den buffer von linget kopiert. (default wert)
	0	Filenamen	der historybuffer wird weggeschrieben
	1	Filenamen	eine alte historyfile wird geoeffnet
	2	Filnamen	der filenamen wird als defaultfilnamen fuer
				die historyfile gesetzt
VERFAHREN

BEISPIEL
	linectrl(-2,"^A^B")	wenn ^A bzw. ^B getippt wird, wird lineget
				abgebrochen
BIBLIOTHEK
.	/usr/mlle/cmd/lib/u/libuti.a	-- Utility Library PCS und MIPS
SUBROUTINEN

STICHWORTE

SIEHE AUCH

SONSTIGES

*****************************************************************************/

void linectrl (flag,line)
int flag;
char * line;
{
	if(flag == 0) {
		if (line == NULL)
			histclose(histfilenam);
	} else {
		linectrl_cf(flag,line,strlen(line) );
	}
}

void linectrl_(flag,line,flen)
int *flag;
char *line;
int flen;
{
	linectrl_cf(*flag,line,flen);
}

static void linectrl_cf(flag,line,flen)
int flag;
char *line;
int flen;
{
	buflen = 0;
	if(flag == -2) {
		strncat(ctrlarray, line, flen);
	} else {
		strncpy(buffer, line, flen);
		buffer[flen] = '\0';

		if(flag == -1) {
			buflen = flen;
		} else if (flag == 0) {
			if(*line == '\0')
				histclose(histfilenam);
			else
				histclose(buffer);
		} else {
			if(flag & 2)
				strcpy(histfilenam,buffer);
			if(flag & 1)
				histopen();
			buflen = 0;
		}
	}
}

/*BIBLIOTHEKSFUNKTION*********************************************************
NAME
	lineget, lineget_, flineget
AUTOR
	B.Stanzel
FUNKTION
	Liest Terminalinput in einen Buffer. Dabei kann die Zeile
	editiert werden (wie bei der ksh)
AUFRUF
.	f77:	charnum = lineget(buffer(1:buflen))
.	C:	charnum = lineget(buffer, buflen)
.		charnum = flineget(filed, buffer, buflen)
ARGUMENTE
.	char buffer	buffer in den die zeile geschrieben wird
.	int buflen	laenge des buffers (maximal der zu tippenden zeichen)
.	FILE filed	filedeskriptor
RETURNWERT
.	int charnum	zahl der tatsaechlich getippten zeichen
			> 0:	Zahl der gelesenen Zeichen
			= 0:	Leerzeile
			< 0:	Ein mit linectrl definiertes Kontrollzeichen
				wurde getippt. Das Kontrollzeichen wird als
				letztes Zeichen in den buffer geschrieben
FEHLER
	-
BESCHREIBUNG
	Liest vom Terminal bis zum RETURN oder einem Sonderzeichen
	
	Philosophie:

	1. Schalte das Terminal im "NICHT Canonoschen" Mode um. (bei flineget
	   wird nur umgeschaltet, wenn es ein Terminal ist)

	2. Lese Keyboard und schreibe die Zeichen in einen Buffer.
	   mit den Sonderzeichen ^a ^b ^f ^d und den Pfeil- bzw. funktions-
	   Tasten kann die Zeile (wie bei der ksh) editiert werden.

	3. wenn CR getippt wird, wird das Terminal wieder in den kanonischen
	   Mode geschaltet und die anzahl der getippten zeichen zurueckgegeben.
	   als letztes zeichen wird '\0' in den buffer geschrieben.

	   Ausnahmen:	
	   1.	die mit dem Aufruf von linectrl(-2,...) definierten sonder-
		zeichen brechen ebenfals ab. dabei wird das sonderzeichen
		als letztes zeichen in den buffer geschrieben und als
		returnwert die negative anzahl der getippten zeichen zurueck-
		gegeben

	   2.	bei ^C wird das signal SIGINT gesetzt und ebenfalls -1
		zuru"ckgegeben.
		
	   2.	bei EOF (bei uns ^D) der buffer geloescht und -1 zurueckgegene
		
VERFAHREN

BEISPIEL

BIBLIOTHEK
	
SUBROUTINEN

STICHWORTE

SIEHE AUCH

SONSTIGES

*****************************************************************************/

int lineget_(lesbuf,max)		/* F77 Konvention */
char *lesbuf;
int *max;
{
	return (lineget ((unsigned char *)lesbuf,*max) );
}


int lineget(unsigned char *lesbuf, int max)			/* cc konvention */
{
   int ii, lstr;
   static char check_buffer[maxbuflen];
   char *ckb;
       
	if( max > maxbuflen ) max = maxbuflen;
	
	lstr =  flineget(stdin,lesbuf,max);
	
	ii = 0;
	while( ((lesbuf[ii] < 31)||(lesbuf[ii] > 127)) && ( ii < lstr ) )ii++;
	
	ckb = check_buffer;
	
	lstr -= ii;
	if( lstr )
	{
	   strncpy( ckb, (char *)(lesbuf+ii), maxbuflen );
	   strncpy( (char *)lesbuf, ckb, maxbuflen );
	}
	
	return( lstr );
}


int flineget(filpnt,lesbuf,max)                 /* cc konvention */
FILE *filpnt;
unsigned char *lesbuf;
int max;
{
/*
#if defined(Digital)
	struct termio ttyflags, savflags;
#else
	struct termio ttyflags, savflags;
#endif
*/    
    static struct termios  cooked, raw;
	
	
	int c = 0;
	int dmmy = 0;

	ifp = filpnt;
	cur = 0;
	search = 0;
	searchbuf[0] = '\0';
	lastget = 1;

	/* schalte im nichtkanonoschen mode um 
	if(isatty(fileno(ifp))) {
		(void) ioctl(fileno(ifp), TCGETA, &ttyflags);
		savflags = ttyflags;
		ttyflags.c_lflag &= !ICANON ;
		ttyflags.c_cc[VMIN] = 1;
		ttyflags.c_cc[VTIME] = 0;
		ioctl(fileno(ifp), TCSETA, &ttyflags);
	}
        */
	if(isatty(fileno(ifp))) 
	{
            tcgetattr(0,&cooked);
            memcpy(&raw, &cooked, sizeof(struct termios));
            raw.c_lflag &= ~(ICANON | ECHO ) ;
            raw.c_lflag &= ECHONL;
            raw.c_cc[VMIN] = 1;
            raw.c_cc[VTIME] = 0;
            tcsetattr(0, TCSANOW, &raw);
        }

	
	if(buflen > 0)
		movecur(buflen);
	else
		buflen = 0;

	while( buflen < max - 1 && buflen < maxbuflen - 1 ) {
		if( (c = getc_local(ifp)) < ' ') {	/* Kontrollzeichen */
			if( c == '\033') {	/* ESCAPE */
				c = getc_local(ifp); 
				if ( c == 'O' || c == '[' )
				c = getc_local(ifp);
				if ( (c >= '0') && (c <= '9') ) { 
				  dmmy = c;
				  c = getc_local(ifp);
				  if ( (c >= '0') && (c <= '9'))c = getc_local(ifp);
				  c = esc(dmmy);
				   }
			}
			if(strchr(ctrlarray, c) != NULL) {
				buflen = -buflen - 1;
				buffer[~buflen] = c;
				c = 0;
				goto ende;
			}
			if (c > ' ')
				c |= ESCAPE;
		}

		if(c == ALT_INT) {
			buffer[0] = c && '\177';
			buflen = -1;
			goto ende;
		}

		switch (c) {

		case ctrl('c'):
			goto ende;

		case EOF:
		case ctrl('d'):
			buflen = -1;
			buffer[0] = EOF;
			goto ende;

		case '\b':
		case '\177':
			delete(-1);
			break;

/*		case ctrl('D'):*/
                case esc('3'):
			delete(1);
			break;

		case ctrl('k'):
			delete (buflen - cur);
			break;

		case ctrl('f'):
		case esc('C'):
			movecur ( 1);
			break;

		case ctrl('b'):
		case esc('D'):
			movecur (-1);
			break;

		case ctrl('a'):
		case esc('x'):
			movecur (-cur);
			break;

		case ctrl('e'):
		case esc('y'):
			movecur (buflen - cur);
			break;

		case esc('d'):
			delete (countword(1));
			break;

		case esc('h'):
			delete (countword(-1));
			break;

		case esc('f'):
		case esc('v'):
			movecur (countword(1));
			break;

		case esc('b'):
		case esc('s'):
			movecur (countword(-1));
			break;

		case esc('>'):
		case esc('q'):
			histcur = histlen + 1;
		case ctrl('p'):
		case esc('A'):
			if(gethistline(-1) == 0) sputchar(ctrl('g'));
			break;

		case esc('<'):
		case esc('t'):
			histcur = 0;
		case ctrl('n'):
		case esc('B'):
			if(gethistline( 1) == 0) sputchar(ctrl('g'));
			break;

		case ctrl('w'):
		case ctrl('r'):
			deleteline();
			insert(c);
			break;

		case ctrl('u'):
			deleteline();
			break;

		case '\n':
		case '\r':
			buffer[buflen] = '\0';
			if(buffer[0] == '\022') {	/* ^R = suchen */
				if(buflen > 1) {
					strcpy(searchbuf, &buffer[1]);
					search = buflen - 1;
					histcur = histlen + 1;
				}
				if(search == 0) {
					delete(-1);
					gethistline(-1);
				} else {
					lastget = 1;
					histsearch();
				}
				break;
			} else if(buffer[0] == '\027') { /* ^W */
				if (buflen > 1)
					histclose(&buffer[1]);
				else
					histclose(histfilenam);
				deleteline();
				break;
			}
			goto ende;		

		case '\\' :
			insert ('\\');
			c = getc_local(ifp) ;
			delete (-1);
			if(c == '\n' || c == '\r')
				break;
		default:
			if(c >= ESCAPE)
				sputchar(ctrl('g'));	/* klingel */
			else {
				insert (c);
			}
		}

	}

ende:
	/* Restore Terminalmode 
	if(isatty(fileno(ifp))) {	
		ioctl(fileno(ifp), TCSETA, &savflags);
		sputchar('\n');
	}
	*/
	if(isatty(fileno(ifp))) 
	{
#ifdef sun
           ioctl(0,TCSETS,&cooked);
           ioctl(0,TCFLSH,2);
           printf("\n");
#else
           tcsetattr(0, TCSANOW, &cooked);
           printf("\n");
#endif
        }

	if(c != ALT_INT) {		/* normaler terminal input */
		if(c == 3) {
			buflen = -1;
			histclose(histfilenam);
			kill (getpid(),SIGINT);
		}
	
		c = buflen;
		buflen = abs(buflen);
		if(buflen > 0) {
			for(cur = 0; cur < buflen; cur++)
				*lesbuf++ = buffer[cur];
			if(c < 0)
				buflen--;
			if(buflen > 0)
				linetohistory();
		}

		buflen = 0;
		*lesbuf = '\0';
		return(c);
	}

	/* muss ein sonstiger interrupt sein */
	strcpy((char *)lesbuf, alt_buffer);
	return(strlen((char *)lesbuf) );
}

/*BIBLIOTHEKSFUNKTION*********************************************************
NAME
	set_user_getc
AUTOR
	B.Stanzel
FUNKTION
	Uebergibt die adresse fuer die terminal-inputroutine
AUFRUF
.	int set_user_getc(routine)
ARGUMENTE
	int (*routine) ();
RETURNWERT
	--
FEHLER

BESCHREIBUNG
	uebergibt eine adresse einer routine die getc() ersetzt.
	(damit werden z.B. "select calls" mo"glich)

	*** mit set_getc_default wird die interne getc-routine geladen ***

	die vom "user" definierte routine hat 2 argumente:

	int usergetc(fp, buf)
	FILE * fp;
	char * buf
	{

	wenn am terminal (definiert mit fp) ein zeichen getippt wurde,
	so muss es nach buf[0] geschrieben werden und der returnwert = 0
	gesetzt werden.
	bei allen anderen gera"ten usw. wir ein string nach buf geschrieben
	und ein returnwert != 0 gesetzt.
VERFAHREN

BEISPIEL
	extern int usergetc();
	set_user_getc(usergetc);
BIBLIOTHEK
.	/usr/mlle/cmd/lib/u/libuti.a	-- Utility Library PCS und MIPS
SUBROUTINEN

STICHWORTE

SIEHE AUCH

SONSTIGES

*****************************************************************************/

void set_user_getc(func)
int (*func) ();
{
	user_getc = func;
}

/*
static void set_getc_default()
{
	user_getc = getc_default;
}
*/

/*INTERNE PROZEDUR************************************************************
NAME
	movecur
AUTOR
	B.Stanzel
FUNKTION
	bewege den curser
AUFRUF
	i = movecur(n)
ARGUMENTE
	-
RETURNWERT
	-
FEHLER
	-
BESCHREIBUNG
	bewege den curser n zeichen nach rechts [n>0] bzw. nach links [n<0]
	i gibt die tatsaechlichen cursorschritte zurueck
*****************************************************************************/

static int movecur (n)
int n;
{
	int i;

	i = 0;
	if (n < 0) {
		while ( (cur - 1) >= 0 && n++ < 0) {
			sputchar (8);
			if (buffer[--cur] < ' ') sputchar('\b');
			i++;
		}

	} else if (n > 0) {
		while ( (cur + 1) <= buflen && n-- > 0) {
			prichar(buffer[cur]);
			cur++;
			i++;
		}
	}
	return (i);
}
			

/*INTERNE PROZEDUR************************************************************
NAME
	delete
AUTOR
	B.Stanzel
FUNKTION
	loesche n zeichen
AUFRUF
	delete(n)
ARGUMENTE
	-
RETURNWERT
	-
FEHLER
	-
BESCHREIBUNG
	loescht n zeichen nach rechts [n>0] bzw. nach links [n<0]
	schiebt den rest der zeile nach links
*****************************************************************************/

static void delete (int n)
{
	int i, l, j;

	if (n < 0)
		l = movecur(n);
	else
		l = n;

	if (l <= 0)
		return;

	if ((j = buflen - cur) > 0) {		/* loesche rest der zeile */
		for (i = cur; i < buflen ; i++) {
			sputchar(' ');
			if(buffer[i] < ' ') {
				j++;
				sputchar(' ');
			}
		}
		for (i = 0; i < j; i++)
			sputchar('\b');
	}

	if (l < (buflen - cur) ) {
		buflen = buflen - l;
		for (i = cur; i < buflen ; i++)
			buffer[i] = buffer[i+l];
	} else {
		buflen = cur;
	}
	movecur(- movecur(buflen - cur));	/* retype zeile */
}

/*INTERNE PROZEDUR************************************************************
NAME
	deleteline
AUTOR
	B.Stanzel
FUNKTION
	loesche die ganze zeile
AUFRUF
	delete(n)
ARGUMENTE
	-
RETURNWERT
	-
FEHLER
	-
BESCHREIBUNG
	loescht die ganze zeile
*****************************************************************************/

static void deleteline()
{
	movecur(-cur);
	delete(buflen);
}


/*INTERNE PROZEDUR************************************************************
NAME
	insert
AUTOR
	B.Stanzel
FUNKTION
	fuegt ein zeichen ein
AUFRUF
	insert(c)
ARGUMENTE
	-
RETURNWERT
	-
FEHLER
	-
BESCHREIBUNG
	fuegt das zeichen c in den buffer ein
	druckt das zeichen
*****************************************************************************/

static void insert(c)
int c;
{
	int i;

	if (cur < buflen) {
		for(i = buflen; i > cur; i--)
			buffer[i] = buffer[i-1];
	}
	buflen++;
	buffer[cur] = c;
	movecur(1 - movecur(buflen - cur));	/* retype line */
}


/*INTERNE PROZEDUR************************************************************
NAME
	prichar
AUTOR
	B.Stanzel
FUNKTION
	druckt ein zeichen
AUFRUF
	prichar(c)
ARGUMENTE
	-
RETURNWERT
	-
FEHLER
	-
BESCHREIBUNG
	druckt ein zeichen. kontrollzeichen werden mit ^ markiert
*****************************************************************************/

static void prichar(c)
int c;
{
	if (c < ' ') {
		sputchar('^');
		sputchar(c | 64);
	} else {
		sputchar(c);
	}
}


/*INTERNE PROZEDUR************************************************************
NAME
	countword
AUTOR
	B.Stanzel
FUNKTION
	zaehle zeichen in einem wort
AUFRUF
	i = countword (n)
ARGUMENTE
	-
RETURNWERT
	-
FEHLER
	-
BESCHREIBUNG
	zaehlt die zeichen bis zum naechsten, nicht alphanumerischen zeichen.
	n < 0 nach links, n > 0 nach rechts
	i die zahl der zeichen bis zum trennzeichne
*****************************************************************************/

static int countword (n)
int n;
{
	int i,  f;
	i = cur;
	f = 0;
	while (testchar(buffer[i]) == 0) {
		i = i + n;
		if (i < 0) return(f);
		if (i > buflen) return(f);
		f = f + n;
	}
	while (testchar(buffer[i]) != 0) {
		i = i + n;
		if (i < 0) return(f);
		if (i > buflen) return(f);
		f = f + n;
	}
	return (f);
}


/*INTERNE PROZEDUR************************************************************
NAME
	testchar
AUTOR
	B.Stanzel
FUNKTION
	prueft on ein alphanumerisches zeichen
AUFRUF
	testchar(c)
ARGUMENTE
	-
RETURNWERT
	-
FEHLER
	-
BESCHREIBUNG
	return 0 wenn c kein alphanumerisches zeichen (sonst return 1)
*****************************************************************************/

static int testchar(c)
int c;		/* 0 wenn zeichen weder buchstabe noch ziffer */
{
	if (isalpha(c) != 0) return (1);
	return (isdigit(c));
}


/*INTERNE PROZEDUR************************************************************
NAME
	histsearch
AUTOR
	B.Stanzel
FUNKTION
	suche den historybuffer nach dem string (string steht in searchbuf)
AUFRUF
	void hisatsearch()
ARGUMENTE
	-
RETURNWERT
	-
FEHLER
	-
BESCHREIBUNG
*****************************************************************************/

static void histsearch()
{
	int i,j, k;

	if (histcur > 3) {
		for (i = histcur - 2; i >= 0; i--) {
			for (j = i, k = search - 1;
			       k >= 0 && histbuf[j] == searchbuf[k]; j--, k--);
			if(k == -1) {	
				while( histbuf[j] != '\0' ) j--;
				histcur = j + 1;
				gethistline( 1);
				return;
			}			
		}
	}
	deleteline();
	sputchar(ctrl('g'));	/* nicht gefunden: klingel */
}


/*INTERNE PROZEDUREN**********************************************************
NAME
	hist*
AUTOR
	B.Stanzel
FUNKTION
	Routinen zum manipulieren der History-File                         
AUFRUF
	-
ARGUMENTE
	-
RETURNWERT
	-
FEHLER
	-
ANMERKUNG
	Die history-File ist eine ganz normale zeilenorientierte ASCII-File
	allerdings du"rfuen die zeilen nicht mit ' ' oder '	' beginnen

	im history-buffer werden die zeilen mit '\0' voneinander getrennt:
	(das letzte zeichen im buffer muss immer '\0' sein )

*****************************************************************************/


/*INTERNE PROZEDUREN**********************************************************
NAME
	histopen
AUTOR
	B.Stanzel
FUNKTION
	open der historyfile
	diese File muss vom benutzer aufgerufen werden 
	wenn Fehler beim open: starte mit einem leeren buffer
	file=	filenamen der history-file
		wenn file == ' ', dann wird der history mechanismus 
		benutzt, ohne dasS am ende eine history file weggeschrieben
		wird
*****************************************************************************/

static void histopen()
{
	FILE *fopen();
	int fclose();
	int c;

	cur = 0;

	if ( (fp = fopen(buffer, "r")) != NULL) {
		buflen = 0;
		for(;;) {
			if( (c = getc(fp)) == EOF) break;

			if(c != 10) {			/* 10 = lf */
				buffer[buflen++] = c;	
			} else {
				linetohistory ();
				buflen = 0;
			}
		}
		fclose(fp);
	}

}

/*INTERNE PROZEDUREN**********************************************************
NAME
	histopen
AUTOR
	B.Stanzel
FUNKTION
	schreibe den die history-file weg
	wird bei ^C aufgerufen. wenn der anwender das programm u"ber
	eine eigene routine abbricht, muss er diese routine selbs aufrufen
*****************************************************************************/

static void histclose(nam)
char *nam;
{
	int fclose();
	int i;

	if (*nam != '\0') {
		if ( (fp = fopen(nam, "w")) != NULL) {	/* file ok */
			for (i = 0; i <= histlen ; i++) {
				if(histbuf[i] == '\0')
					putc (ctrl('j'), fp);
				else
					putc (histbuf[i], fp);
			}
			fclose(fp);
		}
	}
}


/*INTERNE PROZEDUREN**********************************************************
NAME
	linetohistory
AUTOR
	B.Stanzel
FUNKTION
	schreibe die gerade aktuelle zeile in den history buffer 
*****************************************************************************/

static void linetohistory()
{
	int i;

	/* strip anfangs- und end-leerzeichen */
	cur = 0;
	buffer[buflen] = '\0';
	while(buffer[cur] == ' ' || buffer[cur] == '	') cur++;
	if ( (buflen - cur) > 0) {
		i = buflen - 1;
		while(buffer[i] == ' ' || buffer[i] == '	') i--;
		buflen = i - cur + 1;			/* zeilenlaenge */
		if ( buflen > 0) {
			if ((histbuflen - histlen -2) < buflen) {
				histcur = buflen;
				while (histbuf[histcur++] != 0);

				histlen = histlen - histcur;
				for(i = 0;i < histlen; i++)
					histbuf[i] = histbuf[i + histcur];
			}
			histbuf[histlen++] = '\0';
			for(i = 0;i < buflen; i++)
				histbuf[histlen++] = buffer[cur++];
			histbuf[histlen] = '\0';
		}
	}
	histcur = histlen + 1;
	for(i = histlen;i < histbuflen;i++)
		histbuf[i] = '\0';
}


/*INTERNE PROZEDUREN**********************************************************
NAME
	gethistline(n)
AUTOR
	B.Stanzel
FUNKTION
	lese naechste (+n) bzw. vorherige(-n) zeilen aus dem historybuffer
	(return = 0, wenn zeile leer)
*****************************************************************************/

static int gethistline(n)
int n;
{
	int i;

	if (n <= 0) {
		if (histcur < 3) return(0); 
		i = histcur - 2;
		while(histbuf[i] != '\0') i--;
		histcur = i + 1;
	}
	if(histcur > histlen) return(0);
	deleteline();

	if(lastget < 0 && n > 0) {
		while (histbuf[histcur] != '\0') 
			histcur++;
		histcur++;
	}

	lastget = n;
	i = histcur;
	while (histbuf[i] != '\0')
		buffer[buflen++] = histbuf[i++];

	if (n > 0) histcur = i + 1;
	buffer[buflen] = '\0';
	cur = 0;
	if(n != 0)
		movecur(buflen);
	return(buflen) ;
}		

/*INTERNE PROZEDUREN**********************************************************
NAME
	sputchar(c)
AUTOR
	B.Stanzel
FUNKTION
	gibt nichts aus, wenn stdin kein terminal ist
*****************************************************************************/

static void sputchar (c)
char c;
{
	if(isatty(fileno(ifp)))
		putc(c,stdout);
}

/*INTERNE PROZEDUREN**********************************************************
NAME
	getc_local(fp)
AUTOR
	B.Stanzel
FUNKTION
	liest ein zeichen ein
	bei einem terminal kann u"ber eine routine eingelesen werden,
	damit sind z.B. select-calls mo"glich

*****************************************************************************/

static int getc_default(FILE *fp, char *buf)
{
	*buf = getc(fp);
	return (0);
}




int getc_local(fp)
FILE * fp;
{
	int c;

	if(isatty(fileno(ifp))) {
		c = user_getc (fp,alt_buffer);
		if(c == 0)
			c = *alt_buffer;
		else
			c = ALT_INT;
	} else {
		c = getc(fp);
	}
	return(c);
}
