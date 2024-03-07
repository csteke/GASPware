
CDEBUGFLAGS=-O

COPTS =
# COPTS = -Aa -D_HPUX_SOURCE -L/usr/lib/X11R4 -I/usr/include/X11R4 # For HP-UX 8.0x
# COPTS = -Ae -L/usr/lib/X11R5 -I/usr/include/X11R5 # For HP-UX 9.0x


CFLAGS= $(CDEBUGFLAGS) $(COPTS) -I.. -I.

# ******* replce -lYgl with -lgl to use standard gl ****

REQUIREDLIBS 	= -L.. -lglwidget -lYgl -lX11 -lXext -lm

# REQUIREDLIBS 	= -L.. -lglwidget -lgl -lX11  -lm


# *************************** End of configuration *************************


TARGETS = simple lissa

all: 	$(TARGETS)

.c:
	$(CC) $(CFLAGS) -o $@ $< $(REQUIREDLIBS) -lm

clean:
	/bin/rm -f *.o *~ $(TARGETS)
