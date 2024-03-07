OBJS		= glwidget.o
TARGET          = libglwidget.a

CDEBUGFLAGS=-O

COPTS =
# COPTS = -Aa -D_HPUX_SOURCE -L/usr/lib/X11R4 -I/usr/include/X11R4 # For HP-UX 8.0x
# COPTS = -Ae -L/usr/lib/X11R5 -I/usr/include/X11R5 # For HP-UX 9.0x

CFLAGS		= $(CDEBUGFLAGS) $(COPTS) $(MULTIBUF) $(YGL_PREFIX) 

all: 	$(TARGET)

.c.o:	
	$(CC) $(CFLAGS) -c -I. $<

$(OBJS):	glwidget.h

$(TARGET):	$(OBJS)
	/bin/rm -f $@
	ar rv $@ $(OBJS)
	ranlib $@

install:	$(TARGET) glwidget.h
	cp glwidget.h /usr/include/gl/glwidget.h
	cp $(TARGET) /usr/lib/$(TARGET) 
	ranlib /usr/lib/$(TARGET)

clean:
	/bin/rm -f *.o *~ $(TARGET)

