NAME = zchex

OBJ0 =zchex.obj
OBJ1 =zpass1.obj, zpass2.obj
OBJ2 =zutilty_sub.obj, zdaniel_new.obj, zvaxutl.obj, zerr_suite.obj
OBJX =$(OBJ0), $(OBJ1), $(OBJ2)

F = .for
E = .exe
M = .mak
NAMEEXE = $(NAME)ai$(E)
NAMEMAK = $(NAME)ai$(M)

OBJF =dan_ioi.obj

OBJS=$(OBJX),$(OBJF)
FORT = fort
FFLAG=
LINK = link
LFLAG= /EXE=$(NAMEEXE)
 
 
$(NAMEEXE) : $(NAMEMAK) $(OBJS)
	$(LINK) $(OBJS) $(LFLAG)
 
#zpass1.o: zpass1.for
#	$(FORT) $(FFLAG) $<

#%.o: %.for
.for.obj :
	$(FORT) $(FFLAG) $<

clean :
	rm $(OBJS)
	rm $(NAMEEXE)
