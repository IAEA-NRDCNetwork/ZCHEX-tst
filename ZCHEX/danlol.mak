NAME = danlo
E = .exe
M = .mak
O = .o
F = .for
NAMEOBJ = $(NAME)$(O)
NAMEEXE = $(NAME)l$(E)
NAMEMAK = $(NAME)l$(M)

OBJ0 = $(NAMEOBJ)
OBJS = $(OBJ0) $(OBJ1)

FORT = g77
FORT = f95
FFLAG= -c
LINK = g77
LINK = f95
LFLAG= -o $(NAMEEXE)

#FORT = fort
#FFLAG= 
#LINK = link
#LFLAG= 


$(NAMEEXE) : $(NAMEMAK) $(OBJS)
	$(LINK) $(OBJS) $(LFLAG)

%$(O):%$(F)
	$(FORT) $(FFLAG) $<

clean :
	rm $(OBJS)
	rm $(NAMEEXE)
