HC = ghc
HC_OPTS = -O -Wall
.SUFFIXES : .o .hs .hi .hc

OBJS = Chess.hi Chess/Util.hi Chess/Notation.hi Chess/Notation/FEN.hi \
 Chess/Notation/PGN.hi

all : $(OBJS)

.o.hi : ;
.hs.o :
	$(HC) -c $< $(HC_OPTS)

Chess.o Chess/Notation.o Chess/Notation/FEN.o Chess/Notation/PGN.o : Chess/Util.hi
Chess/Notation.o Chess/Notation/FEN.o Chess/Notation/PGN.o : Chess.hi
Chess/Notation/FEN.o Chess/Notation/PGN.o : Chess/Notation.hi

doc : Chess.hs Chess/*.hs Chess/Notation/*.hs
	haddock -ho doc -t Chess Chess.hs Chess/*.hs Chess/Notation/*.hs

clean :
	( cd Chess; rm -f *.o *.hi )
	( cd Chess/Notation; rm -f *.o *.hi )
	rm -f *.o *.hi
	rm -rf doc
