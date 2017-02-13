
#eva: FORCE
#	ghc --make -O -fspec-constr-count=16 -threaded -feager-blackholing -with-rtsopts="-N2 -qw -s" eva.hs -o eva
#	ghc --make -O -fspec-constr-count=16 -threaded -with-rtsopts="-s" eva.hs -o eva

%: %.hs FORCE
	ghc --make -O -fspec-constr-count=16 -threaded $*.hs -o $*
#	ghc --make -O -fspec-constr-count=16 -threaded -rtsopts $*.hs -o $*

all:
	find . -iname "*.hs" -exec ghc --make -O -fspec-constr-count=16 -threaded {} \;
#	find . -iname "*.hs" -exec ghc --make -O -fspec-constr-count=16 -threaded -rtsopts {} \;

clean:
	find . \( -iname "*.aux" -o -iname "*.log" -o -iname "*.hi" -o -iname "*.o" -o -iname "*.out" -o -iname "*.p_o" -o -iname "*.prof" -o -iname "*~" \) -exec rm {} \;

.SILENT: silent
silent:
	echo "Hier sieht man nur diesen Text."

.PHONY: nixsilent
nixsilent:
	echo "Hier wird das Kommando mit ausgegeben."

FORCE:

# ENDE
