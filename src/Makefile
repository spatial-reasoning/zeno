
qstr:
	ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts qstr.hs -o qstr
#	ghc --make -fspec-constr-count=16 -threaded -rtsopts qstr.hs -o qstr

%: %.hs
	ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts $*.hs -o $*

all:
	find . -iname "*.hs" -exec ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts {} \;

clean:
	rm -f qstr
	find . \( -iname "*.aux" -o -iname "*.log" -o -iname "*.hi" -o -iname "*.o" -o -iname "*.out" -o -iname "*~" \) -exec rm {} \;

silent:
	echo "Hier sieht man nur diesen Text."

nixsilent:
	echo "Hier wird das Kommando mit ausgegeben."

.SILENT: silent
.PHONY: nixsilent

# ENDE
