
compareRandomNetworks:
	ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts compareRandomNetworks.hs -o compareRandomNetworks
#	ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts -prof compare.hs -o compare
#	ghc --make -O2 -fspec-constr-count=16 -rtsopts -prof compare.hs -o compare

compareGivenNetworks:
	ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts compareGivenNetworks.hs -o compareGivenNetworks
#	ghc --make -fspec-constr-count=16 -threaded -rtsopts qstr.hs -o qstr

%: %.hs
	ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts $*.hs -o $*

all:
	find . -iname "*.hs" -exec ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts {} \;

clean:
	rm -f compareRandomNetworks compareGivenNetworks
	find . \( -iname "*.aux" -o -iname "*.log" -o -iname "*.hi" -o -iname "*.o" -o -iname "*.out" -o -iname "*.prof" -o -iname "*~" \) -exec rm {} \;

silent:
	echo "Hier sieht man nur diesen Text."

nixsilent:
	echo "Hier wird das Kommando mit ausgegeben."

.SILENT: silent
.PHONY: nixsilent

# ENDE
