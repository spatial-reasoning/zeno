
compareAndAdjust:
	mkdir -p ../bin
	ghc --make -O2 -fspec-constr-count=16 -threaded compareAndAdjust.hs -o ../bin/compareAndAdjust
#	ghc --make -O2 -fspec-constr-count=16 -threaded -split-objs compareAndAdjust.hs -o ../bin/compareAndAdjust
#	ghc --make -O2 -fspec-constr-count=16 -rtsopts -prof -auto-all -caf-all -fforce-recomp compareAndAdjust.hs -o ../bin/compareAndAdjust
#	ghc --make -O2 -fspec-constr-count=16 -rtsopts -prof -auto-all -caf-all compareAndAdjust.hs -o ../bin/compareAndAdjust

compareRandomNetworks:
	mkdir -p ../bin
	ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts compareRandomNetworks.hs -o ../bin/compareRandomNetworks
#	ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts -prof compare.hs -o compare
#	ghc --make -O2 -fspec-constr-count=16 -rtsopts -prof -auto-all -caf-all -fforce-recomp ../bin/compareRandomNetworks.hs -o ../bin/compareRandomNetworks
#	ghc --make -O2 -fspec-constr-count=16 -rtsopts -prof -auto-all -caf-all compareRandomNetworks.hs -o ../bin/compareRandomNetworks

compareGivenNetworks:
	mkdir -p ../bin
	ghc --make -O2 -fspec-constr-count=16 -threaded compareGivenNetworks.hs -o ../bin/compareGivenNetworks
#	ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts compareGivenNetworks.hs -o ../bin/compareGivenNetworks
#	ghc --make -fspec-constr-count=16 -threaded -rtsopts qstr.hs -o qstr

%: %.hs
	ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts $*.hs -o $*

all:
	find . -iname "*.hs" -exec ghc --make -O2 -fspec-constr-count=16 -threaded -rtsopts {} \;

clean:
	rm -f ../bin/*
	find . \( -iname "*.aux" -o -iname "*.log" -o -iname "*.hi" -o -iname "*.o" -o -iname "*.out" -o -iname "*.prof" -o -iname "*~" \) -exec rm {} \;

silent:
	echo "Hier sieht man nur diesen Text."

nixsilent:
	echo "Hier wird das Kommando mit ausgegeben."

.SILENT: silent
.PHONY: nixsilent

# ENDE
