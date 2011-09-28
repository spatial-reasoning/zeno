# Kleines Makefile zum Erzeugen eines
# PDF-Dokumentes aus einer LaTeX-Datei.

%: %.hs
	ghc --make -O2 -threaded $*.hs -o $*

all:
	find . -iname "*.hs" -exec ghc --make -O2 -threaded {} \;

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
