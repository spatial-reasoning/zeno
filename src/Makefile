# Kleines Makefile zum Erzeugen eines
# PDF-Dokumentes aus einer LaTeX-Datei.

%: %.hs
	ghc --make -O2 -threaded $*.hs -o $*

clean:
	rm -f qstr *.aux *.log *.hi *.o *.out *~

silent:
	echo "Hier sieht man nur diesen Text."

nixsilent:
	echo "Hier wird das Kommando mit ausgegeben."

.SILENT: silent
.PHONY: nixsilent

# ENDE
