# Kleines Makefile zum Erzeugen eines
# PDF-Dokumentes aus einer LaTeX-Datei.

%: %.hs
	ghc $*.hs -o $*

clean:
	rm -f *.aux *.log *.hi *.o *.out *~

silent:
	echo "Hier sieht man nur diesen Text."

nixsilent:
	echo "Hier wird das Kommando mit ausgegeben."

.PHONY: nixsilent silent
.SILENT: silent

# ENDE
