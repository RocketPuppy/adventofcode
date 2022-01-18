SHELL=nix-shell
.SHELLFLAGS=shell.nix --pure --run

.PHONY: shell
shell:
	bash

.PHONY: ghci
ghci:
	ghci

.PHONY: clean
clean:
	rm *.o
	rm *.hi

Main: Main.hs Parser.hs Lexer.hs
	ghc -O Main
