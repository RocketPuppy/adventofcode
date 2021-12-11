SHELL=nix-shell
.SHELLFLAGS=shell.nix --pure --run

.PHONY: shell
shell:
	bash

.PHONY: clean
clean: Parser/Makefile Grammar
	make -C $(<D) clean
	unlink Grammar

Parser/Grammar/Test: Parser/Makefile
	make -C Parser/ Grammar/Test

Parser/Makefile: grammar.cf
	bnfc --makefile=$(@F) --outputdir=$(@D) -d --haskell $<

Grammar: Parser/Grammar
	ln -s $< $@

Main: Parser/Grammar/Test Grammar Main.hs
	ghc -O Main
