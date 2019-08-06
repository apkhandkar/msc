eclosure: Main.hs FSMicro.hs Helpers.hs EClosure.hs
	ghc Main.hs -o eclosure
	rm *.hi && rm *.o
	strip eclosure

eclosure-debug: Main.hs FSMicro.hs Helpers.hs EClosure.hs
	ghc Main.hs -g -o eclosure-debug
	rm *.hi && rm *.o
	strip eclosure
