prolog: *.hs
	ghc --make -O2 Main.hs -o prolog

clean:
	-rm -f *~ *.hi *.o prolog
