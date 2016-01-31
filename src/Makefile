all: hhttp

hhttp: *.hs */*.hs
	ghc -threaded -O3 -O Main.hs -o hhttp

clean:
	-rm -f *.o *.hi */*.o */*.hi hhttp
