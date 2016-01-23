all: hhttp

hhttp: *.hs
	ghc -O3 -O Main.hs -o hhttp

clean:
	-rm -f *.o *.hi */*.o */*.hi hhttp
