.PHONY: clean

mac:
	ghc -O2 -isrc -o bin/mac/miniJavac src/miniJavac.hs 
	ghc -O2 -isrc -o bin/mac/miniJava  src/miniJava.hs 

linux:
	ghc -O2 -isrc -o bin/linux/miniJavac src/miniJavac.hs 
	ghc -O2 -isrc -o bin/linux/miniJava  src/miniJava.hs 

clean:
	rm \
		src/*.o                      src/*.hi                    \
		src/ASTs/*.o                 src/ASTs/*.hi               \
		src/Parser/*.o               src/Parser/*.hi             \
		src/Parser/Tokens/*.o        src/Parser/Tokens/*.hi      \
		src/Parser/Productions/*.o   src/Parser/Productions/*.hi \
		src/Interp/*.o               src/Interp/*.hi             \
