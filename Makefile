mastermind: mastermind.hs
	ghc -O2 -XBangPatterns mastermind.hs

frege: build/Mastermind.class

build/Mastermind.class: mastermind.fr
	[ -d build ] || mkdir build
	java -Xss1m -jar fregec.jar -d build mastermind.fr

run: frege
	java -cp build:fregec.jar Mastermind

clean:
	rm -fr mastermind *.o *.hi build
