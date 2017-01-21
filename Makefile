mastermind: mastermind.hs
	ghc -O2 -XBangPatterns mastermind.hs

frege: build/Mastermind.class

build/Mastermind.class: mastermind.fr
	[ -d build ] || mkdir build
	java -Xss1m -jar fregec.jar -O -d build mastermind.fr

run: frege
	java -cp build:fregec.jar Mastermind

scala: Mastermind.class

Mastermind.class: mastermind.scala
	scalac -opt:l:classpath mastermind.scala

run-scala: Mastermind.class
	bash -c "time scala Mastermind"

clean:
	rm -fr mastermind *.o *.hi build *.class
