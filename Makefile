all: format build

format:
	sbt scalafmtAll

build:
	sbt compile assembly 

test:
	bash test.sh

clean:
	sbt clean && rm -rf wacc-33-compiler.jar

.PHONY: all format compile clean 
