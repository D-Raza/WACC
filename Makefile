all: format test build

format:
	sbt scalafmtAll

test:
	bash test.sh

build:
	sbt compile assembly 

clean:
	sbt clean && rm -rf wacc_examples/ wacc-33-compiler.jar

.PHONY: all format compile clean 
