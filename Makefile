all: format compile

format:
	sbt scalafmtAll

build:
	sbt compile assembly 

clean:
	sbt clean && rm -rf wacc-33-compiler.jar

.PHONY: all format compile clean 
