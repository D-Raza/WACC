milestone := "calc_examples"

all: build

build:
	sbt compile assembly 

check: build
	bash test.sh $(milestone)

format:
	sbt scalafmtAll

clean:
	sbt clean && rm -rf wacc_examples/ wacc-33-compiler.jar test.log

.PHONY: all build check format clean 
