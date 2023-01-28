milestone := "calc_examples"

all: lint check build

lint:
	sbt scalafmtCheck

format:
	sbt scalafmt

test: build
	bash test.sh $(milestone)

build:
	sbt compile assembly 

clean:
	sbt clean && rm -rf wacc_examples/ wacc-33-compiler.jar test.log

.PHONY: all build check lint format clean 
