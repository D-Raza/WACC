milestone := "wacc_examples/valid/function/simple_functions/"
all: build

lint:
	sbt scalafmtCheck

format:
	sbt scalafmt

test: build
	bash test.sh $(milestone)

check:
	sbt test

build:
	sbt compile assembly 

clean:
	sbt clean && rm -rf wacc_examples/ test_exec/ wacc-33-compiler.jar *.s test.log

.PHONY: all lint format test check build clean 
