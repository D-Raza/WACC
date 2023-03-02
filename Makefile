milestone := "wacc_examples/valid/@(basic|sequence|variables|expressions|if|while|IO)"
# milestone := "wacc_examples/valid/function/simple_functions/"

# TODO: 
# array, function, scope, runtimeErr, pairs, advanced

all: lint test build

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
