all:
	sbt compile assembly

clean:
	sbt clean && rm -rf wacc-33-compiler.jar

.PHONY: all clean
