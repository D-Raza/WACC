all:
	sbt compile assembly

clean:
	sbt clean

.PHONY: all clean
