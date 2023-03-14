
.PHONY: all clea

all: dist/main.js dist/index.html

clean:
	rm -rf dist

dist:
	mkdir -p $@

dist/main.js: src/Main.elm dist
	elm make --output=$@ $<

dist/index.html: html/index.html dist
	cp $< $@