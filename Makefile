all:

build: build.sh
	sh build.sh

%.pdf: %.dot
	dot -Tpdf $< > $@
