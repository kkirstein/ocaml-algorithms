#
# Makefile for Machine Learning tools
#

# platform dependent settings
UNAME_O = $(shell uname -o)
ifeq ($(UNAME_O), Cygwin)
	EXT_EXE = .exe
else
	EXT_EXE =
endif

# targets for distribution
dist_sources = bin/nqueens.exe bin/salesman.exe
dist_target = $(foreach f,$(dist_sources),./dist/$(basename $(notdir $(f)$(EXT_EXE))))

all: build

clean:
	dune clean

distclean: clean
	rm -rf dist

build:
	dune build

test: build
	ALCOTEST_QUICK_TESTS=1 dune runtest

# make distribution package (folder)
dist: build
	rm -rf dist
	mkdir -p dist
	$(foreach f,$(dist_sources),cp _build/default/$(f) ./dist/$(basename $(notdir $(f)))$(EXT_EXE);)
	ldd $(dist_target) \
	 | awk '$$2  == "=>" && $$3 !~ /WINDOWS/ && $$3 ~/^\// && !seen[$$3]++ { print "cp", $$3, "./dist" }' | sh

.PHONY: 	all clean build test
