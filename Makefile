.SUFFIXES:
.DELETE_ON_ERROR:
.ONESHELL:
export SHELL := /bin/bash
export SHELLOPTS := errexit:noclobber


NAMES := core ch-2-4 ch-3
FILE_NAMES := $(subst -,_,$(NAMES))


.PHONY: all test
all: test
test: $(FILE_NAMES:%=src/sicp/%.tested)


src/sicp/%.tested: src/sicp/%.clj
	readonly ns="$$(echo "$*" | sed -e 's/_/-/g')"
	lein do test sicp."$$ns", typed check sicp."$$ns"
	touch $@
