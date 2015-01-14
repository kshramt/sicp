.SUFFIXES:
.DELETE_ON_ERROR:
.ONESHELL:
export SHELL := /bin/bash
export SHELLOPTS := errexit:noclobber


PANDOC := pandoc
PANDOC_FLAGS := --standalone --mathml --to=html5 --smart --self-contained

NAMES := core ch-2-4 ch-3
FILE_NAMES := $(subst -,_,$(NAMES))


.PHONY: all check
all:
check: $(FILE_NAMES:%=src/sicp/%.clj.tested)


src/sicp/%.clj.tested: src/sicp/%.clj
	readonly ns="$$(echo "$*" | sed -e 's/_/-/g')"
	lein do test sicp."$$ns", typed check sicp."$$ns"
	touch $@


%.html: %.md
	$(PANDOC) $(PANDOC_FLAGS) -o $@ $<
