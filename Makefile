.SUFFIXES:
.DELETE_ON_ERROR:
.ONESHELL:
export SHELL := /bin/bash
export SHELLOPTS := errexit:noclobber


PANDOC := pandoc
PANDOC_FLAGS := --standalone --mathml --to=html5 --smart --self-contained

NAMES := core ch-2-4 ch-3 pair
FILE_NAMES := $(subst -,_,$(NAMES))


.PHONY: all check type_check unit_test
all:
check: type_check unit_test
type_check: $(FILE_NAMES:%=src/sicp/%.clj.type_checked)
unit_test: $(FILE_NAMES:%=src/sicp/%.clj.unit_tested)


src/sicp/ch_3.clj.unit_tested: src/sicp/pair.clj
src/sicp/ch_3.clj.type_checked: src/sicp/pair.clj


src/sicp/%.clj.unit_tested: src/sicp/%.clj
	readonly ns="$$(echo "$*" | sed -e 's/_/-/g')"
	lein test sicp."$$ns"
	touch $@


src/sicp/%.clj.type_checked: src/sicp/%.clj
	readonly ns="$$(echo "$*" | sed -e 's/_/-/g')"
	lein typed check sicp."$$ns"
	touch $@


%.html: %.md
	$(PANDOC) $(PANDOC_FLAGS) -o $@ $<
