.SUFFIXES:
.DELETE_ON_ERROR:
.ONESHELL:
export SHELL := /bin/bash
export SHELLOPTS := errexit:noclobber


PANDOC := pandoc
PANDOC_FLAGS := --standalone --mathml --to=html5 --smart --self-contained

NAMES := \
   core \
   ch-2-4 \
   ch-3 \
   ch-4 \
   pair \
   deque \
   table \
   queue \
   digital-circuit-simulator \
   constraint \
   stream

FILE_NAMES := $(subst -,_,$(NAMES))


ns_of_file = $(subst _,-,$(1))


.PHONY: all check type_check unit_test
all:
check: type_check unit_test
type_check: $(FILE_NAMES:%=src/sicp/%.clj.type_checked)
unit_test: $(FILE_NAMES:%=src/sicp/%.clj.unit_tested)


define suffix_loop_template =
src/sicp/ch_3.clj.$(1): src/sicp/pair.clj.$(1)
src/sicp/deque.clj.$(1): src/sicp/pair.clj.$(1)
src/sicp/table.clj.$(1): src/sicp/pair.clj.$(1)
src/sicp/queue.clj.$(1): src/sicp/pair.clj.$(1)
src/sicp/digital_circuit_simulator.clj.$(1): src/sicp/queue.clj
endef
$(foreach suf,unit_tested type_checked, \
   $(eval $(call suffix_loop_template,$(suf))))


src/sicp/%.clj.unit_tested: src/sicp/%.clj
	lein test sicp.$(call ns_of_file,$*)
	touch $@


src/sicp/%.clj.type_checked: src/sicp/%.clj
	lein typed check sicp.$(call ns_of_file,$*)
	touch $@


%.html: %.md
	$(PANDOC) $(PANDOC_FLAGS) -o $@ $<
