.SUFFIXES:
.DELETE_ON_ERROR:
.ONESHELL:
export SHELL := /bin/bash
export SHELLOPTS := errexit:noclobber


PANDOC := pandoc
PANDOC_FLAGS := --standalone --mathml --to=html5 --smart --self-contained

LEIN := lein

NAMES := \
   core \
   ch-2-4 \
   ch-3 \
   pair \
   deque \
   table \
   queue \
   digital-circuit-simulator \
   constraint \
   stream

FILE_NAMES := $(subst -,_,$(NAMES))
type_check_file_names := $(FILE_NAMES:%=src/sicp/%.clj.type_checked)
unit_test_file_names = $(patsubst %,src/sicp/%.clj.unit_tested,$(FILE_NAMES) ch_4 scheme scheme1)


ns_of_file = $(subst _,-,$(1))


.PHONY: all check type_check unit_test
all:
check: type_check unit_test
type_check: $(type_check_file_names)
unit_test: $(unit_test_file_names)


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
	$(LEIN) test sicp.$(call ns_of_file,$*) && touch $@


src/sicp/%.clj.type_checked: src/sicp/%.clj
	$(LEIN) typed check sicp.$(call ns_of_file,$*) && touch $@


%.html: %.md
	$(PANDOC) $(PANDOC_FLAGS) -o $@ $<
