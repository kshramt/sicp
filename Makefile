.SUFFIXES:
.DELETE_ON_ERROR:
.ONESHELL:
export SHELL := /bin/bash


.PHONY: all test
all:
test:
	for n in sicp.core sicp.ch-2-4
	do
	   lein test "$${n}"
	   lein typed check "$${n}"
	done
