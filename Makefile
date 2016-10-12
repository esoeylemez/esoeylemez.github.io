.PHONY: all

targets = \
	$(patsubst %.md,%.html,$(wildcard *.md) $(wildcard tutorial/*.md))

PANDOC = pandoc \
	--from=markdown \
	--smart \
	--standalone \
	--toc \
	--variable=basedir:$(BASEDIR)
	--wrap=none

PANDOC_HTML = $(PANDOC) \
	--base-header-level=2 \
	--css=style.css \
	--html-q-tags \
	--section-divs \
	--template=template.html \
	--to=html5


all: $(targets)


tutorial/%.html: BASEDIR = ../
%.html: %.md Makefile template.html
	$(PANDOC_HTML) -o $@ $<
