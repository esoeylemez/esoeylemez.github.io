.PHONY: all

targets = \
	$(patsubst %.md,%.html,$(wildcard *.md))

PANDOC = pandoc \
	--base-header-level=2 \
	--from=markdown \
	--smart \
	--standalone \
	--toc \
	--wrap=none

PANDOC_HTML = $(PANDOC) \
	--css=style.css \
	--html-q-tags \
	--template=template.html \
	--to=html5


all: $(targets)


%.html: %.md Makefile template.html
	$(PANDOC_HTML) -o $@ $<
