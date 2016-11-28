.PHONY: all

md_dirs = \
	. \
	essay \
	tutorial

sources = \
	$(wildcard \
		$(patsubst %,%/*.lhs,$(md_dirs)) \
		$(patsubst %,%/*.md,$(md_dirs)))

targets = \
	$(patsubst %.md,%.html,$(patsubst %.lhs,%.html,$(sources)))

PANDOC = pandoc \
	--smart \
	--standalone \
	--toc \
	--variable=basedir:$(BASEDIR)
	--wrap=none

PANDOC_HTML = \
	--base-header-level=2 \
	--css=style.css \
	--html-q-tags \
	--mathml \
	--section-divs \
	--template=template.html \
	--to=html5

PANDOC_LHS = \
	--indented-code-classes=haskell


all: $(targets)


essay/%.html tutorial/%.html: BASEDIR = ../
essay/%.html tutorial/mastering-folds/%.html: BASEDIR = ../../

%.html: %.md Makefile template.html
	$(PANDOC) $(PANDOC_HTML) --from=markdown+tex_math_double_backslash -o $@ $<

%.html: %.lhs Makefile template.html
	$(PANDOC) $(PANDOC_HTML) $(PANDOC_LHS) --from=markdown+lhs+tex_math_double_backslash -o $@ $<
