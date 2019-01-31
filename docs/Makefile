DOC  = aesa-report

CODE_DIR = ../src
MD_DIR   = ./input
FIGS_SRC = ./figs_src
FIGS_DIR = ./figs

BIB_FILE := refs.bib
MD_FILES := $(patsubst $(CODE_DIR)/%.lhs, $(MD_DIR)/%.md, $(wildcard $(CODE_DIR)/*.lhs))
PDF_FIGS := $(patsubst $(FIGS_SRC)/%.tex, $(FIGS_DIR)/%.pdf, $(wildcard $(FIGS_SRC)/*.tex))
SVG_FIGS := $(patsubst $(FIGS_SRC)/%.tex, $(FIGS_DIR)/%.svg, $(wildcard $(FIGS_SRC)/*.tex))

html: $(DOC).html

pdf: $(DOC).pdf

$(DOC).html: Makefile $(BIB_FILE) $(FIGS_DIR) $(MD_DIR) $(SVG_FIGS) $(MD_FILES)
	pandoc $(MD_DIR)/* \
		--mathml \
		--filter pandoc-crossref \
		--filter pandoc-citeproc \
		--filter=scripts/includes-svg.hs \
		--bibliography=refs.bib \
		-f markdown -t html -s -o $@

$(DOC).pdf: Makefile $(BIB_FILE) $(FIGS_DIR) $(MD_DIR) $(PDF_FIGS) $(MD_FILES)
	pandoc $(MD_DIR)/* \
		--filter pandoc-citeproc \
		--bibliography=refs.bib \
		-s -o $@

input/%.md: $(CODE_DIR)/%.lhs
	sed -nf scripts/bird2md.sed < $(<) > $@
	sed -i 's/^ #/#/g' $@

$(PDF_FIGS): figs/%.pdf: figs_src/%.tex
	pdflatex $(<)
	mv $(*F).pdf $@
	rm *.log
	rm *.aux

$(SVG_FIGS): figs/%.svg: figs/%.pdf
	pdf2svg $(<) $@

clean:
	rm -f $(MD_FILES)
	rm -f $(DOC).html $(DOC).pdf
	rm -rf $(FIGS_DIR)

$(MD_DIR):
	mkdir -p $(MD_DIR)

$(FIGS_DIR):
	mkdir -p $(FIGS_DIR)

.PHONY: clean html pdf
