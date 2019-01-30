DOC  = aesa-report

CODE_DIR = ../src
MD_DIR   = ./input
FIGS_SRC = ./figs_src
FIGS_DIR = ./figs

BIB_FILE := refs.bib
MD_FILES := $(patsubst $(CODE_DIR)/%.lhs, $(MD_DIR)/%.md, $(wildcard $(CODE_DIR)/*.lhs))
TIKZ_FIG := $(patsubst $(FIGS_SRC)/%.tex, $(FIGS_DIR)/%.svg, $(wildcard $(FIGS_SRC)/*.tex))


html: $(DOC).html

pdf: $(DOC).pdf

$(DOC).html: Makefile $(BIB_FILE) $(FIGS_DIR) $(MD_DIR) $(TIKZ_FIG) $(MD_FILES)
	echo $(TIKZ_FIG)
	pandoc $(MD_DIR)/* \
		--mathml \
		--filter pandoc-citeproc \
		--bibliography=refs.bib \
		-f markdown -t html -s -o $@

$(DOC).pdf: Makefile $(BIB_FILE) $(FIGS_DIR) $(MD_DIR) $(TIKZ_FIG) $(MD_FILES)
	pandoc $(MD_DIR)/* \
		--filter pandoc-citeproc \
		--bibliography=refs.bib \
		-s -o $@

input/%.md: $(CODE_DIR)/%.lhs
	sed -nf bird2md.sed < $(<) > $@
	sed -i 's/^ #/#/g' $@

$(TIKZ_FIG): figs/%.svg: figs_src/%.tex
	pdflatex $(<)
	pdf2svg $(*F).pdf $@
	rm $(*F).pdf
	rm *.log
	rm *.aux

clean:
	rm -f $(MD_FILES)
	rm -f $(DOC).html $(DOC).pdf

$(MD_DIR):
	mkdir -p $(MD_DIR)

$(FIGS_DIR):
	mkdir -p $(FIGS_DIR)

.PHONY: clean html pdf
