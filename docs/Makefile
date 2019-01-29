DOC  = aesa-report

CODE_DIR = ../src
MD_DIR   = ./input
HTML_DIR = ./html
PDF_DIR  = ./pdf

BIB_FILE := refs.bib

MD_FILES   := $(patsubst $(CODE_DIR)/%.lhs, $(MD_DIR)/%.md, $(wildcard $(CODE_DIR)/*.lhs))
HTML_FILES := $(patsubst $(CODE_DIR)/%.lhs, $(HTML_DIR)/%.html, $(wildcard $(CODE_DIR)/*.lhs))
PDF_FILES  := $(patsubst $(CODE_DIR)/%.lhs, $(PDF_DIR)/%.pdf, $(wildcard $(CODE_DIR)/*.lhs))


html: $(DOC).html

pdf: $(DOC).pdf

$(DOC).html: Makefile $(BIB_FILE) html/$(DOC).html

$(DOC).pdf: Makefile $(BIB_FILE) $(PDF_FILES)


html/$(DOC).html: $(MD_FILES)
	pandoc $(MD_DIR)/* \
		--mathml \
		--filter pandoc-citeproc \
		--bibliography=refs.bib \
		-f markdown -t html -s -o $@

pdf/%.pdf: input/%.md
	pandoc $(<) -s -o $@

input/%.md: $(CODE_DIR)/%.lhs
	sed -nf bird2md.sed < $(<) > $@
	sed -i 's/^ #/#/g' $@

clean:
	rm -f $(MD_FILES)
	rm -f $(HTML_FILES)

.PHONY: clean html pdf
