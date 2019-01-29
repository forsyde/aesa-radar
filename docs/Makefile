DOC  = aesa-report

CODE_DIR = ../src
MD_DIR   = ./input

BIB_FILE := refs.bib
MD_FILES := $(patsubst $(CODE_DIR)/%.lhs, $(MD_DIR)/%.md, $(wildcard $(CODE_DIR)/*.lhs))


html: $(DOC).html

pdf: $(DOC).pdf

$(DOC).html: Makefile $(BIB_FILE) $(MD_DIR) $(MD_FILES)
	pandoc $(MD_DIR)/* \
		--mathml \
		--filter pandoc-citeproc \
		--bibliography=refs.bib \
		-f markdown -t html -s -o $@

$(DOC).pdf: Makefile $(BIB_FILE) $(MD_DIR) $(MD_FILES)
	pandoc $(MD_DIR)/* \
		--filter pandoc-citeproc \
		--bibliography=refs.bib \
		-s -o $@

input/%.md: $(CODE_DIR)/%.lhs
	sed -nf bird2md.sed < $(<) > $@
	sed -i 's/^ #/#/g' $@

clean:
	rm -f $(MD_FILES)
	rm -f $(DOC).html $(DOC).pdf

$(MD_DIR):
	mkdir -p $(MD_DIR)

.PHONY: clean html pdf
