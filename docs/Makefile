DOC  = aesa-report

CODE_DIR = ../src
MD_DIR   = ./input
TXT_DIR  = ./text
FIGS_SRC = ./figs_src
FIGS_DIR = ./figs

LHS_FILES :=	21:../src/ForSyDe/Shallow/AESA.lhs \
		31:../src/ForSyDe/Atom/AESA/Types.lhs \
		32:../src/ForSyDe/Atom/AESA1.lhs \
		33:../src/ForSyDe/Atom/AESA/Coefs.lhs \
		34:../src/ForSyDe/Atom/AESA2.lhs 

BIB_FILE := refs.bib
TX_FILES := $(patsubst $(TXT_DIR)/%.md, $(MD_DIR)/%.md, $(wildcard $(TXT_DIR)/*.md))
TIKZ_FIGS:= $(patsubst $(FIGS_SRC)/%.tex, $(FIGS_DIR)/%.pdf, $(wildcard $(FIGS_SRC)/*.tex))
PDF_FIGS := $(patsubst $(FIGS_SRC)/%.pdf, $(FIGS_DIR)/%.pdf, $(wildcard $(FIGS_SRC)/*.pdf))
SVG_FIGS := $(patsubst $(FIGS_SRC)/%.tex, $(FIGS_DIR)/%.svg, $(wildcard $(FIGS_SRC)/*.tex))
SVG_FIGS += $(patsubst $(FIGS_SRC)/%.pdf, $(FIGS_DIR)/%.svg, $(wildcard $(FIGS_SRC)/*.pdf))

MD_FILES = $(sort $(foreach file,$(LHS_FILES),$(call make-md-targets,$(call index,$(file)),$(call name,$(file)))))

# Makes target PDF names with figure names extracted.
# 1. index
# 2. path to .lhs file
make-md-targets=$(patsubst %,$(MD_DIR)/$(1)_%.md,$(notdir $(basename $(2))))

index=$(word 1,$(subst :, ,$(1)))
name=$(word 2,$(subst :, ,$(1)))

define md-template
  $(2) : $(1)
	sed -nf scripts/bird2md.sed < $$(<) > $$@
	sed -i 's/^ #/#/g' $$@
endef

html: $(DOC).html

pdf: $(DOC).pdf

$(DOC).html: Makefile $(BIB_FILE) $(FIGS_DIR) $(MD_DIR) $(SVG_FIGS) $(TX_FILES) $(MD_FILES)
	pandoc $(MD_DIR)/* \
		--mathml --number-sections \
		--filter pandoc-crossref \
		--filter pandoc-citeproc \
		--filter=scripts/includes-svg.hs \
		--bibliography=refs.bib \
		-f markdown -t html -s -o $@

$(DOC).pdf: Makefile $(BIB_FILE) $(FIGS_DIR) $(MD_DIR) $(PDF_FIGS) $(TIKZ_FIGS) $(TX_FILES) $(MD_FILES)
	pandoc $(MD_DIR)/* \
		--number-sections \
		--filter pandoc-crossref \
		--filter pandoc-citeproc \
		--bibliography=refs.bib \
		-s -o $@

# generate rules for Markdown files
$(foreach file,$(LHS_FILES),\
	$(eval $(call md-template,$(call name,$(file)),\
		$(call make-md-targets,$(call index,$(file)),$(call name,$(file))))))


input/%.md: $(TXT_DIR)/%.md
	cp $(<) $@

$(TIKZ_FIGS): figs/%.pdf: figs_src/%.tex
	pdflatex $(<)
	mv $(*F).pdf $@
	rm *.log
	rm *.aux

$(PDF_FIGS): figs/%.pdf: figs_src/%.pdf
	cp $(<) $@

$(SVG_FIGS): figs/%.svg: figs/%.pdf
	pdf2svg $(<) $@

clean:
	rm -f $(MD_DIR)/*
	rm -f $(DOC).html $(DOC).pdf

superclean: clean
	rm -rf $(FIGS_DIR)

$(MD_DIR):
	mkdir -p $(MD_DIR)

$(FIGS_DIR):
	mkdir -p $(FIGS_DIR)

.PHONY: clean html pdf superclean
