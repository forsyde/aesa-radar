DOC  = aesa-report

CODE_DIR = ../aesa-atom/src
MD_DIR   = ./input
TXT_DIR  = ./text
FIGS_SRC = ./figs_src
FIGS_DIR = ./figs

LHS_FILES :=	21:../aesa-atom/src/AESA/CubesAtom.lhs \
		22:../aesa-atom/src/AESA/Params.lhs \
		23:../aesa-atom/src/AESA/Coefs.lhs \
		31:../aesa-shallow/src/AESA/CubesShallow.lhs \
		41:../aesa-atom/test/SpecCube.lhs \
		51:../aesa-atom/src/AESA/StreamsAtom.lhs #\

BIB_FILE := refs.bib
TX_FILES := $(patsubst $(TXT_DIR)/%.md, $(MD_DIR)/%.md, $(wildcard $(TXT_DIR)/*.md))
TIKZ_FIGS:= $(patsubst $(FIGS_SRC)/%.tex, $(FIGS_DIR)/%.pdf, $(wildcard $(FIGS_SRC)/*.tex))
PDF_FIGS := $(patsubst $(FIGS_SRC)/%.pdf, $(FIGS_DIR)/%.pdf, $(wildcard $(FIGS_SRC)/*.pdf))
PNG_FIGS := $(patsubst $(FIGS_SRC)/%.tex, $(FIGS_DIR)/%.png, $(wildcard $(FIGS_SRC)/*.tex))
PNG_FIGS += $(patsubst $(FIGS_SRC)/%.pdf, $(FIGS_DIR)/%.png, $(wildcard $(FIGS_SRC)/*.pdf))
PNG_ORIG_FIGS += $(patsubst $(FIGS_SRC)/%.png, $(FIGS_DIR)/%.png, $(wildcard $(FIGS_SRC)/*.png))

MD_FILES = $(sort $(foreach file,$(LHS_FILES),$(call make-md-targets,$(call index,$(file)),$(call name,$(file)))))

# Makes target PDF names with figure names extracted.
# 1. index
# 2. path to .lhs file
make-md-targets=$(patsubst %,$(MD_DIR)/$(1)_%.md,$(notdir $(basename $(2))))

index=$(word 1,$(subst :, ,$(1)))
name=$(word 2,$(subst :, ,$(1)))

define md-template
  $(2) : $(1) 
	bash scripts/bird2md.sh $$(<) $$@
#	sed -nf scripts/bird2md.sed < $$(<) > $$@
#	sed -i 's/^ #/#/g' $$@

endef

html: $(DOC).html

pdf: $(DOC).pdf
#	pdflatex --jobname aesa-full \
#		"\documentclass{article}\usepackage{pdfpages}\begin{document}\includepdf[pages=-]{title}\includepdf[pages=-]{aesa-report}\end{document}"
#	rm aesa-full.log
#	gs -q -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=aesa-full.pdf title.pdf aesa-report.pdf

$(DOC).html: Makefile $(BIB_FILE) $(FIGS_DIR) $(MD_DIR) $(PNG_FIGS) $(PNG_ORIG_FIGS) $(TX_FILES) $(MD_FILES) template/html.template
	pandoc $(MD_DIR)/* \
		--mathml --number-sections --toc \
		--template template/html.template \
		--filter pandoc-crossref \
		--filter pandoc-citeproc \
		--filter=scripts/includes-png.hs \
		--bibliography=refs.bib \
		-f markdown -t html -s -o $@


$(DOC).pdf: Makefile title.pdf $(BIB_FILE) $(FIGS_DIR) $(MD_DIR) $(PDF_FIGS) $(TIKZ_FIGS) $(PNG_ORIG_FIGS) $(TX_FILES) $(MD_FILES) template/latex.template
	pandoc $(MD_DIR)/* \
		-V papersize:a4 \
		--number-sections --toc \
		--template template/latex.template \
		--filter pandoc-crossref \
		--filter pandoc-citeproc \
		--bibliography=refs.bib \
		-f markdown+table_captions+raw_tex -s -o $@


title.pdf : title.tex
	pdflatex title.tex
	rm -f title.aux title.log title.out

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

$(PNG_FIGS): figs/%.png: figs/%.pdf
	convert -density 150 $< -scale 80% -quality 90 $@
#	 sed -i 's|<svg|<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" |g' $@
#	pdf2svg $(<) $@

$(PNG_ORIG_FIGS): figs/%.png: figs_src/%.png
	cp $(<) $@


clean:
	rm -f $(MD_DIR)/*
	rm -f $(DOC).html $(DOC).pdf title.pdf aesa-full.pdf

superclean: clean
	rm -rf $(FIGS_DIR)

$(MD_DIR):
	mkdir -p $(MD_DIR)

$(FIGS_DIR):
	mkdir -p $(FIGS_DIR)

.PHONY: clean html pdf superclean
