## Dependencies

Install the latest pandoc (later than 1.7). If your OS distribution is ancient (like mine), download and install a later version from the GitHub repo, e.g._

	wget https://github.com/jgm/pandoc/releases/download/2.5/pandoc-2.5-1-amd64.deb
	sudo dpkg -i pandoc-2.5-1-amd64.deb
	rm pandoc-2.5-1-amd64.deb

	 sudo apt install pandoc-citeproc pdf2svg

	cabal install pandoc-types

	git clone git@github.com:lierdakil/pandoc-crossref.git
	cd pandoc-crossref
	stack install   # Strong dependencies on TH
