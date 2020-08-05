NAME             := nginx-haskell-module-yadwe
NAME_TEX_CONTENT := $(NAME)-content.tex
RST_COLUMNS      := 160
RST_IN_REPLACE   := s/shelloutput/console/; s/^\(\s*\)|||/\1$$/
define RST_DOC_NAME
====================================================\n$\
Nginx Haskell module (yet another doc with examples)\n$\
====================================================
endef
RST_OUT_REPLACE1 := 1,4d; 5 i $(RST_DOC_NAME)
RST_OUT_REPLACE2 := s/\.\. code::/.. code-block::/
INDEX_RST        := index.rst

all : html pdf rst

html: $(NAME).md
	pandoc -o $(NAME).html -Fvimhl --standalone -c ./pandoc.css $(NAME).md

pdf: $(NAME).md $(NAME).tex
	pandoc -tlatex -o $(NAME_TEX_CONTENT) -Fvimhl $(NAME).md
	latexmk $(NAME)

rst: $(NAME).md
	sed -e '$(RST_IN_REPLACE)' $(NAME).md | \
	pandoc --columns=$(RST_COLUMNS) -s --toc -o ../$(INDEX_RST)
	sed -i -e '$(RST_OUT_REPLACE1)' -e '$(RST_OUT_REPLACE2)' ../$(INDEX_RST)

clean:
	latexmk -c $(NAME)
	rm -f $(NAME_TEX_CONTENT)

