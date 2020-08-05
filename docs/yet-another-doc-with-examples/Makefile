NAME             := nginx-haskell-module-yadwe
NAME_TEX_CONTENT := $(NAME)-content.tex
RST_COLUMNS      := 160
RST_IN_REPLACE   := s/shelloutput/console/; s/^\(\s*\)|||/\1$$/
RST_OUT_REPLACE  := s/\.\. code::/.. code-block::/

all : html pdf rst

html: $(NAME).md
	pandoc -o $(NAME).html -Fvimhl --standalone -c ./pandoc.css $(NAME).md

pdf: $(NAME).md $(NAME).tex
	pandoc -tlatex -o $(NAME_TEX_CONTENT) -Fvimhl $(NAME).md
	latexmk $(NAME)

rst: $(NAME).md
	sed -e '$(RST_IN_REPLACE)' $(NAME).md | \
	pandoc --columns=$(RST_COLUMNS) -o $(NAME).rst
	sed -i -e '$(RST_OUT_REPLACE)' $(NAME).rst

clean:
	latexmk -c $(NAME)
	rm -f $(NAME_TEX_CONTENT)

