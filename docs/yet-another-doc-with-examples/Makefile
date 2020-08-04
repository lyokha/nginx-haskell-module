NAME             := nginx-haskell-module-yadwe
NAME_TEX_CONTENT := $(NAME)-content.tex
RST_SED_REPLACE  := s/shelloutput/shell-session/; s/^\(\s*\)|||/\1$$/
RST_COLUMNS      := 160

all : html pdf rst

html: $(NAME).md
	pandoc -o $(NAME).html -Fvimhl --standalone -c ./pandoc.css $(NAME).md

pdf: $(NAME).md $(NAME).tex
	pandoc -tlatex -o $(NAME_TEX_CONTENT) -Fvimhl $(NAME).md
	latexmk $(NAME)

rst: $(NAME).md
	sed -e '$(RST_SED_REPLACE)' $(NAME).md | \
	pandoc --columns $(RST_COLUMNS) -o ../index.rst

clean:
	latexmk -c $(NAME)
	rm -f $(NAME_TEX_CONTENT)

