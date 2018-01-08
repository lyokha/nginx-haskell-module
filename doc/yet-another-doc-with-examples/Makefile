NAME             := nginx-haskell-module-yadwe
NAME_TEX_CONTENT := $(NAME)-content.tex

all : html pdf

html: $(NAME).md
	pandoc -o $(NAME).html -Fvimhl --standalone -c ./pandoc.css $(NAME).md

pdf: $(NAME).md $(NAME).tex
	pandoc -tlatex -o $(NAME_TEX_CONTENT) -Fvimhl $(NAME).md
	latexmk $(NAME)

clean:
	latexmk -c $(NAME)
	rm -f $(NAME_TEX_CONTENT)

