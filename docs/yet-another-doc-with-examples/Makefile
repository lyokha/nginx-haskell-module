NAME             := nginx-haskell-module-yadwe
NAME_TEX_CONTENT := $(NAME)-content.tex
RST_COLUMNS      := 160
RST_IN_REPLACE   := s/shelloutput/console/; s/^\(\s*\)|||/\1$$/
RST_DOC_TITLE    := =============================\n$\
                    Yet another doc with examples\n$\
                    =============================\n
RST_OUT_REPLACE1 := 1i $(RST_DOC_TITLE)
RST_OUT_REPLACE2 := s/\.\. code::/.. code-block::/

all : html pdf rst

html: $(NAME).md
	pandoc -fmarkdown -thtml \
	        -o $(NAME).html -Fvimhl --standalone -c ./pandoc.css $(NAME).md

pdf: $(NAME).md $(NAME).tex
	pandoc -fmarkdown -tlatex-smart \
	        -o $(NAME_TEX_CONTENT) -Fvimhl $(NAME).md
	latexmk $(NAME)

rst: $(NAME).md
	sed -e '$(RST_IN_REPLACE)' $(NAME).md | \
	pandoc -fmarkdown -trst \
	        --columns=$(RST_COLUMNS) -o $(NAME).rst
	sed -i -e '$(RST_OUT_REPLACE1)' -e '$(RST_OUT_REPLACE2)' $(NAME).rst

clean:
	latexmk -c $(NAME)
	rm -f $(NAME_TEX_CONTENT)

