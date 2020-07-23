all: typing.pdf

typing.dvi: typing.tex
	platex typing.tex
	platex typing.tex

typing.pdf: typing.dvi
	dvipdfmx -p letter typing.dvi

clean:
	rm -f *.dvi *.log *.aux *.pdf