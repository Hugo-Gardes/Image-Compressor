##
## EPITECH PROJECT, 2022
## B-CPE-210-MPL-2-1-solostumper05-hugo.gardes
## File description:
## Makefile
##

all:
	rm -f imageCompressor
	stack build
	cp .stack-work/install/x86_64-linux-tinfo6/*/8.10.7/bin/imageCompressor-exe .
	mv imageCompressor-exe imageCompressor

clean:
	stack clean

fclean:
	rm -f imageCompressor
	stack purge
	rm -f imageCompressor-exe

re:	fclean all