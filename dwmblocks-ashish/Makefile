#PREFIX = /usr/local
PREFIX = ${HOME}/.local

CC = gcc
CFLAGS = -O3 -Wall -Wextra
CFLAGSEXTRA = -Wno-missing-field-initializers -Wno-unused-parameter

all: dwmblocks sigdwmblocks

dwmblocks: dwmblocks.c blocks.h
	${CC} -o $@ -lX11 ${CFLAGS} ${CFLAGSEXTRA} $<

sigdwmblocks: sigdwmblocks.c
	${CC} -o $@ ${CFLAGS} $<

xgetrootname: xgetrootname.c
	${CC} -o $@ -lX11 ${CFLAGS} $<

clean:
	rm -f dwmblocks sigdwmblocks

install: all
	mkdir -p ${DESTDIR}${PREFIX}
	install -m 0755 dwmblocks ${DESTDIR}${PREFIX}/dwmblocks
	install -m 0755 sigdwmblocks ${DESTDIR}${PREFIX}/sigdwmblocks

uninstall:
	rm -f ${DESTDIR}${PREFIX}/dwmblocks ${DESTDIR}${PREFIX}/sigdwmblocks

.PHONY: all clean install uninstall
