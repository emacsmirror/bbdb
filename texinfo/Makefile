MAKEINFO=makeinfo

info:	bbdb.info

all:	bbdb.dvi bbdb.info

bbdb.info: bbdb.texinfo
	$(MAKEINFO) bbdb.texinfo

bbdb.dvi: bbdb.texinfo
	texi2dvi bbdb.texinfo

clean:
	$(RM) -f bbdb.info* *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.log *.pg \
	  *.toc *.tp *.vr *.vrs


