CAMLC=camlc
COMPFLAGS=-W
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
CPP=/lib/cpp -P
CAMLDEP=camldep
RM=rm -f
TOP=../.
CONFIG=$(TOP)/config

all::

all::trec

trec: utils.zo recterme.zo lexer.zo parser.zo trec.zo
	$(CAMLC) $(LINKFLAGS)  utils.zo recterme.zo lexer.zo parser.zo trec.zo -o trec

clean::
	$(RM) trec

parser.ml parser.mli: parser.mly
	@$(RM) parser.ml parser.mli
	$(CAMLYACC) parser.mly
	@chmod a-w parser.ml parser.mli

clean::
	$(RM) parser.ml parser.mli

beforedepend:: parser.ml parser.mli recterme.ml recterme.mli

lexer.ml: lexer.mll
	@$(RM) lexer.ml
	$(CAMLLEX) lexer.mll
	@chmod a-w lexer.ml

clean::
	$(RM) lexer.ml

beforedepend:: lexer.ml

# (end Imakefile)

clean::
	$(RM) *.zi *.zix *.zo *~ #*#

beforedepend::

makefile: beforedepend
	imake -I$(CONFIG) -DTOPDIR=$(TOP)
	$(CAMLDEP) *.mli *.ml >> Makefile

makefiles:: makefile

.SUFFIXES: .mli .zi .ml .zo

.mli.zi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.zo:
	$(CAMLC) $(COMPFLAGS) -c $<

# Generated dependencies below

trec.zo: utils.zi parser.zi lexer.zo
utils.zo: utils.zi
recterme.zo: recterme.zi
lexer.zo: parser.zi
parser.zo: parser.zi
