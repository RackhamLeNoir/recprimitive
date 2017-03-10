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

all::crec

crec: utils.zo combin.zo terme.zo lexer.zo parser.zo crec.zo
	$(CAMLC) $(LINKFLAGS)  utils.zo combin.zo terme.zo lexer.zo parser.zo crec.zo -o crec

clean::
	$(RM) crec

parser.ml parser.mli: parser.mly
	@$(RM) parser.ml parser.mli
	$(CAMLYACC) parser.mly
	@chmod a-w parser.ml parser.mli

clean::
	$(RM) parser.ml parser.mli

beforedepend:: parser.ml parser.mli combin.ml combin.mli terme.ml terme.mli

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

utils.zo : utils.zi
crec.zo: utils.zi combin.zi parser.zi lexer.zo
combin.zo: combin.zi utils.zi
terme.zo: terme.zi utils.zi
lexer.zo: parser.zi
parser.zo: parser.zi utils.zi
