
OCAMLC     = ocamlc
OCAMLC_OPT = -c
OCAMLLEX   = ocamllex
OCAMLYACC  = ocamlyacc

TARGET = interp

all: $(TARGET)

GENERATED= \
				parser.mli \
				parser.ml \
				lexer.ml

CMOFILES= \
				syntax.cmo \
				value.cmo \
				environment.cmo \
				parser.cmo \
				lexer.cmo \
				ast_printer.cmo \
				interp.cmo \
				main.cmo

CMIFILES = \
				syntax.cmi \
				value.cmi \
				environment.cmi \
				interp.cmi \
				ast_printer.cmi


%.cmo: %.ml $(CMIFILES)
	$(OCAMLC) $(OCAMLC_OPT) $<

%.cmi: %.mli %.ml
	$(OCAMLC) $(OCAMLC_OPT) $<

$(TARGET): $(CMOFILES)
	$(OCAMLC) -o $@ $^

lexer.ml: lexer.mll
	$(OCAMLLEX) $<

parser.ml: parser.mly syntax.cmi
	$(OCAMLYACC) $<

parser.mli: parser.mly
	$(OCAMLYACC) $<

parser.cmo: parser.ml parser.cmi
	$(OCAMLC) $(OCAMLC_OPT) $<

environment.cmi: environment.mli syntax.cmi
	$(OCAMLC) $(OCAMLC_OPT) $<

ast_printer.cmo: ast_printer.ml $(CMIFILES)
	$(OCAMLC) $(OCAMLC_OPT) $<

syntax.cmo: syntax.ml syntax.cmi
	$(OCAMLC) $(OCAMLC_OPT) $<

value.cmo: value.ml $(CMIFILES)
	$(OCAMLC) $(OCAMLC_OPT) $<

interp.cmo: interp.ml syntax.cmi environment.cmi
	$(OCAMLC) $(OCAMLC_OPT) $<


clean:
	rm -f *.cmi *.cmo $(GENERATED) $(TARGET)
