.PHONY: default all clean FORCE

# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

LLVMCONFIG=llvm-config
LLVMLDFLAGS=-L`$(LLVMCONFIG) --libdir`
LLVMPACKAGES=llvm,llvm.scalar_opts,llvm.analysis,llvm.all_backends


OCAMLBUILD=ocamlbuild
OCAMLBUILDFLAGS=-use-ocamlfind -pkgs $(LLVMPACKAGES)  -lflags -cclib,$(LLVMLDFLAGS) -no-hygiene
MV=mv

default: tony$(EXE)

extend.cmo: extend.ml
	$(OCAMLC) -pp "camlp5o pa_extend.cmo q_MLast.cmo" -I +camlp5 -c $<

Main.native: FORCE
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $@

Main.byte: FORCE
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $@
Main.d.byte: FORCE
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $@

tony$(EXE): Main.native
	$(MV) $^ $@

clean:
	$(OCAMLBUILD) -clean
	$(RM) *.cmo
	$(RM) a.ll a.s a.out
	$(RM) *~
