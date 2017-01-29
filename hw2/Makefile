DIRS=util,x86,Int64_overflow

main.native:
	ocamlbuild -Is $(DIRS) main.native

main.byte:
	ocamlbuild -Is $(DIRS) main.byte

all: main.native

clean:
	ocamlbuild -clean
	rm main.native
