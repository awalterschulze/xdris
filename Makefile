deps:
	(cd vendor/specdris && ./project --install)

build:
	idris --build hello.ipkg

install: build
	idris --install hello.ipkg

run: install
	idris -p hello Main.idr -o main
	./main

clean:
	idris --clean hello.ipkg
	rm ./main || true
	rm *.ibc || true

test:
	idris $(DEPS) --testpkg hello.ipkg