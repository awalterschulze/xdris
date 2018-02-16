build:
	idris --build xdris.ipkg

install: build
	idris --install xdris.ipkg

run: install
	idris -p xdris Main.idr -o main
	./main

clean:
	idris --clean xdris.ipkg
	rm ./main || true
	rm *.ibc || true

test:
	idris $(DEPS) --testpkg xdris.ipkg

deps:
	(cd vendor/specdris && ./project --install)