build: hello.idr
	idris hello.idr -o hello

run: build
	./hello

check:
	idris hello.idr --check