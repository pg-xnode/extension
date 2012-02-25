build:
	make -C src xnode.so

install: build
	make -C src install

clean:
	make -C src clean
