build:
	$(MAKE) -C src xnode.so

install: build
	$(MAKE) -C src install

clean:
	$(MAKE) -C src clean
