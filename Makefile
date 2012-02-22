build:
	make -C src xnode.so

install: pg_xnode
	make -C src install

clean:
	make -C src clean
