build:
	$(MAKE) -C src xnode.so

install: build
	$(MAKE) -C src install

# Dependency on 'install' target is not defined here because non-root user may be used
# to run 'installcheck' target. If 'install' was called in such case to satisfy the dependency,
# it would fail.

installcheck:
	$(MAKE) -C src installcheck

clean:
	$(MAKE) -C src clean

