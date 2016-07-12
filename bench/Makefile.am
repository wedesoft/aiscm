SUFFIXES = .c .scm

BENCH_ENVIRONMENT = LD_LIBRARY_PATH=$(srcdir)/.libs:$(LD_LIBRARY_PATH) \
	GC_INITIAL_HEAP_SIZE=1G GC_USE_ENTIRE_HEAP=Y

EXTRA_LTLIBRARIES = libguile-aiscm-bench.la

libguile_aiscm_bench_la_SOURCES = cbench.c
libguile_aiscm_bench_la_CFLAGS = $(GUILE_CFLAGS)
libguile_aiscm_bench_la_LDFLAGS = -no-undefined -rpath /nowhere
libguile_aiscm_bench_la_LIBADD = $(GUILE_LIBS)

noinst_DATA = bench.scm

bench-local: bench.scm libguile-aiscm-bench.la
	$(BENCH_ENVIRONMENT) $(GUILE) bench.scm

EXTRA_DIST = bench.scm

CLEANFILES = *~ *.tmp *.go* *.la

MAINTAINERCLEANFILES = Makefile.in

maintainer-clean-local:
	rm -Rf .deps
