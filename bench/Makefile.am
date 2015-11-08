SUFFIXES = .c .scm

BENCH_ENVIRONMENT = LD_LIBRARY_PATH=$(srcdir)/.libs:$(LD_LIBRARY_PATH) \
	GC_INITIAL_HEAP_SIZE=1G GC_USE_ENTIRE_HEAP=Y

EXTRA_LTLIBRARIES = libguile-bench.la

libguile_bench_la_SOURCES = cbench.c
libguile_bench_la_CFLAGS = $(GUILE_CFLAGS)
libguile_bench_la_LDFLAGS = -no-undefined -rpath /nowhere
libguile_bench_la_LIBADD = $(GUILE_LIBS)

noinst_DATA = bench.scm

bench-local: bench.scm libguile-bench.la
	$(BENCH_ENVIRONMENT) $(GUILE) bench.scm

EXTRA_DIST = bench.scm

CLEANFILES = *~ *.tmp *.go* *.la

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = Makefile.in
