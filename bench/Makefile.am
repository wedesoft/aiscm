SUFFIXES = .c .scm

BENCH_ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(top_builddir)/bench/.libs:$(LD_LIBRARY_PATH)
BENCH_RUN = $(GUILE) --no-auto-compile -L $(top_builddir)

EXTRA_LTLIBRARIES = libguile-bench.la

libguile_bench_la_SOURCES = cbench.c
libguile_bench_la_CFLAGS = $(GUILE_CFLAGS)
libguile_bench_la_LDFLAGS = -no-undefined -rpath /nowhere
libguile_bench_la_LIBADD = $(GUILE_LIBS)

noinst_SCRIPTS = bench.scm

bench-local: bench.scm libguile-bench.la
	$(BENCH_ENVIRONMENT) $(BENCH_RUN) bench.scm

EXTRA_DIST = bench.scm

CLEANFILES = *~ *.tmp *.go*

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = Makefile.in
