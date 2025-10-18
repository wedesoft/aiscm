# ===========================================================================
#          http://www.gnu.org/software/autoconf-archive/ax_llvm.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_LLVMC([llvm-libs])
#
# DESCRIPTION
#
#   Test for the existance of llvm, and make sure that it can be linked with
#   the llvm-libs argument that is passed on to llvm-config i.e.:
#
#     llvm --libs <llvm-libs>
#
#   llvm-config will also include any libraries that are depended apon.
#
# LICENSE
#
#   Copyright (c) 2008 Andy Kitchen <agimbleinthewabe@gmail.com>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 12

AC_DEFUN([AX_LLVMC],
[
AC_ARG_WITH([llvm],
	AS_HELP_STRING([--with-llvm@<:@=DIR@:>@], [use llvm (default is yes) - it is possible to specify the root directory for llvm (optional)]),
	[
    if test "$withval" = "no"; then
		want_llvm="no"
    elif test "$withval" = "yes"; then
        want_llvm="yes"
        ac_llvm_config_path=`which llvm-config-19`
    else
	    want_llvm="yes"
        ac_llvm_config_path="$withval"
	fi
    ],
    [want_llvm="yes"])

	succeeded=no
	if test -z "$ac_llvm_config_path"; then
		ac_llvm_config_path=`which llvm-config-19`
	fi

	if test "x$want_llvm" = "xyes"; then
		if test -e "$ac_llvm_config_path"; then
			LLVM_CFLAGS=`$ac_llvm_config_path --cflags`
			LLVM_LDFLAGS=`$ac_llvm_config_path --ldflags`
			LLVM_LIBS="$($ac_llvm_config_path --libs $1)"

			AC_REQUIRE([AC_PROG_CC])
			CFLAGS_SAVED="$CFLAGS"
			CFLAGS="$CFLAGS $LLVM_CFLAGS"
			export CFLAGS

			LDFLAGS_SAVED="$LDFLAGS"
			LDFLAGS="$LDFLAGS $LLVM_LDFLAGS"
			export LDFLAGS

			LIBS_SAVED="$LIBS"
			LIBS="$LIBS $LLVM_LIBS"
			export LIBS

			AC_CACHE_CHECK(can compile with and link with llvm-c 11 ([$1]),
						   ax_cv_llvm,
		[AC_LINK_IFELSE([AC_LANG_PROGRAM([[@%:@include <llvm-c/ExecutionEngine.h>
													]],
					   [[LLVMLinkInMCJIT(); return 0;]])],
			   ax_cv_llvm=yes, ax_cv_llvm=no)
			])

			if test "x$ax_cv_llvm" = "xyes"; then
				succeeded=yes
			fi

			CFLAGS="$CFLAGS_SAVED"
      LDFLAGS="$LDFLAGS_SAVED"
      LIBS="$LIBS_SAVED"
		else
			succeeded=no
		fi
	fi

		if test "$succeeded" != "yes" ; then
			AC_MSG_ERROR([[We could not detect the llvm libraries make sure that llvm-config-19 is on your path or specified by --with-llvm.]])
		else
			AC_SUBST(LLVM_CFLAGS)
			AC_SUBST(LLVM_LDFLAGS)
			AC_SUBST(LLVM_LIBS)
			AC_DEFINE(HAVE_LLVM,,[define if the llvm library is available])
		fi
])
