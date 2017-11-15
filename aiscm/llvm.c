
// AIscm - Guile extension for numerical arrays and tensors.
// Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Wedekind <jan@wedesoft.de>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
#include <libguile.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include "util-helpers.h"


static scm_t_bits llvm_tag;

struct llvm_t {
  LLVMModuleRef module;
};

static struct llvm_t *get_self_no_check(SCM scm_self)
{
  return (struct llvm_t *)SCM_SMOB_DATA(scm_self);
}

SCM llvmcontext_destroy(SCM scm_self)
{
  struct llvm_t *self = get_self_no_check(scm_self);
  if (self->module) {
    LLVMDisposeModule(self->module);
    self->module = NULL;
  };
  return SCM_UNSPECIFIED;
}

size_t free_llvm(SCM scm_self)
{
  struct llvm_t *self = get_self_no_check(scm_self);
  llvmcontext_destroy(scm_self);
  scm_gc_free(self, sizeof(struct llvm_t), "llvm");
  return 0;
}

SCM make_llvmcontext(void)
{
  SCM retval;
  struct llvm_t *self;
  self = (struct llvm_t *)scm_gc_calloc(sizeof(struct llvm_t), "llvm");
  SCM_NEWSMOB(retval, llvm_tag, self);
  return retval;
}

void init_llvm(void)
{
  LLVMLinkInMCJIT();
  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();

  llvm_tag = scm_make_smob_type("llvmcontext", sizeof(struct llvm_t));
  scm_set_smob_free(llvm_tag, free_llvm);
  scm_c_define_gsubr("make-llvmcontext"   , 0, 0, 0, SCM_FUNC(make_llvmcontext   ));
  scm_c_define_gsubr("llvmcontext-destroy", 1, 0, 0, SCM_FUNC(llvmcontext_destroy));
}
