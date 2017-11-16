
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

static scm_t_bits llvm_function_tag;

struct llvm_t {
  LLVMModuleRef module;
};

struct llvm_function_t {
  LLVMBuilderRef builder;
  LLVMValueRef function;
};

static struct llvm_t *get_llvm_no_check(SCM scm_self)
{
  return (struct llvm_t *)SCM_SMOB_DATA(scm_self);
}

static struct llvm_t *get_llvm(SCM scm_self)
{
  scm_assert_smob_type(llvm_tag, scm_self);
  return get_llvm_no_check(scm_self);
}

static struct llvm_function_t *get_llvm_function_no_check(SCM scm_self)
{
  return (struct llvm_function_t *)SCM_SMOB_DATA(scm_self);
}

static struct llvm_function_t *get_llvm_function(SCM scm_self)
{
  scm_assert_smob_type(llvm_function_tag, scm_self);
  return get_llvm_function_no_check(scm_self);
}

SCM llvm_context_destroy(SCM scm_self);

size_t free_llvm(SCM scm_self)
{
  struct llvm_t *self = get_llvm_no_check(scm_self);
  llvm_context_destroy(scm_self);
  scm_gc_free(self, sizeof(struct llvm_t), "llvm");
  return 0;
}

SCM llvm_function_destroy(SCM scm_self);

size_t free_llvm_function(SCM scm_self)
{
  struct llvm_function_t *self = get_llvm_function_no_check(scm_self);
  llvm_function_destroy(scm_self);
  scm_gc_free(self, sizeof(struct llvm_function_t), "function");
  return 0;
}

SCM make_llvm_context(void)
{
  SCM retval;
  struct llvm_t *self;
  self = (struct llvm_t *)scm_gc_calloc(sizeof(struct llvm_t), "llvm");
  SCM_NEWSMOB(retval, llvm_tag, self);
  self->module = LLVMModuleCreateWithName("aiscm");
  return retval;
}

SCM llvm_context_destroy(SCM scm_self)
{
  struct llvm_t *self = get_llvm_no_check(scm_self);
  if (self->module) {
    LLVMDisposeModule(self->module);
    self->module = NULL;
  };
  return SCM_UNSPECIFIED;
}

SCM make_llvm_function(SCM scm_llvm, SCM scm_name)
{
  SCM retval;
  struct llvm_t *llvm = get_llvm(scm_llvm);
  struct llvm_function_t *self;
  self = (struct llvm_function_t *)scm_gc_calloc(sizeof(struct llvm_function_t), "llvm");
  SCM_NEWSMOB(retval, llvm_function_tag, self);
  self->builder = LLVMCreateBuilder();
  self->function = LLVMAddFunction(llvm->module, scm_to_locale_string(scm_name), LLVMFunctionType(LLVMVoidType(), NULL, 0, 0));
  LLVMSetFunctionCallConv(self->function, LLVMCCallConv);
  LLVMBasicBlockRef entry = LLVMAppendBasicBlock(self->function, "entry");
  LLVMPositionBuilderAtEnd(self->builder, entry);
  return retval;
}

SCM llvm_function_destroy(SCM scm_self)
{
  struct llvm_function_t *self = get_llvm_function_no_check(scm_self);
  if (self->builder) {
    LLVMDisposeBuilder(self->builder);
    self->builder = NULL;
  };
  return SCM_UNSPECIFIED;
}

SCM llvm_function_ret(SCM scm_self)
{
  struct llvm_function_t *self = get_llvm_function(scm_self);
  LLVMBuildRetVoid(self->builder);
  return SCM_UNSPECIFIED;
}

void init_llvm(void)
{
  LLVMLinkInMCJIT();
  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();

  llvm_tag = scm_make_smob_type("llvmcontext", sizeof(struct llvm_t));
  scm_set_smob_free(llvm_tag, free_llvm);

  llvm_function_tag = scm_make_smob_type("llvmfunction", sizeof(struct llvm_function_t));
  scm_set_smob_free(llvm_function_tag, free_llvm_function);

  scm_c_define_gsubr("make-llvm-context"    , 0, 0, 0, SCM_FUNC(make_llvm_context    ));
  scm_c_define_gsubr("llvm-context-destroy" , 1, 0, 0, SCM_FUNC(llvm_context_destroy ));
  scm_c_define_gsubr("make-llvm-function"   , 2, 0, 0, SCM_FUNC(make_llvm_function   ));
  scm_c_define_gsubr("llvm-function-destroy", 1, 0, 0, SCM_FUNC(llvm_function_destroy));
  scm_c_define_gsubr("llvm-function-ret"    , 1, 0, 0, SCM_FUNC(llvm_function_ret    ));
}
