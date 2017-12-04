
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
#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include "util-helpers.h"


static scm_t_bits llvm_tag;

static scm_t_bits llvm_function_tag;

static scm_t_bits llvm_value_tag;

struct llvm_t {
  LLVMModuleRef module;
  LLVMExecutionEngineRef engine;
};

struct llvm_function_t {
  LLVMBuilderRef builder;
  LLVMValueRef function;
};

struct llvm_value_t {
  LLVMValueRef value;
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

static struct llvm_value_t *get_llvm_value_no_check(SCM scm_self)
{
  return (struct llvm_value_t *)SCM_SMOB_DATA(scm_self);
}

static struct llvm_value_t *get_llvm_value(SCM scm_self)
{
  scm_assert_smob_type(llvm_value_tag, scm_self);
  return get_llvm_value_no_check(scm_self);
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
  char *error = NULL;
  if (LLVMCreateJITCompilerForModule(&self->engine, self->module, 2, &error)) {
    SCM scm_error = scm_from_locale_string(error);
    LLVMDisposeMessage(error);
    scm_misc_error("make-llvm", "Error initialising JIT engine: ~a", scm_list_1(scm_error));
  };
  return retval;
}

SCM llvm_context_destroy(SCM scm_self)
{
  struct llvm_t *self = get_llvm_no_check(scm_self);
  if (self->engine) {
    if (self->module) {
      char *error = NULL;
      LLVMRemoveModule(self->engine, self->module, &self->module, &error);
      if (error) LLVMDisposeMessage(error);
    };
    LLVMDisposeExecutionEngine(self->engine);
    self->engine = NULL;
  };
  if (self->module) {
    LLVMDisposeModule(self->module);
    self->module = NULL;
  };
  return SCM_UNSPECIFIED;
}

static LLVMTypeRef llvm_type(SCM scm_type)
{
  switch (scm_to_int(scm_type)) {
    case 8:
      return LLVMInt32Type();
    default:
      return LLVMVoidType();
  };
}

SCM make_llvm_function(SCM scm_llvm, SCM scm_type, SCM scm_name)
{
  SCM retval;
  struct llvm_t *llvm = get_llvm(scm_llvm);
  struct llvm_function_t *self;
  self = (struct llvm_function_t *)scm_gc_calloc(sizeof(struct llvm_function_t), "llvmfunction");
  SCM_NEWSMOB(retval, llvm_function_tag, self);
  self->builder = LLVMCreateBuilder();
  self->function = LLVMAddFunction(llvm->module, scm_to_locale_string(scm_name), LLVMFunctionType(llvm_type(scm_type), NULL, 0, 0));
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

SCM llvm_function_return(SCM scm_self, SCM scm_value)
{
  struct llvm_function_t *self = get_llvm_function(scm_self);
  struct llvm_value_t *value = get_llvm_value(scm_value);
  LLVMBuildRet(self->builder, value->value);
  return SCM_UNSPECIFIED;
}

SCM llvm_function_return_void(SCM scm_self)
{
  struct llvm_function_t *self = get_llvm_function(scm_self);
  LLVMBuildRetVoid(self->builder);
  return SCM_UNSPECIFIED;
}

static SCM scm_from_llvm_value(LLVMTypeRef type, LLVMGenericValueRef value)
{
  switch (LLVMGetTypeKind(type)) {
    case LLVMIntegerTypeKind:
      return scm_from_int(LLVMGenericValueToInt(value, 0));
    default:
      return SCM_UNSPECIFIED;
  };
}

SCM llvm_context_apply(SCM scm_llvm, SCM scm_function)
{
  struct llvm_t *llvm = get_llvm(scm_llvm);
  struct llvm_function_t *function = get_llvm_function(scm_function);
  LLVMGenericValueRef result = LLVMRunFunction(llvm->engine, function->function, 0, NULL);
  LLVMTypeRef return_type = LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(function->function)));
  SCM retval = scm_from_llvm_value(return_type, result);
  LLVMDisposeGenericValue(result);
  return retval;
}

SCM llvm_verify_module(SCM scm_llvm)
{
  struct llvm_t *llvm = get_llvm(scm_llvm);
  char *error = NULL;
  if (LLVMVerifyModule(llvm->module, LLVMPrintMessageAction, &error)) {
    SCM scm_error = scm_from_locale_string(error);
    LLVMDisposeMessage(error);
    scm_misc_error("verify-module", "Module is not valid: ~a", scm_list_1(scm_error));
  };
  return SCM_UNSPECIFIED;
}

SCM make_llvm_constant(SCM scm_type, SCM scm_value)
{
  SCM retval;
  struct llvm_value_t *self;
  self = (struct llvm_value_t *)scm_gc_calloc(sizeof(struct llvm_value_t), "llvmvalue");
  SCM_NEWSMOB(retval, llvm_value_tag, self);
  self->value = LLVMConstInt(llvm_type(scm_type), scm_to_int(scm_value), 0);
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

  llvm_function_tag = scm_make_smob_type("llvmfunction", sizeof(struct llvm_function_t));
  scm_set_smob_free(llvm_function_tag, free_llvm_function);

  llvm_value_tag = scm_make_smob_type("llvmvalue", sizeof(struct llvm_value_t));

  scm_c_define_gsubr("make-llvm-context"        , 0, 0, 0, SCM_FUNC(make_llvm_context        ));
  scm_c_define_gsubr("llvm-context-destroy"     , 1, 0, 0, SCM_FUNC(llvm_context_destroy     ));
  scm_c_define_gsubr("make-llvm-function"       , 3, 0, 0, SCM_FUNC(make_llvm_function       ));
  scm_c_define_gsubr("llvm-function-destroy"    , 1, 0, 0, SCM_FUNC(llvm_function_destroy    ));
  scm_c_define_gsubr("llvm-function-return"     , 2, 0, 0, SCM_FUNC(llvm_function_return     ));
  scm_c_define_gsubr("llvm-function-return-void", 1, 0, 0, SCM_FUNC(llvm_function_return_void));
  scm_c_define_gsubr("llvm-context-apply"       , 2, 0, 0, SCM_FUNC(llvm_context_apply       ));
  scm_c_define_gsubr("llvm-verify-module"       , 1, 0, 0, SCM_FUNC(llvm_verify_module       ));
  scm_c_define_gsubr("make-llvm-constant"       , 2, 0, 0, SCM_FUNC(make_llvm_constant       ));
}
