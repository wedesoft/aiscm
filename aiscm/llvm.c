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


static scm_t_bits llvm_module_tag;

static scm_t_bits llvm_function_tag;

static scm_t_bits llvm_value_tag;

struct llvm_module_t {
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

static struct llvm_module_t *get_llvm_no_check(SCM scm_self)
{
  return (struct llvm_module_t *)SCM_SMOB_DATA(scm_self);
}

static struct llvm_module_t *get_llvm(SCM scm_self)
{
  scm_assert_smob_type(llvm_module_tag, scm_self);
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

SCM llvm_module_destroy(SCM scm_self);

size_t free_llvm_module(SCM scm_self)
{
  struct llvm_module_t *self = get_llvm_no_check(scm_self);
  llvm_module_destroy(scm_self);
  scm_gc_free(self, sizeof(struct llvm_module_t), "llvm");
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

static LLVMTypeRef llvm_type(int type)
{
  switch (type) {
    case SCM_FOREIGN_TYPE_FLOAT:
      return LLVMFloatType();
    case SCM_FOREIGN_TYPE_DOUBLE:
      return LLVMDoubleType();
    case SCM_FOREIGN_TYPE_UINT8:
    case SCM_FOREIGN_TYPE_INT8:
      return LLVMInt8Type();
    case SCM_FOREIGN_TYPE_UINT16:
    case SCM_FOREIGN_TYPE_INT16:
      return LLVMInt16Type();
    case SCM_FOREIGN_TYPE_UINT32:
    case SCM_FOREIGN_TYPE_INT32:
      return LLVMInt32Type();
    case SCM_FOREIGN_TYPE_UINT64:
    case SCM_FOREIGN_TYPE_INT64:
      return LLVMInt64Type();
    default:
      return LLVMVoidType();
  };
}

static int llvm_type_to_foreign_type(LLVMTypeRef type)
{
  LLVMDumpType(type);
  switch (LLVMGetTypeKind(type)) {
    case LLVMFloatTypeKind:
      return SCM_FOREIGN_TYPE_FLOAT;
    case LLVMDoubleTypeKind:
      return SCM_FOREIGN_TYPE_DOUBLE;
    case LLVMIntegerTypeKind:
      switch (LLVMGetIntTypeWidth(type)) {
        case 8:
          return SCM_FOREIGN_TYPE_INT8;
        case 16:
          return SCM_FOREIGN_TYPE_INT16;
        case 32:
          return SCM_FOREIGN_TYPE_INT32;
        case 64:
          return SCM_FOREIGN_TYPE_INT64;
        default:
          return SCM_FOREIGN_TYPE_VOID;
      };
    default:
      return SCM_FOREIGN_TYPE_VOID;
  };
}

static LLVMValueRef scm_to_llvm_value(int type, SCM scm_value)
{
  switch (type) {
    case SCM_FOREIGN_TYPE_FLOAT:
    case SCM_FOREIGN_TYPE_DOUBLE:
      return LLVMConstReal(llvm_type(type), scm_to_double(scm_value));
    case SCM_FOREIGN_TYPE_UINT8:
    case SCM_FOREIGN_TYPE_UINT16:
    case SCM_FOREIGN_TYPE_UINT32:
    case SCM_FOREIGN_TYPE_UINT64:
      return LLVMConstInt(llvm_type(type), scm_to_uint64(scm_value), 0);
    case SCM_FOREIGN_TYPE_INT8:
    case SCM_FOREIGN_TYPE_INT16:
    case SCM_FOREIGN_TYPE_INT32:
    case SCM_FOREIGN_TYPE_INT64:
      return LLVMConstInt(llvm_type(type), scm_to_int64(scm_value), 1);
    default:
      return NULL;
  };
}

SCM make_llvm_module_base(void)
{
  SCM retval;
  struct llvm_module_t *self;
  self = (struct llvm_module_t *)scm_gc_calloc(sizeof(struct llvm_module_t), "llvm");
  SCM_NEWSMOB(retval, llvm_module_tag, self);
  self->module = LLVMModuleCreateWithName("aiscm");
  return retval;
}

SCM llvm_module_destroy(SCM scm_self)
{
  struct llvm_module_t *self = get_llvm_no_check(scm_self);
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

SCM llvm_dump_module(SCM scm_self)
{
  struct llvm_module_t *self = get_llvm(scm_self);
  LLVMDumpModule(self->module);
  return SCM_UNSPECIFIED;
}

static LLVMTypeRef function_type(SCM scm_return_type, SCM scm_argument_types)
{
  int n_arguments = scm_ilength(scm_argument_types);
  LLVMTypeRef *parameters = scm_gc_malloc_pointerless(n_arguments * sizeof(LLVMTypeRef), "make-llvm-function");
  for (int i=0; i<n_arguments; i++) {
    parameters[i] = llvm_type(scm_to_int(scm_car(scm_argument_types)));
    scm_argument_types = scm_cdr(scm_argument_types);
  };
  return LLVMFunctionType(llvm_type(scm_to_int(scm_return_type)), parameters, n_arguments, 0);
}

SCM make_llvm_function(SCM scm_llvm, SCM scm_return_type, SCM scm_name, SCM scm_argument_types)
{
  SCM retval;
  struct llvm_module_t *llvm = get_llvm(scm_llvm);
  struct llvm_function_t *self;
  self = (struct llvm_function_t *)scm_gc_calloc(sizeof(struct llvm_function_t), "llvmfunction");
  SCM_NEWSMOB(retval, llvm_function_tag, self);
  self->builder = LLVMCreateBuilder();
  self->function = LLVMAddFunction(llvm->module,
                                   scm_to_locale_string(scm_name),
                                   function_type(scm_return_type, scm_argument_types));
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

SCM llvm_compile_module(SCM scm_llvm, SCM scm_name)
{
  struct llvm_module_t *self = get_llvm(scm_llvm);
  if (self->engine != NULL)
    scm_misc_error("llvm-compile", "LLVM module already compiled", SCM_EOL);
  char *error = NULL;
  if (LLVMCreateJITCompilerForModule(&self->engine, self->module, 2, &error)) {
    SCM scm_error = scm_from_locale_string(error);
    LLVMDisposeMessage(error);
    scm_misc_error("llvm-compile", "Error initialising JIT engine: ~a", scm_list_1(scm_error));
  };
  return SCM_UNSPECIFIED;
}

SCM llvm_get_function_address(SCM scm_llvm, SCM scm_name)
{
  struct llvm_module_t *self = get_llvm(scm_llvm);
  return scm_from_pointer((void *)LLVMGetFunctionAddress(self->engine, scm_to_locale_string(scm_name)), NULL);
}

SCM llvm_verify_module(SCM scm_llvm)
{
  struct llvm_module_t *llvm = get_llvm(scm_llvm);
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
  int type = scm_to_int(scm_type);
  self->value = scm_to_llvm_value(type, scm_value);
  return retval;
}

SCM llvm_get_type(SCM scm_self)
{
  struct llvm_value_t *self = get_llvm_value(scm_self);
  return scm_from_int(llvm_type_to_foreign_type(LLVMTypeOf(self->value)));
}

SCM llvm_build_load(SCM scm_function, SCM scm_type, SCM scm_address)
{
  SCM retval;
  struct llvm_function_t *function = get_llvm_function(scm_function);
  struct llvm_value_t *result = (struct llvm_value_t *)scm_gc_calloc(sizeof(struct llvm_value_t), "llvmvalue");
  SCM_NEWSMOB(retval, llvm_value_tag, result);
  struct llvm_value_t *address = get_llvm_value(scm_address);
  int type = scm_to_int(scm_type);
  LLVMValueRef pointer = LLVMConstIntToPtr(address->value, LLVMPointerType(llvm_type(type), 0));
  result->value = LLVMBuildLoad(function->builder, pointer, "");
  return retval;
}

SCM llvm_build_store(SCM scm_function, SCM scm_type, SCM scm_value, SCM scm_address, SCM scm_offset)
{
  struct llvm_function_t *function = get_llvm_function(scm_function);
  struct llvm_value_t *value = get_llvm_value(scm_value);
  struct llvm_value_t *address = get_llvm_value(scm_address);
  int type = scm_to_int(scm_type);
  LLVMValueRef pointer = LLVMConstIntToPtr(address->value, LLVMPointerType(llvm_type(type), 0));
  LLVMValueRef offset = scm_to_llvm_value(SCM_FOREIGN_TYPE_UINT64, scm_offset);
  pointer = LLVMBuildGEP(function->builder, pointer, &offset, 1, "llvm-build-store");
  LLVMBuildStore(function->builder, value->value, pointer);
  return SCM_UNSPECIFIED;
}

SCM llvm_get_param(SCM scm_function, SCM scm_index)
{
  SCM retval;
  struct llvm_function_t *function = get_llvm_function(scm_function);
  int index = scm_to_int(scm_index);
  struct llvm_value_t *result = (struct llvm_value_t *)scm_gc_calloc(sizeof(struct llvm_value_t), "llvmvalue");
  SCM_NEWSMOB(retval, llvm_value_tag, result);
  result->value = LLVMGetParam(function->function, index);
  return retval;
}

SCM llvm_build_unary(LLVMValueRef (*build_unary)(LLVMBuilderRef, LLVMValueRef, const char*),
                     SCM scm_function, SCM scm_value)
{
  SCM retval;
  struct llvm_function_t *function = get_llvm_function(scm_function);
  struct llvm_value_t *value = get_llvm_value(scm_value);
  struct llvm_value_t *result = (struct llvm_value_t *)scm_gc_calloc(sizeof(struct llvm_value_t), "llvmvalue");
  SCM_NEWSMOB(retval, llvm_value_tag, result);
  result->value = (*build_unary)(function->builder, value->value, "");
  return retval;
}

SCM llvm_build_neg(SCM scm_function, SCM scm_value)
{
  return llvm_build_unary(LLVMBuildNeg, scm_function, scm_value);
}

SCM llvm_build_fneg(SCM scm_function, SCM scm_value)
{
  return llvm_build_unary(LLVMBuildFNeg, scm_function, scm_value);
}

SCM llvm_build_not(SCM scm_function, SCM scm_value)
{
  return llvm_build_unary(LLVMBuildNot, scm_function, scm_value);
}

SCM llvm_build_binary(LLVMValueRef (*build_binary)(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char*),
                      SCM scm_function, SCM scm_value_a, SCM scm_value_b)
{
  SCM retval;
  struct llvm_function_t *function = get_llvm_function(scm_function);
  struct llvm_value_t *value_a = get_llvm_value(scm_value_a);
  struct llvm_value_t *value_b = get_llvm_value(scm_value_b);
  struct llvm_value_t *result = (struct llvm_value_t *)scm_gc_calloc(sizeof(struct llvm_value_t), "llvmvalue");
  SCM_NEWSMOB(retval, llvm_value_tag, result);
  result->value = (*build_binary)(function->builder, value_a->value, value_b->value, "");
  return retval;
}

SCM llvm_build_add(SCM scm_function, SCM scm_value_a, SCM scm_value_b)
{
  return llvm_build_binary(LLVMBuildAdd, scm_function, scm_value_a, scm_value_b);
}

SCM llvm_build_fadd(SCM scm_function, SCM scm_value_a, SCM scm_value_b)
{
  return llvm_build_binary(LLVMBuildFAdd, scm_function, scm_value_a, scm_value_b);
}

SCM llvm_build_sub(SCM scm_function, SCM scm_value_a, SCM scm_value_b)
{
  return llvm_build_binary(LLVMBuildSub, scm_function, scm_value_a, scm_value_b);
}

SCM llvm_build_fsub(SCM scm_function, SCM scm_value_a, SCM scm_value_b)
{
  return llvm_build_binary(LLVMBuildFSub, scm_function, scm_value_a, scm_value_b);
}

SCM llvm_build_mul(SCM scm_function, SCM scm_value_a, SCM scm_value_b)
{
  return llvm_build_binary(LLVMBuildMul, scm_function, scm_value_a, scm_value_b);
}

SCM llvm_build_fmul(SCM scm_function, SCM scm_value_a, SCM scm_value_b)
{
  return llvm_build_binary(LLVMBuildFMul, scm_function, scm_value_a, scm_value_b);
}

SCM llvm_build_cast(LLVMValueRef (*build_cast)(LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, const char*),
                    SCM scm_function, SCM scm_type, SCM scm_value)
{
  SCM retval;
  struct llvm_function_t *function = get_llvm_function(scm_function);
  struct llvm_value_t *value = get_llvm_value(scm_value);
  LLVMTypeRef type = llvm_type(scm_to_int(scm_type));
  struct llvm_value_t *result = (struct llvm_value_t *)scm_gc_calloc(sizeof(struct llvm_value_t), "llvmvalue");
  SCM_NEWSMOB(retval, llvm_value_tag, result);
  result->value = (*build_cast)(function->builder, value->value, type, "");
  return retval;
}

SCM llvm_build_trunc(SCM scm_function, SCM scm_type, SCM scm_value)
{
  return llvm_build_cast(LLVMBuildTrunc, scm_function, scm_type, scm_value);
}

SCM llvm_build_sext(SCM scm_function, SCM scm_type, SCM scm_value)
{
  return llvm_build_cast(LLVMBuildSExt, scm_function, scm_type, scm_value);
}

SCM llvm_build_zext(SCM scm_function, SCM scm_type, SCM scm_value)
{
  return llvm_build_cast(LLVMBuildZExt, scm_function, scm_type, scm_value);
}

SCM llvm_build_fp_cast(SCM scm_function, SCM scm_type, SCM scm_value)
{
  return llvm_build_cast(LLVMBuildFPCast, scm_function, scm_type, scm_value);
}

SCM llvm_build_fp_to_si(SCM scm_function, SCM scm_type, SCM scm_value)
{
  return llvm_build_cast(LLVMBuildFPToSI, scm_function, scm_type, scm_value);
}

SCM llvm_build_fp_to_ui(SCM scm_function, SCM scm_type, SCM scm_value)
{
  return llvm_build_cast(LLVMBuildFPToUI, scm_function, scm_type, scm_value);
}

SCM llvm_build_si_to_fp(SCM scm_function, SCM scm_type, SCM scm_value)
{
  return llvm_build_cast(LLVMBuildSIToFP, scm_function, scm_type, scm_value);
}

SCM llvm_build_ui_to_fp(SCM scm_function, SCM scm_type, SCM scm_value)
{
  return llvm_build_cast(LLVMBuildUIToFP, scm_function, scm_type, scm_value);
}

SCM llvm_build_call(SCM scm_function, SCM scm_llvm, SCM scm_return_type, SCM scm_function_name, SCM scm_argument_types, SCM scm_values)
{
  SCM retval;
  struct llvm_function_t *function = get_llvm_function(scm_function);
  struct llvm_module_t *llvm = get_llvm(scm_llvm);
  const char *function_name = scm_to_locale_string(scm_function_name);
  LLVMValueRef function_pointer = LLVMAddFunction(llvm->module, function_name, function_type(scm_return_type, scm_argument_types));
  LLVMAddFunctionAttr(function_pointer, LLVMExternalLinkage);
  int n_values = scm_ilength(scm_values);
  LLVMValueRef *values = scm_gc_malloc_pointerless(n_values * sizeof(LLVMValueRef), "llvm-build-call");
  for (int i=0; i<n_values; i++) {
    values[i] = get_llvm_value(scm_car(scm_values))->value;
    scm_values = scm_cdr(scm_values);
  };
  struct llvm_value_t *result = (struct llvm_value_t *)scm_gc_calloc(sizeof(struct llvm_value_t), "llvmvalue");
  SCM_NEWSMOB(retval, llvm_value_tag, result);
  result->value = LLVMBuildCall(function->builder, function_pointer, values, n_values, "llvm-build-call");
  return retval;
}

void init_llvm(void)
{
  LLVMLinkInMCJIT();
  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();

  llvm_module_tag = scm_make_smob_type("llvmmodule", sizeof(struct llvm_module_t));
  scm_set_smob_free(llvm_module_tag, free_llvm_module);

  llvm_function_tag = scm_make_smob_type("llvmfunction", sizeof(struct llvm_function_t));
  scm_set_smob_free(llvm_function_tag, free_llvm_function);

  llvm_value_tag = scm_make_smob_type("llvmvalue", sizeof(struct llvm_value_t));

  scm_c_define_gsubr("make-llvm-module-base"    , 0, 0, 0, SCM_FUNC(make_llvm_module_base    ));
  scm_c_define_gsubr("llvm-module-destroy"      , 1, 0, 0, SCM_FUNC(llvm_module_destroy      ));
  scm_c_define_gsubr("llvm-dump-module"         , 1, 0, 0, SCM_FUNC(llvm_dump_module         ));
  scm_c_define_gsubr("make-llvm-function"       , 4, 0, 0, SCM_FUNC(make_llvm_function       ));
  scm_c_define_gsubr("llvm-function-destroy"    , 1, 0, 0, SCM_FUNC(llvm_function_destroy    ));
  scm_c_define_gsubr("llvm-function-return"     , 2, 0, 0, SCM_FUNC(llvm_function_return     ));
  scm_c_define_gsubr("llvm-function-return-void", 1, 0, 0, SCM_FUNC(llvm_function_return_void));
  scm_c_define_gsubr("llvm-compile-module"      , 1, 0, 0, SCM_FUNC(llvm_compile_module      ));
  scm_c_define_gsubr("llvm-get-function-address", 2, 0, 0, SCM_FUNC(llvm_get_function_address));
  scm_c_define_gsubr("llvm-verify-module"       , 1, 0, 0, SCM_FUNC(llvm_verify_module       ));
  scm_c_define_gsubr("make-llvm-constant"       , 2, 0, 0, SCM_FUNC(make_llvm_constant       ));
  scm_c_define_gsubr("llvm-get-type"            , 1, 0, 0, SCM_FUNC(llvm_get_type            ));
  scm_c_define_gsubr("llvm-build-load"          , 3, 0, 0, SCM_FUNC(llvm_build_load          ));
  scm_c_define_gsubr("llvm-build-store"         , 5, 0, 0, SCM_FUNC(llvm_build_store         ));
  scm_c_define_gsubr("llvm-get-param"           , 2, 0, 0, SCM_FUNC(llvm_get_param           ));
  scm_c_define_gsubr("llvm-build-neg"           , 2, 0, 0, SCM_FUNC(llvm_build_neg           ));
  scm_c_define_gsubr("llvm-build-fneg"          , 2, 0, 0, SCM_FUNC(llvm_build_fneg          ));
  scm_c_define_gsubr("llvm-build-not"           , 2, 0, 0, SCM_FUNC(llvm_build_not           ));
  scm_c_define_gsubr("llvm-build-add"           , 3, 0, 0, SCM_FUNC(llvm_build_add           ));
  scm_c_define_gsubr("llvm-build-fadd"          , 3, 0, 0, SCM_FUNC(llvm_build_fadd          ));
  scm_c_define_gsubr("llvm-build-sub"           , 3, 0, 0, SCM_FUNC(llvm_build_sub           ));
  scm_c_define_gsubr("llvm-build-fsub"          , 3, 0, 0, SCM_FUNC(llvm_build_fsub          ));
  scm_c_define_gsubr("llvm-build-mul"           , 3, 0, 0, SCM_FUNC(llvm_build_mul           ));
  scm_c_define_gsubr("llvm-build-fmul"          , 3, 0, 0, SCM_FUNC(llvm_build_fmul          ));
  scm_c_define_gsubr("llvm-build-trunc"         , 3, 0, 0, SCM_FUNC(llvm_build_trunc         ));
  scm_c_define_gsubr("llvm-build-sext"          , 3, 0, 0, SCM_FUNC(llvm_build_sext          ));
  scm_c_define_gsubr("llvm-build-zext"          , 3, 0, 0, SCM_FUNC(llvm_build_zext          ));
  scm_c_define_gsubr("llvm-build-fp-cast"       , 3, 0, 0, SCM_FUNC(llvm_build_fp_cast       ));
  scm_c_define_gsubr("llvm-build-fp-to-si"      , 3, 0, 0, SCM_FUNC(llvm_build_fp_to_si      ));
  scm_c_define_gsubr("llvm-build-fp-to-ui"      , 3, 0, 0, SCM_FUNC(llvm_build_fp_to_ui      ));
  scm_c_define_gsubr("llvm-build-si-to-fp"      , 3, 0, 0, SCM_FUNC(llvm_build_si_to_fp      ));
  scm_c_define_gsubr("llvm-build-ui-to-fp"      , 3, 0, 0, SCM_FUNC(llvm_build_ui_to_fp      ));
  scm_c_define_gsubr("llvm-build-call"          , 6, 0, 0, SCM_FUNC(llvm_build_call          ));
}
