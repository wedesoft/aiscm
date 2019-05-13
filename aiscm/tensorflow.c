// AIscm - Guile extension for numerical arrays and tensors.
// Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 Jan Wedekind <jan@wedesoft.de>
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
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <libguile.h>
#include <tensorflow/c/c_api.h>
#include "util-helpers.h"


static scm_t_bits tf_tensor_tag;

static scm_t_bits tf_graph_tag;

static scm_t_bits tf_description_tag;

static scm_t_bits tf_output_tag;

static scm_t_bits tf_session_tag;

struct tf_tensor_t {
  TF_Tensor *tensor;
};

struct tf_graph_t {
  TF_Graph *graph;
};

struct tf_description_t {
  TF_OperationDescription *description;
};

struct tf_output_t {
  TF_Output output;
};

struct tf_session_t {
  TF_Session *session;
  struct tf_graph_t *graph;// keep graph alive
};

static TF_Status *status;

static struct tf_tensor_t *get_tf_tensor_no_check(SCM scm_self)
{
  return (struct tf_tensor_t *)SCM_SMOB_DATA(scm_self);
}

static struct tf_tensor_t *get_tf_tensor(SCM scm_self)
{
  scm_assert_smob_type(tf_tensor_tag, scm_self);
  return get_tf_tensor_no_check(scm_self);
}

size_t free_tensor(SCM scm_self)
{
  struct tf_tensor_t *self = get_tf_tensor_no_check(scm_self);
  TF_DeleteTensor(self->tensor);
  scm_gc_free(self, sizeof(struct tf_tensor_t), "tensor");
  return 0;
}

static struct tf_graph_t *get_tf_graph_no_check(SCM scm_self)
{
  return (struct tf_graph_t *)SCM_SMOB_DATA(scm_self);
}

static struct tf_graph_t *get_tf_graph(SCM scm_self)
{
  scm_assert_smob_type(tf_graph_tag, scm_self);
  return get_tf_graph_no_check(scm_self);
}

size_t free_graph(SCM scm_self)
{
  struct tf_graph_t *self = get_tf_graph_no_check(scm_self);
  TF_DeleteGraph(self->graph);
  scm_gc_free(self, sizeof(struct tf_graph_t), "graph");
  return 0;
}

static struct tf_description_t *get_tf_description_no_check(SCM scm_self)
{
  return (struct tf_description_t *)SCM_SMOB_DATA(scm_self);
}

static struct tf_description_t *get_tf_description(SCM scm_self)
{
  scm_assert_smob_type(tf_description_tag, scm_self);
  return get_tf_description_no_check(scm_self);
}

size_t free_description(SCM scm_self)
{
  struct tf_description_t *self = get_tf_description_no_check(scm_self);
  scm_gc_free(self, sizeof(struct tf_description_t), "description");
  return 0;
}

static struct tf_output_t *get_tf_output_no_check(SCM scm_self)
{
  return (struct tf_output_t *)SCM_SMOB_DATA(scm_self);
}

static struct tf_output_t *get_tf_output(SCM scm_self)
{
  scm_assert_smob_type(tf_output_tag, scm_self);
  return get_tf_output_no_check(scm_self);
}

size_t free_output(SCM scm_self)
{
  struct tf_output_t *self = get_tf_output_no_check(scm_self);
  scm_gc_free(self, sizeof(struct tf_output_t), "output");
  return 0;
}

static struct tf_session_t *get_tf_session_no_check(SCM scm_self)
{
  return (struct tf_session_t *)SCM_SMOB_DATA(scm_self);
}

static struct tf_session_t *get_tf_session(SCM scm_self)
{
  scm_assert_smob_type(tf_session_tag, scm_self);
  return get_tf_session_no_check(scm_self);
}

size_t free_session(SCM scm_self)
{
  struct tf_session_t *self = get_tf_session_no_check(scm_self);
  TF_DeleteSession(self->session, status);
  scm_gc_free(self, sizeof(struct tf_session_t), "session");
  return 0;
}

SCM make_tensor(SCM scm_type, SCM scm_shape, SCM scm_size, SCM scm_source)
{
  SCM retval;
  struct tf_tensor_t *self = (struct tf_tensor_t *)scm_gc_calloc(sizeof(struct tf_tensor_t), "make-tensor");
  SCM_NEWSMOB(retval, tf_tensor_tag, self);
  int type = scm_to_int(scm_type);
  int num_dims = scm_to_int(scm_length(scm_shape));
  int64_t *dims = scm_gc_malloc_pointerless(sizeof(int64_t) * num_dims, "make-tensor");
  int count = 1;
  for (int i=0; i<num_dims; i++) {
    dims[i] = scm_to_int(scm_car(scm_shape));
    count = count * dims[i];
    scm_shape = scm_cdr(scm_shape);
  };
  if (type == TF_STRING) {
    SCM* pointer = scm_to_pointer(scm_source);
    size_t encoded_size = 0;
    for (int i=0; i<count; i++) {
      encoded_size += TF_StringEncodedSize(strlen(scm_to_locale_string(*pointer))) + 8;
      pointer++;
    };
    self->tensor = TF_AllocateTensor(type, dims, num_dims, encoded_size);
    int64_t *offsets = TF_TensorData(self->tensor);
    int offset = 0;
    void *result = offsets + count;
    pointer = scm_to_pointer(scm_source);
    encoded_size = encoded_size - count * sizeof(int64_t);
    for (int i=0; i<count; i++) {
      const char *str = scm_to_locale_string(*pointer++);
      int len = TF_StringEncodedSize(strlen(str));
      *offsets++ = offset;
      TF_StringEncode(str, strlen(str), result, encoded_size, status);
      if (TF_GetCode(status) != TF_OK)
        scm_misc_error("make-tensor", TF_Message(status), SCM_EOL);
      offset += len;
      encoded_size -= len;
      result += len;
    };
  } else {
    self->tensor = TF_AllocateTensor(type, dims, num_dims, scm_to_int(scm_size));
    memcpy(TF_TensorData(self->tensor), scm_to_pointer(scm_source), scm_to_int(scm_size));
  };
  return retval;
}

SCM tf_from_tensor(SCM scm_self)
{
  struct tf_tensor_t *self = get_tf_tensor(scm_self);
  int type = TF_TensorType(self->tensor);
  int num_dims = TF_NumDims(self->tensor);
  SCM scm_shape = SCM_EOL;
  for (int i=num_dims - 1; i>=0; i--)
    scm_shape = scm_cons(scm_from_int(TF_Dim(self->tensor, i)), scm_shape);
  size_t size = TF_TensorByteSize(self->tensor);
  void *data;
  if (type == TF_STRING) {
    void *pointer = TF_TensorData(self->tensor);
    const char *str;
    size_t str_len;
    TF_StringDecode(pointer + 8, size - 8, &str, &str_len, status);
    data = scm_gc_malloc(8, "from-tensor");
    *(SCM *)data = scm_from_locale_string(str);
  } else {
    data = scm_gc_malloc_pointerless(size, "from-tensor");
    memcpy(data, TF_TensorData(self->tensor), size);
  };
  return scm_list_3(scm_from_int(type),
                    scm_shape,
                    scm_from_pointer(data, NULL));
}

SCM make_graph(void)
{
  SCM retval;
  struct tf_graph_t *self = (struct tf_graph_t *)scm_gc_calloc(sizeof(struct tf_graph_t), "make-graph");
  SCM_NEWSMOB(retval, tf_graph_tag, self);
  self->graph = TF_NewGraph();
  return retval;
}

SCM tf_graph_export_(SCM scm_graph, SCM scm_file_name)
{
  struct tf_graph_t *graph = get_tf_graph(scm_graph);
  TF_Buffer *buffer = TF_NewBuffer();
  TF_GraphToGraphDef(graph->graph, buffer, status);
  if (TF_GetCode(status) != TF_OK) {
    TF_DeleteBuffer(buffer);
    scm_misc_error("tf-graph-export_", TF_Message(status), SCM_EOL);
  };
  FILE *file = fopen(scm_to_locale_string(scm_file_name), "w");
  if (!file)
    scm_misc_error("tf-graph-export_", strerror(errno), SCM_EOL);
  fwrite(buffer->data, buffer->length, 1, file);
  fclose(file);
  TF_DeleteBuffer(buffer);
  return SCM_UNDEFINED;
}

SCM tf_graph_import_(SCM scm_graph, SCM scm_file_name)
{
  struct tf_graph_t *graph = get_tf_graph(scm_graph);
  FILE *file = fopen(scm_to_locale_string(scm_file_name), "r");
  if (!file)
    scm_misc_error("tf-graph-import_", strerror(errno), SCM_EOL);
  int fd = fileno(file);
  struct stat st;
  fstat(fd, &st);
  size_t size = st.st_size;
  TF_Buffer *buffer = TF_NewBuffer();
  void *data = scm_gc_malloc(size, "tf-graph-import_");
  fread(data, size, 1, file);
  buffer->data = data;
  buffer->length = size;
  fclose(file);
  TF_ImportGraphDefOptions* opts = TF_NewImportGraphDefOptions();
  TF_GraphImportGraphDef(graph->graph, buffer, opts, status);
  TF_DeleteImportGraphDefOptions(opts);
  TF_DeleteBuffer(buffer);
  if (TF_GetCode(status) != TF_OK)
    scm_misc_error("tf-graph-import_", TF_Message(status), SCM_EOL);
  return SCM_UNDEFINED;
}

SCM make_description(SCM scm_graph, SCM scm_op, SCM scm_name)
{
  SCM retval;
  struct tf_graph_t *graph = get_tf_graph(scm_graph);
  struct tf_description_t *self = (struct tf_description_t *)scm_gc_calloc(sizeof(struct tf_description_t), "make-description");
  SCM_NEWSMOB(retval, tf_description_tag, self);
  self->description = TF_NewOperation(graph->graph, scm_to_locale_string(scm_op), scm_to_locale_string(scm_name));
  return retval;
}

SCM tf_finish_operation(SCM scm_description, SCM scm_n_outputs)
{
  SCM retval = SCM_EOL;
  struct tf_description_t *self = get_tf_description(scm_description);
  int n_outputs = scm_to_int(scm_n_outputs);
  TF_Operation *operation = TF_FinishOperation(self->description, status);
  if (TF_GetCode(status) != TF_OK)
    scm_misc_error("tf-finish-operation", TF_Message(status), SCM_EOL);
  for (int i=n_outputs-1; i>=0; i--) {
    SCM element;
    struct tf_output_t *output = (struct tf_output_t *)scm_gc_calloc(sizeof(struct tf_output_t), "tf-finish-operation");
    SCM_NEWSMOB(element, tf_output_tag, output);
    output->output.oper = operation;
    output->output.index = i;
    retval = scm_cons(element, retval);
  };
  if (n_outputs == 1)
    retval = scm_car(retval);
  return retval;
}

SCM tf_add_input(SCM scm_description, SCM scm_input)
{
  struct tf_description_t *self = get_tf_description(scm_description);
  struct tf_output_t *input = get_tf_output(scm_input);
  TF_AddInput(self->description, input->output);
  return SCM_UNDEFINED;
}

SCM tf_add_input_list(SCM scm_description, SCM scm_inputs)
{
  struct tf_description_t *self = get_tf_description(scm_description);
  int num_inputs = scm_ilength(scm_inputs);
  TF_Output *inputs = (TF_Output *)scm_gc_calloc(sizeof(struct TF_Output) * num_inputs, "tf-add-input-list");
  for (int i=0; i<num_inputs; i++) {
    inputs[i] = get_tf_output(scm_car(scm_inputs))->output;
    scm_inputs = scm_cdr(scm_inputs);
  };
  TF_AddInputList(self->description, inputs, num_inputs);
  return SCM_UNDEFINED;
}

SCM tf_set_attr_string(SCM scm_description, SCM scm_name, SCM scm_value)
{
  struct tf_description_t *self = get_tf_description(scm_description);
  TF_SetAttrString(self->description,
                   scm_to_locale_string(scm_name),
                   scm_to_locale_string(scm_value),
                   scm_c_string_length(scm_value));
  return SCM_UNDEFINED;
}

SCM tf_set_attr_int(SCM scm_description, SCM scm_name, SCM scm_value)
{
  struct tf_description_t *self = get_tf_description(scm_description);
  TF_SetAttrInt(self->description, scm_to_locale_string(scm_name), scm_to_int(scm_value));
  return SCM_UNDEFINED;
}

SCM tf_set_attr_int_list(SCM scm_description, SCM scm_name, SCM scm_values)
{
  struct tf_description_t *self = get_tf_description(scm_description);
  int num_values = scm_ilength(scm_values);
  int64_t *values = scm_gc_malloc(sizeof(int64_t) * num_values, "tf-set-attr-int-list");
  for (int i=0; i<num_values; i++) {
    values[i] = scm_to_int(scm_car(scm_values));
    scm_values = scm_cdr(scm_values);
  };
  TF_SetAttrIntList(self->description, scm_to_locale_string(scm_name), values, num_values);
  return SCM_UNDEFINED;
}

SCM tf_set_attr_bool(SCM scm_description, SCM scm_name, SCM scm_value)
{
  struct tf_description_t *self = get_tf_description(scm_description);
  TF_SetAttrBool(self->description, scm_to_locale_string(scm_name), scm_is_true(scm_value));
  return SCM_UNDEFINED;
}

SCM tf_set_attr_float(SCM scm_description, SCM scm_name, SCM scm_value)
{
  struct tf_description_t *self = get_tf_description(scm_description);
  TF_SetAttrFloat(self->description, scm_to_locale_string(scm_name), (float)scm_to_double(scm_value));
  return SCM_UNDEFINED;
}

SCM tf_set_attr_float_list(SCM scm_description, SCM scm_name, SCM scm_values)
{
  struct tf_description_t *self = get_tf_description(scm_description);
  int num_values = scm_ilength(scm_values);
  float *values = scm_gc_malloc(sizeof(float) * num_values, "tf-set-attr-float-list");
  for (int i=0; i<num_values; i++) {
    values[i] = (float)scm_to_double(scm_car(scm_values));
    scm_values = scm_cdr(scm_values);
  };
  TF_SetAttrFloatList(self->description, scm_to_locale_string(scm_name), values, num_values);
  return SCM_UNDEFINED;
}

SCM tf_set_attr_type(SCM scm_description, SCM scm_name, SCM scm_type)
{
  struct tf_description_t *self = get_tf_description(scm_description);
  TF_SetAttrType(self->description, scm_to_locale_string(scm_name), scm_to_int(scm_type));
  return SCM_UNDEFINED;
}

SCM tf_set_attr_shape(SCM scm_description, SCM scm_name, SCM scm_shape)
{
  struct tf_description_t *self = get_tf_description(scm_description);
  int num_dims = scm_ilength(scm_shape);
  int64_t *dims = scm_gc_malloc(sizeof(int64_t) * num_dims, "tf-set-attr-shape");
  for (int i=0; i<num_dims; i++) {
    dims[i] = scm_to_int(scm_car(scm_shape));
    scm_shape = scm_cdr(scm_shape);
  };
  TF_SetAttrShape(self->description, scm_to_locale_string(scm_name), dims, num_dims);
  return SCM_UNDEFINED;
}

SCM tf_set_attr_tensor(SCM scm_description, SCM scm_name, SCM scm_value)
{
  struct tf_description_t *self = get_tf_description(scm_description);
  struct tf_tensor_t *value = get_tf_tensor(scm_value);
  TF_SetAttrTensor(self->description, scm_to_locale_string(scm_name), value->tensor, status);
  if (TF_GetCode(status) != TF_OK)
    scm_misc_error("tf-set-attr-tensor", TF_Message(status), SCM_EOL);
  return SCM_UNDEFINED;
}

SCM make_tf_session(SCM scm_graph)
{
  SCM retval;
  struct tf_session_t *self = (struct tf_session_t *)scm_gc_calloc(sizeof(struct tf_session_t), "make-tf-session");
  SCM_NEWSMOB(retval, tf_session_tag, self);
  self->graph = get_tf_graph(scm_graph);
  TF_SessionOptions *options = TF_NewSessionOptions();
  self->session = TF_NewSession(self->graph->graph, options, status);
  TF_DeleteSessionOptions(options);
  if (TF_GetCode(status) != TF_OK)
    scm_misc_error("make-tf-session", TF_Message(status), SCM_EOL);
  return retval;
}

SCM tf_run(SCM scm_session, SCM scm_input, SCM scm_output)
{
  SCM retval;
  if (scm_is_true(scm_list_p(scm_output))) {
    struct tf_session_t *session = get_tf_session(scm_session);
    int ninputs = scm_ilength(scm_input);
    TF_Output *inputs = scm_gc_malloc(sizeof(TF_Output) * ninputs, "tf-run");
    TF_Tensor **input_values = scm_gc_malloc(sizeof(TF_Tensor *) * ninputs, "tf-run");
    for (int i=0; i<ninputs; i++) {
      memcpy(&inputs[i], &get_tf_output(scm_caar(scm_input))->output, sizeof(TF_Output));
      input_values[i] = get_tf_tensor(scm_cdar(scm_input))->tensor;
      scm_input = scm_cdr(scm_input);
    };
    int noutputs = scm_ilength(scm_output);
    TF_Output *output = scm_gc_malloc(sizeof(TF_Output) * noutputs, "tf-run");
    TF_Tensor **output_values = scm_gc_malloc(sizeof(TF_Tensor *) * noutputs, "tf-run");
    for (int i=0; i<noutputs; i++) {
      output[i] = get_tf_output(scm_car(scm_output))->output;
      scm_output = scm_cdr(scm_output);
    };
    TF_SessionRun(session->session, NULL, inputs, input_values, ninputs, output, output_values, noutputs, NULL, 0, NULL, status);
    if (TF_GetCode(status) != TF_OK)
      scm_misc_error("tf-run", TF_Message(status), SCM_EOL);
    retval = SCM_EOL;
    for (int i=noutputs-1; i>=0; i--) {
      SCM element;
      struct tf_tensor_t *result = (struct tf_tensor_t *)scm_gc_calloc(sizeof(struct tf_tensor_t), "make-tensor");
      SCM_NEWSMOB(element, tf_tensor_tag, result);
      result->tensor = output_values[i];
      retval = scm_cons(element, retval);
    };
  } else
    retval = scm_car(tf_run(scm_session, scm_input, scm_list_1(scm_output)));
  return retval;
}

SCM tf_add_gradient_(SCM scm_graph, SCM scm_expression, SCM scm_variables)
{
  SCM retval;
  if (scm_is_true(scm_list_p(scm_variables))) {
    struct tf_graph_t *graph = get_tf_graph(scm_graph);
    struct tf_output_t *expression = get_tf_output(scm_expression);
    int nvariables = scm_ilength(scm_variables);
    TF_Output *variables = scm_gc_calloc(sizeof(TF_Output) * nvariables, "tf-add-gradient_");
    for (int i=0; i<nvariables; i++) {
      variables[i] = get_tf_output(scm_car(scm_variables))->output;
      scm_variables = scm_cdr(scm_variables);
    };
    TF_Output *output = scm_gc_calloc(sizeof(TF_Output) * nvariables, "tf-add-gradient_");
    TF_AddGradients(graph->graph, &expression->output, 1, variables, nvariables, NULL, status, output);
    if (TF_GetCode(status) != TF_OK)
      scm_misc_error("tf-add-gradient_", TF_Message(status), SCM_EOL);
    retval = SCM_EOL;
    for (int i=nvariables-1; i>=0; i--) {
      SCM element;
      struct tf_output_t *result = scm_gc_calloc(sizeof(struct tf_output_t), "tf-add-gradient_");
      SCM_NEWSMOB(element, tf_output_tag, result);
      result->output = output[i];
      retval = scm_cons(element, retval);
    };
  } else
    retval = scm_car(tf_add_gradient_(scm_graph, scm_expression, scm_list_1(scm_variables)));
  return retval;
}

SCM tf_outputq(SCM scm_value)
{
  return scm_from_bool(SCM_SMOB_PREDICATE(tf_output_tag, scm_value));
}

SCM tf_graph_operation_by_name_(SCM scm_graph, SCM scm_name)
{
  struct tf_graph_t *graph = get_tf_graph(scm_graph);
  TF_Operation *operation = TF_GraphOperationByName(graph->graph, scm_to_locale_string(scm_name));
  if (!operation)
    scm_misc_error("tf-graph-operation-by-name_", "Operation '~a' not found", scm_list_1(scm_name));
  SCM retval = SCM_EOL;
  int noutputs = TF_OperationNumOutputs(operation);
  for (int i=noutputs-1; i>=0; i--) {
    SCM element;
    struct tf_output_t *output = (struct tf_output_t *)scm_gc_calloc(sizeof(struct tf_output_t), "tf-graph-operation-by-name_");
    SCM_NEWSMOB(element, tf_output_tag, output);
    output->output.oper = operation;
    output->output.index = i;
    retval = scm_cons(element, retval);
  };
  if (noutputs == 1)
    retval = scm_car(retval);
  return retval;
}

SCM tf_operation_names_(SCM scm_graph)
{
  SCM retval = SCM_EOL;
  struct tf_graph_t *graph = get_tf_graph(scm_graph);
  size_t pos = 0;
  TF_Operation *oper;
  while(oper = TF_GraphNextOperation(graph->graph, &pos))
    retval = scm_cons(scm_from_locale_string(TF_OperationName(oper)), retval);
  return retval;
}

void init_tensorflow(void)
{
  tf_tensor_tag = scm_make_smob_type("tensor", sizeof(struct tf_tensor_t));
  scm_set_smob_free(tf_tensor_tag, free_tensor);

  tf_graph_tag = scm_make_smob_type("graph", sizeof(struct tf_graph_t));
  scm_set_smob_free(tf_graph_tag, free_graph);

  tf_description_tag = scm_make_smob_type("description", sizeof(struct tf_description_t));
  scm_set_smob_free(tf_description_tag, free_description);

  tf_output_tag = scm_make_smob_type("output", sizeof(struct tf_output_t));
  scm_set_smob_free(tf_output_tag, free_output);

  tf_session_tag = scm_make_smob_type("session", sizeof(struct tf_session_t));
  scm_set_smob_free(tf_session_tag, free_session);

  status = TF_NewStatus();

  scm_c_define("TF_FLOAT"     , scm_from_int(TF_FLOAT     ));
  scm_c_define("TF_DOUBLE"    , scm_from_int(TF_DOUBLE    ));
  scm_c_define("TF_UINT8"     , scm_from_int(TF_UINT8     ));
  scm_c_define("TF_INT8"      , scm_from_int(TF_INT8      ));
  scm_c_define("TF_UINT16"    , scm_from_int(TF_UINT16    ));
  scm_c_define("TF_INT16"     , scm_from_int(TF_INT16     ));
  scm_c_define("TF_UINT32"    , scm_from_int(TF_UINT32    ));
  scm_c_define("TF_INT32"     , scm_from_int(TF_INT32     ));
  scm_c_define("TF_UINT64"    , scm_from_int(TF_UINT64    ));
  scm_c_define("TF_COMPLEX64" , scm_from_int(TF_COMPLEX64 ));
  scm_c_define("TF_INT64"     , scm_from_int(TF_INT64     ));
  scm_c_define("TF_BOOL"      , scm_from_int(TF_BOOL      ));
  scm_c_define("TF_COMPLEX128", scm_from_int(TF_COMPLEX128));
  scm_c_define("TF_STRING"    , scm_from_int(TF_STRING    ));
  scm_c_define_gsubr("make-tensor"                 , 4, 0, 0, SCM_FUNC(make_tensor                ));
  scm_c_define_gsubr("tf-from-tensor"              , 1, 0, 0, SCM_FUNC(tf_from_tensor             ));
  scm_c_define_gsubr("make-graph"                  , 0, 0, 0, SCM_FUNC(make_graph                 ));
  scm_c_define_gsubr("tf-graph-export_"            , 2, 0, 0, SCM_FUNC(tf_graph_export_           ));
  scm_c_define_gsubr("tf-graph-import_"            , 2, 0, 0, SCM_FUNC(tf_graph_import_           ));
  scm_c_define_gsubr("make-description"            , 3, 0, 0, SCM_FUNC(make_description           ));
  scm_c_define_gsubr("tf-finish-operation"         , 2, 0, 0, SCM_FUNC(tf_finish_operation        ));
  scm_c_define_gsubr("tf-add-input"                , 2, 0, 0, SCM_FUNC(tf_add_input               ));
  scm_c_define_gsubr("tf-add-input-list"           , 2, 0, 0, SCM_FUNC(tf_add_input_list          ));
  scm_c_define_gsubr("tf-set-attr-string"          , 3, 0, 0, SCM_FUNC(tf_set_attr_string         ));
  scm_c_define_gsubr("tf-set-attr-int"             , 3, 0, 0, SCM_FUNC(tf_set_attr_int            ));
  scm_c_define_gsubr("tf-set-attr-int-list"        , 3, 0, 0, SCM_FUNC(tf_set_attr_int_list       ));
  scm_c_define_gsubr("tf-set-attr-bool"            , 3, 0, 0, SCM_FUNC(tf_set_attr_bool           ));
  scm_c_define_gsubr("tf-set-attr-float"           , 3, 0, 0, SCM_FUNC(tf_set_attr_float          ));
  scm_c_define_gsubr("tf-set-attr-float-list"      , 3, 0, 0, SCM_FUNC(tf_set_attr_float_list     ));
  scm_c_define_gsubr("tf-set-attr-type"            , 3, 0, 0, SCM_FUNC(tf_set_attr_type           ));
  scm_c_define_gsubr("tf-set-attr-shape"           , 3, 0, 0, SCM_FUNC(tf_set_attr_shape          ));
  scm_c_define_gsubr("tf-set-attr-tensor"          , 3, 0, 0, SCM_FUNC(tf_set_attr_tensor         ));
  scm_c_define_gsubr("make-tf-session"             , 1, 0, 0, SCM_FUNC(make_tf_session            ));
  scm_c_define_gsubr("tf-run"                      , 3, 0, 0, SCM_FUNC(tf_run                     ));
  scm_c_define_gsubr("tf-add-gradient_"            , 3, 0, 0, SCM_FUNC(tf_add_gradient_           ));
  scm_c_define_gsubr("tf-output?"                  , 1, 0, 0, SCM_FUNC(tf_outputq                 ));
  scm_c_define_gsubr("tf-graph-operation-by-name_" , 2, 0, 0, SCM_FUNC(tf_graph_operation_by_name_));
  scm_c_define_gsubr("tf-operation-names_"         , 1, 0, 0, SCM_FUNC(tf_operation_names_        ));
}
