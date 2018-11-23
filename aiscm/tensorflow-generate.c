#include <gc.h>
#include <stdio.h>
#include <string.h>
#include <ClearSilver.h>
#include <tensorflow/c/c_api.h>
#include "tensorflow/core/framework/op_def.pb-c.h"


NEOERR *output(void *ctx, char *text)
{
  fprintf((FILE *)ctx, "%s", text);
  return NULL;
}

char *kebab_case(const char *camel_case)
{
  int n = strlen(camel_case);
  char *result = (char *)GC_MALLOC_ATOMIC(2 * n + 1);
  char *p = result;
  const char *q = camel_case;
  char previous_is_small_caps = 0;
  while (*q) {
    if (*q >= 'A' && *q <= 'Z') {
      if (previous_is_small_caps)
        *p++ = '-';
      *p++ = *q++ + 'a' - 'A';
      previous_is_small_caps = 0;
    } else {
      previous_is_small_caps = *q >= 'a' && *q <= 'z';
      *p++ = *q++;
    };
  };
  *p = 0;
  return result;
}

int main(void)
{
  GC_INIT();
  TF_Buffer *buffer = TF_GetAllOpList();
  Tensorflow__OpList *op_list = tensorflow__op_list__unpack(NULL, buffer->length, buffer->data);

  for (int i=0; i<op_list->n_op; i++) {
    struct _Tensorflow__OpDef *op = op_list->op[i];
    fprintf(stderr, "Op.%s.name = %s\n", op->name, kebab_case(op->name));
    for (int j=0; j<op->n_input_arg; j++) {
      Tensorflow__OpDef__ArgDef *arg = op->input_arg[j];
      const char *type = arg->type != TENSORFLOW__DATA_TYPE__DT_INVALID ? "scalar" : arg->type_attr;
      fprintf(stderr, "Op.%s.input_arg.%s = %s\n", op->name, arg->name, type);
    };
    for (int j=0; j<op->n_attr; j++) {
      Tensorflow__OpDef__AttrDef *attr = op->attr[j];
      fprintf(stderr, "Op.%s.attr.%s = %s\n", op->name, attr->name, attr->type);
    };
  };

  HDF *hdf;
  hdf_init(&hdf);

  hdf_set_value(hdf, "Op.Identity.name", "identity_");
  hdf_set_value(hdf, "Op.Identity.input_arg.input", "T");
  hdf_set_value(hdf, "Op.Identity.attr.T", "type");

  hdf_set_value(hdf, "Op.Assign.name", "assign");
  hdf_set_value(hdf, "Op.Assign.input_arg.x", "T");
  hdf_set_value(hdf, "Op.Assign.input_arg.y", "T");
  hdf_set_value(hdf, "Op.Assign.attr.T", "type");

  hdf_set_value(hdf, "Op.Const.name", "const_");
  hdf_set_value(hdf, "Op.Const.attr.value", "tensor");
  hdf_set_value(hdf, "Op.Const.attr.dtype", "type");

  hdf_set_value(hdf, "Op.Placeholder.name", "placeholder");
  hdf_set_value(hdf, "Op.Placeholder.attr.dtype", "type");
  hdf_set_value(hdf, "Op.Placeholder.attr.shape", "shape");

  hdf_set_value(hdf, "Op.Variable.name", "variable");
  hdf_set_value(hdf, "Op.Variable.attr.dtype", "type");
  hdf_set_value(hdf, "Op.Variable.attr.shape", "shape");

  CSPARSE *parse;
  cs_init(&parse, hdf);
  cs_parse_file(parse, "tensorflow.scm.in");
  cs_render(parse, stdout, output);

  cs_destroy(&parse);
  hdf_destroy(&hdf);

  tensorflow__op_list__free_unpacked(op_list, NULL);
  TF_DeleteBuffer(buffer);
}
