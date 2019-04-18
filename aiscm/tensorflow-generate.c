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
      *p++ = 'a' - 'A' + *q++;
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

  HDF *hdf;
  hdf_init(&hdf);

  for (int i=0; i<op_list->n_op; i++) {
    struct _Tensorflow__OpDef *op = op_list->op[i];
    char variable[256];
    char value[256];
    snprintf(variable, 256, "Op.%s.name", op->name);
    snprintf(value, 256, "tf-%s", kebab_case(op->name));
    hdf_set_value(hdf, variable, value);
    for (int j=0; j<op->n_input_arg; j++) {
      Tensorflow__OpDef__ArgDef *arg = op->input_arg[j];
      snprintf(variable, 256, "Op.%s.input_arg.%s", op->name, arg->name);
      const char *multiple = arg->number_attr && *arg->number_attr ? "list" : "single";
      hdf_set_value(hdf, variable, multiple);
    };
    for (int j=0; j<op->n_attr; j++) {
      Tensorflow__OpDef__AttrDef *attr = op->attr[j];
      snprintf(variable, 256, "Op.%s.attr.%s", op->name, attr->name);
      snprintf(value, 256, "%s", attr->type);
      hdf_set_value(hdf, variable, value);
    };
    snprintf(variable, 256, "Op.%s.n_output", op->name);
    snprintf(value, 256, "%d", op->n_output_arg);
    hdf_set_value(hdf, variable, value);
  };

  CSPARSE *parse;
  cs_init(&parse, hdf);
  cs_parse_file(parse, "tensorflow.scm.in");
  cs_render(parse, stdout, output);

  cs_destroy(&parse);
  hdf_destroy(&hdf);

  tensorflow__op_list__free_unpacked(op_list, NULL);
  TF_DeleteBuffer(buffer);
}
