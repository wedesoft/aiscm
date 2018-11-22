#include <stdio.h>
#include <ClearSilver.h>
#include <tensorflow/c/c_api.h>
#include "tensorflow/core/framework/op_def.pb-c.h"


NEOERR *output(void *ctx, char *text)
{
  fprintf((FILE *)ctx, "%s", text);
  return NULL;
}

int main(void)
{
  TF_Buffer *buffer = TF_GetAllOpList();
  Tensorflow__OpList *op_list = tensorflow__op_list__unpack(NULL, buffer->length, buffer->data);

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
