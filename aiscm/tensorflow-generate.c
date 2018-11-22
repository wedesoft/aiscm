#include <stdio.h>
#include <ClearSilver.h>

NEOERR *output(void *ctx, char *text)
{
  fprintf((FILE *)ctx, "%s", text);
  return NULL;
}

int main(void)
{
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

  CSPARSE *parse;
  cs_init(&parse, hdf);
  cs_parse_file(parse, "tensorflow.scm.in");
  cs_render(parse, stdout, output);

  cs_destroy(&parse);
  hdf_destroy(&hdf);
}
