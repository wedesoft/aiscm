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

  CSPARSE *parse;
  cs_init(&parse, hdf);
  cs_parse_file(parse, "tensorflow.scm.in");
  cs_render(parse, stdout, output);

  cs_destroy(&parse);
  hdf_destroy(&hdf);
}
