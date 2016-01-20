#include <libguile.h>
#include <magick/MagickCore.h>

SCM magick_read_image(SCM scm_file_name)
{
  SCM retval;
  const char *file_name = scm_to_locale_string(scm_file_name);
  ExceptionInfo *exception_info = AcquireExceptionInfo();
  ImageInfo *image_info = AcquireImageInfo();
  CopyMagickString(image_info->filename, file_name, MaxTextExtent);
  Image *images = ReadImage(image_info, exception_info);
  Image *image = RemoveFirstImageFromList(&images);
  int width = image->columns;
  int height = image->rows;
  int size = width * height * 4;
  void *buf = scm_gc_malloc_pointerless(size, "aiscm magick frame");
  ExportImagePixels(image, 0, 0, image->columns, image->rows, "BGRA", CharPixel, buf, exception_info);
  retval = scm_list_3(scm_list_2(scm_from_int(width), scm_from_int(height)),
                      scm_from_pointer(buf, NULL),
                      scm_from_int(size));
  DestroyImage(image);
  DestroyImageInfo(image_info);
  DestroyExceptionInfo(exception_info);
  return retval;
}

void init_magick(void)
{
  MagickCoreGenesis("libguile-magick", MagickTrue);
  scm_c_define_gsubr("magick-read-image", 1, 0, 0, magick_read_image);
}
