#include <libguile.h>
#include <magick/MagickCore.h>

#include <stdio.h>

SCM magick_read_image(SCM scm_file_name)
{
  SCM retval = SCM_UNDEFINED;
  const char *file_name = scm_to_locale_string(scm_file_name);
  ExceptionInfo *exception_info = AcquireExceptionInfo();
  ImageInfo *image_info = CloneImageInfo((ImageInfo *)NULL);
  CopyMagickString(image_info->filename, file_name, MaxTextExtent);
  Image *images = ReadImage(image_info, exception_info);
  if (exception_info->severity < ErrorException) {
    CatchException(exception_info);
    Image *image = RemoveFirstImageFromList(&images);
    char grey = image->colorspace == GRAYColorspace;
    const char *map = grey ? "I" : "BGRA";
    const char *format = grey ? "GRAY" : "BGRA";
    int width = image->columns;
    int height = image->rows;
    int bytes_per_pixel = grey ? 1 : 4;
    int size = width * height * bytes_per_pixel;
    void *buf = scm_gc_malloc_pointerless(size, "aiscm magick frame");
    ExportImagePixels(image, 0, 0, width, height, map, CharPixel, buf, exception_info);
    if (exception_info->severity < ErrorException) {
      CatchException(exception_info);
      retval = scm_list_4(scm_from_locale_symbol(format),
                          scm_list_2(scm_from_int(width), scm_from_int(height)),
                          scm_from_pointer(buf, NULL),
                          scm_from_int(size));
    };
    DestroyImage(image);
  };
  SCM scm_reason = exception_info->severity < ErrorException ?
    SCM_UNDEFINED : scm_from_locale_string(exception_info->reason);
  DestroyImageInfo(image_info);
  DestroyExceptionInfo(exception_info);
  if (scm_reason != SCM_UNDEFINED)
    scm_misc_error("magick_read_image", "~a", scm_list_1(scm_reason));
  return retval;
}

SCM magick_write_image(SCM scm_format, SCM scm_shape, SCM scm_buf, SCM scm_file_name)
{
  int width = scm_to_int(scm_car(scm_shape));
  int height = scm_to_int(scm_cadr(scm_shape));
  void *buf = scm_to_pointer(scm_buf);
  ExceptionInfo *exception_info = AcquireExceptionInfo();
  ImageInfo *image_info = AcquireImageInfo();
  GetImageInfo(image_info);
  const char *format = scm_format == scm_from_locale_symbol("BGRA") ? "BGRA" : "I";
  Image *image = ConstituteImage(width, height, format, CharPixel, buf, exception_info);
  if (exception_info->severity < ErrorException) {
    CatchException(exception_info);
    Image *images = NewImageList();
    AppendImageToList(&images, image);
    const char *file_name = scm_to_locale_string(scm_file_name);
    WriteImages(image_info, images, file_name, exception_info);
    if (exception_info->severity < ErrorException)
      CatchException(exception_info);
    DestroyImageList(images);
  };
  SCM scm_reason = exception_info->severity < ErrorException ?
    SCM_UNDEFINED : scm_from_locale_string(exception_info->reason);
  DestroyImageInfo(image_info);
  DestroyExceptionInfo(exception_info);
  if (scm_reason != SCM_UNDEFINED)
    scm_misc_error("magick_write_image", "~a", scm_list_1(scm_reason));
  return SCM_UNDEFINED;
}

void init_magick(void)
{
  MagickCoreGenesis("libguile-magick", MagickTrue);
  scm_c_define_gsubr("magick-read-image", 1, 0, 0, magick_read_image);
  scm_c_define_gsubr("magick-write-image", 4, 0, 0, magick_write_image);
}
