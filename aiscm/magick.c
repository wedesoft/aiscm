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
#include <stdio.h>
#include <libguile.h>
#include <MagickCore/MagickCore.h>
#include "util-helpers.h"


SCM magick_read_image(SCM scm_file_name)
{
  SCM retval = SCM_UNDEFINED;
  char *file_name = scm_to_locale_string(scm_file_name);
  ExceptionInfo *exception_info = AcquireExceptionInfo();
  ImageInfo *image_info = CloneImageInfo((ImageInfo *)NULL);
  CopyMagickString(image_info->filename, file_name, MaxTextExtent);
  free(file_name);
  Image *images = ReadImage(image_info, exception_info);
  if (exception_info->severity < ErrorException) {
    CatchException(exception_info);
    Image *image = RemoveFirstImageFromList(&images);
    char grey = image->colorspace == GRAYColorspace;
    const char *format = grey ? "I" : "RGB";
    int width = image->columns;
    int height = image->rows;
    int bytes_per_pixel = grey ? 1 : 3;
    int size = width * height * bytes_per_pixel;
    void *base = scm_gc_malloc_pointerless(size + 15, "aiscm magick frame");
#if defined __x86_64__
    void *mem = (void *)(((int64_t)base + 15) & -16);
#else
    void *mem = (void *)(((int32_t)base + 15) & -16);
#endif
    ExportImagePixels(image, 0, 0, width, height, format, CharPixel, mem, exception_info);
    if (exception_info->severity < ErrorException) {
      CatchException(exception_info);
      retval = scm_list_5(scm_from_locale_symbol(format),
                          scm_list_2(scm_from_int(height), scm_from_int(width)),
                          scm_from_pointer(base, NULL),
                          scm_from_pointer(mem, NULL),
                          scm_from_int(size));
    };
    DestroyImage(image);
    DestroyImageList(images);
  };
  SCM scm_reason = exception_info->severity < ErrorException ?
    SCM_UNDEFINED : scm_from_locale_string(exception_info->reason);
  DestroyImageInfo(image_info);
  DestroyExceptionInfo(exception_info);
  if (scm_reason != SCM_UNDEFINED)
    scm_misc_error("magick-read-image", "~a", scm_list_1(scm_reason));
  return retval;
}

SCM magick_write_image(SCM scm_format, SCM scm_shape, SCM scm_mem, SCM scm_file_name)
{
  int width = scm_to_int(scm_cadr(scm_shape));
  int height = scm_to_int(scm_car(scm_shape));
  void *mem = scm_to_pointer(scm_mem);
  ExceptionInfo *exception_info = AcquireExceptionInfo();
  ImageInfo *image_info = AcquireImageInfo();
  GetImageInfo(image_info);
  char *format = scm_to_locale_string(scm_symbol_to_string(scm_format));
  Image *image = ConstituteImage(width, height, format, CharPixel, mem, exception_info);
  free(format);
  if (exception_info->severity < ErrorException) {
    CatchException(exception_info);
    Image *images = NewImageList();
    AppendImageToList(&images, image);
    char *file_name = scm_to_locale_string(scm_file_name);
    WriteImages(image_info, images, file_name, exception_info);
    free(file_name);
    if (exception_info->severity < ErrorException)
      CatchException(exception_info);
    DestroyImageList(images);
  };
  SCM scm_reason = exception_info->severity < ErrorException ?
    SCM_UNDEFINED : scm_from_locale_string(exception_info->reason);
  DestroyImageInfo(image_info);
  DestroyExceptionInfo(exception_info);
  if (scm_reason != SCM_UNDEFINED)
    scm_misc_error("magick-write-image", "~a", scm_list_1(scm_reason));
  return SCM_UNSPECIFIED;
}

void init_magick(void)
{
  MagickCoreGenesis("libguile-magick", MagickTrue);
  scm_c_define_gsubr("magick-read-image" , 1, 0, 0, SCM_FUNC(magick_read_image ));
  scm_c_define_gsubr("magick-write-image", 4, 0, 0, SCM_FUNC(magick_write_image));
}
