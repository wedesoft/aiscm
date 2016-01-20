#include <magick/MagickCore.h>
#include <stdio.h>

int main(void)
{
  const char *file_name = "ramp.png";
  ExceptionInfo *exception_info = AcquireExceptionInfo();
  ImageInfo *image_info = AcquireImageInfo();
  CopyMagickString(image_info->filename, file_name, MaxTextExtent);
  Image *images = ReadImage(image_info, exception_info);
  Image *image = RemoveFirstImageFromList(&images);
  int width = image->columns;
  int height = image->rows;
  int size = width * height * 4;
  // ExportImagePixels
  PixelPacket *pixel = GetAuthenticPixels(image, 0, 0, image->columns, image->rows, exception_info);
  printf("%d %d %d\n", pixel->red, pixel->green, pixel->blue);
  DestroyImage(image);
  DestroyImageInfo(image_info);
  DestroyExceptionInfo(exception_info);
}
