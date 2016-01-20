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
  unsigned char *buf = (char *)malloc(size);
  // ExportImagePixels
  ExportImagePixels(image, 0, 0, image->columns, image->rows, "RGBA", CharPixel, buf, exception_info);
  printf("%d %d %d\n", buf[0], buf[1], buf[2]);
  printf("%d %d %d\n", buf[24], buf[25], buf[26]);
  printf("%d %d %d\n", buf[4], buf[5], buf[6]);
  free(buf);
  DestroyImage(image);
  DestroyImageInfo(image_info);
  DestroyExceptionInfo(exception_info);
}
