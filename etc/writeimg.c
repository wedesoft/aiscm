#include <magick/MagickCore.h>
#include <stdio.h>

int main(void)
{
  const char *file_name = "out.png";
  int i, j;
  int width = 6;
  int height = 4;
  int size = width * height;
  unsigned char *buf = (char *)malloc(size);
  for (j=0; j<height; j++)
    for (i=0; i<width; i++)
      buf[j * width + i] = j * 16 + i;
  ExceptionInfo *exception_info = AcquireExceptionInfo();
  ImageInfo *image_info = AcquireImageInfo();
  GetImageInfo(image_info);
  Image *image = ConstituteImage(width, height, "I", CharPixel, buf, exception_info);
  CatchException(exception_info);
  Image *images = NewImageList();
  AppendImageToList(&images, image);
  WriteImages(image_info, images, file_name, exception_info);
  CatchException(exception_info);
  DestroyImageList(images);
  free(buf);
  DestroyImageInfo(image_info);
  DestroyExceptionInfo(exception_info);
}
