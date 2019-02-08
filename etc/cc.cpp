#include <cstdio>
#include <opencv2/core.hpp>
#include <opencv2/imgproc.hpp>


int main(void)
{
  char data[] = {1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1};
  cv::Mat m(3, 4, CV_8UC1, &data[0]);
  cv::Mat l(3, 4, CV_16UC1);
  connectedComponents(m, l, 8, CV_16UC1);
  for (int j=0; j<3; j++) {
    for (int i=0; i<4; i++)
      printf(" %1d", m.at<unsigned char>(j, i));
    printf("\n");
  };
  printf("\n");
  for (int j=0; j<3; j++) {
    for (int i=0; i<4; i++)
      printf(" %1d", l.at<unsigned short int>(j, i));
    printf("\n");
  };
  return 0;
}
