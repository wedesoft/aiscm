#include <opencv2/aruco/charuco.hpp>
#include <opencv2/imgcodecs.hpp>

int main(void)
{
  cv::Mat m(700, 500, CV_8UC1);
  cv::Ptr< cv::aruco::Dictionary > dict = cv::aruco::getPredefinedDictionary(cv::aruco::DICT_4X4_50);
  cv::Ptr< cv::aruco::CharucoBoard > charuco = cv::aruco::CharucoBoard::create(7, 5, 0.04, 0.02, dict);
  charuco->draw(cv::Size(700, 500), m, 0, 1);
  cv::imwrite("board.png", m);
  return 0;
}
