#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#include <linux/videodev2.h>
#include <libguile.h>

static scm_t_bits v4l2_tag;

struct v4l2_t {
  int fd;
};

static int xioctl(int fd, int request, void *arg)
{
  int r;
  do {
    r = ioctl(fd, request, arg);
  } while (r == -1 && errno == EINTR);
  return r;
}

size_t free_v4l2(SCM v4l2_smob)
{
  struct v4l2_t *v4l2 = (struct v4l2_t *)SCM_SMOB_DATA(v4l2_smob);
  if (v4l2->fd) close(v4l2->fd);
  scm_gc_free(v4l2, sizeof(struct v4l2_t), "v4l2");
  return 0;
}

SCM close_v4l2(SCM self)
{
  struct v4l2_t *v4l2 = (struct v4l2_t *)SCM_SMOB_DATA(self);
  if (v4l2->fd) close(v4l2->fd);
  v4l2->fd = 0;
  return self;
}

SCM make_v4l2(SCM device_name, SCM channel)
{
  SCM retval;
  struct v4l2_t *v4l2;
  struct stat st;
  int fd;
  const char *device_c_str = scm_to_locale_string(device_name);
  if (stat(device_c_str, &st)) scm_syserror("make_v4l2");
  if (!S_ISCHR(st.st_mode)) scm_misc_error("make_v4l2", "'~a' is not a device", scm_list_1(device_name));
  fd = open(device_c_str, O_RDWR, 0);
  if (fd == -1) scm_syserror("make_v4l2");
  v4l2 = (struct v4l2_t *)scm_gc_malloc(sizeof(struct v4l2_t), "v4l2");
  SCM_NEWSMOB(retval, v4l2_tag, v4l2);
  v4l2->fd = fd;
  struct v4l2_capability capability;
  if (xioctl(fd, VIDIOC_QUERYCAP, &capability)) {
    close_v4l2(retval);
    scm_syserror("make_v4l2");
  };
  if (capability.capabilities & V4L2_CAP_VIDEO_CAPTURE == 0) {
    close_v4l2(retval);
    scm_misc_error("make_v4l2", "'~a' is not a video capture device", scm_list_1(device_name));
  };
  int c = scm_to_int(channel);
  if (xioctl(fd, VIDIOC_S_INPUT, &c)) {
    close_v4l2(retval);
    scm_misc_error("make_v4l2", "Error selecting channel ~a for device '~a'",
                   scm_list_2(channel, device_name));
  };
  return retval;
}

void init_v4l2(void)
{
  v4l2_tag = scm_make_smob_type("v4l2", sizeof(struct v4l2_t));
  scm_set_smob_free(v4l2_tag, free_v4l2);
  scm_c_define_gsubr("make-v4l2", 2, 0, 0, make_v4l2);
  scm_c_define_gsubr("close-v4l2", 1, 0, 0, close_v4l2);
}
