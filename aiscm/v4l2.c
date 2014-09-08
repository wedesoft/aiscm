#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <linux/videodev2.h>
#include <libguile.h>

static scm_t_bits v4l2_tag;

struct v4l2_t {
  int fd;
  struct v4l2_format format;
  struct v4l2_requestbuffers req;
  struct v4l2_buffer buf[2];
  void *map[2];
  struct v4l2_buffer frame;
  char frame_used;
  char capture;
};

static int xioctl(int fd, int request, void *arg)
{
  int r;
  do {
    r = ioctl(fd, request, arg);
  } while (r == -1 && errno == EINTR);
  return r;
}

SCM v4l2_close(SCM scm_self)
{
  int i;
  struct v4l2_t *self = (struct v4l2_t *)SCM_SMOB_DATA(scm_self);
  if (self->capture) {
    enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    xioctl(self->fd, VIDIOC_STREAMOFF, &type);
    self->capture = 0;
  };
  for (i=0; i<2; i++)
    if (self->map[i] != MAP_FAILED) {
      munmap(self->map[i], self->buf[i].length);
      self->map[i] = MAP_FAILED;
    };
  if (self->fd != -1) {
    close(self->fd);
    self->fd = -1;
  };
  return SCM_UNSPECIFIED;
}

size_t free_v4l2(SCM scm_self)
{
  struct v4l2_t *self = (struct v4l2_t *)SCM_SMOB_DATA(scm_self);
  v4l2_close(scm_self);
  scm_gc_free(self, sizeof(struct v4l2_t), "v4l2");
  return 0;
}

SCM make_v4l2(SCM scm_name, SCM channel)
{
  SCM retval;
  struct v4l2_t *self;
  struct stat st;
  int i;
  const char *name = scm_to_locale_string(scm_name);
  if (stat(name, &st)) scm_syserror("make_v4l2");
  if (!S_ISCHR(st.st_mode)) scm_misc_error("make_v4l2", "'~a' is not a device", scm_list_1(scm_name));
  self = (struct v4l2_t *)scm_gc_calloc(sizeof(struct v4l2_t), "v4l2");
  SCM_NEWSMOB(retval, v4l2_tag, self);
  self->fd = -1;
  self->map[0] = MAP_FAILED;
  self->map[1] = MAP_FAILED;
  self->fd = open(name, O_RDWR, 0);
  if (self->fd == -1) scm_syserror("make_v4l2");
  struct v4l2_capability cap;
  if (xioctl(self->fd, VIDIOC_QUERYCAP, &cap)) {
    v4l2_close(retval);
    scm_misc_error("make_v4l2", "Error querying capabilities of device '~a'", scm_list_1(scm_name));
  };
  if (cap.capabilities & V4L2_CAP_VIDEO_CAPTURE == 0) {
    v4l2_close(retval);
    scm_misc_error("make_v4l2", "'~a' is not a video capture device", scm_list_1(scm_name));
  };
  int c = scm_to_int(channel);
  if (xioctl(self->fd, VIDIOC_S_INPUT, &c)) {
    v4l2_close(retval);
    scm_misc_error("make_v4l2", "Error selecting channel ~a for device '~a'",
                   scm_list_2(channel, scm_name));
  };
  // TODO: implement selection of video mode
  self->format.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  self->format.fmt.pix.width = 640;
  self->format.fmt.pix.height = 480;
  self->format.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
  self->format.fmt.pix.field = V4L2_FIELD_ANY;
  if (xioctl(self->fd, VIDIOC_S_FMT, &self->format)) {
    v4l2_close(retval);
    scm_misc_error("make_v4l2", "Error switching device '~a' to selected format",
                   scm_list_1(scm_name));
  };
  if (cap.capabilities & V4L2_CAP_STREAMING) {
    self->req.count = 2;
    self->req.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    self->req.memory = V4L2_MEMORY_MMAP;
    if (xioctl(self->fd, VIDIOC_REQBUFS, &self->req)) {
      // TODO: Try user pointer I/O
      v4l2_close(retval);
      scm_misc_error("make_v4l2", "Memory mapped I/O not supported for device '~a'",
                     scm_list_1(scm_name));
    };
    if (self->req.count < 2) {
      v4l2_close(retval);
      scm_misc_error("make_v4l2", "Insufficient buffer memory on device '~a'",
                     scm_list_1(scm_name));
    };
    for (i=0; i<2; i++) {
      self->buf[i].type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      self->buf[i].memory = V4L2_MEMORY_MMAP;
      self->buf[i].index = i;
      if (xioctl(self->fd, VIDIOC_QUERYBUF, &self->buf[i])) {
        v4l2_close(retval);
        scm_misc_error("make_v4l2", "Error querying buffer ~a for device '~a'",
                       scm_list_2(scm_from_int(i), scm_name));
      };
      self->map[i] = mmap(NULL, self->buf[i].length, PROT_READ | PROT_WRITE,
                          MAP_SHARED, self->fd, self->buf[i].m.offset);
      if (self->map[i] == MAP_FAILED) {
        v4l2_close(retval);
        scm_misc_error("make_v4l2", "Error mapping capture buffer ~a for device '~a'",
                       scm_list_2(scm_from_int(i), scm_name));
      };
    }
    for (i=0; i<2; i++) {
      if (xioctl(self->fd, VIDIOC_QBUF, &self->buf[i])) {
        v4l2_close(retval);
        scm_misc_error("make_v4l2", "Error enqueuing buffer ~a for device '~a'",
                       scm_list_2(scm_from_int(i), scm_name));
      };
    };
    enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if (xioctl(self->fd, VIDIOC_STREAMON, &type)) {
      v4l2_close(retval);
      scm_misc_error("make_v4l2", "Error starting memory-mapped capture process for device '~a'",
                     scm_list_1(scm_name));
    };
    self->capture = 1;
  } else {
    v4l2_close(retval);
    scm_misc_error("make_v4l2", "Device '~a' neither supports streaming nor reading from device",
                   scm_list_1(scm_name));
  };
  return retval;
}

SCM v4l2_read(SCM scm_self)
{
  struct v4l2_t *self = (struct v4l2_t *)SCM_SMOB_DATA(scm_self);
  if (self->fd <= 0)
    scm_misc_error("make_v4l2", "Device is not open. Did you call close before?",
                   SCM_UNDEFINED);
  if (self->frame_used) {
    if (xioctl(self->fd, VIDIOC_QBUF, &self->frame)) scm_sys_error("make_v4l2");
    self->frame_used = 0;
  };
  memset(&self->frame, 0, sizeof(struct v4l2_buffer));
  self->frame.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  self->frame.memory = V4L2_MEMORY_MMAP;
  if (xioctl(self->fd, VIDIOC_DQBUF, &self->frame)) scm_sys_error("make_v4l2");
  self->frame_used = 1;
  return scm_list_4(scm_from_int(self->format.fmt.pix.pixelformat),
                    scm_from_int(self->format.fmt.pix.width),
                    scm_from_int(self->format.fmt.pix.height),
                    scm_from_pointer(self->map[self->frame.index], NULL));
}

void init_v4l2(void)
{
  v4l2_tag = scm_make_smob_type("v4l2", sizeof(struct v4l2_t));
  scm_set_smob_free(v4l2_tag, free_v4l2);
  scm_c_define("V4L2_PIX_FMT_YUYV", scm_from_int(V4L2_PIX_FMT_YUYV));
  scm_c_define_gsubr("make-v4l2", 2, 0, 0, make_v4l2);
  scm_c_define_gsubr("v4l2-close", 1, 0, 0, v4l2_close);
  scm_c_define_gsubr("v4l2-read-orig", 1, 0, 0, v4l2_read);
}
