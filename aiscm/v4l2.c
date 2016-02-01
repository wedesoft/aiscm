#include <fcntl.h>
#include <unistd.h>
#include <malloc.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <linux/videodev2.h>
#include <libguile.h>

static scm_t_bits videodev2_tag;

struct videodev2_t {
  int fd;
  enum {IO_READ, IO_MMAP, IO_USERPTR} io;
  struct v4l2_format format;
  struct v4l2_requestbuffers req;
  struct v4l2_buffer buf[2];
  void *map[2];
  void *user[2];
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

SCM videodev2_destroy(SCM scm_self)
{
  scm_assert_smob_type(videodev2_tag, scm_self);
  int i;
  struct videodev2_t *self = (struct videodev2_t *)SCM_SMOB_DATA(scm_self);
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

size_t free_videodev2(SCM scm_self)
{
  struct videodev2_t *self = (struct videodev2_t *)SCM_SMOB_DATA(scm_self);
  videodev2_destroy(scm_self);
  scm_gc_free(self, sizeof(struct videodev2_t), "videodev2");
  return 0;
}

SCM make_videodev2(SCM scm_name, SCM scm_channel, SCM scm_select)
{
  SCM retval;
  struct videodev2_t *self;
  struct stat st;
  int i;
  const char *name = scm_to_locale_string(scm_name);
  if (stat(name, &st)) scm_syserror("make-videodev2");
  if (!S_ISCHR(st.st_mode)) scm_misc_error("make-videodev2", "'~a' is not a device", scm_list_1(scm_name));
  self = (struct videodev2_t *)scm_gc_calloc(sizeof(struct videodev2_t), "v4l2");
  SCM_NEWSMOB(retval, videodev2_tag, self);
  self->fd = -1;
  self->map[0] = MAP_FAILED;
  self->map[1] = MAP_FAILED;
  self->fd = open(name, O_RDWR, 0);
  if (self->fd == -1) scm_syserror("make-videodev2");
  struct v4l2_capability cap;
  if (xioctl(self->fd, VIDIOC_QUERYCAP, &cap)) {
    videodev2_destroy(retval);
    scm_misc_error("make-videodev2", "Error querying capabilities of device '~a'", scm_list_1(scm_name));
  };
  if (cap.capabilities & V4L2_CAP_VIDEO_CAPTURE == 0) {
    videodev2_destroy(retval);
    scm_misc_error("make-videodev2", "'~a' is not a video capture device", scm_list_1(scm_name));
  };
  int c = scm_to_int(scm_channel);
  if (xioctl(self->fd, VIDIOC_S_INPUT, &c)) {
    videodev2_destroy(retval);
    scm_misc_error("make-videodev2", "Error selecting channel ~a for device '~a'",
                   scm_list_2(scm_channel, scm_name));
  };
  SCM scm_selection = SCM_EOL;
  int format_index = 0;
  while (1) {
    struct v4l2_fmtdesc format;
    memset(&format, 0, sizeof(format));
    format.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    format.index = format_index++;
    if (xioctl(self->fd, VIDIOC_ENUM_FMT, &format)) break;
    int size_index = 0;
    while (1) {
      struct v4l2_frmsizeenum pix;
      memset(&pix, 0, sizeof(pix));
      pix.pixel_format = format.pixelformat;
      pix.index = size_index++;
      if (xioctl(self->fd, VIDIOC_ENUM_FRAMESIZES, &pix)) break;
      if (pix.type == V4L2_FRMSIZE_TYPE_DISCRETE) {
        SCM scm_fmt = scm_list_3(scm_from_int(format.pixelformat),
            scm_from_int(pix.discrete.width),
            scm_from_int(pix.discrete.height));
        scm_selection = scm_cons(scm_fmt, scm_selection);
      } else if (pix.type == V4L2_FRMSIZE_TYPE_STEPWISE) {
        unsigned int
          w = pix.stepwise.min_width,
            h = pix.stepwise.min_height;
        while (w <= pix.stepwise.max_width && h <= pix.stepwise.max_height) {
          SCM scm_fmt = scm_list_3(scm_from_int(format.pixelformat),
              scm_from_int(w),
              scm_from_int(h));
          scm_selection = scm_cons(scm_fmt, scm_selection);
          w += pix.stepwise.step_width;
          h += pix.stepwise.step_height;
        };
      } else {
        SCM scm_fmt = scm_list_3(scm_from_int(format.pixelformat),
            scm_from_int(pix.stepwise.max_width),
            scm_from_int(pix.stepwise.max_height));
        scm_selection = scm_cons(scm_fmt, scm_selection);
      };
    };
  }
  SCM scm_selected = scm_call_1(scm_select, scm_selection);
  self->format.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  self->format.fmt.pix.pixelformat = scm_to_int(scm_car(scm_selected));
  self->format.fmt.pix.width = scm_to_int(scm_cadr(scm_selected));
  self->format.fmt.pix.height = scm_to_int(scm_caddr(scm_selected));
  self->format.fmt.pix.field = V4L2_FIELD_ANY;
  if (xioctl(self->fd, VIDIOC_S_FMT, &self->format)) {
    videodev2_destroy(retval);
    scm_misc_error("make-videodev2", "Error switching device '~a' to selected format",
                   scm_list_1(scm_name));
  };
  if (cap.capabilities & V4L2_CAP_STREAMING) {
    self->req.count = 2;
    self->req.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    self->req.memory = V4L2_MEMORY_MMAP;
    if (xioctl(self->fd, VIDIOC_REQBUFS, &self->req)) {
      self->req.memory = V4L2_MEMORY_USERPTR;
      if (xioctl(self->fd, VIDIOC_REQBUFS, &self->req)) {
        videodev2_destroy(retval);
        scm_misc_error("make-videodev2", "Failed to set up memory mapped and user pointer I/O for device '~a'",
                       scm_list_1(scm_name));
      };
      int page_size = getpagesize();
      int buffer_size = (self->format.fmt.pix.sizeimage + page_size - 1) & ~(page_size - 1);
      self->user[0] = memalign(page_size, buffer_size);
      self->user[1] = memalign(page_size, buffer_size);
      if (self->user[0] == NULL || self->user[1] == NULL) {
        videodev2_destroy(retval);
        scm_misc_error("make-videodev2", "Could not allocate memory for user pointer I/O for device '~a'",
                       scm_list_1(scm_name));
      };
      for (i=0; i<2; i++) {
        self->buf[i].type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        self->buf[i].memory = V4L2_MEMORY_USERPTR;
        self->buf[i].index = i;
        self->buf[i].m.userptr = (unsigned long)self->user[i];
        self->buf[i].length = buffer_size;
      };
      for (i=0; i<2; i++) {
        if (xioctl(self->fd, VIDIOC_QBUF, &self->buf[i])) {
          videodev2_destroy(retval);
          scm_misc_error("make-videodev2", "Error enqueuing user memory buffer ~a for device '~a'",
                         scm_list_2(scm_from_int(i), scm_name));
        };
      };
      enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      if (xioctl(self->fd, VIDIOC_STREAMON, &type)) {
        videodev2_destroy(retval);
        scm_misc_error("make-videodev2", "Error starting user pointer capture process for device '~a'",
                         scm_list_1(scm_name));
      };
      self->capture = 1;
      self->io = IO_USERPTR;
    } else {
      if (self->req.count < 2) {
        videodev2_destroy(retval);
        scm_misc_error("make-videodev2", "Insufficient buffer memory on device '~a'",
                       scm_list_1(scm_name));
      };
      for (i=0; i<2; i++) {
        self->buf[i].type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        self->buf[i].memory = V4L2_MEMORY_MMAP;
        self->buf[i].index = i;
        if (xioctl(self->fd, VIDIOC_QUERYBUF, &self->buf[i])) {
          videodev2_destroy(retval);
          scm_misc_error("make-videodev2", "Error querying buffer ~a for device '~a'",
                         scm_list_2(scm_from_int(i), scm_name));
        };
        self->map[i] = mmap(NULL, self->buf[i].length, PROT_READ | PROT_WRITE,
                            MAP_SHARED, self->fd, self->buf[i].m.offset);
        if (self->map[i] == MAP_FAILED) {
          videodev2_destroy(retval);
          scm_misc_error("make-videodev2", "Error mapping capture buffer ~a for device '~a'",
                         scm_list_2(scm_from_int(i), scm_name));
        };
      }
      for (i=0; i<2; i++) {
        if (xioctl(self->fd, VIDIOC_QBUF, &self->buf[i])) {
          videodev2_destroy(retval);
          scm_misc_error("make-videodev2", "Error enqueuing buffer ~a for device '~a'",
                         scm_list_2(scm_from_int(i), scm_name));
        };
      };
      enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      if (xioctl(self->fd, VIDIOC_STREAMON, &type)) {
        videodev2_destroy(retval);
        scm_misc_error("make-videodev2", "Error starting memory-mapped capture process for device '~a'",
                       scm_list_1(scm_name));
      };
      self->capture = 1;
      self->io = IO_MMAP;
    };
  } else if (cap.capabilities & V4L2_CAP_STREAMING) {
    self->io = IO_READ;
  } else {
    videodev2_destroy(retval);
    scm_misc_error("make-videodev2", "Device '~a' neither supports streaming nor reading from device",
                   scm_list_1(scm_name));
  };
  return retval;
}

SCM videodev2_grab(SCM scm_self)
{
  scm_assert_smob_type(videodev2_tag, scm_self);
  SCM retval;
  struct videodev2_t *self = (struct videodev2_t *)SCM_SMOB_DATA(scm_self);
  if (self->fd <= 0)
    scm_misc_error("videodev2-grab", "Device is not open. Did you call 'destroy' before?",
                   SCM_UNDEFINED);
  int width = self->format.fmt.pix.width;
  int height = self->format.fmt.pix.height;
  if (self->io == IO_READ) {
    int size = self->format.fmt.pix.sizeimage;
    void *buf = scm_gc_malloc_pointerless(size, "aiscm v4l2 frame");
    if (read(self->fd, buf, size) == -1) scm_syserror("videodev2-read");
    retval = scm_list_4(scm_from_int(self->format.fmt.pix.pixelformat),
                        scm_from_int(width),
                        scm_from_int(height),
                        scm_from_pointer(buf, NULL));
  } else {
    if (self->frame_used) {
      if (xioctl(self->fd, VIDIOC_QBUF, &self->frame)) scm_sys_error("videodev2-read");
      self->frame_used = 0;
    };
    memset(&self->frame, 0, sizeof(struct v4l2_buffer));
    self->frame.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    self->frame.memory = self->io == IO_MMAP ? V4L2_MEMORY_MMAP : V4L2_MEMORY_USERPTR;
    if (xioctl(self->fd, VIDIOC_DQBUF, &self->frame)) scm_sys_error("videodev2-read");
    self->frame_used = 1;
    void *p = self->io == IO_MMAP ? self->map[self->frame.index] : self->user[self->frame.index];
    retval = scm_list_4(scm_from_int(self->format.fmt.pix.pixelformat),
                        scm_list_2(scm_from_int(width), scm_from_int(height)),
                        scm_from_pointer(p, NULL),
                        scm_from_int(self->format.fmt.pix.sizeimage));
  };
  return retval;
}

void init_v4l2(void)
{
  videodev2_tag = scm_make_smob_type("videodev2", sizeof(struct videodev2_t));
  scm_set_smob_free(videodev2_tag, free_videodev2);
  scm_c_define("V4L2_PIX_FMT_RGB24" ,scm_from_int(V4L2_PIX_FMT_RGB24));
  scm_c_define("V4L2_PIX_FMT_BGR24" ,scm_from_int(V4L2_PIX_FMT_BGR24));
  scm_c_define("V4L2_PIX_FMT_GREY"  ,scm_from_int(V4L2_PIX_FMT_GREY));
  scm_c_define("V4L2_PIX_FMT_YUV420",scm_from_int(V4L2_PIX_FMT_YUV420));
  scm_c_define("V4L2_PIX_FMT_UYVY"  ,scm_from_int(V4L2_PIX_FMT_UYVY));
  scm_c_define("V4L2_PIX_FMT_YUYV"  ,scm_from_int(V4L2_PIX_FMT_YUYV));
  scm_c_define("V4L2_PIX_FMT_MJPEG" ,scm_from_int(V4L2_PIX_FMT_MJPEG));
  scm_c_define_gsubr("make-videodev2", 3, 0, 0, make_videodev2);
  scm_c_define_gsubr("videodev2-destroy", 1, 0, 0, videodev2_destroy);
  scm_c_define_gsubr("videodev2-grab", 1, 0, 0, videodev2_grab);
}
