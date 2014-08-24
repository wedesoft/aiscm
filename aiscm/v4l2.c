#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>
#include <libguile.h>

static scm_t_bits v4l2_tag;

struct v4l2_t {
  int fd;
};

size_t free_v4l2(SCM v4l2_smob)
{
  struct v4l2_t *v4l2 = (struct v4l2_t *)SCM_SMOB_DATA(v4l2_smob);
  if (v4l2->fd) close(v4l2->fd);
  scm_gc_free(v4l2, sizeof(struct v4l2_t), "v4l2");
  return 0;
}

SCM make_v4l2(SCM device_name)
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
  return retval;
}

void init_v4l2(void)
{
  v4l2_tag = scm_make_smob_type("v4l2", sizeof(struct v4l2_t));
  scm_set_smob_free(v4l2_tag, free_v4l2);
  scm_c_define_gsubr("make-v4l2", 1, 0, 0, make_v4l2);
}
