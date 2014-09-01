#include <X11/Xlib.h>
#include <libguile.h>

static scm_t_bits xdisplay_tag;

struct xdisplay_t {
  Display *display;
};

SCM close_xdisplay(SCM smob)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(smob);
  if (self->display) {
    XCloseDisplay(self->display);
    self->display = NULL;
  };
  return SCM_UNSPECIFIED;
}

size_t free_xdisplay(SCM smob)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(smob);
  close_xdisplay(smob);
  scm_gc_free(self, sizeof(struct xdisplay_t), "xdisplay");
  return 0;
}

SCM make_xdisplay(SCM name)
{
  SCM retval;
  struct xdisplay_t *self;
  const char *name_c_str = scm_to_locale_string(name);
  Display *display = XOpenDisplay(*name_c_str == '\0' ? (const char *)NULL : name_c_str);
  if (!display) scm_syserror("make_xdisplay");
  self = (struct xdisplay_t *)scm_gc_calloc(sizeof(struct xdisplay_t), "xdisplay");
  SCM_NEWSMOB(retval, xdisplay_tag, self);
  self->display = display;
  return retval;
}

SCM width_display(SCM smob)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(smob);
  return scm_from_signed_integer(DisplayWidth(self->display, DefaultScreen(self->display)));
}

SCM height_display(SCM smob)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(smob);
  return scm_from_signed_integer(DisplayHeight(self->display, DefaultScreen(self->display)));
}

void init_xorg(void)
{
  xdisplay_tag = scm_make_smob_type("xdisplay", sizeof(struct xdisplay_t));
  scm_set_smob_free(xdisplay_tag, free_xdisplay);
  scm_c_define_gsubr("make-xdisplay", 1, 0, 0, make_xdisplay);
  scm_c_define_gsubr("width-display", 1, 0, 0, width_display);
  scm_c_define_gsubr("height-display", 1, 0, 0, height_display);
  scm_c_define_gsubr("close-xdisplay", 1, 0, 0, close_xdisplay);
}
