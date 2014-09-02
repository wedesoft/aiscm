#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <libguile.h>

static scm_t_bits xdisplay_tag;

static scm_t_bits xwindow_tag;

struct xdisplay_t {
  Display *display;
};

struct xwindow_t {
  Display *display;
  Window window;
  Colormap color_map;
  XVisualInfo visual_info;
};

SCM close_xdisplay(SCM scm_self)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(scm_self);
  if (self->display) {
    XCloseDisplay(self->display);
    self->display = NULL;
  };
  return SCM_UNSPECIFIED;
}

size_t free_xdisplay(SCM scm_self)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(scm_self);
  close_xdisplay(scm_self);
  scm_gc_free(self, sizeof(struct xdisplay_t), "xdisplay");
  return 0;
}

SCM make_xdisplay(SCM scm_name)
{
  SCM retval;
  struct xdisplay_t *self;
  const char *name = scm_to_locale_string(scm_name);
  Display *display = XOpenDisplay(*name == '\0' ? (const char *)NULL : name);
  if (!display) scm_syserror("make_xdisplay");
  self = (struct xdisplay_t *)scm_gc_calloc(sizeof(struct xdisplay_t), "xdisplay");
  SCM_NEWSMOB(retval, xdisplay_tag, self);
  self->display = display;
  return retval;
}

SCM width_display(SCM scm_self)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(scm_self);
  return scm_from_signed_integer(DisplayWidth(self->display, DefaultScreen(self->display)));
}

SCM height_display(SCM scm_self)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(scm_self);
  return scm_from_signed_integer(DisplayHeight(self->display, DefaultScreen(self->display)));
}

SCM close_xwindow(SCM scm_self)
{
  struct xwindow_t *self = (struct xwindow_t *)SCM_SMOB_DATA(scm_self);
  if (self->window) {
    XDestroyWindow(self->display, self->window);
    self->window = 0;
  };
  if (self->color_map) {
    XFreeColormap(self->display, self->color_map);
  };
  return SCM_UNSPECIFIED;
}

size_t free_xwindow(SCM scm_self)
{
  struct xwindow_t *self = (struct xwindow_t *)SCM_SMOB_DATA(scm_self);
  close_xwindow(scm_self); // <-> hide
  scm_gc_free(self, sizeof(struct xwindow_t), "xwindow");
  return 0;
}

SCM make_xwindow(SCM scm_display, SCM scm_width, SCM scm_height)
{
  SCM retval;
  struct xwindow_t *self;
  struct xdisplay_t *display;
  self = (struct xwindow_t *)scm_gc_calloc(sizeof(struct xwindow_t), "xwindow");
  SCM_NEWSMOB(retval, xwindow_tag, self);
  display = (struct xdisplay_t *)SCM_SMOB_DATA(scm_display);
  self->display = display->display;
  if (!XMatchVisualInfo(self->display, DefaultScreen(self->display),
                        24, TrueColor, &self->visual_info))
    scm_syserror("make_xwindow");
  self->color_map = XCreateColormap(self->display, DefaultRootWindow(self->display),
                                    self->visual_info.visual, AllocNone);
  if (!self->color_map) scm_syserror("make_xwindow");
  XSetWindowAttributes attributes;
  attributes.colormap = self->color_map;
  attributes.event_mask = KeyPressMask | ExposureMask | StructureNotifyMask;
  self->window = XCreateWindow(self->display, RootWindow(self->display, self->visual_info.screen),
                               0, 0, scm_to_int(scm_width), scm_to_int(scm_height),
                               0, self->visual_info.depth, InputOutput, self->visual_info.visual,
                               CWColormap | CWEventMask, &attributes);
  if (!self->window) scm_syserror("make_xwindow");
  return retval;
}

Bool wait_for_notify(Display *d, XEvent *e, char *arg)
{
  return (e->type == MapNotify || e->type == UnmapNotify) &&
         (e->xmap.window == (Window)arg);
}

SCM show_xwindow(SCM scm_self)
{
  XEvent event;
  struct xwindow_t *self = (struct xwindow_t *)SCM_SMOB_DATA(scm_self);
  XMapWindow(self->display, self->window);
  XIfEvent(self->display, &event, wait_for_notify, (char *)self->window);
  return scm_self;
}

SCM hide_xwindow(SCM scm_self)
{
  XEvent event;
  struct xwindow_t *self = (struct xwindow_t *)SCM_SMOB_DATA(scm_self);
  XUnmapWindow(self->display, self->window);
  XIfEvent(self->display, &event, wait_for_notify, (char *)self->window);
  return scm_self;
}

void init_xorg(void)
{
  xdisplay_tag = scm_make_smob_type("xdisplay", sizeof(struct xdisplay_t));
  xwindow_tag = scm_make_smob_type("xwindow", sizeof(struct xwindow_t));
  scm_set_smob_free(xdisplay_tag, free_xdisplay);
  scm_set_smob_free(xwindow_tag, free_xwindow);
  scm_c_define_gsubr("make-xdisplay", 1, 0, 0, make_xdisplay);
  scm_c_define_gsubr("width-display", 1, 0, 0, width_display);
  scm_c_define_gsubr("height-display", 1, 0, 0, height_display);
  scm_c_define_gsubr("close-xdisplay", 1, 0, 0, close_xdisplay);
  scm_c_define_gsubr("make-xwindow", 3, 0, 0, make_xwindow);
  scm_c_define_gsubr("show-xwindow", 1, 0, 0, show_xwindow);
  scm_c_define_gsubr("hide-xwindow", 1, 0, 0, hide_xwindow);
  scm_c_define_gsubr("close-xwindow", 1, 0, 0, close_xwindow);
}
