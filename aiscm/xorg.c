#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <libguile.h>

#ifndef timersub
#define timersub(a, b, result)                                                \
  do {                                                                        \
    (result)->tv_sec = (a)->tv_sec - (b)->tv_sec;                             \
    (result)->tv_usec = (a)->tv_usec - (b)->tv_usec;                          \
    if ((result)->tv_usec < 0) {                                              \
      --(result)->tv_sec;                                                     \
      (result)->tv_usec += 1000000;                                           \
    }                                                                         \
  } while (0)
#endif

static scm_t_bits xdisplay_tag;

static scm_t_bits xwindow_tag;

struct xdisplay_t {
  Display *display;
  SCM scm_windows;
  char quit;
};

struct xwindow_t {
  struct xdisplay_t *display;
  Window window;
  int width;
  int height;
  Colormap color_map;
  XVisualInfo visual_info;
  GC gc;
  SCM scm_frame;
  SCM scm_converted;
  Atom wm_protocols;
  Atom wm_delete_window;
};

SCM xdisplay_close(SCM scm_self)
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
  self->scm_windows = SCM_EOL;
  self->display = display;
  return retval;
}

SCM xdisplay_width(SCM scm_self)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(scm_self);
  return scm_from_signed_integer(DisplayWidth(self->display, DefaultScreen(self->display)));
}

SCM xdisplay_height(SCM scm_self)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(scm_self);
  return scm_from_signed_integer(DisplayHeight(self->display, DefaultScreen(self->display)));
}

static Bool always_true(Display *display, XEvent *event, XPointer pointer)
{
  return True;
}

void xwindow_paint(struct xwindow_t *window);

void handle_event(struct xdisplay_t *self, XEvent *event)
{
  SCM scm_windows = self->scm_windows;
  struct xwindow_t *window = NULL;
  while (!scm_is_null_and_not_nil(scm_windows)) {
    struct xwindow_t *w = (struct xwindow_t *)SCM_SMOB_DATA(scm_car(scm_windows));
    if (w->window == event->xany.window) {
      window = w;
      break;
    };
    scm_windows = scm_cdr(scm_windows);
  };
  if (window) {
    switch (event->type) {
      case ClientMessage:
        if ((event->xclient.message_type == window->wm_protocols ) &&
            ((Atom)event->xclient.data.l[0] == window->wm_delete_window))
          self->quit = 1;
        break;
      case KeyPress:
        switch (event->xkey.keycode) {
          case 0x09:
          case 0x41:
            self->quit = 1;
        };
        break;
      case ConfigureNotify:
        while (XCheckTypedWindowEvent(self->display, window->window,
                                      ConfigureNotify, event));
        window->scm_converted = SCM_UNDEFINED;
        window->width = event->xconfigure.width;
        window->height = event->xconfigure.height;
        xwindow_paint(window);
        break;
      case Expose:
        while (XCheckTypedWindowEvent(self->display, window->window,
                                      Expose, event));
        xwindow_paint(window);
        break;
    };
  };
}

SCM xdisplay_process_events(SCM scm_self)
{
  XEvent event;
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(scm_self);
  while (XCheckIfEvent(self->display, &event, always_true, NULL))
    handle_event(self, &event);
  return scm_self;
}

SCM xdisplay_event_loop(SCM scm_self, SCM scm_timeout)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(scm_self);
  double timeout = scm_to_double(scm_timeout);
  if (timeout >= 0) {
    struct timeval t0;
    double elapsed;
    gettimeofday(&t0, NULL);
    do {
      xdisplay_process_events(scm_self);
      struct timeval t;
      struct timeval difference;
      int usecs_remaining;
      gettimeofday(&t, NULL);
      timersub(&t, &t0, &difference);
      elapsed = difference.tv_sec + difference.tv_usec * 1.0E-6;
      usecs_remaining = (int)((timeout - elapsed) * 1.0E+6);
      if (usecs_remaining > 0) {
        struct timeval tval;
        tval.tv_usec = usecs_remaining % 1000000;
        tval.tv_sec  = usecs_remaining / 1000000;
        int fd = ConnectionNumber(self->display);
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(fd, &fds);
        select(fd + 1, &fds, NULL, NULL, &tval);
      };
    } while (!self->quit && elapsed < timeout);
  } else {
    while (!self->quit) {
      XEvent event;
      XNextEvent(self->display, &event);
      handle_event(self, &event);
    };
  };
  return scm_self;
}

SCM xdisplay_quit(SCM scm_self)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(scm_self);
  return self->quit ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM xdisplay_set_quit(SCM scm_self, SCM scm_quit)
{
  struct xdisplay_t *self = (struct xdisplay_t *)SCM_SMOB_DATA(scm_self);
  self->quit = scm_quit != SCM_BOOL_F;
  return xdisplay_quit(scm_self);
}

SCM xwindow_close(SCM scm_self)
{
  struct xwindow_t *self = (struct xwindow_t *)SCM_SMOB_DATA(scm_self);
  if (self->gc) {
    XFreeGC(self->display->display, self->gc);
    self->gc = 0;
  };
  if (self->window) {
    XDestroyWindow(self->display->display, self->window);
    self->window = 0;
  };
  if (self->color_map) {
    XFreeColormap(self->display->display, self->color_map);
    self->color_map = 0;
  };
  self->display->scm_windows = scm_delete(scm_self, self->display->scm_windows);
  return SCM_UNSPECIFIED;
}

size_t free_xwindow(SCM scm_self)
{
  struct xwindow_t *self = (struct xwindow_t *)SCM_SMOB_DATA(scm_self);
  close_xwindow(scm_self);
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
  self->scm_frame = SCM_UNDEFINED;
  self->scm_converted = SCM_UNDEFINED;
  display = (struct xdisplay_t *)SCM_SMOB_DATA(scm_display);
  self->display = display;
  self->width = scm_to_int(scm_width);
  self->height = scm_to_int(scm_height);
  if (!XMatchVisualInfo(display->display, DefaultScreen(display->display),
                        24, TrueColor, &self->visual_info))
    scm_syserror("make_xwindow");
  self->color_map = XCreateColormap(display->display, DefaultRootWindow(display->display),
                                    self->visual_info.visual, AllocNone);
  if (!self->color_map) scm_syserror("make_xwindow");
  XSetWindowAttributes attributes;
  attributes.colormap = self->color_map;
  attributes.event_mask = KeyPressMask | ExposureMask | StructureNotifyMask;
  self->window = XCreateWindow(display->display, RootWindow(display->display, self->visual_info.screen),
                               0, 0, self->width, self->height,
                               0, self->visual_info.depth, InputOutput, self->visual_info.visual,
                               CWColormap | CWEventMask, &attributes);
  if (!self->window) scm_syserror("make_xwindow");
  XGCValues xgcv;
  self->gc = XCreateGC(display->display, self->window, 0L, &xgcv);
  if (!self->gc) scm_syserror("make_xwindow");
  self->wm_protocols = XInternAtom(display->display, "WM_PROTOCOLS", False);
  self->wm_delete_window = XInternAtom(display->display, "WM_DELETE_WINDOW", False);
  XSetWMProtocols(display->display, self->window, &self->wm_delete_window, 1);
  display->scm_windows = scm_cons(retval, display->scm_windows);
  return retval;
}

static Bool wait_for_notify(Display *d, XEvent *e, char *arg)
{
  return (e->type == MapNotify || e->type == UnmapNotify) &&
         (e->xmap.window == (Window)arg);
}

SCM xwindow_show(SCM scm_self)
{
  XEvent event;
  struct xwindow_t *self = (struct xwindow_t *)SCM_SMOB_DATA(scm_self);
  XMapWindow(self->display->display, self->window);
  XIfEvent(self->display->display, &event, wait_for_notify, (char *)self->window);
  return scm_self;
}

SCM xwindow_title(SCM scm_self, SCM scm_title)
{
  struct xwindow_t *self = (struct xwindow_t *)SCM_SMOB_DATA(scm_self);
  XStoreName(self->display->display, self->window, scm_to_locale_string(scm_title));
  return scm_title;
}

SCM xwindow_resize(SCM scm_self, SCM scm_width, SCM scm_height)
{
  struct xwindow_t *self = (struct xwindow_t *)SCM_SMOB_DATA(scm_self);
  int
    width = scm_to_int(scm_width),
    height = scm_to_int(scm_height);
  XResizeWindow(self->display->display, self->window, width, height);
  self->width = width;
  self->height = height;
  XFlush(self->display->display);
  return scm_self;
}

SCM xwindow_write(SCM scm_self, SCM scm_frame)
{
  struct xwindow_t *self = (struct xwindow_t *)SCM_SMOB_DATA(scm_self);
  self->scm_frame = scm_frame;
  self->scm_converted = SCM_UNDEFINED;
  xwindow_paint(self);
  return scm_frame;
}

SCM xwindow_hide(SCM scm_self)
{
  XEvent event;
  struct xwindow_t *self = (struct xwindow_t *)SCM_SMOB_DATA(scm_self);
  XUnmapWindow(self->display->display, self->window);
  XIfEvent(self->display->display, &event, wait_for_notify, (char *)self->window);
  return scm_self;
}

static SCM scm_convert;

void xwindow_paint(struct xwindow_t *self)
{
  if (!SCM_UNBNDP(self->scm_frame)) {
    if (SCM_UNBNDP(self->scm_converted))
      self->scm_converted = scm_call_4(scm_convert,
                                       self->scm_frame,
                                       scm_from_locale_symbol("BGRA"),
                                       scm_from_int(self->width),
                                       scm_from_int(self->height));
    char *data = scm_to_pointer(scm_slot_ref(self->scm_converted, scm_from_locale_symbol("data")));
    XImage *img = XCreateImage(self->display->display, self->visual_info.visual,
                               24, ZPixmap, 0, data, self->width, self->height,
                               32, self->width * 4);
    if (!img) scm_syserror("xwindow_paint");
    img->byte_order = LSBFirst;
    XPutImage(self->display->display, self->window, self->gc,
        img, 0, 0, 0, 0, self->width, self->height);
    img->data = (char *)NULL;
    XDestroyImage(img);
  };
}

void init_xorg(void)
{
  scm_convert = scm_c_public_ref("aiscm frame", "convert");
  xdisplay_tag = scm_make_smob_type("xdisplay", sizeof(struct xdisplay_t));
  xwindow_tag = scm_make_smob_type("xwindow", sizeof(struct xwindow_t));
  scm_set_smob_free(xdisplay_tag, free_xdisplay);
  scm_set_smob_free(xwindow_tag, free_xwindow);
  scm_c_define_gsubr("make-xdisplay", 1, 0, 0, make_xdisplay);
  scm_c_define_gsubr("xdisplay-width", 1, 0, 0, xdisplay_width);
  scm_c_define_gsubr("xdisplay-height", 1, 0, 0, xdisplay_height);
  scm_c_define_gsubr("xdisplay-process-events", 1, 0, 0, xdisplay_process_events);
  scm_c_define_gsubr("xdisplay-event-loop", 2, 0, 0, xdisplay_event_loop);
  scm_c_define_gsubr("xdisplay-quit?", 1, 0, 0, xdisplay_quit);
  scm_c_define_gsubr("xdisplay-quit=", 2, 0, 0, xdisplay_set_quit);
  scm_c_define_gsubr("xdisplay-close", 1, 0, 0, xdisplay_close);
  scm_c_define_gsubr("make-xwindow", 3, 0, 0, make_xwindow);
  scm_c_define_gsubr("xwindow-show", 1, 0, 0, xwindow_show);
  scm_c_define_gsubr("xwindow-title=", 2, 0, 0, xwindow_title);
  scm_c_define_gsubr("xwindow-resize", 3, 0, 0, xwindow_resize);
  scm_c_define_gsubr("xwindow-write", 2, 0, 0, xwindow_write);
  scm_c_define_gsubr("xwindow-hide", 1, 0, 0, xwindow_hide);
  scm_c_define_gsubr("xwindow-close", 1, 0, 0, xwindow_close);
}
