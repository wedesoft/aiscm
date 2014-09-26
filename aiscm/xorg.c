#include <GL/glu.h>
#include <GL/glx.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <libguile.h>

#ifndef timersub
#define timersub(a, b, result)                       \
  do {                                               \
    (result)->tv_sec = (a)->tv_sec - (b)->tv_sec;    \
    (result)->tv_usec = (a)->tv_usec - (b)->tv_usec; \
    if ((result)->tv_usec < 0) {                     \
      --(result)->tv_sec;                            \
      (result)->tv_usec += 1000000;                  \
    }                                                \
  } while (0)
#endif

static scm_t_bits display_tag;

static scm_t_bits window_tag;

struct display_t {
  Display *display;
  SCM scm_windows;
  char quit;
};

struct window_t {
  struct display_t *display;
  Window window;
  enum {IO_XIMAGE, IO_OPENGL} io;// TODO: IO_XVIDEO
  int width;
  int height;
  Colormap color_map;
  XVisualInfo *visual_info;
  GC gc;
  SCM scm_image;
  SCM scm_converted;
  Atom wm_protocols;
  Atom wm_delete_window;
};

SCM window_destroy(SCM scm_self);

SCM display_destroy(SCM scm_self)
{
  scm_assert_smob_type(display_tag, scm_self);
  struct display_t *self = (struct display_t *)SCM_SMOB_DATA(scm_self);
  while (!scm_is_null_and_not_nil(self->scm_windows))
    window_destroy(scm_car(self->scm_windows));
  if (self->display) {
    XCloseDisplay(self->display);
    self->display = NULL;
  };
  return SCM_UNSPECIFIED;
}

size_t free_display(SCM scm_self)
{
  struct display_t *self = (struct display_t *)SCM_SMOB_DATA(scm_self);
  display_destroy(scm_self);
  scm_gc_free(self, sizeof(struct display_t), "display");
  return 0;
}

SCM make_display(SCM scm_name)
{
  SCM retval;
  struct display_t *self;
  const char *name = scm_to_locale_string(scm_name);
  Display *display = XOpenDisplay(*name == '\0' ? (const char *)NULL : name);
  if (!display) scm_syserror("make_display");
  self = (struct display_t *)scm_gc_calloc(sizeof(struct display_t), "display");
  SCM_NEWSMOB(retval, display_tag, self);
  self->scm_windows = SCM_EOL;
  self->display = display;
  return retval;
}

SCM display_shape(SCM scm_self)
{
  scm_assert_smob_type(display_tag, scm_self);
  struct display_t *self = (struct display_t *)SCM_SMOB_DATA(scm_self);
  int width = DisplayWidth(self->display, DefaultScreen(self->display));
  int height = DisplayHeight(self->display, DefaultScreen(self->display));
  return scm_list_2(scm_from_int(width), scm_from_int(height));
}

static Bool always_true(Display *display, XEvent *event, XPointer pointer)
{
  return True;
}

void window_paint(struct window_t *window);

void handle_event(struct display_t *self, XEvent *event)
{
  SCM scm_windows = self->scm_windows;
  struct window_t *window = NULL;
  while (!scm_is_null_and_not_nil(scm_windows)) {
    struct window_t *w = (struct window_t *)SCM_SMOB_DATA(scm_car(scm_windows));
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
        if (window->io == IO_XIMAGE) window->scm_converted = SCM_UNDEFINED;
        window->width = event->xconfigure.width;
        window->height = event->xconfigure.height;
        window_paint(window);
        break;
      case Expose:
        while (XCheckTypedWindowEvent(self->display, window->window,
                                      Expose, event));
        window_paint(window);
        break;
    };
  };
}

SCM display_process_events(SCM scm_self)
{
  scm_assert_smob_type(display_tag, scm_self);
  XEvent event;
  struct display_t *self = (struct display_t *)SCM_SMOB_DATA(scm_self);
  while (XCheckIfEvent(self->display, &event, always_true, NULL))
    handle_event(self, &event);
  return scm_self;
}

SCM display_event_loop(SCM scm_self, SCM scm_timeout)
{
  scm_assert_smob_type(display_tag, scm_self);
  struct display_t *self = (struct display_t *)SCM_SMOB_DATA(scm_self);
  double timeout = scm_to_double(scm_timeout);
  if (timeout >= 0) {
    struct timeval t0;
    double elapsed;
    gettimeofday(&t0, NULL);
    do {
      display_process_events(scm_self);
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

SCM display_quit(SCM scm_self)
{
  scm_assert_smob_type(display_tag, scm_self);
  struct display_t *self = (struct display_t *)SCM_SMOB_DATA(scm_self);
  return self->quit ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM display_set_quit(SCM scm_self, SCM scm_quit)
{
  scm_assert_smob_type(display_tag, scm_self);
  struct display_t *self = (struct display_t *)SCM_SMOB_DATA(scm_self);
  self->quit = scm_quit != SCM_BOOL_F;
  return display_quit(scm_self);
}

SCM window_destroy(SCM scm_self)
{
  scm_assert_smob_type(window_tag, scm_self);
  struct window_t *self = (struct window_t *)SCM_SMOB_DATA(scm_self);
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
  if (self->display) {
    self->display->scm_windows = scm_delete(scm_self, self->display->scm_windows);
    self->display = NULL;
  };
  return SCM_UNSPECIFIED;
}

size_t free_window(SCM scm_self)
{
  struct window_t *self = (struct window_t *)SCM_SMOB_DATA(scm_self);
  window_destroy(scm_self);
  scm_gc_free(self, sizeof(struct window_t), "window");
  return 0;
}

SCM make_window(SCM scm_display, SCM scm_width, SCM scm_height, SCM scm_io)
{
  SCM retval;
  struct window_t *self;
  struct display_t *display;
  self = (struct window_t *)scm_gc_calloc(sizeof(struct window_t), "window");
  SCM_NEWSMOB(retval, window_tag, self);
  self->scm_image = SCM_UNDEFINED;
  self->scm_converted = SCM_UNDEFINED;
  display = (struct display_t *)SCM_SMOB_DATA(scm_display);
  self->display = display;
  self->io = scm_to_int(scm_io);
  self->width = scm_to_int(scm_width);
  self->height = scm_to_int(scm_height);
  switch (self->io) {
    case IO_XIMAGE:
      self->visual_info = (XVisualInfo *)scm_gc_malloc_pointerless(sizeof(XVisualInfo), "XVisualInfo");
      if (!XMatchVisualInfo(display->display, DefaultScreen(display->display),
                            24, TrueColor, self->visual_info))
        scm_syserror("make_window");
      break;
    case IO_OPENGL: {
      int attributes[] = {GLX_RGBA,
                          GLX_RED_SIZE, 1,
                          GLX_GREEN_SIZE, 1,
                          GLX_BLUE_SIZE, 1,
                          GLX_DEPTH_SIZE, 0, None};
      self->visual_info = glXChooseVisual(display->display, DefaultScreen(display->display),
                                          attributes);
      if (!self->visual_info) scm_syserror("make_window");
      break;}
  };
  self->color_map = XCreateColormap(display->display, DefaultRootWindow(display->display),
                                    self->visual_info->visual, AllocNone);
  if (!self->color_map) scm_syserror("make_window");
  XSetWindowAttributes attributes;
  attributes.colormap = self->color_map;
  attributes.event_mask = KeyPressMask | ExposureMask | StructureNotifyMask;
  self->window = XCreateWindow(display->display, RootWindow(display->display, self->visual_info->screen),
                               0, 0, self->width, self->height,
                               0, self->visual_info->depth, InputOutput, self->visual_info->visual,
                               CWColormap | CWEventMask, &attributes);
  if (!self->window) scm_syserror("make_window");
  XGCValues xgcv;
  self->gc = XCreateGC(display->display, self->window, 0L, &xgcv);
  if (!self->gc) scm_syserror("make_window");
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

SCM window_show(SCM scm_self)
{
  scm_assert_smob_type(window_tag, scm_self);
  XEvent event;
  struct window_t *self = (struct window_t *)SCM_SMOB_DATA(scm_self);
  XMapWindow(self->display->display, self->window);
  XIfEvent(self->display->display, &event, wait_for_notify, (char *)self->window);
  return scm_self;
}

SCM window_title(SCM scm_self, SCM scm_title)
{
  scm_assert_smob_type(window_tag, scm_self);
  struct window_t *self = (struct window_t *)SCM_SMOB_DATA(scm_self);
  XStoreName(self->display->display, self->window, scm_to_locale_string(scm_title));
  return scm_title;
}

SCM window_resize(SCM scm_self, SCM scm_width, SCM scm_height)
{
  scm_assert_smob_type(window_tag, scm_self);
  struct window_t *self = (struct window_t *)SCM_SMOB_DATA(scm_self);
  int
    width = scm_to_int(scm_width),
    height = scm_to_int(scm_height);
  XResizeWindow(self->display->display, self->window, width, height);
  self->width = width;
  self->height = height;
  XFlush(self->display->display);
  return scm_self;
}

SCM window_write(SCM scm_self, SCM scm_image)
{
  scm_assert_smob_type(window_tag, scm_self);
  struct window_t *self = (struct window_t *)SCM_SMOB_DATA(scm_self);
  self->scm_image = scm_image;
  self->scm_converted = SCM_UNDEFINED;
  window_paint(self);
  return scm_image;
}

SCM window_hide(SCM scm_self)
{
  scm_assert_smob_type(window_tag, scm_self);
  XEvent event;
  struct window_t *self = (struct window_t *)SCM_SMOB_DATA(scm_self);
  XUnmapWindow(self->display->display, self->window);
  XIfEvent(self->display->display, &event, wait_for_notify, (char *)self->window);
  return scm_self;
}

static SCM scm_convert;

void gl_error(const char *context)
{
  SCM msg = SCM_EOL;
  while (1) {
    GLenum err = glGetError();
    if (err == GL_NO_ERROR) break;
    msg = scm_cons(scm_from_locale_string(gluErrorString(err)), msg);
  };
  SCM str = scm_list_1(scm_string_join(msg,
                                       scm_from_locale_string(", "),
                                       scm_from_locale_symbol("infix")));
  scm_misc_error(context, "~a", scm_list_1(str));
}

void window_paint(struct window_t *self)
{
  if (!SCM_UNBNDP(self->scm_image)) {
    switch (self->io) {
      case IO_XIMAGE: {
        if (SCM_UNBNDP(self->scm_converted))
          self->scm_converted = scm_call_3(scm_convert,
                                           self->scm_image,
                                           scm_from_locale_symbol("BGRA"),
                                           scm_list_2(scm_from_int(self->width),
                                                      scm_from_int(self->height)));
        char *data = scm_to_pointer(scm_slot_ref(self->scm_converted, scm_from_locale_symbol("data")));
        XImage *img = XCreateImage(self->display->display, self->visual_info->visual,
                                   24, ZPixmap, 0, data, self->width, self->height,
                                   32, self->width * 4);
        if (!img) scm_syserror("window_paint");
        img->byte_order = LSBFirst;
        XPutImage(self->display->display, self->window, self->gc,
                  img, 0, 0, 0, 0, self->width, self->height);
        img->data = (char *)NULL;
        XDestroyImage(img);}
      case IO_OPENGL: {
        if (SCM_UNBNDP(self->scm_converted))
          self->scm_converted = scm_call_2(scm_convert,
                                           self->scm_image,
                                           scm_from_locale_symbol("RGB"));
        GLXContext context =
          glXCreateContext(self->display->display,
                           self->visual_info, 0, GL_TRUE);
        if (!context) gl_error("window_paint");
        if (!glXMakeCurrent(self->display->display, self->window, context)) gl_error("window_paint");
        glLoadIdentity();
        glViewport(0, 0, self->width, self->height);
        glOrtho(0, self->width, self->height, 0, -1.0, 1.0);
        glDisable(GL_DITHER);
        glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
        glRasterPos2i(0, 0);
        SCM shape = scm_slot_ref(self->scm_converted, scm_from_locale_symbol("shape"));
        int
          width = scm_to_int(scm_car(shape)),
          height = scm_to_int(scm_cadr(shape));
        char *data = scm_to_pointer(scm_slot_ref(self->scm_converted, scm_from_locale_symbol("data")));
        glPixelZoom((float)self->width / width, -(float)self->height / height);
        // TODO: fast rendering of grayscale images
        glDrawPixels(width, height, GL_RGB, GL_UNSIGNED_BYTE, data);
        glEnable(GL_DITHER);
        glFinish();
        glXDestroyContext(self->display->display, context);
      };
    };
  };
}

void init_xorg(void)
{
  scm_convert = scm_c_public_ref("aiscm image", "convert");
  display_tag = scm_make_smob_type("display", sizeof(struct display_t));
  window_tag = scm_make_smob_type("window", sizeof(struct window_t));
  scm_set_smob_free(display_tag, free_display);
  scm_set_smob_free(window_tag, free_window);
  scm_c_define("IO-XIMAGE" ,scm_from_int(IO_XIMAGE));
  scm_c_define("IO-OPENGL" ,scm_from_int(IO_OPENGL));
  scm_c_define_gsubr("make-display", 1, 0, 0, make_display);
  scm_c_define_gsubr("display-shape", 1, 0, 0, display_shape);
  scm_c_define_gsubr("display-process-events", 1, 0, 0, display_process_events);
  scm_c_define_gsubr("display-event-loop", 2, 0, 0, display_event_loop);
  scm_c_define_gsubr("display-quit?", 1, 0, 0, display_quit);
  scm_c_define_gsubr("display-quit=", 2, 0, 0, display_set_quit);
  scm_c_define_gsubr("display-destroy", 1, 0, 0, display_destroy);
  scm_c_define_gsubr("make-window", 4, 0, 0, make_window);
  scm_c_define_gsubr("window-show", 1, 0, 0, window_show);
  scm_c_define_gsubr("window-title=", 2, 0, 0, window_title);
  scm_c_define_gsubr("window-resize", 3, 0, 0, window_resize);
  scm_c_define_gsubr("window-write", 2, 0, 0, window_write);
  scm_c_define_gsubr("window-hide", 1, 0, 0, window_hide);
  scm_c_define_gsubr("window-destroy", 1, 0, 0, window_destroy);
}
