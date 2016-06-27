#include <libguile.h>
#include <libavformat/avformat.h>


static scm_t_bits format_context_tag;

struct format_context_t {
  AVFormatContext *fmt_ctx;
};

SCM format_context_destroy(SCM scm_self)
{
  scm_assert_smob_type(format_context_tag, scm_self);
  struct format_context_t *self = (struct format_context_t *)SCM_SMOB_DATA(scm_self);
  if (self->fmt_ctx) {
    avformat_close_input(&self->fmt_ctx);
    self->fmt_ctx = NULL;
  };
  return SCM_UNSPECIFIED;
}

size_t free_format_context(SCM scm_self)
{
  struct format_context_t *self = (struct format_context_t *)SCM_SMOB_DATA(scm_self);
  format_context_destroy(scm_self);
  scm_gc_free(self, sizeof(struct format_context_t), "format-context");
  return 0;
}

SCM open_format_context(SCM scm_file_name)
{
  SCM retval;
  struct format_context_t *self;
  const char *file_name = scm_to_locale_string(scm_file_name);
  self = (struct format_context_t *)scm_gc_calloc(sizeof(struct format_context_t), "format-context");
  SCM_NEWSMOB(retval, format_context_tag, self);
  if (avformat_open_input(&self->fmt_ctx, file_name, NULL, NULL) < 0) {
    format_context_destroy(retval);
    scm_misc_error("open-format-context", "Error opening video file '~a'", scm_list_1(scm_file_name));
  };
  if (avformat_find_stream_info(self->fmt_ctx, NULL) < 0) {
    format_context_destroy(retval);
    scm_misc_error("open-format-context", "Could not find stream information", SCM_EOL);
  };
  return retval;
}

void init_ffmpeg(void)
{
  format_context_tag = scm_make_smob_type("format-context", sizeof(struct format_context_t));
  scm_set_smob_free(format_context_tag, free_format_context);
  av_register_all();
  scm_c_define_gsubr("open-format-context", 1, 0, 0, open_format_context);
}
