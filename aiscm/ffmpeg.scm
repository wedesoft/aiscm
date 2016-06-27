(define-module (aiscm ffmpeg)
  #:export (open-input-video))
(load-extension "libguile-ffmpeg" "init_ffmpeg")
(define (open-input-video file-name) #f)
