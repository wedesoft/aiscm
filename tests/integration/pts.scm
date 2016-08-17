(use-modules (aiscm ffmpeg) (aiscm xorg) (aiscm util))
(define video (open-ffmpeg-input "av-sync.mp4"))
(pts= video 15)
(show (read-video video))
