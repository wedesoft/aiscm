(use-modules (aiscm ffmpeg) (aiscm xorg) (aiscm core))
; Creative commons audio-video sync test video https://www.youtube.com/watch?v=GKBKa9Za-FQ
(define video (open-ffmpeg-input "av-sync.mp4"))
(pts= video 15)
(show (read-image video))
