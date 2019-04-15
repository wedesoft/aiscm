(use-modules (ice-9 ftw) (ice-9 regex) (srfi srfi-26) (aiscm core) (aiscm ffmpeg))

(define file-names (filter (cut string-match "speech-.*\\.mp3" <>) (scandir ".")))
(define chunk 512)

(define data
  (map
    (lambda (file-name)
      (let* [(match (string-match "speech-(.*)-(.*)-(.*)\\.mp3" file-name))
             (word  (match:substring match 2))
             (input (open-ffmpeg-input file-name))
             (count (string->number (match:substring match 3)))
             (n     (/ count chunk)) ]
        (list word (reshape (to-array (read-audio input count)) (list n chunk)))))
    file-names))
