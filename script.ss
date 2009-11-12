#lang scheme
(require "clean.ss")

(define suffix (make-parameter ".clean"))

(define (main . args)
  (cond [(null? args) (process-stream (current-input-port)
                                      (current-output-port))]
        [else (for-each (lambda (str) (process-file str (suffix))) args)]))

(command-line
 #:program "cleaner"
 #:once-each
 [("-s" "--suffix") s "append a suffix to output files"
  (suffix s)]
 #:args filenames
 (apply main filenames))