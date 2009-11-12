#lang scheme
(require "clean.ss")

(define (main . args)
  (cond [(null? args) (process-stream (current-input-port)
                                      (current-output-port))]
        [else (for-each (lambda (str) (process-file str ".clean")) args)]))

(apply main (vector->list (current-command-line-arguments)))