#lang scheme
(require mzlib/pregexp)
(provide clean process-stream process-file)

(define-syntax with-input-file
  (syntax-rules ()
    [(_ file body ...)
     (call-with-input-file file (lambda (file) body ...))]))

(define-syntax with-output-file
  (syntax-rules ()
    [(_ file body ...)
     (call-with-output-file file (lambda (file) body ...)
                            #:exists 'replace)]))

;; entity creates an XML unicode entity
;; integer -> bytes
(define (entity n)
  (string->bytes/utf-8 (format "&#~A;" n)))

(define (fix n)
  ;; windows-1252 characters that are illegal in SGML
  ;; converted to their Unicode 'Best Fit' according to
  ;; http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WindowsBestFit/bestfit1252.txt
  (case n
    [(128)  (entity 8364)] ; Euro Sign
    [(130)  (entity 8218)] ; Single Low-9 Quotation Mark
    [(131)  (entity 402) ] ; Latin Small Letter F With Hook
    [(132)  (entity 8222)] ; Double Low-9 Quotation Mark
    [(133)  (entity 8230)] ; Horizontal Ellipsis
    [(134)  (entity 8224)] ; Dagger
    [(135)  (entity 8225)] ; Double Dagger
    [(136)  (entity 710) ] ; Modifier Letter Circumflex Accent
    [(137)  (entity 8240)] ; Per Mille Sign
    [(138)  (entity 352) ] ; Latin Capital Letter S With Caron
    [(139)  (entity 8249)] ; Single Left-Pointing Angle Quotation Mark
    [(140)  (entity 338) ] ; Latin Capital Ligature Oe
    [(142)  (entity 381) ] ; Latin Capital Letter Z With Caron
    [(145)  (entity 8216)] ; Left Single Quotation Mark
    [(146)  (entity 8217)] ; Right Single Quotation Mark
    [(147)  (entity 8220)] ; Left Double Quotation Mark
    [(148)  (entity 8221)] ; Right Double Quotation Mark
    [(149)  (entity 8226)] ; Bullet
    [(150)  (entity 8211)] ; En Dash
    [(151)  (entity 8212)] ; Em Dash
    [(152)  (entity 732) ] ; Small Tilde
    [(153)  (entity 8482)] ; Trade Mark Sign
    [(154)  (entity 353) ] ; Latin Small Letter S With Caron
    [(155)  (entity 8250)] ; Single Right-Pointing Angle Quotation Mark
    [(156)  (entity 339) ] ; Latin Small Ligature Oe
    [(158)  (entity 382) ] ; Latin Small Letter Z With Caron
    [(159)  (entity 376) ] ; Latin Capital Letter Y With Diaeresis
    [else (entity n)]))    ; otherwise make an entity and hope for the best


;; bytes -> bytes
(define (clean str)
  (define (P str acc)
    (define (rest str)
      (subbytes str 1))
    
    (define (multibyte n)
      (with-handlers
       ([exn:fail:contract?
         (lambda (exn)
           ;; if it fails to produce a valid multibyte
           ;; character we make an entity and get on with our lives
           (P (rest str)
              (bytes-append acc (entity (bytes-ref str 0)))))])
       
       (let* ([utf-str (bytes->string/utf-8 str #f 0 n)]
              [ch (char->integer (string-ref utf-str 0))])
         (P (subbytes str n)
            (bytes-append acc (entity ch))))))
    
    (if (= 0 (bytes-length str))
        acc
        (let ([current (bytes-ref str 0)])
          (cond [;; control characters can be omitted
                 ;; as can the Byte Order Mark
                 (or (<= current #x19)
                     (<= #xFE current #xFF)) (P (rest str) acc)]

                ;; ASCII characters are included as is
                [(<= current #x80) (P (rest str) 
                                      (bytes-append acc (bytes current)))]
                
                ;; these ranges indicate a multi-byte
                ;; character and will be converted to
                ;; XML entities -> &#xNNNN;
                [(<= #xC2 current #xDF) (multibyte 2)]
                [(<= #xE0 current #xEF) (multibyte 3)]
                [(<= #xF0 current #xF4) (multibyte 4)]

                ;; Anything else is illegal and will get passed
                ;; through fix before being included
                [else (P (rest str) 
                         (bytes-append acc (fix current)))]))))
  (P str #""))

(define (process-stream in out)
  (do ([line (read-bytes-line in)
             (read-bytes-line in)])
      ((eof-object? line))
    (write-bytes (clean line) out)
    (newline out)))

(define (process-file filename suffix)
  (define temp (open-output-bytes))
  (define in-file  (string->path filename))
  (define out-file (string->path (string-append filename
                                                suffix)))
  (display filename)
  (newline)
  (with-input-file  in-file (process-stream in-file temp)) 
  (with-output-file out-file (write-bytes (get-output-bytes temp) out-file)))