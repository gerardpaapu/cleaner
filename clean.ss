#lang scheme
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
  (string->bytes/latin-1 (format "&#~A;" n)))

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

;; -> number?
;; Read a single character and write a clean version
;; returns the number of bytes consumed
(define (clean)
  (define ch (read-byte))
  (unless (eof-object? ch)
         (cond ;; print newlines and horizontal tabs
               [(or (= ch #x09)
                    (= ch #x0A)) (write-byte ch)]
               
               ;; other control characters can be omitted
               ;; as can the Byte Order Mark
               [(or (<= ch #x19)
                    (<= #xFE ch #xFF))]
               
               ;; the BOM in utf-8 may be ef bb ff
               [(and (= ch #xef)
                     (let ([next (peek-bytes 2 0)]
                           [bom (bytes #xbb #xbf)])
                        (bytes=? next bom)))
                (read-bytes 2)]

               ;; ASCII characters are included as is
               [(<= ch #x80) (write-byte ch)]

               ;; these ranges indicate a multi-byte
               ;; character and will be converted to
               ;; XML entities like &#xNNNN;
               [(<= #xC2 ch #xDF)
                (write-bytes (multibyte* ch (read-bytes 1)))]
               
               [(<= #xE0 ch #xEF)
                (write-bytes (multibyte* ch (read-bytes 2)))]
               
               [(<= #xF0 ch #xF4)
                (write-bytes (multibyte* ch (read-bytes 3)))]

               ;; Remove Windows-1252 characters
               [else
                 (write-bytes (fix ch))])

         ;; continue until eof
         (clean)))


(define (multibyte* head tail)
  (multibyte (bytes-append (make-bytes 1 head) tail)))

;; Return the xml-entity to show the utf-8 character represented by `bytes`
(define (multibyte bytes)
  (with-handlers ([exn? (lambda () (entity 65533))])
    (let* ([str (bytes->string/utf-8 bytes)]
           [char (string-ref str 0)]
           [int (char->integer char)])
      (entity int))))
   
(define (process-stream in out)
  (parameterize ([current-input-port in]
                 [current-output-port out])
     (clean)))

(define (process-file filename suffix)
  (define temp (open-output-bytes))
  (define in-file  (string->path filename))
  (define out-file (string->path (string-append filename
                                                suffix)))
  (printf "writing ~a to ~a..." in-file out-file)
  (with-input-file  in-file (process-stream in-file temp)) 
  (with-output-file out-file (write-bytes (get-output-bytes temp) out-file))
  (printf " DONE!~n"))
