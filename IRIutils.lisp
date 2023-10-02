;;;-*- Mode: common-lisp; syntax: common-lisp; package: iri; base: 10 -*-
;;;
;;;; Utilities for IRI strings module
;;;
;;; This module is an implementation of Internationalized Resource Identifies 
;;; (IRIs). The IRI is specified by RFC3987, see 
;;; http://www.ietf.org/rfc/rfc3987.txt or http://tools.ietf.org/html/rfc3987.
;;; The URL is currently specified by RFC3986, and it is still referenciable.
;;; ----------------------------------------------------------------------------------
;;; Copyright (c) 2014-2019 Seiji Koide <koide@ontolonomy.co.jp>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this 
;;; software and associated documentation files (the "Software"), to deal in the Software 
;;; without restriction, including without limitation the rights to use, copy, modify, 
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to 
;;; permit persons to whom the Software is furnished to do so, subject to the following 
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies 
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, 
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE 
;;; USE OR OTHER DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------------
;; History
;; 2015/02/09    ucschar-code-p, gen-delims-char-p bug fixed, iunreserved-code-p 
;;               and iunreserved-char-p fixed.
;; 2013/01/09    File created.
;;; ==================================================================================

(cl:defpackage :iri
  (:use :common-lisp )
  (:export #:alpha-p #:ucschar-p #:unreserved-char-p #:iunreserved-char-p
           #:decompose #:*in-directive*
           #:iri #:iri-p #:iri-colon-pos #:iri-question-pos #:iri-number-sign-pos 
           #:iri-end #:iri-string #:iri-node #:iri-form #:iri= #:iri-equal 
           #:iri-hier-part #:decompose-iri #:fragment-exists-p #:iri-fragment 
           #:iri-plist #:iri-dtree #:iri= 
           #:angle-bracket-reader)
)

(in-package :iri)

;;;
;;; Characters in IRIs
;;;
;;; An IRI string is a sequence of characters from the Unicode/ISO_10646.
;;;

(defmacro digit-code-p (code)
  `(<= #x30 ,code #x39))
(defmacro digit-p (char)
  `(char<= #\0 ,char #\9))

(defmacro hexdig-code-p (code)
  `(or (<= #x30 ,code #x39)
       (<= #x41 ,code #x46)
       (<= #x61 ,code #x66)))
(defmacro hexdig-char-p (char)
  `(or (char<= #\0 ,char #\9)
       (char<= #\A ,char #\F)
       (char<= #\a ,char #\f)))

(defmacro upper-alpha-code-p (code)
  `(<= #x41 ,code #x5A))
(defmacro upper-alpha-p (char)
  `(char<= #\A ,char #\Z))

(defmacro lower-alpha-code-p (code)
  `(<= #x61 ,code #x7A))
(defmacro lower-alpha-p (char)
  `(char<= #\a ,char #\z))

(defmacro alpha-code-p (code)
  `(or (lower-alpha-code-p ,code)
       (upper-alpha-code-p ,code)))
(defmacro alpha-p (char)
  `(or (lower-alpha-p ,char)
       (upper-alpha-p ,char)))

(defmacro ucschar-code-p (code)
  "ucschar        = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
                  / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
                  / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
                  / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
                  / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
                  / %xD0000-DFFFD / %xE1000-EFFFD"
  `(or (<=   #xA0 ,code #xD7FF)
       (<= #xF900 ,code #xFDCF)
       (<= #xFDF0 ,code #xFFEF)
       (and (>= ,code #x10000)
            (or (<= #x10000 ,code #x1FFFD)
                (<= #x20000 ,code #x2FFFD)
                (<= #x30000 ,code #x3FFFD)
                (<= #x40000 ,code #x4FFFD)
                (<= #x50000 ,code #x5FFFD)
                (<= #x60000 ,code #x6FFFD)
                (<= #x70000 ,code #x7FFFD)
                (<= #x80000 ,code #x8FFFD)
                (<= #x90000 ,code #x9FFFD)
                (<= #xA0000 ,code #xAFFFD)
                (<= #xB0000 ,code #xBFFFD)
                (<= #xC0000 ,code #xCFFFD)
                (<= #xD0000 ,code #xDFFFD)
                (<= #xE1000 ,code #xEFFFD)))))
(defmacro ucschar-p (char)
  `(let ((_code (char-code ,char)))
     (ucschar-code-p _code)))
            
(defmacro plus-code-p (code)
  `(= ,code #x2B))
(defmacro plus-char-p (char)
  `(char= ,char #\+))

(defmacro minus-code-p (code)
  `(= ,code #x2D))
(defmacro minus-char-p (char)
  `(char= ,char #\-))

(defmacro period-code-p (code)
  `(= ,code #x2E))
(defmacro period-char-p (char)
  `(char= ,char #\.))

(defmacro tilde-code-p (code)
  `(= ,code #x7E))
(defmacro tilde-char-p (char)
  `(char= ,char #\~))

(defmacro underscore-code-p (code)
  `(= ,code #x5F))
(defmacro underscore-char-p (char)
  `(char= ,char #\_))

(defmacro slash-code-p (code)
  `(= ,code #x2F))
(defmacro slash-char-p (char)
  `(char= ,char #\/))

(defmacro question-code-p (code)
  `(= ,code #x3F))
(defmacro question-char-p (char)
  `(char= ,char #\?))

(defmacro colon-code-p (code)
  `(= ,code #x3A))
(defmacro colon-char-p (char)
  `(char= ,char #\:))

(defmacro atmark-code-p (code)
  `(= ,code #x40))
(defmacro atmark-char-p (char)
  `(char= ,char #\@))

(defmacro percent-code-p (code)
  `(= ,code #x25))
(defmacro percent-char-p (char)
  `(char= ,char #\%))

;;;
;;; IRI dependent macros
;;;

(defmacro unreserved-code-p (code)
  "unreserved  = ALPHA / DIGIT / '-' / '.' / '_' / '~' for URL."
  `(or (alpha-code-p ,code)
       (digit-code-p ,code)
       (minus-code-p ,code)
       (period-code-p ,code)
       (underscore-code-p ,code)
       (tilde-code-p ,code)))
(defmacro unreserved-char-p (char)
  `(let ((_code (char-code ,char)))
     (unreserved-code-p _code)))

(defmacro iunreserved-code-p (code)
   "iunreserved := ALPHA / DIGIT / '-' / '.' / '_' / '~' / ucschar"
  `(or (alpha-code-p ,code)
       (digit-code-p ,code)
       (minus-code-p ,code)
       (period-code-p ,code)
       (underscore-code-p ,code)
       (tilde-code-p ,code)
       (ucschar-code-p ,code)))
(defmacro iunreserved-char-p (char)
  `(or (alpha-p ,char)
       (digit-p ,char)
       (char= ,char #\-)
       (char= ,char #\.)
       (char= ,char #\_)
       (char= ,char #\~)
       (ucschar-p ,char)))

(defparameter *gen-delims-characters* ":/?#[]@")
(defparameter *sub-delims-characters* "!$&'()*+,;=")

(defmacro gen-delims-char-p (char)
  `(find ,char *gen-delims-characters*))
(defmacro sub-delims-char-p (char)
  `(find ,char *sub-delims-characters* :test #'char=))

;;;
;;; Prefix "i" means iri specific.
;;;

(defmacro iprivate-code-p (code)
  "iprivate := %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD"
  `(or (<=   #xE000 ,code #xF8FF)
       (<=  #xF0000 ,code #xFFFFD)
       (<= #x100000 ,code #x10FFFD)))
(defmacro iprivate-char-p (char)
  `(iprivate-code-p (char-code ,char)))

;;;
;;; Note that pct-encoding requires a special treatment for parsing, because it 
;;; is composed of three letters, i.e., '%'+2hex, and it must be treated as one 
;;; char in semantics.
;;;

(defun pct-encoded-p (str start &optional (end (length str)))
  "tests <str> sequence starts with '%' and trails two HEXDIG."
  (when (>= start end) (return-from pct-encoded-p (values nil start)))
  (unless (percent-char-p (char str start))
    (return-from pct-encoded-p (values nil start)))
  (when (>= (1+ start) end) (return-from pct-encoded-p (values nil (1+ start))))
  (unless (hexdig-char-p (char str (1+ start)))
    (return-from pct-encoded-p (values nil (1+ start))))
  (unless (hexdig-char-p (char str (+ start 2)))
    (return-from pct-encoded-p (values nil (+ start 2))))
  (values t (+ start 3)))

(defun ipchar-p (str start)
  ;; ipchar := <iunreserved> | <pct-encoded> | <sub-delims> | ":" | "@"
  (let ((ch (char str start)))
    (cond ((or (iunreserved-char-p ch) (sub-delims-char-p ch) 
               (colon-char-p ch) (atmark-char-p ch))
           (values t (1+ start)))
          ((percent-char-p ch)
           (multiple-value-bind (pct-encoded pos) (pct-encoded-p str start)
               (if pct-encoded (values t pos)
                 (values nil start))))
          (t (values nil start)))))

(defun isegment-p (str start &optional (end (length str)))
  ;; isegment := ipchar*
  (multiple-value-bind (ipchar pos) (ipchar-p str start)
    (unless ipchar (return-from isegment-p (values nil pos)))
    (loop while (and (< pos end)
                     (multiple-value-setq (ipchar pos) (ipchar-p str pos)))
        finally (return-from isegment-p (values t pos)))))

(defun dec-octet-p (str start &optional (end (length str)))
  ;; note that this algorithm causes an error for "0n" and "0nn".
  "dec-octet      = DIGIT                 ; 0-9
                  / %x31-39 DIGIT         ; 10-99
                  / '1' 2DIGIT            ; 100-199
                  / '2' %x30-34 DIGIT     ; 200-249
                  / '25' %x30-35          ; 250-255"
  (let (1stdigit 2nddigit 3rddigit)
    (when (>= start end)
      (return-from dec-octet-p (values nil start)))
    (setq 1stdigit (char str start))
    (when (not (digit-p 1stdigit))
      (return-from dec-octet-p (values nil start)))
    ;; (digit-p 1stdigit)
    (when (or (>= (+ start 1) end)
              (not (digit-p (setq 2nddigit (char str (1+ start))))))
      (return-from dec-octet-p (values t (+ start 1))))             ; 0-9
    ;; (digit-p 1stdigit) (digit-p 2nddigit)
    (when (or (>= (+ start 2) end)
              (not (digit-p (setq 3rddigit (char str (+ start 2))))))
      (if (char= 1stdigit #\0)
          (return-from dec-octet-p (values nil start))
        (return-from dec-octet-p (values t (+ start 2)))))          ; 10-99
    ;; (digit-p 1stdigit) (digit-p 2nddigit) (digit-p 3rddigit)
    (when (and (< (+ start 3) end)
               (digit-p (char str (+ start 3))))
      (return-from dec-octet-p
        (values nil (+ start 3))))  ; 4th digit causes an error.
    (cond ((char= 1stdigit #\1) ; (digit-p 2nddigit) (digit-p 3rddigit)
           (return-from dec-octet-p (values t (+ start 3)))) ; 100-199
          ((char= 1stdigit #\2)
           (if (char<= 2nddigit #\4)
               (return-from dec-octet-p (values t (+ start 3)))     ; 200-249
             (if (char>= 2nddigit #\6)
                 (return-from dec-octet-p (values nil (+ start 1)))
               (if (char<= 3rddigit #\5)
                   (return-from dec-octet-p (values t (+ start 3))) ; 250-255
                 (return-from dec-octet-p (values nil (+ start 2)))))))
          (t (return-from dec-octet-p (values nil start))))))

(defun IPv4address-p (str start &optional (end (length str)))
  "IPv4address    = dec-octet '.' dec-octet '.' dec-octet '.' dec-octet"
  (multiple-value-bind (1st-octet pos) (dec-octet-p str start end)
    (unless 1st-octet (return-from IPv4address-p (values nil pos)))
    (unless (period-char-p (char str pos)) 
      (return-from IPv4address-p (values nil pos)))
    (multiple-value-bind (2nd-octet pos) (dec-octet-p str (1+ pos) end)
      (unless 2nd-octet (return-from IPv4address-p (values nil pos)))
      (unless (period-char-p (char str pos)) 
        (return-from IPv4address-p (values nil pos)))
      (multiple-value-bind (3rd-octet pos) (dec-octet-p str (1+ pos) end)
        (unless 3rd-octet (return-from IPv4address-p (values nil pos)))
        (unless (period-char-p (char str pos)) 
          (return-from IPv4address-p (values nil pos)))
        (multiple-value-bind (4th-octet pos) (dec-octet-p str (1+ pos) end)
          (unless 4th-octet (return-from IPv4address-p (values nil pos)))
          (values t pos))))))
#|
(defun IPv6address-p (str start &optional (end (length str)))
  "IPv6address    =                            6( h16 ':' ) ls32
                  /                       '::' 5( h16 ':' ) ls32
                  / [               h16 ] '::' 4( h16 ':' ) ls32
                  / [ *1( h16 ':' ) h16 ] '::' 3( h16 ':' ) ls32
                  / [ *2( h16 ':' ) h16 ] '::' 2( h16 ':' ) ls32
                  / [ *3( h16 ':' ) h16 ] '::'    h16 ':'   ls32
                  / [ *4( h16 ':' ) h16 ] '::'              ls32
                  / [ *5( h16 ':' ) h16 ] '::'              h16
                  / [ *6( h16 ':' ) h16 ] '::'
   h16            = 1*4HEXDIG
   ls32           = ( h16 ':' h16 ) / IPv4address"
  (let (h16 ls32 pos)
    (flet ((double-colon-p (pos)
                           (and (< (1+ pos) end)
                                (colon-char-p (char str pos))
                                (colon-char-p (char str (1+ pos)))))
           (single-colon-p (pos)
                           (and (< pos end)
                                (colon-char-p (char str pos))
                                (not (colon-char-p (char str (1+ pos))))))
           (h16-p (pos)
                  (setq h16 nil)
                  (unless (and (< pos end) (hexdig-char-p (char str pos))) 
                    (return-from h16-p pos))
                  (setq h16 t)
                  ;; 1HEXDIG
                  (unless (and (< (+ pos 1) end)  (hexdig-char-p (char str (+ pos 1))))
                    (return-from h16-p (+ pos 1)))
                  ;; 2HEXDIG
                  (unless (and (< (+ pos 2) end) (hexdig-char-p (char str (+ pos 2)))) 
                    (return-from h16-p (+ pos 2)))
                  ;; 3HEXDIG
                  (unless (and (< (+ pos 3) (hexdig-char-p (char str (+ pos 3))))) 
                    (return-from h16-p (+ pos 3)))
                  ;; 4HEXDIG
                  (unless (and (< (+ pos 4) (hexdig-char-p (char str (+ pos 4)))))
                    (return-from h16-p (+ pos 4)))
                  ;; or over
                  (setq h16 nil)
                  (+ pos 4)))
      (flet ((how-many-h16 ()
                           (loop for i from 1 to 6
                               do (setq pos (h16-p pos))
                                 (unless h16
                                   (return-from IPv6address-p (values nil pos)))
                                 (when (double-colon-p pos) 
                                   (return-from how-many-h16 i))
                                 (unless (single-colon-p pos)
                                   (return-from IPv6address-p (values nil pos)))
                                 (incf pos)
                               finally (return i)))
             (ls32-p (pos)
                     (let ((ps (h16-p pos)))
                       (if h16 
                           (cond ((colon-char-p (char str ps))
                                  (setq ps (h16-p (1+ ps)))
                                  (cond (h16 (setq ls32 t) ps)
                                        (t (setq ls32 nil) ps)))
                                 (t (setq ls32 nil) ps))
                         (multiple-value-bind (ipv4 ps) (IPv4address-p str pos)
                           (cond (ipv4 (setq ls32 t) ps)
                                 (t (setq ls32 nil) ps)))))))
        (when (double-colon-p start)
          ;; 5(<h16>":") <ls32>
          (setq pos (+ start 2))
          (setq pos (h16-p pos))            ; 1h16
          (unless h16 (return-from IPv6address-p (values nil pos)))
          (unless (single-colon-p pos) (return-from IPv6address-p (values nil pos)))
          (setq pos (h16-p (1+ pos)))       ; 2h16
          (unless h16 (return-from IPv6address-p (values nil pos)))
          (unless (single-colon-p pos) (return-from IPv6address-p (values nil pos)))
          (setq pos (h16-p (1+ pos)))       ; 3h16
          (unless h16 (return-from IPv6address-p (values nil pos)))
          (unless (single-colon-p pos) (return-from IPv6address-p (values nil pos)))
          (setq pos (h16-p (1+ pos)))       ; 4h16
          (unless h16 (return-from IPv6address-p (values nil pos)))
          (unless (single-colon-p pos) (return-from IPv6address-p (values nil pos)))
          (setq pos (h16-p (1+ pos)))       ; 5h16
          (unless h16 (return-from IPv6address-p (values nil pos)))
          (unless (single-colon-p pos) (return-from IPv6address-p (values nil pos)))
          (setq pos (ls32-p (1+ pos)))      ; ls32
          (if ls32 (return-from IPv6address-p (values t pos))
            (return-from IPv6address-p (values nil pos))))
        ;;
        (setq pos start)
        (let ((h16-count (how-many-h16 pos)))
          (cond ((= h16-count 6)
                 (setq pos (ls32-p pos))      ; ls32
                 (if ls32 (return-from IPv6address-p (values t pos))
                   (return-from IPv6address-p (values nil pos))))
                (t (assert (double-colon-p pos))
                   (setq pos (+ pos 2))
                   (cond ((= h16-count 1)
                          ;; 4(<h16>":")<ls32>
                          (setq pos (h16-p pos))            ; 1h16
                          (unless h16
                            (return-from IPv6address-p (values nil pos)))
                          (unless (single-colon-p pos) 
                            (return-from IPv6address-p (values nil pos)))
                          (setq pos (h16-p (1+ pos)))       ; 2h16
                          (unless h16 
                            (return-from IPv6address-p (values nil pos)))
                          (unless (single-colon-p pos) 
                            (return-from IPv6address-p (values nil pos)))
                          (setq pos (h16-p (1+ pos)))       ; 3h16
                          (unless h16 
                            (return-from IPv6address-p (values nil pos)))
                          (unless (single-colon-p pos) 
                            (return-from IPv6address-p (values nil pos)))
                          (setq pos (h16-p (1+ pos)))       ; 4h16
                          (unless h16 
                            (return-from IPv6address-p (values nil pos)))
                          (unless (single-colon-p pos) 
                            (return-from IPv6address-p (values nil pos)))
                          (setq pos (ls32-p (1+ pos)))      ; ls32
                          (if ls32 (return-from IPv6address-p (values t pos))
                            (return-from IPv6address-p (values nil pos))))
                         ((= h16-count 2)
                          )
                         ((= h16-count 3)
                          )
                         ((= h16-count 4)
                          )
                         ((= h16-count 5)
                          )
                         ((= h16-count 6)
                          )
                         ((= h16-count 7)
                          )
                         ))))))))
|#
;;; ----------------------------------------------------------------------------------
