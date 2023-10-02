;;;-*- Mode: common-lisp; syntax: common-lisp; package: iri; base: 10 -*-
;;;
;;;; IRI strings module
;;;
;;; This module is an implementation of Internationalized Resource Identifies 
;;; (IRIs). The IRI is specified by RFC3987, see 
;;; http://www.ietf.org/rfc/rfc3987.txt.
;;; ----------------------------------------------------------------------------------
;;; Copyright (c) 2014-2019 Seiji Koide <koide@ontolonomy.co.jp>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a 
;;; copy of this software and associated documentation files (the "Software"), 
;;; to deal in the Software without restriction, including without limitation 
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense, 
;;; and/or sell copies of the Software, and to permit persons to whom the 
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included 
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------------
;; History
;; 2015/02/09    Percent encoding/decoding
;; 2012/12/30    File created.
;;; ==================================================================================

(in-package :iri)

(defvar *in-directive* nil "True if it is in directives.")

#|
(in-package :iri)
(%iri-p "http://server.ontologies.jp")
(%iri-p "http://server.ontologies.jp/")
(%iri-p "http://server.ontologies.jp/top")
(%iri-p "http://server.ontologies.jp/top/")
(%iri-p "http://server.ontologies.jp/top/middle")
(%iri-p "http://server.ontologies.jp/top/middle/")
(%iri-p "http://server.ontologies.jp/top/middle/bottom")
(%iri-p "http://server.ontologies.jp/top/middle/bottom/")
(%iri-p "http://server.ontologies.jp/top/middle/bottom#")
(%iri-p "http://server.ontologies.jp/top/middle/bottom/#")
(%iri-p "http://server.ontologies.jp/top/middle/bottom#mydata")
(%iri-p "http://server.ontologies.jp/top/middle/bottom/#mydata")

(%iri-p "http://www.wikipediaontology.org/class/\u56FD\u9053200\u53F7\u30D0\u30A4\u30D1\u30B9")
(%iri-p "http://ontologies.jp/jwo/class/:")
|#
#|
(defun iri-ref-p (str &optional (start 0) (end (length str)))
  ;; IRI-reference  := <IRI> | <irelative-ref>
  (or (%iri-p str start end)
      (irelative-ref-p str start end)))

(defun irelative-ref-p (str &optional (start 0) (end (length str)))
  "irelative-ref  = irelative-part [ '?' iquery ] [ '#' ifragment ]"
  (declare (ignore start end))
  )
|#
(defun %iri-p (str &optional (start 0) (end (length str)))
  "IRI := <scheme> ':' <ihier-part>[ '?' <iquery> ] [ '#' <ifragment> ]"
  (multiple-value-bind (valid-scheme next-pos) (scheme-p str start)
    (when (or (not valid-scheme) (>= next-pos (length str)))
      (return-from %iri-p (values nil next-pos)))
    (let ((iquery-start (position #\? str :start next-pos))
          (ifragment-start (position #\# str :start next-pos)))
;;;      (format t "~%iquery-start = ~S~%ifragment-start = ~S" iquery-start ifragment-start)
      (when iquery-start (incf iquery-start))
      (when ifragment-start (incf ifragment-start))
      (multiple-value-bind (valid-ihier nex-pos)
          (ihier-part-p str next-pos (or iquery-start ifragment-start end))
        (unless valid-ihier (return-from %iri-p (values nil nex-pos)))
        (cond ((and iquery-start ifragment-start)
               (error "Not Yet!"))
              (iquery-start (iquery-p str iquery-start end))
              ((and ifragment-start (< ifragment-start end))
               (ifragment-p str ifragment-start end))
              (t ;; valid
               (values t nex-pos)))))))                                                                                                                                                                                                                                                                                           

(defun ihier-part-p (str start &optional (end (length str)))
  "ihier-part := '//'<iauthority> <ipath-abempty> | <ipath-absolute> | <ipath-rootless> | <ipath-empty>"
  (unless (char= (char str start) #\/)
    (return-from ihier-part-p (values nil start)))
  (let ((pos (1+ start)))
    (unless (char= (char str pos) #\/)
      (return-from ihier-part-p (values nil pos)))
    (let (iauthority (pos (1+ pos)))
      (multiple-value-setq (iauthority pos) (iauthority-p str pos end))
      (when iauthority
        (when (>= pos end) (return-from ihier-part-p (values t pos)))
        (let (ipath-abempty)
          (multiple-value-setq (ipath-abempty pos) (ipath-abempty-p str pos end))
          (when ipath-abempty (return-from ihier-part-p (values t pos))))))
    (let (ipath-absolute (pos (1+ pos)))
      (multiple-value-setq (ipath-absolute pos) (ipath-absolute-p str pos end))
      (when ipath-absolute (return-from ihier-part-p (values t pos))))
    (let (ipath-rootless (pos (1+ pos)))
      (multiple-value-setq (ipath-rootless pos) (ipath-rootless-p str pos end))
      (when ipath-rootless (return-from ihier-part-p (values t pos))))
    ;; otherwise ipath-empty
    (values t pos)
    ))

(defun iauthority-p (str start &optional (end (length str)))
  "iauthority := [ <iuserinfo>'@' ]<ihost>[ ':' <port> ]"
  (let ((atpos (position #\@ str :start start :end end :test #'char=)))
    (when atpos
      (multiple-value-bind (iuserinfo pos) (iuserinfo-p str start atpos)
        (unless iuserinfo (return-from iauthority-p (values nil pos)))
        (unless (= pos atpos) (return-from iauthority-p (values nil pos)))))
    (multiple-value-bind (ihost pos) (ihost-p str (if atpos (1+ atpos) start) end)
      (unless ihost (return-from iauthority-p (values nil pos)))
      (when (and (< (1+ pos) end) (char= (char str (1+ pos)) #\:))
        (incf pos)
        (multiple-value-bind (port? ps) (port-p str pos end)
          (declare (ignore port?))
          (values t ps))))))
#|
(iri::iauthority-p "seiji:12345@ontolonomy.co.jp:8990" 0)
|#
(defun iuserinfo-p (str start &optional (end (length str)))
  "iuserinfo  := ( <iunreserved> | <pct-encoded> | <sub-delims> | ':' )*"
  (let ((pos start))
    (flet ((%iuserinfo-char-p (char) 
                              (cond ((or (iunreserved-char-p char)
                                         (sub-delims-char-p char)
                                         (char= char #\:))
                                     (incf pos)
                                     t)
                                    ((pct-encoded-p str pos)))))
      (unless (%iuserinfo-char-p (char str start)) 
        (return-from iuserinfo-p (values nil start)))
      (loop for i from pos to (1- end)
          while (%iuserinfo-char-p (char str i))
          finally (return (values t i))))))
#|
(iri::iuserinfo-p "seiji:12345" 0)
|#

(defun ihost-p (str start &optional (end (length str)))
  "ihost := <IP-literal> | <IPv4address> | <ireg-name>"
  (multiple-value-bind (IP-literal pos) (IP-literal-p str start end)
    (when IP-literal (return-from ihost-p (values t pos))))
  (multiple-value-bind (IPv4address pos) (IPv4address-p str start end)
    (when IPv4address (return-from ihost-p (values t pos))))
  (multiple-value-bind (ireg-name pos) (ireg-name-p str start end)
    (when ireg-name (return-from ihost-p (values t pos))))
  (values nil start))

(defun IP-literal-p (str start &optional (end (length str)))
  "IP-literal := '[' ( IPv6address / IPvFuture  ) ']'"
  (unless (char= (char str start) #\])
    (return-from IP-literal-p (values nil start)))
;;;  (multiple-value-bind (IPv6address pos) (IPv6address-p str (1+ start) end)
;;;    (when IPv6address (return-from IP-literal-p (values t pos))))
  (multiple-value-bind (IPvFuture pos) (IPvFuture-p str (1+ start) end)
    (when IPvFuture (return-from IP-literal-p (values t pos))))
  (values nil start))

(defun port-p (str start &optional (end (length str)))
  "port := DIGIT*"
  (unless (digit-p (char str start)) (return-from port-p (values nil start)))
  (loop for pos from start to (1- end)
      while (digit-p (char str pos))
      finally (return (values t pos))))

(defun ipath-abempty-p (str start &optional (end (length str)))
  "ipath-abempty  := ( '/' isegment )*"
  (unless (char= (char str start) #\/)
    (return-from ipath-abempty-p (values nil start)))
  (loop for pos from start to (1- end)
      while (char= (char str pos) #\/)
      do (setq pos (nth-value 1 (isegment-p str (1+ pos) end)))
      finally (return (values t pos))))

(defun ipath-absolute-p (str start &optional (end (length str)))
  "ipath-absolute := '/' [ isegment-nz ( '/' isegment )* ]"
  ;; isegment       = *ipchar      this allows empty isegment
  ;; isegment-nz    = 1*ipchar
  (unless (char= (char str start) #\/)
    (return-from ipath-absolute-p (values nil start)))
  (ipath-rootless-p str (1+ start) end))

(defun ipath-rootless-p (str start &optional (end (length str)))
  "ipath-rootless = isegment-nz ( '/' isegment )*"
  (multiple-value-bind (isegment pos) (isegment-p str start end)
    (unless isegment (return-from ipath-rootless-p (values nil start)))
    (loop while (and (< pos end) (char= (char str pos) #\/))
        do (when (>= (1+ pos) end)
             (return-from ipath-rootless-p (values t (1+ pos))))
          (multiple-value-setq (isegment pos) (isegment-p str (1+ pos) end))
        finally (return (values t pos)))))

#|
(iri::ipath-rootless-p "top/middle/botom" 0)
|#

(defun scheme-p (str start)
  "returns true and ihier-part start position, if substring of <str> from 
   <start> includes a colon, and the substring by <start> and the colon position 
   may be a scheme of iri. Otherwise returns nil and the position of illegal 
   character in <str> or length of <str> if exhausted."
  (declare (type fixnum start)
           (type string str))
  (let ((first-code (char-code (char str start))))
    (unless (or (<= 65 first-code 90) (<= 97 first-code 122))
      (return-from scheme-p (values nil start))))
  (loop for pos from (1+ start) to (1- (length str))
      for code fixnum = (char-code (char str pos))
      do (when (= code 58)                         ; ':'
           (return-from scheme-p (values t (1+ pos))))
        (unless
             (or (alpha-code-p code)
                 (digit-code-p code)
                 (plus-code-p code)                ; '+'
                 (minus-code-p code)               ; '-'
                 (period-code-p code))             ; '.'
             (return-from scheme-p (values nil pos)))
        finally (return-from scheme-p (values nil pos))))

;; irelative-ref  := <irelative-part>["?"<iquery>]["#"<ifragment>]
;; irelative-part := "//"<iauthority> <ipath-abempty> | <ipath-absolute>

(defun ireg-name-p (str start end)
  "ireg-name := ( iunreserved / pct-encoded / sub-delims )*"
  (loop for pos from start to (1- end)
      for ch = (char str pos)
      do (cond ((iunreserved-char-p ch))
               ((sub-delims-char-p ch))
               ((percent-char-p ch)
                (multiple-value-bind (pct-encoded pos1) 
                    (pct-encoded-p str pos end)
                  (if pct-encoded (setq pos 3)
                    (return-from ireg-name-p (values nil pos1)))))
               (t (return-from ireg-name-p (values nil pos))))
      finally (return-from ireg-name-p (values t pos))))

(defun IPvFuture-p (str start &optional (end (length str)))
  (declare (ignore end))
  ;; "v"<HEXDIG>+"."(<unreserved> | <sub-delims> | ":")+
  (unless (char= (char str start) #\v)
    (return-from IPvFuture-p (values nil start)))
  (unless (hexdig-char-p (char str (1+ start)))
    (return-from IPvFuture-p (values nil (1+ start))))
  (let ((pos (+ start 2)))
    (loop while (hexdig-char-p (char str pos))
        do (incf pos))
    (unless (period-char-p (char str pos)) 
      (return-from IPvFuture-p (values nil pos)))
    (incf pos)
;;;    (unless xxxx)
    ))

(defun iquery-p (str start &optional (end (length str)))
  ;; (ipchar | iprivate | "/" | "?")*
  (when (>= start end) (return-from iquery-p (values nil start)))
  (multiple-value-bind (ipchar pos) (ipchar-p str start)
    (if ipchar (%iquery-p str pos end)
      (let ((ch (char str start)))
        (if (or (iprivate-char-p ch) (slash-char-p ch) (question-char-p ch))
            (%iquery-p str (1+ start) end)
          (values nil start))))))
(defun %iquery-p (str pos end)
  (when (>= pos end) (return-from %iquery-p (values t pos)))
  (multiple-value-bind (ipchar pos1) (ipchar-p str pos)
    (if ipchar (%iquery-p str pos1 end)
      (let ((ch (char str pos)))
        (if (or (iprivate-char-p ch) (slash-char-p ch) (question-char-p ch))
            (%iquery-p str (1+ pos) end)
          (values t pos))))))

(defun ifragment-p (str start &optional (end (length str)))
  ;; (ipchar | "/" | "?")*
  (when (>= start end) (return-from ifragment-p (values nil start)))
  (multiple-value-bind (ipchar pos) (ipchar-p str start)
    (if ipchar (%ifragment-p str pos end)
      (let ((ch (char str start)))
        (if (or (slash-char-p ch) (question-char-p ch))
            (%ifragment-p str (1+ start) end)
          (values nil start))))))
(defun %ifragment-p (str pos end)
  (when (>= pos end) (return-from %ifragment-p (values t pos)))
  (multiple-value-bind (ipchar pos1) (ipchar-p str pos)
    (if ipchar (%ifragment-p str pos1 end)
      (let ((ch (char str pos)))
        (if (or (slash-char-p ch) (question-char-p ch))
            (%ifragment-p str (1+ pos) end)
          (values t pos))))))

;;;
;;;; IRI Decomposition
;;;

(defun decompose (iristr &optional (start 0) (end (length iristr)) (verbose nil))
  "returns three values of colon, question, and number-sign positions in <iristr>.
   schema = subseq(start, colon-pos), hier-part = subseq(colon-pos + 1, question-pos), 
   query = subseq(question-pos + 1, number-sign-pos), fragment = subseq(number-sign-pos + 1, end)"
  (let ((last-pos (1- end))
        (colon-pos (position #\: iristr :test #'char= :start start :end end)))
    (let ((?-pos (position #\? iristr :test #'char= :start (1+ colon-pos) :end end)))
      (when (and ?-pos (= ?-pos (1+ colon-pos)))
        (error "No hier part in IRI: ~A." iristr))
      (when (and ?-pos (= ?-pos last-pos))
        (error "? found but no query in IRI: ~A." iristr))
      (let ((number-sign-pos
             (position #\# iristr :test #'char=
                       :start (or (and ?-pos (1+ ?-pos)) colon-pos)
                       :end end)))
        (when (and (not *in-directive*) verbose number-sign-pos (= number-sign-pos last-pos))
          (warn "# found but no fragment in IRI: ~A." iristr))
        (cond ((and ?-pos number-sign-pos)
               (values colon-pos ?-pos number-sign-pos))
              (?-pos
               ;; query but fragment
               (values colon-pos ?-pos end))
              (number-sign-pos
               ;; fragment but query
               (values colon-pos number-sign-pos number-sign-pos))
              (t (values colon-pos end end)))))))

;;;   ihier-part     = "//" iauthority ipath-abempty
;;;                  / ipath-absolute
;;;                  / ipath-rootless
;;;                  / ipath-empty
;;;   iauthority     = [ iuserinfo "@" ] ihost [ ":" port ]
;;;   ipath-abempty  = *( "/" isegment )

(defun get-iri-ipath (iristr &optional (start 0) (end (length iristr)))
  (multiple-value-bind (colon-pos ?-pos number-sign-pos) 
      (decompose iristr start end)
    (declare (ignore number-sign-pos))
    (assert (char= (char iristr (1+ colon-pos)) #\/))
    (cond ((char= (char iristr (+ colon-pos 2)) #\/)    ; "//"
           (subseq iristr (+ colon-pos 3) ?-pos))
          (t (subseq iristr (+ colon-pos 2) ?-pos)))))

;;;
;;;; Percent Encoding/Decoding
;;;
;;; In this part, the coding reflects RFC 3987 Chanpter 3.
;;; However, we assume that IRI strings are already normarized with UTF-8 in NFC.
;;; 日本語ファイル.html ->
;;; %E6%97%A5%E6%9C%AC%E8%AA%9E%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB.html
;;;
;;; Note that MacOS is problematic in use of UTF-8. The standard of UTF-8 is NFD 
;;; and not NFC in MacOS. It causes no matching to Japanese strings encoded 
;;; in NFC, specifically in case that strings which include Japanese 
;;; dakuten (https://ja.wikipedia.org/wiki/%E6%BF%81%E7%82%B9) and 
;;; han-dakuten (https://ja.wikipedia.org/wiki/%E5%8D%8A%E6%BF%81%E7%82%B9).

(defun %encode-to-octets (str &optional (start 0) (end (length str)))
  (coerce (loop for pos from start to (1- end) by 3
              collect (parse-integer str :start (1+ pos) :end (+ pos 3) :radix 16))
          '(array (unsigned-byte 8) (*))))

(defun octets-%encode (octets)
  (nstring-upcase
   (format nil "~{%~X~}"
     (delete 0 (coerce octets 'cl:list)))))

(defun string-%decode (str &optional (start 0) (end (length str)))
  "transforms a percent decoded string <str> to UTF-8 string."
  (labels ((decode-str (start end)
                       (if (>= start end) ""
                         (let ((non-pct-encoded-start
                                (loop for pos from start to (1- end) by 3
                                    while (pct-encoded-p str pos end)
                                    finally (return pos))))
                           (concatenate 'cl:string
					(#+:allegro excl:octets-to-string #+:sbcl sb-ext:octets-to-string
                              (%encode-to-octets str start non-pct-encoded-start)
                              :external-format :UTF-8)
                             (not-decode-str non-pct-encoded-start end)))))
           (not-decode-str (start end)
                           (if (>= start end) ""
                             (let ((pct-encoded-start
                                    (loop for pos from start to (1- end)
                                        until (pct-encoded-p str pos end)
                                        finally (return pos))))
                               (concatenate 'cl:string
                                 (subseq str start pct-encoded-start)
                                 (decode-str pct-encoded-start end))))))
    (cond ((>= start end) "")
          ((pct-encoded-p str start end)
           (decode-str start end))
          (t (not-decode-str start end)))))

(defun string-%encode (str &optional (start 0) (end (length str)))
  "transforms a UTF-8 string to a percent decoded string ."
  (labels ((encode-str (start end)
                       (if (>= start end) ""
                         (let ((non-uscchar-start
                                (loop for pos from start to (1- end)
                                    while (ucschar-p (char str pos))
                                    finally (return pos))))
                           (concatenate 'cl:string
                             (octets-%encode 
                              (#+:allegro excl:string-to-octets #+:sbcl sb-ext:string-to-octets str
                                                     :start start
                                                     :end non-uscchar-start
                                                     :null-terminate nil
                                                     :external-format :UTF-8))
                             (not-encode-str non-uscchar-start end)))))
           (not-encode-str (start end)
                           (if (>= start end) ""
                             (let ((uscchar-start
                                    (loop for pos from start to (1- end)
                                        until (ucschar-p (char str pos))
                                        finally (return pos))))
                               (concatenate 'cl:string
                                 (subseq str start uscchar-start)
                                 (encode-str uscchar-start end))))))
    (cond ((>= start end) "")
          ((ucschar-p (char str start))
           (encode-str start end))
          (t (not-encode-str start end)))))
#|
(in-package :iri)
(string= "日本語ファイル.html"
         (string-%decode (string-%encode "日本語ファイル.html")))
|#

;; End of module
;;
;; ----------------------------------------------------------------------------

