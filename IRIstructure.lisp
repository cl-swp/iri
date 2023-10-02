;;;-*- Mode: common-lisp; syntax: common-lisp; package: iri; base: 10 -*-
;;;
;;;; IRI structure module
;;;
;;; ----------------------------------------------------------------------------------
;;; Copyright (c) 2014-2023 Seiji Koide <koide@ontolonomy.co.jp>
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
;;; ==================================================================================

(cl:provide :iri)

(in-package :iri)

;;;(defstruct (iri (:print-function
;;;                 (lambda (x stream depth)
;;;                   (declare (ignore depth))
;;;                   (format stream "<~A>" (iri-string x)))))
;;;  schema
;;;  hier-part
;;;  query
;;;  fragment
;;;  string)

(defstruct (iri (:print-function
                 (lambda (x stream depth)
                   (declare (ignore depth))
                   (format stream "<~A>" (iri-string x))))
                (:predicate nil))
  colon-pos
  question-pos
  number-sign-pos
  end
  string
  node
  plist
  dtree)

(defgeneric iri-p (object)
  (:documentation "This predicate function returns true if object is an instance of iri.")
  )
(defmethod iri-p ((object cl:string))
  (cond ((char= (char object 0) #\<) (error "check ~S" object))
        (t nil)))
(defmethod iri-p ((object iri:iri))
  (%iri-p (iri-string object)))
;;;#+:Allegro
;;;(defmethod iri-p ((object net.uri:uri))
;;;  (net.uri:uri-p object))
;;;#-:Allegro               ; puri
;;;(defmethod iri-p ((object uri:uri))
;;;  (uri:uri-p object))

;;;
;;; In this implementation, unbound iri-node returns nil.
;;;

(defun iri= (x y)
  "This implementation minds the character-case of letters in iri strings."
  (let ((x-str (iri-string x))
        (y-str (iri-string y)))
    (string= x-str y-str)))

(defun iri-equal (x y)
  "This implementation ignores the different schema 'http' and 'https' and the character-case in iri hier-part."
  (cond ((string= (iri-schema x) (iri-schema y)) (iri= x y))
        ((and (or (string= (iri-schema x) "http") (string= (iri-schema x) "https"))
              (or (string= (iri-schema y) "http") (string= (iri-schema y) "https")))
         (string-equal (iri-hier-part x) (iri-hier-part y)))))

(defun iri (iristr &optional (start 0) (end (length iristr)) (verbose nil))
  (multiple-value-bind (colon-pos question-pos number-sign-pos) 
      (decompose iristr start end verbose)
    (make-iri :colon-pos    colon-pos
              :question-pos question-pos
              :number-sign-pos    number-sign-pos
              :end          end
              :string       (subseq iristr start end))))

(defun iri-schema (iri)
  (subseq (iri-string iri) 0 (iri-colon-pos iri)))

(defun iri-hier-part (iri)
  (subseq (iri-string iri) (1+ (iri-colon-pos iri)) (iri-question-pos iri)))

(defun iri-query (iri)
  (let ((question-pos (iri-question-pos iri))
        (number-sign-pos (iri-number-sign-pos iri)))
    (when (< question-pos number-sign-pos)
      (subseq (iri-string iri) (1+ question-pos) number-sign-pos))))

(defun fragment-exists-p (iri)
  (< (iri-number-sign-pos iri) (iri-end iri)))

(defun iri-fragment (iri)
  (let ((number-sign-pos (iri-number-sign-pos iri))
        (end (iri-end iri)))
    (when (< number-sign-pos end)
      (subseq (iri-string iri) (1+ number-sign-pos) end))))

#|
(iri:iri "http://server.ontologies.jp")
(iri:iri "http://server.ontologies.jp/")
(iri:iri "http://server.ontologies.jp/top")
(iri:iri "http://server.ontologies.jp/top/")
(iri:iri "http://server.ontologies.jp/top/middle")
(iri:iri "http://server.ontologies.jp/top/middle/")
(iri:iri "http://server.ontologies.jp/top/middle/bottom")
(iri:iri "http://server.ontologies.jp/top/middle/bottom/")
(iri:iri "http://server.ontologies.jp/top/middle/bottom#")
(iri:iri "http://server.ontologies.jp/top/middle/bottom/#")
(iri:iri "http://server.ontologies.jp/top/middle/bottom#mydata")
(iri:iri "http://server.ontologies.jp/top/middle/bottom/#mydata")
(iri:iri-fragment (iri:iri "http://server.ontologies.jp/top/middle/bottom"))
(iri:iri-fragment (iri:iri "http://server.ontologies.jp/top/middle/bottom/"))
(iri:iri-fragment (iri:iri "http://server.ontologies.jp/top/middle/bottom#"))
(iri:iri-fragment (iri:iri "http://server.ontologies.jp/top/middle/bottom#mydata"))
(iri:iri-fragment (iri:iri "http://server.ontologies.jp/top/middle/bottom/#mydata"))

(iri:iri "https://ja.wikipedia.org/wiki/SMAP×SMAP")

(iri:iri "http://ontologies.jp/jwo/class/:")
(iri:iri-fragment (iri:iri "http://ontologies.jp/jwo/class/:"))
|#

;;;
;;;
;;;

(defun decompose-iri (iri)
  "returns a localname string and a namespace string from <iri>. 
   If no fragment and no pathname, this functions two nils."
  (%decompose-iri (iri:iri-string iri)
                  0
                  (iri:iri-colon-pos iri)
                  (iri:iri-question-pos iri)
                  (iri:iri-number-sign-pos iri)
                  (iri:iri-end iri)))
(defun %decompose-iri (iristr start colon-pos question-pos number-sign-pos end)
  "gets <iri> and retrieves a prefix and a localname from <iri> in a regular form."
  (macrolet ((fragment-exists-p () `(< number-sign-pos end))
             (iri-fragment () `(subseq iristr (1+ number-sign-pos) end))
             (iri-butfragment () `(subseq iristr start (1+ number-sign-pos)))
             (path-file-exists-p () 
                                 `(and (find #\/ iristr :test #'char= :start (+ colon-pos 3))
                                       (char/= #\/ (char iristr (1- (min question-pos number-sign-pos))))))
             (iri-path ()
                       `(let ((start (+ colon-pos 3)) ; skip "//"
                              (end (min question-pos number-sign-pos)))
                          (let ((last-/-pos
                                  (position #\/ iristr :test #'char=
                                            :start start :end end
                                            :from-end t)))
                            ;; skip userinfo, host, and port
                            (when last-/-pos
                              (subseq iristr (1+ last-/-pos) end)))))
             (iri-butpath ()
                          `(let ((last-/-pos
                                  (position #\/ iristr :test #'char=
                                            :start colon-pos
                                            :end (min question-pos number-sign-pos)
                                            :from-end t)))
                             (subseq iristr start (1+ last-/-pos))))
             )
    (cond ((fragment-exists-p)
           (values (iri-fragment) (iri-butfragment)))
          ((path-file-exists-p)                  ; file
           (let ((path (iri-path)))
             (let ((type (pathname-type path)))
               (cond ((member type '("txt" "text" "rdf" "rdfs" "htm" "html") 
                              :test #'string-equal)
                      (values (pathname-name path) (iri-butpath)))
                     (t (values path (iri-butpath)))))))
          (t (values nil nil)))))
#|
(iri:decompose-iri (iri:iri "http://server.ontologies.jp"))  -> nil nil
(iri:decompose-iri (iri:iri "http://server.ontologies.jp/top"))
(iri:decompose-iri (iri:iri "http://server.ontologies.jp/top/middle"))
(iri:decompose-iri (iri:iri "http://server.ontologies.jp/top/middle/"))  -> nil nil
(iri:decompose-iri (iri:iri "http://server.ontologies.jp/top/middle/bottom"))
(iri:decompose-iri (iri:iri "http://server.ontologies.jp/top/middle/bottom/"))  -> nil nil
(iri:decompose-iri (iri:iri "http://server.ontologies.jp/top/middle/bottom#")) -> "" "http://server.ontologies.jp/top/middle/bottom#"
(iri:decompose-iri (iri:iri "http://server.ontologies.jp/top/middle/bottom#mydata"))
(iri:decompose-iri (iri:iri "http://server.ontologies.jp/top/middle/bottom/#mydata"))
(iri:decompose-iri (iri:iri "http://rdf.freebase.com/ns/type.object.name"))
(iri:decompose-iri (iri:iri "http://rdf.freebase.com/ns/type.object.name.rdf"))
(iri:decompose-iri (iri:iri "http://ontologies.jp/jwo/class/:")) -> Error: illegal namestring: ":" [0] in Windows

(iri:decompose-iri (iri:iri "https://ja.wikipedia.org/wiki/SMAP×SMAP"))

|#
;; ----------------------------------------------------------------------------

