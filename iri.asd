(asdf:defsystem :iri
  :name "iri"
  :author "Seiji Koide <koide@ontolonomy.co.jp>"
  :version "0.9.0"
  :maintainer "Seiji Koide <koide@ontolonomy.co.jp>"
  :licence "MIT"
  :description "Utilities for IRI strings module"
  :serial t
  :components ((:file "IRIutils")
               (:file "IRIstring")
               (:file "IRIstructure")))
