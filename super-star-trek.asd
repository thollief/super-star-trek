;; sbcl --eval '(asdf:load-system :super-star-trek)'
;; (asdf:operate 'asdf:program-op :super-star-trek)

(asdf:defsystem "super-star-trek"
    :description "super-star-trek: re-implementation of Super Star Trek in Lisp"
    :version "0.0.1"
    :author "multiple"
    :licence "non-commercial, BSD"
    :depends-on ("cl-charms" "cl-utilities")
    :components ((:file "packages")
                 (:file "sbcl-support")
                 (:file "events")
                 (:file "help")
                 (:file "super-star-trek")
                 )
    :build-operation "asdf:program-op"
    :build-pathname "sst"
    :entry-point "sst:sst")
