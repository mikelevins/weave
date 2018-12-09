;;;; weave.asd

(asdf:defsystem #:weave
  :description "Describe weave here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:uiop :trivial-open-browser :clack :cl-emb :parenscript :remote-js)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "project")
                                     (:file "weave")))))

;;; (asdf:load-system :weave)
