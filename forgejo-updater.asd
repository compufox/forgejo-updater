;;;; forgejo-updater.asd

(asdf:defsystem #:forgejo-updater
  :description "Describe forgejo-updater here"
  :author "a. fox"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-feedparser #:clss #:drakma #:with-user-abort
               #:plump #:str #:unix-opts)
  :components ((:file "package")
               (:file "forgejo-updater"))
  :build-operation "program-op"
  :build-pathname "bin/fupdater"
  :entry-point "forgejo-updater::main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
