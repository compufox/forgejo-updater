;;;; forgejo-updater.lisp

(in-package #:forgejo-updater)

(declaim (inline user-is-root-p restart-process))

(eval-when (:compile-toplevel)
  (defconstant +forgejo-release-rss+ "https://codeberg.org/forgejo/forgejo/releases.rss"))

(defvar *current-program-version* nil
  "the current running forgejo program's version")

(defvar *verbose* nil)

(opts:define-opts
 (:name :help
  :description "prints this help"
  :short #\h
  :long "help")
 (:name :force
  :description "forces the updater to download the latest version"
  :short #\f
  :long "force")
 (:name :download
  :description "downloads the new Forgejo version"
  :short #\d
  :long "download")
 (:name :download-location
  :description "specify the full file path to download the new release"
  :short #\o
  :long "output"
  :arg-parser #'identity
  :meta-var "FILE")
 (:name :update
  :description "replaces the forgejo binary with new version automatically"
  :short #\u
  :long "update")
 (:name :restart-process
  :description "restarts the forgejo systemd unit after updating"
  :short #\r
  :long "restart")
 (:name :arch
  :description "specifies the arch to download (default: linux-amd64)"
  :long "arch"
  :arg-parse #'identity
  :meta-var "ARCH")
 (:name :restart-command
  :description "pass the command to restart the forgejo process (default: sudo systemctl restart forgejo)"
  :long "restart-command"
  :arg-parser #'identity
  :meta-var "COMMAND")
 (:name :version
  :description "prints the program's version"
  :long "version")
 (:name :verbose
  :description "prints logging info"
  :short #\v
  :long "verbose")
 (:name :force-root
  :description "force binary to run as root"
  :long "run-as-root"))

(defmacro logger (&rest rest)
  `(when *verbose*
     (format t ,@rest)))

(defun parse-links (dom)
  (logger "Parsing releases...~%")
  (loop :for elt :across (clss:select "li > a" dom)
        :when (string= "noopener noreferrer" (plump:attribute elt "rel"))
        :collect `(,(str:trim (plump:text elt)) .
                   ,(str:trim (plump:attribute elt "href")))))

(defun ensure-keyserv-added ()
  (unless (handler-case (uiop:run-program '("gpg" "-k" "EB114F5E6C0DC2BCDD183550A4B61A2DC5923710"))
            (uiop/run-program:subprocess-error (e)
              nil))
    (uiop:run-program '("gpg" "--keyserver" "keys.openpgp.org" "--recv" "EB114F5E6C0DC2BCDD183550A4B61A2DC5923710"))))

(defun verify-file-integrity (signature-path file-path)
  (logger "verifying binary integrity...~%")
  (handler-case
      (uiop:run-program (list "gpg" "--verify" signature-path file-path))
    (uiop:subprocess-error (e)
      (return-from verify-file-integrity nil)))
  t)

(defun download-release (link arch download-location)
  ;; fetch HTML from release page
  ;; cache location of forgejo binary
  ;; parse HTML for specified link & arch
  ;; download correct version to tmp dir
  (let* ((dom (plump:parse (drakma:http-request link :decode-content t)))
         (rel-link (assoc arch (parse-links dom)
                          :test #'(lambda (k v)
                                    (str:ends-with-p k v)))))
    (if rel-link
      (let ((save-path (or download-location 
                           (str:concat "/tmp/" (car (last (str:split "/" link)))))))
        (with-open-file (out save-path
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :supersede
                             :element-type '(unsigned-byte 8))
          (loop :with file := (drakma:http-request (cdr rel-link) :want-stream t)
                :for byte := (read-byte file nil nil)
                :while byte
                :do (write-byte byte out)))
        save-path)
      (format t "Unable to find download link for sepecified arch: ~A~%" arch))))

(defun update-binary (new-release)
  ;; update to user UIOP provided funcs instead of calling
  ;; the command line directly
  (let ((old-path (uiop:run-program "which forgejo" :output '(:string :stripped t))))
    (uiop:run-program (list "chmod" "+x" new-release))

    (logger "Backing up old version to ~A~%" (str:concat old-path ".old"))
    (uiop:run-program (list "mv" old-path (str:concat old-path ".old")))
    (uiop:run-program (list "mv" new-release old-path))))

(defun restart-process (cmd)
  (logger "Restarting service...~%")
  (uiop:run-program cmd))

(defun user-is-root-p ()
  (string= "root" (uiop:run-program "whoami" :output '(:string :stripped t))))

(defun main ()
  "binary entry point"
  (multiple-value-bind (opts args) (opts:get-opts)
    (setf *verbose* (getf opts :verbose))

    (when (getf opts :help)
      (opts:describe :usage-of "fupdater")
      (uiop:quit 0))
    
    (when (getf opts :version)
      (format t "fupdater v~A~&"
              #.(asdf:component-version (asdf:find-system :forgejo-updater)))
      (uiop:quit 0))

    (when (user-is-root-p)
      (if (getf opts :force-root)
          (logger "running as root (derogatory)~%") 
          (progn
            (format t "refusing to run as root user~%")
            (uiop:quit 1))))
    
    (setf *current-program-version*
          (if (getf opts :force)
              ""
              (str:concat "v"
                          (str:replace-all "+" "-"
                                           (nth 3
                                                (str:words
                                                 (uiop:run-program '("forgejo" "--version")
                                                                   :output '(:string :stripped t))))))))

    ;; checks and adds the forgejo signing keys as needed
    (ensure-keyserv-added)
    
    (handler-case 
        (with-user-abort
          (let* ((feed (feedparser:parse-feed (drakma:http-request +forgejo-release-rss+ :decode-content t)))
                 (most-recent (first (gethash :entries feed))))
            (if (string< *current-program-version* (gethash :title most-recent))
              (if (getf opts :download)
                  (let ((path (download-release (gethash :link most-recent)
                                                (getf opts :arch "linux-amd64")
                                                (getf opts :download-location)))
                        (sig (download-release (gethash :link most-recent)
                                               (str:concat (getf opts :arch "linux-amd64") ".asc")
                                               (getf opts :download-location "/tmp/forgejo-update.sig"))))
                    (logger "Found new version!~%")
                    (if path
                        (if (getf opts :update)
                            (if (verify-file-integrity sig path)
                                (progn
                                  (update-binary path)
                                  
                                  (when (getf opts :restart-process)
                                    (restart-process (getf opts :restart-command "sudo systemctl restart forgejo"))))
                                (format t "Unable to verify release, not upgrading.~%"))
                            (format t "Downloaded release ~A to ~A~%" (gethash :title most-recent) path))
                        (format t "Unable to find release for arch ~A~%" (getf opts :arch "linux-amd64"))))
                  (format t "New Forgejo Version Available: ~A~%" (gethash :title most-recent)))
              (logger "No new release found...~%"))))
      
      (user-abort ()
        (format t "~&Quitting...~%")
        (uiop:quit 0))
      
      (error (e)
        (format t "~A~%" e)
        (uiop:quit 1)))))
