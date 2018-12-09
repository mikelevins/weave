;;;; project.lisp
;;;; a directory structure containing the files needed for a wevae
;;;; application to run

(in-package #:weave)

;;; ---------------------------------------------------------------------
;;; known projects
;;; ---------------------------------------------------------------------

(defparameter *weave-projects* (make-hash-table))

(defmethod find-project ((pname symbol))
  (gethash pname *weave-projects* nil))

(defmethod define-project ((pname symbol)(path pathname))
  (assert (uiop/pathname:absolute-pathname-p path) ()
          "Project pathname must be an absolute pathname; instead found: ~S"
          path)
  (setf (gethash pname *weave-projects*)
        path))

(defmethod define-project ((pname symbol)(path string))
  (define-project pname (pathname path)))

;;; ---------------------------------------------------------------------
;;; setting up project paths
;;; ---------------------------------------------------------------------

(defparameter *project-root-directory* nil)
(defparameter *source-directory* #p"src/")
(defparameter *public-directory* #p"public/")
(defparameter *public-css-directory* #p"public/css/")
(defparameter *public-images-directory* #p"public/images/")
(defparameter *public-js-directory* #p"public/js/")

(defmethod project-root-directory ((project-name symbol))
  (find-project project-name))

(defmethod set-project-root ((project-name symbol)(path pathname))
  (define-project project-name path))

(defmethod set-project-root ((project-name symbol)(path string))
  (set-project-root project-name (pathname path)))

(defmethod project-directory ((project-name symbol)(dirname (eql :root)))
  (project-root-directory project-name))

(defmethod project-directory ((project-name symbol)(dirname (eql :public)))
  (merge-pathnames "public/" (project-root-directory project-name)))

(defmethod project-directory ((project-name symbol)(dirname (eql :src)))
  (merge-pathnames "src/" (project-root-directory project-name)))

(defmethod project-directory ((project-name symbol)(dirname (eql :templates)))
  (merge-pathnames "templates/" (project-root-directory project-name)))

(defmethod project-directory ((project-name symbol)(dirname (eql :public/css)))
  (merge-pathnames "public/css/" (project-root-directory project-name)))

(defmethod project-directory ((project-name symbol)(dirname (eql :public/images)))
  (merge-pathnames "public/images/" (project-root-directory project-name)))

(defmethod project-directory ((project-name symbol)(dirname (eql :public/js)))
  (merge-pathnames "public/js/" (project-root-directory project-name)))

;;; (define-project :hello #p"/Users/mikel/weave-projects/hello/")
;;; (find-project :hello)
;;; (find-project :goodbye)
;;; (project-directory :hello :root)
;;; (project-directory :hello :public/js)

;;; error: not an absolute path:
;;; (define-project :hello #p"hello/")

;;; ---------------------------------------------------------------------
;;; creating project directories
;;; ---------------------------------------------------------------------

(defmethod ensure-project-directories ((project-name symbol))
  (assert (find-project project-name)()
          "Project ~S not found" project-name)
  (dolist (dirname (list :root :public :src :templates :public/css :public/images :public/js))
    (let ((dir (project-directory project-name dirname)))
      (ensure-directories-exist dir))))

;;; (ensure-project-directories :hello)
