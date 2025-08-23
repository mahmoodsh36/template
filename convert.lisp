;; this is a hack to gather all latex previews and compile them all at once
;; before exporting. this is to speed things up. one latex conversion commands
;; is faster than running it once for every file independently.
(defun compile-all-latex-previews (rmr)
  (let ((all-snippets)
        ;; snippets-at-once is how many snippets should be compiled at the same time.
        ;; one case in which this is helpful is when there are too many errors (>100)
        ;; and latex refuses to compile all previews at once and exits. so we
        ;; break them into batches, which make it more likely that more latex
        ;; previews will be rendered overall.
        (snippets-at-once 2500))
    (loop for node in (cltpt/roam:roamer-nodes rmr)
          for this-tree = (cltpt/roam:node-text-obj node)
          do (cltpt/base:map-text-object
              this-tree
              (lambda (obj)
                (when (or (typep obj 'cltpt/latex:inline-math)
                          (typep obj 'cltpt/latex:display-math)
                          (typep obj 'cltpt/latex:latex-env))
                  (push (cltpt/base:text-object-text obj) all-snippets)))))
    (loop for i from 0 to (length all-snippets) by snippets-at-once
          do (cltpt/latex:generate-svgs-for-latex
              (subseq all-snippets
                      i
                      (min (+ i snippets-at-once)
                           (length all-snippets)))))))

(defun title-to-filename (title)
  (with-output-to-string (out)
    (loop for char across (string-downcase title)
          do (cond ((or (alphanumericp char)
                        (char= char #\-)
                        (char= char #\.))
                    (write-char char out))
                   ((or (char= char #\space)
                        (char= char #\/)
                        (char= char #\\))
                    (write-char #\_ out))))))

(defvar *my-metadata* nil)
(defvar *blog-dir* "/home/mahmooz/work/blog/")
(defvar *blog-static-dir* "/home/mahmooz/work/blog/static/")
(defvar *template-dir* (truename "~/work/template/"))
(defvar *template-static-dir* (truename "~/work/template/static/"))

(defun generate ()
  (cltpt/base:ensure-directory *blog-dir*)
  (setf cltpt/org-mode::*org-enable-macros* t)
  (setf cltpt:*debug* 1)
  (cltpt/zoo::init)
  (uiop:with-current-directory (*blog-dir*)
    (let* ((other-head-contents
             (uiop:read-file-string
              (uiop:merge-pathnames* *template-dir* "head.html")))
           (other-preamble-contents
             (uiop:read-file-string
              (uiop:merge-pathnames* *template-dir* "preamble.html")))
           (cltpt/html:*html-static-route* "/")
           ;; (cltpt/latex:*latex-previews-cache-directory* #P"./")
           (cltpt/latex:*latex-previews-cache-directory* #P"")
           (*my-metadata*
             (list :other-head-contents other-head-contents
                   :other-preamble-contents other-preamble-contents))
           (cltpt/html:*html-postamble* "</body></html>")
           (cltpt/html:*html-preamble*
             "<!DOCTYPE html>
<html>
<head>
  <meta charset=\"UTF-8\">
  <title> %title </title>
  #(getf cl-user::*my-metadata* :other-head-contents)
</head>
<body>
  #(getf cl-user::*my-metadata* :other-preamble-contents)")
           (rmr (cltpt/roam:from-files
                 '((:path ("/home/mahmooz/brain/notes/")
                    :regex ".*\\.org"
                    :format "org-mode")))))
      (compile-all-latex-previews rmr)
      (cltpt/roam:convert-all
       rmr
       (cltpt/base:text-format-by-name "html")
       "%(identity cl-user::*blog-dir*)%(cl-user::title-to-filename title).html")
      (mapc
       (lambda (item)
         (uiop:copy-file (uiop:merge-pathnames* *template-dir* item)
                         (uiop:merge-pathnames* (truename "~/work/blog/")
                                                item)))
       (list "head.html" "preamble.html" "search.html"))
      (cltpt/base:ensure-directory *blog-static-dir*)
      (mapc
       (lambda (item)
         (uiop:copy-file (uiop:merge-pathnames*
                          (truename *template-static-dir*)
                          item)
                         (uiop:merge-pathnames*
                          (truename *blog-static-dir*)
                          item)))
       (uiop:directory-files *template-static-dir*)))))

;; should place the static file in the dir and return the href to it
(defun export-static-file (filepath)
  (uiop:copy-file
   filepath
   (cltpt/base:change-dir filepath *blog-dir*))
  (pathname-name filepath))

;; find "entries", files tagged with 'entry', as in 'blog entry'
(defun entry-nodes (rmr)
  (let ((final-nodes))
    (loop for node in (cltpt/roam:roamer-nodes rmr)
          for text-obj = (cltpt/roam:node-text-obj node)
          when (typep text-obj 'cltpt/org-mode::org-document)
            do (let ((tags (cltpt/base:text-object-property text-obj :tags)))
                 (when (member "entry" tags :test 'equal)
                   (push node final-nodes))))
    final-nodes))

;; /home/mahmooz/work/emacs.d/lisp/config-org.el
(defun generate-collage-html (entries)
  (with-output-to-string (out)
    (write-sequence "<div class=\"collage\">" out)
    (loop for entry in entries
          do (format nil "<div class='card fancy-button' data-ref='blk:~A'>
  <img src='~A' class='card-image' />
  <span class='card-title'>~A</span>
  <span class='card-subtitle'>~A</span>
  <span class='card-subtitle'>~A</span>
</div>"
                     (getf entry :id)
                     (when (getf entry :id)
                       (if (file-exists-p (getf entry :image))
                           (export-static-file (getf entry :image))
                           (format t "collage image ~A doesnt exist~%"
                                   (getf entry :image))))
                     (getf entry :title)
                     (or (getf entry :subtitle) "")
                     (or (getf entry :subsubtitle) "")))
    (write-sequence  "</div>" out)))