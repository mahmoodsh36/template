#!/usr/bin/env -S sbcl --script
(require 'asdf)
(asdf:load-system :cltpt)

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

(generate)