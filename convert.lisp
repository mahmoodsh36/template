(asdf:load-system :cl-json)

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
(defvar *rmr*)
(defvar *main-files*
  '("/home/mahmooz/brain/notes/1678745440.org" ;; graph theory
    "/home/mahmooz/brain/notes/1656514223.org" ;; blog
    "/home/mahmooz/brain/notes/1729677442.6352403.org" ;; circuit complexity
    "/home/mahmooz/brain/notes/1709969723.org" ;; computability theory
    "/home/mahmooz/brain/notes/1723812143.2079227.org" ;; theory of computation
    "/home/mahmooz/brain/notes/1712601655.org" ;; programming
    "/home/mahmooz/brain/notes/1725811841.8613749.org" ;; media
    ))
(defvar *filepath-format*
  "%(cl-user::title-to-filename root-title).html")

(defun from-brain (filepath)
  (cltpt/file-utils:join-paths "/home/mahmooz/brain/" filepath))

;; get svg by #+name: or whatever?
(defun get-latex-preview-svg-by-blk-id (blk-id)
  (let* ((dest-node (cltpt/roam:get-node-by-id (cltpt/roam:current-roamer) blk-id)))
    (when dest-node
      (let* ((text-obj (cltpt/roam:node-text-obj dest-node))
             (match (cltpt/base:text-object-property text-obj :combinator-match))
             (latex-env-match
               (or (car (cltpt/combinator:find-submatch
                         match
                         'cltpt/org-mode::latex-env-1))
                   (car (cltpt/combinator:find-submatch
                         match
                         'cltpt/org-mode::latex-env))))
             (latex-env-text (getf latex-env-match :match)))
        (cdar (cltpt/latex:generate-svgs-for-latex (list latex-env-text)))))))

;; this is for postponing the execution to after roamer is done.
(defun get-latex-preview-svg-by-blk-id-1 (blk-id)
  (cons
   'after-roam
   (lambda ()
     (get-latex-preview-svg-by-blk-id blk-id))))


(defun generate ()
  (cltpt/file-utils:ensure-directory *blog-dir*)
  (setf cltpt/org-mode::*org-enable-macros* t)
  (setf cltpt:*debug* nil)
  (cltpt/zoo::init)
  (setf *rmr*
        (cltpt/roam:from-files
         '((:path ("/home/mahmooz/brain/notes/")
            :regex ".*\\.org"
            :format "org-mode"))))
  (uiop:with-current-directory (*blog-dir*)
    (let* ((files-to-convert
             (loop for main-file in *main-files*
                   for node = (find main-file
                                    (cltpt/roam:roamer-nodes *rmr*)
                                    :key (lambda (node)
                                           (cltpt/roam:node-file node))
                                    :test 'string=)
                   append (cons main-file (find-linked-files *rmr* node))))
           (other-head-contents
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
  #(getf cl-user::*my-metadata* :other-preamble-contents)"))
      (compile-all-latex-previews *rmr*)
      (generate-index *rmr*)
      (cltpt/roam:convert-all
       *rmr*
       (cltpt/base:text-format-by-name "html")
       *filepath-format*
       (lambda (filepath)
         (member filepath files-to-convert :test 'string=)))
      (export-metadata-to-json
       *rmr*
       "search.json"
       (lambda (filepath)
         (member filepath files-to-convert :test 'string=)))
      (mapc
       (lambda (item)
         (uiop:copy-file (uiop:merge-pathnames* *template-dir* item)
                         (uiop:merge-pathnames* (truename "~/work/blog/")
                                                item)))
       (list "head.html" "preamble.html" "search.html"))
      (cltpt/file-utils:ensure-directory *blog-static-dir*)
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
  (when (and filepath (uiop:probe-file* filepath))
    (uiop:copy-file
     filepath
     (cltpt/file-utils:change-dir filepath *blog-dir*))
    (cltpt/file-utils:file-basename filepath)))

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
          do (format out "<div class='card fancy-button' data-ref='blk:~A'>
  <img src='~A' class='card-image' />
  <span class='card-title'>~A</span>
  <span class='card-subtitle'>~A</span>
  <span class='card-subtitle'>~A</span>
</div>"
                     (getf entry :id)
                     (when (getf entry :id)
                       (if (uiop:probe-file* (getf entry :image))
                           (export-static-file (getf entry :image))
                           (format t "collage image ~A doesnt exist~%"
                                   (getf entry :image))))
                     (getf entry :title)
                     (or (getf entry :subtitle) "")
                     (or (getf entry :subsubtitle) "")))
    (write-sequence "</div>" out)
    out))

(defun generate-index (rmr)
  (let* ((entries (entry-nodes rmr))
         (index-file (uiop:merge-pathnames* *blog-dir* "index.html"))
         ;; (entries-html
         ;;   (loop for entry in entries
         ;;         collect (format
         ;;                  nil
         ;;                  "title: ~A~%"
         ;;                  (cltpt/roam:node-title entry))
         ;;           into strings
         ;;         finally (return (apply 'concatenate 'string strings))))
         (entries-html
           (generate-collage-html
            (loop for entry in entries
                  collect (list :title (cltpt/roam:node-title entry)
                                :id (cltpt/roam:node-id entry)
                                :image (cltpt/org-mode::text-object-org-keyword-value (cltpt/roam:node-text-obj entry) "image")
                                )))))
    (cltpt/file-utils:write-file
     index-file
     (cltpt/base::convert-text
      (cltpt/base:text-format-by-name "org-mode")
      (cltpt/base:text-format-by-name "html")
      (format nil
              "~A~%~A~%~A"
              "#+begin_export html"
              entries-html
              "#+end_export")))))

(defun find-linked-files (rmr root)
  "helper function to find all linked files from a node."
  (let ((linked-files)
        (nodes-left (list root)))
    (loop
      while nodes-left
      for node = (pop nodes-left)
      for text-obj = (cltpt/roam:node-text-obj node)
      do (loop
           for link-obj
             in (cltpt/base:find-children-recursively
                 text-obj
                 (lambda (child)
                   (cltpt/base:text-object-property child :dest)))
           do (let* ((link (cltpt/roam:resolve-link
                            rmr
                            node
                            link-obj
                            (if (cltpt/base:text-object-property link-obj :type)
                                (intern (cltpt/base:text-object-property link-obj :type))
                                'cltpt/roam::id)
                            (cltpt/base:text-object-property link-obj :dest)))
                     (linked-file
                       (when link
                         (cltpt/roam:node-file (cltpt/roam:link-dest-node link)))))
                (when linked-file
                  (unless (member linked-file
                                  (cons (cltpt/roam:node-file node) linked-files)
                                  :test 'string=)
                    (push linked-file linked-files)
                    (push (cltpt/roam:link-dest-node link) nodes-left))))))
    linked-files))

(defun encode-list-of-plists-to-json (list-of-plists)
  "encodes a list of plists to a JSON string using cl-json's explicit encoder."
  (cl-json:with-explicit-encoder
    (cl-json:encode-json-to-string
      `(:array ,@(loop for plist in list-of-plists
                       collect `(:plist ,@plist))))))

(defun export-metadata-to-json (rmr output-file file-predicate)
  (let* ((metadata
           (loop for node in (cltpt/roam:roamer-nodes rmr)
                 when (funcall file-predicate (cltpt/roam:node-file node))
                 collect (list
                          :id (cltpt/roam:node-id node)
                          :title (cltpt/roam:node-title node)
                          :filepath (cltpt/roam:node-info-format-str node *filepath-format*)
                          :description (cltpt/roam:node-desc node)))))
    (with-open-file (stream output-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (encode-list-of-plists-to-json metadata)
                    stream))))