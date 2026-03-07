(in-package :cl-user)

;; this script depends on my specific filesystem structure and may not work for others.

;; these are defined here but set in `init' function
(defvar *work-dir* nil)
(defvar *brain-dir* nil)
(defvar *data-dir* nil)
(defvar *blog-dir* nil)
(defvar *base-dir* nil)
(defvar *template-dir* nil)
(defvar *template-static-dir* nil)
(defvar *main-files* nil)
(defvar *excluded-files* nil)

(defun from-work (filepath)
  (cltpt/file-utils:join-paths *work-dir* filepath))
(defun from-brain (filepath)
  (cltpt/file-utils:join-paths *brain-dir* filepath))
(defun from-data (filepath)
  (cltpt/file-utils:join-paths *data-dir* filepath))
(defun expand-brain-paths (relative-paths)
  (mapcar #'from-brain relative-paths))

(defun init ()
  (setf *work-dir*
        (or (sb-ext:posix-getenv "WORK_DIR")
            (error "couldnt get WORK_DIR env var"))
        *brain-dir*
        (or (sb-ext:posix-getenv "BRAIN_DIR")
            (error "couldnt get BRAIN_DIR env var"))
        *data-dir*
        (or (sb-ext:posix-getenv "DATA_DIR")
            (error "couldnt get DATA_DIR env var"))
        *blog-dir*
        (from-work "blog")
        *base-dir*
        (from-work "template")
        *template-dir*
        (cltpt/file-utils:join-paths *base-dir* "template")
        *template-static-dir*
        (cltpt/file-utils:join-paths *base-dir* "static")
        *main-files*
        (expand-brain-paths
         '("notes/1678745440.org"              ;; graph theory
           "notes/1729677442.6352403.org"      ;; circuit complexity
           "notes/1709969723.org"              ;; computability theory
           "notes/1723812143.2079227.org"      ;; theory of computation
           "notes/1712601655.org"              ;; programming
           "notes/1725811841.8613749.org"      ;; media
           "notes/1725958715.1511376.org"      ;; formal logic
           "notes/1709021041.org"              ;; calculus
           "notes/1725810860.5115163.org"      ;; databases
           "notes/1677361099.org"              ;; machine learning
           "notes/1709021027.org"))            ;; linear algebra
        *excluded-files*
        (expand-brain-paths
         '("notes/1658738374.org"
           "notes/1709712959.org"
           "notes/1711448392.org"
           "notes/1712751117.org"
           "notes/1714912201.org"
           "notes/1716399914.org"
           "notes/1696961489.org"
           "notes/1707059561.org"
           "notes/1659132042.org"
           "notes/1664793548.org"))))

(defvar *static-filepath-format*
  "%(getf *file-info* :filename)")
(defvar *filepath-format*
  "%(cltpt/publish:title-to-filename (getf *file-info* :root-title)).html")
(defvar *rmr*)
(defvar *card-node* nil)

;; set my custom org-attach file-to-id function
(defun my-id-to-attach-dir (src-file id)
  (cltpt/file-utils:join-paths
   (cltpt/file-utils:file-dirpath src-file)
   "data"
   id))

(setf cltpt/base:*id-to-attach-dir-func* 'my-id-to-attach-dir)

(defun get-latex-preview-svg-by-blk-id (blk-id)
  (let* ((dest-node (cltpt/roam:get-node-by-id (cltpt/roam:current-roamer) blk-id)))
    (when dest-node
      (let* ((text-obj (cltpt/roam:node-text-obj dest-node))
             (latex-env-text (cltpt/base:text-object-contents text-obj)))
        (cdar (cltpt/latex-previews:generate-previews-for-latex (list latex-env-text)))))))

(defun export-static-file (filepath dest-dir)
  (when (and filepath (uiop:probe-file* filepath))
    (let ((new-filepath (cltpt/file-utils:change-dir filepath dest-dir)))
      (unless (uiop:probe-file* new-filepath)
        (uiop:copy-file filepath new-filepath)))
    (cltpt/file-utils:file-basename filepath)))

(defun entry-nodes (rmr)
  (let ((final-nodes))
    (loop for node in (cltpt/roam:roamer-nodes rmr)
          for text-obj = (cltpt/roam:node-text-obj node)
          when (typep text-obj 'cltpt/org-mode::org-document)
            do (let ((tags (cltpt/base:text-object-property text-obj :tags)))
                 (when (member "entry" tags :test 'equal)
                   (push node final-nodes))))
    final-nodes))

(defun node-date (node)
  (let* ((text-obj (cltpt/roam:node-text-obj node))
         (actual-date-str (cltpt/base:alist-get
                           (cltpt/base:text-object-property
                            text-obj
                            :keywords-alist)
                           "actual_date"))
         (date-str
           (or actual-date-str
               (cltpt/base:text-object-property text-obj :date)))
         (timestamp-match
           (cltpt/combinator:apply-rule
            nil
            cltpt/org-mode::org-timestamp
            (cltpt/combinator:reader-from-input date-str)
            0))
         (dummy-obj (make-instance 'cltpt/base::text-object)))
    (setf (cltpt/buffer:buffer-own-text dummy-obj) date-str)
    (cltpt/base:text-object-init dummy-obj date-str timestamp-match)
    (cltpt/base:text-object-finalize dummy-obj)
    (cltpt/org-mode::org-timestamp-match-to-time dummy-obj timestamp-match)))

(defun blog-nodes (rmr)
  (sort
   (loop for node in (cltpt/roam:roamer-nodes rmr)
         for text-obj = (cltpt/roam:node-text-obj node)
         for val = (cltpt/base:alist-get
                    (cltpt/base:text-object-property
                     text-obj
                     :keywords-alist)
                    "export_section")
         when (and (equal val "blog") (node-date node))
           collect node)
   'local-time:timestamp>
   :key #'node-date))

(defun generate-posts-list (nodes)
  (let ((entries
          (loop for node in nodes
                collect (let ((*card-node* node)
                              (cltpt/base:*convert-info*
                                (cltpt/base:merge-plist
                                 cltpt/base:*convert-info*
                                 (list
                                  :text-obj (cltpt/roam:node-text-obj node)
                                  :dest-fmt cltpt/base:*simple-format*
                                  :src-fmt cltpt/base:*simple-format*))))
                          (cltpt/publish:convert-template*
                           (cltpt/file-utils:join-paths *template-dir* "card.html"))))))
    (format nil
            "<div class=\"posts-list\">
~A
</div>"
            (cltpt/base:concat entries))))

(defun generate-blog-entries-html (nodes)
  (generate-posts-list nodes))

(defun generate-collage (nodes collage-title dest-dir)
  (with-output-to-string (out)
    (format out
            "<div class=\"container\">
<div class=\"gallery-section\">
<h2 class=\"section-title\">~A</h2>
<div class=\"gallery-grid\">"
            collage-title)
    (loop for node in nodes
          for title = (cltpt/roam:node-title node)
          for text-obj = (cltpt/roam:node-text-obj node)
          do (let* ((cltpt/roam:*roam-parse-data*
                      (list :roamer *rmr*
                            :filepath-format nil
                            :node node))
                    (image (cltpt/org-mode::text-object-org-keyword-value
                            (cltpt/roam:node-text-obj node)
                            "image"))
                    (image1 (if (uiop:probe-file* image)
                                (export-static-file image dest-dir)
                                (format t
                                        "collage image ~A doesnt exist~%"
                                        image)))
                    (href (cltpt/roam:node-info-format-str node *filepath-format*))
                    (entry-html "<div class=\"gallery-item\" onclick=\"window.location='~A'\">
  <div class=\"item-image\" />
    <img src=\"~A\" />
  </div>
  <div class=\"item-overlay\"><h3 class=\"item-title\">~A</h3><p class=\"item-subtitle\"></p></div>
</div>"))
               (format out entry-html href image1 title)))
    (write-sequence "</div>
</div>
</div>"
                    out)))

(defun encode-list-of-plists-to-json (list-of-plists)
  (let ((list-of-alists
          (loop for plist in list-of-plists
                collect (loop for (key value) on plist by #'cddr
                              collect (cons (string-downcase (symbol-name key))
                                            value)))))
    (cl-json:encode-json-to-string list-of-alists)))

(defun export-metadata-to-json (rmr output-file file-predicate)
  (let ((metadata
          (loop for node in (cltpt/roam:roamer-nodes rmr)
                for text-obj = (cltpt/roam:node-text-obj node)
                for tags = (cltpt/base:text-object-property text-obj :tags)
                when (and (funcall file-predicate (cltpt/roam:node-file node))
                          (not (and tags (member "noexport" tags :test 'string=))))
                  collect (list
                           :date (cltpt/base:text-object-property text-obj :date)
                           :tags tags
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

(defun generate ()
  (init)
  (setf cltpt/org-mode::*org-enable-macros* t)
  (cltpt/zoo::init)
  (let ((rmr-files `((:path (,(concatenate 'string (from-brain "notes") "/"))
                      :glob "*.org"
                      :format "org-mode"))))
    (generate-from-files-to-dir rmr-files *blog-dir*)))

;; this project acts as a cltpt/publish theme directory.
;; we pre-create a roamer to compute blog-nodes for include-files and for search.json export.
;; publish creates its own roamer internally from the same files.
(defun generate-from-files-to-dir (rmr-files dest-dir)
  (let* ((rmr (cltpt/roam:from-files rmr-files))
         (*rmr* rmr)
         ;; root files to include main files and blog-tagged posts.
         ;; publish will expand these transitively via find-linked-files.
         (include-files
           (remove-duplicates
            (append *main-files*
                    (mapcar #'cltpt/roam:node-file (blog-nodes rmr)))
            :test #'string=))
         ;; Full expansion needed for search.json (mirrors what publish computes).
         (files-to-convert
           (loop for main-file in include-files
                 for node = (find main-file
                                  (cltpt/roam:roamer-nodes rmr)
                                  :key #'cltpt/roam:node-file
                                  :test #'string=)
                 append (cons main-file
                              (cltpt/utils:find-linked-files
                               rmr node *excluded-files*))))
         (file-predicate
           (lambda (filepath)
             (member filepath files-to-convert :test #'string=)))
         (cltpt/base:*author* "mahmood")
         (cltpt/latex-previews:*latex-preview-preamble*
           "\\documentclass[11pt]{article}
\\usepackage{\\string~/.emacs.d/common}"))
    ;; remove bbox=preview from the dvisvgm command. i prefer it this way so that
    ;; images arent cut-off by width.
    (setf (getf (cdar cltpt/latex-previews::*latex-preview-pipelines*) :image-converter)
          "dvisvgm --page=1- --no-fonts --relative --clipjoin --optimize -o %B-%9p.svg %f")
    (cltpt/file-utils:ensure-dir-exists (cltpt/file-utils:as-dir-path dest-dir))
    (cltpt/publish:publish
     dest-dir
     rmr-files
     :include-files include-files
     :exclude-files *excluded-files*
     :templates (mapcar (lambda (f) (cltpt/file-utils:join-paths *template-dir* f))
                        '("index.html" "about.html" "archive.html" "blog.html"))
     :template-file (cltpt/file-utils:join-paths *template-dir* "page.html")
     :theme-dir *base-dir*
     :html-static-route "/static/"
     :filepath-format *filepath-format*
     :static-filepath-format *static-filepath-format*)
    ;; copy css/js from theme static/ into $dest/static/.
    (let ((dest-dir-static (cltpt/file-utils:join-paths dest-dir "static")))
      (cltpt/file-utils:ensure-dir-exists
       (concatenate 'string dest-dir-static "/"))
      (mapc
       (lambda (item)
         (setf item (uiop:unix-namestring item))
         (uiop:copy-file
          item
          (cltpt/file-utils:join-paths
           dest-dir-static
           (cltpt/file-utils:file-basename item))))
       (uiop:directory-files (cltpt/file-utils:as-dir-path *template-static-dir*))))
    ;; robots.txt at the site root.
    (uiop:copy-file
     (cltpt/file-utils:join-paths *template-static-dir* "robots.txt")
     (cltpt/file-utils:join-paths dest-dir "robots.txt"))
    ;; search.json for the frontend search feature.
    (export-metadata-to-json rmr (cltpt/file-utils:join-paths dest-dir "search.json") file-predicate)))