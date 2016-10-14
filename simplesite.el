;;; simplesite.el --- Simple static site generator -*- lexical-binding: t -*-

;; Copyright 2015 Jerry Xu
;;
;; Author: GuanghuiXu gh_xu@qq.com
;; Maintainer: GuanghuiXu gh_xu@qq.com
;; Created: 2015-4-28
;; Version: 0.1
;; Keywords: local
;; Homepage: https://github.com/jerryxgh/simplesite
;; Package-Version: 0.1
;; Package-Requires: ((emacs "24.3") (org "8.0") (f "0.17.3") (mustache "0.23"))
;;

;; This file is not part of GNU Emacs.

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    simplesite--private-function
;;
;; for private functions.

;;; Commentary:

;; Simple static site generator.

;; Put this file into your load-path and the following into your ~/.emacs:

;; (require 'simplesite)

;;; Change Log:

;; Version 0.1 2015-4-28 GuanghuiXu
;;   - Initial release

;;; TODO:
;;  - create a db to store all posts and theme info
;;  - skip copying if theme not changed
;;  - delete generating files if the source file is deleted
;;  - add cask to build simplesite
;;  - add a command to add or update post header

;;; Code:

(require 'color)
(require 'ox)

(require 's)
(require 'dash)
(require 'ht)
(require 'mustache)
(require 'f)
(require 'parse-time)

;;; constants
(defconst simplesite-version "0.1")

(defconst simplesite-log-buffer "*[simplesite-log]*"
  "Name of the temporary buffer used by simplesite.")

(defconst simplesite-installed-directory
  (if load-file-name (file-name-directory load-file-name))
  "Directory where simplesite is loaded from, ends with '/'.")

;;; custom options
(defgroup simplesite nil
  "Options for generating static site."
  :tag "Static site generator based on org." :group 'org)

(defconst SIMPLESITE-LOG-ERROR 0 "ERROR log level.")
(defconst SIMPLESITE-LOG-WARN 0 "WARN log level.")
(defconst SIMPLESITE-LOG-INFO 1 "INFO log level.")
(defconst SIMPLESITE-LOG-DEBUG 2 "DEBUG log level.")

(defcustom simplesite-log-level 'SIMPLESITE-LOG-INFO
  "Log level of simplesite.
Should be one of `SIMPLESITE-LOG-ERROR', `SIMPLESITE-LOG-WARN',
`SIMPLESITE-LOG-INFO' or `SIMPLESITE-LOG-DEBUG'."
  :group 'simplesite
  :type '(choice
          (const SIMPLESITE-LOG-ERROR)
          (const SIMPLESITE-LOG-WARN)
          (const SIMPLESITE-LOG-INFO)
          (const SIMPLESITE-LOG-DEBUG))
  :safe #'symbolp)

(defcustom simplesite-log-message nil
  "Output log to message."
  :group 'simplesite
  :type 'boolean)

(defcustom simplesite-author user-full-name
  "Directory of all source files for generating site."
  :group 'simplesite :type 'string)

(defcustom simplesite-source-directory nil
  "Directory of all source files for generating site."
  :group 'simplesite :type 'string)

(defcustom simplesite-output-directory nil
  "Output directory for generated files."
  :group 'simplesite :type 'string)

(defcustom simplesite-index-posts-max 20
  "Maximal number of posts shown in index page."
  :group 'simplesite :type 'int)

(defcustom simplesite-site-domain nil
  "Domain name of entire site.
It is recommended to assign with prefix http:// or https://,  http will be
considered if not assigned."
  :group 'simplesite :type 'string)

(defcustom simplesite-site-title "simplesite"
  "Main title of entire site."
  :group 'simplesite :type 'string)

(defcustom simplesite-site-main-desc "description"
  "Main description of entire site."
  :group 'simplesite :type 'string)

(defcustom simplesite-site-main-keywords "keywords"
  "Main keywords of entire site."
  :group 'simplesite :type 'string)

(defcustom simplesite-site-sub-title "subtitle"
  "Subtitle of entire site."
  :group 'simplesite :type 'string)

(defcustom simplesite-theme-directory
  (concat simplesite-installed-directory "themes/")
  "Directory stores themes for page rendering.
By default, it points to the directory `themes' in simplesite installation
directory."
  :group 'simplesite :type 'string)

(defcustom simplesite-theme "next"
  "Theme used for page generation."
  :group 'simplesite :type 'string)

(defcustom simplesite-personal-github-link nil
  "Personal github link."
  :group 'simplesite :type 'string)

(defcustom simplesite-personal-twitter-link nil
  "Personal twitter link."
  :group 'simplesite :type 'string)

(defcustom simplesite-personal-weibo-link nil
  "Personal weibo link."
  :group 'simplesite :type 'string)

(defcustom simplesite-personal-douban-link nil
  "Personal douban link."
  :group 'simplesite :type 'string)

(defcustom simplesite-personal-zhihu-link nil
  "Personal zhihu link."
  :group 'simplesite :type 'string)

(defcustom simplesite-personal-avatar nil
  "Link to an avatar image."
  :group 'simplesite :type 'string)

(defcustom simplesite-personal-disqus-shortname nil
  "Personal disqus shortname."
  :group 'simplesite :type 'string)

(defcustom simplesite-personal-duoshuo-shortname nil
  "Personal duoshuo shortname."
  :group 'simplesite :type 'string)

(defcustom simplesite-confound-email t
  "This is used to determine whether email should be confounded or not."
  :group 'simplesite :type 'boolean)

(defcustom simplesite-tag-cloud-min-font 12
  "Minimal font size in pixel of tags in tag cloud."
  :group 'simplesite :type 'int)

(defcustom simplesite-tag-cloud-max-font 30
  "Maximal font size in pixel of tags in tag cloud."
  :group 'simplesite :type 'int)

(defcustom simplesite-tag-cloud-start-color "#ccc"
  "Start color of tags in tag cloud."
  :group 'simplesite :type 'string)

(defcustom simplesite-tag-cloud-end-color "#111"
  "End color of tags in tag cloud."
  :group 'simplesite :type 'string)

(defvar simplesite--dest-directory (concat simplesite-output-directory
                                           "/dest")
  "Destination directory of generated files.
All generated files except 'index.html' of site should in this
directory, when do generating, this directory will be deleted and
recreated.")

;;; theme
(defun simplesite-prepare-theme (theme theme-dir output-dir load-dir)
  "Copy resources of THEME in THEME-DIR to OUTPUT-DIR.

If THEME not exist in THEME-DIR, use default theme `next' under LOAD-DIR."
  (let ((theme-resource-output-dir (expand-file-name "resources/" output-dir))
        (theme-resource-dir (simplesite-get-theme-resource-dir theme
                                                               theme-dir)))
    (unless (file-directory-p theme-resource-dir)
      (simplesite--warn "Theme %s not found, use default theme `next' instead." theme)
      (setq theme-dir (concat load-dir"themes/"))
      (setq theme-resource-dir (simplesite-get-theme-resource-dir theme
                                                                  theme-dir)))
    (if (file-directory-p theme-resource-output-dir)
        (delete-directory theme-resource-output-dir t))
    (copy-directory theme-resource-dir theme-resource-output-dir t t t)))

(defun simplesite-get-theme-resource-dir (theme theme-dir)
  "Return resource directory of THEME under THEME-DIR ending with /."
  (file-name-as-directory
   (expand-file-name
    (format "%s/resources" theme) theme-dir)))

(defun simplesite-get-theme-template-dir (theme theme-dir)
  "Return theme template directory of THEME under THEME-DIR ending with /."
  (file-name-as-directory
   (expand-file-name
    (format "%s/templates" theme) theme-dir)))


;;; backends
(defun simplesite-parse-all-src-files (src-dir dist-dir progress-reporter)
  "Parse all src files to hashmap list, return sorted list.

SRC-DIR: directory to hold all source files.
DIST-DIR: directory to hold all generated files.
PROGRESS-REPORTER: a progress reporter."
  (let* ((src-file-list (simplesite--get-all-src-files src-dir))
         (src-file-count (length src-file-list))
         (i 0)
         (post-list
          (mapcar #'(lambda (src-file)
                      (setq i (+ i 1))
                      (progress-reporter-update
                       progress-reporter (+ 5 (/ (* 50 i) src-file-count)))

                      (simplesite--export-org-file src-file src-dir dist-dir))
                  src-file-list)))

    ;; delete nil elements
    (setq post-list (-filter #'(lambda (e) e) post-list))

    ;; sort files by date in descendent order
    (setq post-list (sort post-list
                          #'(lambda (a b)
                              (if (string= (ht-get b "date") (ht-get a "date"))
                                  (time-less-p (ht-get b "time-stamp")
                                               (ht-get a "time-stamp"))
                                (string< (ht-get b "date")
                                         (ht-get a "date"))))))
    ;; set previous post and next post for every post
    (let ((next-post (car post-list)))
      (mapc #'(lambda (e)
                (ht-set e "next-post" next-post)
                (ht-set next-post "prev-post" e)
                (setq next-post e))
            (cdr post-list)))

    ;; set post-count
    (let ((post-count (length post-list)))
      (mapc #'(lambda (e)
                (ht-set e "post-count" post-count))
            post-list))

    post-list))

(defun simplesite--get-all-src-files (directory)
  "Return all source files under DIRECTORY and it's subdirectory.

TODO: make regexp configurable."
  (simplesite--walk-directory directory "^.*\\.org$"))

(defun simplesite--walk-directory
    (&optional directory match ignore-directories)
  "Walk through DIRECTORY tree.
If DIRECTOR is nil, use `default-directory' as startpoint.
Argument MATCH can be a predicate or a regexp.
Argument IGNORE-DIRECTORIES can be list of file names to be ignored."
  (unless directory
    (setq directory default-directory))
  (unless (file-directory-p directory)
    (setq directory (file-name-directory directory)))
  (let (directory-stack result)
    (if (file-exists-p directory)
        (push directory directory-stack))
    (while directory-stack
      (let* ((current-directory (pop directory-stack))
             (file-list (directory-files current-directory t)))
        (mapc #'(lambda (file)
                  (if (not (file-symlink-p file))
                      (let ((file-name (file-name-nondirectory file)))
                        (if (file-directory-p file)
                            (if (and (not (equal file-name "."))
                                     (not (equal file-name ".."))
                                     (not (member file-name ignore-directories)))
                                (push file directory-stack))
                          (if match
                              (and (if (functionp match)
                                       (funcall match file)
                                     (and (stringp match)
                                          (string-match match file-name)))
                                   (push file result))
                            (push file result))))))
              file-list)))
    result))


(defvar simplesite--file-links-in-srcs nil
  "List of all links in source files.
This is used between `org-html-link' and `simplesite--export-org-file'")

;; (defvar simplesite--current-src-output-dir nil
;;   "Current source file's output directory.
;; This is used between `org-html-link' and `simplesite--export-org-file'")

;;; advice of org-html-link to get list of links
(defadvice org-html-link (before simplesite-org-html-link)
  "Advice of org-html-link to get list of links."
  (let* ((link (ad-get-arg 0))
         (type (plist-get (nth 1 link) :type))
         (path (plist-get (nth 1 link) :path))
         (raw-link (plist-get (nth 1 link) :raw-link))
         (dst-filename (f-filename raw-link))
         link-plist)
    (cond ((string= "file" type)
           (plist-put (nth 1 link) :path dst-filename)
           (plist-put (nth 1 link) :raw-link dst-filename)
           (setq link-plist (plist-put link-plist :type type))
           (setq link-plist (plist-put link-plist :path path))
           (setq link-plist (plist-put link-plist :raw-link raw-link))
           (setq link-plist (plist-put link-plist :dst-filename dst-filename))
           (add-to-list 'simplesite--file-links-in-srcs link-plist)
           ;; TODO: process other type links of org
           ))))

(defun simplesite--process-links-in-srcs (org-file links output-dir)
  "Process ORG-FILE's LINKS, which is output to OUTPUT-DIR.

Copy images to OUTPUT-DIR directory and rename it to link name."
  (mapc #'(lambda (link)
            (let* ((type (plist-get link :type))
                   ;; (path (plist-get link :path))
                   (raw-link (plist-get link :raw-link))
                   (dst-filename (plist-get link :dst-filename))
                   (src-file (expand-file-name raw-link (f-dirname org-file)))
                   (dst-file (expand-file-name dst-filename output-dir)))
              (if (not (file-directory-p output-dir))
                  (mkdir output-dir t))
              (cond ((string= "file" type)
                     (if (file-exists-p src-file)
                         (f-copy src-file dst-file)
                       (simplesite--debug "file '%s' not exist" src-file))
                     ;; TODO: process other type links of org
                     ))))
        links))

;;; org backend - generate html by and from org-mode
(defun simplesite--export-org-file (org-file src-dir dist-dir)
  "Export one ORG-FILE, return a hashmap contain all result.

SRC-DIR should be `simplesite-source-directory',
DIST-DIR should be `simplesite--dest-directory',
but to be simple, try to not use global variables."
  (simplesite--debug "processing '%s'" org-file)
  (let ((output-dir (simplesite--compute-output-dir org-file dist-dir src-dir))
        (uri (simplesite--compute-uri org-file src-dir))
        post)
    (with-temp-buffer
      (setq buffer-file-coding-system 'utf-8)
      (insert-file-contents org-file)

      ;; collect attributes
      (setq post
            (ht ("file" org-file)
                ("md5" (simplesite--md5-file org-file))
                ("title" (or (simplesite--get-org-option "TITLE")
                             "Untitled"))
                ("author" (or (simplesite--get-org-option "AUTHOR")
                              user-full-name "Unknown Author"))
                ("description" (or (simplesite--get-org-option "DESCRIPTION")
                                   "No Description"))
                ("keywords" (or (simplesite--get-org-option "TAGS") ""))
                ("category" (or (simplesite--get-org-option "CATEGORY")
                                (simplesite--get-category org-file src-dir)
                                "default"))
                ("uri" uri)
                ("time-stamp" (nth 5 (file-attributes org-file)))
                ("date" (or (let* ((date-str (simplesite--get-org-option "DATE"))
                                   (time (and date-str
                                              (parse-time-string date-str))))
                              (and time
                                   (not (equal (parse-time-string "") time))
                                   (simplesite--format-iso-8601-date
                                    (apply 'encode-time
                                           (mapcar #'(lambda (e) (or e 0))
                                                   time)))))
                            (simplesite--format-iso-8601-date
                             (nth 5 (file-attributes org-file)))))
                ("email" user-mail-address)
                ("output-dir" output-dir)))
      (let ((tags (ht-get post "keywords")))
        (if tags
            (ht-set post
                    "tags"
                    (delete "" (mapcar 'string-trim
                                       (split-string tags "[:,]+" t))))))
      (setq simplesite--file-links-in-srcs nil)
      (ht-set post "post-content" (org-export-as 'html nil nil t nil))
      (simplesite--process-links-in-srcs org-file simplesite--file-links-in-srcs
                                         output-dir))

    post))

(defun simplesite--compute-output-dir (org-file dist-dir src-dir)
  "Get output directory of ORG-FILE, which ends with /.

Result = DIST-DIR + \"/posts\" + (ORG-FILE - SRC-DIR) - suffix + /."
  (concat
   (file-name-sans-extension
    (concat dist-dir "/posts" (s-chop-prefix src-dir org-file)))
   "/"))

(defun simplesite--compute-uri (org-file src-dir)
  "Get uri of ORG-FILE.

Result = \"/dest/posts\" + (ORG-FILE - SRC-DIR - suffix)."
  (format "/dest/posts%s/index.html" (file-name-sans-extension
                                      (s-chop-prefix src-dir org-file))))

(defun simplesite--get-category (org-file src-dir)
  "Get category of ORG-FILE.

Result = first directory of (ORG-FILE - SRC-DIR), if nil, return filename."
  (let ((src-dir (directory-file-name src-dir)))
    (while (not (string= src-dir (f-parent org-file)))
      (setq org-file (f-parent org-file)))
    (file-name-base org-file)))

(defun simplesite--get-org-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward match-regexp nil t)
          (match-string-no-properties 2 nil)))))

(defun simplesite--format-iso-8601-date (date)
  "Format DATE to iso-8601 format."
  (concat
   (format-time-string "%Y-%m-%d" date)))

(defun simplesite--format-iso-8601-time (time)
  "Format TIME to iso-8601 format."
  (concat
   (format-time-string "%Y-%m-%dT%T" time)
   (funcall (lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
            (format-time-string "%z" time))))

(defun simplesite--md5-file (file)
  "Return md5 digest of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally file)
    (md5 (current-buffer))))

;;; index page
(defun simplesite-generate-index (post-list common-map)
  "Generate index page based on POST-LIST using COMMON-MAP."
  (let ((mustache-partial-paths
         (list (simplesite-get-theme-template-dir simplesite-theme
                                                  simplesite-theme-directory))))

    (ht-set common-map "page-title" simplesite-site-title)
    (ht-set common-map "content" (simplesite--render-index-content post-list))
    (f-write
     (mustache-render
      (f-read (concat (simplesite-get-theme-template-dir
                       simplesite-theme simplesite-theme-directory)
                      "layout.mustache"))
      common-map)
     'utf-8
     (concat simplesite-output-directory "/index.html"))))

(defun simplesite--render-index-content (post-list)
  "Render index content based on POST-LIST."
  (mustache-render
   (f-read (concat (simplesite-get-theme-template-dir
                    simplesite-theme simplesite-theme-directory)
                   "index.mustache"))
   (ht ("post-list" post-list))))

;;; category index page
(defun simplesite-generate-categories (post-list common-map)
  "Generate categorie pages based on POST-LIST using COMMON-MAP.

POST-LIST: hash table list of all source files."
  (let ((category-list (simplesite--parse-categories post-list)))
    (simplesite--generate-categories-index category-list common-map)
    (mapc #'(lambda (e)
              (simplesite--generate-category-page e common-map))
          category-list)))

(defun simplesite--generate-categories-index (category-list common-map)
  "Generate categories index page baaed on CATEGORY-LIST using COMMON-MAP.

CATEGORY-LIST: hash table of <category, file>."
  (let ((mustache-partial-paths
         (list (simplesite-get-theme-template-dir simplesite-theme
                                                  simplesite-theme-directory)))
        (output-dir (concat simplesite--dest-directory "/categories")))
    (if (not (file-directory-p output-dir))
        (mkdir output-dir t))
    (ht-set common-map "page-title" (concat "Categories - "
                                            simplesite-site-title))
    (ht-set common-map "content" (simplesite--render-categories-index-content
                                  category-list))
    (f-write
     (mustache-render
      (f-read (concat (simplesite-get-theme-template-dir
                       simplesite-theme simplesite-theme-directory)
                      "layout.mustache"))

      common-map)
     'utf-8
     (concat output-dir "/index.html"))))

(defun simplesite--generate-category-page (category common-map)
  "Generate one category page based CATEGORY using COMMON-MAP."
  (let* ((mustache-partial-paths
          (list (simplesite-get-theme-template-dir simplesite-theme
                                                   simplesite-theme-directory)))
         (cate-name (ht-get category "name"))
         (output-dir (concat simplesite--dest-directory "/categories/"
                             cate-name)))
    (if (not (file-directory-p output-dir))
        (mkdir output-dir t))
    (ht-set common-map "page-title" (concat "Categories: " cate-name " - "
                                            simplesite-site-title))
    (ht-set common-map "content" (simplesite--render-category-page-content
                                  category))
    (f-write
     (mustache-render
      (f-read (concat (simplesite-get-theme-template-dir
                       simplesite-theme simplesite-theme-directory)
                      "layout.mustache"))
      common-map)
     'utf-8
     (concat output-dir "/index.html"))))

(defun simplesite--render-categories-index-content (category-list)
  "Render categories index content based on CATEGORY-LIST."
  (mustache-render
   (f-read (concat (simplesite-get-theme-template-dir
                    simplesite-theme simplesite-theme-directory)
                   "categories-index.mustache"))
   (ht ("categories" category-list)
       ("category-count" (length category-list)))))

(defun simplesite--render-category-page-content (category)
  "Render category page content based on CATEGORY."
  (mustache-render
   (f-read (concat (simplesite-get-theme-template-dir
                    simplesite-theme simplesite-theme-directory)
                   "category.mustache"))
   category))

(defun simplesite--parse-categories (post-list)
  "Group POST-LIST by category."
  (let ((category-map (ht-create))
        categoriy-list)
    (mapc #'(lambda (post)
              (let ((category-name (ht-get post "category")))
                (ht-set category-map
                        category-name
                        (cons post (ht-get category-map category-name)))))
          (reverse post-list))
    ;; convert hashtable to list
    (setq categoriy-list
          (ht-map #'(lambda (key value)
                      (ht ("name" key)
                          ("uri" (format "/dest/categories/%s/index.html" key))
                          ("count" (length value))
                          ("posts" value)))
                  category-map))
    ;; sort by category name
    (setq categoriy-list
          (sort categoriy-list
                #'(lambda (a b)
                    (string< (ht-get a "name")
                             (ht-get b "name")))))
    categoriy-list))

;;; tag index page
(defun simplesite-generate-tags (post-list common-map)
  "Generate tag pages based on POST-LIST using COMMON-MAP.

POST-LIST: hash table list of all source files."
  (let ((tag-list (simplesite--parse-tags post-list)))
    (simplesite--generate-tags-index tag-list common-map)
    (mapc #'(lambda (post)
              (simplesite--generate-tag-page post common-map))
          tag-list)))

(defun simplesite--generate-tags-index (tag-list common-map)
  "Generate tags index page based on TAG-LIST using COMMON-MAP.

TAG-LIST: hash table of <tag, file>."
  (let ((mustache-partial-paths
         (list (simplesite-get-theme-template-dir
                simplesite-theme simplesite-theme-directory)))
        (output-dir (concat simplesite--dest-directory "/tags")))
    (if (not (file-directory-p output-dir))
        (mkdir output-dir t))
    (ht-set common-map "page-title" (concat "Tags - " simplesite-site-title))
    (ht-set common-map "content" (simplesite--render-tags-index-content
                                  tag-list))
    (f-write
     (mustache-render
      (f-read (concat (simplesite-get-theme-template-dir
                       simplesite-theme simplesite-theme-directory)
                      "layout.mustache"))
      common-map)
     'utf-8
     (concat output-dir "/index.html"))))

(defun simplesite--generate-tag-page (tag common-map)
  "Generate tag page based on TAG using COMMON-MAP."
  (let* ((mustache-partial-paths
          (list (simplesite-get-theme-template-dir
                 simplesite-theme simplesite-theme-directory)))
         (tag-name (ht-get tag "name"))
         (output-dir (concat simplesite--dest-directory "/tags/" tag-name)))
    (if (not (file-directory-p output-dir))
        (mkdir output-dir t))
    (ht-set common-map "page-title" (concat "Tags: " tag-name " - "
                                            simplesite-site-title))
    (ht-set common-map "content" (simplesite--render-tag-page-content tag))
    (f-write
     (mustache-render
      (f-read (concat (simplesite-get-theme-template-dir
                       simplesite-theme simplesite-theme-directory)
                      "layout.mustache"))
      common-map)
     'utf-8
     (concat output-dir "/index.html"))))

(defun simplesite--render-tags-index-content (tag-list)
  "Render tags index content based on TAG-LIST."
  (mustache-render
   (f-read (concat (simplesite-get-theme-template-dir
                    simplesite-theme simplesite-theme-directory)
                   "tags-index.mustache"))
   (ht ("tags" tag-list)
       ("tag-count" (length tag-list)))))

(defun simplesite--render-tag-page-content (tag)
  "Render tag page content based on TAG."
  (mustache-render
   (f-read (concat (simplesite-get-theme-template-dir
                    simplesite-theme simplesite-theme-directory)
                   "tag.mustache"))
   tag))

(defun simplesite--parse-tags (post-list)
  "Group POST-LIST by tag."
  (let ((tag-map (ht-create))
        (tag-max-count 1)
        tag-color-gradient
        tag-list)
    (mapc #'(lambda (post)
              (let ((tags (ht-get post "tags")))
                (if (not tags)
                    (ht-set tag-map
                            "no-tag"
                            (cons post (ht-get tag-map "no-tag")))
                  (mapc #'(lambda (tag-name)
                            (ht-set tag-map
                                    tag-name
                                    (cons post (ht-get tag-map tag-name))))
                        tags))))
          (reverse post-list))
    ;; convert tag from hashtable to list and initialize tag-max-count
    (setq tag-list
          (ht-map #'(lambda (key value)
                      (let ((tag-count (length value)))
                        (if (> tag-count tag-max-count)
                            (setq tag-max-count tag-count))
                        (ht ("name" key)
                            ("uri" (format "/dest/tags/%s/index.html" key))
                            ("posts" value)
                            ("count" tag-count))))
                  tag-map))

    ;; set tag font size and color in tag cloud
    (setq tag-color-gradient (simplesite--compute-tag-color-gradient
                              tag-max-count))
    (mapc #'(lambda (post)
              (let ((tag-count (ht-get post "count")))
                (ht-set post "font-size" (simplesite--compute-tag-font-size
                                          tag-count tag-max-count))
                (ht-set post "color" (nth (- tag-count 1) tag-color-gradient))))
          tag-list)

    ;; sort by tag name
    (setq tag-list
          (sort tag-list
                #'(lambda (a b)
                    (string< (ht-get a "name")
                             (ht-get b "name")))))
    tag-list))

(defun simplesite--compute-tag-font-size (count max-count)
  "Compute font size of a tag with COUNT posts.

MAX-COUNT: maximal posts that all tags have."
  (if (<= max-count 1)
      simplesite-tag-cloud-min-font
    (+ (round (* (/ (- count 1.0) (- max-count 1.0))
                 (- simplesite-tag-cloud-max-font
                    simplesite-tag-cloud-min-font)))

       simplesite-tag-cloud-min-font)))

(defun simplesite--compute-tag-color-gradient (max-count)
  "Return a list with MAX-COUNT colors from `simplesite-tag-cloud-start-color' \
to `simplesite-tag-cloud-end-color'."
  (mapcar #'(lambda (color)
              (apply #'color-rgb-to-hex color))
          (cons (color-name-to-rgb simplesite-tag-cloud-start-color)
                (append
                 (color-gradient
                  (color-name-to-rgb simplesite-tag-cloud-start-color)
                  (color-name-to-rgb simplesite-tag-cloud-end-color)
                  (- max-count 2))

                 (list (color-name-to-rgb simplesite-tag-cloud-end-color))))))

;;; post page
(defun simplesite-generate-post (common-map-post)
  "Generate post using COMMON-MAP-POST.
COMMON-MAP-POST is the union of common-map and post."
  ;; show comment in post
  (ht-set common-map-post "show-comment" t)
  (ht-set common-map-post "page-title" (concat (ht-get common-map-post "title")
                                               " - "
                                               simplesite-site-title))
  (ht-set common-map-post "content" (simplesite--render-post-content
                                     common-map-post))
  (let ((mustache-partial-paths
         (list (simplesite-get-theme-template-dir simplesite-theme
                                                  simplesite-theme-directory)))

        (output-dir (ht-get common-map-post "output-dir")))
    (if (not (file-directory-p output-dir))
        (mkdir output-dir t))
    (f-write
     (mustache-render
      (f-read (concat (simplesite-get-theme-template-dir
                       simplesite-theme simplesite-theme-directory)
                      "layout.mustache"))
      common-map-post)
     'utf-8
     (concat output-dir "index.html"))))

(defun simplesite--render-post-content (post)
  "Render content component of post based on POST."
  (mustache-render
   (f-read (concat (simplesite-get-theme-template-dir
                    simplesite-theme simplesite-theme-directory)
                   "post.mustache"))
   post))

;;; archive index page
(defun simplesite-generate-archives (post-list common-map)
  "Generate archive pages of all POST-LIST using COMMON-MAP.

POST-LIST: hash table list of all source files."
  (let ((archive-list (simplesite--parse-archives post-list)))
    (simplesite--generate-archives-index archive-list common-map)))

(defun simplesite--generate-archives-index (archive-list common-map)
  "Generate archives index page baaed on ARCHIVE-LIST using COMMON-MAP.

ARCHIVE-LIST: hash table of <archive, file>."
  (let ((mustache-partial-paths
         (list (simplesite-get-theme-template-dir simplesite-theme
                                                  simplesite-theme-directory)))
        (output-dir (concat simplesite--dest-directory "/archives")))
    (if (not (file-directory-p output-dir))
        (mkdir output-dir t))
    (ht-set common-map "page-title" (concat "Archives - " simplesite-site-title))
    (ht-set common-map "content" (simplesite--render-archives-index-content
                                  archive-list common-map))
    (f-write
     (mustache-render
      (f-read (concat (simplesite-get-theme-template-dir
                       simplesite-theme simplesite-theme-directory)
                      "layout.mustache"))

      common-map)
     'utf-8
     (concat output-dir "/index.html"))))

(defun simplesite--render-archives-index-content (archive-list common-map)
  "Render archives index content based on ARCHIVE-LIST using COMMON-MAP."
  (ht-set common-map "archives" archive-list)
  (mustache-render
   (f-read (concat (simplesite-get-theme-template-dir
                    simplesite-theme simplesite-theme-directory)
                   "archives-index.mustache"))
   common-map))

(defun simplesite--render-archive-page-content (archive)
  "Render archive page content based on ARCHIVE."
  (mustache-render
   (f-read (concat (simplesite-get-theme-template-dir
                    simplesite-theme simplesite-theme-directory)
                   "archive.mustache"))
   archive))

(defun simplesite--extract-year-of-date (date)
  "Extract year of DATE."
  (substring date 0 (string-match "-" date)))

(defun simplesite--parse-archives (post-list)
  "Group POST-LIST by year(or archive)."
  (let ((archive-map (ht-create))
        archive-list)
    (mapc #'(lambda (post)
              (let ((archive-name (simplesite--extract-year-of-date
                                   (ht-get post "date"))))
                (ht-set archive-map
                        archive-name
                        (cons post (ht-get archive-map archive-name)))))
          (reverse post-list))
    ;; convert hashtable to list
    (setq archive-list
          (ht-map #'(lambda (key value)
                      (ht ("name" key)
                          ("uri" (format "/dest/archives/%s/index.html" key))
                          ("posts" value)))
                  archive-map))
    ;; sort by category name
    (setq archive-list
          (sort archive-list
                #'(lambda (a b)
                    (string< (ht-get b "name")
                             (ht-get a "name")))))
    archive-list))

(defun simplesite--get-avatar-uri ()
  "Prepare avatar according to `simplesite-personal-avatar'and return uri.

Should be called after theme preparation."
  (let ((result "/dest/resources/images/default_avatar.jpg"))
    (if simplesite-personal-avatar
        (if (or (s-starts-with-p "http://" simplesite-personal-avatar)
                (s-starts-with-p "https://" simplesite-personal-avatar))
            (setq result simplesite-personal-avatar)
          (if (file-exists-p simplesite-personal-avatar)
              (progn
                (f-copy simplesite-personal-avatar
                        (concat simplesite--dest-directory
                                "/resources/"))
                (setq result (expand-file-name
                              (f-filename simplesite-personal-avatar)
                              "/dest/resources/")))
            )))
    result))

;; From http://emacs-china.org/blog/2015/04/20/org-mode-%E5%AF%BC%E5%87%BA-html-%E6%97%B6%E5%88%A0%E9%99%A4%E4%B8%AD%E6%96%87%E4%B8%8E%E4%B8%AD%E6%96%87%E4%B9%8B%E9%97%B4%E5%A4%9A%E4%BD%99%E7%9A%84%E7%A9%BA%E6%A0%BC/
(defun simplesite--org-clear-space (text backend _info)
  "When export as HTML, delete extra space in multibyte language likd Chinese.
TEXT is the transcoded data as a string.
BACKEND is a symbal like 'html.
_INFO is communication channel, as a plist."
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]")
          (string text))
      ;; remove extra space
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
             "\\1\\2" string))
      ;; ;; remove space before bold word
      ;; (setq string
      ;;       (replace-regexp-in-string
      ;;        (format "\\(%s\\) +\\(<\\)" regexp)
      ;;        "\\1\\2" string))
      ;; ;; remove space after bold word
      ;; (setq string
      ;;       (replace-regexp-in-string
      ;;        (format "\\(>\\) +\\(%s\\)" regexp)
      ;;        "\\1\\2" string))
      string)))

(defun simplesite--check-config ()
  "Do some check before generating."
  (simplesite--debug "checking configuration...")
  (unless (and simplesite-source-directory
               (file-directory-p simplesite-source-directory))
    (error "Source directory `%s' is not exist"
           simplesite-source-directory))
  (unless (and simplesite-output-directory
               (file-directory-p simplesite-output-directory))
    (setq simplesite-output-directory
          (f-parent simplesite-source-directory)))
  (setq simplesite-source-directory (directory-file-name
                                     simplesite-source-directory))
  (setq simplesite-output-directory (directory-file-name
                                     simplesite-output-directory))
  (setq simplesite--dest-directory (concat simplesite-output-directory "/dest"))
  (if (s-starts-with-p simplesite--dest-directory simplesite-source-directory)
      (error "Source directory `%s' should not under `%s'"
             simplesite-source-directory simplesite--dest-directory)))

;;; debug
(defun simplesite--log-init()
  "Create or clear `simplesite-log-buffer'."
  (if (not (memq simplesite-log-level
                 '(SIMPLESITE-LOG-ERROR
                   SIMPLESITE-LOG-WARN
                   SIMPLESITE-LOG-INFO
                   SIMPLESITE-LOG-DEBUG)))
      (setq simplesite-log-level 'SIMPLESITE-LOG-INFO))
  (with-current-buffer (get-buffer-create simplesite-log-buffer)
    (toggle-truncate-lines -1)
    (erase-buffer)
    (set-buffer-modified-p nil)
    (display-buffer (current-buffer) 'not-this-window)
    (select-window (get-buffer-window (current-buffer)))))

(defun simplesite--log-quit ()
  "Quit simplesite log window."
  (let* ((buffer (get-buffer simplesite-log-buffer))
         (window (and buffer (get-buffer-window buffer))))
    (if window
      ;; save-selected-window prevents `quit-window' from changing the current
      ;; buffer (see https://github.com/flycheck/flycheck/issues/648).
      (save-selected-window
        (quit-window nil window)))))

(defun simplesite--log (level &rest args)
  "Log in LEVEL using ARGS.
LEVEL should be one of `SIMPLESITE-LOG-INFO-ERROR', `SIMPLESITE-LOG-INFO' or
`SIMPLESITE-LOG-DEBUG', if not, it will be ignored and use `SIMPLESITE-LOG-INFO'
as default log level."
  (if (not (memq level
                 '(SIMPLESITE-LOG-ERROR
                   SIMPLESITE-LOG-WARN
                   SIMPLESITE-LOG-INFO
                   SIMPLESITE-LOG-DEBUG)))
      (setq level 'SIMPLESITE-LOG-INFO))
  (if (<= (symbol-value level) (symbol-value simplesite-log-level))
      (let ((log-msg
             (concat (format-time-string "%Y-%m-%d %T " (current-time) t)
                     "["
                     (symbol-name level)
                     "]: "
                     (apply 'format args)
                     "\n")))
        (with-current-buffer (get-buffer-create simplesite-log-buffer)
          (goto-char (point-max))
          (insert log-msg)
          (redisplay))
        (if simplesite-log-message
            (apply 'message log-msg)))))

(defun simplesite--error (&rest args)
  "Log in error level using ARGS."
  (apply 'simplesite--log 'SIMPLESITE-LOG-ERROR args))

(defun simplesite--warn (&rest args)
  "Log in warning level using ARGS."
  (apply 'simplesite--log 'SIMPLESITE-LOG-WARN args))

(defun simplesite--info (&rest args)
  "Log in info level using ARGS."
  (apply 'simplesite--log 'SIMPLESITE-LOG-INFO args))

(defun simplesite--debug (&rest args)
  "Log in debug level using ARGS."
  (apply 'simplesite--log 'SIMPLESITE-LOG-DEBUG args))

;;; main process
(defun simplesite--do-generate ()
  "Generate site, this is the entrance function."
  (let ((progress-reporter
         (make-progress-reporter "[simplesite]: generating..." 0 100)))
    (simplesite--debug "begin generating")
    ;; check configurations
    (simplesite--check-config)
    ;; cleanup old files generated
    (simplesite--debug "clearing up old files generated...")
    (if (f-exists? simplesite--dest-directory)
        (f-delete simplesite--dest-directory t))
    (mkdir simplesite--dest-directory t)
    ;; Prepare  theme resource files
    (simplesite--debug "copying theme files...")
    (simplesite-prepare-theme simplesite-theme
                              simplesite-theme-directory
                              ;; simplesite-output-directory
                              simplesite--dest-directory
                              simplesite-installed-directory)
    (progress-reporter-update progress-reporter 5)
    (let* ((post-list (simplesite-parse-all-src-files
                       simplesite-source-directory
                       simplesite--dest-directory
                       progress-reporter))
           (category-list (simplesite--parse-categories post-list))
           (tag-list (simplesite--parse-tags post-list))
           (archive-list (simplesite--parse-archives post-list))

           (post-count (length post-list))
           (category-count (length category-list))
           (category-index-uri "/dest/categories/index.html")
           (tag-count (length tag-list))
           (tag-index-uri "/dest/tags/index.html")
           (archive-count (length archive-list))
           (archive-index-uri "/dest/archives/index.html")
           (avatar-uri (simplesite--get-avatar-uri))
           (i 0)

           (common-map (ht ("page-title" simplesite-site-title)
                           ("site-title" simplesite-site-title)
                           ("site-sub-title" simplesite-site-sub-title)
                           ("keywords" simplesite-site-main-keywords)
                           ("description" simplesite-site-main-desc)
                           ("author" simplesite-author)
                           ("year" (format-time-string "%Y" (current-time)))
                           ("post-count" post-count)
                           ("category-count" category-count)
                           ("tag-count" tag-count)
                           ("archive-count" archive-count)
                           ("category-uri" category-index-uri)
                           ("tag-count" tag-count)
                           ("tag-uri" tag-index-uri)
                           ("archive-count" archive-count)
                           ("archive-uri" archive-index-uri)
                           ("github-link" simplesite-personal-github-link)
                           ("twitter-link" simplesite-personal-twitter-link)
                           ("weibo-link" simplesite-personal-weibo-link)
                           ("douban-link" simplesite-personal-douban-link)
                           ("zhihu-link" simplesite-personal-zhihu-link)
                           ("avatar-uri" avatar-uri)
                           ("about-uri" (if (f-exists?
                                             (concat simplesite-source-directory
                                                     "/about.org"))
                                            "/dest/posts/about/index.html"
                                          nil))
                           ("site-domain" simplesite-site-domain)
                           ("show-comment" nil)
                           ("disqus-shortname"
                            simplesite-personal-disqus-shortname)
                           ("disqus-comment"
                            (if simplesite-personal-disqus-shortname t nil))
                           ("duoshuo-shortname"
                            simplesite-personal-duoshuo-shortname)
                           ("duoshuo-comment"
                            (if (and (not simplesite-personal-disqus-shortname)
                                     simplesite-personal-duoshuo-shortname)
                                t nil))
                           )))

      (setq post-list (-map #'(lambda (post)
                                (ht-merge common-map post))
                            post-list))
      ;; generate posts
      (mapc #'(lambda (post)
                (when (ht-get post "post-content")
                  (ht-set post "post-category-uri"
                          (format "/dest/categories/%s/index.html"
                                  (ht-get post "category")))
                  (simplesite-generate-post post)
                  (setq i (+ i 1))
                  (progress-reporter-update progress-reporter
                                            (+ 55 (/ (* 40 i) post-count)))
                  ;; release post content to save memory
                  (ht-remove post "post-content")
                  (ht-remove post "content")))
            post-list)

      (simplesite-generate-index (-take simplesite-index-posts-max post-list)
                                 (ht-copy common-map))
      (progress-reporter-update progress-reporter 95)
      (simplesite-generate-categories post-list (ht-copy common-map))
      (progress-reporter-update progress-reporter 96)
      (simplesite-generate-tags post-list (ht-copy common-map))
      (progress-reporter-update progress-reporter 98)
      (simplesite-generate-archives post-list (ht-copy common-map))
      (progress-reporter-update progress-reporter 99)
      (progress-reporter-done progress-reporter)
      (simplesite--info "Generating successfully!"))))

;; (defun simplesite--org-export-link-filter (transcoded backend channel)
;;   "TODO: process links in org file."
;;   (simplesite--debug "transcoded: %s" transcoded)
;;   (simplesite--debug "backend: %s" backend)
;;   nil)

;;;###autoload
(defun simplesite-generate ()
  "Activate some advice before generating and deactivate them after it."
  (interactive)
  (let ((footnote-format-backup org-html-footnote-format))
    ;; prepare for generating
    (simplesite--log-init)
    (ad-activate 'org-html-link)
    ;; (add-to-list 'org-export-filter-link-functions
    ;;              'simplesite--org-export-link-filter)
    (setq org-html-footnote-format "<sup>[%s]</sup>")
    (add-to-list 'org-export-filter-paragraph-functions
                 'simplesite--org-clear-space)
    (with-demoted-errors "Warning: %S"
      (simplesite--do-generate)
      (if (y-or-n-p "Generation done, preview it? ")
          (if (not (featurep 'simple-httpd))
              (simplesite--warn "package simple-httpd not installed")
            (httpd-serve-directory simplesite-output-directory)
            (message "please browse 'http://127.0.0.1:8080' to preview")
            (browse-url "http://127.0.0.1:8080")))
      (simplesite--log-quit))
    ;; after generating, restore environment
    (ad-deactivate 'org-html-link)
    (setq org-html-footnote-format footnote-format-backup)
    (setq org-export-filter-paragraph-functions
          (delq 'simplesite--org-clear-space
                org-export-filter-paragraph-functions))))

(provide 'simplesite)

;;; simplesite.el ends here
