;;; simplesite.el --- Simple static site generator -*- lexical-binding: t -*-

;; Copyright 2015 Jerry Xu
;;
;; Author: GuanghuiXu gh_xu@qq.com
;; Maintainer: Jerry Xu gh_xu@qq.com
;; Created: 28 Apr 2015
;; Version: 0.1
;; Keywords: blog, generator
;; Homepage: https://github.com/jerryxgh/simplesite
;; Package-Requires: ((org "8.0") (f "0.17.3")
;; (s) (ht "0.9") (dash "1.2.0") (mustache "0.23"))
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Simple static site generator.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'simplesite)

;;; Change Log:

;; Version 0.1  2014/12/04 GuanghuiXu
;;   - Initial release

;;; Code:

(require 'ss-backends)
(require 'ss-index)
(require 'ss-post)
(require 'ss-tags)
(require 'ss-archives)
(require 'ss-categories)
(require 'ss-org-backend)

(defconst simplesite-version "0.1")

(defun ss-generate ()
  "Generate site."
  (interactive)
  ;; check important variables
  (message "Checking configuration...")
  (ss--check-variables)
  ;; copy theme resource files
  (message "Copying theme files...")
  (ss-prepare-theme ss-theme
                    ss-theme-directory
                    ss-dist-directory
                    ss-load-directory)
  (let ((file-tlist (ss-parse-all-src-files ss-source-directory
                                            ss-dist-directory)))
    ;; generate post if the file is changed, then release post-content
    (mapc #'(lambda (attr-table)
              (when (ht-get attr-table "post-content")
                (ss-generate-post attr-table)
                (ht-remove attr-table "post-content")
                (ht-remove attr-table "content")))
          file-tlist)

    (ss-generate-index file-tlist)
    (ss-generate-categories file-tlist)
    (ss-generate-tags file-tlist)
    (ss-generate-archives file-tlist)
    (mapc #'(lambda (attr-table)
              (message (ht-get attr-table "date")))
          file-tlist)
    (message "Generating successfully!")))

(defun ss--check-variables ()
  "Do some check before generate site."
  (unless (and ss-source-directory
               (file-directory-p ss-source-directory))
    (error "Directory `%s' is not properly configured" ss-source-directory))
  (unless (and ss-dist-directory
               (file-directory-p ss-dist-directory))
    (setq ss-dist-directory
          (f-parent ss-source-directory)))
  (setq ss-source-directory (directory-file-name ss-source-directory))
  (setq ss-dist-directory (directory-file-name ss-dist-directory)))

(defun ss--correct-links (post-content)
  "Correct links in exported POST-CONTENT.

TODO: not implemented."
  post-content)

(provide 'simplesite)

;;; simplesite.el ends here
