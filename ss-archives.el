;;; ss-archives.el --- generate archives page -*- lexical-binding: t -*-

;; Copyright 2015 Jerry Xu
;;
;; Author: Jerry Xu gh_xu@qq.com
;; Maintainer: Jerry Xu gh_xu@qq.com
;; Version: 0.0
;; Keywords: blog, static site, generator
;; Homepage: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(require 'ss-options)
(require 'ss-theme)
(require 'color)

(defun ss-generate-archives (file-tlist)
  "Generate archives of all FILE-TLIST.

FILE-TLIST: hash table list of all source files."
  (let ((mustache-partial-paths
         (list (ss-get-theme-template-dir ss-theme ss-theme-directory)))
        (output-dir (concat ss-dist-directory "/archives")))
    (if (not (file-directory-p output-dir))
        (mkdir output-dir t))
    (f-write
     (mustache-render
      (f-read (concat (ss-get-theme-template-dir ss-theme ss-theme-directory)
                      "layout.mustache"))
      (ht ("page-title" (concat "Archives - " ss-site-title))
          ("site-title" ss-site-title)
          ("site-sub-title" ss-site-sub-title)
          ("content" (ss--render-archives-index-content file-tlist))
          ("keywords" ss-site-main-keywords)
          ("description" ss-site-main-desc)
          ("author" ss-author)))
     'utf-8
     (concat output-dir "/index.html"))))

(defun ss--group-file-by-year (file-tlist)
  "Group FILE-TLIST by year and change date of file from yyyy-MM-dd to MM-dd."
  (let ((year-group (ht-create)))
    (mapc #'(lambda ()
              )
          file-tlist)))

(defun ss--render-archives-index-content (file-tlist)
  "Render archives index content based on FILE-TLIST."
  (mustache-render
   (f-read (concat (ss-get-theme-template-dir ss-theme ss-theme-directory)
                   "archives-index.mustache"))
   (ht ("archives" file-tlist)
       ("year-group" (length file-tlist)))))

(provide 'ss-archives)

;;; ss-archives.el ends here
