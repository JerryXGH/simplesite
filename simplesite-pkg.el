;;; simplesite-pkg.el --- simplesite package file -*- lexical-binding: t -*-

;; Copyright 2015 Jerry Xu
;;
;; Author: GuanghuiXu gh_xu@qq.com
;; Maintainer: GuanghuiXu gh_xu@qq.com
;; Created: 2015-4-28
;; Version: 0.1
;; Keywords: blog
;; Homepage: https://github.com/jerryxgh/simplesite
;; Package-Version: 0.1
;; Package-Requires: ((emacs "24") (org "8.0") (f "0.17.3") (mustache "0.23"))
;;

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Package file for simplesite.

(define-package "simplesite" "0.1-alpha" "Simple static site generator"
  '((dash "2.12.1")
    (ht "2.0")
    (s "1.10.0")
    (mustache "0.23")
    (f "0.17.3")
    (org "8.0")))

;;; simplesite-pkg.el ends here
