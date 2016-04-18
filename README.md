# simplesite
[![License GPL 2](https://img.shields.io/badge/license-GPL_2-green.svg)](http://www.gnu.org/licenses/gpl-2.0.txt)

simplesite is an emacs plugin, it is used to convert your org files into an
static web site, then you can use apache or nginx like servers to server it, or
use github pages to publish it.

## Installation

If you would like to install the package manually, download or clone it and put
on Emacs' `load-path`, then you can require it in your init file like this:

    (require 'simplesite)

## Usage

Add this to .emacs:

    (setq simplesite-author "<your name>"
          simplesite-personal-avatar "<path to your avatar image>"
          simplesite-source-directory "<path to org source file directory>"
          simplesite-output-directory "<path to output directory>"
          simplesite-personal-github-link "<github link>"
          simplesite-site-domain "http://jerryxgh.github.io"
     )
then set comment shortname:
     (setq simplesite-personal-disqus-shortname "<shortname in disqus>")
or
     (setq simplesite-personal-duoshuo-shortname "<shortname in duoshuo>")

run <kbd>M-x simplesite-generate</kbd> to generate static site.

If you have installed `simple-httpd`, then you can preview your site by:
<kbd>M-x httpd-serve-directory</kbd>, then choose `simplesite-output-directory`
to server, then open browse http://localhost:8080, you can see it.

Enjoying!


## License
Copyright (C) 2016 GuanghuiXu

Distributed under GNU GPL, version 2.
