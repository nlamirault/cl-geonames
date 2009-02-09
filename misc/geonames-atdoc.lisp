;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          geonames-atdoc.lisp
;;;; Purpose:       cl-geonames documentation tool
;;;; Programmer:    Nicolas Lamirault <nlamirault@gmail.com>
;;;;
;;;; This file, part of cl-geonames, is Copyright (c) 2009 by Nicolas Lamirault
;;;;
;;;; cl-geonames users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************

(require :asdf)
(asdf:oos 'asdf:load-op :atdoc)
(asdf:oos 'asdf:load-op :cl-geonames)


(let* ((path (namestring
              (asdf:component-relative-pathname (asdf:find-system :cl-geonames))))
       (dir (concatenate 'string path "/www/api/")))
       (ensure-directories-exist dir)
  (atdoc:generate-html-documentation '(:cl-geonames)
                                     dir
                                     :index-title "cl-geonames API reference"
                                     :heading "Geonames for Common Lisp"
                                     ;;:css "orange-sans.css"
                                     :single-page-p t
                                     :include-internal-symbols-p nil)
  (atdoc:generate-latex-documentation '(:cl-geonames)
                                      dir
                                      :title "cl-geonames API reference")
  (atdoc:generate-info-documentation '(:cl-geonames)
                                     dir
                                     :name "cl-geonames"
                                     :title "cl-geonames API referenc"))
