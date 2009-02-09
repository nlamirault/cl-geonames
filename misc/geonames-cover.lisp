;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          geonames-cover.lisp
;;;; Purpose:       cl-geonames code coverage.
;;;; Programmer:    Nicolas Lamirault <nlamirault@gmail.com>
;;;;
;;;; This file, part of cl-geonames, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-geonames users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************

(require :asdf)
(require :sb-cover)



(declaim (optimize sb-cover:store-coverage-data))

(asdf:oos 'asdf:load-op :cl-geonames-test)    

(cl-geonames-test:run-cl-geonames-tests)

(let* ((path (namestring
              (asdf:component-relative-pathname (asdf:find-system :cl-geonames))))
       (dir (concatenate 'string path "/www/coverage/")))
  (ensure-directories-exist dir)
  (sb-cover:report dir))
     
(declaim (optimize (sb-cover:store-coverage-data 0)))
