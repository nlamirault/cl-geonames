;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       cl-geonames specials informations.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-geonames, is Copyright (c) 2006 by Nicolas Lamirault
;;;;
;;;; cl-geonames users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :cl-geonames)




(eval-when (:compile-toplevel :load-toplevel :execute)

  (unless (boundp '+geonames-server+)
    (defconstant +geonames-server+ "http://ws.geonames.org"
      "The Geonames web service."))

  (defmacro def-geonames-uri (name uri &optional doc)
    `(unless (boundp ',name)
       (defconstant ,name
         (concatenate 'string
                      +geonames-server+
                      ,uri)
        ,@(when doc
            (list doc))))))


;;(defparameter *geonames-server* "http://ws.geonames.org")


;;(defparameter *geonames-search* "/search?"
(def-geonames-uri +geonames-search+ "/search?"
  "Web service for the geonames full text search")


;;(defparameter *geonames-postal-code-search* "/postalCodeSearch?"
(def-geonames-uri +geonames-postal-code-search+ "/postalCodeSearch?"
  "Web service to retreive postal codes and places.")


;;(defparameter *geonames-placename-lookup* "/postalCodeLookup?"
(def-geonames-uri +geonames-placename-lookup+ "/postalCodeLookup?"
  "Web service to retrieve placename lookup with postal code.")


;;(defparameter *geonames-find-near-postal-code* "/findNearbyPostalCodes?"
(def-geonames-uri +geonames-find-near-postal-code+ "/findNearbyPostalCodes?"
  "Web service to find nearby postal codes / reverse geocoding")


;;(defparameter *geonames-postal-code-country* "/postalCodeCountryInfo?"
(def-geonames-uri +geonames-postal-code-country+ "/postalCodeCountryInfo?"
  "Web service to retrieve countries for which postal code geocoding is available.")


;;(defparameter *geonames-find-near-place-name* "/findNearbyPlaceName?"
(def-geonames-uri +geonames-find-near-place-name+ "/findNearbyPlaceName?"
  "Web service to find nearby place name / reverse geocoding")


;;(defparameter *geonames-country-info* "/countryInfo?"
(def-geonames-uri +geonames-country-info+ "/countryInfo?"
  "Web service to retrieve informations about countries")


;;(defparameter *geonames-country-code* "/countrycode?"
(def-geonames-uri +geonames-country-code+ "/countrycode?"
  "Web service to retreive country code from latitude/longitude.")


;;(defparameter *geonames-country-subdivision* "/countrySubdivision?"
(def-geonames-uri +geonames-country-subdivision+ "/countrySubdivision?"
  "Web service to retreive the administrative subdivison (state, province,...).")


;;(defparameter *geonames-elevation-srtm3* "/srtm3?"
(def-geonames-uri +geonames-elevation-srtm3+ "/srtm3?"
  "Web service to retreive the elevation in meters according to srtm3.")


;;(defparameter *geonames-elevation-gtopo30* "/gtopo30?"
(def-geonames-uri +geonames-elevation-gtopo30+ "/gtopo30?"
  "Web service to retreive the elevation in meters according to gtopo30.")


;;(defparameter *geonames-timezone* "/timezone?"
(def-geonames-uri +geonames-timezone+ "/timezone?"
  "Web service to retreive the timezone at the lat/lng.")



(defparameter *debug* nil "If T, active some logs.")