;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api.lisp
;;;; Purpose:       cl-geonames Common Lisp API
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



(defun geo-perform (url &key (type :xml))
  "Make a query to Geonames.
@arg[url]{the URL to the Geonames request}
@arg[type]{is the format type of the returned document. It could be :xml or :json}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
  (let ((request (format nil "~A&type=~A" url type)))
    (when *debug*
      (format t "Geonames request: ~A~%" request))
    (multiple-value-bind (body-stream status-code headers uri stream must-close)
        (drakma:http-request request)
      (declare (ignore headers stream must-close uri))
      (if (and status-code (= status-code 200))
          (cond ((equal type :json)
                 (json:decode-json-from-string body-stream))
                ((equal type :xml)
                 (s-xml:parse-xml-string body-stream))
                ((equal type :plain)
                 body-stream)
                (t (error 'geonames-output-error :output type)))
          (error 'geonames-request-error :code status-code :message body-stream)))))


;; Macro for Geonames requests.

(defmacro with-geonames-stream (stream type &body body)
  "Macro which creates an input stream, a HTTP url, add parameters executing body,
and performs the HTTP request defined by uri."
  `(let (uri)
     (with-output-to-string (,stream)
       ,@body
       (setf uri (get-output-stream-string ,stream)))
    (geo-perform uri :type ,type)))


(defun geo-search (query name exact-name &key country codes fclass
                   fcode language (max-rows 100) (start-row 0)
                   (style :medium) (type :xml))
  "@short{Returns the names found for the searchterm, the search is using an AND operator.}
@arg[query]{is the search over all attributes of a place : place name, country name,
continent, admin codes,...}
@arg[exact-name]{is the exact place name.}
@arg[country]{is a list of country code (country code or ISO-3166).}
@arg[max-rows]{is the maximal number of rows in the document returned
by the service. Default is 100, the maximal allowed value is 1000.}
@arg[start-row]{is used for paging results. If you want to get results 30 to 40,
use start-row=30 and max-rows=10. Default is 0.}
@arg[fclass]{is a list of the feature classes (character A,H,L,P,R,S,T,U,V).}
@arg[fcode]{is a list of all the feature code(s).}
@arg[language]{specify in which language place name and country name will be
returned.}
@arg[type]{is the format type of the returned document.}
@arg[style]{specify the verbosity of returned xml document. It could be :short,
:medium, :long or :full.}
@return{A list of names}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
  (with-geonames-stream os type
    ;;(format os "~A" *geonames-server*)
      ;;(format os "~A" *geonames-search*)
    (format os "~A" +geonames-search+)
    (format os "q=~A&name=~A&name_equals=~A" query name exact-name)
    (when max-rows
      (if (<= max-rows 1000)
          (format os "&maxRows=~A" max-rows)
          (error 'geonames-query-error
                 :message (format nil "MAX-ROWS must be inferior to 1000 : ~A"
                                  max-rows))))
    (when start-row
      (format os "&startRow=~A" start-row))
    (when country
      (format os "~{&country=~A~}" country))
    (when codes
      (loop for code in codes
            as i = 1 then (1+ i)
            do (format os "&adminCode~A=~A" i code)))
    (when fclass
      (format os "~{&fclass=~A~}" fclass))
    (when fcode
      (format os "~{&fcode=~A~}" fcode))
    (when language
      (format os "&lang=~A" language))
    (when style
      (format os "&style=~A" style))))
;;;     (setf url (get-output-stream-string os)))
;;;     (geo-perform url :type type)))


(defun geo-postal-code-search (postal-code place-name &key country (max-rows 100)
                               (style :medium) (type :xml))
  "@short{A list of postal codes and places for the POSTAL-CODE / PLACE-NAME query.}
@arg[postal-code]{is the postal code to search.} ;
@arg[place-name]{is all fields : placename,postal code, country, admin name.}
@arg[country]{is a list of country code (country code or ISO-3166).}
@arg[max-rows]{is the maximal number of rows in the document returned by the service.
The maximal allowed value is 1000.}
@arg[type]{is the format type of the returned document.}
@arg[style]{specify the verbosity of returned xml document. It could be :short,
:medium, :long or :full.}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
;;;       (format os "~A" *geonames-server*)
;;;       (format os "~A" *geonames-postal-code-search*)
  (with-geonames-stream os type
    (format os "~A" +geonames-postal-code-search+)
    (when postal-code
      (format os "&postalcode=~A" postal-code))
    (when place-name
      (format os "&placename=~A" place-name))
    (when country
      (format os "~{&country=~A~}" country))
    (when max-rows
      (if (<= max-rows 1000)
          (format os "&maxRows=~A" max-rows)
          (error 'geonames-query-error
                 :message (format nil "MAX-ROWS must be inferior to 1000 : ~A"
                                  max-rows))))
    (when style
      (format os "&stylep=~A" style))
;;;     (setf url (get-output-stream-string os)))
;;;   (geo-perform url :type type)))
    ))


(defun geo-placename-lookup (postal-code &key country (max-rows 20) style)
  "@short{A list of places for the given postalcode (in JSON format).}
@arg[postal-code]{is the postal code to search.}
@arg[country]{is a list of country code (country code or ISO-3166).}
@arg[max-rows]{is the maximal number of rows in the document returned by the
service. The maximal allowed value is 1000.}
@arg[style]{specify the verbosity of returned xml document. It could be :short,
:medium, :long or :full.}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
;;;       (format os "~A" *geonames-server*)
;;;       (format os "~A" *geonames-placename-lookup*)
  (with-geonames-stream os :json
    (format os "~A" +geonames-placename-lookup+)
    (format os "postalcode=~A" postal-code)
    (when country
      (format os "~{&country=~A~}" country))
    (when max-rows
      (if (<= max-rows 1000)
          (format os "&maxRows=~A" max-rows)
          (error 'geonames-query-error
                 :message (format nil "MAX-ROWS must be inferior to 1000 : ~A"
                                  max-rows))))
    (when style
      (format os "&stylep=~A" style))
;;;     (setf url (get-output-stream-string os)))
;;;     (geo-perform url :type :json)))
    ))


(defun geo-find-nearby-postal-code (postal-code &key country radius (max-rows 5)
                                    (type :xml))
  "@short{A list of postalcodes and places for the POSTAL-CODE query.}
@arg[postal-code]{is the postal code to search}
@arg[country]{is a list of country code (country code or ISO-3166).}
@arg[radius]{represents kilometers.}
@arg[max-rows]{is the maximal number of rows in the document returned by the
service.}
@arg[type]{is the format type of the returned document.}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
;;;       (format os "~A" *geonames-server*)
;;;       (format os "~A" *geonames-find-near-postal-code*)
  (with-geonames-stream os type
    (format os "~A" +geonames-find-near-postal-code+)
    (format os "postalcode=~A" postal-code)
    (when country
      (format os "~{&country=~A~}" country))
    (when radius
      (format os "&radius=~A" radius))
    (when max-rows
      (format os "&maxRows=~A" max-rows))
;;;     (setf url (get-output-stream-string os)))
;;;     (geo-perform url :type type)))
    ))


(defun geo-find-nearby-postal-code-geocoding (latitude longitude
                                               &key country radius (max-rows 5)
                                               (style :medium) (type :xml))
  "@short{A list of postalcodes and places for the LATITUDE / LONGITUDE query.}
@arg[latitude]{the geographic coordinate.}
@arg[longitude]{the geographic coordinate.}
@arg[country]{is a list of country code (country code or ISO-3166).}
@arg[radius]{represents kilometers.}
@arg[max-rows]{is the maximal number of rows in the document returned by
the service.}
@arg[style]{specify the verbosity of returned xml document. It could be :short,
:medium, :long or :full.}
@arg[type]{is the format type of the returned document.}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
;;;       (format os "~A" *geonames-server*)
;;;       (format os "~A" *geonames-find-near-postal-code*)
  (with-geonames-stream os type
    (format os "~A" +geonames-find-near-postal-code+)
    (format os "lat=~A&lng=~A" latitude longitude)
    (when country
      (format os "~{&country=~A~}" country))
    (when radius
      (format os "&radius=~A" radius))
    (when max-rows
      (format os "&maxRows=~A" max-rows))
    (when style
      (format os "&style=~A" style))
;;;     (setf url (get-output-stream-string os)))
;;;     (geo-perform url :type type)))
    ))


(defun geo-postal-code-country-info ()
  "@short{Countries for which postal code geocoding is available.}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
;;;       (format os "~A" *geonames-server*)
;;;       (format os "~A" *geonames-postal-code-country*)
  (with-geonames-stream os :xml
    (format os "~A" +geonames-postal-code-country+)
;;;     (setf url (get-output-stream-string os)))
;;;     (geo-perform url :type :xml)))
    ))


(defun geo-find-nearby-place-name (latitude longitude &key radius (max-rows 10)
                                   (style :medium) (type :xml))
  "@short{A list of postalcodes and places for the LATITUDE / LONGITUDE query.}
@arg[latitude]{the geographic coordinate.}
@arg[longitude]{the geographic coordinate.}
@arg[radius]{represents kilometers}.
@arg[style]{specify the verbosity of returned xml document. It could be :short,
:medium, :long or :full.}
@arg[type]{is the format type of the returned document.}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
;;;       (format os "~A" *geonames-server*)
;;;       (format os "~A" *geonames-find-near-place-name*)
  (with-geonames-stream os type
    (format os "~A" +geonames-find-near-place-name+)
    (format os "lat=~A&lng=~A" latitude longitude)
    (when radius
      (format os "&radius=~A" radius))
    (when max-rows
      (format os "&maxRows=~A" max-rows))
    (when style
      (format os "&style=~A" style))
;;;     (setf url (get-output-stream-string os)))
;;;   (geo-perform url :type type)))
    ))


(defun geo-country-info (&key country language)
  "@short{Retrieve informations about country :
Bounding Box, Capital, Area in square km, Population.}
@arg[country]{is a list of country code (country code or ISO-3166)}
@arg[language]{specify in which language place name and country name 
will be returned.}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
;;;       (format os "~A" *geonames-server*)
;;;       (format os "~A" *geonames-country-info*)
  (with-geonames-stream os :xml
    (format os "~A" +geonames-country-info+)
    (when country
      (format os "~{&country=~A~}" country))
    (when language
      (format os "&lang=~A" language))
;;;     (setf url (get-output-stream-string os)))
;;;     (geo-perform url :type :xml)))
    ))


(defun geo-country-code (latitude longitude &key (style :medium) (type :xml))
  "@short{The iso country code for the given LATITUDE/LONGITUDE.}
@arg[latitude]{the geographic coordinate.}
@arg[longitude]{the geographic coordinate.}
@arg[style]{specify the verbosity of returned xml document. It could be :short,
:medium, :long or :full.}
@arg[type]{is the format type of the returned document.}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
;;;       (format os "~A" *geonames-server*)
;;;       (format os "~A" *geonames-country-code*)
  (with-geonames-stream os type
    (format os "~A" +geonames-country-code+)
    (format os "&lat=~A&lng=~A" latitude longitude)
    (when style
      (format os "&style=~A" style))
;;;     (setf url (get-output-stream-string os)))
;;;     (geo-perform url :type type)))
    ))


(defun geo-country-subdivision (latitude longitude &key language)
  "@short{The country and the administrative subdivison (state, province,...) 
for the given LATITUDE/LONGITUDE.}
@arg[latitude]{the geographic coordinate.}
@arg[longitude]{the geographic coordinate.}
@arg[language]{specify in which language place name and country 
name will be returned.}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
;;;       (format os "~A" *geonames-server*)
;;;       (format os "~A" *geonames-country-subdivision*)
  (with-geonames-stream os :xml
    (format os "~A" +geonames-country-subdivision+)
    (format os "&lat=~A&lng=~A" latitude longitude)
    (when language
      (format os "&lang=~A" language))
;;;     (setf url (get-output-stream-string os)))
;;;     (geo-perform url)))
    ))


(defun geo-elevation-srtm3 (latitude longitude)
  "@short{A single number giving the elevation in meters according to srtm3,
ocean areas have been masked as 'no data' and have been assigned a value of
-32768.}
@arg[latitude]{the geographic coordinate.}
@arg[longitude]{the geographic coordinate.}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
;;;       (format os "~A" *geonames-server*)
;;;       (format os "~A" *geonames-elevation-srtm3*)
  (with-geonames-stream os :plain
    (format os "~A" +geonames-elevation-srtm3+)    
    (format os "&lat=~A&lng=~A" latitude longitude)
;;;     (setf url (get-output-stream-string os)))
;;;     (geo-perform url :type :plain)))
    ))


(defun geo-elevation-gtopo30 (latitude longitude)
  "@short{A single number giving the elevation in meters according to gtopo30,
ocean areas have been masked as 'no data' and have been assigned a value of -9999.}
@arg[latitude]{the geographic coordinate.}
@arg[longitude]{the geographic coordinate.}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
;;;       (format os "~A" *geonames-server*)
;;;       (format os "~A" *geonames-elevation-gtopo30*)
  (with-geonames-stream os :plain
    (format os "~A" +geonames-elevation-gtopo30+)
    (format os "&lat=~A&lng=~A" latitude longitude)
;;;     (setf url (get-output-stream-string os)))
;;;     (geo-perform url :type :plain)))
    ))


(defun geo-timezone (latitude longitude &key (type :xml))
  "@short{Get the timezone at the LATITUDE/LONGITUDE with gmt offset (1. January)
and dst offset (1. July)}.
@arg[latitude]{the geographic coordinate.}
@arg[longitude]{the geographic coordinate.}
@arg[type]{is the format type of the returned document.}
@see-condition{geonames-output-error}
@see-condition{geonames-request-error}"
;;;   (let (url)
;;;     (with-output-to-string (os)
;;;       (format os "~A" *geonames-server*)
;;;       (if (equal type :json)
;;;           (format os "~A" (format nil *geonames-timezone* type))
;;;           (format os "~A" (format nil *geonames-timezone* "")))
;;;       (format os "&lat=~A&lng=~A" latitude longitude)
;;;       (setf url (get-output-stream-string os)))
;;;     (geo-perform url :type type)))
  (with-geonames-stream os type
    (format os "~A" (format nil +geonames-timezone+ type))
    (format os "&lat=~A&lng=~A" latitude longitude)))
