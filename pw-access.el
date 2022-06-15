;;; pw-access.el --- PipeWire access  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Milan Zamazal <pdm@zamazal.org>

;; COPYRIGHT NOTICE
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; A generic interface for communication with PipeWire (https://pipewire.org).
;; It abstracts communication with PipeWire to be backend independent.
;; Only functions from this module may communicate with PipeWire.
;;
;; Currently, pw-cli is used to talk to PipeWire.  This is not optimal
;; because pw-cli doesn't seem to have documented output format and
;; the format changes accross PipeWire versions.  But there seem to be
;; no better options currently.  This module should allow switching to
;; other communication means easily, without any changes needed
;; outside this module, except for using a different communication
;; class.

(require 'eieio)

(defclass pw-accessor ()
  ()
  :documentation
  "Base PipeWire interface class.
All PipeWire interfaces should derive from this class.")

(cl-defgeneric pw-access-objects (class)
  "Return all the objects currently reported by PipeWire.
It is a list of object data.  Each of the elements has a form
(OBJECT-ID . INFO) where OBJECT-ID is a numeric OBJECT-ID as
reported by PipeWire and INFO is an association list of items
(NAME . VALUE) where NAME is a string item name as reported by
PipeWire and VALUE is the corresponding value.  VALUE is a number for
object ids, a string otherwise.
A special entry with `type' symbol as its name contains the PipeWire
type of the objects, as a string (e.g. \"Device\", \"Node\", \"Port\",
 \"Client\", ...).")

(cl-defgeneric pw-access-properties (class node-id)
  "Return properties of the given node.
NODE-ID is a numeric PipeWire Node id (other kinds of PipeWire objects
are not supported in this method).
Object properties may be, unlike object info items, settable.

An assocation list is returned.  Each list element is of the form
(PROPERTY . VALUE) where PROPERTY is a string name of the given
property.  VALUE can be:

- \"true\" or \"false\" for boolean values (t and nil are not used to
  avoid confusion with nil representing invalid or unavailable value).
- A number for numeric values (ids, integers, floats).
- A string for string values.
- A list of elements of any of these types for arrays and structs.")

(cl-defgeneric pw-access-set-properties (class node-id properties)
  "Set PROPERTIES of the given node.
NODE-ID is a numeric PipeWire Node id (other kinds of PipeWire objects
are not supported in this method).
PROPERTIES is an association list in the same format as in
`pw-access-properties'.  It needn't contain all the properties, just
the properties to be changed.")

(cl-defgeneric pw-access-current-profile (class device-id)
  "Return current profile of the given device.
DEVICE-ID is a numeric PipeWire Device id (other kinds of PipeWire
objects are not supported in this method).

The profile is an association list with elements of the form
(PROPERTY . VALUE), in the same format as properties in
`pw-access-properties'.")

(cl-defgeneric pw-access-profiles (class device-id)
  "Return available profiles of the given device.
DEVICE-ID is a numeric PipeWire Device id (other kinds of PipeWire
objects are not supported in this method).

Return a list of profiles, which are in the same format as in
`pw-access-current-profile'.")

(cl-defgeneric pw-access-set-profile (class device-id profile-index)
  "Set the profile of the given device.
DEVICE-ID is a numeric PipeWire Device id (other kinds of PipeWire
objects are not supported in this method).
PROFILE-INDEX is a numeric index of the profile to set, as returned
from PipeWire.")

(cl-defgeneric pw-access-defaults (class)
  "Return default sinks and sources.
An association lists is returned.  Each list element is of the form
(KEY . NAME) where KEY is a string identifying the given kind of
default sink or source as reported by PipeWire and NAME is a string
name of the node assigned to the default.")

(cl-defgeneric pw-access-set-default (class key node-name)
  "Set default sink or source.
KEY is a string identifying the given kind of default sink or source
as reported in `pw-access-defaults' and NODE-NAME is a string name of
the node that should be assigned to KEY.")

;; pw-cli interface

(defvar pw-cli-command "pw-cli"
  "pw-cli command to use.")

(defvar pw-cli-metadata-command "pw-metadata"
  "pw-metadata command to use.")

(defclass pw-cli-accessor (pw-accessor)
  ()
  :documentation
  "pw-cli based interface to PipeWire.
Note this interface may not work with all PipeWire versions.")

(defun pw-cli--command (command args)
  (apply #'call-process command nil t nil args)
  (goto-char (point-min)))

(defun pw-cli--next-line ()
  (goto-char (line-beginning-position 2)))

(defun pw-cli--parse-list ()
  (let ((objects '()))
    (while (re-search-forward "^[\t]id \\([0-9]+\\), type PipeWire:Interface:\\(.*\\)/.*$" nil t)
      (let ((id (string-to-number (match-string 1)))
            (properties `((type . ,(match-string 2)))))
        (pw-cli--next-line)
        (while (looking-at "^ [\t][\t]\\([a-z.]+\\) = \"\\(.*\\)\"")
          (let ((property (match-string 1))
                (value  (match-string 2)))
            (when (string-suffix-p ".id" property)
              (setq value (string-to-number value)))
            (push (cons property value) properties))
          (pw-cli--next-line))
        (push (cons id properties) objects)))
    (nreverse objects)))

(cl-defmethod pw-access-objects ((_class pw-cli-accessor))
  (with-temp-buffer
    (pw-cli--command pw-cli-command '("list-objects"))
    (pw-cli--parse-list)))

(defun pw-cli--read-property (&optional nesting)
  (unless nesting
    (setq nesting 0))
  (when (looking-at (concat (make-string (+ 6 (* 2 nesting)) ? )
                            "\\([A-Za-z:]+\\) \\(.*\\)"))
    (let ((type (match-string 1))
          (value (match-string 2)))
      (pcase type
        ("Bool"
         (if (equal value "true") 'true 'false))
        ((or "Float" "Id" "Int")
         (string-to-number value))
        ("String"
         (substring value 1 -1))
        ((or "Array:" "Struct:")
         (let ((array '())
               item)
           (pw-cli--next-line)
           (while (setq item (pw-cli--read-property (1+ nesting)))
             (push item array)
             (pw-cli--next-line))
           (nreverse array)))))))
  
(defun pw-cli--parse-properties ()
  (pw-cli--next-line)
  (let ((end (or (save-excursion (re-search-forward "^  Object:" nil t))
                 (point-max)))
        (properties '()))
    (while (and (< (point) end)
                (re-search-forward "^    Prop: key \\([A-Za-z:]+\\)" end t))
      (pw-cli--next-line)
      (let ((property (car (last (split-string (match-string 1) ":"))))
            (value (pw-cli--read-property)))
        (when value
          (push (cons property value) properties))))
    (goto-char end)
    properties))

(cl-defmethod pw-access-properties ((_class pw-cli-accessor) node-id)
  (with-temp-buffer
    (pw-cli--command pw-cli-command `("enum-params" ,(number-to-string node-id) "Props"))
    (pw-cli--parse-properties)))

(defun pw-cli--format-property-value (value)
  (cond
   ((consp value)
    (concat "[ " (mapconcat #'pw-cli--format-property-value value ", ") " ]"))
   ((numberp value)
    (number-to-string value))
   (t
    value)))

(defun pw-cli--format-property (property)
  (format "%s: %s" (car property) (pw-cli--format-property-value (cdr property))))

(defun pw-cli--set-parameter (object-id parameter value)
  (let* ((formatted (mapconcat #'pw-cli--format-property value ", "))
         (param-value (concat "{ " formatted " }")))
    (call-process pw-cli-command nil pw-cli-command nil
                  "set-param" (number-to-string object-id) parameter param-value)))

(cl-defmethod pw-access-set-properties ((_class pw-cli-accessor) node-id properties)
  (pw-cli--set-parameter node-id "Props" properties))

(cl-defmethod pw-access-current-profile ((_class pw-cli-accessor) device-id)
  (with-temp-buffer
    (pw-cli--command pw-cli-command `("enum-params" ,(number-to-string device-id) "Profile"))
    (pw-cli--parse-properties)))

(cl-defmethod pw-access-profiles ((_class pw-cli-accessor) device-id)
  (with-temp-buffer
    (pw-cli--command pw-cli-command `("enum-params" ,(number-to-string device-id) "EnumProfile"))
    (cl-loop for profile = (pw-cli--parse-properties) then (pw-cli--parse-properties)
             while profile
             collect profile)))

(cl-defmethod pw-access-set-profile ((_class pw-cli-accessor) device-id profile-index)
  (pw-cli--set-parameter device-id "Profile" `(("index" . ,profile-index) ("save" . "true"))))

(defun pw-cli--parse-metadata ()
  (let ((metadata '()))
    (while (re-search-forward
            "key:'\\([a-z.]+\\)'.*\\(value\\|\"name\"\\): ?['\"]\\([^'\"]+\\)['\"]"
            nil t)
      (push (cons (match-string 1) (match-string 3)) metadata))
    metadata))

(cl-defmethod pw-access-defaults ((_class pw-cli-accessor))
  (with-temp-buffer
    (pw-cli--command pw-cli-metadata-command '("0"))
    (pw-cli--parse-metadata)))

(cl-defmethod pw-access-set-default ((_class pw-cli-accessor) property node-name)
  (call-process pw-cli-metadata-command nil pw-cli-metadata-command nil
                "0" property (format "{ \"name\": \"%s\" }" node-name)))

(provide 'pw-access)
