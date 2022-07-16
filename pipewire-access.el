;;; pipewire-access.el --- PipeWire access  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Milan Zamazal <pdm@zamazal.org>

;; Author: Milan Zamazal <pdm@zamazal.org>
;; Version: 1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: multimedia
;; URL: https://git.zamazal.org/pdm/pipewire-0

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

;;; Commentary:
;;
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

;;; Code:

(require 'eieio)

(defclass pipewire-accessor ()
  ()
  :documentation
  "Base PipeWire interface class.
All PipeWire interfaces should derive from this class.")

(cl-defgeneric pipewire-access-objects (class)
  "Return all the objects currently reported by PipeWire.
It is a list of object data.  Each of the elements has a form
\(OBJECT-ID . INFO) where OBJECT-ID is a numeric OBJECT-ID as
reported by PipeWire and INFO is an association list of items
\(NAME . VALUE) where NAME is a string item name as reported by
PipeWire and VALUE is the corresponding value.  VALUE is a number for
object ids, a string otherwise.
A special entry with `type' symbol as its name contains the PipeWire
type of the objects, as a string (e.g. \"Device\", \"Node\", \"Port\",
 \"Client\", ...).
CLASS is a PipeWire interface, see symbol `pipewire-accessor'.")

(cl-defgeneric pipewire-access-properties (class node-id)
  "Return properties of the given node.
NODE-ID is a numeric PipeWire Node id (other kinds of PipeWire objects
are not supported in this method).
Object properties may be, unlike object info items, settable.

An assocation list is returned.  Each list element is of the form
\(PROPERTY . VALUE) where PROPERTY is a string name of the given
property.  VALUE can be:

- \"true\" or \"false\" for boolean values (t and nil are not used to
  avoid confusion with nil representing invalid or unavailable value).
- A number for numeric values (ids, integers, floats).
- A string for string values.
- A list of elements of any of these types for arrays and structs.

CLASS is a PipeWire interface, see symbol `pipewire-accessor'.")

(cl-defgeneric pipewire-access-set-properties (class node-id properties)
  "Set PROPERTIES of the given node.
NODE-ID is a numeric PipeWire Node id (other kinds of PipeWire objects
are not supported in this method).
PROPERTIES is an association list in the same format as in
`pipewire-access-properties'.  It needn't contain all the properties, just
the properties to be changed.
CLASS is a PipeWire interface, see symbol `pipewire-accessor'.")

(cl-defgeneric pipewire-access-current-profile (class device-id)
  "Return current profile of the given device.
DEVICE-ID is a numeric PipeWire Device id (other kinds of PipeWire
objects are not supported in this method).

The profile is an association list with elements of the form
\(PROPERTY . VALUE), in the same format as properties in
`pipewire-access-properties'.

CLASS is a PipeWire interface, see symbol `pipewire-accessor'.")

(cl-defgeneric pipewire-access-profiles (class device-id)
  "Return available profiles of the given device.
DEVICE-ID is a numeric PipeWire Device id (other kinds of PipeWire
objects are not supported in this method).

Return a list of profiles, which are in the same format as in
`pipewire-access-current-profile'.

CLASS is a PipeWire interface, see symbol `pipewire-accessor'.")

(cl-defgeneric pipewire-access-set-profile (class device-id profile-index)
  "Set the profile of the given device.
DEVICE-ID is a numeric PipeWire Device id (other kinds of PipeWire
objects are not supported in this method).
PROFILE-INDEX is a numeric index of the profile to set, as returned
from PipeWire.
CLASS is a PipeWire interface, see symbol `pipewire-accessor'.")

(cl-defgeneric pipewire-access-defaults (class)
  "Return default sinks and sources.
An association lists is returned.  Each list element is of the form
\(KEY . NAME) where KEY is a string identifying the given kind of
default sink or source as reported by PipeWire and NAME is a string
name of the node assigned to the default.
CLASS is a PipeWire interface, see symbol `pipewire-accessor'.")

(cl-defgeneric pipewire-access-set-default (class key node-name)
  "Set default sink or source.
KEY is a string identifying the given kind of default sink or source
as reported in `pipewire-access-defaults' and NODE-NAME is a string name of
the node that should be assigned to KEY.
CLASS is a PipeWire interface, see symbol `pipewire-accessor'.")

;; pw-cli interface

(defvar pipewire-cli-command "pw-cli"
  "Command to invoke pw-cli.")

(defvar pipewire-cli-metadata-command "pw-metadata"
  "Command to invoke pw-metadata.")

(defclass pipewire-cli-accessor (pipewire-accessor)
  ()
  :documentation
  "Command line based interface to PipeWire.
Note this interface may not work with all PipeWire versions.")

(defun pipewire-cli--command (command args)
  (apply #'call-process command nil t nil args)
  (goto-char (point-min)))

(defun pipewire-cli--next-line ()
  (goto-char (line-beginning-position 2)))

(defun pipewire-cli--parse-list ()
  (let ((objects '()))
    (while (re-search-forward "^[\t]id \\([0-9]+\\), type PipeWire:Interface:\\(.*\\)/.*$" nil t)
      (let ((id (string-to-number (match-string 1)))
            (properties `((type . ,(match-string 2)))))
        (pipewire-cli--next-line)
        (while (looking-at "^ [\t][\t]\\([a-z.]+\\) = \"\\(.*\\)\"")
          (let ((property (match-string 1))
                (value  (match-string 2)))
            (when (string-suffix-p ".id" property)
              (setq value (string-to-number value)))
            (push (cons property value) properties))
          (pipewire-cli--next-line))
        (push (cons id properties) objects)))
    (nreverse objects)))

(cl-defmethod pipewire-access-objects ((_class pipewire-cli-accessor))
  (with-temp-buffer
    (pipewire-cli--command pipewire-cli-command '("list-objects"))
    (pipewire-cli--parse-list)))

(defun pipewire-cli--read-property (&optional nesting)
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
           (pipewire-cli--next-line)
           (while (setq item (pipewire-cli--read-property (1+ nesting)))
             (push item array)
             (pipewire-cli--next-line))
           (nreverse array)))))))

(defun pipewire-cli--parse-properties ()
  (pipewire-cli--next-line)
  (let ((end (or (save-excursion (re-search-forward "^  Object:" nil t))
                 (point-max)))
        (properties '()))
    (while (and (< (point) end)
                (re-search-forward "^    Prop: key \\([A-Za-z:]+\\)" end t))
      (pipewire-cli--next-line)
      (let ((property (car (last (split-string (match-string 1) ":"))))
            (value (pipewire-cli--read-property)))
        (when value
          (push (cons property value) properties))))
    (goto-char end)
    properties))

(cl-defmethod pipewire-access-properties ((_class pipewire-cli-accessor) node-id)
  (with-temp-buffer
    (pipewire-cli--command pipewire-cli-command `("enum-params" ,(number-to-string node-id) "Props"))
    (pipewire-cli--parse-properties)))

(defun pipewire-cli--format-property-value (value)
  (cond
   ((consp value)
    (concat "[ " (mapconcat #'pipewire-cli--format-property-value value ", ") " ]"))
   ((numberp value)
    (number-to-string value))
   (t
    value)))

(defun pipewire-cli--format-property (property)
  (format "%s: %s" (car property) (pipewire-cli--format-property-value (cdr property))))

(defun pipewire-cli--format-properties (properties)
  (concat "{ " (mapconcat #'pipewire-cli--format-property properties ", ") " }"))

(defun pipewire-cli--set-parameter (object-id parameter value)
  (let* ((formatted (pipewire-cli--format-properties value)))
    (call-process pipewire-cli-command nil pipewire-cli-command nil
                  "set-param" (number-to-string object-id) parameter formatted)))

(cl-defmethod pipewire-access-set-properties ((_class pipewire-cli-accessor) node-id properties)
  (pipewire-cli--set-parameter node-id "Props" properties))

(cl-defmethod pipewire-access-current-profile ((_class pipewire-cli-accessor) device-id)
  (with-temp-buffer
    (pipewire-cli--command pipewire-cli-command `("enum-params" ,(number-to-string device-id) "Profile"))
    (pipewire-cli--parse-properties)))

(cl-defmethod pipewire-access-profiles ((_class pipewire-cli-accessor) device-id)
  (with-temp-buffer
    (pipewire-cli--command pipewire-cli-command `("enum-params" ,(number-to-string device-id) "EnumProfile"))
    (cl-loop for profile = (pipewire-cli--parse-properties) then (pipewire-cli--parse-properties)
             while profile
             collect profile)))

(cl-defmethod pipewire-access-set-profile ((_class pipewire-cli-accessor) device-id profile-index)
  (pipewire-cli--set-parameter device-id "Profile" `(("index" . ,profile-index) ("save" . "true"))))

(defun pipewire-cli--parse-metadata ()
  (let ((metadata '()))
    (while (re-search-forward
            "key:'\\([a-z.]+\\)'.*\\(value\\|\"name\"\\): ?['\"]\\([^'\"]+\\)['\"]"
            nil t)
      (push (cons (match-string 1) (match-string 3)) metadata))
    metadata))

(cl-defmethod pipewire-access-defaults ((_class pipewire-cli-accessor))
  (with-temp-buffer
    (pipewire-cli--command pipewire-cli-metadata-command '("0"))
    (pipewire-cli--parse-metadata)))

(cl-defmethod pipewire-access-set-default ((_class pipewire-cli-accessor) property node-name)
  (call-process pipewire-cli-metadata-command nil pipewire-cli-metadata-command nil
                "0" property (format "{ \"name\": \"%s\" }" node-name)))

(provide 'pipewire-access)

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:

;;; pipewire-access.el ends here
