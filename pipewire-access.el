;;; pipewire-access.el --- PipeWire generic access  -*- lexical-binding: t -*-

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

(provide 'pipewire-access)

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:

;;; pipewire-access.el ends here
