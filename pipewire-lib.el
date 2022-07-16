;;; pipewire-lib.el --- PipeWire library  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Milan Zamazal <pdm@zamazal.org>

;; Author: Milan Zamazal <pdm@zamazal.org>
;; Version: 1
;; Package-Requires: ((emacs "28.1"))
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
;; Backend-independent library to access PipeWire functionality.
;; It abstracts data returned from `pipewire-access' methods and provides
;; functions to work with them.
;;
;; pipewire-lib caches data retrieved from PipeWire and uses the cached
;; data.  The cache can be invalidated by calling `pipewire-lib-refresh'.

;;; Code:

(require 'cl-lib)
(require 'pipewire-access)
(require 'pipewire-cli)

(defvar pipewire-lib--accessor (pipewire-cli-accessor))

(defvar pipewire-lib--objects '())
(defvar pipewire-lib--bindings nil)
(defvar pipewire-lib--defaults nil)

(defun pipewire-lib-refresh ()
  "Clear cache of objects retrieved from PipeWire."
  (setq pipewire-lib--objects (pipewire-access-objects pipewire-lib--accessor)
        pipewire-lib--bindings nil
        pipewire-lib--defaults nil))

(defun pipewire-lib-objects (&optional type)
  "Return a list of PipeWire objects.
TYPE is a string identifying PipeWire objects types (e.g. \"Device\",
\"Node\", \"Port\", \"Client\", ...).  If specified, return only
objects of the given type.
The format of the list elements is unspecified, use pipewire-lib functions
to access their data.
Note that PipeWire data is cached, if you need its up-to-date
version, call `pipewire-lib-refresh' first."
  (unless pipewire-lib--objects
    (pipewire-lib-refresh))
  (let ((objects pipewire-lib--objects))
    (when type
      (setq objects (cl-remove-if-not
                     (lambda (o) (string= (cdr (assq 'type (cdr o))) type))
                     objects)))
    objects))

(defun pipewire-lib-get-object (id)
  "Return PipeWire object identified by ID.
If such an object doesn't exist, return nil.
Note that PipeWire data is cached, if you need its up-to-date
version, call `pipewire-lib-refresh' first."
  (assoc id pipewire-lib--objects))

(defun pipewire-lib-object-id (object)
  "Return id of the given PipeWire OBJECT."
  (car object))

(defun pipewire-lib--object-info (object)
  (cdr object))

(defun pipewire-lib-object-value (object key &optional default)
  "Return PipeWire OBJECT value identified by KEY.
KEY is a string corresponding to a PipeWire value identifier.
If the given KEY doesn't exist in OBJECT, return DEFAULT."
  (or (cdr (assoc key (pipewire-lib--object-info object)))
      default))

(defun pipewire-lib-properties (object)
  "Return names of PipeWire OBJECT properties.
The returned value is a list of strings.
The corresponding values can be retrieved using `pipewire-lib-object-value'
function."
  (cl-remove-if-not #'stringp (mapcar #'car (pipewire-lib--object-info object))))

(defun pipewire-lib-object-type (object)
  "Return PipeWire type of OBJECT as a string.
E.g. \"Device\", \"Node\", \"Port\", \"Client\", ..."
  (pipewire-lib-object-value object 'type))

(defun pipewire-lib--profile-name (profile)
  (cdr (or (assoc "description" profile)
           (assoc "name" profile))))

(defun pipewire-lib-current-profile (device-id)
  "Return the current profile name of the given device.
DEVICE-ID is the numeric id of the device.
The returned profile name is a string, or nil if it cannot be found."
  (pipewire-lib--profile-name (pipewire-access-current-profile pipewire-lib--accessor device-id)))

(defun pipewire-lib-profiles (device-id)
  "Return list of available profiles of the given device.
DEVICE-ID is the numeric id of the device.
A list of strings (possibly empty) is returned."
  (mapcar #'pipewire-lib--profile-name (pipewire-access-profiles pipewire-lib--accessor device-id)))

(defun pipewire-lib-set-profile (device-id profile)
  "Set the profile of the given device.
DEVICE-ID is the numeric id of the device.
PROFILE is a string name of the profile, it must be one of the values
returned from `pipewire-lib-profiles'."
  (let* ((all-profiles (pipewire-access-profiles pipewire-lib--accessor device-id))
         (properties (cl-find profile all-profiles :key #'pipewire-lib--profile-name :test #'equal)))
    (unless properties
      (error "Profile %s of device %s not found" profile device-id))
    (let ((index (cdr (assoc "index" properties))))
      (unless index
        (error "Index of %s profile of device %s not found" profile device-id))
      (pipewire-access-set-profile pipewire-lib--accessor device-id index))))

(defun pipewire-lib-parent-node (object)
  "Return parent node of OBJECT.
This is typically used for ports.
Behavior is undefined if OBJECT has no parent node."
  (pipewire-lib-get-object (pipewire-lib-object-value object "node.id")))

(defun pipewire-lib--node (object)
  (if (equal (pipewire-lib-object-type object) "Node")
      object
    (pipewire-lib-parent-node object)))

(defun pipewire-lib--node-parameters (object-or-id &optional refresh)
  (let* ((object (if (numberp object-or-id)
                     (pipewire-lib-get-object object-or-id)
                   object-or-id))
         (node (pipewire-lib--node object))
         (parameters (pipewire-lib-object-value node 'parameters)))
    (when (or refresh (not parameters))
      (setq parameters (pipewire-access-properties pipewire-lib--accessor (pipewire-lib-object-id node)))
      (setcdr node (cons (cons 'parameters parameters)
                         (assq-delete-all 'parameters (cdr node)))))
    parameters))

(defun pipewire-lib-default-nodes ()
  "Return assignments of PipeWire Nodes to default sinks and sources.
An association lists with elements of the form (KEY . ID) is
returned, where KEY is a string identifying the given kind of
default sink or source as reported by PipeWire and ID is the
corresponding PipeWire node numeric id.
Note that PipeWire data is cached, if you need its up-to-date
version, call `pipewire-lib-refresh' first."
  (unless pipewire-lib--defaults
    (let ((defaults (pipewire-access-defaults pipewire-lib--accessor))
          (nodes (mapcar (lambda (o)
                           (cons (pipewire-lib-object-value o "node.name") (pipewire-lib-object-id o)))
                         (pipewire-lib-objects "Node"))))
      (setq pipewire-lib--defaults
            (cl-remove-if-not #'cdr
                              (mapcar (lambda (d)
                                        (cons (car d) (cdr (assoc (cdr d) nodes))))
                                      defaults)))))
  pipewire-lib--defaults)

(defun pipewire-lib--default-node (key)
  (pipewire-lib-get-object (cdr (assoc key (pipewire-lib-default-nodes)))))

(defun pipewire-lib-bindings ()
  "Return bindings between PipeWire objects.
An association lists with elements of the form (PARENT . CHILD) is
returned where PARENT and CHILD are numeric ids of PipeWire objects.
Note that PipeWire data is cached, if you need its up-to-date
version, call `pipewire-lib-refresh' first."
  (or pipewire-lib--bindings
      (setq pipewire-lib--bindings
            (apply #'nconc
                   (mapcar (lambda (o)
                             (let ((o-id (pipewire-lib-object-id o)))
                               (mapcar (lambda (p)
                                         (cons o-id (cdr p)))
                                       (cl-remove-if-not #'numberp (pipewire-lib--object-info o)
                                                         :key #'cdr))))
                           (pipewire-lib-objects))))))

(defun pipewire-lib-children (id &optional type)
  "Return child objects of the object identified by numeric PipeWire ID.
If a string TYPE is specified then only children of the given PipeWire
type are returned.
Note that PipeWire data is cached, if you need its up-to-date
version, call `pipewire-lib-refresh' first."
  (let ((children (mapcar #'pipewire-lib-get-object
                          (mapcar #'car (cl-remove-if (lambda (b) (/= (cdr b) id))
                                                      (pipewire-lib-bindings))))))
    (when type
      (setq children (cl-remove-if-not (lambda (o) (equal (pipewire-lib-object-type o) type))
                                       children)))
    children))

(defun pipewire-lib--node-ports (node &optional regexp)
  (when node
    (let ((ports (pipewire-lib-children (pipewire-lib-object-id node) "Port")))
      (if regexp
          (cl-delete-if-not (lambda (o)
                              (if-let ((name (pipewire-lib-object-value o "port.name")))
                                  (string-match regexp name)))
                            ports)
        ports))))

(defun pipewire-lib-default-audio-sink ()
  "Return a PipeWire object that is the current default audio sink."
  (pipewire-lib--default-node "default.audio.sink"))

(defun pipewire-lib-default-audio-source ()
  "Return a PipeWire object that is the current default audio source."
  (pipewire-lib--default-node "default.audio.source"))

(defun pipewire-lib-default-playback-ports ()
  "Return list of PipeWire objects that are default playback ports."
  (pipewire-lib--node-ports (pipewire-lib-default-audio-sink) "^playback"))

(defun pipewire-lib-default-capture-ports ()
  "Return list of PipeWire objects that are default capture ports."
  (pipewire-lib--node-ports (pipewire-lib-default-audio-source) "^capture"))

(defun pipewire-lib--volume-% (volume)
  (when volume
    (round (* 100 volume))))

(defun pipewire-lib--volume-float (volume)
  (/ (float volume) 100))

(defun pipewire-lib--object-parameters (object &optional refresh)
  (let* ((node-p (equal (pipewire-lib-object-type object) "Node"))
         (parameters (pipewire-lib--node-parameters object refresh))
         (monitor-p (unless node-p
                      (equal (pipewire-lib-object-value object "port.monitor") "true")))
         (node-id (pipewire-lib-object-id (pipewire-lib--node object)))
         (port-id (unless node-p
                    (pipewire-lib-object-value object "port.id"))))
    (list node-p parameters monitor-p node-id port-id)))

(defun pipewire-lib-muted-p (object &optional refresh)
  "Return whether the given PipeWire OBJECT is muted.
Applicable only to Nodes and Ports.
If REFRESH is non-nil then retrive fresh information from PipeWire
rather than using cached data to obtain the result."
  (cl-destructuring-bind (_node-p parameters monitor-p _node-id _port-id)
      (pipewire-lib--object-parameters object refresh)
    (eq (cdr (assoc (if monitor-p "monitorMute" "mute") parameters)) 'true)))

(defun pipewire-lib-toggle-mute (object &optional refresh)
  "Toggle mute status of the given PipeWire OBJECT.
Return the new boolean mute status of OBJECT.
Applicable only to Nodes and Ports.
If REFRESH is non-nil then retrive fresh information from PipeWire
rather than using cached data to obtain the result."
  (cl-destructuring-bind (_node-p _parameters monitor-p node-id _port-id)
      (pipewire-lib--object-parameters object refresh)
    (let* ((mute (not (pipewire-lib-muted-p object)))
           (property (if monitor-p "monitorMute" "mute"))
           (value (if mute "true" "false")))
      (pipewire-access-set-properties pipewire-lib--accessor node-id (list (cons property value)))
      mute)))

(defun pipewire-lib-volume (object &optional refresh)
  "Return volume of the given PipeWire OBJECT.
The returned value is an integer in the range 0-100.
Applicable only to Nodes and Ports.
If REFRESH is non-nil then retrive fresh information from PipeWire
rather than using cached data to obtain the result."
  (cl-destructuring-bind (node-p parameters monitor-p _node-id port-id)
      (pipewire-lib--object-parameters object refresh)
    (pipewire-lib--volume-%
     (if node-p
         (cdr (assoc "volume" parameters))
       (nth port-id (cdr (assoc (if monitor-p "monitorVolumes" "channelVolumes") parameters)))))))

(defun pipewire-lib-set-volume (volume object &optional single-p)
  "Set the volume of PipeWire OBJECT to VOLUME.
VOLUME must be an integer in the range 0-100.
If SINGLE-P is non-nil, set the volume only for a single channel,
otherwise set the volume to the same value for all the related channels."
  (cl-destructuring-bind (node-p parameters monitor-p node-id port-id)
      (pipewire-lib--object-parameters object)
    (let* ((property (cond
                      (node-p "volume")
                      (monitor-p "monitorVolumes")
                      (t "channelVolumes")))
           (float-volume (pipewire-lib--volume-float volume))
           (value (if node-p
                      float-volume
                    (let ((orig-value (cdr (assoc property parameters))))
                      (if single-p
                          (cl-substitute float-volume nil orig-value
                                         :test #'always :start port-id :count 1)
                        (make-list (length orig-value) float-volume))))))
      (pipewire-access-set-properties pipewire-lib--accessor node-id (list (cons property value))))))

(defun pipewire-lib--set-default-node (object stored-p)
  (let ((suffix (mapconcat #'downcase
                           (split-string (pipewire-lib-object-value object "media.class") "/")
                           "."))
        (prefix (if stored-p "default.configured." "default."))
        (node-name (pipewire-lib-object-value object "node.name")))
    (pipewire-access-set-default pipewire-lib--accessor (concat prefix suffix) node-name)))

(defun pipewire-lib-set-default (object stored-p)
  "Set PipeWire OBJECT as the default sink or source.
If STORED-P is non-nil, set the stored default sink or source,
otherwise set the current default sink or source."
  (pcase (pipewire-lib-object-type object)
    ("Device"
     (dolist (node (pipewire-lib-children (pipewire-lib-object-id object) "Node"))
       (pipewire-lib--set-default-node node stored-p)))
    ("Node"
     (pipewire-lib--set-default-node object stored-p))
    (_
     (error "Cannot set this kind of object as default"))))

(provide 'pipewire-lib)

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:

;;; pipewire-lib.el ends here
