;;; pw-lib.el --- PipeWire library  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Milan Zamazal <pdm@zamazal.org>

;; Author: Milan Zamazal <pdm@zamazal.org>
;; Package-Version: 1
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
;; It abstracts data returned from `pw-access' methods and provides
;; functions to work with them.
;;
;; pw-lib caches data retrieved from PipeWire and uses the cached
;; data.  If   The cache can be invalidated by calling `pw-lib-refresh'.

(require 'cl-lib)
(require 'pw-access)

(defvar pw-lib--accessor (pw-cli-accessor))

(defvar pw-lib--objects '())
(defvar pw-lib--bindings nil)
(defvar pw-lib--defaults nil)

(defun pw-lib-refresh ()
  "Clear cache of objects retrieved from PipeWire."
  (setq pw-lib--objects (pw-access-objects pw-lib--accessor)
        pw-lib--bindings nil
        pw-lib--defaults nil))

(defun pw-lib-objects (&optional type)
  "Return a list of PipeWire objects.
TYPE is a string identifying PipeWire objects types (e.g. \"Device\",
\"Node\", \"Port\", \"Client\", ...).  If specified, return only
objects of the given type.
The format of the list elements is unspecified, use pw-lib functions
to access their data.
Note that PipeWire data is cached, if you need its up-to-date
version, call `pw-lib-refresh' first."
  (unless pw-lib--objects
    (pw-lib-refresh))
  (let ((objects pw-lib--objects))
    (when type
      (setq objects (cl-remove-if-not
                     (lambda (o) (string= (cdr (assq 'type (cdr o))) type))
                     objects)))
    objects))

(defun pw-lib-get-object (id)
  "Return PipeWire object identified by ID.
If such an object doesn't exist, return nil.
Note that PipeWire data is cached, if you need its up-to-date
version, call `pw-lib-refresh' first."
  (assoc id pw-lib--objects))

(defun pw-lib-object-id (object)
  "Return id of the given PipeWire OBJECT."
  (car object))

(defun pw-lib--object-info (object)
  (cdr object))
  
(defun pw-lib-object-value (object key &optional default)
  "Return PipeWire OBJECT value identified by KEY.
KEY is a string corresponding to a PipeWire value identifier.
If the given KEY doesn't exist in OBJECT, return DEFAULT."
  (or (cdr (assoc key (pw-lib--object-info object)))
      default))

(defun pw-lib-object-type (object)
  "Return PipeWire type of OBJECT as a string.
E.g. \"Device\", \"Node\", \"Port\", \"Client\", ..."
  (pw-lib-object-value object 'type))

(defun pw-lib--profile-name (profile)
  (cdr (or (assoc "description" profile)
           (assoc "name" profile))))

(defun pw-lib-current-profile (device-id)
  "Return the current profile name of the given device.
DEVICE-ID is the numeric id of the device.
The returned profile name is a string, or nil if it cannot be found."
  (pw-lib--profile-name (pw-access-current-profile pw-lib--accessor device-id)))

(defun pw-lib-profiles (device-id)
  "Return list of available profiles of the given device.
DEVICE-ID is the numeric id of the device.
A list of strings (possibly empty) is returned."
  (mapcar #'pw-lib--profile-name (pw-access-profiles pw-lib--accessor device-id)))

(defun pw-lib-set-profile (device-id profile)
  "Set the profile of the given device.
DEVICE-ID is the numeric id of the device.
PROFILE is a string name of the profile, it must be one of the values
returned from `pw-lib-profiles'. "
  (let* ((all-profiles (pw-access-profiles pw-lib--accessor device-id))
         (properties (cl-find profile all-profiles :key #'pw-lib--profile-name :test #'equal)))
    (unless properties
      (error "Profile %s of device %s not found" profile device-id))
    (let ((index (cdr (assoc "index" properties))))
      (unless index
        (error "Index of %s profile of device %s not found" profile device-id))
      (pw-access-set-profile pw-lib--accessor device-id index))))

(defun pw-lib-parent-node (object)
  "Return parent node of `object'.
This is typically used for ports.
Behavior is undefined if `object' has no parent node."
  (pw-lib-get-object (pw-lib-object-value object "node.id")))

(defun pw-lib--node (object)
  (if (equal (pw-lib-object-type object) "Node")
      object
    (pw-lib-parent-node object)))

(defun pw-lib--node-parameters (object-or-id &optional refresh)
  (let* ((object (if (numberp object-or-id)
                     (pw-lib-get-object object-or-id)
                   object-or-id))
         (node (pw-lib--node object))
         (parameters (pw-lib-object-value node 'parameters)))
    (when (or refresh (not parameters))
      (setq parameters (pw-access-properties pw-lib--accessor (pw-lib-object-id node)))
      (setcdr node (cons (cons 'parameters parameters)
                         (assq-delete-all 'parameters (cdr node)))))
    parameters))

(defun pw-lib-default-nodes ()
  "Return assignments of PipeWire Nodes to default sinks and sources.
An association lists with elements of the form (KEY . ID) is
returned, where KEY is a string identifying the given kind of
default sink or source as reported by PipeWire and ID is the
corresponding PipeWire node numeric id.
Note that PipeWire data is cached, if you need its up-to-date
version, call `pw-lib-refresh' first."
  (unless pw-lib--defaults
    (let ((defaults (pw-access-defaults pw-lib--accessor))
          (nodes (mapcar (lambda (o)
                           (cons (pw-lib-object-value o "node.name") (pw-lib-object-id o)))
                         (pw-lib-objects "Node"))))
      (setq pw-lib--defaults
            (cl-remove-if-not #'cdr
                              (mapcar (lambda (d)
                                        (cons (car d) (cdr (assoc (cdr d) nodes))))
                                      defaults)))))
  pw-lib--defaults)

(defun pw-lib--default-node (key)
  (pw-lib-get-object (cdr (assoc key (pw-lib-default-nodes)))))

(defun pw-lib-bindings ()
  "Return bindings between PipeWire objects.
An association lists with elements of the form (PARENT . CHILD) is
returned where PARENT and CHILD are numeric ids of PipeWire objects.
Note that PipeWire data is cached, if you need its up-to-date
version, call `pw-lib-refresh' first."
  (or pw-lib--bindings
      (setq pw-lib--bindings
            (apply #'nconc
                   (mapcar (lambda (o)
                             (let ((o-id (pw-lib-object-id o)))
                               (mapcar (lambda (p)
                                         (cons o-id (cdr p)))
                                       (cl-remove-if-not #'numberp (pw-lib--object-info o)
                                                         :key #'cdr))))
                           (pw-lib-objects))))))

(defun pw-lib-children (id &optional type)
  "Return child objects of the object identified by numeric PipeWire ID.
If a string TYPE is specified then only children of the given PipeWire
type are returned.
Note that PipeWire data is cached, if you need its up-to-date
version, call `pw-lib-refresh' first."
  (let ((children (mapcar #'pw-lib-get-object
                          (mapcar #'car (cl-remove-if (lambda (b) (/= (cdr b) id))
                                                      (pw-lib-bindings))))))
    (when type
      (setq children (cl-remove-if-not (lambda (o) (equal (pw-lib-object-type o) type))
                                       children)))
    children))

(defun pw-lib--node-ports (node &optional regexp)
  (when node
    (let ((ports (pw-lib-children (pw-lib-object-id node) "Port")))
      (if regexp
          (cl-delete-if-not (lambda (o)
                              (if-let ((name (pw-lib-object-value o "port.name")))
                                  (string-match regexp name)))
                            ports)
        ports))))

(defun pw-lib-default-audio-sink ()
  "Return a PipeWire object that is the current default audio sink."
  (pw-lib--default-node "default.audio.sink"))

(defun pw-lib-default-audio-source ()
  "Return a PipeWire object that is the current default audio source."
  (pw-lib--default-node "default.audio.source"))

(defun pw-lib-default-playback-ports ()
  "Return list of PipeWire objects that are default playback ports."
  (pw-lib--node-ports (pw-lib-default-audio-sink) "^playback"))

(defun pw-lib-default-capture-ports ()
  "Return list of PipeWire objects that are default capture ports."
  (pw-lib--node-ports (pw-lib-default-audio-source) "^capture"))

(defun pw-lib--volume-% (volume)
  (when volume
    (round (* 100 volume))))

(defun pw-lib--volume-float (volume)
  (/ (float volume) 100))

(defun pw-lib--object-parameters (object &optional refresh)
  (let* ((node-p (equal (pw-lib-object-type object) "Node"))
         (parameters (pw-lib--node-parameters object refresh))
         (monitor-p (unless node-p
                      (equal (pw-lib-object-value object "port.monitor") "true")))
         (node-id (pw-lib-object-id (pw-lib--node object)))
         (port-id (unless node-p
                    (pw-lib-object-value object "port.id"))))
    (list node-p parameters monitor-p node-id port-id)))

(defun pw-lib-muted-p (object &optional refresh)
  "Return whether the given PipeWire object is muted.
Applicable only to Nodes and Ports.
If REFRESH is non-nil then retrive fresh information from PipeWire
rather than using cached data to obtain the result."
  (cl-destructuring-bind (_node-p parameters monitor-p _node-id _port-id)
      (pw-lib--object-parameters object refresh)
    (eq (cdr (assoc (if monitor-p "monitorMute" "mute") parameters)) 'true)))

(defun pw-lib-toggle-mute (object &optional refresh)
  "Toggle mute status of the given PipeWire OBJECT.
Return the new boolean mute status of OBJECT.
Applicable only to Nodes and Ports.
If REFRESH is non-nil then retrive fresh information from PipeWire
rather than using cached data to obtain the result."
  (cl-destructuring-bind (_node-p _parameters monitor-p node-id _port-id)
      (pw-lib--object-parameters object refresh)
    (let* ((mute (not (pw-lib-muted-p object)))
           (property (if monitor-p "monitorMute" "mute"))
           (value (if mute "true" "false")))
      (pw-access-set-properties pw-lib--accessor node-id (list (cons property value)))
      mute)))

(defun pw-lib-volume (object &optional refresh)
  "Return volume of the given PipeWire object.
The returned value is an integer in the range 0-100.
Applicable only to Nodes and Ports.
If REFRESH is non-nil then retrive fresh information from PipeWire
rather than using cached data to obtain the result."
  (cl-destructuring-bind (node-p parameters monitor-p _node-id port-id)
      (pw-lib--object-parameters object refresh)
    (pw-lib--volume-%
     (if node-p
         (cdr (assoc "volume" parameters))
       (nth port-id (cdr (assoc (if monitor-p "monitorVolumes" "channelVolumes") parameters)))))))
   
(defun pw-lib-set-volume (volume object &optional single-p)
  "Set the volume of PipeWire OBJECT to VOLUME.
VOLUME must be an integer in the range 0-100.
If SINGLE-P is non-nil, set the volume only for a single channel,
otherwise set the volume to the same value for all the related channels."
  (cl-destructuring-bind (node-p parameters monitor-p node-id port-id)
      (pw-lib--object-parameters object)
    (let* ((property (cond
                      (node-p "volume")
                      (monitor-p "monitorVolumes")
                      (t "channelVolumes")))
           (float-volume (pw-lib--volume-float volume))
           (value (if node-p
                      float-volume
                    (let ((orig-value (cdr (assoc property parameters))))
                      (if single-p
                          (cl-substitute float-volume nil orig-value
                                         :test #'always :start port-id :count 1)
                        (make-list (length orig-value) float-volume))))))
      (pw-access-set-properties pw-lib--accessor node-id (list (cons property value))))))

(defun pw-lib--set-default-node (object stored-p)
  (let ((suffix (mapconcat #'downcase
                           (split-string (pw-lib-object-value object "media.class") "/")
                           "."))
        (prefix (if stored-p "default.configured." "default."))
        (node-name (pw-lib-object-value object "node.name")))
    (pw-access-set-default pw-lib--accessor (concat prefix suffix) node-name)))

(defun pw-lib-set-default (object stored-p)
  "Set PipeWire OBJECT as the default sink or source.
If STORED-P is non-nil, set the stored default sink or source,
otherwise set the current default sink or source."
  (pcase (pw-lib-object-type object)
    ("Device"
     (dolist (node (pw-lib-children (pw-lib-object-id object) "Node"))
       (pw-lib--set-default-node node stored-p)))
    ("Node"
     (pw-lib--set-default-node object stored-p))
    (_
     (error "Cannot set this kind of object as default."))))
                           
(provide 'pw-lib)

;;; pw-lib.el ends here
