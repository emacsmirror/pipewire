;;; pipewire.el --- PipeWire user interface  -*- lexical-binding: t -*-

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
;; PipeWire user interface and library.
;; It currently uses pw-cli and pw-metadata command line utilities to
;; interact with PipeWire.
;;
;; An interactive PipeWire buffer can be displayed using `M-x pipewire'.
;; There you can view basic PipeWire status and change some settings.
;; `pipewire-increase-volume', `pipewire-decrease-volume' and
;; `pipewire-toggle-muted' functions can be used also standalone and
;; are suitable to bind on the multimedia keys.
;;
;; The package can be used also non-interactively in Elisp programs.
;; See pipewire-lib.el source file for available functions.

;;; Code:

(require 'pipewire-lib)

(defgroup pipewire ()
  "PipeWire user interface."
  :group 'multimedia)

(defcustom pipewire-volume-step 5
  "How many percent points to add or subtract when changing volumes."
  :type 'number
  :group 'pipewire)

(defcustom pipewire-osd-enable t
  "If non-nil, display on screen display indicator for some operations.
The indicator is displayed only on graphical terminals."
  :type 'boolean
  :group 'pipewire)

(defcustom pipewire-osd-timeout 3
  "Number of seconds to show an on screen display indicator."
  :type 'number
  :group 'pipewire)

(defcustom pipewire-osd-width (/ 100 pipewire-volume-step)
  "Width of the on screen display indicator in characters."
  :type 'natnum
  :group 'pipewire)

(defcustom pipewire-osd-volume-on-color "lime green"
  "Color to use in the on screen display indicator for active volume."
  :type 'color
  :group 'pipewire)

(defcustom pipewire-osd-volume-off-color "pale green"
  "Color to use in the on screen display indicator for inactive volume."
  :type 'color
  :group 'pipewire)

(defcustom pipewire-osd-frame-parameters
  `((left . 0.05)
    (top . 0.95))
  "Alist of frame parameters for the on screen display indicator."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'pip-frame)

(defface pipewire-label
  '((t (:weight bold :overline t)))
  "Face to use for PipeWire node group labels."
  :group 'pipewire)

(defface pipewire-default-object
  '((t (:weight bold)))
  "Face to use for PipeWire default sinks and sources."
  :group 'pipewire)

(defface pipewire-muted
  '((t (:strike-through t)))
  "Face to use for muted PipeWire sinks and sources."
  :group 'pipewire)

(defface pipewire-volume
  '((t (:inverse-video t)))
  "Face to use for displaying volumes of PipeWire objects."
  :group 'pipewire)

(defvar pipewire-buffer "*PipeWire*")
(defvar pipewire-properties-buffer "*PipWire-properties*")

(defun pipewire--label (label)
  (propertize (concat label ":") 'face 'pipewire-label))

(defun pipewire--object-volume (object)
  (propertize (pipewire-lib-volume object) 'face 'pipewire-volume))

(defun pipewire--object-name (object)
  (let* ((type (pipewire-lib-object-type object))
         (description-properties (if (equal type "Client")
                                     '("application.name")
                                   (let ((prefix (concat (downcase type) ".")))
                                     (mapcar (lambda (suffix) (concat prefix suffix))
                                             '("nick" "description" "name"))))))
    (or (cl-find-if #'identity
                    (mapcar (lambda (p) (pipewire-lib-object-value object p))
                            description-properties))
        "")))

(defun pipewire--object-label (object default-ids)
  (let* ((id (pipewire-lib-object-id object))
         (type (pipewire-lib-object-type object))
         (text (format "%4s: %s" id (pipewire--object-name object)))
         (profile (when (equal type "Device")
                    (pipewire-lib-current-profile (pipewire-lib-object-id object))))
         (face (if (member id default-ids) 'pipewire-default-object 'default))
         (media-class (pipewire-lib-object-value object "media.class")))
    (when media-class
      (setq text (format "%s (%s)" text media-class)))
    (when profile
      (setq text (format "%s: %s" text profile)))
    (let ((volume-p (member type '("Node" "Port"))))
      (when (and volume-p (pipewire-lib-muted-p object))
        (setq face `(:inherit (pipewire-muted ,face))))
      (let ((label (propertize text 'face face)))
        (when volume-p
          (let ((volume (pipewire-lib-volume object)))
            (when volume
              (setq label (concat label " "
                                  (propertize (number-to-string volume)
                                              'face 'pipewire-volume))))))
        label))))

(defun pipewire--insert-line (line object)
  (insert (propertize line 'pipewire-object-id (pipewire-lib-object-id object)) "\n"))

(defun pipewire-refresh (&optional _ignore-auto _noconfirm)
  "Refresh PipeWire buffer."
  (interactive)
  (when (and (not (eq major-mode 'pipewire-mode))
             (not (equal (buffer-name) pipewire-buffer)))
    (error "Not in a PipeWire buffer"))
  (pipewire-lib-refresh)
  (let ((inhibit-read-only t)
        (default-ids (mapcar #'cdr (pipewire-lib-default-nodes)))
        (current-line (count-lines (point-min) (min (1+ (point)) (point-max)))))
    (erase-buffer)
    (insert (pipewire--label "Devices") "\n")
    (dolist (device (pipewire-lib-objects "Device"))
      (pipewire--insert-line (pipewire--object-label device default-ids) device)
      (dolist (node (pipewire-lib-children (pipewire-lib-object-id device) "Node"))
        (pipewire--insert-line (concat "  " (pipewire--object-label node default-ids)) node)
        (dolist (port (pipewire-lib-children (pipewire-lib-object-id node) "Port"))
          (pipewire--insert-line (concat "    " (pipewire--object-label port default-ids)) port))))
    (insert (pipewire--label "Clients") "\n")
    (dolist (client (pipewire-lib-objects "Client"))
      (pipewire--insert-line (pipewire--object-label client default-ids) client))
    (goto-char (point-min))
    (forward-line (1- current-line))))

(defun pipewire--current-object-id ()
  (get-text-property (point) 'pipewire-object-id))

(defun pipewire--current-object (&optional use-default-p allowed-types)
  (let* ((id (pipewire--current-object-id))
         (object (when id (pipewire-lib-get-object id))))
    (when (and object
               allowed-types
               (not (member (pipewire-lib-object-type object) allowed-types)))
      (setq object nil))
    (when (and use-default-p (not object))
      (setq object (or (car (pipewire-lib-default-playback-ports))
                       (pipewire-lib-default-audio-sink))))
    object))

(defvar pipewire--osd-timer nil)
(defvar pipewire--osd-frame nil)
(defvar pipewire--osd-buffer nil)
(defvar pipewire--osd-buffer-name "*pipewire-osd*")

(defun pipewire--osd-display (string)
  (when pipewire--osd-timer
    (cancel-timer pipewire--osd-timer))
  (let ((frame-width (+ 2 (length string))))
    (when (and pipewire--osd-frame
               (not (= frame-width (frame-width pipewire--osd-frame))))
      (delete-frame pipewire--osd-frame)
      (setq pipewire--osd-frame nil))
    (with-current-buffer (setq pipewire--osd-buffer (get-buffer-create pipewire--osd-buffer-name))
      (erase-buffer)
      (insert " " string)
      (setq mode-line-format nil)
      (unless pipewire--osd-frame
        (setq pipewire--osd-frame (make-frame `((unsplittable . t)
                                                ,@pipewire-osd-frame-parameters
                                                (minibuffer . nil)
                                                (parent-frame . ,(selected-frame))
                                                (width . ,(+ 2 (length string)))
                                                (height . 1)
                                                (min-width . 1)
                                                (min-height . 1)
                                                (left-fringe . 0)
                                                (right-fringe . 0)
                                                (no-other-frame . t)
                                                (undecorated . t)
                                                (vertical-scroll-bars . nil)
                                                (horizontal-scroll-bars . nil)
                                                (menu-bar-lines . 0)
                                                (tool-bar-lines . 0)
                                                (tab-bar-lines . 0)
                                                (cursor-type . nil)))))))
  (setq pipewire--osd-timer
        (run-with-timer
         pipewire-osd-timeout nil
         (lambda ()
           (when pipewire--osd-frame
             (ignore-errors (delete-frame pipewire--osd-frame)))
           (when pipewire--osd-buffer
             (ignore-errors (kill-buffer pipewire--osd-buffer)))
           (setq pipewire--osd-frame nil
                 pipewire--osd-timer nil
                 pipewire--osd-buffer nil)))))

(defmacro pipewire--osd (&rest body)
  (declare (debug (body))
           (indent defun))
  (let (($string (gensym)))
    `(when (and window-system pipewire-osd-enable)
       (if-let ((,$string (progn ,@body)))
           (pipewire--osd-display ,$string)))))

(defun pipewire--update (&optional message)
  (if (get-buffer pipewire-buffer)
      (with-current-buffer pipewire-buffer
        (pipewire-refresh))
    (pipewire-lib-refresh))
  (when message
    (message message)))

(defun pipewire--osd-volume (object)
  (pipewire--osd
    (unless (eq (pipewire--current-object-id) (pipewire-lib-object-id object))
      (let* ((object* (pipewire-lib-get-object (pipewire-lib-object-id object))) ; refreshed version
             (volume (pipewire-lib-volume object*))
             (muted-p (pipewire-lib-muted-p object*))
             (step (/ 100.0 pipewire-osd-width))
             (mark (if muted-p ?- ?|))
             (n-active (round (/ volume step)))
             (n-inactive (- pipewire-osd-width n-active)))
        (format "%s%s"
                (propertize (make-string n-active mark)
                            'face `(:background ,pipewire-osd-volume-on-color))
                (propertize (make-string n-inactive mark)
                            'face `(:background ,pipewire-osd-volume-off-color)))))))

(defun pipewire--update-muted (object muted-p)
  (let* ((object-name (pipewire--object-name object))
         (parent-node (pipewire-lib-parent-node object))
         (node-info (if parent-node
                       (format " in %s" (pipewire--object-name parent-node))
                     "")))
    (pipewire--update (format "%s%s %s" object-name node-info (if muted-p "muted" "unmuted")))))

;;;###autoload
(defun pipewire-toggle-muted ()
  "Switch mute status of an audio output or input.
If on a Node or Port in a PipeWire buffer, apply it on the given
object.  Otherwise apply it on the default audio sink."
  (interactive)
  (let* ((object (pipewire--current-object t '("Node" "Port")))
         (muted-p (pipewire-lib-toggle-mute object)))
    (pipewire--update-muted object muted-p)
    (pipewire--osd-volume object)))

;;;###autoload
(defun pipewire-toggle-microphone ()
  "Switch mute status of the default audio input."
  (interactive)
  (let* ((object (car (pipewire-lib-default-capture-ports)))
         (muted-p (pipewire-lib-toggle-mute object)))
    (pipewire--update-muted object muted-p)))

;;;###autoload
(defun pipewire-set-volume (volume &optional object single-p)
  "Set volume of an audio output or input.
VOLUME must be a number in the range 0-100.
If OBJECT is given (only Nodes and Ports are allowed) or if on a Node
or Port in a PipeWire buffer, apply it on the given object.
Otherwise apply it on the default audio sink.
If SINGLE-P is nil, apply it on all related channels, otherwise on the
corresponding object only."
  (interactive "nVolume: ")
  (setq volume (max 0 (min 100 volume)))
  (unless object
    (setq object (pipewire--current-object t '("Node" "Port"))))
  (pipewire-lib-set-volume volume object single-p)
  (pipewire--update (format "Volume %s for %s" volume (pipewire--object-name object)))
  (pipewire--osd-volume object))

(defun pipewire--change-volume (step &optional single-p)
  (let* ((object (pipewire--current-object t '("Node" "Port")))
         (volume (pipewire-lib-volume object))
         (new-volume (max 0 (min 100 (+ volume step)))))
    (pipewire-set-volume new-volume object single-p)))

;;;###autoload
(defun pipewire-increase-volume (&optional single-p)
  "Increase volume of an audio output or input.
The volume is increased by `pipewire-volume-step'.
If on a Node or Port in a PipeWire buffer, apply it on all the
channels of the given object, unless SINGLE-P is non-nil.
Otherwise apply it on the default audio sink."
  (interactive)
  (pipewire--change-volume pipewire-volume-step single-p))

;;;###autoload
(defun pipewire-increase-volume-single ()
  "Increase volume of an audio output or input.
The volume is increased by `pipewire-volume-step'.
If on a Node or Port in a PipeWire buffer, apply it on the given
object.  Otherwise apply it on the default audio sink."
  (interactive)
  (pipewire-increase-volume t))

;;;###autoload
(defun pipewire-decrease-volume (&optional single-p)
  "Decrease volume of an audio output or input.
The volume is decreased by `pipewire-volume-step'.
If on a Node or Port in a PipeWire buffer, apply it on all the
channels of the given object, unless SINGLE-P is non-nil.
Otherwise apply it on the default audio sink."
  (interactive)
  (pipewire--change-volume (- pipewire-volume-step) single-p))

;;;###autoload
(defun pipewire-decrease-volume-single ()
  "Decrease volume of an audio output or input.
The volume is decreased by `pipewire-volume-step'.
If on a Node or Port in a PipeWire buffer, apply it on the given
object.  Otherwise apply it on the default audio sink."
  (interactive)
  (pipewire-decrease-volume t))

;;;###autoload
(defun pipewire-set-default ()
  "Set default sink or source.
If on a Node in a PipeWire buffer, apply it on the given object.
If on a Device, apply it on all its nodes.
Otherwise ask for the Node to set as the default Node."
  (interactive)
  (let ((object (or (pipewire--current-object nil '("Device" "Node"))
                    (let* ((default-node-ids (mapcar #'cdr (pipewire-lib-default-nodes)))
                           (nodes (cl-remove-if
                                   (lambda (n) (member (pipewire-lib-object-id n) default-node-ids))
                                   (pipewire-lib-objects "Node")))
                           (node-mapping (mapcar (lambda (n) (cons (pipewire--object-name n)
                                                                   (pipewire-lib-object-id n)))
                                                 nodes))
                           (node-name (completing-read "Default node: " node-mapping nil t)))
                      (pipewire-lib-get-object (cdr (assoc node-name node-mapping)))))))
    (pipewire-lib-set-default object nil)
    (pipewire-lib-set-default object t)
    (pipewire--update)))

(defun pipewire-set-profile ()
  "Set profile of the device at the current point."
  (interactive)
  (if-let ((device (pipewire--current-object nil '("Device")))
           (device-id (pipewire-lib-object-id device))
           (profiles (pipewire-lib-profiles device-id)))
      (progn
        (pipewire-lib-set-profile device-id (completing-read "Select profile: " profiles nil t))
        ;; Without this, ports of the device may not be displayed on the update:
        (sit-for 0)
        (pipewire--update))
    (error "Nothing to set a profile for here")))

(defun pipewire-properties ()
  "Display properties of the object at the current point."
  (interactive)
  (if-let ((object (pipewire--current-object)))
      (progn
        (pop-to-buffer pipewire-properties-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (p (sort (pipewire-lib-properties object) #'string-lessp))
            (insert (format "%s: %s\n" p (pipewire-lib-object-value object p)))))
        (goto-char (point-min))
        (view-mode))
    (error "No PipeWire object here")))

(defvar pipewire-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'pipewire-set-default)
    (define-key map "m" 'pipewire-toggle-muted)
    (define-key map "p" 'pipewire-set-profile)
    (define-key map "v" 'pipewire-set-volume)
    (define-key map "=" 'pipewire-increase-volume)
    (define-key map "-" 'pipewire-decrease-volume)
    (define-key map "+" 'pipewire-increase-volume-single)
    (define-key map "_" 'pipewire-decrease-volume-single)
    (define-key map " " 'pipewire-properties)
    map))

(define-derived-mode pipewire-mode special-mode "PW"
  "Major mode for PipeWire user interface.
Selected PipeWire objects are displayed and basic operations may be
applied on some of them or the buffer:
\\{pipewire-mode-map}"
  (set (make-local-variable 'revert-buffer-function)
       'pipewire-refresh))

;;;###autoload
(defun pipewire ()
  "Display a PipeWire buffer."
  (interactive)
  (pop-to-buffer pipewire-buffer)
  (pipewire-refresh)
  (pipewire-mode))

(provide 'pipewire)

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:

;;; pipewire.el ends here
