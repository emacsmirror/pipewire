;;; pw-ui.el --- PipeWire user interface  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Milan Zamazal <pdm@zamazal.org>

;; Author: Milan Zamazal <pdm@zamazal.org>
;; Package-Version: 1
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
;; PipeWire user interface based on pw-lib.
;; An interactive buffer can be displayed using `M-x pipewire'.
;; `pipewire-increase-volume', `pipewire-decrease-volume' and
;; `pipewire-toggle-muted' functions are also suitable to bind on the
;; multimedia keys.

(require 'pw-lib)

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

(defun pw-ui--label (label)
  (propertize (concat label ":") 'face 'pipewire-label))

(defun pw-ui--object-volume (object)
  (propertize (pw-lib-volume object) 'face 'pipewire-volume))

(defun pw-ui--object-name (object)
  (let* ((type (pw-lib-object-type object))
         (description-properties (if (equal type "Client")
                                     '("application.name")
                                   (let ((prefix (concat (downcase type) ".")))
                                     (mapcar (lambda (suffix) (concat prefix suffix))
                                             '("description" "name"))))))
    (or (cl-find-if #'identity
                    (mapcar (lambda (p) (pw-lib-object-value object p))
                            description-properties))
        "")))

(defun pw-ui--object-label (object default-ids)
  (let* ((id (pw-lib-object-id object))
         (type (pw-lib-object-type object))
         (text (format "%4s: %s" id (pw-ui--object-name object)))
         (profile (when (equal type "Device")
                    (pw-lib-current-profile (pw-lib-object-id object))))
         (face (if (member id default-ids) 'pipewire-default-object 'default))
         (media-class (pw-lib-object-value object "media.class")))
    (when media-class
      (setq text (format "%s (%s)" text media-class)))
    (when profile
      (setq text (format "%s: %s" text profile)))
    (let ((volume-p (member type '("Node" "Port"))))
      (when (and volume-p (pw-lib-muted-p object))
        (setq face `(:inherit (pipewire-muted ,face))))
      (let ((label (propertize text 'face face)))
        (when volume-p
          (let ((volume (pw-lib-volume object)))
            (when volume
              (setq label (concat label " "
                                  (propertize (number-to-string volume)
                                              'face 'pipewire-volume))))))
        label))))

(defun pw-ui--insert-line (line object)
  (insert (propertize line 'pw-object-id (pw-lib-object-id object)) "\n"))

(defun pipewire-refresh (&optional _ignore-auto _noconfirm)
  "Refresh PipeWire buffer."
  (interactive)
  (when (and (not (eq major-mode 'pipewire-mode))
             (not (equal (buffer-name) pipewire-buffer)))
    (error "Not in a PipeWire buffer"))
  (pw-lib-refresh)
  (let ((inhibit-read-only t)
        (default-ids (mapcar #'cdr (pw-lib-default-nodes)))
        (current-line (count-lines (point-min) (min (1+ (point)) (point-max)))))
    (erase-buffer)
    (insert (pw-ui--label "Devices") "\n")
    (dolist (device (pw-lib-objects "Device"))
      (pw-ui--insert-line (pw-ui--object-label device default-ids) device)
      (dolist (node (pw-lib-children (pw-lib-object-id device) "Node"))
        (pw-ui--insert-line (concat "  " (pw-ui--object-label node default-ids)) node)
        (dolist (port (pw-lib-children (pw-lib-object-id node) "Port"))
          (pw-ui--insert-line (concat "    " (pw-ui--object-label port default-ids)) port))))
    (insert (pw-ui--label "Clients") "\n")
    (dolist (client (pw-lib-objects "Client"))
      (pw-ui--insert-line (pw-ui--object-label client default-ids) client))
    (goto-char (point-min))
    (forward-line (1- current-line))))

(defun pw-ui--current-object-id ()
  (get-text-property (point) 'pw-object-id))

(defun pw-ui--current-object (&optional use-default-p allowed-types)
  (let* ((id (pw-ui--current-object-id))
         (object (when id (pw-lib-get-object id))))
    (when (and object
               (not (null allowed-types))
               (not (member (pw-lib-object-type object) allowed-types)))
      (setq object nil))
    (when (and use-default-p (not object))
      (setq object (or (car (pw-lib-default-playback-ports))
                       (pw-lib-default-audio-sink))))
    object))

(defvar pw-ui--osd-timer nil)
(defvar pw-ui--osd-frame nil)
(defvar pw-ui--osd-buffer nil)
(defvar pw-ui--osd-buffer-name "*pipewire-osd*")

(defun pw-ui--osd-display (string)
  (when pw-ui--osd-timer
    (cancel-timer pw-ui--osd-timer))
  (let ((frame-width (+ 2 (length string))))
    (when (and pw-ui--osd-frame
               (not (= frame-width (frame-width pw-ui--osd-frame))))
      (delete-frame pw-ui--osd-frame)
      (setq pw-ui--osd-frame nil))
    (with-current-buffer (setq pw-ui--osd-buffer (get-buffer-create pw-ui--osd-buffer-name))
      (erase-buffer)
      (insert " " string)
      (setq mode-line-format nil)
      (unless pw-ui--osd-frame
        (setq pw-ui--osd-frame (make-frame `((unsplittable . t)
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
  (setq pw-ui--osd-timer
        (run-with-timer
         pipewire-osd-timeout nil
         (lambda ()
           (when pw-ui--osd-frame
             (ignore-errors (delete-frame pw-ui--osd-frame)))
           (when pw-ui--osd-buffer
             (ignore-errors (kill-buffer pw-ui--osd-buffer)))
           (setq pw-ui--osd-frame nil
                 pw-ui--osd-timer nil
                 pw-ui--osd-buffer nil)))))

(defmacro pw-ui--osd (&rest body)
  (declare (debug (body))
           (indent defun))
  (let (($string (gensym)))
    `(when (and window-system pipewire-osd-enable)
       (if-let ((,$string (progn ,@body)))
           (pw-ui--osd-display ,$string)))))

(defun pw-ui--update (&optional message)
  (if (get-buffer pipewire-buffer)
      (with-current-buffer pipewire-buffer
        (pipewire-refresh))
    (pw-lib-refresh))
  (when message
    (message message)))

(defun pw-ui--osd-volume (object)
  (pw-ui--osd
    (unless (eq (pw-ui--current-object-id) (pw-lib-object-id object))
      (let* ((object* (pw-lib-get-object (pw-lib-object-id object))) ; refreshed version
             (volume (pw-lib-volume object*))
             (muted-p (pw-lib-muted-p object*))
             (step (/ 100.0 pipewire-osd-width))
             (mark (if muted-p ?- ?|))
             (n-active (round (/ volume step)))
             (n-inactive (- pipewire-osd-width n-active)))
        (format "%s%s"
                (propertize (make-string n-active mark)
                            'face `(:background ,pipewire-osd-volume-on-color))
                (propertize (make-string n-inactive mark)
                            'face `(:background ,pipewire-osd-volume-off-color)))))))

(defun pw-ui--update-muted (object muted-p)
  (let* ((object-name (pw-ui--object-name object))
         (parent-node (pw-lib-parent-node object))
         (node-info (if parent-node
                       (format " in %s" (pw-ui--object-name parent-node))
                     "")))
    (pw-ui--update (format "%s%s %s" object-name node-info (if muted-p "muted" "unmuted")))))

;;;###autoload
(defun pipewire-toggle-muted ()
  "Switch mute status of an audio output or input.
If on a Node or Port in a PipeWire buffer, apply it on the given
object.  Otherwise apply it on the default audio sink."
  (interactive)
  (let* ((object (pw-ui--current-object t '("Node" "Port")))
         (muted-p (pw-lib-toggle-mute object)))
    (pw-ui--update-muted object muted-p)
    (pw-ui--osd-volume object)))

;;;###autoload
(defun pipewire-toggle-microphone ()
  "Switch mute status of the default audio input."
  (interactive)
  (let* ((object (car (pw-lib-default-capture-ports)))
         (muted-p (pw-lib-toggle-mute object)))
    (pw-ui--update-muted object muted-p)))

;;;###autoload
(defun pipewire-set-volume (volume &optional object single-p)
  "Set volume of an audio output or input.
VOLUME must be a number in the range 0-100.
If OBJECT is given (only Nodes and Ports are allowed) or if on a Node
or Port in a PipeWire buffer, apply it on the given object.
Otherwise apply it on the default audio sink."
  (interactive "nVolume: ")
  (setq volume (max 0 (min 100 volume)))
  (unless object
    (setq object (pw-ui--current-object t '("Node" "Port"))))
  (pw-lib-set-volume volume object single-p)
  (pw-ui--update (format "Volume %s for %s" volume (pw-ui--object-name object)))
  (pw-ui--osd-volume object))

(defun pw-ui--change-volume (step &optional single-p)
  (let* ((object (pw-ui--current-object t '("Node" "Port")))
         (volume (pw-lib-volume object))
         (new-volume (max 0 (min 100 (+ volume step)))))
    (pipewire-set-volume new-volume object single-p)))

;;;###autoload
(defun pipewire-increase-volume (&optional single-p)
  "Increase volume of an audio output or input.
The volume is increased by `pipewire-volume-step'.
If on a Node or Port in a PipeWire buffer, apply it on all the
channels of the given object.  Otherwise apply it on the default audio
sink."
  (interactive)
  (pw-ui--change-volume pipewire-volume-step single-p))

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
channels of the given object.  Otherwise apply it on the default audio
sink."
  (interactive)
  (pw-ui--change-volume (- pipewire-volume-step) single-p))

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
  (let ((object (or (pw-ui--current-object nil '("Device" "Node"))
                    (let* ((default-node-ids (mapcar #'cdr (pw-lib-default-nodes)))
                           (nodes (cl-remove-if
                                   (lambda (n) (member (pw-lib-object-id n) default-node-ids))
                                   (pw-lib-objects "Node")))
                           (node-mapping (mapcar (lambda (n) (cons (pw-ui--object-name n)
                                                                   (pw-lib-object-id n)))
                                                 nodes))
                           (node-name (completing-read "Default node: " node-mapping nil t)))
                      (pw-lib-get-object (cdr (assoc node-name node-mapping)))))))
    (pw-lib-set-default object nil)
    (pw-lib-set-default object t)
    (pw-ui--update)))

(defun pipewire-set-profile ()
  "Set profile of the device at the current point."
  (interactive)
  (if-let ((device (pw-ui--current-object nil '("Device")))
           (device-id (pw-lib-object-id device))
           (profiles (pw-lib-profiles device-id)))
      (progn
        (pw-lib-set-profile device-id (completing-read "Select profile: " profiles nil t))
        ;; Without this, ports of the device may not be displayed on the update:
        (sit-for 0)
        (pw-ui--update))
    (error "Nothing to set a profile for here")))

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

(provide 'pw-ui)

;;; pw-ui.el ends here
