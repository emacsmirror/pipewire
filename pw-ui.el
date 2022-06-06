;;; pw-ui.el --- PipeWire user interface  -*- lexical-binding: t -*-

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

;; PipeWire user interface based on pw-lib.
;; An interactive buffer can be displayed using `M-x pipewire'.
;; `pipewire-increase-volume', `pipewire-decrease-volume' and
;; `pipewire-toggle-muted' functions are also suitable to bind on the
;; multimedia keys.

(require 'pw-lib)

(defgroup pipewire ()
  "PipeWire user interface.")

(defcustom pipewire-volume-step 5
  "How many percent points to add or subtract when changing volumes."
  :type 'number
  :group 'pipewire)

(defface pipewire-label-face
  '((t (:weight bold :overline t)))
  "Face to use for PipeWire node group labels."
  :group 'pipewire)

(defface pipewire-default-object-face
  '((t (:weight bold)))
  "Face to use for PipeWire default sinks and sources."
  :group 'pipewire)

(defface pipewire-muted-face
  '((t (:strike-through t)))
  "Face to use for muted PipeWire sinks and sources."
  :group 'pipewire)

(defface pipewire-volume-face
  '((t (:inverse-video t)))
  "Face to use for displaying volumes of PipeWire objects."
  :group 'pipewire)

(defvar pipewire-buffer "*PipeWire*")

(defun pw-ui--label (label)
  (propertize (concat label ":") 'face 'pipewire-label-face))

(defun pw-ui--object-volume (object)
  (propertize (pw-lib-volume object) 'face 'pipewire-volume-face))

(defun pw-ui--object-name (object)
  (let* ((type (pw-lib-object-type object))
         (description-properties (if (equal type "Client")
                                     '("application.name")
                                   (let ((prefix (concat (downcase type) ".")))
                                     (mapcar #'(lambda (suffix) (concat prefix suffix))
                                             '("description" "name"))))))
    (or (cl-find-if #'identity
                    (mapcar #'(lambda (p) (pw-lib-object-value object p))
                            description-properties))
        "")))

(defun pw-ui--object-label (object default-ids)
  (let* ((id (pw-lib-object-id object))
         (type (pw-lib-object-type object))
         (text (format "%4s: %s" id (pw-ui--object-name object)))
         (face (if (member id default-ids) 'pipewire-default-object-face 'default))
         (media-class (pw-lib-object-value object "media.class")))
    (when media-class
      (setq text (format "%s (%s)" text media-class)))
    (let ((volume-p (member type '("Node" "Port"))))
      (when (and volume-p (pw-lib-muted-p object))
        (setq face `(:inherit (pipewire-muted-face ,face))))
      (let ((label (propertize text 'face face)))
        (when volume-p
          (let ((volume (pw-lib-volume object)))
            (when volume
              (setq label (concat label " "
                                  (propertize (number-to-string volume)
                                              'face 'pipewire-volume-face))))))
        label))))

(defun pw-ui--insert-line (line object)
  (insert (propertize line 'pw-object-id (pw-lib-object-id object)) "\n"))

(defun pipewire-refresh (&optional ignore-auto noconfirm)
  "Refresh PipeWire buffer."
  (interactive)
  (when (and (not (eq major-mode 'pipewire-mode))
             (not (equal (buffer-name) pipewire-buffer)))
    (error "Not in a PipeWire buffer"))
  (pw-lib-refresh)
  (let ((inhibit-read-only t)
        (bindings (pw-lib-bindings))
        (default-ids (mapcar #'cdr (pw-lib-default-nodes)))
        (current-line (count-lines (point-min) (min (1+ (point)) (point-max)))))
    (erase-buffer)
    (insert (pw-ui--label "Devices") "\n")
    (dolist (device (pw-lib-objects "Device"))
      (pw-ui--insert-line (pw-ui--object-label device default-ids) device)
      (dolist (node (pw-lib-children (pw-lib-object-id device) bindings "Node"))
        (pw-ui--insert-line (concat "  " (pw-ui--object-label node default-ids)) node)
        (dolist (port (pw-lib-children (pw-lib-object-id node) bindings "Port"))
          (pw-ui--insert-line (concat "    " (pw-ui--object-label port default-ids)) port))))
    (insert (pw-ui--label "Clients") "\n")
    (dolist (client (pw-lib-objects "Client"))
      (pw-ui--insert-line (pw-ui--object-label client default-ids) client))
    (goto-char (point-min))
    (forward-line (1- current-line))))

(defun pw-ui--update (&optional message)
  (when (get-buffer pipewire-buffer)
    (with-current-buffer pipewire-buffer
      (pipewire-refresh)))
  (when message
    (message message)))

(defun pw-ui--current-object (&optional use-default-p allowed-types)
  (let* ((id (get-text-property (point) 'pw-object-id))
         (object (when id (pw-lib-get-object id))))
    (when (and object
               (not (null allowed-types))
               (not (member (pw-lib-object-type object) allowed-types)))
      (setq object nil))
    (when (and use-default-p (not object))
      (setq object (pw-lib-default-audio-sink)))
    object))

;;;###autoload
(defun pipewire-toggle-muted ()
  "Switch mute status of an audio output or input.
If on a Node or Port in a PipeWire buffer, apply it on the given
object.  Otherwise apply it on the default audio sink."
  (interactive)
  (let* ((object (pw-ui--current-object t '("Node" "Port")))
         (muted-p (pw-lib-toggle-mute object)))
    (pw-ui--update (format "%s %s" (pw-ui--object-name object) (if muted-p "muted" "unmuted")))))

;;;###autoload
(defun pipewire-set-volume (volume &optional object)
  "Set volume of an audio output or input.
VOLUME must be a number in the range 0-100.
If OBJECT is given (only Nodes and Ports are allowed) or if on a Node
or Port in a PipeWire buffer, apply it on the given object.
Otherwise apply it on the default audio sink."
  (interactive "nVolume: ")
  (setq volume (max 0 (min 100 volume)))
  (unless object
    (setq object (pw-ui--current-object t '("Node" "Port"))))
  (pw-lib-set-volume volume object)
  (pw-ui--update (format "Volume %s for %s" volume (pw-ui--object-name object))))

(defun pw-ui--change-volume (step)
  (let* ((object (pw-ui--current-object t '("Node" "Port")))
         (volume (pw-lib-volume object))
         (new-volume (max 0 (min 100 (+ volume step)))))
    (pipewire-set-volume new-volume object)))
    
;;;###autoload
(defun pipewire-increase-volume ()
  "Increase volume of an audio output or input.
The volume is increased by `pipewire-volume-step'.
If on a Node or Port in a PipeWire buffer, apply it on the given
object.  Otherwise apply it on the default audio sink."
  (interactive)
  (pw-ui--change-volume pipewire-volume-step))

;;;###autoload
(defun pipewire-decrease-volume ()
  "Decrease volume of an audio output or input.
The volume is decreased by `pipewire-volume-step'.
If on a Node or Port in a PipeWire buffer, apply it on the given
object.  Otherwise apply it on the default audio sink."
  (interactive)
  (pw-ui--change-volume (- pipewire-volume-step)))

;;;###autoload
(defun pipewire-set-default ()
  "Set default sink or source.
If on a Node in a PipeWire buffer, apply it on the given object.
Otherwise ask for the Node to set as the default Node."
  (interactive)
  (let ((object (or (pw-ui--current-object nil '("Node"))
                    (let* ((default-node-ids (mapcar #'cdr (pw-lib-default-nodes)))
                           (nodes (cl-remove-if
                                   #'(lambda (n) (member (pw-lib-object-id n) default-node-ids))
                                   (pw-lib-objects "Node")))
                           (node-mapping (mapcar #'(lambda (n) (cons (pw-ui--object-name n)
                                                                     (pw-lib-object-id n)))
                                                 nodes))
                           (node-name (completing-read "Default node: " node-mapping nil t)))
                      (pw-lib-get-object (cdr (assoc node-name node-mapping)))))))
    (pw-lib-set-default object nil)
    (pw-lib-set-default object t)
    (pw-ui--update)))

(defvar pipewire-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'pipewire-set-default)
    (define-key map "m" 'pipewire-toggle-muted)
    (define-key map "v" 'pipewire-set-volume)
    (define-key map "+" 'pipewire-increase-volume)
    (define-key map "=" 'pipewire-increase-volume)
    (define-key map "-" 'pipewire-decrease-volume)
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
  (switch-to-buffer pipewire-buffer)
  (pipewire-refresh)
  (pipewire-mode))

(provide 'pw-ui)
