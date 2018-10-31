;;; package --- Functions related to move cursor among buffers
;;; Code:
;;; Commentary:

(defun ce-specific-window (&optional n) ;; make n optional
  "Select a specific window with order N."
  (let ((number (or n 1)))
    (other-window number)))

(defun ce-other-window-backwards ()
  "Select the previous window."
  (interactive)
  (specific-window -1))

(defun ce-other-window-forwards ()
  "Select the next window."
  (interactive)
  (specific-window))

(defun ce-say-hello (name)
  "Say hello to a person with name NAME."
  (let ((message (format "Hello %s \n" name)))
    (insert message)))

(defun ce-write-names-other-window ()
  "Write a list of names in new window."
  (interactive)
  (let ((names '("David" "Kirk" "Henry")))
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (goto-char (point-min))
    (mapc 'say-hello names))
  (other-window-forwards))

(defun ce-line-to-top ()
  "Move current line to top of window."
  (interactive)
  (recenter 0))

(global-set-key "\C-x\C-p" 'ce-other-window-backward)
(global-set-key "\M-!" 'ce-line-to-top)

;;; buffer-movement.el ends here
