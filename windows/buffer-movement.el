;;; package --- Functions related to move cursor among buffers
;;; Code:
;;; Commentary:

(defun specific-window (&optional n) ;; make n optional
  "Select a specific window with order N."
  (if n
      (other-window n)
    (other-window 1)))

(defun other-window-backwards ()
  "Select the previous window."
  (interactive)
  (specific-window -1))

(defun other-window-forwards ()
  "Select the next window."
  (interactive)
  (specific-window))

(defun say-hello (name)
  "Say hello to a person with name NAME."
  (let ((message (format "Hello %s \n" name)))
    (insert message)))

(defun write-names-other-window ()
  "Write a list of names in new window."
  (interactive)
  (let ((names '("David" "Kirk" "Henry")))
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (goto-char (point-min))
    (mapc 'say-hello names))
    (other-window-forwards))

(write-names-other-window)

(global-set-key "\C-x\C-p" 'other-window-backward)

;;; buffer-movement.el ends here
