;;; package --- Functions related to move cursor among buffers
;;; Code:
;;; Commentary:

(defun specific-window (n)
  "Select a specific window with order N."
  (other-window n))

(defun other-window-backwards ()
  "Select the previous window."
  (interactive)
  (specific-window -1))

(defun other-window-forwards ()
  "Select the next window."
  (interactive)
  (specific-window 1))

(defun write-names-other-window ()
  "Write a list of names in new window."
  (let ((names '("David" "Kirk" "Henry")))
    (switch-to-buffer-other-window "*test*") ;; Moves to another window
    (goto-char (point-min))                  ;; Goes to the beginning of the window buffer
    (mapc 'insert names))                    ;; Inserts all names
    (specific-window 1))                     ;; Goes back to first window

;;;(defun boldify-names ()
;;;  "Boldify everything."
;;;  (interactive)
;;;  (switch-to-buffer-other-window "*test*")
;;;  (goto-char (point-min))
;;;  (while (re-search-forward "bonjour \\(.+\\)!" nil 't)
;;;    (add-text-properties (match-beginning 1)
;;;                         (match-end 1)
;;;                         (list 'face 'bold)))
;;;  (other-window 1))

(global-set-key "\C-x\C-p" 'other-window-backward)

;;; buffer-movement.el ends here
