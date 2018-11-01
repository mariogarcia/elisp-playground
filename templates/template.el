;;; package --- templates
;;; Code:
;;; Commentary:

(defvar template-file-name "file-template"
  "*The name of the file to look for when a 'find-file' request fails.")

(defvar template-replacements-alist
  '(("%filename%" . (lambda ()
                      (file-name-nondirectory (buffer-file-name)))))
  "A list which specifies what substitutions to perform.")

(defun find-template-file ()
  "Search the current directory and its parents for a file matching the name configured for template files."
  (let ((path (file-name-directory (buffer-file-name)))
        (ext (file-name-extension (buffer-file-name)))
        attempt result)
    (while (and (not result) (> (length path) 0))
      (setq attempt (concat path template-file-name "-" ext))
      (message "finding %s" attempt)
      (if (file-readable-p attempt)
          (setq result attempt)
        (setq path (if (string-equal path "/")
                       ""
                     (file-name-directory (substring path 0 -1))))))
    result))

(defun template-file-not-found-hook ()
  "Call this when a 'find-file' command has not been able to find the specified file."
  (condition-case nil
      (if (and (find-template-file)
               (y-or-n-p "Start with template file? "))
          (progn (buffer-disable-undo)
                 (insert-file-contents (find-template-file))
                 (goto-char (point-min))

                 (let ((the-list template-replacements-alist))
                   (while the-list
                     (goto-char (point-min))
                     (replace-string (car (car the-list))
                                     (funcall (cdr (car the-list)))
                                     nil)
                     (setq the-list (cdr the-list))))
                 (goto-char (point-min))
                 (buffer-enable-undo)
                 (set-buffer-modified-p nil)))
    ('quit (kill-buffer (current-buffer))
           (signal 'quit "Quit"))))

(or (memq 'template-file-not-found-hook find-file-not-found-functions)
    (setq find-file-not-found-functions
          (append find-file-not-found-functions '(template-file-not-found-hook))))

(provide 'template)

;;; template.el ends here
