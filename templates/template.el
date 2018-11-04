;;; package --- templates
;;; Code:
;;; Commentary:

(require 'seq)

(defvar template-file-name "template-file"
  "*The name of the file to look for when a 'find-file' request fails.")

(defvar template-replacements-alist
  '(("%filename%" . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%classname%" . (lambda () (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
    ("%package%" . (lambda () (template-insert-package (buffer-file-name))))
    ("%author%" . user-full-name))
  "A list which specifies what substitutions to perform.")

(defun template-insert-package (file-name)
  "Insert package from FILE-NAME."
  (let ((pwd (file-name-directory file-name))
        (ext (file-name-extension file-name))
        result)
    (cond ((member ext '("groovy" "java"))
           (resolve-jvm-package ext pwd))
          (t
           ""))))

(ert-deftest test-template-insert-package ()
  (should (equal (template-insert-package "/home/x/A.java") ""))
  (should (equal (template-insert-package "/home/x/A.groovy") ""))
  (should (equal (template-insert-package "/home/x/src/main/java/io/xxx/A.java") "package io.xxx"))
  (should (equal (template-insert-package "/home/x/src/main/groovy/io/xxx/A.groovy") "package io.xxx")))

(defun resolve-jvm-package (lang path)
  "Resolve LANG file package from PATH."
  (let ((convention (concat "src/main/" lang "/")))
    (if (string-match convention path)
        (let*
            ((result (substring path (match-end 0)))
             (list (ce-filter-empty-or-nil-string (split-string result "/")))
             (pkg (ce-string-join list ".")))
             (concat "package " pkg))
      "")))

(ert-deftest test-resolve-jvm-package ()
  (should (equal (resolve-jvm-package "java" "/a/b/c/src/main/java/io/xxx/core") "package io.xxx.core"))
  (should (equal (resolve-jvm-package "java" "/a/b/c/src/main/java/io/xxx/core/") "package io.xxx.core")))

(defun ce-filter-empty-or-nil-string (list)
  "Filter nil and empty strings from LIST."
  (seq-filter (lambda (x)
                (not (or
                      (equal "" x)
                      (not x)
                      )))
              list))

(defun ce-string-join (list sep)
  "Join LIST with SEP."
  (mapconcat 'identity list sep))

(defun find-template-file ()
  "Search the current directory and its parents for a file matching the name configured for template files."
  (let* ((file-name (buffer-file-name))
         (path (file-name-directory file-name))
         (ext (file-name-extension file-name))
         attempt result)
    (while (and (not result) (> (length path) 0))
      (setq attempt (concat path template-file-name "-" ext))
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
