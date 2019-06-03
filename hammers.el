;;; hammers -- setup hammers config

;;; Commentary:

;;; Code:

(require 'org)
(require 'ob)
(require 'ob-tangle)
(require 'em-glob)
(require 'subr-x)

(defun m/tangle (file)
  "Give an 'org-mode' FILE, tangle the source code."
  (interactive "fOrg File: ")
  (find-file file)
  (org-babel-tangle)
  (kill-buffer))

(defun m/resolve (path)
  "Interpolation variable like ${var} $var in PATH with environment or elisp variables."
  (if (string-prefix-p "~/" path)
      (m/resolve (concat (getenv "HOME") (substring path 1)))
    (let ((s (or (string-match "${\\([^ }]*\\)}" path)
                 (string-match "$\\([A-z_]*\\)" path)))
          (e (match-end 0)))
      (if (not s)
          path
        (m/resolve
         (concat (substring path 0 s) (m/var (match-string 1 path)) (substring path e))))))
  )

(defun m/files (glob &optional full)
  "Give an GLOB, return files matched GLOB.  If FULL is specified, return absolute pathnames for each file."
  (interactive "M")
  (let ((resolved (m/resolve glob)))
    (condition-case nil
        (directory-files (file-name-directory resolved)
                         full
                         (eshell-glob-regexp (file-name-nondirectory resolved)))
      (error '()))))

(defun m/var (name)
  "Return value of variable or environment identified by NAME."
  (or (getenv name) (eval (read name))))

(defun m/envsubst (str)
  "Interpolation variable like ${var} $var in STR with environment or elisp variables."
  (if (string-prefix-p "~/" str)
      (m/envsubst (concat (getenv "HOME") (substring str 1)))
    (let ((s (or (string-match "${\\([^ }]*\\)}" str)
                 (string-match "$\\([A-z_]*\\)" str)))
          (e (match-end 0)))
      (if (not s)
          str
        (m/envsubst
         (concat (substring str 0 s) (m/var (match-string 1 str)) (substring str e)))))))

(defun m/tangles (path)
  "Give an PATH, tangle all 'org-mode' file."
  (interactive "D")
  (mapc 'm/tangle (m/files path t)))

(defun m/path (path &rest extra)
  "Return path according the PATH & EXTRA arguments."
  (let ((parts (cons (m/resolve path) extra)))
    (concat "/" (mapconcat '(lambda (p) (cond ((and (string-prefix-p "/" p) (string-suffix-p "/" p)) (substring p 1 -1))
                                         ((string-prefix-p "/" p) (substring p 1))
                                         ((string-suffix-p "/" p) (substring p 0 -1))
                                         (t p)))
                           parts "/"))))

(defconst m/root (if load-file-name
                             (file-name-directory load-file-name)
                           (file-name-directory (buffer-file-name))))

(m/tangles "${m/root}/hammers/emacs/*.org")

(message "Finished building hammers. Resetting Emacs...")
(require 'bootstrap (m/path "${user-emacs-directory}" "bootstrap.el"))

(provide 'm/hammers)

;;; hammers.el ends here
