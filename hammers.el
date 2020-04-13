;;; hammers -- setup hammers config

;;; Commentary:

;;; Code:

(require 'org)
(require 'ob)
(require 'ob-tangle)
(require 'em-glob)
(require 'subr-x)

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (dot . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (latex . t)
   (ledger . t)
   (ocaml . nil)
   (octave . t)
   (plantuml . t)
   (python . t)
   (ruby . t)
   (screen . nil)
   (shell . t)
   (sql . t)
   (sqlite . t)))

(defconst m/os
  (let ((os (symbol-name system-type)))
    (cond ((string= os "darwin") 'macos)
	  ((string-prefix-p "gnu" os) 'linux)
	  ((or (string-prefix-p "ms" os) (string-prefix-p "windows" os)) 'windows))))

(defun m/tangle (file)
  "Give an 'org-mode' FILE, tangle the source code."
  (interactive "fOrg File: ")
  (find-file file)
  (org-babel-tangle)
  (kill-buffer))

(defun m/evaluate (file)
  "Give an 'org-mode' FILE, tangle the source code."
  (interactive "fOrg File: ")
  (find-file file)
  (org-babel-execute-buffer)
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

(defun m/evaluates (path)
  "Give an PATH, tangle all 'org-mode' file."
  (interactive "D")
  (mapc 'm/evaluate (m/files path t)))

(defun m/path (path &rest extra)
  "Return path according the PATH & EXTRA arguments."
  (let ((parts (cons (m/resolve path) extra)))
    (concat "/" (mapconcat #'(lambda (p) (cond ((and (string-prefix-p "/" p) (string-suffix-p "/" p)) (substring p 1 -1))
                                          ((string-prefix-p "/" p) (substring p 1))
                                          ((string-suffix-p "/" p) (substring p 0 -1))
                                          (t p)))
                           parts "/"))))

(defun m/link (src dest)
  "Link the SRC to the DEST."
  (shell-command (concat "mkdir -p $(dirname " (m/resolve dest) ")"))
  (shell-command (concat "ln -sfn " (m/resolve src) " " (m/resolve dest))))

(defconst m/root (directory-file-name
		  (if load-file-name
		      (file-name-directory load-file-name)
		    (file-name-directory (buffer-file-name)))))

(defvar m/conf.d (expand-file-name user-emacs-directory))

(m/tangles "${m/root}/hammers/emacs/*.org")
(m/tangles "${m/root}/hammers/zsh/*.org")
(m/tangles "${m/root}/hammers/git/*.org")
(m/tangles "${m/root}/hammers/ssh/*.org")
(m/tangles "${m/root}/hammers/tmux/*.org")
(m/tangles "${m/root}/hammers/vim/*.org")
(m/tangles "${m/root}/hammers/rg/*.org")
(m/tangles "${m/root}/hammers/gdb/*.org")
(if (eq m/os 'macos)
    (progn (m/tangles "${m/root}/hammers/brew/*.org")
	   (m/evaluates "${m/root}/hammers/brew/*.org")))

(if (or (eq m/os 'macos)
	(eq m/os 'linux))
    (progn     (m/evaluate "${m/root}/hammers/emacs/chinese.org")
	       (m/evaluate "${m/root}/hammers/emacs/lsp.org")))
(m/link "${m/root}/hammers/tmux/plugins" "~/.tmux/plugins")
(m/link "${m/root}/hammers/zsh/zplug" "~/.zsh/zplug")
(m/link "${m/root}/hammers/vim/bundle" "~/.vim/bundle")
(m/link "${m/root}/hammers/emacs/3rdparty" "~/.emacs.d/3rdparty")

(message "Finished building hammers. Please Restart Emacs.")

(provide 'm/hammers)

;;; hammers.el ends here
