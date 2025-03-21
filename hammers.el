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
  (message "tangling: %s" file)
  (find-file file)
  (org-babel-tangle)
  (kill-buffer)
  (message "tangled: %s" file))

(defun m/evaluate (file)
  "Give an 'org-mode' FILE, tangle the source code."
  (interactive "fOrg File: ")
  (let ((resolved (m/resolve file)))
    (message "evaluating: %s" resolved)
    (find-file resolved)
    (org-babel-execute-buffer)
    (kill-buffer)
    (message "evaluated: %s" resolved))
  )

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


(defun m/system (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer 
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun m/system-pipefail (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer 
    (if (eq 0 (apply 'call-process program nil (current-buffer) nil args))
        (string-trim-right (buffer-string))
      (progn
        (message "%s" (buffer-string))
        (throw 'm/system-pipefail (format "%s execute fail" program))))))

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
(defun m/copy (src dest)
  "Link the SRC to the DEST."
  (let* ((resolved-src (m/resolve src))
	 (resolved-dest (m/resolve dest)))
    (message "copy: %s to %s" resolved-src resolved-dest)
    (make-directory (directory-file-name resolved-dest) t)
    (m/system-pipefail "rsync" "-ravz"  resolved-src resolved-dest)
    (message "copied: %s to %s" resolved-src resolved-dest)))

(defun m/clone (src dest)
  "clone the SRC to the DEST."
  (let* ((resolved-src (m/resolve src))
	 (resolved-dest (m/resolve dest))
         (src-branch (m/system-pipefail "git" "-C" resolved-src "rev-parse" "--abbrev-ref" "HEAD"))
         (src-url (m/system-pipefail "git" "-C" resolved-src "remote" "get-url" "origin"))
         )
    (message "clone: %s to %s" resolved-src resolved-dest)
    (if (file-directory-p resolved-dest)
        (let* ((dest-branch (m/system-pipefail "git" "-C" resolved-dest "rev-parse" "--abbrev-ref" "HEAD")))
	  (progn
	    (message "repo under: %s exists, pull from %s" resolved-dest resolved-src)
            ;; TODO need check current dir is valid git & the git has valid filelocal remote
	    (m/system-pipefail "git" "-C" resolved-dest  "pull" "filelocal" dest-branch)))
      (progn
	(message "repo under: %s not exists, clone from  %s" resolved-dest resolved-src)
        (make-directory (directory-file-name resolved-dest) t)
	(m/system-pipefail "git" "clone" "--no-hardlinks" resolved-src resolved-dest)
	(m/system-pipefail "git" "-C" resolved-dest "remote" "rename" "origin" "filelocal")
	(m/system-pipefail "git" "-C" resolved-dest "remote" "add" "origin" src-url)
	(m/system-pipefail "git" "-C" resolved-dest "checkout" src-branch)
	))))

(defun m/untar (src dest)
  "clone the SRC to the DEST."
  (let* ((resolved-src (m/resolve src))
	 (resolved-dest (m/resolve dest)))
    (message "untar: %s to %s" resolved-src resolved-dest)
    (make-directory resolved-dest t)
    (let* ((tmp (make-temp-file "untar" t)))
      (m/system-pipefail "tar" "-xJf" resolved-src "-C" tmp)
      (m/system-pipefail "rsync" "-ravz" (format "%s/" tmp) resolved-dest))
    (message "untared: %s to %s" resolved-src resolved-dest)))

(defun m/rsync (src dest)
  "clone the SRC to the DEST."
  (let* ((resolved-src (m/resolve src))
	 (resolved-dest (m/resolve dest)))
    (message "rsync: %s to %s" resolved-src resolved-dest)
    (make-directory (directory-file-name resolved-dest) t)
    (m/system-pipefail "rsync" "-raz" resolved-src resolved-dest)
    (message "synced: %s to %s" resolved-src resolved-dest)))

(defun m/link (src dest)
  "Link the SRC to the DEST."
  (let* ((resolved-src (m/resolve src))
	 (resolved-dest (m/resolve dest)))
    (message "linking: %s to %s" resolved-src resolved-dest)
    (make-directory (directory-file-name resolved-dest) t)
    (m/system-pipefail "ln" "-sfn" resolved-src resolved-dest)
    (message "linked: %s to %s" resolved-src resolved-dest)))

(defconst m/root.d (directory-file-name
		    (if load-file-name
		        (file-name-directory load-file-name)
		      (file-name-directory (buffer-file-name)))))

(defvar m/home.d (directory-file-name (expand-file-name "~")))
(defvar m/mail.d (expand-file-name "mails" m/home.d))
(defvar m/xdg.conf.d (expand-file-name ".config" m/home.d))
(defvar m/xdg.cache.d (expand-file-name ".cache" m/home.d))
(defvar m/xdg.data.d (expand-file-name ".local/share" m/home.d))
(defvar m/xdg.state.d (expand-file-name ".local/state" m/home.d))

(defun tangle-if-absent (path)
  (let* ((filename (m/resolve path)))
    (if (file-exists-p filename)
	"no"
      filename)))

(message "updating submodule...")
(m/system-pipefail "git" "submodule" "update" "--init" "--recursive")
(message "all submodule updated")

(m/tangles "${m/root.d}/hammers/emacs/*.org")
(m/tangles "${m/root.d}/hammers/emacs/snippets/*.org")
(m/tangles "${m/root.d}/hammers/git/*.org")
(m/untar "${m/root.d}/hammers/emacs/3rdparty/pyim-bigdict.tar.xz" "${m/xdg.data.d}/emacs/pyim/dicts")

(if (or (eq m/os 'macos)
	(eq m/os 'linux))
    (progn
      (m/tangles "${m/root.d}/hammers/scripts/*.org")
      (m/tangles "${m/root.d}/hammers/msmtp/*.org")
      (m/tangles "${m/root.d}/hammers/proxychains/*.org")
      (m/tangles "${m/root.d}/hammers/notmuch/*.org")
      (m/tangles "${m/root.d}/hammers/mime/*.org")
      (m/tangles "${m/root.d}/hammers/mbsync/*.org")
      (m/tangles "${m/root.d}/hammers/zsh/*.org")
      (m/evaluates "${m/root.d}/hammers/zsh/*.org")
      (m/tangles "${m/root.d}/hammers/ssh/*.org")
      (m/tangles "${m/root.d}/hammers/csh/*.org")
      (m/tangles "${m/root.d}/hammers/direnv/*.org")
      (m/tangles "${m/root.d}/hammers/tmux/*.org")
      (m/tangles "${m/root.d}/hammers/vim/*.org")
      (m/tangles "${m/root.d}/hammers/rg/*.org")
      (m/tangles "${m/root.d}/hammers/gnupg/*.org")
      (m/tangles "${m/root.d}/hammers/gdb/*.org")))

(if (or (eq m/os 'macos)
	(eq m/os 'linux))
    (progn
      (m/copy "${m/root.d}/hammers/emacs/3rdparty/modes/" "${m/xdg.conf.d}/emacs/3rdparty/modes")
      (m/copy "${m/root.d}/hammers/emacs/3rdparty/bin/" "${m/xdg.data.d}/emacs/bin")))

;; tangle brew and hammerspoon
(if (eq m/os 'macos)
    (progn (m/tangles "${m/root.d}/hammers/brew/*.org")
	   (m/tangles "${m/root.d}/hammers/hammerspoon/*.org")
	   (m/tangles "${m/root.d}/hammers/yabai/*.org")
	   (m/tangles "${m/root.d}/hammers/karabiner/*.org")
	   (m/tangles "${m/root.d}/hammers/skhd/*.org")))

;; copy hammerspoon config
(if (eq m/os 'macos)
    (progn 
      (m/copy "${m/root.d}/hammers/hammerspoon/Spoons" "${m/xdg.conf.d}/hammerspoon/")))

;; eval config script inside org for macos only
(if (eq m/os 'macos)
    (progn
      (m/evaluates "${m/root.d}/hammers/brew/*.org")
      (m/evaluates "${m/root.d}/hammers/hammerspoon/*.org")))

;; eval config script inside org for the linux or macos
(if (or (eq m/os 'macos)
	(eq m/os 'linux))
    (progn (m/evaluates "${m/root.d}/hammers/zsh/*.org")))

;; clone config file
(if (or (eq m/os 'macos)
	(eq m/os 'linux))
    (progn
      ;; start copy config file
      (m/clone "${m/root.d}/hammers/emacs/3rdparty/systemrdl-mode" "${m/xdg.conf.d}/emacs/3rdparty/systemrdl-mode")
      (m/clone "${m/root.d}/hammers/tmux/plugins/tpm" "${m/xdg.conf.d}/tmux/plugins/tpm")
      (m/clone "${m/root.d}/hammers/tmux/plugins/tmux-sensible" "${m/xdg.conf.d}/tmux/plugins/tmux-sensible")
      (m/clone "${m/root.d}/hammers/tmux/plugins/tmux-resurrect" "${m/xdg.conf.d}/tmux/plugins/tmux-resurrect")
      (m/clone "${m/root.d}/hammers/tmux/plugins/tmux-copycat" "${m/xdg.conf.d}/tmux/plugins/tmux-copycat")
      (m/clone "${m/root.d}/hammers/tmux/plugins/tmux-open" "${m/xdg.conf.d}/tmux/plugins/tmux-open")
      (m/clone "${m/root.d}/hammers/tmux/plugins/tmux-urlview" "${m/xdg.conf.d}/tmux/plugins/tmux-urlview")
      (m/clone "${m/root.d}/hammers/tmux/plugins/tmux-yank" "${m/xdg.conf.d}/tmux/plugins/tmux-yank")
      (m/clone "${m/root.d}/hammers/tmux/plugins/extrakto" "${m/xdg.conf.d}/tmux/plugins/extrakto")
      (m/clone "${m/root.d}/hammers/tmux/plugins/tmux-colors-solarized" "${m/xdg.conf.d}/tmux/plugins/tmux-colors-solarized")
      (m/clone "${m/root.d}/hammers/tmux/plugins/tmux-themepack" "${m/xdg.conf.d}/tmux/plugins/tmux-themepack")
      (m/clone "${m/root.d}/hammers/zsh/zplug" "${m/xdg.conf.d}/zsh/zplug/src")
      (m/clone "${m/root.d}/hammers/zsh/plugins/junegunn/fzf" "${m/xdg.conf.d}/zsh/plugins/junegunn/fzf")
      (m/clone "${m/root.d}/hammers/zsh/plugins/robbyrussell/oh-my-zsh" "${m/xdg.conf.d}/zsh/plugins/robbyrussell/oh-my-zsh")
      (m/clone "${m/root.d}/hammers/zsh/plugins/zsh-users/zsh-autosuggestions" "${m/xdg.conf.d}/zsh/plugins/zsh-users/zsh-autosuggestions")
      (m/clone "${m/root.d}/hammers/zsh/plugins/zsh-users/zsh-completions" "${m/xdg.conf.d}/zsh/plugins/zsh-users/zsh-completions")
      (m/clone "${m/root.d}/hammers/zsh/plugins/zsh-users/zsh-history-substring-search" "${m/xdg.conf.d}/zsh/plugins/zsh-users/zsh-history-substring-search")
      (m/clone "${m/root.d}/hammers/zsh/plugins/zsh-users/zsh-syntax-highlighting" "${m/xdg.conf.d}/zsh/plugins/zsh-users/zsh-syntax-highlighting")
      (m/clone "${m/root.d}/hammers/gdb/plugins/dashboard" "${m/xdg.conf.d}/gdb/dashboard")
      (m/clone "${m/root.d}/hammers/vim/plugins/Vundle.vim" "${m/xdg.conf.d}/vim/plugins/Vundle.vim")
      (m/clone "${m/root.d}/hammers/vim/plugins/ctrlp.vim" "${m/xdg.conf.d}/vim/plugins/ctrlp.vim")
      (m/clone "${m/root.d}/hammers/vim/plugins/ctrlsf.vim" "${m/xdg.conf.d}/vim/plugins/ctrlsf.vim")
      (m/clone "${m/root.d}/hammers/vim/plugins/delimitMate" "${m/xdg.conf.d}/vim/plugins/delimitMate")
      (m/clone "${m/root.d}/hammers/vim/plugins/incsearch-easymotion.vim" "${m/xdg.conf.d}/vim/plugins/incsearch-easymotion.vim")
      (m/clone "${m/root.d}/hammers/vim/plugins/incsearch-fuzzy.vim" "${m/xdg.conf.d}/vim/plugins/incsearch-fuzzy.vim")
      (m/clone "${m/root.d}/hammers/vim/plugins/incsearch.vim" "${m/xdg.conf.d}/vim/plugins/incsearch.vim")
      (m/clone "${m/root.d}/hammers/vim/plugins/numbers.vim" "${m/xdg.conf.d}/vim/plugins/numbers.vim")
      (m/clone "${m/root.d}/hammers/vim/plugins/surround.vim" "${m/xdg.conf.d}/vim/plugins/surround.vim")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-airline" "${m/xdg.conf.d}/vim/plugins/vim-airline")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-airline-themes" "${m/xdg.conf.d}/vim/plugins/vim-airline-themes")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-autoformat" "${m/xdg.conf.d}/vim/plugins/vim-autoformat")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-easy-align" "${m/xdg.conf.d}/vim/plugins/vim-easy-align")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-easymotion" "${m/xdg.conf.d}/vim/plugins/vim-easymotion")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-gitgutter" "${m/xdg.conf.d}/vim/plugins/vim-gitgutter")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-go" "${m/xdg.conf.d}/vim/plugins/vim-go")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-javascript" "${m/xdg.conf.d}/vim/plugins/vim-javascript")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-json-bundle" "${m/xdg.conf.d}/vim/plugins/vim-json-bundle")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-jsx" "${m/xdg.conf.d}/vim/plugins/vim-jsx")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-multiple-cursors" "${m/xdg.conf.d}/vim/plugins/vim-multiple-cursors")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-mustache-handlebars" "${m/xdg.conf.d}/vim/plugins/vim-mustache-handlebars")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-obsession" "${m/xdg.conf.d}/vim/plugins/vim-obsession")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-tmux-navigator" "${m/xdg.conf.d}/vim/plugins/vim-tmux-navigator")
      (m/clone "${m/root.d}/hammers/vim/plugins/gruvbox" "${m/xdg.conf.d}/vim/plugins/gruvbox")
      (m/clone "${m/root.d}/hammers/vim/plugins/awesome-vim-colorschemes" "${m/xdg.conf.d}/vim/plugins/awesome-vim-colorschemes")
      (m/clone "${m/root.d}/hammers/vim/plugins/vim-colors-solarized" "${m/xdg.conf.d}/vim/plugins/vim-colors-solarized")))

;; init chinese
(if (eq m/os 'macos)
    (progn  (m/clone "${m/root.d}/hammers/emacs/3rdparty/liberime" "${m/xdg.conf.d}/emacs/3rdparty/liberime")
            (shell-command (m/resolve "${m/root.d}/hammers/emacs/3rdparty/rimeable"))))

(if (or (eq m/os 'macos)
	(eq m/os 'linux))
    (progn (shell-command (m/resolve "${m/root.d}/hammers/fonts/powerline/install.sh"))))

(message "Finished building hammers. Please Restart Emacs.")

(provide 'm/hammers)

;;; hammers.el ends here
