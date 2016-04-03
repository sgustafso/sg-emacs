;;; sg-edit.el --- colliection of basic editing features.
;; Copyright (C) 2008-2016 Stephen Gustafson

;; Author: Stephen Gustafson

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.
;; If not, write to the Free Software Foundation, 675 Mass Ave,
;; Cambridge, MA 02139, USA.

;; ------------------------------------------------------------
;;; Commentary:

;; This package provides a collection of basic editing and navigation functions
;; for Emacs. Some of these do things not supported in vanilla Emacs, while
;; some are slight modifications on builtin functions to fix things that annoy
;; me (eg., the behavior of backward-kill-word for deleting
;; trailing whitespace).
;; To use, add (require 'sg-edit) to your emacs.el.
;; Some default keybindings can be set afterwards by adding (sg-edit-default-keybindings)


;;
;; Copying, killing, etc.
;;


(defun sg-copy-line-as-kill ()
  "Save the line as if killed, but don't kill it."
  (interactive)
  (save-excursion
    (let ((beg (point)))
      (end-of-line)
      (copy-region-as-kill beg (point)))))


(defun sg-backward-kill-word ()
  "Kill characters backward until encountering the beginning of a word.
  Unlike backward-kill-ward, the first character encountered is a whitespace
  character, then all whitespace up the first word encountered will be removed,
  but not the word."
  (interactive)
  (let (wordStart)
    (save-excursion
      ;; (if (re-search-backward "[^[:space:]]" nil t)
      (if (re-search-backward "[^ \t\r\n]" nil t)
          (setq wordStart (point))))
    (if (not wordStart)
        (return nil))
    (if (>=  (+ wordStart 1) (point))
          (backward-kill-word 1)
      (delete-region (+ wordStart 1) (point)))))


;;
;; Indentation
;;


(defun sg-shift-region (direction)
  "Shifts a region of code rigidly according to @direction.
   Use -1,1 to specify direction.
   If no region is selected, the current line from the cursor position
   and onwards is shifted."
  (interactive)
  (let (pos1 pos2 insertOrRemoveTab endPt jumpToEnd)
    (setq insertOrRemoveTab nil)
    (if (and transient-mark-mode mark-active)
        (progn (setq pos1 (region-beginning))
               (setq pos2 (region-end)))
      (progn
        (setq pos1 (point))
        (setq pos2 (line-end-position))
        (setq insertOrRemoveTab t)
        (if (not (number-or-marker-p (string-match "[^ \n]" (thing-at-point 'line))))
            (setq jumpToEnd t))
        ;(if (not (equal pos1 (line-beginning-position)))
        ;    (setq insertOrRemoveTab t))
        ;;(message "%s to %s" pos1 pos2))
      )
    (setq endPt (point))
    (save-excursion
      (if (> direction 0)
          (if insertOrRemoveTab
              (insert-tab)
            (indent-rigidly pos1 pos2 tab-width))
        (indent-rigidly pos1 pos2 (* tab-width direction)))
      (setq deactivate-mark nil))
    (if jumpToEnd (goto-char (line-end-position))))))


(defun sg-shift-region-left ()
  "Rigidly shift a region of code to the left"
  (interactive)
  (sg-shift-region -1))


(defun sg-shift-region-right ()
  "Rigidly shift a region of code to the right"
  (interactive)
  (sg-shift-region 1))


;;
;; Commenting
;;

(defun sg-comment-region (direction)
  "Non-smart, basic commenting.
   All lines covering the selecting region are commented.
   If no region is selected, the current line is highlighted."
   (interactive)
   (let (pos1 pos2)
     (if (and transient-mark-mode
              mark-active)
         (progn
           (setq pos1 (region-beginning))
           (setq pos2 (region-end)))
       (progn
         (setq pos1 (line-beginning-position))
         (setq pos2 (line-end-position))))
     (save-excursion
       (if (> direction 0)
           (comment-region pos1 pos2)
         (uncomment-region pos1 pos2))
       (setq deactivate-mark nil))))


(defun sg-comment-region-incr ()
  "Increase the comment depth"
  (interactive)
  (sg-comment-region 1))


(defun sg-comment-region-decr ()
  "Decrease the comment depth"
  (interactive)
  (sg-comment-region -1))


;;
;; Buffer searching.
;;


(defvar sg-buffer-match-ignore-list
  (list
   "*Help*" "*Backtrace*" "*Completions*")
  "List of buffers to ignore in buffer pattern matching")


(defun sg-buffer-best-match (pattern)
  "If pattern matches a buffer in the buffer list, return that pattern.
   Otherwise return the first buffer matching '^.*pattern.*$'
   If nothing matches, return nil."
  (interactive)
  (let (bufName rePattern)
    ;; look for a direct match first
    (dolist (buf (buffer-list (selected-frame)) nil)
      (setq bufName (with-current-buffer buf (buffer-name)))
      (if (string= bufName pattern)
          (return bufName)))
    ;; no direct match, so look for a pattern match
    (setq rePattern (concat "^.*" pattern ".*$"))
    (dolist (buf (buffer-list (selected-frame)) nil)
      (setq bufName (with-current-buffer buf (buffer-name)))
      (if (string-match rePattern bufName)
           ;; matches the rePattern. Make sure it's not on the ignore list.
           (dolist (ignoreBuf sg-buffer-match-ignore-list nil)
             (progn
               (message "Testing buffer '%s' against '%s'" bufName ignoreBuf)
                (if (not (string= bufName ignoreBuf))
                    (progn
                      (message "Buffer '%s' is NOT ignore buf '%s'" bufName ignoreBuf)
                      (return bufName))
                  (message "Ignoring buffer: %s" bufName))
                (message "Testing if buffer '%s' matches '%s'" bufName ignoreBuf)))))))


(defun sg-list-buffers-matching (pattern &optional partial)
  "Return a list of buffers whose name matches the given pattern."
  (let (buf bufList bufName rePattern matches)
    (setq matches '())
    (if partial
        (setq rePattern (concat "^.*" pattern ".*$"))
      (setq rePattern pattern))
    (setq bufList (buffer-list))
    (while bufList
      (setq buf (car bufList))
      (if buf
          (progn
            (setq bufName (with-current-buffer buf (buffer-name)))
            (if (string-match rePattern bufName)
                (add-to-list 'matches bufName t))))
      (setq bufList (cdr bufList)))
    matches))


(defun sg-list-buffered-files-matching (pattern &optional partial)
  "Return a list of buffers whose file name matches the given pattern."
  (let (buf bufName bufFile bufList rePattern matches)
    (setq matches '())
    (if partial
        (setq rePattern (concat "^.*" pattern ".*$"))
      (setq rePattern pattern))
    (setq bufList (buffer-list))
    (while bufList
      (setq buf (car bufList))
      (if buf
          (progn
            (setq bufName (with-current-buffer buf (buffer-name)))
            (setq bufFile (with-current-buffer buf (buffer-file-name)))
            (message "checking %s %s" bufName bufFile)
            (if (and bufFile (string-match rePattern bufFile))
                (add-to-list 'matches bufName t))))
      (setq bufList (cdr bufList)))
    matches))


;;
;; Buffer/file switching
;;


(defun sg-find-file-other-window ()
  "Variation of find-file-other-window with improved shell-mode integratino.
   If we call this from within shell-mode, the directory being browsed
   in the shell becomes the initial directory for our file search."
  (interactive)
  (if (string= mode-name "Shell")
      (progn
        (dirs)
        (call-interactively 'find-file-other-window))
    (call-interactively 'find-file-other-window)))


(defun sg-switch-to-buffer (buf)
  "Switch to buffer by pattern. I.e,. 'mess'->'*Message*', and so on."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer: ")))
  ;; get the best buffer match for this input.
  (let (bufName matchingBuf)
    (if (bufferp buf)
        (setq bufName (with-current-buffer buf (buffer-name)))
      (setq bufName buf))
    (setq matchingBuf (sg-buffer-best-match bufName))
    (message "Matching buffer: %s" matchingBuf)
    (if matchingBuf
        (switch-to-buffer matchingBuf)
      (switch-to-buffer bufName))))


(defun sg-switch-to-buffer-other-frame (buf)
  "Switch to buffer in other frame by pattern. I.e,. 'mess'->'*Message*', and so on."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other frame: ")))
  ;; get the best buffer match for this input.
  (let (bufName matchingBuf)
    (if (bufferp buf)
        (setq bufName (with-current-buffer buf (buffer-name)))
      (setq bufName buf))
    (setq matchingBuf (sg-buffer-best-match bufName))
    (if matchingBuf
        (switch-to-buffer-other-frame matchingBuf)
      (switch-to-buffer-other-frame bufName))))


(defun sg-switch-to-buffer-other-window (buf)
  "Switch to buffer in other window by pattern. I.e,. 'mess'->'*Message*', and so on."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other window: ")))
  ;; get the best buffer match for this input.
  (let (bufName matchingBuf)
    (if (bufferp buf)
        (setq bufName (with-current-buffer buf (buffer-name)))
      (setq bufName buf))
    (setq matchingBuf (sg-buffer-best-match bufName))
    (if matchingBuf
        (switch-to-buffer-other-window matchingBuf)
      (switch-to-buffer-other-window bufName))))


(defun sg-window-switch-dir (x y)
  "Switch to the closest buffer in the given <x,y> direction"
  (interactive)
  (let (win pt coords wx wy trgX trgY left right top bottom winList)
    (setq winList (window-list))
    (setq pt (pos-visible-in-window-p (point) (selected-window) t))
    (setq wx (nth 0 pt) wy (nth 1 pt))
    (setq coords (window-pixel-edges))
    (setq left (nth 0 coords) top (nth 1 coords) right (nth 2 coords) bottom (nth 3 coords))
    (setq trgX (+ wx left) trgY (+ wy top))
    (if (/= x 0)
        (if (>= x 0)
            (setq trgX (+ right 25))
          (setq trgX (- left 25))))
    (if (/= y 0)
        (if (>= y 0)
            (setq trgY (+ bottom 25))
          (setq trgY (- top 25))))
    (if (> trgX (frame-pixel-width))
        (setq trgX 25)      
      (if (< trgX 0)
          (setq trgX (- (frame-pixel-width) 25))))
    (if (> trgY (frame-pixel-height))
        (setq trgY 25)     
      (if (< trgY 0)        
          (setq trgY (- (frame-pixel-height) 25))))
    (setq win (posn-window (posn-at-x-y trgX trgY (selected-frame))))
    (if (windowp win)
        (select-window win))))


(defun sg-window-switch-left ()
  (interactive)
  (sg-window-switch-dir -1 0))


(defun sg-window-switch-right ()
  (interactive)
  (sg-window-switch-dir 1 0))


(defun sg-window-switch-up ()
  (interactive)
  (sg-window-switch-dir 0 -1))


(defun sg-window-switch-down ()
  (interactive)
  (sg-window-switch-dir 0 1))


;;
;; Buffer killing
;;


(defun sg-kill-buffers-matching (pattern &optional partial)
  "Kill buffers whose name matches a given pattern."
  (interactive "Kill buffers matching:")
  (let (bufNames)
    (setq bufNames (sg-list-buffers-matching pattern partial))
    (while bufNames
      (call-interactively (lambda ()
                            (interactive)
                            (kill-buffer (car bufNames))))
      (setq bufNames (cdr bufNames)))))


(defun sg-kill-buffered-files-matching (pattern &optional partial)
  "Kill buffers whose file-name matches a given pattern."
  (interactive "Kill buffers matching:")
  (let (bufNames)
    (setq bufNames (sg-list-buffered-files-matching pattern partial))
    (while bufNames
      (call-interactively (lambda ()
                            (interactive)
                            (kill-buffer (car bufNames))))
      (setq bufNames (cdr bufNames)))))


(defun sg-kill-file-buffers ()
  "Kill all file buffers."
  (interactive)
  (let (buf bufList bufFile)
    (setq bufList (buffer-list))
    (while bufList
      (setq buf (car bufList))
      (if buf
          (progn
            (setq bufFile (with-current-buffer buf (buffer-file-name)))
            (if bufFile
                (call-interactively (lambda ()
                                      (interactive)
                                      (kill-buffer buf))))))
      (setq bufList (cdr bufList)))))


(defun sg-kill-non-file-buffers ()
  "Kill all non-file buffers."
  (interactive)
  (let (buf bufList bufFile)
    (setq bufList (buffer-list))
    (while bufList
      (setq buf (car bufList))
      (if buf
          (progn
            (setq bufFile (with-current-buffer buf (buffer-file-name)))
            (if (not bufFile)
                (call-interactively (lambda ()
                                      (interactive)
                                      (kill-buffer buf))))))
      (setq bufList (cdr bufList)))))


(defun sg-kill-file-buffers-by-ext (extension)
  "Kill all buffers matching a given extension."
  (interactive "sKill buffers with extension:")
  (if extension
      (sg-kill-buffered-files-matching (concat "^.*\\." extension "$") nil)))


;;
;; Misc UI interactions.
;;


(defun sg-keyboard-quit ()
  "Helper to break out of recursive edits.
   In addition to stopping all recursive edits, this will kill all
   *Backtrace* buffers, which have a tendancy to pop up when aborting
   recurive edits.

    TODO: If a buffer was visible in window before backtrace comes up,
          then the window should reverted to the previous buffer.
          Apply same behavior for *Help* windows.
          In fact, make it configurable for arbitrary buffer names via regex.

          If only a single window is open, always revert to the previous buffer,
          and push the offending window to the end of the buffer ring."
  (interactive)
  (let (win)
    (dolist (win (window-list (selected-frame)) nil)
      (if (string-equal
           (with-current-buffer (window-buffer win) (buffer-name))
           "*Backtrace*")
            (delete-window win)))
    (keyboard-quit)))
  

(defun sg-edit-default-keybindings ()
  "Setup default key bindings for sg-edit functions"
  (let ((bindings
         '(("C-g" sg-keyboard-quit)
           ("M-k" sg-copy-line-as-kill)
           ("<C-backspace>" sg-backward-kill-word)
           ("<C-tab>" sg-shift-region-right)
           ("<backtab>" sg-shift-region-left)
           ("C-," sg-shift-region-left)
           ("C-." sg-shift-region-right)
           ("C-;" sg-comment-region-decr)
           ("M-;" sg-comment-region-incr)
           ("C-x 4 f" sg-find-file-other-window)
           ("C-c k e" sg-kill-file-buffers-by-ext)
           ("C-c k f" sg-kill-file-buffers)
           ("C-c k b" sg-kill-non-file-buffers)
           ("C-c k m" sg-kill-buffers-matching)
           ("<M-left>" sg-window-switch-left)
           ("<M-right>" sg-window-switch-right)
           ("<M-up>" sg-window-switch-up)
           ("<M-down>" sg-window-switch-down))))
    (dolist (e bindings)
      (let ((key (car e)) (fn (car (cdr e))))
        (global-set-key (kbd key) fn)))))


(provide 'sg-edit)
