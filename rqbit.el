;; -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Marcus L. Hanestad <marlhan@proton.me>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(use-package request :ensure t)

(require 'request)

(setq rqbit--base-api-url "http://127.0.0.1:3030")
(setq rqbit--buffer-name "*rqbit*")
(setq rqbit--progress-bar-char ?#)
(setq rqbit-update-interval 1)

;; This is a hash table that contains stats and info about torrents.
;;
;; Keys:
;;  - stats : A hash table with the following keys:
;;    + download-speed : String
;;    + upload-speed : String
;;    + uptime : integer (seconds)
(setq rqbit--values (make-hash-table))

(defun rqbit--get-buffer ()
  (with-current-buffer (get-buffer-create rqbit--buffer-name)
    (rqbit-mode)
    (current-buffer)))

(defun rqbit--display-stats (stats)
  (insert
   (propertize "Download speed: " 'face 'font-lock-function-name-face)
   (gethash 'download-speed stats)
   (propertize " Upload speed: " 'face 'font-lock-function-name-face)
   (gethash 'upload-speed stats)
   (propertize " Uptime: " 'face 'font-lock-function-name-face)
   (seconds-to-string (gethash 'uptime stats))))

(defun rqbit--make-get-request (endpoint on-success on-error)
  (request
    (concat rqbit--base-api-url endpoint)
    :type
    "GET"
    :parser 'json-read
    :success on-success
    :error on-error))

(defun rqbit--make-post-request (endpoint on-success on-error)
  (request
    (concat rqbit--base-api-url endpoint)
    :type
    "POST"
    :parser 'json-read
    :success on-success
    :error on-error))

(defun rqbit--make-progress-bar (prefix progress)
  (let*
      ((width
        (- (window-max-chars-per-line (get-buffer-window rqbit--buffer-name))
           ;; 2 because "[]"
           (length prefix) 2))
         (filled (round (* progress width)))
         (empty (- width filled))
         (bar
          (concat (propertize "[" 'face 'font-lock-function-name-face)
                  (make-string filled rqbit--progress-bar-char)
                  (make-string empty ?\s)
                  (propertize "]" 'face 'font-lock-function-name-face))))
    (concat prefix bar)))

(defun rqbit--resume-download (id)
  (rqbit--make-post-request
    (format "/torrents/%s/start" id)
    (cl-function (lambda (&key data &allow-other-keys)))
    (cl-function (lambda (&key data &allow-other-keys)))))

(defun rqbit--pause-download (id)
  (rqbit--make-post-request
    (format "/torrents/%s/pause" id)
    (cl-function (lambda (&key data &allow-other-keys)))
    (cl-function (lambda (&key data &allow-other-keys)))))

(defun rqbit--remove-download (id)
  (let ((delete (y-or-n-p "Delete files? "))
        (torrents-table (gethash 'torrents rqbit--values (make-hash-table))))
    (remhash id torrents-table)
    (puthash 'torrents torrents-table rqbit--values)
    (if delete
        (rqbit--make-post-request (format "/torrents/%s/delete" id)
                                  (cl-function
                                   (lambda (&key data &allow-other-keys)))
                                  (cl-function
                                   (lambda (&key data &allow-other-keys))))
      (rqbit--make-post-request (format "/torrents/%s/forget" id)
                                (cl-function
                                 (lambda (&key data &allow-other-keys)))
                                (cl-function
                                 (lambda (&key data &allow-other-keys)))))))

(defun rqbit--display-torrents (torrents)
  (maphash (lambda (id values)
             (let* ((state (nth 0 values))
                   (name (nth 1 values))
                   (info-hash (nth 2 values))
                   (progress-bytes (nth 3 values))
                   (total-bytes (nth 4 values))
                   (download-speed (nth 5 values))
                   (upload-speed (nth 6 values))
                   (time-remaining (nth 7 values))
                   (progress-percent (/ (float progress-bytes) total-bytes))
                   (label
                    (format "%3d%% %s%s%s" (round (* progress-percent 100))
                            (file-size-human-readable progress-bytes)
                            (propertize "/" 'face 'font-lock-function-name-face)
                            (file-size-human-readable total-bytes))))
               (if (string= state "paused")
                   (insert-text-button "[Resume]"
                                       'action
                                       (lambda (_) (rqbit--resume-download id))
                                       'help-echo "Resume download")
                   (insert-text-button "[Pause]"
                                       'action
                                       (lambda (_) (rqbit--pause-download id))
                                       'help-echo "Pause download"))
               (insert " ")
               (insert-text-button "[Remove]" 'action
                                   (lambda (_) (rqbit--remove-download id))
                                   'help-echo "Remove a download")
             (insert " " (propertize name 'face 'font-lock-type-face))
             (newline)
             (if (string= state "paused")
                 (setq label
                       (concat label
                               (propertize " idle" 'face 'font-lock-comment-face)))
               (progn
                (when download-speed
                  (setq label
                        (concat label
                                (propertize " Down: " 'face
                                            'font-lock-function-name-face)
                                download-speed)))
                (when upload-speed
                  (setq label
                        (concat label
                                (propertize " Up: " 'face
                                            'font-lock-function-name-face)
                                upload-speed)))
                (when time-remaining
                  (setq label
                        (concat label
                                (propertize " ETA: " 'face
                                            'font-lock-function-name-face)
                                time-remaining)))))
             (insert
              (rqbit--make-progress-bar (concat label " ") progress-percent))
             (newline 2)))
           torrents))

(defun rqbit--display ()
  (with-current-buffer (rqbit--get-buffer)
    (read-only-mode -1)
    (let ((marker (point))
          (stats (gethash 'stats rqbit--values))
          (torrents (gethash 'torrents rqbit--values)))
      (erase-buffer)
      (if stats
          (rqbit--display-stats stats)
        (insert "Stats: n/a"))
      (newline 2)
      (if torrents
          (rqbit--display-torrents torrents)
        (progn
          (insert "Torrents: n/a")
          (newline)))
      (goto-char marker)
      (when global-hl-line-mode
        (global-hl-line-highlight))
      (when hl-line-mode
        (hl-line-highlight)))
    (read-only-mode 1)))

;; (rqbit--display)

(defun rqbit--get-stats ()
  (rqbit--make-get-request
   "/stats"
    ;; {
    ;;    "download_speed" : {
    ;;       "human_readable" : string,
    ;;       "mbps" : int
    ;;    },
    ;;    "fetched_bytes" : int,
    ;;    "peers" : {
    ;;       "connecting" : int,
    ;;       "dead" : int,
    ;;       "live" : int,
    ;;       "not_needed" : int,
    ;;       "queued" : int,
    ;;       "seen" : int,
    ;;       "steals" : int
    ;;    },
    ;;    "upload_speed" : {
    ;;       "human_readable" : string,
    ;;       "mbps" : int
    ;;    },
    ;;    "uploaded_bytes" : int,
    ;;    "uptime_seconds" : int
    ;; }
   (cl-function (lambda (&key data &allow-other-keys)
                  (let* ((download-speed (cdr (assoc 'download_speed data)))
                         (download-speed-human
                          (cdr (assoc 'human_readable download-speed)))
                         (upload-speed (cdr (assoc 'upload_speed data)))
                         (upload-speed-human
                          (cdr (assoc 'human_readable upload-speed)))
                         (uptime (cdr (assoc 'uptime_seconds data)))
                         (stats-table (make-hash-table)))
                    (puthash 'download-speed download-speed-human stats-table)
                    (puthash 'upload-speed upload-speed-human stats-table)
                    (puthash 'uptime uptime stats-table)
                    ;; Update global values table
                    (puthash 'stats stats-table rqbit--values))
                  (rqbit--display)))
   (cl-function (lambda (&key error-thrown &allow-other-keys)
                  (message "Error: %S" error-thrown)))))

(defun rqbit--get-torrent-stats (id name info-hash)
  (rqbit--make-get-request
   (concat "/torrents/" (format "%s" id) "/stats/v1")
    ;; {
    ;; "error" : <idk?>,
    ;; "file_progress" : [
    ;;     int,
    ;;     int,
    ;;     int,
    ;;     int
    ;; ],
    ;; "finished" : bool,
    ;; "live" : <idk?>,
    ;; "progress_bytes" : int,
    ;; "state" : string,
    ;; "total_bytes" : int,
    ;; "uploaded_bytes" : int
    ;; }
   (cl-function (lambda (&key data &allow-other-keys)
                  (let* ((state (cdr (assoc 'state data)))
                        (progress-bytes (cdr (assoc 'progress_bytes data)))
                        (total-bytes (cdr (assoc 'total_bytes data)))
                        (live (cdr (assoc 'live data)))
                        (download-speed
                          (cdr (assoc 'download_speed live)))
                        (upload-speed (cdr (assoc 'upload_speed live)))
                        (download-speed-human
                          (cdr (assoc 'human_readable download-speed)))
                        (upload-speed-human
                          (cdr (assoc 'human_readable upload-speed)))
                        (time-remaining
                          (cdr (assoc 'time_remaining live)))
                        (time-remaining-human
                          (cdr (assoc 'human-readable time-remaining)))
                        ;; (torrent-table (make-hash-table)))
                         (torrent-table
                          (gethash 'torrents rqbit--values (make-hash-table))))
                    ;; A torrent is a list: (
                    ;;   state: string
                    ;;   name: string,
                    ;;   info-hash: string,
                    ;;   progress-bytes: int,
                    ;;   total-bytes: int,
                    ;;   ; If state is "live" the following elements are present
                    ;;   download-speed: string,
                    ;;   upload-speed: string,
                    ;;   time-remaining: optional string,
                    ;; )
                    (puthash id
                             (list state name info-hash progress-bytes
                                   total-bytes download-speed-human
                                   upload-speed-human time-remaining-human)
                             torrent-table)
                    (puthash 'torrents torrent-table rqbit--values))
                  (rqbit--display)))
   (cl-function (lambda (&key error-thrown &allow-other-keys)
                  (message ("Error: %S" error-thrown))))))

;; (rqbit--get-torrent-stats 0 "test" "test")

(defun rqbit--get-torrents ()
  (rqbit--make-get-request
   "/torrents"
    ;; {
    ;;    "torrents" : [
    ;;       {
    ;;          "id" : int,
    ;;          "info_hash" : string,
    ;;          "name" : string,
    ;;          "output_folder" : string,
    ;;       }
    ;;    ]
    ;; }
   (cl-function (lambda (&key data &allow-other-keys)
                (let* ((torrents (cdr (assoc 'torrents data))))
                  (dotimes (i (length torrents))
                    (let* ((torrent (aref torrents i))
                           (id (cdr (assoc 'id torrent)))
                           (name (cdr (assoc 'name torrent)))
                           (info-hash (cdr (assoc 'info_hash torrent))))
                      (rqbit--get-torrent-stats id name info-hash))))))
   (cl-function (lambda (&key error-thrown &allow-other-keys)
                  (message "Error: %S" error-thrown)))))

;; (rqbit--get-torrents)

(defun rqbit--update ()
  (rqbit--get-stats)
  (rqbit--get-torrents))

(defun rqbit-add(magnet-link)
  (interactive "sMagnet: ")
  (request
    (concat rqbit--base-api-url "/torrents")
  :type
  "POST"
  :data magnet-link))

(defvar rqbit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") 'rqbit-add)
    map)
  "Keymap for `rqbit-mode'")

(define-derived-mode rqbit-mode prog-mode
  "rqbit"
  "Major mode for rqbit bittorrent client"
  (use-local-map rqbit-mode-map))

(defun rqbit ()
  (interactive)
  (let ((buffer (get-buffer rqbit--buffer-name)))
    (if buffer
        ;; If the buffer is open, we are receiving updates (no need to setup the timer again),
        ;; so we just switch to it
        (progn
          (switch-to-buffer (rqbit--get-buffer))
          (rqbit-mode))
      (progn
        (switch-to-buffer (rqbit--get-buffer))
        (rqbit-mode)
        ;; Setup a timer to run the update function every second
        (setq update-timer
              (run-at-time nil rqbit-update-interval #'rqbit--update))
        ;; Stop the timer when the buffer is killed to ensure that it's not opened again when it shouldn't
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (cancel-timer update-timer))
                  nil t)
        (rqbit--display)))))

(provide 'rqbit)
