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

;; (setq lexical-binding t)

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
  (let ((buffer (get-buffer rqbit--buffer-name)))
    (if buffer
        buffer
      (generate-new-buffer rqbit--buffer-name))))

(defun rqbit--display-stats (stats)
  (insert
   "Stats: Download speed: "
   (gethash 'download-speed stats)
   " Upload speed: "
   (gethash 'upload-speed stats)
   " Uptime: "
   (seconds-to-string (gethash 'uptime stats))))

(defun rqbit--make-progress-bar (prefix progress)
  (let*
      ((width
        (- (window-max-chars-per-line (get-buffer-window rqbit--buffer-name))
           ;; 2 because "[]"
           (length prefix) 2))
         (filled (round (* progress width)))
         (empty (- width filled))
         (bar
          (concat "[" (make-string filled rqbit--progress-bar-char)
                  (make-string empty ?\s) "]")))
    (concat prefix bar)))

;; (rqbit--make-progress-bar " 50% " 0.5)

(defun rqbit--display-torrents (torrents)
  (maphash (lambda (id values)
             (let* ((name (nth 0 values))
                   (info-hash (nth 1 values))
                   (progress-bytes (nth 2 values))
                   (total-bytes (nth 3 values))
                   (progress-percent (/ (float progress-bytes) total-bytes)))
             (insert name)
             (newline)
             (insert
              (rqbit--make-progress-bar
               (format "%3d%% " (round (* progress-percent 100)))
               progress-percent))
             (newline)))
           torrents))

(defun rqbit--display ()
  (with-current-buffer (rqbit--get-buffer)
    (read-only-mode -1)
    (erase-buffer)
    ;; (message "%S" rqbit--values)
    (let ((stats (gethash 'stats rqbit--values))
          (torrents (gethash 'torrents rqbit--values)))
      (if stats
          (rqbit--display-stats stats)
        (insert "Stats: n/a"))
      (newline 2)
      (if torrents
          (rqbit--display-torrents torrents)
        (progn
          (insert "Torrents: n/a")
          (newline))))
    (read-only-mode 1)))

;; (rqbit--display)

(defun rqbit--make-get-request (endpoint on-success on-error)
  (request
    (concat rqbit--base-api-url endpoint)
    :type
    "GET"
    :parser 'json-read
    :success on-success
    :error on-error))

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
                  (let ((state (cdr (assoc 'state data)))
                        (progress-bytes (cdr (assoc 'progress_bytes data)))
                        (total-bytes (cdr (assoc 'total_bytes data)))
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
                    (if (string= state "live")
                        (let* ((live (cdr (assoc 'live data)))
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
                                (cdr (assoc 'human-readable time-remaining))))
                          (puthash id
                                   (list state name info-hash progress-bytes
                                         total-bytes download-speed-human
                                         upload-speed-human time-remaining-human)
                                   torrent-table))
                      (puthash id
                               (list state name info-hash progress-bytes
                                     total-bytes)
                               torrent-table))
                    ;; (message "State: %S" state)
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

(defun rqbit ()
  (interactive)
  (let ((buffer (get-buffer rqbit--buffer-name)))
    (if buffer
        ;; If the buffer is open, we are receiving updates (no need to setup the timer again),
        ;; so we just switch to it
        (switch-to-buffer (rqbit--get-buffer))
      (progn
        (switch-to-buffer (rqbit--get-buffer))
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
