;;; peertube.el --- Query PeerTube videos in Emacs -*- lexical-binding: t; -*-

;; This file is NOT part of Emacs.

;; Copyright (C) 2020  Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'json)
(require 'cl-lib)
(require 'transmission)

(defgroup peertube nil
  "Query PeerTube videos in Emacs."
  :group 'convenience)

(defcustom peertube-account-length 15
  "Length of the creator of the video."
  :type 'integer)

(defcustom peertube-title-length 50
  "Length of the title of the video."
  :type 'integer)

(defvar peertube-videos '()
  "List of videos displayed in the *peertube* buffer.")

(define-derived-mode peertube-mode tabulated-list-mode "peertube"
  "Major mode for peertube.")

(defvar peertube-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" 'peertube-open-video)
    (define-key map "s" 'peertube-search)
    (define-key map "d" 'peertube-open-video)
    (define-key map "g" 'peertube-draw-buffer)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map)
  "Keymap for `peertube-mode'.")

(defun peertube-quit ()
  "Close peertube buffer."
  (interactive)
  (quit-window))
(defun peertube--format-account (account)
  "Format the ACCOUNT name in the *peertube* buffer."
  (propertize account))

(defun peertube--format-title (title)
  "Format the video TITLE int the *peertube* buffer."
  (propertize title))

(defun peertube--format-duration (duration)
  "Format the DURATION from seconds to hh:mm:ss in the *peertube* buffer."
  (let ((formatted-string (concat (format-seconds "%.2h" duration)
				  ":"
				  (format-seconds "%.2m" (mod duration 3600))
				  ":"
				  (format-seconds "%.2s" (mod duration 60))
				  "  ")))
    (propertize formatted-string)))
						  
(defun peertube--format-views (views)
  "Format the VIEWS in the *peertube* buffer.

Format to thousands (K) or millions (M) if necessary."
  (let ((formatted-string (cond ((< 1000000 views) (format "%5sM" (/ (round views 100000) (float 10))))
				((< 1000 views) (format "%5sK" (/ (round views 100) (float 10))))
				(t (format "%6s" views)))))
    (propertize formatted-string)))

(defun peertube--format-tags (tags)
  "Format the TAGS in the *peertube* buffer."
  (let ((formatted-string (if (eq (length tags) 0)
			      (format "")
			    (format "%s" tags))))
    (propertize formatted-string)))

(defun peertube--format-date (date)
  "Format the DATE in the *peertube* buffer."
  (propertize (seq-take date 10)))
			  
(defun peertube--insert-entry (video)
  "Insert VIDEO into the current buffer."
  (list (peertube-video-url video)
	(vector (peertube--format-duration (peertube-video-duration video))
		(peertube--format-title (peertube-video-title video))
		(peertube--format-account (peertube-video-account video))
		(peertube--format-date (peertube-video-date video))
		(peertube--format-views (peertube-video-views video))
		(peertube--format-tags (peertube-video-tags video)))))
  
(defun peertube--draw-buffer ()
  "Draw buffer with video entries."
  (interactive)
  (erase-buffer)
  (setq tabulated-list-format `[("Duration" 10 t)
				("Title" 50 t)
				("Account" ,peertube-account-length t)
				("Date" 10 t)
				("Views" 6 t)
				("Tags" 10 nil)])
  (setq tabulated-list-entries (mapcar 'peertube--insert-entry peertube-videos))
  (tabulated-list-init-header)
  (tabulated-list-print))
  
(defun peertube--get-current-video ()
  "Get the currently selected video."
  (aref peertube-videos (1- (line-number-at-pos))))

;; (defun peertube-download ()
;;   (interactive)
;;   (let ((url (peertube-video-url (peertube--get-current-video))))
;;     ;; https://peertube.dsmouse.net/videos/watch/670b41d7-71bc-4619-ad9e-947136fa6916

(defun peertube-open ()
  (interactive)
  (let ((url (peertube-video-url (peertube--get-current-video))))
    (browse-url url)))
  
;; (defun peertube-buffer ()
;;   "Draw the '*peertube*' buffer and switch to it."
;;   (interactive)
;;   (switch-to-buffer "*peertube*")
;;   (erase-buffer))
  
(defun peertube-search (query)
  "Search PeerTube for QUERY."
  (interactive "sSearch PeerTube: ")
  ;; (peertube-buffer)
  (setq peertube-videos (peertube-query query))
  (peertube--draw-buffer))


  ;; (seq-do (lambda (v)
  ;; 	    (peertube--insert-video v))
  ;; 	  peertube-videos))

  ;; (let ((videos (peertube-query query)))
  ;;   (seq-do (lambda (v)
  ;; 	      (peertube--insert-video v))
  ;; 	    peertube-videos)))

;; Store metadata for PeerTube videos
(cl-defstruct (peertube-video (:constructor peertube--create-video)
			  (:copier nil))
  "Metadata for a PeerTube video."
  (title "" :read-only t)
  (account "" :read-only t)
  (channel "" :read-only t)
  (date "" :read-only t)
  (category "" :read-only t)
  (language "" :read-only t)
  (duration 0 :read-only t)
  (tags [] :read-only t)
  (url "" :read-only t)
  (views 0 :read-only t)
  (likes 0 :read-only t)
  (dislikes 0 :read-only t)
  (nsfw nil :read-only t))

(defun peertube--call-api (query)
  "Call the PeerTube search API with QUERY as the search term.

Curl is used to call 'search.joinpeertube.org', the result gets parsed by `json-read'."
  (with-temp-buffer
    (call-process "curl" nil t nil "--silent" "-X" "GET" (concat "https://sepiasearch.org/api/v1/search/videos?search=" query "&"))
      (goto-char (point-min))
      ;; ((total . [0-9]{4}) (data . [(... ... ...) (... ... ...) ...]))
      ;;                             └───────────────────────────────┘
      ;;                                   extract useful data
      (cdr (car (cdr (json-read))))))

(defun peertube-query (query)
  (interactive)
  (let ((videos (peertube--call-api query)))
    (dotimes (i (length videos))
      (let ((v (aref videos i)))
	(aset videos i
	      (peertube--create-video :title (assoc-default 'name v)
				      :account (assoc-default 'name (assoc-default 'account v))
				      :channel (assoc-default 'name (assoc-default 'channel v))
				      :date (assoc-default 'publishedAt v)
				      :category (assoc-default 'label (assoc-default 'category v))
				      :language (assoc-default 'label (assoc-default 'language v))
				      :duration (assoc-default 'duration v)
				      :tags (assoc-default 'tags v)
				      :url (assoc-default 'url v)
				      :views (assoc-default 'views v)
				      :likes (assoc-default 'likes v)
				      :dislikes (assoc-default 'dislikes v)
				      :nsfw (assoc-default 'nsfw v)))))
    videos))

(provide 'peertube)

;;;###autoload
(defun peertube ()
  "Enter peertube."
  (interactive)
  (switch-to-buffer "*peertube*")
  (unless (eq major-mode 'peertube-mode)
    (peertube-mode))
  (call-interactively 'peertube-search))
  ;; (when (seq-empty-p ytel-search-term)
  ;;   (call-interactively #'ytel-search)))

;;; peertube.el ends here
