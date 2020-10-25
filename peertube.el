(require 'json)
(require 'cl-lib)


(defun peertube--format-account (account)
  "Format ACCOUNT name."
  (propertize (concat (format "%-11s" (seq-take account 11)) "   ")))
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
