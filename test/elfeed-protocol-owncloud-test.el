(require 'cl-lib)
(require 'ert)
(require 'elfeed)

(defvar elfeed-protocol-owncloud-fixtures-dir (concat (file-name-directory load-file-name) "fixtures/owncloud/"))

(defvar elfeed-protocol-owncloud-fixture-feeds
  (concat elfeed-protocol-owncloud-fixtures-dir "feeds.json"))

(defvar elfeed-protocol-owncloud-fixture-entries
  (concat elfeed-protocol-owncloud-fixtures-dir "entries.json"))

(ert-deftest elfeed-protocol-owncloud-parse-feeds ()
  (with-fixture elfeed-protocol-owncloud-fixture-feeds
    (with-elfeed-test
      (let* ((url "https://user:pass@myhost.com:443")
             (proto-url (concat "owncloud+" url))
             (proto-id (elfeed-protocol-owncloud-id url))
             (elfeed-feeds (list proto-url))
             (elfeed-protocol-owncloud-feeds (elfeed-protocol-owncloud--parse-feeds url))
             (feed1-url (elfeed-protocol-owncloud--get-subfeed-url url 1))
             (feed1 (elfeed-db-get-feed
                     (elfeed-protocol-format-subfeed-id proto-id feed1-url)))
             (feed2-url (elfeed-protocol-owncloud--get-subfeed-url url 2))
             (feed2 (elfeed-db-get-feed
                     (elfeed-protocol-format-subfeed-id proto-id feed2-url)))
             )
        (should (string=
                 feed1-url
                 "http://www.example.com/feed/"))
        (should (string=
                 feed2-url
                 "http://www.example2.com/rss.jsp"))
        (should (string=
                 (elfeed-feed-url feed1)
                 (elfeed-protocol-format-subfeed-id proto-id "http://www.example.com/feed/")))
        (should (string=
                 (elfeed-feed-url feed2)
                 (elfeed-protocol-format-subfeed-id proto-id "http://www.example2.com/rss.jsp")))
        (should (string=
                 (elfeed-feed-title feed1)
                 "Feed 1"))
        (should (string=
                 (elfeed-feed-title feed2)
                 "Feed 2"))
        ))
    ))

(ert-deftest elfeed-protocol-owncloud-parse-entries ()
  (with-fixture elfeed-protocol-owncloud-fixture-feeds
    (with-elfeed-test
      (let* ((url "https://user:pass@myhost.com:443")
             (proto-url (concat "owncloud+" url))
             (proto-id (elfeed-protocol-owncloud-id url))
             (elfeed-feeds (list (list proto-url :autotags
                                       '(("http://www.example.com/feed/" tag1)
                                         ("http://www.example2.com/rss.jsp" tag2)))))
             (elfeed-protocol-owncloud-feeds (elfeed-protocol-owncloud--parse-feeds url)))
        (with-fixture elfeed-protocol-owncloud-fixture-entries
          (let* ((entries (elfeed-protocol-owncloud--parse-entries url))
                 (entry1 (elt entries 0))
                 (entry2 (elt entries 1)))
            (should (elfeed-protocol-owncloud-entry-p entry1))
            (should (elfeed-protocol-owncloud-entry-p entry2))
            (should (string=
                     (elfeed-entry-title entry1)
                     "Entry 1"))
            (should (string=
                     (elfeed-entry-title entry2)
                     "Entry 2"))
            (should (equal
                     (elfeed-entry-tags entry1)
                     '(tag1)))
            (should (equal
                     (elfeed-entry-tags entry2)
                     '(star tag2 unread)))
            )
          )))))
