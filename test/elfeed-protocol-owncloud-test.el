(require 'cl-lib)
(require 'ert)
(require 'elfeed)

(defvar elfeed-protocol-owncloud-fixture-dir (concat (file-name-directory load-file-name) "fixtures/owncloud/"))

(defvar elfeed-protocol-owncloud-fixture-categories
  (concat elfeed-protocol-owncloud-fixture-dir "categories.json"))

(defvar elfeed-protocol-owncloud-fixture-feeds
  (concat elfeed-protocol-owncloud-fixture-dir "feeds.json"))

(defvar elfeed-protocol-owncloud-fixture-entries
  (concat elfeed-protocol-owncloud-fixture-dir "entries.json"))

(ert-deftest elfeed-protocol-owncloud-parse-categories ()
  (with-elfeed-test
    (with-fixture elfeed-protocol-owncloud-fixture-categories
      (let* ((proto-url "owncloud+https://user:pass@myhost.com:443")
             (host-url (elfeed-protocol-url proto-url))
             (proto-id (elfeed-protocol-owncloud-id host-url))
             (elfeed-protocol-owncloud-categories (elfeed-protocol-owncloud--parse-result
                                                    (elfeed-protocol-owncloud--parse-categories
                                                     host-url result))))
        (should (gethash proto-id elfeed-protocol-owncloud-categories))
        (should (string-equal
                 (elfeed-protocol-owncloud--get-category-name host-url 1)
                 "Category 1"))
        ;; for invalid feed
        (should (not (elfeed-protocol-owncloud--get-category-name host-url 12345)))))))

(ert-deftest elfeed-protocol-owncloud-parse-feeds ()
  (with-elfeed-test
    (with-fixture elfeed-protocol-owncloud-fixture-feeds
      (let* ((url "https://user:pass@myhost.com:443")
             (proto-url (concat "owncloud+" url))
             (proto-id (elfeed-protocol-owncloud-id url))
             (elfeed-protocol-feeds (list proto-url))
             (elfeed-protocol-owncloud-feeds (elfeed-protocol-owncloud--parse-result
                                               (elfeed-protocol-owncloud--parse-feeds url result)))
             (feed1-url (elfeed-protocol-owncloud--get-subfeed-url url 1))
             (feed1 (elfeed-db-get-feed
                     (elfeed-protocol-format-subfeed-id proto-id feed1-url)))
             (feed2-url (elfeed-protocol-owncloud--get-subfeed-url url 2))
             (feed2 (elfeed-db-get-feed
                     (elfeed-protocol-format-subfeed-id proto-id feed2-url))))
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
                 "Feed 2"))))))

(ert-deftest elfeed-protocol-owncloud-parse-entries ()
  (with-elfeed-test
    (with-fixture elfeed-protocol-owncloud-fixture-categories
      (let* ((proto-url "owncloud+https://user:pass@myhost.com:443")
             (host-url (elfeed-protocol-url proto-url))
             (proto-id (elfeed-protocol-owncloud-id host-url))
             (elfeed-protocol-owncloud-categories (elfeed-protocol-owncloud--parse-result
                                                    (elfeed-protocol-owncloud--parse-categories
                                                     host-url result))))
      (with-fixture elfeed-protocol-owncloud-fixture-feeds
        (let* ((url "https://user:pass@myhost.com:443")
               (proto-url (concat "owncloud+" url))
               (proto-id (elfeed-protocol-owncloud-id url))
               (elfeed-protocol-feeds (list (list proto-url :autotags
                                         '(("http://www.example.com/feed/" tag1)
                                           ("http://www.example2.com/rss.jsp" tag2)))))
               (elfeed-protocol-owncloud-feeds (elfeed-protocol-owncloud--parse-result
                                                 (elfeed-protocol-owncloud--parse-feeds url result))))
          (with-fixture elfeed-protocol-owncloud-fixture-entries
            (let* ((entries (elfeed-protocol-owncloud--parse-result
                              (elfeed-protocol-owncloud--parse-entries url result)))
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
              (should (string=
                       (plist-get (nth 0 (elfeed-meta entry1 :authors)) :name)
                       "author1 <author1@www.example.com>"))
              (should (string=
                       (plist-get (nth 0 (elfeed-meta entry2 :authors)) :name)
                       "author2 <author2@www.example.com>"))
              (should (equal
                       (elfeed-entry-tags entry1)
                       '(Category\ 1 tag1)))
              (should (equal
                       (elfeed-entry-tags entry2)
                     '(star tag2 unread)))))))))))
