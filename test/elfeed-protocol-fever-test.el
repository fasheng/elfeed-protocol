(require 'cl-lib)
(require 'ert)
(require 'elfeed)

(defvar elfeed-protocol-fever-fixture-dir (concat (file-name-directory load-file-name) "fixtures/fever/"))

(defvar elfeed-protocol-fever-fixture-feeds
  (concat elfeed-protocol-fever-fixture-dir "feeds.json"))

(defvar elfeed-protocol-fever-fixture-entries
  (concat elfeed-protocol-fever-fixture-dir "entries.json"))

(ert-deftest elfeed-protocol-fever-parse-feeds ()
  (with-fixture elfeed-protocol-fever-fixture-feeds
    (with-elfeed-test
      (let* ((proto-url "fever+https://user:pass@myhost.com")
             (host-url (elfeed-protocol-url proto-url))
             (proto-id (elfeed-protocol-fever-id host-url))
             (elfeed-feeds (list proto-url))
             (elfeed-protocol-fever-feeds (elfeed-protocol-fever--parse-result
                                           (elfeed-protocol-fever--parse-feeds
                                            host-url (map-elt result 'feeds))))
             (test-feed-url (elfeed-protocol-fever--get-subfeed-url host-url 1))
             (test-feed (elfeed-db-get-feed
                         (elfeed-protocol-format-subfeed-id proto-id test-feed-url))))
        (should (string=
                 test-feed-url
                 "http://tt-rss.org/forum/rss.php"))
        (should (string=
                 (elfeed-feed-url test-feed)
                 (elfeed-protocol-format-subfeed-id proto-id "http://tt-rss.org/forum/rss.php")))
        (should (string=
                 (elfeed-feed-title test-feed)
                 "Tiny Tiny RSS: Forum"))))))

(ert-deftest elfeed-protocol-fever-parse-entries ()
  (with-fixture elfeed-protocol-fever-fixture-feeds
    (with-elfeed-test
      (let* ((proto-url "fever+https://user:pass@myhost.com")
             (host-url (elfeed-protocol-url proto-url))
             (proto-id (elfeed-protocol-fever-id host-url))
             (elfeed-feeds (list (list proto-url
                                       :autotags
                                       '(("http://tt-rss.org/forum/rss.php" ttrss)
                                         ("https://github.com/FreshRSS/FreshRSS/releases.atom" freshrss)))))
             (elfeed-protocol-fever-feeds (elfeed-protocol-fever--parse-result
                                           (elfeed-protocol-fever--parse-feeds
                                            host-url (map-elt result 'feeds)))))
        (with-fixture elfeed-protocol-fever-fixture-entries
          (let* ((entries (elfeed-protocol-fever--parse-result
                            (elfeed-protocol-fever--parse-entries
                             host-url (map-elt result 'items))))
                 (entry1 (elt entries 0))
                 (entry2 (elt entries 1))
                 (entry3 (elt entries 2))
                 (entry4 (elt entries 3)))
            (should (elfeed-protocol-fever-entry-p entry1))
            (should (elfeed-protocol-fever-entry-p entry2))
            (should (elfeed-protocol-fever-entry-p entry3))
            (should (elfeed-protocol-fever-entry-p entry4))
            (should (string=
                     (elfeed-entry-title entry1)
                     "Pictures not shown in some feeds with figure block"))
            (should (string=
                     (elfeed-entry-title entry2)
                     "PDO is coming, here's what you need to know"))
            (should (string=
                     (elfeed-entry-title entry3)
                     "FreshRSS 1.13.1"))
            (should (string=
                     (elfeed-entry-title entry4)
                     "Cloned FreshRSS 1.13.1"))
            (should (equal
                     (elfeed-entry-tags entry1)
                     '(ttrss)))
            (should (equal
                     (elfeed-entry-tags entry2)
                     '(star ttrss unread)))
            (should (equal
                     (elfeed-entry-tags entry3)
                     '(freshrss)))
            (should (equal
                     (elfeed-entry-tags entry4)
                     '(freshrss)))))))))
