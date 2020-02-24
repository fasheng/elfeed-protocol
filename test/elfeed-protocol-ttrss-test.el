(require 'cl-lib)
(require 'ert)
(require 'elfeed)

(defvar elfeed-protocol-ttrss-fixture-dir (concat (file-name-directory load-file-name) "fixtures/ttrss/"))

(defvar elfeed-protocol-ttrss-fixture-feeds
  (concat elfeed-protocol-ttrss-fixture-dir "feeds.json"))

(defvar elfeed-protocol-ttrss-fixture-entries
  (concat elfeed-protocol-ttrss-fixture-dir "entries.json"))

(defvar elfeed-protocol-ttrss-fixture-no-feed-id-entries
  (concat elfeed-protocol-ttrss-fixture-dir "entries-no-feed-id.json"))

(ert-deftest elfeed-protocol-ttrss-parse-feeds ()
  (with-fixture elfeed-protocol-ttrss-fixture-feeds
    (with-elfeed-test
      (let* ((proto-url "ttrss+https://user:pass@myhost.com:443")
             (host-url (elfeed-protocol-url proto-url))
             (proto-id (elfeed-protocol-ttrss-id host-url))
             (elfeed-feeds (list proto-url))
             (elfeed-protocol-ttrss-feeds (elfeed-protocol-ttrss--parse-result
                                            (elfeed-protocol-ttrss--parse-feeds
                                             host-url content)))
             (test-feed-url (elfeed-protocol-ttrss--get-subfeed-url host-url 1))
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
                 "Tiny Tiny RSS: Forum"))
        ))))

(ert-deftest elfeed-protocol-ttrss-parse-entries ()
  (with-fixture elfeed-protocol-ttrss-fixture-feeds
    (with-elfeed-test
      (let* ((proto-url "ttrss+https://user:pass@myhost.com")
             (host-url (elfeed-protocol-url proto-url))
             (proto-id (elfeed-protocol-ttrss-id host-url))
             (elfeed-protocol-ttrss-fetch-tags t)
             (elfeed-feeds (list (list proto-url
                                       :autotags
                                       '(("http://tt-rss.org/forum/rss.php" tag1)))))
             (elfeed-protocol-ttrss-feeds (elfeed-protocol-ttrss--parse-result
                                            (elfeed-protocol-ttrss--parse-feeds
                                             host-url content))))
        (with-fixture elfeed-protocol-ttrss-fixture-entries
          (let* ((entries (elfeed-protocol-ttrss--parse-result
                            (elfeed-protocol-ttrss--parse-entries
                             host-url content)))
                 (entry1 (elt entries 0))
                 (entry2 (elt entries 1)))
            (should (elfeed-protocol-ttrss-entry-p entry1))
            (should (elfeed-protocol-ttrss-entry-p entry2))
            (should (string=
                     (elfeed-entry-title entry1)
                     "Pictures not shown in some feeds with figure block"))
            (should (string=
                     (elfeed-entry-title entry2)
                     "PDO is coming, here's what you need to know"))
            (should (equal
                     (elfeed-entry-tags entry1)
                     '(tag1)))
            (should (equal
                     (elfeed-entry-tags entry2)
                     '(ttrss_tag2 ttrss_tag1 publish star tag1 unread)))
            (should (string=
                     (cdr (elfeed-entry-id entry1))
                     "SHA1:aeb92f1daca1aadd1e58ca9b8c820fab48703ef2"))
            (should (string=
                     (cdr (elfeed-entry-id entry2))
                     "urn:sha1:0b36197f75b63a54cf6152972f18f704479380d3"))
            ))))))

(ert-deftest elfeed-protocol-ttrss-parse-no-feed-id-entries ()
  (with-fixture elfeed-protocol-ttrss-fixture-feeds
    (with-elfeed-test
      (let* ((proto-url "ttrss+https://user:pass@myhost.com")
             (host-url (elfeed-protocol-url proto-url))
             (proto-id (elfeed-protocol-ttrss-id host-url))
             (elfeed-feeds (list (list proto-url
                                       :autotags
                                       '(("http://tt-rss.org/forum/rss.php" tag1)))))
             (elfeed-protocol-ttrss-feeds (elfeed-protocol-ttrss--parse-result
                                            (elfeed-protocol-ttrss--parse-feeds
                                             host-url content))))

        (with-fixture elfeed-protocol-ttrss-fixture-no-feed-id-entries

          (let* ((entries (elfeed-protocol-ttrss--parse-result
                            (elfeed-protocol-ttrss--parse-entries
                             host-url content)))
                 (entry1 (elt entries 0)))
            (should (equal (length entries) 1))
            (should (elfeed-protocol-ttrss-entry-p entry1))
            (should (string=
                     (elfeed-entry-title entry1)
                     "Pictures not shown in some feeds with figure block"))
            (should (equal
                     (elfeed-entry-tags entry1)
                     '(star tag1 unread)))
            ))))))
