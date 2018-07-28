(require 'cl-lib)
(require 'ert)
(require 'elfeed)

(defvar elfeed-protocol-ttrss-fixture-dir "./fixtures/ttrss/")

(defvar elfeed-protocol-ttrss-fixture-feeds
  (concat elfeed-protocol-ttrss-fixture-dir "feeds.json"))

(defvar elfeed-protocol-ttrss-fixture-entries
  (concat elfeed-protocol-ttrss-fixture-dir "entries.json"))

(defvar elfeed-protocol-ttrss-fixture-no-feed-id-entries
  (concat elfeed-protocol-ttrss-fixture-dir "entries-no-feed-id.json"))

(ert-deftest elfeed-protocol-ttrss-parse-feeds ()
  (with-temp-buffer
    (insert-file-contents elfeed-protocol-ttrss-fixture-feeds)
    (goto-char (point-min))
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
  (with-temp-buffer
    (insert-file-contents elfeed-protocol-ttrss-fixture-feeds)
    (goto-char (point-min))
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
       (with-temp-buffer
         (insert-file-contents elfeed-protocol-ttrss-fixture-entries)
         (goto-char (point-min))
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
           ))))))

(ert-deftest elfeed-protocol-ttrss-parse-no-feed-id-entries ()
  (with-temp-buffer
    (insert-file-contents elfeed-protocol-ttrss-fixture-feeds)
    (goto-char (point-min))
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
       (with-temp-buffer
         (insert-file-contents elfeed-protocol-ttrss-fixture-no-feed-id-entries)
         (goto-char (point-min))
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
