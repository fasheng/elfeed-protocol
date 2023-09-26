(require 'cl-lib)
(require 'ert)
(require 'elfeed)

(defvar elfeed-protocol-newsblur-fixture-dir (concat (file-name-directory load-file-name) "fixtures/newsblur/"))

(defvar elfeed-protocol-newsblur-fixture-feeds
  (concat elfeed-protocol-newsblur-fixture-dir "feeds.json"))

(defvar elfeed-protocol-newsblur-fixture-entries
  (concat elfeed-protocol-newsblur-fixture-dir "entries.json"))

(ert-deftest elfeed-protocol-newsblur-parse-categories ()
  (with-elfeed-test
    (with-fixture elfeed-protocol-newsblur-fixture-feeds
      (let* ((proto-url "newsblur+https://user:pass@myhost.com")
             (host-url (elfeed-protocol-url proto-url))
             (proto-id (elfeed-protocol-newsblur-id host-url))
             (elfeed-protocol-newsblur-categories (elfeed-protocol-newsblur--parse-result
                                                   (elfeed-protocol-newsblur--parse-categories
                                                    host-url result))))
        (should (gethash proto-id elfeed-protocol-newsblur-categories))
        (should (string-equal
                 (elfeed-protocol-newsblur--get-category-name host-url 569)
                 "Writers"))
        (should (string-equal
                 (elfeed-protocol-newsblur--get-category-name host-url 1186180)
                 "Blogs"))
        (should (string-equal
                 (elfeed-protocol-newsblur--get-category-name host-url 50)
                 "Blogs/Photoblogs"))
        ;; for feed without category
        (should (not (elfeed-protocol-newsblur--get-category-name host-url 8032018)))
        ;; for invalid feed
        (should (not (elfeed-protocol-newsblur--get-category-name host-url 12345)))))))

(ert-deftest elfeed-protocol-newsblur-parse-feeds ()
  (with-elfeed-test
    (with-fixture elfeed-protocol-newsblur-fixture-feeds
      (let* ((proto-url "newsblur+https://user:pass@newsblur.com")
             (host-url (elfeed-protocol-url proto-url))
             (proto-id (elfeed-protocol-newsblur-id host-url))
             (elfeed-protocol-feeds (list proto-url))
             (elfeed-protocol-newsblur-feeds (elfeed-protocol-newsblur--parse-result
                                               (elfeed-protocol-newsblur--parse-feeds
                                                host-url result)))
             (test-orig-feed-url (elfeed-protocol-newsblur--get-subfeed-url host-url 569))
             (test-feed (elfeed-db-get-feed
                         (elfeed-protocol-format-subfeed-id proto-id test-orig-feed-url))))
        (should (string=
                 test-orig-feed-url
                 "http://anildash.com/"))
        (should (string=
                 (elfeed-feed-url test-feed)
                 (elfeed-protocol-format-subfeed-id proto-id "http://anildash.com/")))
        (should (string=
                 (elfeed-feed-title test-feed)
                 "Anil Dash"))))))

(ert-deftest elfeed-protocol-newsblur-parse-entries ()
  (with-elfeed-test
    (with-fixture elfeed-protocol-newsblur-fixture-feeds
      (let* ((proto-url "newsblur+https://user:pass@myhost.com")
             (host-url (elfeed-protocol-url proto-url))
             (proto-id (elfeed-protocol-newsblur-id host-url))
             (elfeed-protocol-newsblur-categories (elfeed-protocol-newsblur--parse-result
                                                   (elfeed-protocol-newsblur--parse-categories
                                                    host-url result))))
      (with-fixture elfeed-protocol-newsblur-fixture-feeds
        (let* ((proto-url "newsblur+https://user:pass@myhost.com")
               (host-url (elfeed-protocol-url proto-url))
               (proto-id (elfeed-protocol-newsblur-id host-url))
               (elfeed-protocol-feeds (list (list proto-url
                                         :autotags
                                         '(("http://anildash.com/" tag1)))))
               (elfeed-protocol-newsblur-feeds (elfeed-protocol-newsblur--parse-result
                                                 (elfeed-protocol-newsblur--parse-feeds
                                                  host-url result))))
          (with-fixture elfeed-protocol-newsblur-fixture-entries
            (let* ((entries (elfeed-protocol-newsblur--parse-result
                              (elfeed-protocol-newsblur--parse-entries
                               host-url result)))
                   (entry1 (elt entries 0))
                   (entry2 (elt entries 1)))
              (should (elfeed-protocol-newsblur-entry-p entry1))
              (should (elfeed-protocol-newsblur-entry-p entry2))
              (should (string=
                       (elfeed-entry-title entry1)
                       "Ask HN: What software/service helps you be an effective remote developer?"))
              (should (string=
                       (elfeed-entry-title entry2)
                       "Jessica Jones’ second season gets its first teaser"))
              (should (string=
                       (plist-get (nth 0 (elfeed-meta entry1 :authors)) :name)
                       "pearphp"))
              (should (string=
                       (plist-get (nth 0 (elfeed-meta entry2 :authors)) :name)
                       "Andrew Liptak"))
              (should (equal
                       (elfeed-entry-tags entry1)
                       '(Writers tag1)))
              (should (equal
                       (elfeed-entry-tags entry2)
                     `(hyperloop ,(intern "fundings & exits") Blogs star unread)))))))))))
