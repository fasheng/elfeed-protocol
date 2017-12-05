(require 'cl-lib)
(require 'ert)
(require 'elfeed)

(defvar elfeed-protocol-ttrss-test-feeds-json
  "{
   \"seq\":0,
   \"status\":0,
   \"content\":[
     {
       \"feed_url\":\"http://tt-rss.org/forum/rss.php\",
       \"title\":\"Tiny Tiny RSS: Forum\",
       \"id\":1,
       \"unread\":46,
       \"has_icon\":true,
       \"cat_id\":0,
       \"last_updated\":1494163883,
       \"order_id\":0
     }
   ]
}")

(defvar elfeed-protocol-ttrss-test-entries-json
  "{
  \"seq\": 0,
  \"status\": 0,
  \"content\": [
    {
      \"id\": 1,
      \"unread\": false,
      \"marked\": false,
      \"published\": false,
      \"updated\": 1494158580,
      \"is_updated\": false,
      \"title\": \"Pictures not shown in some feeds with figure block\",
      \"link\": \"http://discourse.tt-rss.org/t/pictures-not-shown-in-some-feeds-with-figure-block/79/14\",
      \"feed_id\": \"1\",
      \"tags\": [
        \"\"
      ],
      \"attachments\": [],
      \"content\": \"content1\",
      \"labels\": [],
      \"feed_title\": \"Tiny Tiny RSS: Forum\",
      \"comments_count\": 0,
      \"comments_link\": \"\",
      \"always_display_attachments\": false,
      \"author\": \"@fox\",
      \"score\": 0,
      \"note\": null,
      \"lang\": \"\"
    },
    {
      \"id\": 2,
      \"unread\": true,
      \"marked\": true,
      \"published\": true,
      \"updated\": 1512246430,
      \"is_updated\": false,
      \"title\": \"PDO is coming, here's what you need to know\",
      \"link\": \"http://discourse.tt-rss.org/t/pdo-is-coming-heres-what-you-need-to-know/689/6\",
      \"feed_id\": \"1\",
      \"tags\": [
        \"ttrss_tag1\",
        \"ttrss_tag2\"
      ],
      \"content\": \"content2\",
      \"labels\": [],
      \"feed\_title\": \"Tiny Tiny RSS: Forum\",
      \"comments\_count\": 0,
      \"comments\_link\": \"\",
      \"always\_display_attachments\": false,
      \"author\": \"@fox\",
      \"score\": 0,
      \"note\": null,
      \"lang\": \"\"
    }
  ]
}")


(ert-deftest elfeed-protocol-ttrss-parse-feeds ()
  (with-temp-buffer
    (insert elfeed-protocol-ttrss-test-feeds-json)
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
    (insert elfeed-protocol-ttrss-test-feeds-json)
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
         (insert elfeed-protocol-ttrss-test-entries-json)
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
