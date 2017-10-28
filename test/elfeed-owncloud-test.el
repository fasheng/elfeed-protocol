(require 'cl-lib)
(require 'ert)
(require 'elfeed)

(defvar elfeed-owncloud-test-feeds-json
  "{
  \"starredCount\": 1,
  \"feeds\": [
    {
      \"id\": 1,
      \"url\": \"http://www.example.com/feed/\",
      \"title\": \"Feed 1\",
      \"faviconLink\": \"http://www.example.com/favicon.ico\",
      \"added\": 1492783825,
      \"folderId\": 11,
      \"unreadCount\": 2,
      \"ordering\": 0,
      \"link\": \"http://www.example.com/\",
      \"pinned\": false,
      \"updateErrorCount\": 0,
      \"lastUpdateError\": null
    },
    {
      \"id\": 2,
      \"url\": \"http://www.example2.com/rss.jsp\",
      \"title\": \"Feed 2\",
      \"faviconLink\": \"\",
      \"added\": 1492784045,
      \"folderId\": 12,
      \"unreadCount\": 0,
      \"ordering\": 0,
      \"link\": \"http://www.example2.com/\",
      \"pinned\": false,
      \"updateErrorCount\": 0,
      \"lastUpdateError\": \"\"
    }
  ],
  \"newestItemId\": 2
}")

(defvar elfeed-owncloud-test-entries-json
  "{
  \"items\": [
    {
      \"id\": 1,
      \"guid\": \"646062e8942103b24ae2470b552577dd\",
      \"guidHash\": \"646062e8942103b24ae2470b552577dd\",
      \"url\": \"http://www.example.com/test.html?from=rss\",
      \"title\": \"Entry 1\",
      \"author\": \"www.example.com\",
      \"pubDate\": 1493260874,
      \"body\": \"<p>body 1...</p>\",
      \"enclosureMime\": null,
      \"enclosureLink\": null,
      \"feedId\": 1,
      \"unread\": true,
      \"starred\": false,
      \"lastModified\": 1493260874,
      \"rtl\": false,
      \"fingerprint\": \"d20ec9d7c728c6486f418e3d725b03ad\",
      \"contentHash\": \"3e0d73ae064930f5f0c0eebb3258c100\"
    },
    {
      \"id\": 2,
      \"guid\": \"06b3c14d9433ea9bbdea131bd37168d8\",
      \"guidHash\": \"06b3c14d9433ea9bbdea131bd37168d8\",
      \"url\": \"http://www.example.com/test2.html?from=rss\",
      \"title\": \"Entry 2\",
      \"author\": \"www.example.com\",
      \"pubDate\": 1493260874,
      \"body\": \"<p>body 2...</p>\",
      \"enclosureMime\": \"video/webm\",
      \"enclosureLink\": \"http://www.example/test.webm\",
      \"feedId\": 1,
      \"unread\": true,
      \"starred\": true,
      \"lastModified\": 1493260874,
      \"rtl\": false,
      \"fingerprint\": \"6fb19b56a1c30f8e952af57b809d3dec\",
      \"contentHash\": \"61206dd01bba2282c261215bfc24c6e5\"
    }
  ]
}")

(ert-deftest elfeed-owncloud-feed-list ()
  (with-temp-buffer
    (insert elfeed-owncloud-test-feeds-json)
    (goto-char (point-min))
    (with-elfeed-test
     (let* ((elfeed-owncloud-feeds (elfeed-owncloud--parse-feeds))
            (feed1-url (elfeed-owncloud--get-feed-url 1))
            (feed1 (elfeed-db-get-feed feed1-url))
            (feed2-url (elfeed-owncloud--get-feed-url 2))
            (feed2 (elfeed-db-get-feed feed2-url)))
       (should (string=
                feed1-url
                "http://www.example.com/feed/"))
       (should (string=
                feed2-url
                "http://www.example2.com/rss.jsp"))
       (should (string=
                (elfeed-feed-title feed1)
                "Feed 1"))
       (should (string=
                (elfeed-feed-title feed2)
                "Feed 2"))))))

(ert-deftest elfeed-owncloud-entry-list ()
  (with-temp-buffer
    (insert elfeed-owncloud-test-feeds-json)
    (goto-char (point-min))
    (with-elfeed-test
     (let* ((elfeed-owncloud-feeds (elfeed-owncloud--parse-feeds)))
       (with-temp-buffer
         (insert elfeed-owncloud-test-entries-json)
         (goto-char (point-min))
         (let* ((entries (elfeed-owncloud--parse-entries))
                (entry1 (elt entries 0))
                (entry2 (elt entries 1)))
           (should (elfeed-owncloud-is-owncloud-entry entry1))
           (should (elfeed-owncloud-is-owncloud-entry entry2))
           (should (string=
                    (elfeed-entry-title entry1)
                    "Entry 1"))
           (should (string=
                    (elfeed-entry-title entry2)
                   "Entry 2"))))))))
