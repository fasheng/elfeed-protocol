(require 'cl-lib)
(require 'ert)
(require 'elfeed)

(defvar elfeed-protocol-newsblur-test-feeds-json "{
  \"folders\": [
    {
      \"Writers\": [
        76,
        38,
        569,
        183139,
        5708255,
        21309
      ]
    },
    {
      \"Blogs\": [
        6227976,
        6227690,
        3581,
        5994357,
        1186180,
        5636682,
        526,
        5261603,
        {
          \"Photoblogs\": [
            50,
            551953,
            34,
            776101,
            3556,
            1480028,
            1095,
            588075,
            5984253,
            5752038,
            1627
          ]
        },
        6605581
      ]
    }
  ],
  \"saved_searches\": [],
  \"user_id\": 39040,
  \"social_profile\": {
    \"website\": \"\",
    \"following_user_ids\": [
      32048,
      13,
      33405,
      26578,
      219352
    ],
    \"following_count\": 5,
    \"shared_stories_count\": 0,
    \"private\": true,
    \"large_photo_url\": \"https://www.gravatar.com/avatar/e3c7a29b7538122f0ee20f8e9cf05f42\",
    \"id\": \"social:39040\",
    \"feed_address\": \"http://www.newsblur.com/social/rss/39040/homepage\",
    \"user_id\": 39040,
    \"feed_link\": \"http://homepage.newsblur.com/\",
    \"follower_user_ids\": [
      33143,
      33727,
      39285,
      35059
    ],
    \"location\": \"\",
    \"popular_publishers\": [],
    \"follower_count\": 4,
    \"username\": \"homepage\",
    \"bio\": \"\",
    \"average_stories_per_month\": 0,
    \"feed_title\": \"homepage's blurblog\",
    \"photo_service\": \"gravatar\",
    \"stories_last_month\": 0,
    \"photo_url\": \"https://www.gravatar.com/avatar/e3c7a29b7538122f0ee20f8e9cf05f42\",
    \"num_subscribers\": 4,
    \"protected\": true
  },
  \"starred_counts\": [
    {
      \"count\": 0,
      \"feed_address\": \"http://www.newsblur.com/reader/starred_rss/39040/75ddb05c6a00/\",
      \"tag\": \"\",
      \"feed_id\": null
    }
  ],
  \"starred_count\": 0,
  \"is_staff\": false,
  \"result\": \"ok\",
  \"authenticated\": true,
  \"feeds\": {
    \"1186180\": {
      \"subs\": 14627,
      \"favicon_url\": \"https://s3.amazonaws.com/icons.newsblur.com/1186180.png\",
      \"is_push\": false,
      \"feed_opens\": 183034,
      \"id\": 1186180,
      \"s3_icon\": true,
      \"feed_link\": \"http://bestof.metafilter.com/\",
      \"updated_seconds_ago\": 1055,
      \"favicon_fetching\": false,
      \"ng\": 0,
      \"favicon_border\": \"004c72\",
      \"last_story_date\": \"2017-12-09 17:01:18\",
      \"nt\": 1,
      \"not_yet_fetched\": false,
      \"updated\": \"17 minutes\",
      \"average_stories_per_month\": 10,
      \"ps\": 0,
      \"feed_address\": \"http://feeds.feedburner.com/BestOfMetafilter\",
      \"feed_title\": \"Best of MetaFilter\",
      \"favicon_fade\": \"2489bb\",
      \"is_newsletter\": false,
      \"last_story_seconds_ago\": 51343,
      \"favicon_color\": \"016698\",
      \"stories_last_month\": 19,
      \"active\": true,
      \"fetched_once\": true,
      \"favicon_text_color\": \"white\",
      \"subscribed\": true,
      \"num_subscribers\": 14627,
      \"s3_page\": false,
      \"min_to_decay\": 22,
      \"search_indexed\": true
    },
    \"569\": {
      \"subs\": 16058,
      \"favicon_url\": \"https://s3.amazonaws.com/icons.newsblur.com/569.png\",
      \"is_push\": false,
      \"feed_opens\": 40418,
      \"id\": 569,
      \"s3_icon\": true,
      \"feed_link\": \"http://anildash.com/\",
      \"updated_seconds_ago\": 965,
      \"favicon_fetching\": false,
      \"ng\": 0,
      \"favicon_border\": \"818180\",
      \"last_story_date\": \"2017-09-11 13:07:05\",
      \"nt\": 0,
      \"not_yet_fetched\": false,
      \"updated\": \"16 minutes\",
      \"average_stories_per_month\": 3,
      \"ps\": 0,
      \"feed_address\": \"http://feeds.dashes.com/AnilDash\",
      \"feed_title\": \"Anil Dash\",
      \"favicon_fade\": \"cfcfce\",
      \"is_newsletter\": false,
      \"last_story_seconds_ago\": 7754996,
      \"favicon_color\": \"acacab\",
      \"stories_last_month\": 0,
      \"active\": true,
      \"fetched_once\": true,
      \"favicon_text_color\": \"white\",
      \"subscribed\": true,
      \"num_subscribers\": 16058,
      \"s3_page\": false,
      \"min_to_decay\": 720,
      \"search_indexed\": true
    }
  },
  \"social_services\": {
    \"facebook\": {
      \"syncing\": false,
      \"facebook_picture_url\": null,
      \"facebook_uid\": null
    },
    \"twitter\": {
      \"twitter_username\": null,
      \"syncing\": false,
      \"twitter_picture_url\": null,
      \"twitter_uid\": null
    },
    \"gravatar\": {
      \"gravatar_picture_url\": \"https://www.gravatar.com/avatar/e3c7a29b7538122f0ee20f8e9cf05f42\"
    },
    \"appdotnet\": {
      \"syncing\": false,
      \"appdotnet_uid\": null,
      \"appdotnet_picture_url\": null
    },
    \"upload\": {
      \"upload_picture_url\": null
    }
  },
  \"categories\": null,
  \"social_feeds\": [
    {
      \"username\": \"popular\",
      \"ps\": 0,
      \"user_id\": 32048,
      \"subscription_user_id\": 32048,
      \"feed_link\": \"http://popular.newsblur.com/\",
      \"feed_address\": \"http://www.newsblur.com/social/rss/32048/popular\",
      \"feed_opens\": 59743,
      \"num_subscribers\": 42590,
      \"shared_stories_count\": 2163,
      \"private\": false,
      \"feed_title\": \"The People Have Spoken\",
      \"protected\": false,
      \"location\": \"\",
      \"photo_url\": \"https://s3.amazonaws.com/avatars.newsblur.com/avatars/32048/thumbnail_profile_1358188630.jpg\",
      \"large_photo_url\": \"https://s3.amazonaws.com/avatars.newsblur.com/avatars/32048/large_profile_1358188630.jpg\",
      \"is_trained\": false,
      \"ng\": 0,
      \"nt\": 0,
      \"id\": \"social:32048\",
      \"page_url\": \"/social/page/32048/popular\"
    }
  ]
}")

(defvar elfeed-protocol-newsblur-test-entries-json "{
  \"code\": 1,
  \"authenticated\": true,
  \"hidden_stories_removed\": 0,
  \"elapsed_time\": 0.18,
  \"user_search\": null,
  \"stories\": [
    {
      \"friend_shares\": [],
      \"story_authors\": \"pearphp\",
      \"intelligence\": {
        \"feed\": 0,
        \"tags\": 0,
        \"author\": 0,
        \"title\": 1
      },
      \"story_permalink\": \"https://news.ycombinator.com/item?id=15889939\",
      \"reply_count\": 0,
      \"comment_user_ids\": [],
      \"story_timestamp\": \"1512880499\",
      \"share_user_ids\": [],
      \"story_hash\": \"6188470:5a2628\",
      \"id\": \"https://news.ycombinator.com/item?id=15889939\",
      \"comment_count\": null,
      \"score\": 1,
      \"story_tags\": [],
      \"share_count\": null,
      \"friend_comments\": [],
      \"story_date\": \"2017-12-10 04:34:59\",
      \"short_parsed_date\": \"Yesterday, 11:34pm\",
      \"guid_hash\": \"5a2628\",
      \"image_urls\": [],
      \"story_feed_id\": 569,
      \"long_parsed_date\": \"Yesterday, December 9th 11:34pm\",
      \"public_comments\": [],
      \"read_status\": 1,
      \"has_modifications\": false,
      \"story_title\": \"Ask HN: What software/service helps you be an effective remote developer?\",
      \"story_content\": \"<p>Article URL: <a href=\\\"https://news.ycombinator.com/item?id=15889939\\\">https://news.ycombinator.com/item?id=15889939</a></p><p>Comments URL: <a href=\\\"https://news.ycombinator.com/item?id=15889939\\\">https://news.ycombinator.com/item?id=15889939</a></p><p>Points: 54</p><p># Comments: 29</p>\"
    },
    {
      \"friend_shares\": [],
      \"story_authors\": \"Andrew Liptak\",
      \"intelligence\": {
        \"feed\": 0,
        \"tags\": 0,
        \"author\": 0,
        \"title\": 0
      },
      \"story_permalink\": \"https://www.theverge.com/2017/12/9/16756896/jessica-jones-season-2-teaser-trailer-watch\",
      \"reply_count\": 0,
      \"comment_user_ids\": [],
      \"story_timestamp\": \"1512858665\",
      \"share_user_ids\": [],
      \"story_hash\": \"576138:52eed5\",
      \"id\": \"https://www.theverge.com/2017/12/9/16756896/jessica-jones-season-2-teaser-trailer-watch\",
      \"comment_count\": null,
      \"score\": 0,
      \"story_tags\": [
        \"fundings & exits\",
        \"hyperloop\"
      ],
      \"share_count\": null,
      \"friend_comments\": [],
      \"story_date\": \"2017-12-09 22:31:05\",
      \"short_parsed_date\": \"Yesterday, 5:31pm\",
      \"guid_hash\": \"52eed5\",
      \"image_urls\": [
        \"https://cdn.vox-cdn.com/thumbor/KReBajkoWVvP5vYpwxqXWi8cD0w=/270x0:1920x864/fit-in/1200x630/cdn.vox-cdn.com/uploads/chorus_asset/file/9841323/Screen_Shot_2017_12_09_at_5.18.58_PM.png\"
      ],
      \"story_feed_id\": 1186180,
      \"long_parsed_date\": \"Yesterday, December 9th 5:31pm\",
      \"public_comments\": [],
      \"read_status\": 0,
      \"has_modifications\": false,
      \"story_title\": \"Jessica Jones’ second season gets its first teaser\",
      \"story_content\": \"<img alt=\\\"\\\" src=\\\"https://cdn.vox-cdn.com/thumbor/PqIG8gJs1QVUZtPx8_b0qvCS9dE=/484x0:1780x864/1310x873/cdn.vox-cdn.com/uploads/chorus_image/image/57920759/Screen_Shot_2017_12_09_at_5.18.58_PM.0.png\\\" />\\n\\n\\n\\n  <p id=\\\"iJKRgd\\\">When we last saw Jessica Jones in Netflix’s <a href=\\\"https://www.theverge.com/2017/8/18/16166630/the-defenders-trailers-updates-commentary-netflix-marvel-team-up\\\"><em>The Defenders</em></a>, she escaped from the destruction of Midland Circle along with Luke Cage and Danny Rand, who all look to move on with their lives after stopping The Hand. Netflix debuted the first trailer for the next season of <em>Marvel’s Jessica Jones</em>, showing that while Jones is trying to move on, her past is catching up to her. </p>\\n<p id=\\\"nKeEGa\\\">In the teaser, we see that Jones is back to working as a private investigator, musing that people have their secrets, or at least someone else’s. It seems that there’s still some surprises in her past: her friend Trish telling her that knowing what was done to her might be helpful. Jones isn’t sold on the idea, asking “what if facing it makes me worse?” Along the way,...</p>\\n  <p>\\n    <a href=\\\"https://www.theverge.com/2017/12/9/16756896/jessica-jones-season-2-teaser-trailer-watch\\\">Continue reading&hellip;</a>\\n  </p>\",
      \"starred\": true
    }
  ],
  \"result\": \"ok\",
  \"message\": null,
  \"classifiers\": {
    \"576138\": {
      \"authors\": {},
      \"feeds\": {},
      \"titles\": {
        \"iPhone\": 1
      },
      \"tags\": {}
    },
    \"6188470\": {
      \"authors\": {},
      \"feeds\": {},
      \"titles\": {
        \"NYTimes\": 1,
        \"Python\": 1,
        \"Deep Learning\": 1,
        \"JavaScript\": 1,
        \"Facebook\": -1,
        \"Ask HN\": 1
      },
      \"tags\": {}
    }
  },
  \"user_profiles\": []
}")


(ert-deftest elfeed-protocol-newsblur-parse-feeds ()
  (with-temp-buffer
    (insert elfeed-protocol-newsblur-test-feeds-json)
    (goto-char (point-min))
    (with-elfeed-test
     (let* ((proto-url "newsblur+https://user:pass@newsblur.com")
            (host-url (elfeed-protocol-url proto-url))
            (proto-id (elfeed-protocol-newsblur-id host-url))
            (elfeed-feeds (list proto-url))
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
                "Anil Dash"))
       ))))

(ert-deftest elfeed-protocol-newsblur-parse-entries ()
  (with-temp-buffer
    (insert elfeed-protocol-newsblur-test-feeds-json)
    (goto-char (point-min))
    (with-elfeed-test
     (let* ((proto-url "newsblur+https://user:pass@myhost.com")
            (host-url (elfeed-protocol-url proto-url))
            (proto-id (elfeed-protocol-newsblur-id host-url))
            (elfeed-feeds (list (list proto-url
                                      :autotags
                                      '(("http://anildash.com/" tag1)))))
            (elfeed-protocol-newsblur-feeds (elfeed-protocol-newsblur--parse-result
                                           (elfeed-protocol-newsblur--parse-feeds
                                            host-url result))))
       (with-temp-buffer
         (insert elfeed-protocol-newsblur-test-entries-json)
         (goto-char (point-min))
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
           (should (equal
                    (elfeed-entry-tags entry1)
                    '(tag1)))
           (should (equal
                    (elfeed-entry-tags entry2)
                    `(hyperloop ,(intern "fundings & exits") star unread)))
           ))))))
