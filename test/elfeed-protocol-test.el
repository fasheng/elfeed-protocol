(require 'cl-lib)
(require 'ert)
(require 'elfeed)

(ert-deftest elfeed-protocol-register ()
  (let* (elfeed-protocol-list)
    (elfeed-protocol-register "owncloud" (list :update 'elfeed-protocol-owncloud-update
                                               :pre-tag 'elfeed-protocol-owncloud-pre-tag
                                               :pre-untag 'elfeed-protocol-owncloud-pre-untag))
    (should (eq (elfeed-protocol-update-func "owncloud") 'elfeed-protocol-owncloud-update))
    (should (eq (elfeed-protocol-pre-tag-func "owncloud") 'elfeed-protocol-owncloud-pre-tag))
    (should (eq (elfeed-protocol-pre-untag-func "owncloud") 'elfeed-protocol-owncloud-pre-untag))
    (elfeed-protocol-unregister "owncloud")
    (should (eq (elfeed-protocol-update-func "owncloud") nil))
    ))

(ert-deftest elfeed-protocol-meta-data ()
  (with-elfeed-test
   (let* ((elfeed-feeds '("owncloud+https://user1:pass1@myhost.com:443"
                          ("owncloud+https://user2@myhost.com"
                           :password "pass2")))
          (proto-id1 "owncloud+https://user1@myhost.com:443")
          (proto-id2 "owncloud+https://user2@myhost.com"))
     (should (string=
              (elfeed-protocol-type "owncloud+https://user1:pass1@myhost.com:443")
              "owncloud"))
     (should (string=
              (elfeed-protocol-url "owncloud+https://user1:pass1@myhost.com:443")
              "https://user1:pass1@myhost.com:443"))
     (should (string=
              (elfeed-protocol-meta-url proto-id1)
              "owncloud+https://user1:pass1@myhost.com:443"))
     (should (string=
              (elfeed-protocol-meta-url proto-id2)
              "owncloud+https://user2@myhost.com"))
     (should (string=
              (elfeed-protocol-meta-user proto-id1)
              "user1"))
     (should (string=
              (elfeed-protocol-meta-user proto-id2)
              "user2"))
     (should (string=
              (elfeed-protocol-meta-password proto-id1)
              "pass1"))
     (should (string=
              (elfeed-protocol-meta-password proto-id2)
              "pass2"))
     )))
