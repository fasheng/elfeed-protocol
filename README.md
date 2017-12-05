elfeed-protocol
==============
[![MELPA](http://melpa.org/packages/elfeed-protocol-badge.svg)](http://melpa.org/#/elfeed-protocol) [![MELPA Stable](https://stable.melpa.org/packages/elfeed-protocol-badge.svg)](https://stable.melpa.org/#/elfeed-protocol)

Provide extra protocols to make self-hosting RSS readers works
with [elfeed](https://github.com/skeeto/elfeed),
including
[Nextcloud/ownCloud News](https://nextcloud.com/),
[Tiny Tiny RSS](https://tt-rss.org/fox/tt-rss),
[NewsBlur(TODO)](https://newsblur.com/) and even more.

# Installation through MELPA

    ;; Install through package manager
    M-x package-install <ENTER>
    elfeed-protocol <ENTER>

# Initialization
Setup elfeed-protocol, then switch to search view and and press G to update entries:

```emacs-lisp
;; curl recommend
(setq elfeed-use-curl t)
(elfeed-set-timeout 36000)
(setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate
setup extra protocol feeds
(require 'elfeed-protocol)
(setq elfeed-feeds (list
                    "owncloud+https://user1:pass1@myhost.com"
                    (list "owncloud+https://user2@myhost.com"
                          :password "password/with|special@characters:"
                          :autotags '(("example.com" comic)))))
(elfeed-protocol-enable)
```

To make `elfeed-org` tag rules works together with `elfeed-protocol`, just add a
after advice for `elfeed`:

```emacs-lisp
(defadvice elfeed (after configure-elfeed-feeds activate)
  "Make elfeed-org autotags rules works with elfeed-protocol."
  (setq elfeed-protocol-tags elfeed-feeds)
  (setq elfeed-feeds (list
                      (list "owncloud+https://user:pass@myhost.com"
                          :autotags elfeed-protocol-tags))))
```

# Protocol Details
## owncloud (ownCloud News)
1. Fetch articles by the modified time
1. Support sync unread and starred tags, the starred tag name defined
   in `elfeed-protocol-owncloud-star-tag` which default value is `star`. For
   example, if user add `star` tag to one article, the star stat will
   be sync to server, too

Example:
```emacs-lisp
(setq elfeed-feeds (list
                    "owncloud+https://user1:pass1@myhost.com"
                    (list "owncloud+https://user2@myhost.com"
                          :password "password/with|special@characters:"
                          :autotags '(("example.com" comic)))))
```

## ttrss (Tiny Tiny RSS, requires version: 1.7.6)
1. Fetch articles by the entry ID
1. Fetch tags in remote
1. Support sync unread, starred and published tags, the starred tag
   name defined in `elfeed-protocol-ttrss-star-tag` which default
   value is `star`, and the published tag name defined in
   `elfeed-protocol-ttrss-publish-tag` which default value is
   `publish`

Example:
```emacs-lisp
(setq elfeed-feeds (list
                    "ttrss+https://user1:pass1@myhost.com"
                    (list "ttrss+https://user2@myhost.com"
                          :password "password/with|special@characters:"
                          :autotags '(("example.com" comic)))))
```

# Run Unit-Tests

Install `cask` package firstly, and then `make install; make test`

# Deploy Services for Testing
## Nextcloud/ownCloud News
1.  Fetch docker image and run it

        docker pull nextcloud
        docker run --rm -p 80:80 nextcloud

2.  Open <http://127.0.0.1> in browser to setup Nextcloud
    1.  Create admin user and select database to SQLite, then press "Finish setup"
    2.  Press left top popup menu and select "+Apps", select
        "Multimedia", and enable the "News" app
    3.  Press left top popup menu and switch to "News" app, then
        subscribe some feeds

3.  Setup `elfeed-protocol`

    ```emacs-lisp
    (setq elfeed-feeds '("owncloud+http://<admin>:<password>@localhost"))
    ```

## Tiny Tiny RSS
1.  Fetch related docker images and run them

        docker pull clue/ttrss
        docker pull nornagon/postgres
        docker run --rm -d --name ttrssdb nornagon/postgres
        docker run --rm --link ttrssdb:db -p 80:80 clue/ttrss

2.  Open <http://127.0.0.1> in browser to setup Tiny Tiny RSS
    1. Use the default `admin:password` authorization info to login
    2. Enter "Preferences" page to enable "Enable API access" and save configuration

3.  Setup `elfeed-protocol`

    ```emacs-lisp
    (setq elfeed-feeds '("ttrss+http://admin:password@localhost"))
    ```

# Problems
1. Sometimes emacs may be blocked if the parsing downloaded articles
   is too large, for example >50MB. This is caused by the known emacs
   bug that CPU will be in high usage if a text line is too
   long. There three methods to workaround this:
   1. Method 1, limit the download size, for example:

          (setq elfeed-protocol-owncloud-maxsize 1000)

   1. Method 2, for ownCloud, just update articles since special entry
      ID instead the modified time, this could run multiple times to
      keep up to date to avoid download too large entries once time

          M-x elfeed-protocol-owncloud-update-since-id

   1. Method 3, some protocol provide update method to reset the last
      modified time to skip some data, for example:

          M-x elfeed-protocol-owncloud-update-since-now

# License

Released under the terms of the GNU GPLv3+.
