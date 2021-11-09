elfeed-protocol
==============
[![MELPA](http://melpa.org/packages/elfeed-protocol-badge.svg)](http://melpa.org/#/elfeed-protocol) [![MELPA Stable](https://stable.melpa.org/packages/elfeed-protocol-badge.svg)](https://stable.melpa.org/#/elfeed-protocol)

Provide extra protocols to make self-hosting RSS readers work
with [elfeed](https://github.com/skeeto/elfeed),
including
[Fever](https://feedafever.com/api),
[NewsBlur](https://newsblur.com/),
[Nextcloud/ownCloud News](https://nextcloud.com/),
[Tiny Tiny RSS](https://tt-rss.org/fox/tt-rss) and even more.

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

;; setup extra protocol feeds
(setq elfeed-feeds '(
                     ;; format 1
                     "owncloud+https://user:pass@myhost.com"

                     ;; format 2, for password with special characters
                     ("owncloud+https://user@myhost.com"
                      :password "password/with|special@characters:")

                     ;; format 3, for password in file
                     ("owncloud+https://user@myhost.com"
                      :password-file "~/.password")

                     ;; format 4, for password in .authinfo, ensure (auth-source-search :host "myhost.com" :port "443" :user "user4") exists
                     ("owncloud+https://user@myhost.com"
                      :use-authinfo t)

                     ;; format 5, for password in gnome-keyring
                     ("owncloud+https://user@myhost.com"
                      :password (shell-command-to-string "echo -n `secret-tool lookup attribute value`"))

                     ;; format 6, for password in pass(1), using password-store.el
                     ("owncloud+https://user@myhost.com"
                      :password (password-store-get "owncloud/app-pass"))

                     ;; use autotags
                     ("owncloud+https://user@myhost.com"
                      :password "password"
                      :autotags (("example.com" comic)))))

;; enable elfeed-protocol
(elfeed-protocol-enable)
```

To make `elfeed-org` tag rules works together with `elfeed-protocol`, just add a
after advice for `elfeed`:

```emacs-lisp
(defadvice elfeed (after configure-elfeed-feeds activate)
  "Make elfeed-org autotags rules works with elfeed-protocol."
  (setq elfeed-protocol-tags elfeed-feeds)
  (setq elfeed-feeds (list
                      (list "owncloud+https://user@myhost.com"
                            :password '(password-store-get "owncloud/app-pass")
                            :autotags elfeed-protocol-tags))))
```

# Protocol Details

## fever (Fever)
1. Fetch articles by the entry ID
1. Support sync unread, starred(saved) tags, the starred tag name
   defined in `elfeed-protocol-fever-star-tag` which default value is
   `star`
1. Support multiple fetching methods:
   - `elfeed-protocol-fever-update-older`
   - `elfeed-protocol-fever-update-star`

**NOTE**: User must provide Fever API URL manually. For Tiny Tiny RSS
Fever plugin, it is `https://your-ttrss-server/plugins/fever/`.

Example:
```emacs-lisp
(setq elfeed-feeds (list
                    (list "fever+https://user@myhost.com"
                          :api-url "https://myhost.com/plugins/fever/"
                          :password "password/with|special@characters:"
                          :autotags '(("example.com" comic)))))
```

## newsblur (NewsBlur)
1. Fetch articles from recent pages
1. Fetch articles for special feed
1. Fetch tags in remote
1. Support sync unread, starred(saved) tags, the starred tag name
   defined in `elfeed-protocol-ttrss-star-tag` which default value is
   `star`

**NOTE**: A file for storing session cookies has to be specified via
`elfeed-curl-extra-arguments` like in the following example.

Example:
```emacs-lisp
(setq elfeed-protocol-newsblur-maxpages 20)
(setq elfeed-curl-extra-arguments '("--cookie-jar" "/tmp/newsblur-cookie"
                                    "--cookie" "/tmp/newsblur-cookie"))
(setq elfeed-feeds (list
                    "newsblur+https://user1:pass1@newsblur.com"
                    (list "newsblur+https://user2@newsblur.com"
                          :password "password/with|special@characters:"
                          :autotags '(("example.com" comic)))))
```

## owncloud (ownCloud News)
1. Fetch articles with the modified time by default
1. Fetch articles for special feed
1. Support sync unread and starred tags, the starred tag name defined
   in `elfeed-protocol-owncloud-star-tag` which default value is `star`. For
   example, if user add `star` tag to one article, the star stat will
   be sync to server, too
1. Support multiple fetching methods:
   - `elfeed-protocol-owncloud-update-since-timestamp`
   - `elfeed-protocol-owncloud-update-since-id`
   - `elfeed-protocol-owncloud-update-older`

Example:
```emacs-lisp
(setq elfeed-protocol-owncloud-maxsize 1000)
(setq elfeed-protocol-owncloud-update-with-modified-time t)
(setq elfeed-feeds (list
                    "owncloud+https://user1:pass1@myhost.com"
                    (list "owncloud+https://user2@myhost.com"
                          :password "password/with|special@characters:"
                          :autotags '(("example.com" comic)))))
```

## ttrss (Tiny Tiny RSS, requires version: 1.7.6)
1. Fetch articles by the entry ID
1. Fetch articles for special feed
1. Fetch category as tag
1. Support sync unread, starred and published tags, the starred tag
   name defined in `elfeed-protocol-ttrss-star-tag` which default
   value is `star`, and the published tag name defined in
   `elfeed-protocol-ttrss-publish-tag` which default value is
   `publish`
1. Support multiple fetching methods:
   - `elfeed-protocol-ttrss-update-older`
   - `elfeed-protocol-ttrss-update-star`

**NOTE**: For Tiny Tiny RSS only allow fetch Maximize 200 entries each
time, so if your own much more starred entries, just run
`elfeed-protocol-ttrss-update-star` manually to fetch them all

Example:
```emacs-lisp
(setq elfeed-protocol-ttrss-maxsize 200) ; bigger than 200 is invalid
(setq elfeed-feeds (list
                    "ttrss+https://user1:pass1@myhost.com"
                    (list "ttrss+https://user2@myhost.com"
                          :password "password/with|special@characters:"
                          :autotags '(("example.com" comic)))))
```

# Run Unit-Tests

Install `cask` system package firstly, and then run following commands

```shell
make init
make test
make checkdoc
make elint
make package-lint
```

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

# Report Issues

Please collect logs in buffer `*elfeed-log*` with the following config
before reporting issues:

```emacs-lisp
(setq elfeed-log-level 'debug)
(toggle-debug-on-error)

;; for more logs
(setq elfeed-protocol-log-trace t)
(setq elfeed-protocol-fever-maxsize 10)
(setq elfeed-protocol-newsblur-maxpages 1)
(setq elfeed-protocol-owncloud-maxsize 10)
(setq elfeed-protocol-ttrss-maxsize 10)
```

# Q&A

1. When I run elfeed-update I get the error: `elfeed-feeds malformed, bad entry`

   Don't forget to enable elfeed-protocol at first:
    ```emacs-lisp
   (elfeed-protocol-enable)
    ```

1. Not working if my password contains special characters like `@#$/:`.

   Use format 2 instead in previous example for complex password:
    ```emacs-lisp
    ;; format 2, for password with special characters
    ("owncloud+https://user@myhost.com"
     :password "password/with|special@characters:")
    ```

1. How to fetch my older headlines in server?

   `fever`, `owncloud` and `ttrss` protocol provide method to fetch
   older headlines. And the update operations could not executed in
   the same time, so `run-at-time` with some delays(for example 15s)
   will help you:
    ```emacs-lisp
    (setq my-elfeed-update-timer
          (run-at-time 15 15
                       (lambda () (when (= elfeed-curl-queue-active 0)
                                    (elfeed-protocol-ttrss-update-older "ttrss+https://user@host")))))
    (cancel-timer my-elfeed-update-timer)
    ```

1. Sometimes emacs may be blocked if the parsing downloaded articles
   is too large, for example >50MB.

   This is caused by the known emacs bug that CPU will be in high
   usage if a text line is too long. There three methods to workaround
   this:
   1. Method 1, limit the download size, for example:

          (setq elfeed-protocol-owncloud-maxsize 1000)

   1. Method 2, for ownCloud, just update articles since special entry
      ID instead the modified time, this could run multiple times to
      keep up to date to avoid download too large entries once time

          M-x elfeed-protocol-owncloud-update-since-id

   1. Method 3, some protocol provide update method to reset the last
      modified time to skip some data, for example:

          M-x elfeed-protocol-owncloud-update-since-timestamp

# Donation

- BTC [3DMLQ8f4Ui5Adka9Y44MVM2cKT6yBVZdY5](https://www.blockchain.com/btc/address/3DMLQ8f4Ui5Adka9Y44MVM2cKT6yBVZdY5)
- ETH [0x561c694EF2bf32C23759c4Abd7D132161DaE13F8](https://www.blockchain.com/eth/address/0x561c694EF2bf32C23759c4Abd7D132161DaE13F8)

# License

Released under the terms of the GNU GPLv3+.
