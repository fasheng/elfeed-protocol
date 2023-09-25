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

**NOTE:** Since version `0.9.0` , elfeed-protocol use variable
`elfeed-protocol-feeds` instead of `elfeed-feeds` to fix conflict
issues to extensions that modify or require `elfeed-feeds`.

# Installation through MELPA

```
;; Install through package manager
M-x package-install <ENTER>
elfeed-protocol <ENTER>
```

# Initialization
Setup elfeed-protocol, then switch to search view and and press G to update entries:

```emacs-lisp
;; curl recommend
(setq elfeed-use-curl t)
(elfeed-set-timeout 36000)
(setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate

;; setup feeds
(setq elfeed-protocol-feeds '(("owncloud+https://user@myhost.com"
                               :password "my-password")))

;; enable elfeed-protocol
(setq elfeed-protocol-enabled-protocols '(fever newsblur owncloud ttrss))
(elfeed-protocol-enable)
```

# Protocol Details

## fever (Fever)
1. Fetch articles with the entry ID one by one by default. For some
   service that don't provide valid entry ID like FressRSS, just set
   `elfeed-protocol-fever-update-unread-only` to t as a workaround
1. Fetch remote category as tag
1. Support sync unread, starred(saved) tags, the starred tag name
   defined in `elfeed-protocol-fever-star-tag` which default value is
   `star`
1. Support multiple fetching methods:
   - `elfeed-protocol-fever-update-older`
   - `elfeed-protocol-fever-update-star`

**NOTE**: Many self-hosted RSS server provide Fever API, and user must
provide the URL manually. Here is a list of Fever API URLs I collected
for some popular RSS servers:

- Tiny Tiny RSS's Fever plugin
  ```
  https://your-ttrss-server/plugins/fever/
  ```
- FreshRSS
  ```
  https://your-freshrss-server/api/fever.php
  ```
- miniflux
  ```
  https://your-miniflux-server/fever/
  ```

Example:
```emacs-lisp
(setq elfeed-protocol-fever-update-unread-only nil)
(setq elfeed-protocol-fever-fetch-category-as-tag t)
(setq elfeed-protocol-feeds '(("fever+https://user@myhost.com"
                               :api-url "https://myhost.com/plugins/fever/"
                               :password "my-password")))
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
(setq elfeed-protocol-feeds '(("newsblur+https://user@newsblur.com"
                               :password "my-password")))
```

## owncloud (ownCloud News)
1. Fetch articles with the modified time by default
1. Fetch articles for special feed
1. Fetch remote category as tag
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
(setq elfeed-protocol-owncloud-fetch-category-as-tag t)
(setq elfeed-protocol-feeds '(("owncloud+https://user@myhost.com"
                               :password "my-password")))
```

## ttrss (Tiny Tiny RSS, requires version: 1.7.6)
1. Fetch articles by the entry ID
1. Fetch articles for special feed
1. Fetch remote category as tag
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
(setq elfeed-protocol-ttrss-fetch-category-as-tag t)
(setq elfeed-protocol-feeds '(("ttrss+https://user@myhost.com"
                               :password "my-password")))
```

# Extra settings

## All example formats for elfeed-protocol-feeds

```emacs-lisp
(setq elfeed-protocol-feeds '(
                              ;; same format with elfeed-fedds
                              "http://foo/"
                              ("http://baz/" comic)

                              ;; format 1
                              "owncloud+https://user:pass@myhost.com"

                              ;; format 2, for username or password with special characters
                              ("owncloud+https://user@domain.com@myhost.com"
                               :password "password/with|special@characters:")

                              ;; format 3, for password in file
                              ("owncloud+https://user@myhost.com"
                               :password-file "~/.password")

                              ;; format 4, for password in .authinfo,
                              ;; ensure (auth-source-search :host "myhost.com" :port "443" :user "user4") exists
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
```

## Work with elfeed-org

Since version `0.9.0`, elfeed-protocol could work together with
elfeed-org without any aditional setup.

## Work with elfeed-summary

To fix `0 / 0` zero count issue for all feeds, just active the
following advice for `rmh-elfeed-org-export-feed`:

```emacs-lisp
(defun elfeed-protocol-advice-rmh-elfeed-org-export-feed (headline)
 "Advice for `rmh-elfeed-org-export-feed', add elfeed-protocol ID as suffix for each feed."
  (let* ((url (car headline))
         (proto-id (car (elfeed-protocol-feed-list))))
    (when proto-id
      (setcar headline (elfeed-protocol-format-subfeed-id proto-id url)))))
(advice-add 'rmh-elfeed-org-export-feed :before #'elfeed-protocol-advice-rmh-elfeed-org-export-feed)
```

Besides, don't use `elfeed-summary-update` to fetach articles,
use `elfeed-update` instead, and press `r` to refresh UI manually:
```emacs-lisp
(define-key elfeed-summary-mode-map (kbd "R") #'elfeed-update)
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
    (setq elfeed-protocol-feeds '("owncloud+http://<admin>:<password>@localhost"))
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
    (setq elfeed-protocol-feeds '("ttrss+http://admin:password@localhost"))
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

1. When I run elfeed-update I get the error: `elfeed-protocol-feeds malformed, bad entry`

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

1. Why the articles still are unread even they were mark read in other client?

   Well, only ownCloud News API support two-way synchronization for it
   fetch articles with modified time. And other API only fetch
   articles id by id. So your issue just the desired result~

   However here is a workaround. For example fever, you could reset
   the update mark so it will re-fetch the last 1000 articles in
   following updates and will sync the read state:

   ```emacs-lisp
   (let* ((proto-id "fever+https://user@miniflux-host")
          (last-id (elfeed-protocol-fever-get-update-mark proto-id 'update)))
     (elfeed-protocol-fever-set-update-mark  proto-id 'update (- last-id 1000)))
   ```

   And Fever limit 50 max size for per request, so update timer may help you:

   ```emacs-lisp
   (run-at-time 300 300
                (lambda () (when (= elfeed-curl-queue-active 0)
                             (elfeed-update))))
   ```

   Or you could use `elfeed-untag-1` mark all selected articles as
   read(will not call curl process) then execute
   `elfeed-protocol-fever-reinit` fetch all unread articles:

   ```emacs-lisp
   (cl-loop for entry in (elfeed-search-selected)
            do (elfeed-untag-1 entry 'unread))
   ```

   Hope helps.

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
