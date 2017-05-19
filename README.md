elfeed-backends
==============
[![MELPA](http://melpa.org/packages/elfeed-org-badge.svg)](http://melpa.org/#/elfeed-org)

Provide extra backends to make self-hosting RSS readers works
with [elfeed](https://github.com/skeeto/elfeed),
including
[Nextcloud/ownCloud News](https://nextcloud.com/),
[Tiny Tiny RSS(TODO)](https://tt-rss.org/fox/tt-rss),
[NewsBlur(TODO)](https://newsblur.com/) and even more.

# Installation through MELPA

    ;; Install through package manager
    M-x package-install <ENTER>
    elfeed-backends <ENTER>

# Initialization
Setup elfeed-backends, then switch to search view and and press G to update entries:

        (setq elfeed-backends-source 'ocnews)
        (setq elfeed-backends-ocnews-url "http://127.0.0.1:8080")
        (setq elfeed-backends-ocnews-username "user")
        (setq elfeed-backends-ocnews-password "password")
        (setq elfeed-use-curl t)
        (elfeed-set-timeout 36000)
        (setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate

# Have a Try
If you never use such slef-hosting RSS readers, why not deploy one in 5 minutes. For
example Nextcloud:

1.  Fetch Nextcloud image and run it

        docker pull nextcloud
        docker run --rm -p 8080:80 nextcloud

2.  Open <http://127.0.0.1:8080> in browser to setup
    1.  Create admin user and select database to SQLite, then press "Finish setup"
    2.  Press left top popup menu and select "+Apps", select
        "Multimedia", and enable the "News" app
    3.  Press left top popup menu and switch to "News" app, then
        subscribe some feeds

3.  Setup elfeed-backends or
    other
    [Nextcloud News clients](https://github.com/owncloud/News-Android-App),
    both will works OK

# License

Released under the terms of the GNU GPLv3+.
