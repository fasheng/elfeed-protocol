- OS version: `???`
- emacs version: `???`
- elfeed version: `???`
- elfeed-protocol version: `???`
- curl version: `???`
- server version: `???` (docker image and tag)
- is variable `elfeed-protocol-xxx-feeds` empty: `yes/no`

**elfeed-log**

Please collect logs in buffer `*elfeed-log*` with the following config
before reporting issues:

```emacs-lisp
(setq elfeed-log-level 'debug)
(toggle-debug-on-error)

;; for more logs if necessary
(setq elfeed-protocol-log-trace t)
(setq elfeed-protocol-fever-maxsize 5)
(setq elfeed-protocol-owncloud-maxsize 5)
(setq elfeed-protocol-ttrss-maxsize 5)
(setq elfeed-protocol-newsblur-maxpages 1)
```

**error backtrace**

```
```
