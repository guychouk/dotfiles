;;; early-init.el --- Suppress UI before frame creation -*- lexical-binding: t -*-

(setq inhibit-startup-screen t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
