
;; 1. UI elements: Use the alist method for the cleanest start
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-message t)

(tooltip-mode -1) ;; show tooltips in Echo Area (hover over description)
