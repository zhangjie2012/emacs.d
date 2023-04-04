(setq inhibit-automatic-native-compilation t)

(setq read-process-output-max (* 1024 1024))  ;; 1mb
(setq gc-cons-threshold (* 1024 1024 100))  ;; 100mb

(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8-unix)
(set-terminal-coding-system  'utf-8-unix)
(set-keyboard-coding-system  'utf-8-unix)
;; (set-selection-coding-system 'utf-8-unix)
(prefer-coding-system        'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)
      default-buffer-file-coding-system 'utf-8-unix) ;; Windows/ Linux/Mac all LF

(setq system-time-locale "C")
;; (setq system-time-locale "zh_CN.UTF-8")
