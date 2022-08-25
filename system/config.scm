(use-modules (gnu))
(use-service-modules networking ssh)
(use-package-modules screen ssh)

(operating-system
 (host-name "biosoft")
 (timezone "Europe/London")
 (locale "en_GB.utf8"))
