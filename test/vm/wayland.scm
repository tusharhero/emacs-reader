;; Guix system configuration with Wayland, for testing purposes.

;; Copyright (C) 2025  Tushar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(use-modules (gnu)
	     (gnu packages)
	     (gnu packages base)
	     (gnu packages terminals)
	     (gnu services xorg)
	     (gnu services sddm)
	     (gnu packages pdf))

(use-service-modules desktop)
(use-package-modules bootloaders emacs wm pdf)

(operating-system
 (host-name "waylandvm")

 (users (cons (user-account
               (name "user")
               (group "users")
               (supplementary-groups '("wheel" "netdev"
                                       "audio" "video")))
              %base-user-accounts))

 (packages (append (list
                    sway
		    foot
                    emacs-pgtk
		    gnu-make
		    mupdf)
		   %base-packages))

 (services
  (append (list (service sddm-service-type
			 (sddm-configuration
			  (auto-login-user "user")
			  (auto-login-session "sway.desktop"))))
	  (modify-services %desktop-services
			   (delete gdm-service-type))))


 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets '("/boot/efi"))))

 (file-systems (append
                (list (file-system
                       (device (file-system-label "root"))
                       (mount-point "/")
                       (type "ext4"))
                      (file-system
                       (device (file-system-label "boot-efi"))
                       (mount-point "/boot/efi")
                       (type "vfat")))
                %base-file-systems)))
