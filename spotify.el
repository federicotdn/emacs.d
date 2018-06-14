(require 'dbus)

(defun spotify-call-method (method)
  "Call any Spotify method."
  (dbus-call-method
   :session
   "org.mpris.MediaPlayer2.spotify"
   "/org/mpris/MediaPlayer2"
   "org.mpris.MediaPlayer2.Player"
   method))

(defun spotify-play-pause ()
  "Play or pause Spotify."
  (interactive)
  (spotify-call-method "PlayPause"))

(defun spotify-next ()
  "Skip to next Spotify track."
  (interactive)
  (spotify-call-method "Next"))

(defun spotify-previous ()
  "Go to previous Spotify track."
  (interactive)
  (spotify-call-method "Previous"))
