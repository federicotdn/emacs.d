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
  (progn
    (spotify-call-method "PlayPause")
    (message "Music paused/resumed.")))

(defun spotify-next ()
  "Skip to next Spotify track."
  (interactive)
  (progn
    (spotify-call-method "Next")
    (message "Skipped to next track.")))

(defun spotify-previous ()
  "Go to previous Spotify track."
  (interactive)
  (progn
    (spotify-call-method "Previous")
    (message "Back to previous track.")))
