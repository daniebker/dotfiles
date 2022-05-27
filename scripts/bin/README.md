## FLAC2MP3
Requires `lame` to be installed.

`brew install lame`

`find ~/Music/ -name '*.flac' -print0 | xargs -0 ~/bin/flac2mp3`
