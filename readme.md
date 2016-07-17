# helm-hunks - a helm interface for git hunks

A helm interface for jumping around git hunks. Along with helm-follow-mode, this plugin will let you jump around and stage git hunks like never before!

## Features

With `helm-hunks` you can..

### Jump around

Run `M-x helm-hunks`, turn on `helm-follow-mode` with `C-c C-f`, and jump around using `M-p` and `M-n`.

### Preview changes

Hit `C-c p` inside helm-hunks to show diff previews in-line with the hunks. For even faster navigation, narrow the selection by typing in the helm buffer, like in any other helm plugin - this even works for parts of the actual diff!

### Stage hunks

For hunks you're ready to commit, hit `C-s` to stage the hunk directly from the helm-hunks buffer.

### Open hunk in other frame/window

Use `C-c C-o` and `C-c o` to open hunks in "other frame" and "other window", respectively.

### Refresh git gutter indicators

Add a hook to `helm-hunks-refresh-hook` to refresh your favorite git gutter when hunks are staged. A hook for `git-gutter+` is already in place.

```
(add-hook 'helm-hunks-refresh-hook 'git-gutter+-refresh)
```

## Ideas

PRs welcome!

- [ ] Kill hunks
- [ ] Show staged hunks
- [ ] Allow resetting staged hunks

## Credits/inspiration

git-gutter+ - https://github.com/nonsequitur/git-gutter-plus

helm-ag - https://github.com/syohex/emacs-helm-ag

## License

Copyright (C) 2016 Torgeir Thoresen

Author: @torgeir

Keywords: helm git hunks vc

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
