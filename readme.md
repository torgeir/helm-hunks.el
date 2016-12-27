# helm-hunks.el

A helm interface for git hunks.

## Installing

It's on [MELPA](https://melpa.org/#/getting-started), so run `M-x package-install helm-hunks` to install it.

Or preferably using the ingenious [use-package](https://github.com/jwiegley/use-package) declaration

```
(use-package helm-hunks
  :commands (helm-hunks
             helm-hunks-current-buffer
             helm-hunks-staged
             helm-hunks-staged-current-buffer))
```

## Features

Along with helm-follow-mode, this plugin will let you jump around and stage git hunks like never before!

<img src="https://github.com/torgeir/helm-hunks.el/blob/master/helm-hunks.gif?raw=true" width="80%" alt="helm-hunks-features"/>

### Jump around

Run `M-x helm-hunks`, turn on `helm-follow-mode` with `C-c C-f`, and jump around using `C-p` and `C-n`. Run `helm-hunks-current-buffer` to jump around the current buffer only.

### Preview changes

Hit `C-c C-p` inside helm-hunks to show diff previews in-line with the hunks. For even faster navigation, narrow the selection by typing in the helm buffer, like in any other helm plugin - this even works for parts of the actual diff!

### Stage hunks

For hunks you're ready to commit, hit `C-s` to stage the hunk directly from the helm-hunks buffer.

### Open hunk in other frame/window

Use `C-c C-o` and `C-c o` to open hunks in "other frame" and "other window", respectively.

### Unstage/reset hunks

To browse already staged hunks, run `helm-hunks-staged` or `helm-hunks-staged-current-buffer`, and use `C-u` to unstage/reset a hunk.

### Refresh git gutter indicators

Add a hook to `helm-hunks-refresh-hook` to refresh your favorite git gutter when hunks are staged. A hook for `git-gutter+` is already in place.

```
(add-hook 'helm-hunks-refresh-hook 'git-gutter+-refresh)
```

## Ideas

PRs welcome!

- [ ] Kill hunks
- [x] Allow resetting staged hunks
- [x] Show staged hunks

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
