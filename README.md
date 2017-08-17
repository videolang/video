Video
=====
Video is a DSL for describing videos. It is still under heavy development. The API is becoming more stable. Features may get deprecated, but will be marked well before remove. (See the `CONTRIBUTING.md` file for details.) This ReadMe is for stable builds of video except for the *Nightly Development Badges* section.

# Development Badges
## Unstable
[![Build Status](https://travis-ci.org/videolang/video.svg?branch=master)](https://travis-ci.org/videolang/video)
[![Build status](https://ci.appveyor.com/api/projects/status/f2t9op5dflo67ls4?svg=true)](https://ci.appveyor.com/project/LeifAndersen/video)
[![Coverage Status](https://coveralls.io/repos/github/videolang/video/badge.svg?branch=master)](https://coveralls.io/github/videolang/video?branch=master)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/video@video-unstable/index.html)
[![Project Stats](https://www.openhub.net/p/video/widgets/project_thin_badge.gif)](https://www.openhub.net/p/video)

## Testing
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/video@video-testing/index.html)

## Stable
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/video@video/index.html)

# Website

The video website can be found at [lang.video][3]

# Install

Before you can install Video, make sure you have [Racket][1] installed and in your operating systems, path.

The easiest way to install the stable version of the:w
Video is with `raco`, which comes bundled with [Racket][1] simply run:

```
raco pkg install video
```

If you want a particular build of video, you could alternatively run:

```
raco pkg install video-<version>
```

Where <version> is one of [these versions][8]. These versions are either Video releases or one of:

* `video`         - Stable Video. It is the most tested, and is set to the latest stable release.
* `video-testing` - Less stable than `video`, but can still being developed. This branch is set to the latest released or pre-released version, including alphas, betas, and release candidates.
* `video-unstable` - The least stable of the three main branches. This is the same as the `master` branch. It should build most of the time, but can occasnionally fail. If you are experiencing a bug, it _may_ be fixed in this branch.

Finally, you can install video directly from the git repo. The `master` branch (`video-unstable`), is the default branch, but you can alternatively checkout different branches or tags. To install directly from the repo:

```
git clone https://github.com/videolang/video.git
cd video
raco pkg install
```

You can optionally check out a different branch using `git checkout`.

# Uninstall

To uninstall Video, run:

```
raco pkg remove video
```

# Releases

You can find the latest releases for video on the [video releases page][2]. The `master` branch tends to be stable, but is not as thoroughly tested.

# Documentation

The Documentation for Video can be found at [the Racket package server][4].

# Bug reports

Bugs can be filed either [anonymously][6] or on [Video's bug report tracker][7].

# Contributing

If you want to contribute to video, you can read the [`CONTRIBUTING.md`][5] file.4

[1]: https://racket-lang.org
[2]: https://github.com/videolang/video/releases
[3]: http://lang.video
[4]: http://docs.racket-lang.org/video@video/index.html
[5]: https://github.com/videolang/video/blob/master/CONTRIBUTING.md
[6]: https://gitreports.com/issue/videolang/video
[7]: https://github.com/videolang/video/issues
[8]: https://pkgd.racket-lang.org/pkgn/search?q=&tags=vidlang+