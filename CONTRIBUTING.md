Video is still in early stages of development. The basic API is set, although a
lot of the details are subject to change. Once Video reaches v1.0, this API
will be fixed.

Currently, the `master` git branch is used for development. However, it is
important to try to keep it relatively stable. For example, try not to push
code that breaks the tests.

Releases get there own branches/tags, which match their release name (v0.0,
v0.1, etc.). These branches should remain stable. Additionally, tags should be
fixed. So if a patch needs to be applied to one of these branches, it is
important to increment the version number (v0.1.1, v0.1.2, etc). Finally, make
sure to update the LOG file whenever a new release is made.

New releases happen when a major change happens to the language. Currently this
happens 2-3 times a year.

Pre-Releases get their type following the version number with a dash.
Subsequent versions of the same type get a version number attached to the end.
For example, the following versions indicate pre-release versions: v0.2-alpha,
v0.2-beta.3, v0.2-rc.3, etc.

Once the project becomes more stable, we will create a more structured
contributing guide. For the moment, however, feel free to open PRs and push
directly to the master branch.

However, please make sure that you do NOT push broken builds to the `master`
branch.  Additionally, and changes that break backwards compatibility MUST be
noted in the `LOG` file. It would also be nice if your changes keep a linear
history, but this is not strictly necessary.

Any code in the `video/private` folder is not part of the public API, and can
change at any time without notice. Code in `video/tests` are no implementation
code should go there. Documentation for your contributions belongs in
`video/scribblings/`. The identifiers exported by the remaining paths are
either public or protected. If they are documented they are public, and can
only be changed between MAJOR releases. Protected identifiers are not
documented, but still exported by a module (or submodule) outside of these
designated directories. These can change, but should be noted in LOG.

Some experimental code is allowed in this repo.
However, if it is still extremely experimental please put it in the
http://github.com/videolang/render-prototype git repo.
