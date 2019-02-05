# Dlackware Gnome build system and installer

[![Build Status](https://semaphoreci.com/api/v1/belka-ew/dlackware/branches/master/badge.svg)](https://semaphoreci.com/belka-ew/dlackware)
[![License](https://img.shields.io/badge/license-GPL--3.0-blue.svg)](https://choosealicense.com/licenses/gpl-3.0/)

Over the past few months we have created and tested gnome3 + pam + systemd on top of Slackware
The SlackBuilds and order in which to install / build can be located on github.

There are 3 projects on this page

- dlackware (build and install)
- systemd (pam + systemd + rebuilds of stock slackware packages needed for gnome3)
- gnome-systemd (gnome3 which requires systemd to be installed)

## New experimental build system

### Why another build system?

When I started to write the current build system, I thought: It should be
simple since all build information is in the build scripts, the build system
itself should just run build scripts in a particular order. My assumption was
wrong. Even when using build scripts a build system can do a lot: check
downloaded tarballs aren't corrupted, read configuration files, validate the
build scripts are formatted correctly, update a bunch of packages to newer
version and so on.

The main problem of the current build system is that it is written in Bash.
Shell (and Bash) is a great tool to automate routine tasks but it isn't a
programming language for writing programs, though it is often used as such in
Slackware.

- Shell code is unmaintainable. Bash has basic support for functions and some
data structures like arrays, but it is very difficult to split a bash program
into smaller, readable, maintainable and testable parts.

- Shell code isn't testable. Found a bug? Fix it and hope that nothing else
is now broken and that the bug doesn't come back later.

- Shell programs use shell scripts as configuration files. The new build system
uses Yaml, which is a widely used, human-friendly format.

- I have no idea whether it is possible to write a parser in Bash. We actually
could partly use XML-files from JHBuild to build Gnome instead maintaining
our own package list for each release. It is almost impossible to achieve with
the current Bash approach.

- Shell is slow (not important for this build system since the most time is
spent building programs)

So here is an attempt to create a better build system with more potential.
Please note, it is in *experimental* stage and is being tested only on
Slackware-current. For building Gnome don't use `master`, but on of the
releases. We still have to see if we can improve our build process.

### How to give it a try

1. Install Haskell stack:

```shell
wget -qO- https://get.haskellstack.org/ | sh
```

2. Build the build system:

```shell
stack build
```

It will take some time to download the dependencies on first run.

3. Copy `etc/dlackware.yaml.new` to `etc/dlackware.yaml` - here you have some
configuration options.

4. Clone Gnome repository with all submodules into
`/opt/dlackware-scripts/gnome` (the directory can be changed in
`etc/dlackware.yaml`).

5. Run:

```shell
stack exec dlackware -- build
```

or for further options:

```shell
stack exec dlackware -- --help
```

### How to run tests

```shell
stack test
```
