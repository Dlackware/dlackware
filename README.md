# dlackware

builder/installer for Projects using a compile-order

gnome3 + pam + systemd on top of Slackware

Hi All,

Over the past few months we have created and tested gnome3 + pam + systemd on top of Slackware
The SlackBuilds and order in which to install / build can be located on github.

* NOTE (the stage we are in is called pre-alpha for Slackware 14.2, the packages have been build on Slackware64-current (1 februari 2015)

https://github.com/Dlackware (github)
http://bart.dlackware.com/dlackware64-14.2/pre-alpha/ (packages)

There are 3 projects on this page

- dlackware (build and install)
- systemd (pam + systemd + rebuilds of stock slackware packages needed for gnome3)
- gnome-systemd (gnome3 which requires systemd to be installed)

How can you use/build/install these packages ?

you should download all 3 git repos
the file dlackware/etc/dlackware.conf.dist holds the configuration for the compile-order files.

If you directly want to install and test the desktop, you can issue: "./dlackware install" from the dlackware/bin directory
You can also build everything from source, in this case you can issue: "./dlackware build" from the dlackware/bin directory

there are 2 settings.sh files that are recommended to be run, but not required.
the 1st settings.sh file can be found in systemd/minimal/settings.sh (these settings are mainly for use when you build from source)
the 2nd settings.sh file can be found in gnome-systemd/gnome/settings.sh (these enable / disable services)

Issues/bugs that you find can be reported either on the github page or here on the Forum 

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

3. See `dlackbuild.yaml` in the root directory for configuration options.

4. Run:

```shell
stack exec dlackware
```

### How to run tests

```shell
stack test
```
