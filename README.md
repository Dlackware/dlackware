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

