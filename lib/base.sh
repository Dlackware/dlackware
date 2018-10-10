#!/bin/bash
#
# Dlackware build system, inpsired by Dropline Build System
# Copyright 2015-2016 Eugene Wissner, Dachau, Germany
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


# Include additional libraries
. $DLACK_LIB/help.sh

## usage() exit_status
##   Show usage and exit if the status was passed.
##
usage() {
	local prog_name=$(basename $0)

	echo -e "\nDlackware - version $DLACK_VER\n
Usage:
    $prog_name build               - Build all packages.
    $prog_name install             - Install all packages.
    $prog_name download-source     - Download all sources.
    $prog_name help                - Show this help message.\n"

    if [ -n $1 ]
    then
        exit $1
    fi
}

## build() action
##   Build all.
##
## Params:
##   action = "download", "install" or empty for the complete build and installation.
##
build() {
	local repo order old_pkg

	mkdir -p /tmp/dlackware
	tmp=$(mktemp -p /tmp/dlackware)

	for order in "${DLACK_REPOS[@]}"
	do
		repo=$(dirname $DLACK_REPOS_ROOT/$order)

		# Remove comments from the compile order and go line for line
		sed -e 's/\s*#.*//' -e '/^\s*$/d' $DLACK_REPOS_ROOT/$order > $tmp

		for pkg in $(cat $tmp)
		do
			# Unset variables can be set from the previous package
			unset old_pkg VERSION PKGNAM HOMEPAGE DOWNLOAD MD5SUM SRCNAM

			# Check if some package should be replaced
			if [[ $pkg == *%* ]]
			then
				old_pkg=$(echo $pkg | cut -d'%' -f1)%
				pkg=$(echo $pkg | cut -d'%' -f2)
			fi

			if [ ! -d $repo/$pkg ]
			then
				die "$pkg wasn't found."
			fi

			(
			cd $repo/$pkg

			# Read .info file
			source $(basename $pkg).info

			if [ -z $SRCNAM ]
			then
				file1=$PKGNAM-$VERSION.tar.?z*
			else
				file1=$SRCNAM-$VERSION.tar.?z*
			fi

			# Download the source packages if needed
			if [ "$1" != "install" -a -n "$DOWNLOAD" ]
			then
				wget -nc $DOWNLOAD
			fi

			# Break here if we want only to download
			if [ "$1" = "download" ]
			then
				continue
			fi

			# Create md5sum from downloaded file and check it against .info value
			file2=`md5sum $file1 | awk '{ print $1 }'` || exit 1
			file3=`echo $MD5SUM | cut -d ' ' -f1`

			echo $file1
			echo "Checking source   : $file2"
			echo "Checking info file : $file3"

			if [ "$file2" == "$file3" ]
			then
				echo "checksums OK"
			else
				echo "md5 sums mismatch"
				echo "Updating md5sum string"
				sed -i "s/${MD5SUM}/${file2}/g" $PKGNAM.info
			fi

			# Unset variables can be set from the previous package
			unset BUILD TAG OUTPUT PKGTYPE

			# Build
			if [ -z "$1" ]
			then
				sh $PKGNAM.SlackBuild
			fi

			# Install
			# We need some information about the package to be able to install it later
			eval $(grep -m 1 "^BUILD="  $PKGNAM.SlackBuild)
			eval $(grep -m 1 "^TAG="  $PKGNAM.SlackBuild)
			eval $(grep -m 1 "^OUTPUT="  $PKGNAM.SlackBuild)
			PKGTYPE=$(sed -n 's/PKGTYPE=${PKGTYPE:-\(t[[:alpha:]]z\)}/\1/p' $PKGNAM.SlackBuild)

			if grep -q "^ARCH=noarch$" $PKGNAM.SlackBuild
			then
				ARCH=noarch
			else
				case "$( uname -m )" in
					i?86) export ARCH=i486 ;;
					arm*) export ARCH=arm ;;
					*) export ARCH=$( uname -m ) ;;
				esac
			fi

			# Install the package
			/sbin/upgradepkg --reinstall --install-new \
				$old_pkg$OUTPUT/$PKGNAM-$VERSION-$ARCH-$BUILD$TAG.${PKGTYPE:-txz}

			# Delete contens of the default temporary build directory
			rm -rf /tmp/dlackware/package-*
			)
		done

		if [ "$1" != "download" ]
		then
			# Keep MIME database current:
			/usr/bin/update-mime-database /usr/share/mime > /dev/null 2>&1
		fi
	done

	# Remove temporary file again
	rm -f $tmp
}
