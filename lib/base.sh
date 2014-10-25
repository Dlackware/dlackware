#!/bin/bash
#
# Dlackware build system, inpsired by Dropline Build System
# Copyright 2014 Eugene Wissner, Dachau, Germany
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
	$prog_name build             - Build all packages
	$prog_name check-info        - Check for missing and incomplete .info files and fix them
	$prog_name help              - Show this help message\n"

	if [ -n $1 ]
	then
		exit $1
	fi
}

## build()
##   Build all.
##
build() {
	local repo

	tmp=$(mktemp)

	for repo in "${DLACK_REPOS[@]}"
	do
		# Remove comments from the compile order and go line for line
		sed -e 's/\s*#.*//' -e '/^\s*$/d' $repo/compile-order > $tmp

		for pkg in $(cat $tmp)
		do
			# Unset variables can be set from the previous package
			unset old_pkg VERSION BUILD TAG OUTPUT PKGTYPE

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

			# Build
			PKGNAM=$(basename $pkg)
			sh $PKGNAM.SlackBuild

			# Install
			# We need some information about the package to be able to install it later
			eval $(grep -m 1 "^VERSION="  $PKGNAM.SlackBuild)
			eval $(grep -m 1 "^BUILD="  $PKGNAM.SlackBuild)
			eval $(grep -m 1 "^TAG="  $PKGNAM.SlackBuild)
			eval $(grep -m 1 "^OUTPUT="  $PKGNAM.SlackBuild)
			PKGTYPE=$(sed -n 's/PKGTYPE=${PKGTYPE:-\(t[[:alpha:]]z\)}/\1/p' $PKGNAM.SlackBuild)

			case "$( uname -m )" in
				i?86) export ARCH=i486 ;;
				arm*) export ARCH=arm ;;
				*) export ARCH=$( uname -m ) ;;
			esac

			# Install the package
			/sbin/upgradepkg --reinstall --install-new \
				$old_pkg$OUTPUT/$PKGNAM-$VERSION-$ARCH-$BUILD$TAG.${PKGTYPE:-txz}
			)
		done
	done

	# Remove temporary file again
	rm -f $tmp
}

## check-info()
##   Check for missing .info files.
##   Add md5 check sums, replace PRGNAM with PKGNAM.
##
check-info() {
	local tmp repo pkg path PKGNAM VERSION HOMEPAGE DOWNLOAD MD5SUM

	tmp=$(mktemp -d)

	for repo in "${DLACK_REPOS[@]}"
	do

		# Find all packages with SlackBuild but without an .info file
		for pkg in $(find $repo -type f -name '*.SlackBuild')
		do
			path=$(dirname $pkg)
			PKGNAM=$(basename $pkg)
			PKGNAM=${PKGNAM:0:-11}
			if [ ! -e "$path/$PKGNAM.info" ]
			then
				log "$PKGNAM: .info file is missing"
				# Try to get the version from the build script
				VERSION=$(grep -m 1 '^VERSION=' $path/$PKGNAM.SlackBuild)
				VERSION=${VERSION:19:-1}
				read -e -p 'Version: ' -i $VERSION VERSION

				read -p 'Homepage: ' HOMEPAGE
				read -p 'Download: ' DOWNLOAD
				download_name=${DOWNLOAD##*/}
				wget -c -P $tmp $DOWNLOAD
				MD5SUM=$(md5sum $tmp/$download_name)
				MD5SUM=${MD5SUM:0:32} # md5sum has 32 characters, the rest is the filename

				log "$path/$PKGNAM.info:"
				echo "PKGNAM=\"$PKGNAM\"
VERSION=\"$VERSION\"
HOMEPAGE=\"$HOMEPAGE\"
DOWNLOAD=\"$DOWNLOAD\"
MD5SUM=\"$MD5SUM\"" > $path/$PKGNAM.info
				cat $path/$PKGNAM.info
				echo
				echo
			fi
		done
	done

	# Remove temporary directory
	rm -rf $tmp
}
