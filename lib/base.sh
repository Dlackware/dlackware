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
    $prog_name check-info          - Check for missing and incomplete .info files and fix them.
    $prog_name install             - Install all packages.
    $prog_name download-source     - Download all sources.
    $prog_name git [repository]    - Clone the Dlackware script repositories.
                                     If the repository is not specified, clone all.
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

	tmp=$(mktemp -p /tmp/dlackware)

	for order in "${DLACK_REPOS[@]}"
	do
		repo=$(dirname $DLACK_REPOS_ROOT/$order)

		# Remove comments from the compile order and go line for line
		sed -e 's/\s*#.*//' -e '/^\s*$/d' $DLACK_REPOS_ROOT/$order > $tmp

		for pkg in $(cat $tmp)
		do
			# Unset variables can be set from the previous package
			unset old_pkg VERSION PKGNAM HOMEPAGE DOWNLOAD MD5SUM

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

			# Download the source packages
			if [ "$1" != "install" ]
			then
				wget -c $DOWNLOAD
			fi

			# Break here if we want only to download
			if [ "$1" = "download" ]
			then
				continue
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

			case "$( uname -m )" in
				i?86) export ARCH=i486 ;;
				arm*) export ARCH=arm ;;
				*) export ARCH=$( uname -m ) ;;
			esac

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

## check_info()
##   Check for missing .info files.
##   Add md5 check sums, replace PRGNAM with PKGNAM.
##
check_info() {
	local ans tmp order repo pkg path maj_min PKGNAM VERSION HOMEPAGE DOWNLOAD MD5SUM

	tmp=$(mktemp -d /tmp/dlackware.XXXXXX)
	log "Temporary directory $tmp is created.\n"

	for order in "${DLACK_REPOS[@]}"
	do
		repo=$(dirname $order)

		# Find all packages with SlackBuild but without an .info file
		for pkg in $(find $repo -type f -name '*.SlackBuild')
		do
			path=$(dirname $pkg)
			PKGNAM=$(basename $pkg)
			PKGNAM=${PKGNAM:0:-11}
			if [ ! -e "$path/$PKGNAM.info" ]
			then
				log "$PKGNAM: .info file is missing (set the version to an empty string to skip the package)."

				# Try to get the version from the build script
				VERSION=$(grep -Po -m 1 '(?<=VERSION=\${VERSION:-).*(?=})' $path/$PKGNAM.SlackBuild || true)
				if [ -n "$VERSION" ]
				then
					read -e -p 'Version: ' -i $VERSION VERSION
				else
					read -p 'Version: ' VERSION
				fi
				if [ -z "$VERSION" ]
				then
					echo
					continue
				fi

				read -p 'Homepage: ' HOMEPAGE

				# Use gnome ftp as the default download url
				maj_min=${VERSION%.*}
				read -e -p 'Download: ' -i \
					ftp://ftp.gnome.org/pub/gnome/sources/$PKGNAM/$maj_min/$PKGNAM-$VERSION.tar.xz ans

				check_md5 $ans $tmp

				log "$path/$PKGNAM.info:"
				echo "PKGNAM=\"$PKGNAM\"
VERSION=\"$VERSION\"
HOMEPAGE=\"$HOMEPAGE\"
DOWNLOAD=\"$DOWNLOAD\"
MD5SUM=\"$MD5SUM\"" > $path/$PKGNAM.info
				cat $path/$PKGNAM.info

				echo -e "\n"
			else # Some checks on existing .info files
				# Replace PRGNAM with PKGNAM
				if grep --quiet "PRGNAM" $path/$PKGNAM.info
				then
					sed -i 's/PRGNAM/PKGNAM/' $path/$PKGNAM.info
					log "$PKGNAM: PRGNAM replaced with PKGNAM"
					read -p "Press Enter to continue"
					echo
				fi

				unset VERSION HOMEPAGE DOWNLOAD MD5SUM
				source $path/$PKGNAM.info
				
				if [ -z "$HOMEPAGE" ]
				then
					log "$PKGNAM: HOMEPAGE isn't set."
					read -p 'Homepage: ' HOMEPAGE
					echo
				fi

				if [ -z "$MD5SUM" ]
				then
					log "$PKGNAM: MD5SUM isn't set."
					check_md5 $DOWNLOAD $tmp
					echo
				else
					# Keep multi-line output
					DOWNLOAD="$(echo $DOWNLOAD | sed 's/\s/ \\\n          /g')"
					MD5SUM="$(echo $MD5SUM | sed 's/\s/ \\\n        /g')"
				fi

				echo "PKGNAM=\"$PKGNAM\"
VERSION=\"$VERSION\"
HOMEPAGE=\"$HOMEPAGE\"
DOWNLOAD=\"$DOWNLOAD\"
MD5SUM=\"$MD5SUM\"" > $path/$PKGNAM.info
			fi
		done
	done

	# Remove temporary directory
	rm -rf $tmp
}

## git() repository
##   Clone from git.
##
## Params:
##   repository (Optional) = The git repository should be cloned.
##
git() {
	# Set default repositories if not specified.
	if [ -z "$1" ]
	then
		repos=(pam systemd gnome)
	else
		repos=$@
	fi

	for repo in ${repos[@]}
	do
		$(which git) clone "https://github.com/Dlackware/$repo" "$DLACK_REPOS_ROOT/$repo"
	done
}
