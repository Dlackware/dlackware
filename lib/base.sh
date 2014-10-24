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
	$prog_name create PACKAGE    - Create new build script
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

## create() name
##   This will create the build script for the package named 'name' based on a very simple
##   template.
##
create() {
	local path ans handy_ruler
	PKGNAM="$1"

	# List repositories
	echo "Select the repository:"
	read_choice ${DLACK_REPOS[@]}
	path=${DLACK_REPOS[$(($ans-1))]}

	# Select subdirectory
	echo "Subdirectory (leave empty if not needed):"
	read ans
	if [ -n "$ans" ]
	then
		path=$path/$ans/$PKGNAM
	else
		path=$path/$PKGNAM
	fi

	log "Create path: $path.\n"
	mkdir -p $path

	# Select the build template
	echo "Select the build system"
	for tpl in $DLACK_DATA/templates/*.SlackBuild
	do
		build_systems[${#build_systems[@]}+1]=$(basename "$tpl");
	done
	read_choice ${build_systems[@]}

	log "Copy templates into the destination directory.\n"
	install -m0755 $DLACK_DATA/templates/${build_systems[$ans]} $path/$PKGNAM.SlackBuild
	install -m0644 $DLACK_DATA/templates/slack-desc $path/slack-desc

	# slack-desc
	log "Edit slack-desc.\n"
	handy_ruler=$(yes " " | head -n ${#PKGNAM} | tr -d '\n')
	sed -i -e "s/appname/$PKGNAM/g" $path/slack-desc -e "s/       /$handy_ruler/g" $path/slack-desc

	echo -n "Please enter the program version: "
	read VERSION

	# Copyright, package name and version
	log "Set build script copyright, name and version.\n"
	sed -i -e "s/\(P[K|R]GNAM=\)/\1$PKGNAM/" \
		-e "s/<appname>/$PKGNAM/" \
		-e "5s/.*/# Copyright $(date +'%Y') $DLACK_USER/" \
		-e "s/VERSION=\${VERSION:-}/VERSION=\${VERSION:-$VERSION}/" $path/$PKGNAM.SlackBuild

	# Try to guess the url
	url=$(grep "^wget -c" $path/$PKGNAM.SlackBuild)
	if [ -n "$url" ]
	then
		url=${url:8} # Remove "wget -c " from the url
		maj_min_ver=$(echo $VERSION | cut -d. -f1,2)
		url=${url/<maj_min_ver>/$maj_min_ver}
		echo -en "Is $url\nthe correct download url (leave empty if yes, otherwise enter another url)?\n"
		read ans
		if [ -n "$ans" ]
		then
			url="$ans"
		fi
		log "Set url."
		sed -i "s|^wget -c .*$|wget -c $url|" $path/$PKGNAM.SlackBuild
	fi

	# Analize source
	rm -rf /tmp/dlackware/$PKGNAM
	mkdir -p /tmp/dlackware/$PKGNAM
	eval "url=\"$url\"" # Replace variables in the URL
	wget -c -P /tmp/dlackware/$PKGNAM $url
	tar -xvf /tmp/dlackware/$PKGNAM/*.tar.?z* -C /tmp/dlackware/$PKGNAM

	log "Search for man pages."
	if [ "$(find /tmp/dlackware/$PKGNAM -type f -exec grep  '^\.SH NAME$' {} \; | wc -l)" -eq 0 ]
	then
		sed -i -e '/# Compress man pages:/,+3d' \
			-e '\#--mandir=/usr/man \\#d' $path/$PKGNAM.SlackBuild
	fi

	log "Search for info pages."
	if [ "$(find /tmp/dlackware/$PKGNAM -type f -name '*.texi' | wc -l)" -eq 0 ]
	then
		sed -i "/# Compress info pages and remove the package's dir file/,+3d" $path/$PKGNAM.SlackBuild
	fi

	log "Search for perl files.\n"
	if [ "$(find /tmp/dlackware/$PKGNAM -type f -name 'Makefile.PL*' | wc -l)" -eq 0 ]
	then
		sed -i "/# Remove 'special' files/,+5d" $path/$PKGNAM.SlackBuild
	fi

	# Finish
	log "Please don't forget to add a description in slack-desc."
}
