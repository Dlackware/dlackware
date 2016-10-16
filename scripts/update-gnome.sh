#!/bin/bash

##### Configuration
GNOME_VERSION=3.22
MIRROR=http://ftp.gnome.org/mirror/gnome.org/sources
REPO=gnome/base

##### Functions
# Returns 0 if the URL exists, 1 otherwise.
function check_status {
	status=$(curl -Is $1 | head -n1)

	if [[ "$status" == "HTTP/1.1 404 Not Found"* ]]
	then
		return 1
	else
		return 0
	fi
}

##### Program
for prg in $(ls $REPO)
do
	check_status "$MIRROR/$prg/$GNOME_VERSION"

	# Check if the main Gnome version works for the package
	if [ $? -eq 1 ]
	then
		check_status "$MIRROR/$prg"
		# Package not found, skip
		if [ $? -eq  1 ]
		then
			continue
		fi

		pkg_version=$(curl -s $MIRROR/$prg/ | tail -n5 | head -n1 | grep -Eo '[[:digit:]]+\.[[:digit:]]+' | head -n1)
	else
		pkg_version=$GNOME_VERSION
	fi
	pkg_url=$MIRROR/$prg/$pkg_version

	check_status "$pkg_url"
	if [ $? -eq 1 ]
	then
		continue
	fi

	pkg_version=$(curl -s $pkg_url/ | grep -Eo 'LATEST-IS-[[:digit:]\.]+' | head -n1)
	pkg_version=${pkg_version:10}

	. $REPO/$prg/$prg.info

	echo $prg $pkg_version
	sed -i "s/VERSION=\${VERSION:-$VERSION}/VERSION=\${VERSION:-$pkg_version}/" $REPO/$prg/$prg.SlackBuild
	sed -i -e "s/$VERSION/$pkg_version/g" -e 's/3.18/3.22/g' $REPO/$prg/$prg.info
	echo
done
