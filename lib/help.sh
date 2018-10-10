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


## die() message
##   This function emits a message and exits the program.
##
die() {
	local msg=$*
    
	if [ -n "$msg" ]
	then
		tput bel # Bell
		tput setaf 1 # Red colour
		tput bold
		echo "$msg"
		tput sgr0 # Reset
	fi

	exit 1
}

## log() message
##   This function emits a message.
##
log() {
	local msg=$*
    
	tput setaf 2 # Green colour
	tput bold
	echo -e "$msg"
	tput sgr0 # Reset
}
