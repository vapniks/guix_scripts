# zshell functions for deactivating, backing-up & restoring guix profiles
# Source this file to make the functions available.

function deactivate-guix-profiles {
    # Parse command line options.
    local OPTIND=0 option CHANGEINFO CHANGEXDG OLDFLAG profile
    while getopts "hix" option; do
	case $option in
            (h)
		echo "Usage: deactivate-guix-profiles [OPTION]... [PROFILE]...
Remove guix profiles from \$PATH. [PROFILE] should be a guix profile directory/symlink.
Old value of \$PATH will be saved to \$_OLD_PATH.

Options:
 -h   Display this help
 -i   Also remove profiles from \$INFOPATH (old value will be saved to \$_OLD_INFOPATH)
 -x   Also remove profiles from \$XDG_DATA_DIRS (old value will be saved to \$_OLD_XDG_DATA_DIRS)"
		return
		;;
	    (i)
		CHANGEINFO=1
		;;
	    (x)
		CHANGEXDG=1
		;;
	    (*)
		echo "Invalid option!"
		return 1
		;;
	esac
    done
    # Backup the old values 
    OLDFLAG="${${(z)$(set +o|grep extendedglob)}[2]}"
    set -o extendedglob
    backup-paths
    # export _OLD_PATH="${PATH}"
    # if [[ ${CHANGEINFO} ]]; then
    # 	export _OLD_INFOPATH="${INFOPATH}"
    # fi
    # if [[ ${CHANGEXDG} ]]; then
    # 	export _OLD_XDG_DATA_DIRS="${XDG_DATA_DIRS}"
    # fi
    # Remaining arguments are stored in "${@[@]:$OPTIND}"
    for profile in ${@[@]:${OPTIND}}; do
	if [[ $(readlink -f ${profile}) =~ /gnu/store ]]; then
	    # PATH="${${${PATH//${profile}\/(s#)bin}//::#/:}#:}"
	    # if [[ $CHANGEXDG ]]; then
	    # 	XDG_DATA_DIRS="${${${XDG_DATA_DIRS//${profile}\/share}//::#/:}#:}"
	    # fi
	    # if [[ $CHANGEINFO ]]; then
	    # 	INFOPATH="${${${INFOPATH//${profile}\/share\/info}//::#/:}#:}"
	    # fi
	    echo "profile: ${profile}\n"
	    echo "PATH: ${${${PATH//${profile}\/(s#)bin}//::#/:}#:}\n"
	    if [[ ${CHANGEINFO} ]]; then
		echo "INFOPATH ${${${INFOPATH//${profile}\/share\/info}//::#/:}#:}\n"
	    fi
	    if [[ ${CHANGEXDG} ]]; then
		echo "XDG_DATA_DIRS ${${${XDG_DATA_DIRS//${profile}\/share}//::#/:}#:}\n"
	    fi
	else
	    echo "\"$profile\" is not a guix profile!"
	fi
    done
    set "${OLDFLAG}" extendedglob
    unset OLDFLAG CHANGEINFO CHANGEXDG option profile
}

# Restore the contents of relevant env variables from backup variables.
# Only argument is a prefix to use for the backup variable names (default is _OLD_)
function restore-paths {
    if [[ ${1} ]]; then
	if [[ ${1} =~ -h ]]; then
	    echo "Usage: restore-paths [-h|PREFIX]"
	    return
	fi
	eval "PATH=\$${1}PATH"
	eval "INFOPATH=\$${1}INFOPATH"
	eval "XDG_DATA_DIRS=\$${1}XDG_DATA_DIRS"
	echo "PATH, INFOPATH & XDG_DATA_DIRS restored from ${1}PATH, ${1}INFOPATH & ${1}XDG_DATA_DIRS"
    else	
	PATH=${_OLD_PATH}
	INFOPATH=${_OLD_INFOPATH}
	XDG_DATA_DIRS=${_OLD_XDG_DATA_DIRS}
	echo "PATH, INFOPATH & XDG_DATA_DIRS restored from _OLD_PATH, _OLD_INFOPATH & _OLD_XDG_DATA_DIRS"
    fi
}

# Backup relevant env variables to special backup variables.
# Only argument is a prefix to use for the backup variable names (default is _OLD_)
function backup-paths {
    if [[ ${1} ]]; then
	if [[ ${1} =~ -h ]]; then
	    echo "Usage: backup-paths [-h|PREFIX]"
	    return
	fi
	export ${1}PATH=${PATH}
	export ${1}INFOPATH=${INFOPATH}
	export ${1}XDG_DATA_DIRS=${XDG_DATA_DIRS}
	echo "PATH, INFOPATH & XDG_DATA_DIRS backedup to ${1}PATH, ${1}INFOPATH & ${1}XDG_DATA_DIRS"
    else
	export _OLD_PATH=${PATH}
	export _OLD_INFOPATH=${INFOPATH}
	export _OLD_XDG_DATA_DIRS=${XDG_DATA_DIRS}
	echo "PATH, INFOPATH & XDG_DATA_DIRS backedup to _OLD_PATH, _OLD_INFOPATH & _OLD_XDG_DATA_DIRS"	
    fi
}
