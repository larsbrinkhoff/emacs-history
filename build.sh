rm -rf build
mkdir build
cd build
git init

scrub() {
    find lisp -name '*.elc' | tee /dev/stderr | xargs rm
    find info -type f | grep -v -e 'info/dir$' -e 'info/COPYING$' | xargs rm
}

commit() {
    changelog=`head -1 src/ChangeLog`
    date=`echo "$changelog" | sed 's/\([0-9]\)  .*$/\1/'`
    name=`echo "$changelog" | sed 's/^.*  \(.*\)  .*$/\1/'`
    email=`echo "$changelog" | sed 's/^.*  [<(]\(.*\)[)>]$/\1/' | sed 's/ at /@/'`

    scrub

    git add -A .
    export GIT_COMMITTER_DATE="$date"
    export GIT_COMMITTER_NAME="$name"
    export GIT_COMMITTER_EMAIL="$email"
    git commit --date "$date" --author "$name <$email>" -m "Emacs $1."
    git tag emacs-$1
}

release() {
    git rm -r * > /dev/null
    rm -rf *
    echo "-- Emacs $1: Deleted -----------"; ls; $bash
    tar xzf ../$2
    chmod -R u+w .
    echo "-- Emacs $1: Extracted -----------"; ls; $bash
    mv *$1/* .
    rmdir *$1
    echo "-- Emacs $1: Moved -----------"; ls; $bash
    commit $1
}

get_dir() {
    git rm -r * > /dev/null
    rm -rf *
    echo "-- Emacs $1: Deleted -----------"; ls; $bash
    cp -r ../$2/* .
    echo "-- Emacs $1: Moved -----------"; ls; $bash
    patch -p1 < ../emacs-17.61-bsd.diff
    echo "-- Emacs $1: De-BSDified -----------"; ls; $bash
    commit $1
    echo "-- Emacs $1: Committed -----------"; ls; $bash
}

cat_or_zcat() {
    case "$1" in
	*.gz) zcat "$1";;
	*) cat "$1";;
    esac
}

apply_patch() {
    sh ../run/run-$1.sh
    echo "-- Emacs $1: Script -----------"; ls; $bash
    cat_or_zcat ../$2 | patch -f $PATCHOPTS
    find . -name '*.orig' -o -name '*.rej' | xargs rm -f
    echo "-- Emacs $1: Patched -----------"; ls; $bash
    commit $1
}

friedman=ftp.splode.com/pub/users/friedman/emacs
tuhs=www.tuhs.org/UnixArchive/4BSD/Distributions/4.3BSD
bitsavers=bitsavers.org/bits
decuslib=decuslib.com/decus
gwdg=ftp4.gwdg.de/pub/msdos/editors/emacs
funet=ftp.funet.fi/pub/gnu/funet/historical-funet-gnu-area-from-early-1990s
gnu=ftp.gnu.org/old-gnu/emacs
sunfreeware=ftp.tiscali.nl/pub/mirrors/sunfreeware/SOURCES
slackware=mirrors.slackware.com/slackware/slackware-3.1/source/e

PATCHOPTS=-p1

release 16.56 $friedman/emacs-16.56.tar.gz
get_dir 17.61 $tuhs/emacs

# Apply the 17.60 to 17.61 diff in reverse.
(PATCHOPTS="-R -p1" apply_patch 17.60 Usenet/net.emacs/emacs-17.61.diff)

# Some git gymnastics to reverse the order of the last two commits.
git tag -d emacs-17.60
git tag -d emacs-17.61
git branch tmp
git reset HEAD~2
rm -rf * && git checkout tmp -- . && commit 17.60
rm -rf * && git checkout tmp~ -- . && commit 17.61
git branch -D tmp

release 17.62 $bitsavers/DEC/vax/ultrix/3.0/gnuemacs.tar.gz
release 18.41 $bitsavers/MIT/gnu/emacs_18.41.tar.gz
release 18.51 $decuslib/vax88a1/gnusoftware/edist_18_51.tar_z
apply_patch 18.52 $funet/emacs/diff-18.51-18.52.gz
apply_patch 18.53 $funet/emacs/diff-18.52-18.53.gz
apply_patch 18.54 $funet/emacs/diff-18.53-18.54.gz
release 18.55 $gwdg/emacs-18.55.tar.gz
release 18.57 $funet/emacs/emacs-18.57.tar.gz
release 18.58 $funet/emacs/emacs-18.58.tar.gz
release 18.59 $gnu/emacs-18.59.tar.gz
release 19.7 $funet/old/emacs-19.7.tar.gz
release 19.8 $funet/old/emacs-19.8.tar.gz
release 19.9 $funet/old/emacs-19.9.tar.gz
apply_patch 19.10 $gnu/emacs-19.9-19.10.diff.gz
apply_patch 19.11 $gnu/emacs-19.10-19.11.diff.gz
apply_patch 19.12 $gnu/emacs-19.11-19.12.diff.gz
apply_patch 19.13 $gnu/emacs-19.12-19.13.diff.gz
apply_patch 19.14 $gnu/emacs-19.13-19.14.diff.gz
apply_patch 19.15 $gnu/emacs-19.14-19.15.diff.gz
apply_patch 19.16 $gnu/emacs-19.15-19.16.diff.gz
release 19.17 $funet/old/emacs-19.17.tar.gz
release 19.18 $funet/old/emacs-19.18.tar.gz
release 19.19 $funet/old/emacs-19.19.tar.gz
release 19.20 $funet/old/emacs-19.20.tar.gz
release 19.21 $funet/old/emacs-19.21.tar.gz
release 19.22 $funet/emacs/emacs-19.22.tar.gz
release 19.23 $funet/emacs/emacs-19.23.tar.gz
release 19.24 $funet/emacs/emacs-19.24.tar.gz
release 19.25 $funet/emacs/emacs-19.25.tar.gz
release 19.26 $funet/emacs/emacs-19.26.tar.gz
release 19.27 $funet/emacs/emacs-19.27.tar.gz
release 19.28 $funet/emacs/emacs-19.28.tar.gz
release 19.29 $sunfreeware/emacs-19.29.tar.gz
release 19.30 $sunfreeware/emacs-19.30.tar.gz
release 19.31 $slackware/emacs-19.31.tar.gz
apply_patch 19.32 $gnu/emacs-19.31-19.32.diff.gz
apply_patch 19.33 $gnu/emacs-19.32-19.33.diff.gz
release 19.34 $gnu/emacs-19.34b.tar.gz
