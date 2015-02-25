sandy () {
        local DIR=$PWD
        local TARGET="cabal.sandbox.config"
        local PROG=$1
        shift

        while [ ! -e $DIR/$TARGET -a $DIR != "/" ]
        do
                DIR=$(dirname $DIR)
        done
        if test $DIR != "/"
        then
                local DB=$(sed -ne '/^package-db: */{s///p;q;}' "$DIR/$TARGET")
                $PROG -no-user-package-db -package-db="$DB" "$@"
        else
                $PROG "$@"
        fi
}
