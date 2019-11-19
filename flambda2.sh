#!/bin/bash

# parralelism
J=8
# number of errors lines to show from the log file
N=15
# tmpfile for logigng command output
TMPFILE=$(mktemp /tmp/flambda2-build.XXXXXX)

# ##### WARNING !! ############################
# These 4 build directories should be distinct
# for the script to work correctly
# #############################################
# first stage build (4.10 branch)
FIRST_STAGE=
# second stage build (4.10 branch)
SECOND_STAGE=
# developpement repo (flambda2.0-stable branch)
DEV=
# final repo where the optimized compiler is built (flambda2.0-stable branch)
FINAL=

# ##############
# Check function
# ##############
# Check that all avriabels have been correctly set before trying to execute anything
check_aux () {
   if [ -z "$1" ]; then
     echo -e "\e[31mERROR !\e[0m variable $2 is empty"
     exit 1
   fi
}

check () {
  check_aux "${FIRST_STAGE}" "\$FIRST_STAGE"
  check_aux "${SECOND_STAGE}" "\$SECOND_STAGE"
  check_aux "${DEV}" "\$DEV"
  check_aux "${FINAL}" "\$FINAL"
}

# ##################
# Auxiliary function
# ##################
# Run a command in a directory and print some output message if it fails
# $1 : messgae to announce the beginiing
# $2 : directory to cd into before running
# $3 : command to run inside of $2
wrap () {
  echo -ne "$1 ..."
  cd $2 && $3 &> $TMPFILE
  if [ $? -eq 0 ]; then
    echo " done"
  else
    echo " failed !"
    echo -e "\e[31mERROR !\e[0m see full log file ${TMPFILE}, but here's an excerpt:"
    echo -e "\e[1m(DEV) $2 \$ $3\e[0m"
    tail -n ${N} ${TMPFILE}
    exit 1
  fi
}


# ##################
# Cleaning functions
# ##################

clean_aux () {
  wrap "cleaning repo" "$1" "git clean -dfx"
}

clean_tmpfiles () {
  rm -rf /tmp/flambda2-build.*
}

clean () {
  clean_tmpfiles
  clean_aux "${FIRST_STAGE}"
  clean_aux "${SECOND_STAGE}"
  clean_aux "${DEV}"
  clean_aux "${FINAL}"
}


# #########
# Configure
# #########

configure_aux () {
  wrap "configuring repo $1 with option $2" "$1" "./configure $2"
}

configure () {
  configure_aux "${FIRST_STAGE}" "--prefix=${FIRST_STAGE}/_install"
  configure_aux "${SECOND_STAGE}" "--prefix=${SECOND_STAGE}/_install"
  configure_aux "${DEV}" "--enable-flambda"
  configure_aux "${FINAL}" ""
}


# ###############
# Build functions
# ###############

base () {
  # Build first stage
  wrap "building first stage" "${FIRST_STAGE}" "make world.opt -j ${J}"
  wrap "installing first stage" "${FIRST_STAGE}" "make install"
  wrap "copying first stage make_opcodes and cvt_emit" "${FIRST_STAGE}" "cp tools/make_opcodes tools/cvt_emit _install/bin"

  # Build second stage
  wrap "building second stage" "${SECOND_STAGE}" "make world.opt -j ${J}"
  wrap "installing second stage" "${SECOND_STAGE}" "make install"
  wrap "copying second stage make_opcodes and cvt_emit" "${SECOND_STAGE}" "cp tools/make_opcodes tools/cvt_emit _install/bin"
}

# build the flambda2 compiler
flambda2 () {
  # Build the flambda2 compiler
  echo "setting up the env"
  export PATH=${FIRST_STAGE}/_install/bin/:$PATH
  wrap "building flambda2 compiler" "${DEV}" "dune build @world"
  wrap "overriding second stage ocamlopt with flambda2 compiler" "${DEV}" "cp _build/default/ocamlopt.opt ${SECOND_STAGE}/_install/bin"
}

# build the flambda2 stdlib
stdlib () {
  # Building the stdlib and std compiler with flambda2
  echo "setting up the env"
  export FLAMBDA2=y
  export OCAMLPARAM="_,flambda-invariants=0,flambda2-context-on-error=1"
  export PATH=${SECOND_STAGE}/_install/bin/:$PATH
  wrap "building stdlib and compiler with flambda2" "${FINAL}" "dune build @world"
}


# #####################
# Compilation functions
# #####################

compile () {
  export FLAMBDA2=y
  export OCAMLPARAM="_,flambda2-context-on-error=1"
  local COMP="${DEV}/_build/default/ocamlopt.opt"
  ${COMP} -cclib -lm -cclib -ldl -cclib ${DEV}/_build/default/runtime/libasmrun.a \
    -nopervasives -nostdlib "$@"
}

# compile with the flambda2 compiled stdlib
ocamlopt () {
  export FLAMBDA2=y
  export OCAMLPARAM="_,flambda2-context-on-error=1"
  local COMP="${DEV}/_build/default/ocamlopt.opt"
  ${COMP} -cclib -lm -cclib -ldl -cclib ${DEV}/_build/default/runtime/libasmrun.a -nopervasives -nostdlib \
    -I ${FINAL}/_build/default/stdlib/.stdlib.objs/native \
    -I ${FINAL}/_build/default/stdlib/.stdlib.objs/byte \
    ${FINAL}/_build/default/stdlib/stdlib.cmxa \
    -open Stdlib "$@"
}

# #################
# Main Control Flow
# #################

# First, check that variables have been set
check
# now switch on the main command argument
case "$1" in
  'clean' )
    clean
    ;;
  'clean_tmp' )
    clean_tmpfiles
    ;;
  'configure' )
    configure
    ;;
  'base' )
    base
    ;;
  'flambda2' )
    flambda2
    ;;
  'stdlib' )
    stdlib
    ;;
  'build' )
    base
    flambda2
    stdlib
    ;;
  'compile' )
    compile "${@:2}"
    ;;
  'ocamlopt' )
    ocamlopt "${@:2}"
    ;;
  'help' )
    echo "usages:"
    echo "### Base commands"
    echo "./flambda2.sh clean                     : git clean -dfx all 4 git repositories"
    echo "./flambda2.sh configure                 : configure all 4 repositories with the correct options"
    echo "./flambda2.sh build                     : build everything"
    echo "### Advanced commands"
    echo "./flambda2.sh clean_tmp                 : clean the tmp files used for logs"
    echo "./flambda2.sh base                      : build the first and second stages"
    echo "./flambda2.sh flambda2                  : build the flambda2 compiler (and override second stage compiler)"
    echo "./flambda2.sh stdlib                    : build the stdlib (and compiler) with the flambda2 compiler"
    echo "### Compile commands"
    echo "./flambda2.sh compile [file] [args..]   : compile the given file with the flambda2 compiler and no stdlib (assumes the flambda2 compiler has been built)"
    echo "./flambda2.sh ocamlopt [file] [args..]  : compile the given file with the flambda2 compiler and flambda2-built stdlib (assumes the compiler and stdlib have been build)"
    ;;
  *)
    echo "available commands are: help, clean, clean_tmp, configure, build, base, flambda2, stdlib, compiler, ocamlopt"
    echo "see ./flambda2.sh help"
    ;;
esac
# finaly, cleanup the tmpfile
rm -rf $TMPFILE


