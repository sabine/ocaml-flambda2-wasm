cd ..
./optcompile.sh -dlambda -dcmm wasm-tests/$1/test.ml
cd wasm-tests
cd $1
printf "\n\nwat2wasm:\n"
../../../wabt/build/wat2wasm --enable-gc test.wat

printf "\n\nwasm-interp:\n"
../../../wabt/build/wasm-interp --enable-gc test.wasm
cd ..
