build_on_linux () {
  ocaml setup.ml -build
}

build_on_osx () {
  env SQLITE3_OCAML_BREWCHECK=1 ocaml setup.ml -build
}

# install oasis to get the rest of the dependencies for the build
eval `opam config env`
export OPAMYES="true"
opam install oasis

echo building
ocaml setup.ml -configure --prefix prefix

case $TRAVIS_OS_NAME in
  osx) build_on_osx ;;
  linux) build_on_linux ;;
  *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
esac

echo building tests
ocaml setup.ml -configure --enable-tests
case $TRAVIS_OS_NAME in
  osx) build_on_osx ;;
  linux) build_on_linux ;;
  *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
esac

ocaml setup.ml -test
