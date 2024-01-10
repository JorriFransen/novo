rmdir /s /q build
rmdir /s /q build-vs
rmdir /s /q build-clang

meson setup --unity off build
meson setup --unity off build-vs --backend vs

set CC=clang-cl
set CXX=clang-cl
meson setup  --unity off build-clang
