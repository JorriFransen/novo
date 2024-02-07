
md install\include
md install\lib

call configure.bat

nmake /f Nmakefile

copy dyncall\*.lib                  install\lib
copy dyncallback\*.lib              install\lib
copy dynload\*.lib                  install\lib

copy dyncall\dyncall.h              install\include
copy dyncall\dyncall_config.h       install\include
copy dyncall\dyncall_macros.h       install\include
copy dyncall\dyncall_signature.h    install\include
copy dyncall\dyncall_types.h        install\include

copy dyncallback\dyncall_callback.h install\include
copy dynload\dynload.h              install\include
