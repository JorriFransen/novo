
set build_dir=%1

echo %build_dir%

md %build_dir%\include
md %build_dir%\lib

nmake /f Nmakefile

copy dyncall\*.lib                  %build_dir%\lib
copy dyncallback\*.lib              %build_dir%\lib
copy dynload\*.lib                  %build_dir%\lib

copy dyncall\dyncall.h              %build_dir%\include
copy dyncall\dyncall_config.h       %build_dir%\include
copy dyncall\dyncall_macros.h       %build_dir%\include
copy dyncall\dyncall_signature.h    %build_dir%\include
copy dyncall\dyncall_types.h        %build_dir%\include

copy dyncallback\dyncall_callback.h %build_dir%\include
copy dynload\dynload.h              %build_dir%\include

nmake /f Nmakefile clean
