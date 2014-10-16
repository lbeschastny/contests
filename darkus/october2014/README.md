## Решения участников

Все решения я постарался привести к одному виду, поэтому вернул повороты. 
Для проверки решения в директории каждого участника можно сделать:

    make && make check

Чтобы проверить время выполнения, делаем

    make run

Обратите внимание, что надо экспортировать переменную N, в которой задать входной параметр для программф:

    export N=4

Ну или по-другому в зависимости от шелла.

Чтобы как-то автоматизировать запуск решений:

    ./run.sh

## Результаты:

Сформировать результаты мне помог скрипт:

    ./aggregate_results.pl > results.md

Посмотреть можно [тут](https://github.com/maxim-komar/contests/blob/master/darkus/october2014/results.md)


## Комментарии к решениям:

- решение darkus на N=6 исчерпало 64 GB памяти, потому статус failed
- sloc посчитал ручками (github)

## Версии ПО:

    ~ lsb_release -a
    Distributor ID: LinuxMint
    Description:    Linux Mint 17 Qiana
    Release:    17
    Codename:   qiana

    ~  uname -a
    Linux work 3.13.0-24-generic #47-Ubuntu SMP Fri May 2 23:30:00 UTC 2014 x86_64 x86_64 x86_64 GNU/Linux

    ~  ghc -V
    The Glorious Glasgow Haskell Compilation System, version 7.6.3

    1> erlang:system_info(otp_release).   
    "R16B03"

    ~  gdc-4.8 -v
    Using built-in specs.
    COLLECT_GCC=gdc-4.8
    COLLECT_LTO_WRAPPER=/usr/lib/gcc/x86_64-linux-gnu/4.8/lto-wrapper
    Target: x86_64-linux-gnu
    Configured with: ../src/configure -v --with-pkgversion='Ubuntu 4.8.2-19ubuntu1' --with-bugurl=file:///usr/share/doc/gcc-4.8/README.Bugs --enable-languages=c,c++,java,go,d,fortran,objc,obj-c++ --prefix=/usr --program-suffix=-4.8 --enable-shared --enable-linker-build-id --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --with-gxx-include-dir=/usr/include/c++/4.8 --libdir=/usr/lib --enable-nls --with-sysroot=/ --enable-clocale=gnu --enable-libstdcxx-debug --enable-libstdcxx-time=yes --enable-gnu-unique-object --disable-libmudflap --enable-plugin --with-system-zlib --disable-browser-plugin --enable-java-awt=gtk --enable-gtk-cairo --with-java-home=/usr/lib/jvm/java-1.5.0-gcj-4.8-amd64/jre --enable-java-home --with-jvm-root-dir=/usr/lib/jvm/java-1.5.0-gcj-4.8-amd64 --with-jvm-jar-dir=/usr/lib/jvm-exports/java-1.5.0-gcj-4.8-amd64 --with-arch-directory=amd64 --with-ecj-jar=/usr/share/java/eclipse-ecj.jar --enable-objc-gc --enable-multiarch --disable-werror --with-arch-32=i686 --with-abi=m64 --with-multilib-list=m32,m64,mx32 --with-tune=generic --enable-checking=release --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu
    Thread model: posix
    gcc version 4.8.2 (Ubuntu 4.8.2-19ubuntu1) 

    user=> (clojure-version)
    "1.4.0"

    ~  java -version
    java version "1.7.0_65"
    OpenJDK Runtime Environment (IcedTea 2.5.2) (7u65-2.5.2-3~14.04)
    OpenJDK 64-Bit Server VM (build 24.65-b04, mixed mode)

    ~  cat /proc/cpuinfo | grep 'model name'
    model name  : Intel(R) Core(TM) i3-4130 CPU @ 3.40GHz
    model name  : Intel(R) Core(TM) i3-4130 CPU @ 3.40GHz
    model name  : Intel(R) Core(TM) i3-4130 CPU @ 3.40GHz
    model name  : Intel(R) Core(TM) i3-4130 CPU @ 3.40GHz
