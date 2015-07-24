# AIscm [![CircleCI](https://img.shields.io/circleci/project/wedesoft/aiscm.png)](https://circleci.com/gh/wedesoft/aiscm) [![GitHub release](https://img.shields.io/github/release/wedesoft/aiscm.svg)](https://github.com/wedesoft/aiscm/releases) [![GitHub license](https://img.shields.io/github/license/wedesoft/aiscm.svg)](https://www.gnu.org/copyleft/gpl.html)

[![Join the chat at https://gitter.im/wedesoft/aiscm](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/wedesoft/aiscm?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[**AIscm**][1] is a **real-time computer vision extension** for the
[**Guile programming language**][2]. Performance in Scheme is achieved by means
of a JIT compiler.

![](construction.gif "Under construction")

## Dependencies

```Shell
sudo aptitude install colorgcc guile-2.0-dev linux-libc-dev libmjpegtools-dev libx11-dev libxext-dev libxv-dev libxmu-dev libxi-dev libglu1-mesa-dev libgl1-mesa-dev libswscale-dev pandoc
```

## Installation

Download [most recent release from Github][4].

```Shell
tar xzf aiscm-*.tar.gz
cd aiscm-*
./configure CC=colorgcc
make -j 4
make check -j 4
sudo make install
```

## See also

* [AIscm homepage][1]
* [AIscm packages for Debian 8.0][3]

## External links

* [Guile manual](http://www.gnu.org/software/guile/manual/)
* [GOOPS: object-oriented extension to Guile](https://www.gnu.org/software/goops/)
* [System V Application Binary Interface](http://www.x86-64.org/documentation/abi.pdf)
* [Intel® 64 and IA-32 Architectures Software Developer Manuals](http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html)

[1]: http://www.wedesoft.de/aiscm/ "AIscm"
[2]: http://www.gnu.org/software/guile/ "Guile"
[3]: http://software.opensuse.org/download.html?project=home%3Awedesoft&package=aiscm
[4]: https://github.com/wedesoft/aiscm/releases
