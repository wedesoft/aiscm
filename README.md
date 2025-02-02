```
The _    ___
   / \  |_ _|___  ___ _ __ ___
  / _ \  | |/ __|/ __| '_ ` _ \
 / ___ \ | |\__ \ (__| | | | | |
/_/   \_\___|___/\___|_| |_| |_| extension
```

# [AIscm][1]

[![GPLv3](doc/gplv3.png)](https://www.gnu.org/copyleft/gpl.html)

[**AIscm**][1] is a **[Guile][2] extension for numerical arrays and tensors**.
Performance is achieved by using the LLVM JIT compiler.

![AIscm](doc/aiscm.gif "AIscm")

## Download and installation

See doc/installation.md or [website][1] for installation instructions.

### Creating a Docker container

```
make -f Makefile.docker run
```

Within the `tests` folder, you'll find that all unit tests have already been run; you might also have seen the
respective log output during the Docker build.
Integration tests are not yet completely running within Docker, but you can e.g. run one using:

```Shell
cd tests/integration/
make 2d_array.tmp
```

## Run tests

### Unit tests

You can run all tests like this

```Shell
make check
```

One can use *recheck* to run only the test suites which have not completed successfully:

```Shell
make recheck
```

To run a single test suite, you can delete the *log* file and regenerate it using *make*:

```Shell
cd tests
rm -f test_core.log && make test_core.log
cd ..
```

### Integration tests

Running the integration tests requires a graphical display, keyboard interaction, a camera, and a sound device.

```Shell
make integration
```

One can use *reintegration* to run only the integration tests which have not completed successfully:

```Shell
make reintegration
```

# See also

* [AIscm homepage][1]
* [AIscm installation][6]
* [Debian Package Tracker entry for AIscm][8]
* [Debian sbuild][3] (installed from jessie-backports because of [bug 827315][7])
* [Docker][4]
* [managing Debian packages][5]
* [How to run test headlessly with Xvfb][9]

[1]: http://wedesoft.github.io/aiscm/ "AIscm"
[2]: http://www.gnu.org/software/guile/ "Guile programming language"
[3]: https://wiki.debian.org/sbuild
[4]: https://www.docker.com/
[5]: https://www.debian.org/doc/manuals/developers-reference/ch05.en.html#newpackage
[6]: http://wedesoft.github.io/aiscm/installation.html "AIscm installation"
[7]: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=827315
[8]: https://tracker.debian.org/pkg/aiscm
[9]: http://elementalselenium.com/tips/38-headless
