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

## Download

You can download the current state of the software like this:

```Shell
git clone git@github.com:wedesoft/aiscm.git
cd aiscm
```

## Dependencies

You can install the dependencies as follows:

```Shell
sudo apt-get install -q --yes build-essential autoconf automake libtool devscripts equivs
sudo mk-build-deps --install --remove --tool 'apt-get -q --yes' debian/control
```

You may install OpenCV (higher than 3.4.5) as follows:

```Shell
git clone https://github.com/opencv/opencv.git
git clone https://github.com/opencv/opencv_contrib.git
cd opencv
mkdir build
cd build
cmake -DBUILD_EXAMPLES=ON -DOPENCV_GENERATE_PKGCONFIG=YES -DOPENCV_EXTRA_MODULES_PATH=../../opencv_contrib/modules ../
make -j
sudo make install
```

NOTE: ```-DBUILD_EXAMPLES=ON``` is the easiest way to install Aruco.

NOTE: ```-DOPENCV_GENERATE_PKGCONFIG=YES``` is required by pkg-config, since AIscm detects dependencies with pkg-config.

You may install the latest TensorFlow as follows:

https://www.tensorflow.org/install/lang_c

## Installation

Finally you can install the software as follows

```Shell
./autogen.sh
./configure
make -j
make check -j
sudo make install
cd ..
```

### Installation using Docker

Download the `.tar.xz` and the `.tar.xz.asc` files from the
lastest [release](https://github.com/wedesoft/aiscm/releases) into the root folder of the cloned repository, then rename
the files to `aiscm.tar.gz` and `aiscm.tar.gz.asc`, respectively.

Build the Docker image using the command below, where the second line just display some minimal information about the
image. Note that the Docker build will take *some* time.

```Shell
docker build --tag=aiscm .
docker image ls aiscm
```

Now can can start a shell within your running container using:

```Shell
docker run -w /usr/src/aiscm/pkg/aiscm-{release-number} -it aiscm /bin/bash
# e.g.: docker run -w /usr/src/aiscm/pkg/aiscm-0.18.1 -it aiscm /bin/bash
```

Within the `tests` folder, you'll find that all unit tests have already been run; you might also have seen the
respective log output during `docker build`. Integration tests are not yet completely running within Docker, but you can
e.g. run one using:

```Shell
cd tests/integration/
make 2d_array.tmp
```

See below for more information on running the tests.

## Run tests

### Unit tests

You can run all tests like this

```Shell
make check -j
```

One can use *recheck* to run only the test suites which have not completed successfully:

```Shell
make recheck -j
```

To run a single test suite, you can delete the *log* file and regenerate it using *make*:

```Shell
cd tests
rm -f test_asm.log && make test_asm.log
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
