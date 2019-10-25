# Installation

## Using binary package

<div class="figure"><img src="package.png" alt=""/></div>

There are AMD64 binaries for
[![Debian](debian.png "Debian")](https://www.debian.org/) Sid (10).

To install the software, you have to add the AIscm repository to your list of repositories and install via *apt-get*.
Note that the Debian package does not include OpenCV and Tensorflow bindings.
To do this, paste the following lines into your terminal:

```
echo "deb https://wedesoft.github.io/aiscm/apt `lsb_release -cs` main" | sudo tee /etc/apt/sources.list.d/aiscm.list
wget -qO- https://wedesoft.github.io/aiscm/apt/pubkey.gpg | sudo apt-key add -
sudo apt-get update
sudo apt-get install aiscm
```

If you wish, you can additionally register the sources with *apt*:

```
echo "deb-src https://wedesoft.github.io/aiscm/apt `lsb_release -cs` main" | sudo tee -a /etc/apt/sources.list.d/aiscm.list
```

## Compile from source

<div class="figure"><img src="source.png" alt=""/></div>

### Get the source code

You can download the latest version of the code like this:

```
git clone https://github.com/wedesoft/aiscm.git
```

### Install dependencies

You need to install the dependencies:

```
@dependencies.sh@
```

You also need

A recent version of Protobuf-C library and compiler:

```
wget -q https://github.com/protocolbuffers/protobuf/releases/download/v3.10.0/protobuf-all-3.10.0.tar.gz
wget -q https://github.com/protobuf-c/protobuf-c/releases/download/v1.3.2/protobuf-c-1.3.2.tar.gz
tar xzf protobuf-all-3.10.0.tar.gz
tar xzf protobuf-c-1.3.2.tar.gz
cd protobuf-3.10.0
./configure
make -j `nproc`
sudo make install
cd ..
cd protobuf-c-1.3.2
./configure
make -j `nproc`
sudo make install
cd ..
```


The Tensorflow C library (install the GPU version instead if you have a GPU):

```
wget -q https://storage.googleapis.com/tensorflow/libtensorflow/libtensorflow-cpu-linux-x86_64-1.14.0.tar.gz
sudo tar xz -C /usr/local -f libtensorflow-cpu-linux-x86_64-1.14.0.tar.gz
```

The OpenCV library:

```
wget -q https://github.com/opencv/opencv/archive/4.1.2.tar.gz -O opencv-4.1.2.tar.gz
wget -q https://github.com/opencv/opencv_contrib/archive/4.1.2.tar.gz -O opencv_contrib-4.1.2.tar.gz
mkdir opencv-4.1.2/build
cd opencv-4.1.2/build
cmake -DCMAKE_INSTALL_PREFIX=/usr -DOPENCV_GENERATE_PKGCONFIG=YES -DOPENCV_EXTRA_MODULES_PATH=../../opencv_contrib-4.1.2/modules ..
make -j `nproc`
sudo make install
cd ../..
```

### Build AIscm

The software then can be build and installed as follows:

```
cd aiscm
./autogen.sh
./configure
make -j `nproc`
sudo make install
```

## Recommendations

It is recommended to enable a REPL with history and colorized output.
*I.e.* install [guile-colorized][1] and then create a file ```~/.guile``` with the following content.

```
(use-modules (ice-9 readline))
(activate-readline)

(use-modules (ice-9 colorized))
(activate-colorized)
```

It is recommended to edit Scheme documents with Vim or Emacs.
Both editors have plugins for parenthesis editing (ParEdit).
The editors also have plugins for rainbow parentheses.

[1]: https://github.com/NalaGinrut/guile-colorized
