# Installation

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
wget -q https://storage.googleapis.com/tensorflow/versions/2.18.0/libtensorflow-cpu-linux-x86_64.tar.gz
sudo tar xz -C /usr/local -f libtensorflow-cpu-linux-x86_64.tar.gz
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
