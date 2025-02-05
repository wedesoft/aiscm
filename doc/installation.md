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

You also need the Tensorflow C library (install the GPU version instead if you have a GPU):

```
wget -q https://storage.googleapis.com/tensorflow/libtensorflow/libtensorflow-cpu-linux-x86_64-1.15.0.tar.gz
sudo tar xz -C /usr/local -f libtensorflow-cpu-linux-x86_64-1.15.0.tar.gz
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
