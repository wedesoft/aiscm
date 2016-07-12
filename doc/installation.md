# Installation

## Requirements

You need to install [Guile][1] and its development headers. You need at least version 2.0.9
which supports calling foreign functions with up to 127 arguments.

## Installation

### With binaries

There are [64-bit binaries for Debian Jessie][2].

To install the software, you have to add the AIscm repository to your list of repositories and install via apt-get. To do this, paste the following lines into your terminal:

```
echo 'deb http://download.opensuse.org/repositories/home:/wedesoft/Debian_8.0/ /' | sudo tee /etc/apt/sources.list.d/aiscm.list
wget -qO- http://download.opensuse.org/repositories/home:wedesoft/Debian_8.0/Release.key | sudo apt-key add -
sudo apt-get update
sudo apt-get install aiscm
```

### Compile from source

#### Install dependencies

First you need to install the dependencies:

```
sudo apt-get install -q --yes build-essential autoconf automake libtool devscripts equivs
sudo mk-build-deps --install --remove --tool 'apt-get -q --yes' debian/control
```

#### Get the source code

You can download the latest release like this

```
wget `curl -s https://api.github.com/repos/wedesoft/aiscm/releases/latest | grep download_url | cut -d '"' -f 4`
```

#### Build AIscm

```
tar xJf aiscm-*.tar.xz
cd aiscm-*
./configure
make -j
sudo make install
```

#### Testing

You can run the test suite like this:

```
make check -j
```

[1]: http://www.gnu.org/software/guile/
[2]: http://software.opensuse.org/download.html?project=home%3Awedesoft&package=aiscm
