# Installation

## Requirements

You need to install [Guile][1] version 2.0.9 or higher with development headers.

## Installation

### With binaries

There are AMD64 binaries for Debian Jessie (8), Debian Sid (9), Ubuntu Trusty (14.04), and Ubuntu Xenial (16.04)

To install the software, you have to add the AIscm repository to your list of repositories and install via apt-get. To do this, paste the following lines into your terminal:

```
echo "deb https://wedesoft.github.io/aiscm/apt `lsb_release -cs` main" | sudo tee /etc/apt/sources.list.d/aiscm.list
wget -qO- https://wedesoft.github.io/aiscm/apt/pubkey.gpg | sudo apt-key add -
sudo apt-get update
sudo apt-get install aiscm
```

### Compile from source

#### Get the source code

You can download the latest release like this

```
wget `curl -s https://api.github.com/repos/wedesoft/aiscm/releases/latest | grep download_url | cut -d '"' -f 4`
```

#### Install dependencies

You need to install the dependencies:

```
sudo apt-get install -q --yes build-essential autoconf automake libtool
sudo apt-get install guile-2.0-dev linux-libc-dev gettext libmjpegtools-dev imagemagick libmagickcore-dev libpulse-dev libjpeg-dev libx11-dev libxext-dev libxv-dev libxmu-dev libxi-dev libglu1-mesa-dev libgl1-mesa-dev libswscale-dev libavformat-dev libavcodec-dev libavutil-dev pandoc
```

#### Build AIscm

```
tar xJf aiscm-*.tar.xz
cd aiscm-*
./configure
make -j
sudo make install
```

[1]: http://www.gnu.org/software/guile/
