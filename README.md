AIscm
=====

[**AIscm**][1] is a **real-time computer vision extension** for the
[**Guile programming language**][2].

Requirements
------------

You need to install [Guile][2] and its development headers.

```Shell
sudo aptitude install guile-2.0 guile-2.0-dev
```

For running the tests you need to install *unit-test.scm*. It is part of the
*guile-library* package:

```Shell
sudo aptitude install guile-library
```

Installation
------------

```Shell
./configure
make
sudo make install
```

Usage
-----

```Scheme
(use-modules (oop goops))
(use-modules (system foreign))
(use-modules (rnrs bytevectors))
(use-modules (aiscm malloc))
(make-malloc 1024)
```

External links
--------------

* [Guile manual](http://www.gnu.org/software/guile/manual/)
    * [A sample Guile extension](http://www.gnu.org/software/guile/manual/html\_node/A-Sample-Guile-Extension.html)
    * [Installing site packages](http://www.gnu.org/software/guile/manual/html\_node/Installing-Site-Packages.html)
    * [Autoconf macros](https://www.gnu.org/software/guile/docs/docs-1.8/guile-ref/Autoconf-Macros.htm)
    * [Memory blocks](http://www.gnu.org/software/guile/manual/html\_node/Memory-Blocks.html)
    * [Byte vectors](http://www.gnu.org/software/guile/manual/html_node/Bytevectors.html)
* [GOOPS: object-oriented extension to Guile](https://www.gnu.org/software/goops/)
* [Noweb literate programming](http://www.cs.tufts.edu/~nr/noweb/)
* [Markdown syntax](http://daringfireball.net/projects/markdown/syntax)

[1]: https://github.com/wedesoft/aiscm "AIscm"
[2]: http://www.gnu.org/software/guile/ "Guile"
