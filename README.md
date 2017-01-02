# zookicker

![Zookicker](zookicker-icon.png)

[My #1GAM February backup plan](http://www.cipht.net/2015/03/01/zookicker.html).
Released under the GPL; music is CC-BY-SA; fonts are not mine and are
under either SIL or the Ubuntu font license.

## Building for install

```
opam install tsdl tsdl-image tsdl-mixer
oasis setup
ocaml setup.ml -all
ocaml setup.ml -install
```

## Developing

```
oasis setup -setup-update dynamic
ocaml setup.ml -configure --enable-tests
ocaml setup.ml -all
```

## Screenshot

![Level 2 screenshot](zookicker-level-2.png)
