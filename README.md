# zookicker

My #1GAM February backup plan.

## Building for install

```
oasis setup
ocaml setup.ml -all -install
```

## Developing

```
oasis setup -setup-update dynamic
ocaml setup.ml -configure --enable-tests
ocaml setup.ml -all
```
