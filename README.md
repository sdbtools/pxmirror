# pxmirror

Void Linux mirror selection implemented in GNU Prolog.

This is a Prolog implementation of [xmirror](https://docs.voidlinux.org/xbps/repositories/mirrors/changing.html#xmirror).

## How to run

### Run precompiled executable.
```sh
wget https://github.com/sdbtools/pxmirror/releases/latest/download/pxmirror.x86_64.tgz
tar -xzf pxmirror.x86_64.tgz
./pxmirror
```

### Run as a script.
```sh
sudo xbps-install gprolog
git clone https://github.com/sdbtools/pxmirror.git
cd pxmirror
./pxmirror.pl
# or
gprolog --consult-file pxmirror.pl
```

### Compile Prolog code locally and run it.
```sh
sudo xbps-install gprolog gcc
git clone https://github.com/sdbtools/pxmirror.git
cd pxmirror
gplc --min-size pxmirror.pl
./pxmirror
```

## Licensing

This software is released under the GNU GPLv2 license.

## Credits

- [Void Linux](https://voidlinux.org/)
 
