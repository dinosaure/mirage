eval `opam config env`
opam depext -uiy mirage
cd ~
git clone -b easy https://github.com/hannesm/mirage-skeleton.git
make -C mirage-skeleton && rm -rf mirage-skeleton
