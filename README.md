# Setup

## All-one

```shell
. setup.sh
```

## Individually

### Fetch git submodules

```shell
git submodule init
git submodule update --init --recursive
```

## Setup symlinks

Re-run this when new files are added.

```shell
python3 setup.py
```

