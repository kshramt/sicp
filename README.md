# sicp

SICP in Clojure with `clojure.core.typed`.

[![Build Status](https://travis-ci.org/kshramt/sicp.svg?branch=master)](https://travis-ci.org/kshramt/sicp)

## Test

```bash
# Please use GNU Make 3.82 or newer
make check -j$(nproc)
```

## Find Unsafe Codes

```bash
git grep ignore-with-unchecked-cast
git grep '\^:no-check'
git grep tc-ignore
```

## License

GPL v3
