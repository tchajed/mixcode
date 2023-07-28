# Mixcode

Mixing code in your proof. Features:

1. prettifying commented blocks of code,
2. generating commented blocks of code for functions and struct,
3. generating a WP thoerem statement for functions.

Support for code in [Go](https://go.dev/), proof in [Perennial](https://github.com/mit-pdos/perennial) + [Iris](https://github.com/mit-pdos/perennial) + [Coq](https://coq.inria.fr/).

![Example](https://www.dropbox.com/s/pcfvtv2u2r0lssg/mixproof.png?dl=0)

## Setup

Download `mixcode.el`.

Enable it manually by the following Emacs commands (using <kbd>M-x</kbd>):
```elisp
(load /path/to/mixcode.el)
(mixcode-mode)
```

To enable it automatically in Coq mode, add the following to your `.emacs`:
```elisp
(add-hook 'coq-mode-hook
          (lambda ()
            (load-file "/path/to/mixcode.el")
            (mixcode-mode)))
```

## Usage

### Loading source file

`M-x mixcode-load-file RET file-path`

### Generating commented block of code for function/struct

`M-x mixcode-insert-code RET func/struct-name`

Remember to use `TAB` to see what's loaded and to auto-complete!

It can also generate commented block of code based on line numbers:

`M-x mixcode-insert-code-with-numbers RET line-numbers`

### Generating WP theorem statement (incl. commented block of code)

`M-x mixcode-insert-wp RET funcname`

## Known issues

1. WP generation fails for higher-order function
2. Function signature parsing won't work for multiple lines

## Features wanted

1. Generating representation predicates based on struct definition
2. Including comments for function and struct
3. Diffing (comparing commented code with some Go source)
4. Backward syncing (updating commented code and reflecting changes in real code)
