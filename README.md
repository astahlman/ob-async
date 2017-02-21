[![Build Status](https://travis-ci.org/astahlman/ob-async.svg?branch=master)](https://travis-ci.org/astahlman/ob-async)
[![MELPA](https://melpa.org/packages/ob-async-badge.svg)](https://melpa.org/#/ob-async)

`ob-async` enables asynchronous execution of org-babel src blocks,
like this:

![Demo of async sh execution](readme-demo.gif)

`ob-async` isn't tied to src blocks in a specific org-babel
language. Simply add the keyword `:async` to the header-args of any
org-babel src block and invoke `ob-async-org-babel-execute-src-block`.

## Installation

`ob-async` is available in MELPA. If you'd rather install from source,
make sure `ob-async.el` is on your `load-path`, like this.

    (add-to-list 'load-path "$PATH_TO_OB_ASYNC_ROOT_DIR")

Require the package and install `ob-async-org-babel-execute-src-block`
as a ctrl-c ctrl-c hook. Now `ob-async` will handle any source block
which includes `:async` in its header-args.

    (require 'ob-async)
    (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block)

## Development

[Cask](https://github.com/cask/cask) manages dependencies and runs tests. Once Cask is installed, you can `make test`.
