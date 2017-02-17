`ob-async` enables asynchronous execution of org-babel src blocks,
like this:

    sleep 3s && echo 'Done!'

![Demo of async sh execution](readme-demo.gif)

`ob-async` isn't tied to src blocks in a specifc org-babel
language. Simply add the keyword `:async` to the header-args of any
org-babel src block and invoke `ob-async-org-babel-execute-src-block`.

## Quick Start

`git clone` this repository, then add
`ob-async-org-babel-execute-src-block` as a ctrl-c ctrl-c hook.

    (add-to-list 'load-path "$PATH_TO_OB_ASYNC_ROOT_DIR")
    (require 'ob-async)
    (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block)

## Development

If the version of Emacs that's on your `PATH` isn't the version you
normally use, `cp local.mk{.template,}` and `export EMACS` to your
preferred binary.

For example, my `local.mk` looks like this:

	export EMACS := /Applications/Emacs.app/Contents/MacOS/Emacs

Then run the unit tests with `make test`.
