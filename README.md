`ob-async` enables asynchronous execution of org-babel src blocks,
like this:

    sleep 3s && echo 'Done!'

[Demo of async sh execution](readme-demo.gif)

`ob-async` isn't tied to src blocks in a specifc org-babel
language. Simply add the keyword `:async` to the header-args of any
org-babel src block and invoke `org-babel-execute-src-block:async`.

## Quick Start

`git clone` this repository, then add
`org-babel-execute-src-block:async` as a ctrl-c ctrl-c hook.

    (add-to-list 'load-path "$PATH_TO_OB_ASYNC_ROOT_DIR")
    (require 'ob-async)
    (add-to-list 'org-ctrl-c-ctrl-c-hook 'org-babel-execute-src-block:async)

## Development

If the version of Emacs that's on your `PATH` isn't the version you
normally use, edit `configure-local-env.sh` and export `$EMACS` to
your preferred binary.
