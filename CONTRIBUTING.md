# Contributing

Pull Requests are gladly accepted. Feel free to file an Issue to make
sure that we agree on your approach before you spend time coding it.

## Testing

Every commit is tested on Travis CI. Any PR that changes the behavior
of ob-async must also update the
[tests](https://github.com/astahlman/ob-async/blob/master/test/ob-async-test.el). The
Makefile includes a `test` target, which is the same target that
TravisCI will run against your PR.

We test against `org-plus-contrib` from the official org archive and
we take `async` from elpa. Rather than build Emacs from source every
time, we use prebuilt binaries for Emacs 24.5+ from [npostavs'
emacs-travis repository](https://github.com/npostavs/emacs-travis/).




