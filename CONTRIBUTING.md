# Contributing

Pull Requests are gladly accepted. Feel free to file an Issue to make sure that
we agree on your approach before you spend time coding it.

Please don't edit the file `ob-async.el` directly - it's tangled from
`ob-async.org`.  `ob-async-org-babel-execute-src-block` is defined under the
heading
[Definition](https://github.com/astahlman/ob-async/blob/master/ob-async.org#definition).

## Testing

Any PR that changes the behavior of ob-async must also update the tests. The acceptance tests are under the heading [Acceptance Tests](https://github.com/astahlman/ob-async/blob/master/ob-async.org#acceptance-tests).

The Makefile includes a `test` target, which is the same target that TravisCI will run against your PR.
