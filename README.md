<!--
SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>

SPDX-License-Identifier: Apache-2.0
-->

# e9p

Implementation of [9p2000][] file protocol in Erlang

## Goals

- [x] Message parsing
- [ ] Client implementation
    + [x] Establishing connection
    + [ ] Tree walking
    + [ ] IO server implementation for reading/writing files
    + [ ] File/directory creation
    + [ ] File/directory deletion
    + [ ] File stats
- [ ] Server implementation
    + [x] Establishing connection
    + [x] Tree walking
    + [ ] File/directory creation
    + [ ] File/directory deletion
    + [x] File stats
    + [ ] Customisable FS implementations

### Example FS

- [ ] UnixFs - which will simply allow accessing some "real" directory in
  system FS

  **WIP**: Implemented directory reading and file reading.
- [ ] ErlProcFS - which will expose Erlang process tree and other internal data
  via API similar to `procfs` from Linux

## Reasoning

I want to implement `procfs`-like API for Erlang to allow non-Erlang-fluent
operators to navigate through Erlang processes. There is something similar
implemented in [`fuserl`][fuserl], but that uses [`libfuse`][libfuse], which is
[NIF][] implemented. That requires compilation of native code and can be
problematic wrt cross compilation and stuff. On the other hand [9p2000][] is
network protocol that can be implemented fully in Erlang, thus do not require
any additional tools or compilation steps. It can also be accessed remotely if
needed.

[9p2000]: http://ericvh.github.io/9p-rfc/rfc9p2000.html
[fuserl]: https://github.com/tonyrog/fuserl
[libfuse]: https://github.com/libfuse/libfuse
[NIF]: https://www.erlang.org/doc/system/nif.html#

## License

Apache-2.0 with exception to PropEr tests which are GPL-3.0-only. This do not
affect possibility to run this project as dependency of non-GPL projects, as GPL
code is not used in runtime. Unfortunately that workaround is needed due to
PropEr licensing.
