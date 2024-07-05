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
    + [ ] Tree walking
    + [ ] File/directory creation
    + [ ] File/directory deletion
    + [ ] File stats
    + [ ] Customisable FS implementations

### Example FS

- [ ] "Passthrough" - which will simply allow accessing some "real" directory in
  system FS
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
