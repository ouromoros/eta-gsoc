# GSoC 2018 High Performance Web Server with Fibers Final Report

## Pull Requests

A large amount of my work during GSoC consists of testing code and tracking down bugs, some of them lie in previously existing code. Also implementing IO for eta-fibers involves

1. https://github.com/typelead/eta/pull/751 (Merged)
2. https://github.com/typelead/eta-hackage/pull/94 (Merged)
3. https://github.com/typelead/eta-hackage/pull/96 (Merged)
4. https://github.com/typelead/eta-hackage/pull/110 (Merged)
5. https://github.com/agocorona/eta-fibers-new/pull/2 (Open)
6. https://github.com/typelead/eta-hackage/pull/102 (Merged)


While implementing network IO for fibers, there are a certain changes(1) to be made to the runtime system, related with scheduling threads and concurrency problems.

Warp is very big and complex and has a lot of dependencies. Eta is not completely compatible with GHC Haskell, and usually libraries need to be *patched* before they can be compiled in Eta. As a result, I had to submit patches to some packages that were either not patched(2, 6) or had bugs in them(3, 4).

For Web servers, error handling is crucial. And actually most part of a Web server is about handling different kinds of errors. Originally eta-fibers didn't have exception handling. So with some guidance, I implemented exception handling code in eta-fibers-new(5). It is quite challenging, involving a bit of hack, and in fact not entirely satisfactory. But it works.

## Separate Work

All the code is hosted in a github [repositry](https://github.com/ouromoros/eta-gsoc) under my name. and I kept track on my work in the form of pull requests. The work done at different phases are under different Pull Requests:

1. [Phase 1](https://github.com/ouromoros/eta-gsoc/pull/1)
2. [Phase 2.1](https://github.com/ouromoros/eta-gsoc/pull/2)
   [Phase 2.2](https://github.com/ouromoros/eta-gsoc/pull/3)
3. [Phase 3](https://github.com/ouromoros/eta-gsoc/pull/4) (Closed)
   [Phase 3](https://github.com/ouromoros/eta-gsoc/pull/5)

Mainly I have written two libraries, `fibers-network` and `warp-fibers`, and have created two separate github repositry for them.

`fibers-network` implements non-blocking network IO for eta-fibers, and contains most basic functions from `network`. `warp-fibers` is based on `fibers-network` and is a reimplemntation of Warp in eta-fibers. More details and doc are available in the READMEs in their separate directory.

- [fibers-network](https://github.com/ouromoros/fibers-network)
- [warp-fibers](https://github.com/ouromoros/warp-fibers)

## Benchmark

Warp-fibers is a reimplementation of Warp with eta-fibers, a lightweight cooperative threading model. Compared with the normal IO threads, fiber threads are not bound to capabilities and are thus more suitable for applications like a Web server that has high concurrency demand.

We have performed very basic benchmark on Warp-fibers and Warp with a very simple HelloWorld program. Our benchmark environment is as follows:

- Client: Intel® Xeon® Processor E3 (4 Cores, 4 Threads)
- Server: Intel® Core™ i7-6700HQ (4 Cores, 8 Threads)
- Two machines are in one local network

The HelloWorld program can be found under `/warp-demo` and `/warp-fibers-demo` in the [repositry](https://github.com/ouromoros/eta-gsoc). Anyone who wish to reproduce can clone the whole repositry and run `etlas run warp-demo` or `etlas run warp-fibers-demo` to start the server for `warp` or `warp-fibers`.

We use `weighttp` as our tool on the client side for benchmarking as follows:

```
weighttp -n 10000 -c 1000 -t 10 http://127.0.0.1:3000
```

The result for `warp-fibers`:

```
finished in 8 sec, 220 millisec and 319 microsec, 1216 req/s, 184 kbyte/s
requests: 10000 total, 10000 started, 10000 done, 9993 succeeded, 7 failed, 0 errored
status codes: 9993 2xx, 0 3xx, 0 4xx, 0 5xx
traffic: 1548915 bytes total, 1309083 bytes http, 239832 bytes data
```

The result for `warp`:

```
finished in 15 sec, 828 millisec and 503 microsec, 631 req/s, 95 kbyte/s
requests: 10000 total, 10000 started, 10000 done, 9996 succeeded, 4 failed, 0 errored
status codes: 9996 2xx, 0 3xx, 0 4xx, 0 5xx
traffic: 1549537 bytes total, 1309633 bytes http, 239904 bytes data
```

As is obvious, `warp-fibers` has almost double the performance of `warp`. This is a sign that the implementation is quite successful. And because of the nature of `warp-fibers`'s non-blocking threads, the gap between them can potentially go wider if there are requests at a larger scale. (And if more cores are available)

## Bugs

One cannot help noticing that there are failed cases in the results of both `warp` and `warp-fibers`. And actually they appear to have some common problem (so not related to concurrency). A typical one looks like this:

```
java.lang.IllegalArgumentException: Cannot dereference 0x1801000: Exceeded higher bound of the address space.
	at eta.runtime.storage.ManagedHeap.throwIllegalAddressException(ManagedHeap.java:103)
	at eta.runtime.storage.ManagedHeap.getBlock(ManagedHeap.java:88)
	at eta.runtime.io.MemoryManager.getBlockSlow(MemoryManager.java:115)
	at eta.runtime.io.MemoryManager.getBlock(MemoryManager.java:106)
	at eta.runtime.io.MemoryManager.getBoundedBuffer(MemoryManager.java:128)
	at eta.runtime.io.MemoryManager.copy(MemoryManager.java:325)
	at eta.runtime.io.MemoryManager.copy(MemoryManager.java:330)
	at eta.base.Utils.c_memcpy(Utils.java:333)
	at warp_fibers.network.wai.handler.warp.Response$sat$9.apply2V(Response.hs)
    ...
```

~~The problem appears more often in the later process of the benchmark, so it looks like a memory leak problem. We haven't tracked it down yet.~~

~~Since it is a bug that has accumulating effect, `warp` and `warp-fibers` can't be put into practical use until they are solved.~~

---

The bug above has been fixed since 0.8.6b1
