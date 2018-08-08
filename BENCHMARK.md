# Report on the performance of Warp-fibers

## Benchmark

Warp-fibers is a reimplementation of Warp with eta-fibers, a lightweight cooperative threading model. Compared with the normal IO threads, fiber threads are not bound to capabilities and are thus more suitable for applications like a Web server that has high concurrency demand.

We have performed very basic benchmark on Warp-fibers and Warp with a very simple HelloWorld program. Our benchmark environment is as follows:

- Client: Intel® Xeon® Processor E3 (4 Cores, 4 Threads)
- Server: Intel® Core™ i7-6700HQ (4 Cores, 8 Threads)
- Two machines are in one local network

We use `weighttp` as our tool on the client side for benchmarking. It is used as follows:

```
weighttp -n 10000 -c 1000 -t 10 http://<ip_address>:<port_number>
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

The problem appears more often in the later process of the benchmark, so it looks like a memory leak problem. We haven't tracked it down yet.

Since it is a bug that has accumulating effect, `warp` and `warp-fibers` can't be put into practical use until they are solved.
