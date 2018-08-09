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

All the code is hosted in a github [repo](https://github.com/ouromoros/eta-gsoc) under my name. and I kept track on my work in the form of pull requests. The work done at different phases are under different Pull Requests:

1. [Phase 1](https://github.com/ouromoros/eta-gsoc/pull/1)
2. [Phase 2.1](https://github.com/ouromoros/eta-gsoc/pull/2)
   [Phase 2.2](https://github.com/ouromoros/eta-gsoc/pull/3)
3. [Phase 3](https://github.com/ouromoros/eta-gsoc/pull/4) (Closed)
   [Phase 3](https://github.com/ouromoros/eta-gsoc/pull/5)

Mainly I have written two libraries, `fibers-network` and `warp-fibers`, and have created two separate github repositry for them.

`fibers-network` implements non-blocking network IO for eta-fibers, and contains most basic functions from `network`. `warp-fibers` is based on `fibers-network` and is a reimplemntation of Warp in eta-fibers. More details and doc are available in the READMEs in their separate directory.

- [fibers-network](https://github.com/ouromoros/fibers-network)
- [warp-fibers](https://github.com/ouromoros/warp-fibers)

I have performed some simple benchmark on `warp-fibers` in comparison with `warp`. The details can be found in `BENCHMARK.md` in my main repositry `eta-gsoc`. The [report](https://github.com/ouromoros/eta-gsoc/blob/master/BENCHMARK.md) cam be viewed as a proof the correctness and effectiveness of the work done in this project.
