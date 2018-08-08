# GSoC 2018 High Performance Web Server with Fibers Final Report

## Pull Requests

1. https://github.com/typelead/eta/pull/751 (Merged)
2. https://github.com/typelead/eta-hackage/pull/94 (Merged)
3. https://github.com/typelead/eta-hackage/pull/96 (Merged)
4. https://github.com/typelead/eta-hackage/pull/110 (Merged)
5. https://github.com/agocorona/eta-fibers-new/pull/2 (Open)
6. https://github.com/typelead/eta-hackage/pull/102 (Merged)


While implementing network IO for fibers, there are a certain changes(1) to be made to the runtime system, in order to solve concurrency problems and others.

I had to submit patches to some packages that were either not patched(2, 6) or had bugs in them(3, 4).

Also with some guidance, I implemented exception handling code in eta-fibers-new(5). It is quite challenging, involving a bit of hack, and in fact not entirely satisfactory. But it works.

## Separate Work

All the code is hosted in a github [repo](https://github.com/ouromoros/eta-gsoc) under my name. and I kept track on my work in the form of pull requests. The work done at different phases are under different Pull Requests:

1. [Phase 1](https://github.com/ouromoros/eta-gsoc/pull/1)
2. [Phase 2.1](https://github.com/ouromoros/eta-gsoc/pull/2)
   [Phase 2.2](https://github.com/ouromoros/eta-gsoc/pull/3)
3. [Phase 3](https://github.com/ouromoros/eta-gsoc/pull/4) (Closed)
   [Phase 3](https://github.com/ouromoros/eta-gsoc/pull/5)

Mainly I have created two libraries, `fibers-network` and `warp-fibers`, and have created two separate github repositry for them. The README in them should have stated their usage clearly.

- [fibers-network](https://github.com/ouromoros/fibers-network)
- [warp-fibers](https://github.com/ouromoros/warp-fibers)
