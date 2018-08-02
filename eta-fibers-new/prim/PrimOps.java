package eta.fibers;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.TSO;
import eta.runtime.concurrent.Concurrent;
import eta.runtime.concurrent.Fiber;
import eta.runtime.concurrent.MVar;
//import eta.runtime.exception;

import static ghc_prim.ghc.Types.*;
import static eta.runtime.stg.TSO.WhatNext.*;
import static eta.runtime.stg.Closures.*;

public class PrimOps  {
    public static void yieldFiber(StgContext context, int block, Closure cont) {
        TSO tso = context.currentTSO;
        tso.whatNext = (block == 1)? ThreadBlock : ThreadYield;
        tso.closure = cont;
        throw Fiber.yieldException.get();
    }

    public static void addMVarListener(StgContext context, MVar m) {
        m.registerListener(context.currentTSO);
    }

    public static void awakenMVarListeners(StgContext context, MVar m) {
        for (TSO top = m.getListeners(); top != null;) {
            Concurrent.pushToGlobalRunQueue(top);
            TSO oldTop = top;
            top = top.link;
            oldTop.link = null;
        }
    }
}