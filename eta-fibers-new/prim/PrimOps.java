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

import static eta.runtime.stg.TSO.*;
import eta.runtime.exception.*;
import eta.runtime.thunk.UpdateInfo;

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

    public static Closure catchFiber_(StgContext context, Closure io, Closure handler, Closure after) {
        final TSO tso = context.currentTSO;
        final int exceptionsBlocked = tso.showIfFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        final UpdateInfo ui = tso.updateInfoStack.peek();
        Closure result;
        try {
            result = io.applyV(context);
        } catch (FiberYieldException fye) {
            tso.closure = new CatchFiber(tso.closure, handler, after);
            throw fye;
        } catch (java.lang.Exception e) {
            context.raise = null;
            boolean unmask = false;
            Closure exception = null;
            boolean async = e instanceof EtaAsyncException;
            if (async) {
                exception = ((EtaAsyncException) e).exception;
            } else if (e instanceof EtaException) {
                exception = ((EtaException) e).exception;
            } else if (e instanceof StgException) {
                throw e;
            } else {
                exception = EtaException.convertJavaException(tso, e);
            }
            /* TODO: It seems that there should be more logic as this
                     discards the masking state before the catch.

                     Note that unmasking is not done for asynchronous exceptions.
                     This may be due to the fact that raiseAsync &
                     maybePeformBlockedExceptions only run after unmasking has
                     been set. Verify. -RM */
            if (!async && (exceptionsBlocked & TSO_BLOCKEX) == 0) {
                unmask = true;
            }
            tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
            if ((exceptionsBlocked & (TSO_BLOCKEX | TSO_INTERRUPTIBLE)) == TSO_BLOCKEX) {
                tso.removeFlags(TSO_INTERRUPTIBLE);
            }
            if (async) {
                /* TODO: How should we deal with update frames in async case?

                         GHC's behavior is to save the stack frame, which is impossible
                         for us. Maybe we should do something based on whether the
                         current tso owns the thunk? -RM */
                tso.whatNext = ThreadRun;
            }
            result = handler.apply1V(context, exception);
            if (unmask) {
                eta.runtime.exception.Exception.unmaskAsyncExceptionsRet(context, tso);
            }
            tso.resetStack();
        }
        return after.apply1V(context, result);
    }
}