package eta.fibers;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

import eta.runtime.apply.Function2;

public class CatchFiber extends Function2 {
    public final Closure body;
    public final Closure handler;
    public final Closure after;

    public CatchFiber(final Closure body, final Closure handler, final Closure after) {
        this.body    = body;
        this.handler = handler;
        this.after   = after;
    }

    @Override
    public Closure applyV(StgContext context) {
        return PrimOps.catchFiber_(context, body, handler, after);
    }
}