# Warp-fibers

Warp-fibers is a reimplementation of Warp in eta-fibers.

Warp-fibers essentially has the same API with Warp, except a few differences to be noted:

- The supplied functions (`setOn*` functions) should almost always be using `Fiber` monad where `IO` was originally used. This takes into consideration that user might want to perform some non-blocking operations, and you can always `liftIO` normal `IO` monads.
- `Application`, `Request`, `Response` and `StreamingBody` has been redefined to use `Fiber` monad in their type definition, so any use of them should import from `Network.Wai.Handler.Warp.Types` instead of `Network.Wai`.
- Since `Response` has been changed, some utility functions related with `Response` have been added in `Network.Wai.Handler.Warp.ResponseBuilder`, which were originally in `Network.Wai`.

A demo program for `warp-fibers` should look like the following:

```haskell
import Network.Wai.Handler.Warp.ResponseBuilder (responseLBS)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

application _ respond = respond $
  responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

main :: IO ()
main = run 3000 application
```
