{-# LANGUAGE CPP, GHCForeignImportPrim, MagicHash, UnboxedTuples,UnliftedFFITypes, MultiParamTypeClasses, ExistentialQuantification, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Control.Concurrent.Fiber where

import Control.Applicative
import Control.Monad.IO.Class

import GHC.Base
import GHC.Conc.Sync hiding (yield)
import Data.Function

import System.IO.Unsafe
import Data.IORef
import Control.Concurrent(threadDelay)
import Control.Concurrent.MVar
-- import qualified Data.Map as M 
import Data.Typeable
-- import qualified Data.ByteString.Char8 as BS
import Data.Monoid hiding (Any)
import Unsafe.Coerce
import Control.Exception hiding (onException)
import GHC.IO (trampolineIO)


import Data.IORef
import Debug.Trace
x !> y=  trace (show y) x
infixr 0 !>

#ifdef ETA_VERSION


foreign import prim "eta.fibers.PrimOps.yieldFiber"
   yieldFiber# :: Int# -> Any -> State# s -> State# s

yield= callCC $ \k ->  liftIO $ yieldFiber (runFiber $ k ())
    where 
    yieldFiber k= IO $ \s -> case yieldFiber# 0# (unsafeCoerce# k) s of s' -> (# s', () #)

block=  callCC $ \k -> liftIO $ blockFiber (runFiber $ k ()) 
     where
     blockFiber k= IO $ \s -> case yieldFiber# 1# (unsafeCoerce# k) s of s' -> (# s', () #)

data FiberId= FiberId ThreadId# 

forkFiber :: MonadIO m => Fiber () -> m FiberId
forkFiber f = liftIO $ IO $ \s -> case fork# (runFiber f) s of (# s', tid #) -> (# s', FiberId tid #)



-- trampolineIO :: IO a -> IO a
-- trampolineIO (IO m) = IO $ \s -> case trampoline# (unsafeCoerce# (m s)) of (# a #) -> (# freshStateToken# a, unsafeCoerce# a #)

-- foreign import prim "eta.runtime.stg.Stg.trampoline"
--     trampoline# :: Any -> (# Any #)

#else
forkFiber f = liftIO  $ forkIO $ runFiber f `catch` \Empty -> return ()
yield= return()
trampolineIO= id
#endif

-- Type coercion is necessary because continuations can only be modeled fully within Indexed monads. 
-- See paper P. Wadler "Monads and composable continuations" 
-- The symtom of that problem in the typical continuation monad is an extra parameter r that complicates reasoning
-- This monad eliminates the extra parameter by coercing types since, by construction, the contination parameter is of the
--  type of the result of the first term of the bind.
ety :: a -> b 
ety=   dontWorryEverithingisOk
tdyn :: a -> Dyn
tdyn=  dontWorryEverithingisOk
fdyn :: Dyn -> a
fdyn = dontWorryEverithingisOk

dontWorryEverithingisOk= unsafeCoerce

type Dyn= ()

data Fiber  a = Fiber { runFiberC :: (Dyn -> IO a) -> IO a }

unFiber= unIO . runFiber
 where unIO (IO x)= x


instance   Monad Fiber  where
    return  = pure
    m >>= k  = Fiber $ \c -> ety $ runFiberC m (\x -> ety $ runFiberC ( k $ fdyn x) c)


-- instance MonadState  EventF Fiber where
--     get= lift get
--     put= lift . put

-- instance MonadTrans (Fiber ) where
--      lift m = Fiber ((unsafeCoerce m) >>=)

instance MonadIO Fiber  where
    liftIO x= Fiber  (ety x >>=)

callCC :: ((a -> Fiber b) -> Fiber a) -> Fiber a
callCC f = Fiber $ \ c -> runFiberC (f (\ x -> Fiber $ \ _ -> ety $ c $ tdyn x)) c

instance Functor Fiber where
    fmap f m = Fiber $ \c -> ety $ runFiberC m $ \ x->   ety c $ f $ fdyn x
                 
instance Semigroup a => Semigroup (Fiber a) where
    (<>) x y = (<>) <$> x <*> y
instance Monoid a => Monoid (Fiber a) where
    mappend x y = mappend <$> x <*> y
    mempty      = return mempty

instance Applicative Fiber  where
    pure a  = Fiber ($  tdyn a)
    --f <*> v = ety $ Fiber $ \ k -> ety $ runFiberC f $ \ g -> ety $ runFiberC v $ \t -> k $ (ety g) t
    f <*> v =   do
          r1 <- liftIO $ newIORef Nothing
          r2 <- liftIO $ newIORef Nothing
          (fparallel r1 r2)  <|> (vparallel  r1 r2)
      where

      fparallel :: IORef (Maybe(a -> b)) -> IORef (Maybe a) -> Fiber b 
      fparallel r1 r2= ety $ Fiber $ \k  -> 
          runFiberC f $ \g -> do
                (liftIO $ writeIORef r1  $ Just (fdyn  g)) !> "f write r1"
                mt <- liftIO $ readIORef r2  !> "f read r2"
                case mt of 
                  Just t -> k $ (fdyn g) t
                  Nothing -> liftIO $ throw  Empty  !> "throwempty"
                  
      vparallel :: IORef (Maybe(a -> b)) -> IORef (Maybe a) -> Fiber b
      vparallel  r1 r2=  ety $ Fiber $ \k  ->  
          runFiberC v $ \t ->  do
                 (liftIO $ writeIORef r2 $ Just (fdyn t)) !> "v write r2"
                 mg <- liftIO $ readIORef r1 !> "v read r1"
                 case mg of 
                   Nothing -> liftIO $ throw  Empty   !> "throwempty"
                   Just g -> ( k $ (ety g) t)  !> "JUST"

data Empty= Empty  deriving Typeable
instance Show Empty where show _= "Empty"
instance Exception Empty

instance Alternative Fiber where
    empty= liftIO $ throw  Empty 
    f <|>  g= callCC $ \k -> do -- liftIO  $ runFiber  (f >>=k)  `catch` \Empty -> runFiber  (g >>=k) 
            
          
            r <- liftIO $ newIORef False
            let io  f cont= runFiber  (f >>= cont' ) 
                    where cont' x= do liftIO $ (writeIORef r True) !> "write" ; cont x
            liftIO $ do
                 io f k `catch` \(Empty) -> do
                    c <- liftIO $ readIORef r 
                    when c $ throw Empty 
                    io g k      
            
            

    -- f <|>  g = callCC $ \k -> do 
    --     x <-  liftIO $ runFiber f `catch` (\Empty -> (runFiber g) !> "RUNFIBERG" )
    --     k x !> "continuation"
              

runFiber :: Fiber a -> IO a 
runFiber x= trampolineIO $ runFiberC x (return . ety id )

inputLoop= getLine >>= \l -> atomically (writeTVar mvline l)  >> inputLoop

no = unsafePerformIO newEmptyMVar

mvline= unsafePerformIO $  newTVarIO ""

-- option :: String -> Fiber  String
-- --option :: [Char] -> Fiber r (StateT t IO) [Char]
-- option s = waitEvents . atomically $ do
--               x <- readTVar mvline
--               if  x== s then  writeTVar mvline "" >> return s else retry               

-- callCC :: ((a -> Fiber r StateIO b) -> Fiber r m a) -> Fiber r m a

async :: IO a -> Fiber a 
async io= callCC $ \ret -> do
            forkFiber $  ( liftIO io >>= ret ) >> return () 
            empty   !> "async empty"

waitEvents :: IO a -> Fiber a
waitEvents io= callCC $ \ret -> do
    loop ret  
    where
    loop ret =  do
            forkFiber $ do 
                      (liftIO io >>=  ret  >> loop ret)
                      return () 
            empty



testReact = do
  -- forkIO' inputLoop
   forkFiber $ liftIO reactLoop
   r <-  (reactOption "hello")  <>  (reactOption "world")
   liftIO $ print r 
        
   liftIO $ takeMVar no


   
   
class AdditionalOperators m where

  -- | Run @m a@ discarding its result before running @m b@.
  (**>)  :: m a -> m b -> m b

  -- | Run @m b@ discarding its result, after the whole task set @m a@ is
  -- done.
  (<**)  :: m a -> m b -> m a

  atEnd' :: m a -> m b -> m a
  atEnd' = (<**)

  -- | Run @m b@ discarding its result, once after each task in @m a@, and
  -- every time that an event happens in @m a@
  (<***) :: m a -> m b -> m a

  atEnd  :: m a -> m b -> m a
  atEnd  = (<***)

instance AdditionalOperators Fiber  where

  -- (**>) :: Fiber a -> Fiber b -> Fiber b
  (**>) f g =  Fiber $ \c -> ety $ runFiberC f (\_ -> ety $ runFiberC  g c)
      

  -- (<***) :: Fiber a -> Fiber b -> Fiber a
  (<***) f g = 
      ety $ Fiber $ \k -> ety $ runFiberC f $ \x ->  ety $ runFiberC g  (\_ -> k x)   
      where 
      f' = callCC $ \c -> g >> c ()

  -- (<**) :: Fiber a -> Fiber b -> Fiber a
  (<**) f g = ety $ Fiber $ \k -> ety $ runFiberC f $ \x ->  ety $ runFiberC g  (\_ -> k x)  
--f >>= g   = Fiber $ \k ->  runFiberC f $ \x -> ety $ runFiberC ( g $ unsafeCoerce x) k
  

infixr 1 <***, <**, **>   


react 
  :: ((eventdata ->  IO response) -> IO ())
  -> IO  response
  -> Fiber eventdata
react setHandler iob= callCC $ \ret -> do
            liftIO $ setHandler $  \x ->  (runFiber $ ret x) >> iob
            empty

reactOption :: String -> Fiber String
reactOption s = do 
         x <- react setCallback (return ())
         if  x /= s then empty else do 
            --   liftIO $ atomically $ writeTVar mvline ""
               return s
  

reactLoop =  do
          x   <- getLine -- atomically $ readTVar mvline
          mbs <- readIORef rcb
          Prelude.mapM (\cb -> cb x)  mbs
          reactLoop

rcb= unsafePerformIO $ newIORef [] 

setCallback :: (String ->  IO ()) -> IO ()
setCallback cb= atomicModifyIORef rcb $ \cbs ->  (reverse $ cb : cbs,())




   

test2= runFiber $ do
  r  <- return 2
--   r' <- liftIO $ return $ r +5
--   r2 <- callCC $ \ret -> do 
--      ret  100
--      liftIO  $ print "HELLO"
--      return 1
  liftIO $ print $ r
--   liftIO $ print $ "world3"

test1=  do 
  setNumCapabilities 1
  forkFiber $ f "hello"
  forkFiber $ f "world"
  forkFiber $ f "world2"
  
  takeMVar mexit     

  where
  f str= do 
     liftIO $ print str
     yield
     f str

test3 = keep $ do
    r<-  ( async $ return "hello " )   <>   
            (async $ return "world" )
    -- r <-  async ( return "hello ") <|> return "world"
    -- r2 <-  async ( return "hello2 ") <|> return "world2"
    
    -- r <- Fiber $ \c -> runFiberC  (return "hello") c
    
    liftIO $ print  r

test= runFiber $ do
   r <- liftIO $ newIORef 0
   sum  r 1000000
 where
 sum r 0= do r <- liftIO $ readIORef r; liftIO $ print r
 sum r x= do
    liftIO $ modifyIORef r $ \v -> v + x 
    sum r $x -1 

mexit= unsafePerformIO $ newEmptyMVar  

keep :: Fiber () -> IO ()
keep mx=   do
    forkFiber $ liftIO $ (runFiber  mx)  `catch` \Empty -> return ()
    takeMVar mexit     




{-- ---------------------------------backtracking ------------------------


data Backtrack b= forall a r c. Backtrack{backtracking :: Maybe b
                                     ,backStack :: [(b ->Fiber  c,c -> Fiber  a)] }
                                     deriving Typeable



-- | Delete all the undo actions registered till now for the given track id.
-- backCut :: (Typeable b, Show b) => b -> Fiber ()
backCut reason=
     delData $ Backtrack (Just reason)  [] 

-- | 'backCut' for the default track; equivalent to @backCut ()@.
undoCut ::  Fiber ()
undoCut = backCut ()

-- | Run the action in the first parameter and register the second parameter as
-- the undo action. On undo ('back') the second parameter is called with the
-- undo track id as argument.
--
onBack :: (Typeable b, Show b) => Fiber a -> ( b -> Fiber a) -> Fiber a
onBack ac back =  do
     -- Backtrack mreason _  <- getData `onNothing` backStateOf (typeof bac) !> "HANDLER1"
    -- r <-ac
    --  case mreason  !> ("mreason",mreason) of
    --               Nothing     -> ac
    --               Just reason -> bac reason
     registerBack  ac back
  
   where
         
   typeof :: (b -> Fiber a) -> b
   typeof = undefined

-- | 'onBack' for the default track; equivalent to @onBack ()@.
onUndo ::  Fiber a -> Fiber a -> Fiber a
onUndo x y= onBack x (\() -> y)



-- | Register an undo action to be executed when backtracking. The first
-- parameter is a "witness" whose data type is used to uniquely identify this
-- backtracking action. The value of the witness parameter is not used.
--
-- registerBack :: (Typeable a, Show a) => (a -> Fiber a) -> a -> Fiber a
registerBack  ac back = callCC $ \k -> do
   md <- getData `asTypeOf` (Just <$> (backStateOf $ typeof back))  !> "HANDLER"
   case md of
        Just (bss@(Backtrack b (bs@((back',_):_)))) ->
          --  when (isNothing b) $ do
          --      addrx  <- addr back'
          --      addrx' <- addr back        -- to avoid duplicate backtracking points
          --      when (addrx /= addrx') $ do return () !> "ADD"; setData $ Backtrack mwit  ( (back,  k):   unsafeCoerce bs)
           setData $  Backtrack b  ( (back,  k):   unsafeCoerce bs)
        Just (Backtrack b []) -> setData $ Backtrack b  [(back , k)]
        Nothing ->  do
           setData $ Backtrack mwit  [  (back , k)] !> "NOTHING"
   ac
  
   where


   typeof :: (b -> Fiber a) -> b
   typeof = undefined
   mwit= Nothing `asTypeOf` (Just $ typeof back)
   addr x = liftIO $ return . hashStableName =<< (makeStableName $! x)


-- registerUndo :: Fiber a -> Fiber a
-- registerUndo f= registerBack ()  f

-- XXX Should we enforce retry of the same track which is being undone? If the
-- user specifies a different track would it make sense?
--
-- | For a given undo track id, stop executing more backtracking actions and
-- resume normal execution in the forward direction. Used inside an undo
-- action.
--
forward :: (Typeable b, Show b) => b -> Fiber ()
forward reason=  do
    Backtrack _ stack <- getData `onNothing`  (backStateOf reason)
    setData $ Backtrack(Nothing `asTypeOf` Just reason)  stack






-- | Start the undo process for the given undo track id. Performs all the undo
-- actions registered till now in reverse order. An undo action can use
-- 'forward' to stop the undo process and resume forward execution. If there
-- are no more undo actions registered execution stops and a 'stop' action is
-- returned.
--
back :: (Typeable b, Show b) => b -> Fiber a
back reason = do
  Backtrack _ cs <- getData  `onNothing`  backStateOf  reason
  let  bs= Backtrack (Just reason) cs
  setData bs
  goBackt bs           
                                                    !>"GOBACK"

  where

  goBackt (Backtrack _ [] )= empty                       !> "END"
  goBackt (Backtrack Nothing _ )= error "goback: no reason"

  goBackt (Backtrack (Just reason) ((handler,cont) : bs))= do
        
        -- setData $ Backtrack (Just reason) $ tail stack
        -- unsafeCoerce $ first reason !>  "GOBACK2"
        x <- unsafeCoerce handler reason                                        -- !> ("RUNCLOSURE",length stack)

        Backtrack mreason _ <- getData `onNothing`  backStateOf  reason
        -- setData $ Backtrack mreason bs
        --                                                          -- !> "END RUNCLOSURE"

        -- case mr of
        --    Nothing -> return empty                                     --  !> "END EXECUTION"
        case mreason of
                  Nothing    -> do 
                         --setData $ Backtrack Nothing bs
                         unsafeCoerce $ cont x                          !> "FORWARD EXEC"
                  justreason -> do
                        setData $ Backtrack justreason bs
                        goBackt $ Backtrack justreason bs              !> ("BACK AGAIN")
                        empty

backStateOf :: (Monad m, Show a, Typeable a) => a -> m (Backtrack a)
backStateOf reason= return $ Backtrack (Nothing `asTypeOf` (Just reason)) []



------ exceptions ---
--
-- | Install an exception handler. Handlers are executed in reverse (i.e. last in, first out) order when such exception happens in the
-- continuation. Note that multiple handlers can be installed for the same exception type.
--
-- The semantic is thus very different than the one of `Control.Exception.Base.onException`
onException :: Exception e => (e -> Fiber  ()) -> Fiber ()
onException exc= return () `onException'` exc


onException' :: Exception e => Fiber a -> (e -> Fiber a) -> Fiber a
onException' mx f= onAnyException mx $ \e ->
    case fromException e of
       Nothing -> return $ error "do nothing,this should not be evaluated"
       Just e'  -> f e'
  where
  --onAnyException :: Fiber a -> (SomeException ->Fiber a) -> Fiber a
  onAnyException mx f= ioexp `onBack` f
    where 
    ioexp = callCC $ \cont -> do
       st <- get
       ioexp' $ runTransState st (mx >>=cont ) `catch` exceptBack st
    
    ioexp' mx= do
      (mx,st') <- liftIO  mx
      put st'
      case mx of
        Nothing -> empty 
        Just x  -> return x
  
exceptBack st = \(e ::SomeException) -> do  -- recursive catch itself
                      return () !> "CATCHHHHHHHHHHHHH" 
                      runTransState st  (back e ) 
                `catch` exceptBack st


  

-- | Delete all the exception handlers registered till now.
cutExceptions :: Fiber ()
cutExceptions= backCut  (undefined :: SomeException)

-- | Use it inside an exception handler. it stop executing any further exception
-- handlers and resume normal execution from this point on.
continue :: Fiber ()
continue = forward (undefined :: SomeException) !> "CONTINUE"

-- | catch an exception in a Fiber block
--
-- The semantic is the same than `catch` but the computation and the exception handler can be multirhreaded
-- catcht1 mx exc= mx' `onBack` exc
--  where
--  mx'= Fiber $ const $do
--   st <- get
--   (mx, st) <- liftIO $ runTransState st mx `catch` exceptBack st
--   put st 
--   return mx


catcht :: Exception e => Fiber a -> (e -> Fiber a) -> Fiber a
catcht mx exc= do
      rpassed <- liftIO $ newIORef False
      sandbox  $ do
         delData $ Backtrack (Just (undefined :: SomeException))  [] 

         r <- onException'  mx $ \e -> do
                  passed <- liftIO $ readIORef rpassed
                  if not passed then unsafeCoerce continue >> exc e  else empty
         liftIO $ writeIORef rpassed True
         return r
         
   where
   sandbox :: Fiber a -> Fiber a
   sandbox  mx= do
     exState <- getData `onNothing` backStateOf (undefined :: SomeException)
     mx   <*** setState exState 

-- | throw an exception in the Fiber monad
throwt :: Exception e => e -> Fiber a
throwt= back . toException


-- * Extensible State: Session Data Management

-- | Same as 'getSData' but with a more general type. If the data is found, a
-- 'Just' value is returned. Otherwise, a 'Nothing' value is returned.
getData :: (MonadState EventF m, Typeable a) => m (Maybe a)
getData = resp
  where resp = do
          list <- gets mfData
          case M.lookup (typeOf $ typeResp resp) list of
            Just x  -> return . Just $ unsafeCoerce x
            Nothing -> return Nothing
        typeResp :: m (Maybe x) -> x
        typeResp = undefined

-- | Retrieve a previously stored data item of the given data type from the
-- monad state. The data type to retrieve is implicitly determined from the
-- requested type context.
-- If the data item is not found, an 'empty' value (a void event) is returned.
-- Remember that an empty value stops the monad computation. If you want to
-- print an error message or a default value in that case, you can use an
-- 'Alternative' composition. For example:
--
-- > getSData <|> error "no data"
-- > getInt = getSData <|> return (0 :: Int)
getSData :: Typeable a => Fiber a
getSData =   Fiber $ const  $ do
        mx <- getData
        case mx of
          Nothing -> empty
          Just x  -> return x

-- | Same as `getSData`
getState :: Typeable a => Fiber a
getState = getSData

-- | 'setData' stores a data item in the monad state which can be retrieved
-- later using 'getData' or 'getSData'. Stored data items are keyed by their
-- data type, and therefore only one item of a given type can be stored. A
-- newtype wrapper can be used to distinguish two data items of the same type.
--
-- @
-- import Control.Monad.IO.Class (liftIO)
-- import Fiber.Base
-- import Data.Typeable
--
-- data Person = Person
--    { name :: String
--    , age :: Int
--    } deriving Typeable
--
-- test = keep $ do
--      setData $ Person "Alberto"  55
--      Person name age <- getSData
--      liftIO $ print (name, age)
-- @
setData :: (MonadState EventF m, Typeable a) => a -> m ()
setData x = modify $ \st -> st { mfData = M.insert t (unsafeCoerce x) (mfData st) }
  where t = typeOf x

-- | Accepts a function that takes the current value of the stored data type
-- and returns the modified value. If the function returns 'Nothing' the value
-- is deleted otherwise updated.
modifyData :: (MonadState EventF m, Typeable a) => (Maybe a -> Maybe a) -> m ()
modifyData f = modify $ \st -> st { mfData = M.alter alterf t (mfData st) }
  where typeResp :: (Maybe a -> b) -> a
        typeResp   = undefined
        t          = typeOf (typeResp f)
        alterf mx  = unsafeCoerce $ f x'
          where x' = case mx of
                       Just x  -> Just $ unsafeCoerce x
                       Nothing -> Nothing

-- | Same as modifyData
modifyState :: (MonadState EventF m, Typeable a) => (Maybe a -> Maybe a) -> m ()
modifyState = modifyData

-- | Same as 'setData'
setState :: (MonadState EventF m, Typeable a) => a -> m ()
setState = setData

-- | Delete the data item of the given type from the monad state.
delData :: (MonadState EventF m, Typeable a) => a -> m ()
delData x = modify $ \st -> st { mfData = M.delete (typeOf x) (mfData st) }

-- | Same as 'delData'
delState :: (MonadState EventF m, Typeable a) => a -> m ()
delState = delData


-- STRefs for the Fiber monad
-}

-- | If the first parameter is 'Nothing' return the second parameter otherwise
-- return the first parameter..
onNothing :: Monad m => m (Maybe b) -> m b -> m b
onNothing iox iox'= do
       mx <- iox
       case mx of
           Just x -> return x
           Nothing -> iox'


-- testBack = do

--    runFiber $ do
--         return () !> "before"
--         r <-  async (print "hello")  `onBack` \s ->  liftIO $ print $ "received: 111"++ s 
--         r <-  async (print "world")  `onBack` \s ->  liftIO $ print $ "received: 222"++ s 

--         back "exception"
--         empty
--    takeMVar no


-- test1= do
--    runFiber $ do
--         return () !> "before"
--         onException $ \(s :: SomeException) -> liftIO $  print $ "received: 111"++ show s 
--         async $ print "$$$$$$$$$$$$"
--         -- r <-  async (print "hello")  `onException'` \(s :: SomeException) -> liftIO $  print $ "received: 111"++ show s 
--         -- r <-  async (print "world")  `onException'` \(s :: SomeException) -> liftIO $  print $ "received: 222"++ show s 
--         liftIO $ print "AFTER"
--         liftIO $ myThreadId >>= print

--         error "exception"
--    takeMVar no

-- testCatch= do
--    runFiber $ do
--         async $ print "hello"
--         error "error" 
--         return ()
--       `catcht` (\(e :: SomeException) -> liftIO $ print $ "RECEIVED " ++ show e)
      
--    takeMVar no