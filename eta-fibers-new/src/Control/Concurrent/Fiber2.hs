{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Control.Concurrent.Fiber where
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
import GHC.Conc
import System.IO.Unsafe
import Data.IORef
import Control.Concurrent.MVar
import qualified Data.Map as M 
import Data.Typeable
import qualified Data.ByteString.Char8 as BS
import Control.Monad.State 
import Data.Monoid
import Unsafe.Coerce
import System.Mem.StableName
import Control.Exception hiding (onException)

import Debug.Trace
x !> y=  trace (show y) x
infixr 0 !>


type SData= ()

data LifeCycle = Alive | Parent | Listener | Dead
  deriving (Eq, Show)

-- | EventF describes the context of a TransientIO computation:
data EventF = EventF
  { mfData      :: M.Map TypeRep SData
    -- ^ State data accessed with get or put operations
  , mfSequence  :: Int
  , threadId    :: ThreadId
  , freeTh      :: Bool
    -- ^ When 'True', threads are not killed using kill primitives

  , parent      :: Maybe EventF
    -- ^ The parent of this thread

  , children    :: MVar [EventF]
    -- ^ Forked child threads, used only when 'freeTh' is 'False'

  , maxThread   :: Maybe (IORef Int)
    -- ^ Maximum number of threads that are allowed to be created

  , labelth     :: IORef (LifeCycle, BS.ByteString)
    -- ^ Label the thread with its lifecycle state and a label string
  , emptyOut    :: Bool
  } deriving Typeable

-- Type coercion is necessary because continuations can only be modeled fully within Indexed monads. 
-- See paper P. Wadler "Monads and composable continuations" 
-- The symtom of that problem in the typical continaution monad is an extra parameter r that complicates reasoning
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

data Cont m a = Cont { runCont :: (Dyn -> m a) -> m a }

    



type StateIO = StateT EventF IO

type Fiber  = Cont  StateIO 

instance   Monad Fiber  where
    return  = pure
    m >>= k  = Cont $ \c -> ety $ runCont m (\x -> ety $ runCont ( k $ fdyn x) c)


instance MonadState  EventF Fiber where
    get= lift get
    put= lift . put

instance MonadTrans Cont where
    lift m = Cont ((unsafeCoerce m) >>=)

instance MonadIO Fiber  where
    liftIO = lift . liftIO

callCC :: ((a -> Cont  m b) -> Cont  m a) -> Cont  m a
callCC f = Cont $ \ c -> runCont (f (\ x -> Cont $ \ _ ->  ety $ c $ tdyn x)) c



instance Functor (Cont  m) where
    fmap f m = Cont $ \c -> ety $ runCont m $ \ x->   ety c $ f $ fdyn x
                 
instance Monoid a => Monoid (Fiber a) where
    mappend x y = mappend <$> x <*> y
    mempty      = return mempty

instance Applicative Fiber  where
    pure a  = Cont ($  tdyn a)
    --f <*> v = ety $ Cont $ \ k -> ety $ runCont f $ \ g -> ety $ runCont v $ \t -> k $ (ety g) t
    f <*> v =   do
          r1 <- liftIO $ newIORef Nothing
          r2 <- liftIO $ newIORef Nothing
          (fparallel r1 r2)  <|> (vparallel  r1 r2)
      where

      fparallel :: IORef (Maybe(a -> b)) -> IORef (Maybe a) -> Fiber b 
      fparallel r1 r2= ety $ Cont $ \k  -> 
          runCont f $ \g -> do
                (liftIO $ writeIORef r1  $ Just (fdyn  g)) !> "f write r1"
                mt <- liftIO $ readIORef r2  !> "f read r2"
                case mt of 
                  Just t -> k $ (fdyn g) t
                  Nothing -> get >>= liftIO . throw . Empty 
                  
      vparallel :: IORef (Maybe(a -> b)) -> IORef (Maybe a) -> Fiber b
      vparallel  r1 r2=  ety $ Cont $ \k  ->  
          runCont v $ \t ->  do
                 (liftIO $ writeIORef r2 $ Just (fdyn t)) !> "v write r2"
                 mg <- liftIO $ readIORef r1 !> "v read r1"
                 case mg of 
                   Nothing -> get >>= liftIO . throw . Empty  
                   Just g ->  k $ (ety g) t 



newtype Empty= Empty EventF deriving Typeable
instance Show Empty where show _= "Empty"
instance Exception Empty

instance  Alternative Fiber where

    empty= get >>= liftIO . throw . Empty 
    f <|> g= callCC $ \k -> do
        --  st <- get
        --  (x,st') <-  liftIO (runFiberState st f `catch` (\(Empty st) -> runFiberState st (g >>= k) ))   
        --  -- liftIO $ io st f k  `catch` (\(Empty st') -> io st' g k)
        --  put st'
        --  k x

         st <- get
         let io st f cont= runFiberState st  (f >>= cont' ) 
                 where cont' x= do modify $ \st ->st{emptyOut=True} ; cont x
         (x,st') <- liftIO $ io st f k `catch` \(Empty st') -> do
                      let c =  emptyOut  st'
                      when c $ throw (Empty st'{emptyOut=False}) 
                      io st' g k
         put st' 
         k x   




emptyEventF :: ThreadId -> IORef (LifeCycle, BS.ByteString) -> MVar [EventF] -> EventF
emptyEventF th label childs =
  EventF { mfData     = mempty
         , mfSequence = 0
         , threadId   = th
         , freeTh     = False
         , parent     = Nothing
         , children   = childs
         , maxThread  = Nothing
         , labelth    = label
         , emptyOut   = False }


-- | Run a transient computation with a default initial state
runFiber :: Fiber a -> IO ( a, EventF)
-- runFiber :: Cont r (StateT EventF IO) r -> IO (Maybe r, EventF)
runFiber t = do
    th     <- myThreadId
    label  <- newIORef $ (Alive, BS.pack "top")
    childs <- newMVar []
    runFiberState (emptyEventF th label childs) t

runFiberState :: EventF -> Fiber  a ->  IO ( a, EventF)
runFiberState st t= runStateT  (runTrans t) st 
  where  
  runTrans :: Fiber a -> StateIO  a
  runTrans t=   runCont  t  (return . ety id )
  
inputLoop= getLine >>= \l -> atomically (writeTVar mvline l)  >> inputLoop

no = unsafePerformIO newEmptyMVar

mvline= unsafePerformIO $  newTVarIO ""

option :: String -> Fiber  String
--option :: [Char] -> Cont r (StateT t IO) [Char]
option s = waitEvents . atomically $ do
              x <- readTVar mvline
              if  x== s then  writeTVar mvline "" >> return s else GHC.Conc.retry               

-- callCC :: ((a -> Cont r StateIO b) -> Cont r m a) -> Cont r m a

async :: IO a -> Fiber a 
async io= callCC $ \ret -> do
            st <-  get
            liftIO $ forkIO $ runFiberState st ( liftIO io >>= ret ) >> return () 
            empty 

waitEvents :: IO a -> Fiber a
--waitEvents :: IO a -> Cont a (StateIO) a
waitEvents io= callCC $ \ret -> do
    st <- get 
    loop ret st 
    where
    loop ret st=  do
            liftIO $ forkIO $ do 
                      runFiberState st  (liftIO io >>=  ret  >> loop ret st)
                      return () 
            empty



 
        
        


   
   
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

instance AdditionalOperators (Cont StateIO)  where

  -- (**>) :: Fiber a -> Fiber b -> Fiber b
  (**>) f g =  Cont $ \c -> ety $ runCont f (\x -> ety $ runCont  g c)
      

  -- (<***) :: Fiber a -> Fiber b -> Fiber a
  (<***) f g = 
      ety $ Cont $ \k -> ety $ runCont f $ \x ->  ety $ runCont g  (\_ -> k x)   
      where 
      f' = callCC $ \c -> g >> c ()

  -- (<**) :: Fiber a -> Fiber b -> Fiber a
  (<**) f g = ety $ Cont $ \k -> ety $ runCont f $ \x ->  ety $ runCont g  (\_ -> k x)  
--f >>= g   = Cont $ \k ->  runCont f $ \x -> ety $ runCont ( g $ unsafeCoerce x) k
  

infixr 1 <***, <**, **>   


react 
  :: ((eventdata ->  IO response) -> IO ())
  -> IO  response
  -> Fiber eventdata
react setHandler iob= callCC $ \ret -> do
            st <- get
            liftIO $ setHandler $  \x ->  (runFiberState st $ ret x) >> iob
            empty

reactOption :: String -> Fiber String
reactOption s = do 
         x <- react (setCallback s) (return ())
         if  x /= s then empty else do 
               return s
  

reactLoop =  do
          x   <- getLine -- atomically $ readTVar mvline
          mbs <- readIORef rcb
          --foldr (<|>) empty $ map (\cb -> cb x) mbs
          mapM (\(n,cb) -> cb x `catch` \(Empty _) -> return())  mbs
          reactLoop

rcb= unsafePerformIO $ newIORef [] 

setCallback :: String -> (String ->  IO ()) -> IO ()
setCallback name cb= atomicModifyIORef rcb $ \cbs ->  (reverse $ (name,cb) : cbs,())

delCallback name= atomicModifyIORef rcb $ \cbs -> (filter ((/=) name . fst ) cbs,())


----------------------------------backtracking ------------------------


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
{-# NOINLINE onBack #-}
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
--{-# NOINLINE registerUndo #-}
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
       ioexp' $ runFiberState st (mx >>=cont ) `catch` exceptBack st
    
    ioexp' mx= do
      (mx,st') <- liftIO  mx
      put st'
      case mx of
        Nothing -> empty 
        Just x  -> return x
  
exceptBack st = \(e ::SomeException) ->   -- recursive catch itself
                      runFiberState st  (back e ) 
                `catch` exceptBack st


  

-- | Delete all the exception handlers registered till now.
cutExceptions :: Fiber ()
cutExceptions= backCut  (undefined :: SomeException)

-- | Use it inside an exception handler. it stop executing any further exception
-- handlers and resume normal execution from this point on.
continue :: Fiber ()
continue = forward (undefined :: SomeException) !> "CONTINUE"

-- | catch an exception in a Cont block
--
-- The semantic is the same than `catch` but the computation and the exception handler can be multirhreaded
-- catcht1 mx exc= mx' `onBack` exc
--  where
--  mx'= Cont $ const $do
--   st <- get
--   (mx, st) <- liftIO $ runFiberState st mx `catch` exceptBack st
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

-- | throw an exception in the Cont monad
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
getSData =   Cont $ const  $ do
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
-- import Cont.Base
-- import Data.Typeable
--
-- data Person = Person
--    { name :: String
--    , age :: Int
--    } deriving Typeable
--
-- main = keep $ do
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


-- STRefs for the Cont monad


-- | If the first parameter is 'Nothing' return the second parameter otherwise
-- return the first parameter..
onNothing :: Monad m => m (Maybe b) -> m b -> m b
onNothing iox iox'= do
       mx <- iox
       case mx of
           Just x -> return x
           Nothing -> iox'


testBack = do

   runFiber $ do
        return () !> "before"
        r <-  async (print "hello")  `onBack` \s ->  liftIO $ print $ "received: 111"++ s 
        r <-  async (print "world")  `onBack` \s ->  liftIO $ print $ "received: 222"++ s 

        back "exception"
        empty
   takeMVar no


testException= do
   runFiber $ do
        return () !> "before"
        onException $ \(s :: SomeException) -> liftIO $  print $ "received: 111"++ show s 
        async $ print "$$$$$$$$$$$$"
        -- r <-  async (print "hello")  `onException'` \(s :: SomeException) -> liftIO $  print $ "received: 111"++ show s 
        -- r <-  async (print "world")  `onException'` \(s :: SomeException) -> liftIO $  print $ "received: 222"++ show s 
        liftIO $ print "AFTER"
        liftIO $ myThreadId >>= print

        error "exception"
   takeMVar no

mainCatch= do
   runFiber $ do
        async $ print "hello"
        error "error" 
        return ()
      `catcht` (\(e :: SomeException) -> liftIO $ print $ "RECEIVED " ++ show e)
      
   takeMVar no


   

callCCTest= runFiber $ do
  r  <- return 2
  r' <- liftIO $ return $ r +5
  r2 <- callCC $ \ret -> do 
     ret  100
     liftIO  $ print "HELLO"
     return 1
  liftIO $ print $ r2
  liftIO $ print $ "world3"


testAlternative= keep $ do 
     r <- async (return "hello") <|> async (return "world") <|> async (return "world2")
     liftIO $ print r

mainReact =  do
      -- forkIO inputLoop
       forkIO reactLoop
       keep $ do
            r <-  (reactOption "hello")  <|>  (reactOption "world")
            liftIO $ print r

main3= keep $ do
    -- r<- async ( return "hello") <***  liftIO (print "world")
    r <-  ( async (threadDelay 10000 >> return "hello ")  <>  return "world"     )  <|> return "world2"
    -- r <- Cont $ \c -> runCont  (return "hello") c
    liftIO $ putStrLn  r

mexit= unsafePerformIO $ newEmptyMVar  
keep mx= do
   forkIO $( runFiber mx  >> return ()) `catch` \(Empty _) -> return ()
   takeMVar mexit   
   
options=do
   forkIO $ inputLoop
   keep $ do 
    r <- option "hello"  <|> option "world" 
    liftIO $ print r 

main= testAlternative 

looptest= runFiber $ do
    setState "hello"
    r <- liftIO $ newIORef 0
    sum  r 1000000
    s <-   getState 
    liftIO $ putStrLn s
  where
  sum r 0= do r <- liftIO $ readIORef r; liftIO $ print r
  sum r x= do
     liftIO $ modifyIORef r $ \v -> v + x 
     sum r $x -1 