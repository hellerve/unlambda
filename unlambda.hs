import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Exception (catchJust, throwIO)
import qualified Data.Maybe
import GHC.IO.Exception
import System.IO.Error
import Control.Monad.Trans.Maybe

infixl 3 ¢
infixl 4 $>

type EvalState = StateT (Maybe Char) IO

data Term = I 
          | K 
          | K2 Term 
          | S 
          | S2 Term 
          | S3 Term Term 
          | V 
          | E 
          | N
          | Readchar 
          | Printchar Char 
          | Compchar Char 
          | Reprint 
          | D 
          | D2 Term
          | C 
          | Callcc Cont 
          | App Term Term 
            deriving (Eq, Show)

data Cont = Exiter 
          | Nil
          | Dcheck Term Cont 
          | Ddelayed Term Cont 
          | Dundelayed Term Cont
              deriving (Eq, Show)

catchEOF :: forall a. IO a -> (() -> IO a) -> IO a
catchEOF = catchJust (guard.isEOFError)

eofError :: IOException
eofError = IOError Nothing EOF "" "" Nothing Nothing

hMaybeChar :: Handle -> IO (Maybe Char)
hMaybeChar h = fmap Just (hGetChar h) `catchEOF` (\_ -> return Nothing)

maybeChar :: IO (Maybe Char)
maybeChar = hMaybeChar stdin

($>) :: Applicative f => f (a -> b) -> a -> f b
f $> a = f <*> pure a

app :: Term -> Term -> Cont -> EvalState (Cont, Term)
app I a c = return (c, a)
app K a c = return (c, K2 a)
app (K2 a) _ c = return (c, a)
app S a c = return (c, S2 a)
app (S2 a) a2 c = return (c, S3 a a2)
app (S3 a a2) a3 c = descend c (App (App a a3) (App a2 a3))
app V _ c = return (c, V)
app E a _ = liftIO $ exitWith (if a == I then ExitSuccess else ExitFailure 1)
app (D2 right) a c = descend (Ddelayed a c) right
app Readchar a c = do
  curchar <- liftIO maybeChar
  put curchar
  descend c (App a $ maybe V (const I) curchar)
app (Printchar char) a c = liftIO (putChar char) >> return (c, a)
app (Compchar char) a c = do
  cchar <- get
  let eq = Data.Maybe.fromMaybe False ((==) <$> cchar $> char)
  descend c (App a (if eq then  I else V))
app Reprint a c = get >>= descend c . App a . maybe V Printchar
app C a c = descend c (App a $ Callcc c)
app (Callcc cont) a _ = return (cont, a)
app D _ _ = error "D: this never happens"
app (App _ _) _ _ = error "App: this never happens"
app N a c = liftIO loop >> return (c, a)

eval :: Cont -> Term -> EvalState (Cont, Term)
eval Exiter a = app E a Exiter
eval Nil a = app N a Exiter
eval (Ddelayed rv k2) lv = app lv rv k2
eval (Dundelayed lv cont) rv = app lv rv cont
eval (Dcheck right cont) D = eval cont (D2 right)
eval (Dcheck right cont) lv = descend (Dundelayed lv cont) right

descend :: Cont -> Term -> EvalState (Cont, Term)
descend cont (App left right) = descend (Dcheck right cont) left
descend cont tree = eval cont tree

run :: Term -> IO ()
run tree = run' start Nothing
    where 
      start = descend Nil tree
      e' = uncurry eval
      run' begin _state = runStateT begin _state >>= \(r,n) -> run' (e' r) n

buildM :: (Functor m) => (Monad m) => m (Maybe Char) -> m (Maybe Term)
buildM charaction = runMaybeT go
    where
      action = MaybeT charaction
      go = do
        c <- action
        case c of
          '`' -> App <$> go <*> go
          '#' -> line
              where line = action >>= (\n -> if n == '\n' then go else line)
          _ -> lookup c one ¢ return <?> (lookup c two ¢ (`fmap` action) <?> go)
      one = [ ('i', I)
            , ('v', V)
            , ('c', C)
            , ('e', E)
            , ('d', D)
            , ('s', S)
            , ('k', K)
            , ('r', Printchar '\n')
            , ('@', Readchar)
            , ('|', Reprint)
            ]
      two = [ ('.', Printchar)
            , ('?', Compchar)
            ]

(¢) :: forall b c. b -> (b -> c) -> c
(¢) = flip ($)

(<?>) :: forall a a1. (a1 -> a) -> a -> Maybe a1 -> a
(<?>) = flip maybe

hBuild :: Handle -> IO Term
hBuild h = buildM (hMaybeChar h) >>= maybe (throwIO eofError) return

printUsage :: IO ()
printUsage = do printVersion
                putStrLn("\nUsage: " ++
                         "\n\twithout arguments  - runs REPL" ++
                         "\n\t-h/--help          - display this help message" ++
                         "\n\nMore information can be found on " ++
                         "https://github.com/hellerve/unlambda")

printVersion :: IO ()
printVersion = putStrLn "unlambda Version 0.1.0"

printCommands :: IO ()
printCommands = putStrLn "Press Ctrl-C to exit interpreter"

main :: IO ()
main = do
        args <- getArgs
        if null args
            then do printVersion
                    printCommands
                    putStrLn ""
                    replinit
            else 
                if(head args ==  "-h") || (head args == "--help") 
                    then printUsage 
                    else exec args

exec :: [FilePath] -> IO ()
exec args = do
        fhandle <- openFile (head args) ReadMode
        hSetEncoding fhandle latin1
        tree <- fmap Just (hBuild fhandle) `catchEOF`
            (\_ -> putStrLn "Error: input too short" >> return Nothing)
        hClose fhandle
        maybe (return ()) run tree

replinit :: IO ()
replinit = do
    hSetEncoding stdin latin1
    loop

loop :: IO ()
loop = do
    putStr "unlambda> "
    hFlush stdout
    tree <- fmap Just (hBuild stdin) `catchEOF`
        (\_ -> putStrLn "Error: input too short" >> return Nothing)
    maybe (return ()) run tree
