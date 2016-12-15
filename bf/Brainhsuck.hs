-- Suck your brain

import Data.Char
import Control.Monad
import Control.Applicative

--
--
--

type Instruction = Char
type Value       = Int

data Tape a    = Tape [a] a [a]
               deriving(Show)

tTape :: [a] -> Tape a
tTape []       = error "Your brain sucks."
tTape (x : xs) = Tape [] x xs

tGet :: Tape a -> a
tGet (Tape prev x next) = x

tSet :: Tape a -> a -> Tape a
tSet (Tape prev _ next) x = Tape prev x next

tNext :: Tape a -> Tape a
tNext (Tape _ _ [])          = error "Reached end of Tape!"
tNext (Tape prev x (n : ns)) = Tape (x : prev) n ns

tPrev :: Tape a -> Tape a
tPrev (Tape [] _ _)          = error "Reached end of Tape!"
tPrev (Tape (p : ps) x next) = Tape ps p (x : next)

tMap :: Tape a -> (a -> a) -> Tape a
tMap (Tape prev x next) f = Tape prev (f x) next

--
--
--

type Code = String

data Unit a = Unit a
            | Block [Unit a]
            deriving(Show, Eq)

data ParseResult c a = Fail
                     | Result a [c]
                     deriving(Show)

data Parser c a = P ([c] -> ParseResult c a)

instance Functor (Parser c) where
  fmap = liftM

instance Applicative (Parser c) where
  pure a = P (\i -> Result a i)
  (<*>)  = ap

instance Monad (Parser c) where
  return    = pure
  (>>=) p q = P bind
    where
      bind i =
        case r of
          Fail        -> Fail
          Result r i' -> parse (q r) i'
        where
          r = parse p i

instance Alternative (Parser c) where
  empty     = P (\i -> Fail)
  (<|>) p q = P f
    where
      f i =
        case r of
          Result _ _ -> r
          Fail       -> parse q i
        where
          r = parse p i

instance MonadPlus (Parser c) where
  mzero = empty
  mplus = (<|>)

infixl 1 ||>
(||>) :: Parser c [a] -> Parser a b -> Parser c b
(||>) p q = P f
  where
    f i = Result (pExtract (parse q (pExtract r))) (pPass r)
      where
        r = parse p i

parse :: Parser c a -> ([c] -> ParseResult c a)
parse (P p) = p

pMany :: Parser c a -> Parser c [a]
pMany p = q <|> return []
  where
    q =
      do
        x  <- p
        xs <- pMany p
        return (x : xs)

pOne :: Parser a a
pOne = P (\i ->
  case i of
    (c : cs)  -> Result c cs
    otherwise -> Fail
  )

pCond :: (a -> Bool) -> Parser a a
pCond p = f
  where
    f =
      do
        c <- pOne
        if p c then return c else empty

pIs :: Eq a => a -> Parser a a
pIs c = pCond ((==) c)

pIn :: Eq a => [a] -> Parser a a
pIn s = foldl (<|>) empty (map pIs s)

pBlock :: Parser a a -> Parser a a -> Parser a a -> Parser a [a]
pBlock op cp p =
  do
    op
    r <- pMany match >>= return . concat
    cp
    return r
  where
    match = (p >>= return . return) <|> pBlockR op cp p

pBlockR :: Parser a a -> Parser a a -> Parser a a -> Parser a [a]
pBlockR op cp p =
  do
    o <- op
    r <- pMany match >>= return . concat
    c <- cp
    return ((o : r) ++ [c])
  where
    match = (p >>= return . return) <|> pBlockR op cp p

pLevel :: Parser a a -> Parser a [a] -> Parser a [Unit a]
pLevel p q = pMany ((q >>= pb) <|> (p >>= return . Unit))
  where
    pb = (\i ->
      (return . Block . pExtract) (parse (pLevel p q) i)
      )

pSubsitude :: Parser a a -> b -> Parser a b
pSubsitude p s = p >> return s

pDictionary :: [(Parser a a, b)] -> Parser a b
pDictionary ss =
  foldl (<|>) empty (map (\(a, b) -> pSubsitude a b) ss)

pExtract :: ParseResult c a -> a
pExtract Fail         = error "Parse error."
pExtract (Result x _) = x

pPass :: ParseResult c a -> [c]
pPass Fail         = error "Parse error."
pPass (Result _ x) = x

--
--
--

type State = Tape Value

type Execute = State -> IO State

eChain :: [Execute] -> Execute
eChain = foldl (>=>) return

eWelcome :: Execute
eWelcome = \s ->
      putStrLn "Suck your brain!"
  >>  return s

eRight :: Execute
eRight = \s -> return (tNext s)

eLeft :: Execute
eLeft = \s -> return (tPrev s)

ePlus :: Execute
ePlus = \s -> return (tMap s (+1))

eMinus :: Execute
eMinus = \s -> return (tMap s (+ (-1)))

ePut :: Execute
ePut = \s ->
      putChar (chr (tGet s))
  >>  return s

eFetch :: Execute
eFetch = e
  where
    e s =
      do
        c <- getChar
        return (tSet s (ord c))

eBlock :: [Execute] -> Execute
eBlock es = e
  where
    e, f :: Execute
    e s =
      do
        if tGet s == 0
          then return s
          else f s
    f s =
      do
        s' <- eChain es s
        if tGet s == 0
          then return s'
          else e s'

--
--
--

bfAll :: [Char]
bfAll = "<.]>[+,-"

bfNonBrackets :: [Char]
bfNonBrackets = "<.>+,-"

bfInstructions :: [(Parser (Unit Char) (Unit Char), Execute)]
bfInstructions = [ (pIs (Unit '.'), ePut)
                 , (pIs (Unit ','), eFetch)
                 , (pIs (Unit '>'), eRight)
                 , (pIs (Unit '<'), eLeft)
                 , (pIs (Unit '+'), ePlus)
                 , (pIs (Unit '-'), eMinus)
                 ]

bfBlock :: Parser (Unit Char) Execute
bfBlock = p
  where
    p =
      do
        u <- pOne
        case u of
          Unit _   -> empty
          Block us -> return (eBlock (pExtract (parse pBfFull us)))

pBfBlock :: Parser Char [Char]
pBfBlock = pBlock (pIs '[') (pIs ']') (pIn bfNonBrackets)

pBfLevel :: Parser Char [Unit Char]
pBfLevel = pLevel (pIn bfNonBrackets) pBfBlock

pBfFull :: Parser (Unit Char) [Execute]
pBfFull = pMany (pDictionary bfInstructions <|> bfBlock)

bfRun :: [Char] -> Execute
bfRun = (\i ->
  eChain (
    pExtract (
      parse (pBfLevel ||> pBfFull) (filter (flip elem bfAll) i))
  ))

--
--
--

main :: IO ()
main =
  do
    putStrLn "Ready :"
    l <- getLine
    if null l then return () else
      do
        bfRun l tInfinite
        putStrLn ""
        putStrLn "Finished."
        main
  where
    tInfinite = tTape (repeat 0)
