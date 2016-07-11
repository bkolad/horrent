{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable,
RankNTypes, DeriveTraversable, GADTs #-}

module  LensTut where

import Control.Lens
--import Control.Lens.Prism
import qualified Data.ByteString.Char8 as BC
import qualified Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8
import Data.Either
import Control.Applicative
import Types
import qualified Data.ByteString as B
import qualified Data.Map as Map



data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

data Point = Point { _x :: Double, _y :: Double } deriving (Show)

makeLenses ''Atom
makeLenses ''Point

pointP = Point 22 33

shiftPoint :: Point -> Point
shiftPoint = over x (+99)

shiftAtomX :: Atom -> Atom
shiftAtomX atom = over (point . x) (+ 1) atom

atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }

data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)

makeLenses ''Molecule

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)

data D = D1 {v :: Int}
       | D2 {b :: String}


dd = D1 33

data FooBarBaz
  = Foo {_q :: Int}
  | Bar FooBarBaz
  | Baz {_r :: Int, _k :: FooBarBaz}
  deriving Show

makeLenses ''FooBarBaz
makePrisms ''FooBarBaz

kk = over (k . q)  (+1) (Baz 8 (Foo 99))
jj = over (_Foo) (+99) (Baz 8 (Foo 99))

data Tree a = EmptyT | Leaf a | Node (Tree a) a (Tree a)
    deriving (Functor, Foldable, Show)



instance Traversable Tree where
   traverse f EmptyT = pure EmptyT
   traverse f (Leaf x) = Leaf <$> f x
   traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r


tree = Node (Leaf 22) 99 (Node (Leaf 55) (-99) (EmptyT))

gg = over traverse (+99) tree


data BStringT = BString BC.ByteString
    deriving (Eq, Show)

data BEncode = BStr BC.ByteString
             | BInt Int
             | BList [BEncode]
             | BDic (Map.Map BC.ByteString BEncode)
             deriving (Eq, Show)

--makeLenses ''BEncode
makePrisms ''BEncode

-- b = BC.ByteString t = BEncode  s = Bencode  a =BC.ByteString
--prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b

myP :: Prism' BEncode BC.ByteString
myP = prism BStr f
    where
        f (BStr s) = Right s
        f l = Left l


key :: BC.ByteString -> Prism' BEncode BEncode
key k = prism id f
    where
        f dc@(BDic d) =
            case Map.lookup k d of
                Just v -> Right v
                Nothing -> Left dc
        f e = Left e


num::P.Parser String
num = many1 digit

bInt::P.Parser BEncode
bInt = (BInt . read) <$> (char 'i' *> num <* char 'e')

bString :: P.Parser BC.ByteString
bString = do n <- num
             _ <- char ':'
             P.take $ read n

bList :: P.Parser BEncode
bList = (BList) <$> (char 'l' *> (many1 (bInt <|> (BStr <$> bString) <|> bList <|> bDic)) <* char 'e')


dicEntry :: P.Parser (BC.ByteString, BEncode)
dicEntry = ((,)<$>bString <*> bencodParser)

bDic :: P.Parser BEncode
bDic = (BDic . insert) <$>(char 'd' *> many1 dicEntry <* char 'e')
    where
        insert = foldl (\acc (k, v) -> Map.insert k v acc) Map.empty


bencodParser :: P.Parser BEncode
bencodParser = bInt <|> (BStr <$> bString) <|> bList <|> bDic



torrent = "ub222.torrent"

parseFromFile :: String ->  ExceptT String IO BEncode
parseFromFile path = do content <- liftIO $ B.readFile path
                        liftEither $ P.parseOnly bencodParser content






printer:: IO()
printer = do content <- runExceptT $ parseFromFile torrent
             case content of
                  Left l -> print $ "Problem with reading torrent file" ++ (show l)
                  Right dic -> do
                      print $ dic ^? key (BC.pack "announce") . myP--_BStr
                      print $ dic ^? key (BC.pack "info") . key (BC.pack "length") ._BInt
