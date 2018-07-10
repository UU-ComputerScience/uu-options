{-# LANGUAGE TemplateHaskell, FlexibleContexts, NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}
module Options.UU.Demo where
import Data.Lenses.Template
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.Interleaved
import Options.UU.Interleaved
import Data.Monoid
import System.Environment

-- We assume that we store our options in a data type for which we generate lenses

data Prefers  =  Agda | Haskell deriving Show
data Address  =  Address  {  city_ :: String
                          ,  street_ :: String} 
                 deriving Show
data Name     =  Name  {  name_:: String 
                       ,  prefers_:: Prefers
                       ,  ints_ :: [Int]
                       ,  address_ :: Address} 
                 deriving Show

$(deriveLenses ''Name)
$(deriveLenses ''Address)

instance ShowParserType Prefers where
   showType p = " <Agda | Haskell> "

-- The next thing to do is to specify a initial record containing the default values:
defaults = Name  "Atze" Haskell [] 
                 (Address  "Utrecht" 
                           "Princetonplein")

-- Next we define the parser for the options, by specifying for each field what may be specified:

oName =
                 name     `option`   ("name",       pString,      "Name")
            <>   ints     `options`  ("ints",       pNaturalRaw,  "A couple of numbers") 
            <>   prefers  `choose`   [("agda",      Agda,         "in case you prefer Agda")
                                     ,("haskell",   Haskell,      "in case you prefer Haskell")
                                     ] 
            <>   address  `field`
                           (   city     `option`  ("city",   pString, "Home city")  
                           <>  street   `option`  ("street" ,pString, "Home Street" )
                           )
{-
-- | The function `main` may serve as a template for your own option handling. You can also use this module to see what  the effectis  of the various ways of passing options
-- >>> ./Demo -i1 --ints 2 --street=Zandlust -a -nDoaitse -i3 --ints=4 --city=Tynaarlo
--     Name {name_ = "Doaitse", prefers_ = Agda, ints_ = [1,2,3,4], address_ = Address {city_ = "Tynaarlo", street_ = "Zandlust"}}
--
-- >>> ./Demo -i1 --ints 2 --street=Zandlust --name Doaitse -i3 --ints=4 --city=Tynaarlo
--     --name           [Char]         optional  Name
--     --ints           Int            recurring A couple of numbers
--     Choose at least one from(
--     --agda                          required  In case you prefer Agda
--     --haskell                       required  In case you prefer Haskell
--     )
--     --city           [Char]         optional  Home city
--     --street         [Char]         optional  Home Street
--     --
--     --  Correcting steps:
--     --    Inserted  "-a" at position 70 expecting one of ["--agda", "--agda=", "--haskell", "--haskell=", "--ints=", "--ints", "-i", "-h", "-a"]
--     --    Inserted  "\EOT" at position 70 expecting "\EOT"


main  ::IO ()
main = do args  <- getArgs
          case run  defaults oName  (concat (map  (++ "\EOT") args)) of
            Left a        -> case a of
                                   Succes v -> print v
                                   Help   t -> putStrLn t
            Right errors  -> putStrLn errors

-- | The function `demo` can be used from within ghci:
-}

-- >>> demo ["-i2", "--street=Zandlust", "--ints=5", "-nAtze", "--city=Houten", "--agda", "-i3"]
--     Name {name_ = "Atze", prefers_ = Agda, ints_ = [2,5,3], address_ = Address {city_ = "Houten", street_ = "Zandlust"}}
 
demo :: [[Char]] -> IO ()
demo args =  case run  defaults oName  (concat (map  (++ "\EOT") args)) of
                  Left a        -> case a of
                                   Succes v -> print v
                                   Help   t -> putStrLn t
                  Right errors  -> putStr errors
