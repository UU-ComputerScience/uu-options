{-# LANGUAGE NoMonomorphismRestriction,
             FlexibleInstances,  
             ScopedTypeVariables,
             RankNTypes,
             FlexibleContexts,
             CPP,
             TemplateHaskell  #-}

module Options.UU.Interleaved (
  required,
  option,
  options,
  optionsl,
  optionsf,
  flag,
  flags,
  field,
  choose,
  change,
  ShowParserType (..),
  pString,
  pBool,
  run,
  OptionResult (..)
  
  ) where
import Data.Functor.Identity
import Control.Applicative.Interleaved
import Control.Monad.State.Class
import Control.Monad.Trans.State.Lazy
import Text.ParserCombinators.UU -- hiding (pSymbol)
import Text.ParserCombinators.UU.BasicInstances 
import Text.ParserCombinators.UU.Utils hiding (lexeme, pSymbol)
import Data.Lenses
import Data.Lenses.Template

-- For a description of how to use these combinators see the accompanying Demo module. 
-- Further information can be found in a Technical report at http://www.cs.uu.nl/research/techreps/UU-CS-2013-005.html

instance Splittable (P (Str Char String Int)) where
   getPure    = getZeroP
   getNonPure = getOneP

{-
pSymbol :: String -> p (Str Char String Int) String
pSymbol [] = pure []
pSymbol (s:ss) = (:) <$> pSym s <*> pSymbol ss
-}

type OptionParser a = P (Str Char String Int) a

type Option a = Gram (P (Str Char String Int)) a

type BaseEntry s d = forall m r b. MonadState s m =>
     (m () -> StateT r Identity b)
     -> d
     -> (Gram (P (Str Char String Int)) (r -> r), [Char])

type Entry s a = ShowParserType a => BaseEntry s  ([Char], P (Str Char String Int) a, String)

type EntryVal  s a = ShowParserType a => BaseEntry s  ([Char], a, String)

type EntryVals s a = ShowParserType a => BaseEntry s  [([Char], a, String)]

class ShowParserType a where
   showType :: OptionParser a -> String

instance ShowParserType a => ShowParserType [a] where
   showType (p :: OptionParser [a]) = let q :: OptionParser a = undefined
                                      in "[" ++ showType q ++ "]"

instance ShowParserType Int where
   showType p = "Int"

instance ShowParserType Char where
   showType p = "Char"

--instance ShowParserType String where
--   showType p = "String"

instance ShowParserType Bool where
   showType p = "Bool"

data OptionResult a = Succes  a
                    | Help    String


lexeme p = p <* pToken "\EOT"

pString = pMunch (/='\EOT')
pBool   = True <$ pToken "True" <|> False <$ pToken "False"

oG   p  a = mkG ((a `alter`) <$> p) 


required_ :: (MonadState a m) 
     =>  (m () -> StateT r Identity b)
     -> ( [Char]
        , OptionParser (a -> a)
        , String
        , String
        , String
        )
     -> (Gram  (P (Str Char String Int))  (r -> r), [Char])

{-
required_ a   (string, p, tp, kind, info) 
            = let align n t = take n (t++repeat ' ')
                  (p', tp') = case ( getNonPure p, getPure p) of
                                      (Nothing, Just pe)  -> (const pe <$> pToken "\EOT", "")
                                      (Just pne, Nothing) -> ((pToken "\EOT" <|> pure "") *> lexeme pne, tp)
                                      (Just pne, Just pe) -> error "An option can not be both empty and non-empty"
                                      (Nothing, Nothing)  -> error "An option should return a value"
              in (       oG ( pToken ("-"   ++ [head string])             *>  p') a
                    <|>  oG ( pToken ("--"  ++ string)                    *>  p') a
                    <|>  oG ( pToken ("--"  ++ string ++ "=")             *>  p') a
                 , "--"++ align 15 string ++ align 15 tp' ++ align 10 kind ++ info ++"\n"
                 )
-}

required_ a   (string, p, tp, kind, info) 
            = let align n t = take n (t++repeat ' ')
                  p'        = case ( getNonPure p, getPure p) of
                                      (Nothing, Just pe)  -> const pe <$> pToken "\EOT"
                                      (Just pne, Nothing) -> (pToken "\EOT" <|> pure "") *> lexeme pne
                                      (Just pne, Just pe) -> error "An option can not be both empty and non-empty"
                                      (Nothing, Nothing)  -> error "An option should return a value"
              in ( oG ((     pToken ("-"   ++ [head string])
                        <|>  pToken ("--"  ++ string) ) *> (pToken "=" `opt` "") *> noDash *> p') a 
                 , "--"++ align 15 string ++ align 15 tp ++ align 10 kind ++ info ++"\n"
                 )

noDash = pure "" -- needs further work

-- | a `required` entry specied an entry which has to be provided; in the record containing the default values one may put `undefined`
required  :: Entry a a

required a   (string, p, info)  = required_ a (string, const <$> p, showType p, "required", info)

-- | an `option` entry specied an entry which may  be provided; if absent the default value is taken

option :: Entry a a
option   a   (string, p, i)      =  let (r, t) = required_  a (string, const <$> p, showType p, "optional", i) 
                                    in  (r <|> pure id, t)

-- | An `options` entry specifies an element which may occur more than once. The final value contains the list of all the values encountered.
options :: Entry [a] a
options  a   (string, p, i)      =  let (pars, text) =   required_ a  ( string
                                                                     , (:) <$> p
                                                                     , showType p
                                                                     , "recurring"
                                                                     , i)
                                    in  (let pm = (.) <$> pars <*> pm <|> pure id in pm,  text)

-- | An `optionl` entry specifies an element which may occur more than once. The last one encountered is taken
-- optionsl :: Entry a a
optionsl  a (string, p, i)  =  let (pars, t) = options a (string, p, i ++"last  one is taken")  in ( (const. last .($[]))  <$> pars, t)


-- | An `optionf` entry specifies an element which may occur more than once. The first one encountered is taken
-- optionsf :: Entry a a
optionsf  a (string, p, i)  =  let (pars, t) = options a (string, p, i ++"first  one is taken") in ( (head .)  <$> pars, t)

-- | A `flag` entry sets   a field to a specific value when encountered
flag :: EntryVal a a
flag     a   (string, v,i)       =  option    a (string, pure v, i)

-- | A `flags` entry introduces a list of possible parameters, each with a value to which the field should be set
flags :: EntryVals a a
flags    a   table               =  foldr (<>) (pure id, "") (map (flag a) table)

-- | A `set` entry introduces a required entry, which sets a spcific value; it is used in `choose` and probably not very useful by itself.
set :: EntryVal a a
set     a   (string, v,i)      =  required_    a (string, pure (const v), "", "required", i)

-- | A `choose` entry introduces a list of choices for the specific entry; precisely one should be given
choose :: EntryVals a a
choose  a   table              =  let (ps, ts) =  unzip (map (set a) table)
                                  in (foldr (<|>) empty ps, "-- choose at least one from \n" ++ concat (map ("   "++) ts))

-- | A `change` entry is an optional `choose` entry
change :: EntryVals a a 
change  a   table              =  let (ps, ts) =  unzip (map (set a) table)
                                  in (foldr (<|>) (pure id) ps, "You may choose one from(\n" ++ concat ts ++ ")\n")

  

-- | A `field` entry introduces a collection of options which are used to set fields in a sub-record of the main record 
field
  :: (Functor f, Control.Monad.State.Class.MonadState a m) =>
     (m ()
      -> Control.Monad.Trans.State.Lazy.StateT
           r Data.Functor.Identity.Identity b)
     -> (f (a -> a), t) -> (f (r -> r), t)
field s opts = let (p, t) = opts in ((s `alter`) <$> p, t)

-- | The function `run` equips the given option specification with an option to ask for @--help@. It concatenates the files coming from the command line and terminates them with an EOT. 
--   Make sure your command line arguments do not contain an EOT. It parses the command line arguments and updates the `default` record passed to it
run ::
     a                                                      -- ^ the record containing the default values
     -> (Gram (P (Str Char String Int)) (a -> a), String)   -- ^ the specification of the various options
     -> String                                              -- ^ The string containing the given options, separated by EOT
     -> Either (OptionResult a) [Char]                      -- ^ The result is either an updated record (`Succes`) with options or a request for `Help`. In case of erroneous input an error message is returned.

run defaults (p, t) inp = do  let r@(a, errors) =  parse ((,) <$> (   Succes <$> (mkP p <*> pure defaults)
                                                                  <|> Help t <$ pToken "--help\EOT"
                                                                  ) 
                                                              <*> pEnd
                                                         ) (createStr 0 inp)
                              if null errors then  Left a
                                             else  Right (t ++ concat (map (++"\n") ("\n--  Correcting steps:":  map show errors)))


