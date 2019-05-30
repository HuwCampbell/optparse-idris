-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Run

import Options.Applicative.Types
import Options.Applicative.Maybe
import Control.Monad.State
import Control.Monad.Trans

%access public export

data OptWord : Type where
  ParsedWord :  OptName -> Maybe String -> OptWord

parseWord : String -> Maybe OptWord
parseWord s = case unpack s of
  ('-' :: '-' :: w) => Just $ case span (/= '=') w of
        (_, [])         => ParsedWord (LongName . pack $ w) Nothing
        (w', _ :: rest) => ParsedWord (LongName . pack $ w') $ Just . pack $ rest
  ('-' :: w :: [])  => Just $ ParsedWord (ShortName w) Nothing
  ('-' :: w :: rs)  => Just $ ParsedWord (ShortName w) $ Just (pack rs)
  _                 => Nothing

searchParser : {a : Type} -> Parser a -> ({g : ParamType} -> {r : Type} -> Option g r -> MaybeT (StateT (List String) (Either ParseError)) (Parser r)) -> MaybeT (StateT (List String) (Either ParseError)) (Parser a)
searchParser (NilP x) _ = empty
searchParser (OptP o) f = f o
searchParser (AppP p1 p2) f = (<|>)
  ( (\p1' => p1' <*> p2) <$> searchParser p1 f )
  ( (\p2' => p1 <*> p2') <$> searchParser p2 f )
searchParser (AltP p1 p2) f = (<|>)
  ( searchParser p1 f )
  ( searchParser p2 f )

stepParser : {a : Type} -> Parser a -> String -> MaybeT (StateT (List String) (Either ParseError)) (Parser a)
stepParser p arg = case (parseWord arg) of
  Nothing => searchParser p $ \opt => case opt of
    Opt _ (ArgReader fa _) => lift $ lift $ map pure (fa arg)
    Opt _ (CmdReader ps _) => case lookup arg ps of
      Just sub => pure sub
      Nothing  => empty
    _                      => empty
  Just (ParsedWord w wordVal) => searchParser p $ \opt => case opt of
    Opt _ (FlagReader w' a)    => case elem w w' of
      True  => do
        args <- lift $ ST (\x => pure (x, x))
        let poppedArgs  = maybe [] (\w => ("-" <+> w) :: Nil) wordVal <+> args
        lift $ ST (\y => pure ((), poppedArgs))
        lift . lift . Right . pure $ a
      False => empty
    Opt _ (OptionReader w' fa _) => case elem w w' of
      True  => do
        args <- lift $ ST (\x => pure (x, x))
        let argsWord = maybe [] (:: Nil) wordVal <+> args
        case argsWord of
          (a :: rest) => do
            lift $ ST (\y => pure ((), rest))
            lift . lift . map pure $ fa a
          _ => lift $ lift (Left $ ErrorMsg "Input required after option ")
      False => empty
    _ => empty

evalParser : Parser a -> Maybe a
evalParser (NilP r) = r
evalParser (OptP _) = Nothing
evalParser (AppP p1 p2) = evalParser p1 <*> evalParser p2
evalParser (AltP p1 p2) = evalParser p1 <|> evalParser p2

parseError : String -> ParseError
parseError arg = ErrorMsg msg
  where
    msg = case unpack arg of
      ('-'::_) => "Invalid option `" ++ arg ++ "'"
      _        => "Invalid argument `" ++ arg ++ "'"

total
runParser : Parser a -> List String -> Either ParseError (a, List String)
runParser p Nil = maybeToEither (ErrorMsg "Not enough input") $ map (\p' => (p', Nil)) (evalParser p)
runParser p args@(arg :: argt) = do
  (Just p', args') <- runStateT (runMaybeT $ stepParser p arg) argt
    | _ => maybeToEither (parseError arg) (map (\x' => (x', args)) (evalParser p))
  assert_total $ runParser p' args'

total
runParserFully : Parser a -> List String -> Either ParseError a
runParserFully p ls = do
  (res,leftOver) <- runParser p ls
  case leftOver of
    (un :: _) => Left $ parseError un
    Nil       => Right res

-- --------------------------------------------------------------------- [ EOF ]
