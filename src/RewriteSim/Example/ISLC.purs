module RewriteSim.Example.ISLC where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Maybe (Maybe(..), maybe')
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import RewriteSim (mapRule, (%))
import RewriteSim.Example.Common (Expr)
import Type.Proxy (Proxy(..))

example
  :: Tuple String
       { exprs :: Array (String /\ Expr)
       , rules :: Array (Expr -> Maybe Expr)
       }
example = Tuple "ISLC"
  { rules: map (mapRule (show @L) (parse @L >>> maybe' (\_ -> unsafeCrashWith $ "impossible: malformed L: ") snd))
      [ case _ of
          _ -> Nothing
      ]
  , exprs: map (map (map (show @L)))
      [
      ]
  }

subst :: String -> Expr -> Expr -> Expr
subst x a ("var" % [ y % [] ]) | x == y = a
subst x a (l % es) = l % (es # map (subst x a))

data L
  = Lit String
  | Var
  | Lam
  | App
  | Boundary

derive instance Generic L _

instance Show L where
  show x = genericShow x

instance Print L where
  print = genericPrint

instance Parse L where
  parse = genericParse

----------------
-- Print
----------------

type Printer a = a -> String

class Print a where
  print :: Printer a

instance Print String where
  print = identity

----------------
-- GenericPrint
----------------

class GenericPrint a where
  genericPrint' :: Printer a

instance (GenericPrint a, GenericPrint b) => GenericPrint (Sum a b) where
  genericPrint' (Inl a) = genericPrint' a
  genericPrint' (Inr b) = genericPrint' b

instance (IsSymbol name) => GenericPrint (Constructor name NoArguments) where
  genericPrint' (Constructor _) = reflectSymbol (Proxy @name)

instance (IsSymbol name, Print a) => GenericPrint (Constructor name (Argument a)) where
  genericPrint' (Constructor (Argument a)) = reflectSymbol (Proxy @name) <> "\"" <> print a <> "\""

genericPrint :: forall a rep. Generic a rep => GenericPrint rep => Printer a
genericPrint a = genericPrint' @rep (from a)

----------------
-- Parse
----------------

type Parser a = String -> Maybe (String /\ a)

class Parse a where
  parse :: Parser a

instance Parse String where
  parse s = do
    i <- String.indexOf (String.Pattern "\"") s
    let { before, after } = String.splitAt i s
    pure (after /\ before)

----------------
-- GenericParse
----------------

class GenericParse a where
  genericParse' :: Parser a

instance (GenericParse a, GenericParse b) => GenericParse (Sum a b) where
  genericParse' s = (map (map Inl) (genericParse' @a s)) <|> (map (map Inr) (genericParse' @b s))

instance (IsSymbol name) => GenericParse (Constructor name NoArguments) where
  genericParse' s0 = do
    s1 /\ _ <- parseLiteral (reflectSymbol (Proxy @name)) s0
    pure $ s1 /\ Constructor NoArguments

instance (IsSymbol name, Parse a) => GenericParse (Constructor name (Argument a)) where
  genericParse' s0 = do
    s1 /\ _ <- parseLiteral (reflectSymbol (Proxy @name) <> "\"") s0
    s2 /\ a <- map (map Argument) (parse s1)
    pure $ s2 /\ Constructor a

parseLiteral :: String -> Parser String
parseLiteral l s = do
  s' <- String.stripPrefix (String.Pattern l) s
  pure (s' /\ l)

genericParse :: forall a rep. Generic a rep => GenericParse rep => Parser a
genericParse s = map (map to) (genericParse' @rep s)

