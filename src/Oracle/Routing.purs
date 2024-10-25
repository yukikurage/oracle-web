module Oracle.Routing where

import Prelude

import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', parse, print, root, segment, string)
import Routing.Duplex.Generic as G
import Routing.Duplex.Parser (RouteError)

data Page = Home | Music String

derive instance Generic Page _

route :: RouteDuplex' Page
route = root $ G.sum
  { "Home": G.noArgs
  , "Music": string segment
  }

toPath :: Page -> String
toPath = print route

fromPath :: String -> RouteError \/ Page
fromPath = parse route
