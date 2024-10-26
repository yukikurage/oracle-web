module Oracle.Routing where

import Prelude

import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', parse, path, print, root, segment, string)
import Routing.Duplex.Generic as G
import Routing.Duplex.Parser (RouteError)

data Page = Home | Music String | Jacket

derive instance Generic Page _

route :: RouteDuplex' Page
route = root $ G.sum
  { "Home": G.noArgs
  , "Jacket": path "jacket" $ G.noArgs
  , "Music": path "lyrics" $ string segment
  }

toPath :: Page -> String
toPath = print route

fromPath :: String -> RouteError \/ Page
fromPath = parse route
