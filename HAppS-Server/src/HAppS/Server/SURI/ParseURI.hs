module HAppS.Server.SURI.ParseURI(parseURIRef) where

import qualified Data.ByteString.Internal as BB
import qualified Data.ByteString.Unsafe   as BB
import Data.ByteString.Char8 as BC
import Prelude hiding(break,length,null,drop,splitAt)
import Network.URI

import HAppS.Util.ByteStringCompat

parseURIRef :: ByteString -> URI
parseURIRef fs =
  case break (\c -> ':' == c || '/' == c || '?' == c || '#' == c) fs of
  (initial,rest) ->
      let ui = unpack initial
      in case uncons rest of
         Nothing ->
             if null initial then nullURI -- empty uri
                             else -- uri not containing either ':' or '/'
                                  nullURI { uriPath = ui }
         Just (c, rrest) ->
             case c of
             ':' -> pabsuri   rrest $ URI (unpack initial)
             '/' -> puriref   fs    $ URI "" Nothing
             '?' -> pquery    rrest $ URI "" Nothing ui
             '#' -> pfragment rrest $ URI "" Nothing ui ""
             _   -> error "parseURIRef: Can't happen"

pabsuri fs cont =
  if length fs >= 2 && unsafeHead fs == '/' && unsafeIndex fs 1 == '/'
     then pauthority (drop 2 fs) cont
     else puriref fs $ cont Nothing
pauthority fs cont =
  let (auth,rest) = breakChar '/' fs
  in puriref rest $! cont (Just $! pauthinner auth)
pauthinner fs =
  case breakChar '@' fs of
    (a,b) -> pauthport b  $ URIAuth (unpack a)
pauthport fs cont =
  let spl idx = splitAt (idx+1) fs
  in case unsafeHead fs of
      _ | null fs -> cont "" ""
      '['         -> case fmap spl (elemIndexEnd ']' fs) of
                       Just (a,b) | null b              -> cont (unpack a) ""
                                  | unsafeHead b == ':' -> cont (unpack a) (unpack $ unsafeTail b)
                       x                                -> error ("Parsing uri failed (pauthport):"++show x)
      _           -> case breakCharEnd ':' fs of
                       (a,b) -> cont (unpack a) (unpack b)
puriref fs cont =
  let (u,r) = break (\c -> '#' == c || '?' == c) fs
  in case unsafeHead r of
      _ | null r -> cont (unpack u) "" ""
      '?'        -> pquery    (unsafeTail r) $ cont (unpack u)
      '#'        -> pfragment (unsafeTail r) $ cont (unpack u) ""
pquery fs cont =
  case breakChar '#' fs of
    (a,b) -> cont ('?':unpack a) (unpack b)
pfragment fs cont =
  cont $ unpack fs



unsafeTail = BB.unsafeTail
unsafeHead = BB.w2c . BB.unsafeHead
unsafeIndex s = BB.w2c . BB.unsafeIndex s

