{-# LANGUAGE TypeOperators #-}

module Router where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Snap.Types
import Web.Zwaluw

-- | A web handler which, given a Zwaluw Router, and a function to a handler,
-- calls the handler, passing the value.
router :: MonadSnap m => Router () (a :- ()) -> (a -> m b) -> m b
router r f = do
  p <- BS.unpack . rqURI <$> getRequest
  maybe empty f (parse1 r p)

