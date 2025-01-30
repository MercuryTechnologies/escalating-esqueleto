{- HLINT ignore "Use camelCase" -}
module EE3b_select where

import Database.Esqueleto.Experimental
import Database.Esqueleto.Internal.Internal (SqlSelect)
import Types (DB)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT)

b_select :: (SqlSelect a r, MonadIO m, SqlBackendCanRead backend) => _ -> ReaderT backend m _
b_select = undefined
