module Chapter19 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4

textUuid :: IO Text
textUuid = fmap (T.pack . UUID.toString) UUIDv4.nextRandom

