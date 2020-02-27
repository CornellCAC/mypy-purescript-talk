module Talk.Spectacle (SpecSlideId, mkCSlideId) where

import Prelude
-- import Concur.Core (Widget)
-- import Concur.React (HTML)
import Concur.React.Props as P
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

newtype SpecSlideId = SpecSlideId String
derive instance newtypeSpecSlideId :: Newtype SpecSlideId _

mkCSlideId :: String -> SpecSlideId
mkCSlideId sId = SpecSlideId $ "spectacleContent_" <> sId