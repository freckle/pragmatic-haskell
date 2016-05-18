module Entities where

import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    name String
    deriving Show Eq
Paragraph sql=paragraphs
    content String
    userId UserId
    deriving Show Eq
|]
