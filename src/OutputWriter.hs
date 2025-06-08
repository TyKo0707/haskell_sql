module OutputWriter (writeCSV) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv as Csv
import qualified Data.Vector as V

type Header = V.Vector B.ByteString
type Rows = [Csv.NamedRecord]

writeCSV :: FilePath -> Header -> Rows -> IO ()
writeCSV path header rows = do
    let dataset = Csv.encodeByName header rows  -- rows is already a list (from Main.hs)
    BL.writeFile path dataset
