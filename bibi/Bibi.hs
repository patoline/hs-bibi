import Text.Bibi
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment
import System.FilePath
import System.Directory
import System.Console.GetOpt
import qualified Data.Map as M
import Data.List

data Options=Options {
  output::FilePath,
  del::Bool
  }
options=[ Option ['o'] ["output"]
          (ReqArg (\f opts -> opts { output = f }) "FILE")
          "Output file",
          Option ['d'] ["delete"]
          (NoArg (\opts -> opts { del=True }))
          "Delete the output if it exists"]
main=do
  args<-getArgs
  let header = "Usage: bibi [-o output] [-d] files..."
      defaultOptions=Options { output="", del=False }
  case getOpt Permute options args of
    (_,[],[])->return ()
    (o,n,[])->do
      let opts=foldl (flip id) defaultOptions o
      mapM_ (\x->do
                f<-readFile x
                case bibtex x f of
                  Left bib->do { putStrLn "erreur"; print bib }
                  Right bib->do
                    ex<-doesFileExist $ if null $ output opts then replaceExtension x "bibi" else output opts
                    conn<-connectSqlite3 $ if null $ output opts then replaceExtension x "bibi" else output opts
                    if del opts || not ex then
                      mapM_ (\comm->do
                                run conn comm []
                                return ()) create
                      else return ()
                    let (u,v)=partition (\(_,(typ,_))->take 2 typ=="in") $ M.toList bib
                    m<-foldl (\m (k,a)->do
                                 mm<-m
                                 insertDB conn mm k a) (return M.empty) v
                    mapM (\(k,a)->insertDB conn m k a) u
                    disconnect conn
            ) n

    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
