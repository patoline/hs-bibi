{-#OPTIONS -XFlexibleInstances -XBangPatterns #-}
import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server
import Happstack.Server.Internal.Monads
import Happstack.Server.Monads


import System.IO.Unsafe

--import qualified "transformers" Control.Monad.Trans as TRA

import Control.Monad
import Control.Monad.Trans
import System.Environment
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB

import Text.PrettyPrint

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import  Text.Blaze.Html5 as H hiding (map)
import  Text.Blaze.Html5.Attributes as A
import Debug.Trace


create=
   [
  "DROP TABLE IF EXISTS authors",
  "CREATE TABLE authors("++
  "id INTEGER PRIMARY KEY AUTOINCREMENT, first_name TEXT, last_name TEXT)",
  
  "DROP TABLE IF EXISTS articles",
  "CREATE TABLE articles ("++
  "id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, "++
  "source_table TEXT, source INTEGER, volume INTEGER, number INTEGER, date DATETIME, "++
  "page_start INTEGER, page_end INTEGER, "++
  "doi TEXT)",
  
  "DROP TABLE IF EXISTS authors_articles",
  "CREATE TABLE authors_articles ("++
  "id INTEGER PRIMARY KEY AUTOINCREMENT, author INTEGER, article INTEGER)",
  
  "DROP TABLE IF EXISTS journals",
  "CREATE TABLE journals (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, period TEXT, website TEXT)",
  
  "DROP TABLE IF EXISTS books",
  "CREATE TABLE books (id INTEGER PRIMARY KEY AUTOINCREMENT, "++
  "title TEXT, publisher INTEGER, "++
  "volume INTEGER, number INTEGER, date DATETIME, edition TEXT)",

  "DROP TABLE IF EXISTS authors_books",
  "CREATE TABLE authors_books ("++
  "id INTEGER PRIMARY KEY AUTOINCREMENT, author INTEGER, book INTEGER)",
  
  "DROP TABLE IF EXISTS publishers",
  "CREATE TABLE publishers (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, address TEXT, website TEXT)"
  ]

sqlStr x=case safeFromSql x of
  Left _->""
  Right y-> y

-- articles h=do
--   conn<-connectSqlite3 h
--   a0<-quickQuery' conn "SELECT (id,title,source_table,source,volume,number,date,page_start,page_end) FROM articles" []
--   forM_ a0 $ \a1->
--     case map sqlStr a1 of
--       id:tit:sourt:sour:vol:num:date:page0:page1:_->do
--         b0<-quickQuery' conn "SELECT (id,first_name,last_name) FROM authors WHERE id IN (SELECT author FROM articles_authors WHERE article=?" [toSql id]
--         case map sqlStr b0 of
--           id_auth:fname:lname->


getAll h=do
  conn<-connectSqlite3 h
  commit conn
  a0<-prepare conn "SELECT * FROM articles"
  a1<-execute a0 []
  articles<-fetchAllRows' a0
  a2<-getColumnNames a0
    
  b0<-prepare conn "SELECT * FROM authors"
  b1<-execute b0 []
  authors<-fetchAllRows' b0
  b2<-getColumnNames b0

  c0<-prepare conn "SELECT * FROM books"
  c1<-execute c0 []
  books<-fetchAllRows' c0
  c2<-getColumnNames c0
  disconnect conn
  return [("Articles",a2:map (map sqlStr) articles),
          ("Authors",b2:map (map  sqlStr) authors),
          ("Books",c2:map (map sqlStr) books)]


firstPage cont=H.docTypeHtml $ do
  H.head $ do
    H.title $ toHtml "Biberon"
  H.body cont


allTables l=
  forM_ l $ \(a,b)->do
    H.h1 $ toHtml a
    let b'=if null b then [] else
             (take (length $ Prelude.head b) $ repeat "")
    H.table $ H.tbody $ tables a (b++[b'])

    where
      tables _ []=return ()
      tables a x@(h:s)=do
        H.tr $ forM_ h $ \cell->H.td $ H.toHtml cell
        forM_ s $ \row->case row of
          []->return ()
          h0:s0->
            (H.form ! A.action (toValue "/") ! A.method (toValue "GET")) $ do
              H.input
                ! (A.type_ $ toValue "hidden")
                ! (A.name $ toValue "action")
                ! (A.value $ toValue $ "edit"++a)
              H.td $ toHtml h0
              forM_ (zip s0 (tail row)) $ \(title,cell)->
                H.td $ H.input
                ! (A.type_ $ toValue "text")
                ! (A.name $ toValue title)
                ! (A.value $ toValue cell)
              H.td $ H.input ! A.type_ (toValue "submit")


reponse h=
  let mainPage=do
        x<-liftIO $ getAll h;
        ok $ toResponseBS (B.pack "text/html") $ renderHtml $
          firstPage $ do
            allTables x
  in
   msum [
     do {
       nullDir;
       act<-look "action";
       case act of {
         "newauthor"->do {
            firstname<-look "firstname";
            lastname<-look "lastname";
            liftIO $ do {
              conn<-connectSqlite3 h;
              x<-run conn "INSERT INTO authors(first_name,last_name) VALUES (?,?)"
                 [toSql firstname, toSql lastname];
              putStrLn $ "modified rows : "++show (x,firstname,lastname);
              commit conn;
              disconnect conn
              }
            };
         _->return ()
         };
       mainPage
       },
     do {
       nullDir;
       mainPage
       }
     ]

main=do {
  args<-getArgs;

  case args of {
    []->putStrLn "Usage : biberon database";
    h:_->do {
      conn<-connectSqlite3 h;
      mapM_ (\x->run conn x []) create;
      commit conn;
      disconnect conn;
      hPutStrLn stderr "Database created.";
      simpleHTTP nullConf $ reponse h
      }
    }
  }