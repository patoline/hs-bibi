{-# OPTIONS -XPackageImports #-}
module Text.Bibi(
  -- | Parsing of external formats
  bibtex,
  -- | Bibliographic databases
  fields, create, createDB, insertDB,
  authorid, journalid, publisherid,
  ) where

import Text.ParserCombinators.Parsec as P
import Database.HDBC
import Database.HDBC.Sqlite3

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M
import System.IO

{------------- Parser -------------}

spaces'=do
  spaces
  (do {
      _<-char '%';
      _<-many $ noneOf "\n";
      _<-char '\n';
      spaces'
      }) <|> (return ())

biblio=
  (do {
      spaces';char '@';
      itemType<-many1 $ noneOf " \n\t{"; spaces';
      char '{'; spaces';
      key<-many1 $ noneOf ",}"; spaces';
      x<-def; spaces';
      char '}'; spaces';
      y<-biblio;
      return $ M.insert key (map toLower itemType, x) y
      })
  <|> (do {
          eof;
          return M.empty
          })

text c=do
  x<-(between (char '{') (char '}') (text c)) <|> (many $ noneOf $ c:"{%")
  (do { char '%'; _<-many $ noneOf "\n"; char '\n'; return ()}) <|> (return ())
  y<-if null x then return "" else text c
  return $ x++y

def=
  (do {
      char ','; spaces';
      (do {
          key<-many1 (noneOf "\n\t%= }"); spaces';
          char '='; spaces';
          val<-(between (char '{') (char '}') $ text '}') <|>
               (between (char '"') (char '"') $ text '"') <|>
               (between (char '\'') (char '\'') $ text '\''); spaces';
          d<-def;
          return $ M.insert (map toLower key) (intercalate " " $ words val) d
          }) <|> (return M.empty)
      })
  <|>
  (do {
      return M.empty
      })
bibtex::FilePath->String->Either ParseError (M.Map String (String,M.Map String String))
bibtex f str=parse biblio f str



{------------- Base de données -------------}


fields=[
  ("id","INTEGER PRIMARY KEY AUTOINCREMENT"),
  ("booktitle", "TEXT"),
  ("chapter", "TEXT"),
  ("crossref", "INTEGER"),
  ("date", "DATETIME"),
  ("doi", "TEXT"),
  ("edition", "TEXT"),
  ("eprint", "TEXT"),
  ("institution", "INTEGER"),
  ("isbn", "TEXT"),
  ("journal", "INTEGER"),
  ("number", "TEXT"),
  ("organization", "INTEGER"),
  ("pages", "TEXT"),
  ("publisher", "INTEGER"),
  ("school", "INTEGER"),
  ("series", "TEXT"),
  ("title", "TEXT"),
  ("type", "TEXT"),
  ("url", "TEXT"),
  ("volume", "TEXT")
  ]

create=
  [
    "DROP TABLE IF EXISTS authors",
    "CREATE TABLE authors("++
    "id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, author INTEGER, name_format TEXT)",
    "DROP TABLE IF EXISTS institutions",
    "CREATE TABLE institutions("++
    "id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, type TEXT, address TEXT, url TEXT)",
    "DROP TABLE IF EXISTS publishers",
    "CREATE TABLE publishers("++
    "id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, address TEXT, url TEXT)",
    "DROP TABLE IF EXISTS journals",
    "CREATE TABLE journals("++
    "id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, publisher INTEGER)",
    "DROP TABLE IF EXISTS bibliography",
    "CREATE TABLE bibliography ("++(intercalate ", " (map (\ (a,b)->a++" "++b) fields))++")",
    "DROP TABLE IF EXISTS authors_publications",
    "CREATE TABLE authors_publications (id INTEGER PRIMARY KEY AUTOINCREMENT, author INTEGER, article INTEGER, ordre INTEGER)",
    "DROP TABLE IF EXISTS editors_publications",
    "CREATE TABLE editors_publications (id INTEGER PRIMARY KEY AUTOINCREMENT, author INTEGER, article INTEGER, ordre INTEGER)"
  ]

createDB fname=do
  conn<-connectSqlite3 fname
  mapM_ (\comm->do
            run conn comm []
            return ()) create
  return conn


byName::Connection->String->String->IO Int
byName db table j=do
  a<-quickQuery' db ("SELECT id FROM "++table++" WHERE name=?") [toSql j]
  case a of
    []->do
      run db ("INSERT INTO "++table++"(name) VALUES (?)") [toSql j]
      byName db table j
    (h:_):_->return $ fromSql h

authorid db auth=byName db "authors" auth
journalid db auth=byName db "journals" auth

-- | @'institutionid' type name@, where @type@ is the institution's type (school, university, etc.), and @name@ its name.
institutionid::Connection->String->String->IO Int
institutionid db typ j=do
  a<-quickQuery' db ("SELECT id FROM institutions WHERE name=? AND type=?") [toSql j, toSql typ]
  case a of
    []->do
      run db ("INSERT INTO institutions(name,type) VALUES (?,?)") [toSql j, toSql typ]
      institutionid db typ j
    (h:_):_->return $ fromSql h

-- | @'publisherid' name address@, where @address@ is the publisher's address, if known
publisherid::Connection->String->Maybe String->IO Int
publisherid db j addr_=do
  a<-quickQuery' db ("SELECT id,address FROM publishers WHERE name=?") [toSql j]
  let addr=case addr_ of { Nothing->"" ; Just a->a }
  case a of
    []->do
      run db ("INSERT INTO publishers (name,address) VALUES (?,?)") [toSql j, toSql addr]
      publisherid db j addr_
    (h0:h1:_):_->do
      addr1<-case safeFromSql h1 of {
        Left _->return $ toSql addr;
        Right ""->return $ toSql addr;
        Right addr0->do
          if addr/=addr0 then putStrLn $ "Conflict in publisher address "++(show (j,addr,addr0)) else return ()
          return $ toSql addr0
        }
      run db ("INSERT INTO publishers (name,address) VALUES (?,?)") [toSql j, addr1]
      return $ fromSql h0


fillTextFields db _ _ []=return ()
fillTextFields db ii defs ((x,y):s)=
  case M.lookup x defs of
    Nothing -> fillTextFields db ii defs s
    Just a->do
      run db ("UPDATE bibliography SET "++y++"=? WHERE id=?") [toSql a, toSql ii]
      fillTextFields db ii defs s

{- article -}
insertDB db cross key ("article",defs)=do
  case (M.lookup "author" defs, M.lookup "title" defs,
        M.lookup "journal" defs, M.lookup "year" defs) of

    (Just a,Just b,Just c,Just d)->do
      let authors=map (intercalate " ".words) $ splitOn "and" a
      journal<-byName db "journals" c
      verif<-quickQuery' db "SELECT * FROM bibliography WHERE type=? AND title=? AND journal=? AND date=?"
        [toSql "article",toSql b, toSql journal, toSql d]
      if null verif then do
        run db "INSERT INTO bibliography(type, title, journal, date) VALUES (?,?,?,?)"
          [toSql "article",toSql b, toSql journal, toSql d]
        artID_<-quickQuery' db "SELECT last_insert_rowid()" []
        case artID_ of
          (h:_):_->do
            let artID=fromSql h::Int
            run db "DELETE FROM authors_publications WHERE article=?" [toSql artID]
            mapM_ (\(x,y)->do {
                      auth<-byName db "authors" x;
                      run db "INSERT INTO authors_publications (author, article,ordre) VALUES (?,?,?)"
                      [toSql auth, toSql artID, toSql (y::Int)]}) $ zip authors [0..]

            fillTextFields db h defs [("volume","volume"),("number","number"),("pages","pages"),("doi","doi")]
            commit db
            return $ M.insert key h cross
          _->do { rollback db; return cross }
        else do
        --putStrLn $ show key++" already in the base, please delete first"
        return cross
    _->do
      putStrLn "incomplete doc, not added"
      print (key,defs);
      return cross


{- webpage -}
insertDB db cross key ("webpage",defs)=do
  case (M.lookup "author" defs, M.lookup "title" defs, M.lookup "url" defs) of
    (Just a,Just b,Just c)->do
      let authors=map (intercalate " ".words) $ splitOn "and" a
      verif<-quickQuery' db "SELECT * FROM bibliography WHERE type=? AND title=? AND date=?"
        [toSql "webpage",toSql b, toSql c]
      if null verif then do
        run db "INSERT INTO bibliography(type, title, date) VALUES (?,?,?)"
          [toSql "webpage",toSql b, toSql c]
        artID_<-quickQuery' db "SELECT last_insert_rowid()" []
        case artID_ of
          (h:_):_->do
            let artID=fromSql h::Int
            run db "DELETE FROM authors_publications WHERE article=?" [toSql artID]
            mapM_ (\(x,y)->do {
                      auth<-byName db "authors" x;
                      run db "INSERT INTO authors_publications (author, article,ordre) VALUES (?,?,?)"
                      [toSql auth, toSql artID, toSql (y::Int)]}) $ zip authors [0..]

            commit db
            return $ M.insert key h cross
          _->do { rollback db; return cross }
        else do
        --putStrLn $ show key++" already in the base, please delete first"
        return cross
    _->do
      putStrLn "incomplete doc, not added"
      print (key,defs);
      return cross

{- inproceedings -}
insertDB db cross key ("inproceedings",defs)=do
  let crossref=do
        x<-M.lookup "crossref" defs
        M.lookup x cross
  case (M.lookup "author" defs, M.lookup "title" defs,
        crossref, M.lookup "year" defs) of

    (Just a,Just b,Just c,Just d)->do
      let authors=map (intercalate " ".words) $ splitOn "and" a
      verif<-quickQuery' db "SELECT * FROM bibliography WHERE type=? AND title=? AND crossref=? AND date=?"
             [toSql "inproceedings",toSql b, toSql c, toSql d]
      if null verif then do
        run db "INSERT INTO bibliography(type, title, crossref, date) VALUES (?,?,?,?)"
          [toSql "inproceedings",toSql b, toSql c, toSql d]
        artID_<-quickQuery' db "SELECT last_insert_rowid()" []
        case artID_ of
          (h:_):_->do
            let artID=fromSql h::Int
            run db "DELETE FROM authors_publications WHERE article=?" [toSql artID]
            mapM_ (\(x,y)->do {
                      auth<-byName db "authors" x;
                      run db "INSERT INTO authors_publications (author, article,ordre) VALUES (?,?,?)"
                      [toSql auth, toSql artID, toSql (y::Int)]}) $ zip authors [0..]
            fillTextFields db h defs [("pages","pages"),("doi","doi")]
            commit db
            return $ M.insert key h cross
          _->do { rollback db; return cross }
        else do
        --putStrLn $ show key++" already in the base, please delete first"
        return cross
    _->do
      putStrLn "incomplete doc, not added"
      print (key,defs)
      return cross

{- inbook -}
insertDB db cross key ("inbook",defs)=do
  let crossref=do
        x<-M.lookup "crossref" defs
        M.lookup x cross
      position=case M.lookup "pages" defs of
        Nothing->M.lookup "chapter" defs
        x->x
  case (M.lookup "author" defs, M.lookup "title" defs,
        crossref, M.lookup "year" defs,position) of

    (Just a,Just b,Just _,Just d,Just e)->do
      let authors=map (intercalate " ".words) $ splitOn "and" a

      verif<-quickQuery' db "SELECT * FROM bibliography WHERE type=? AND title=? AND crossref=? AND date=?"
             [toSql "inbook",toSql b, toSql crossref, toSql d]
      if null verif then do
        run db "INSERT INTO bibliography(type, title, crossref, date) VALUES (?,?,?,?)"
          [toSql "inbook",toSql b, toSql crossref, toSql d]
        artID_<-quickQuery' db "SELECT last_insert_rowid()" []
        case artID_ of
          (h:_):_->do
            let artID=fromSql h::Int
            run db "DELETE FROM authors_publications WHERE article=?" [toSql artID]
            mapM_ (\(x,y)->do {
                      auth<-byName db "authors" x;
                      run db "INSERT INTO authors_publications (author, article,ordre) VALUES (?,?,?)"
                      [toSql auth, toSql artID, toSql (y::Int)]}) $ zip authors [0..]
            fillTextFields db h defs [("pages","pages"),("chapter","chapter"),("doi","doi")]
            commit db
            return $ M.insert key h cross
          _->do { rollback db; return cross }
        else do
        --putStrLn $ show key++" already in the base, please delete first"
        return cross
    _->do
      putStrLn "incomplete doc, not added"
      print (key,defs)
      return cross

{- book -}
insertDB db cross key ("book",defs)=
  let aut=case M.lookup "author" defs of { Nothing->M.lookup "editor" defs; a->a } in
  case (aut, M.lookup "title" defs, M.lookup "publisher" defs, M.lookup "year" defs) of

    (Just a,Just b,Just c,Just d)->do
      let authors=case M.lookup "author" defs of
            Nothing->[]
            Just aa->map (intercalate " ".words) $ splitOn "and" aa
          editors=case M.lookup "editor" defs of
            Nothing->[]
            Just aa->map (intercalate " ".words) $ splitOn "and" aa
      publisher_<-publisherid db c (M.lookup "address" defs)
      verif<-quickQuery' db "SELECT * FROM bibliography WHERE type=? AND title=? AND publisher=? AND date=?"
        [toSql "book",toSql b, toSql publisher_, toSql d]
      if null verif then do
        run db "INSERT INTO bibliography(type, title, publisher, date) VALUES (?,?,?,?)"
          [toSql "book",toSql b, toSql publisher_, toSql d]
        artID_<-quickQuery' db "SELECT last_insert_rowid()" []
        case artID_ of
          (h:_):_->do
            let artID=fromSql h::Int
            run db "DELETE FROM authors_publications WHERE article=?" [toSql artID]
            mapM_ (\(x,y)->do {
                      auth<-byName db "authors" x;
                      run db "INSERT INTO authors_publications (author,article,ordre) VALUES (?,?,?)"
                      [toSql auth, toSql artID, toSql (y::Int)]}) $ zip authors [0..]
            run db "DELETE FROM editors_publications WHERE article=?" [toSql artID]
            mapM_ (\(x,y)->do {
                      auth<-byName db "authors" x;
                      run db "INSERT INTO editors_publications (author,article,ordre) VALUES (?,?,?)"
                      [toSql auth, toSql artID, toSql (y::Int)]}) $ zip editors [0..]

            fillTextFields db h defs [("volume","volume"),("number","number"),("series","series"),("isbn","isbn"),("doi","doi")]
            commit db
            return $ M.insert key h cross
          _->do { rollback db; return cross }
        else do
        --putStrLn $ show key++" already in the base, please delete first"
        return cross
    _->do
      putStrLn "incomplete doc, not added"
      print (key,defs)
      return cross




{- phdthesis -}
insertDB db cross key ("phdthesis",defs)=
  case (M.lookup "author" defs, M.lookup "title" defs, M.lookup "school" defs, M.lookup "year" defs) of

    (Just a,Just b,Just c,Just d)->do
      let authors=case M.lookup "author" defs of
            Nothing->[]
            Just aa->map (intercalate " ".words) $ splitOn "and" aa
      school<-institutionid db "school" c
      verif<-quickQuery' db "SELECT * FROM bibliography WHERE type=? AND title=? AND school=? AND date=?"
        [toSql "book",toSql b, toSql school, toSql d]
      if null verif then do
        run db "INSERT INTO bibliography(type, title, school, date) VALUES (?,?,?,?)"
          [toSql "book",toSql b, toSql school, toSql d]
        artID_<-quickQuery' db "SELECT last_insert_rowid()" []
        case artID_ of
          (h:_):_->do
            let artID=fromSql h::Int
            run db "DELETE FROM authors_publications WHERE article=?" [toSql artID]
            mapM_ (\(x,y)->do {
                      auth<-byName db "authors" x;
                      run db "INSERT INTO authors_publications (author,article,ordre) VALUES (?,?,?)"
                      [toSql auth, toSql artID, toSql (y::Int)]}) $ zip authors [0..]

            fillTextFields db h defs [("doi","doi")]
            commit db
            return $ M.insert key h cross
          _->do { rollback db; return cross }
        else do
        --putStrLn $ show key++" already in the base, please delete first"
        return cross
    _->do
      putStrLn "incomplete doc, not added"
      print (key,defs)
      return cross




{- proceedings -}
insertDB db cross key ("proceedings",defs)=

  case (M.lookup "title" defs, M.lookup "year" defs) of

    (Just a,Just b)->do
      let editors=case M.lookup "editor" defs of
            Nothing->[]
            Just aa->map (intercalate " ".words) $ splitOn "and" aa

      verif<-quickQuery' db "SELECT * FROM bibliography WHERE type=? AND title=? AND date=?"
             [toSql "proceedings",toSql a, toSql b]
      if null verif then do
        run db "INSERT INTO bibliography(type, title, date) VALUES (?,?,?)"
          [toSql "proceedings",toSql a, toSql b]
        artID_<-quickQuery' db "SELECT last_insert_rowid()" []
        case artID_ of
          (h:_):_->do
            let artID=fromSql h::Int
            run db "DELETE FROM editors_publications WHERE article=?" [toSql artID]
            -- authors
            mapM_ (\x->do {
                      auth<-byName db "authors" x;
                      run db "INSERT INTO editors_publications (author,article) VALUES (?,?)"
                      [toSql auth, toSql artID]}) editors
            -- text fields
            fillTextFields db h defs [("volume","volume"),("number","number"),("series","series"),("doi","doi")]
            -- publisher
            case (M.lookup "publisher" defs) of
              Nothing->return ()
              Just pub->do
                publisher_<-publisherid db pub (M.lookup "address" defs)
                run db ("UPDATE bibliography SET publisher=? WHERE id=?") [toSql publisher_, toSql h]
                return ()
            -- organization
            case (M.lookup "organization" defs) of
              Nothing -> return ()
              Just org->do
                o<-byName db "organizations" org
                run db ("UPDATE bibliography SET organization=? WHERE id=?") [toSql o, toSql h]
                return ()
            commit db
            return $ M.insert key h cross
          _->do { rollback db; return cross }
        else do
        --putStrLn $ show key++" already in the base, please delete first"
        return cross
    _->do
      --putStrLn "incomplete doc, not added"
      --print (key,defs)
      return cross

insertDB db c k (x,_)=do
  hPutStrLn stderr $ "Unrecognized entry :"++show x
  return c
