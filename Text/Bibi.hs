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

biblio names=
  (do {
      spaces';char '@';
      itemType<-many1 $ noneOf " \n\t{"; spaces';
      if map toLower itemType=="string" then do {
        x<-between (char '{') (char '}') (parseName names);
        do { char ',';return ()} <|> return ();
        biblio x
        } else do {
        char '{'; spaces';
        key<-many1 $ noneOf ",}"; spaces';
        x<-def names; spaces';
        char '}'; spaces';
        do { char ',';return ()} <|> return ();
        y<-biblio names;
        return $ M.insert key (map toLower itemType, x) y
        }
      })
  <|> (do {
          eof;
          return M.empty
          })

text 0 _=return ""
text n c=
  do {
    char c;
    return ""
    }
  <|> do {
    char '"';
    x<-text (n+1) '"';
    y<-text n c;
    return $ x++y
    }
  <|> do {
    char '{';
    x<-text (n+1) '}';
    y<-text n c;
    return $ x++y
    }
  <|> do {
    char '\\';
    cc<-anyChar;
    y<-text n c;
    return $ cc:y
    }
  <|> do {
    x<-many $ noneOf $ c:"\\{\"%";
    do { char '%'; many $ noneOf "\n"; char '\n'; return ()} <|> (return ());
    y<-if null x then return "" else text n c;
    return $ x++y
    }

def names=
  (do {
      char ','; spaces';
      (do {
          key<-many1 (noneOf "\n\t%= }"); spaces';
          char '='; spaces';
          val<-do {
            val1<-char '{' <|> char '"';
            x<-text 1 $ if val1=='{' then '}' else '"';
            return x;
            } <|> do {
            do {char '#';return ()}<|>(return ());
            x<-do { y<-many1 $ noneOf "\n \t},\"%";spaces';return y };
            case M.lookup x names of {
              Nothing-> if all isDigit x then return x else (fail $ "undefined name : "++x);
              Just a->return a }
            } <|> do {
            many1 $ noneOf "{\",}"
            };
          spaces';
          d<-def names;
          return $ M.insert (map toLower key) (intercalate " " $ words val) d
          }) <|> (return M.empty)
      })
  <|>
  (do {
      return M.empty
      })

parseName names=do
  key<-many1 (noneOf "\n\t%= }"); spaces';
  char '='; spaces';
  val<-many1 $
       (between (char '"') (char '"') $ many $ noneOf "\"")
       <|> do { char '#'; k<-many1 $ noneOf "\"}";
                case M.lookup k names of {
                  Nothing->fail $ "unknown name "++k;
                  Just a->return a }}
  return $ M.insert key (concat val) names

bibtex::FilePath->String->Either ParseError (M.Map String (String,M.Map String String))
bibtex f str=parse (biblio M.empty) f str



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


insertDB db cross key (bibtype,defs)=do
  case (M.lookup "title" defs) of
    (Just a)->do
      verif<-quickQuery' db "SELECT id FROM bibliography WHERE type=? AND title=?"
        [toSql bibtype,toSql a]
      artID_<-case verif of
        (h:_):_->return $ Just h
        _->do
          run db "INSERT INTO bibliography(type, title) VALUES (?,?)" [toSql bibtype,toSql a]
          artID_<-quickQuery' db "SELECT last_insert_rowid()" []
          case artID_ of
            (h:_):_->return $ Just h
            _->return Nothing
      case artID_ of
        Nothing->do
          hPutStrLn stderr $ "Could not process entry "++key
          rollback db
          return cross
        Just artID_sql->do
          let artID=fromSql artID_sql::Int
          let authors=case M.lookup "author" defs of
                Nothing->[]
                Just a->map (intercalate " ".words) $ splitOn "and" a
          run db "DELETE FROM authors_publications WHERE article=?" [toSql artID]
          mapM_ (\(x,y)->do {
                    auth<-byName db "authors" x;
                    run db "INSERT INTO authors_publications (author, article,ordre) VALUES (?,?,?)"
                    [toSql auth, toSql artID, toSql (y::Int)]}) $ zip authors [0..]

          let editors=case M.lookup "editor" defs of
                Nothing->[]
                Just a->map (intercalate " ".words) $ splitOn "and" a
          run db "DELETE FROM editors_publications WHERE article=?" [toSql artID]
          mapM_ (\(x,y)->do {
                    auth<-byName db "authors" x;
                    run db "INSERT INTO editors_publications (author,article,ordre) VALUES (?,?,?)"
                    [toSql auth, toSql artID, toSql (y::Int)]}) $ zip editors [0..]

          fillTextFields db artID defs [("volume","volume"),("number","number"),("pages","pages"),
                                        ("doi","doi"),("chapter","chapter")]
          case M.lookup "journal" defs of
            Nothing -> return ()
            Just c->do
              journal<-byName db "journals" c
              run db "UPDATE bibliography SET journal=? WHERE id=?" [toSql journal, toSql artID]
              return ()
          case M.lookup "school" defs of
            Nothing->return ()
            Just c->do
              school<-institutionid db "school" c
              run db "UPDATE bibliography SET school=? WHERE id=?" [toSql school, toSql artID]
              return ()
          case M.lookup "organization" defs of
            Nothing -> return ()
            Just org->do
              organization<-byName db "institutions" org
              run db ("UPDATE bibliography SET organization=? WHERE id=?") [toSql organization, toSql artID]
              return ()
          commit db
          return $ M.insert key artID cross
    _->do
      hPutStrLn stderr $ "incomplete doc, not added. key : "++key
      return cross
