module DB (conn) where

--import CoC.Checker (TypeChecker (..), idecls, judge)
--import CoC.Term (Const (..), ITypeDecl (..), Sort (..), Term (..), appArgs, apps, piArgs, subs)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, connect, defaultConnectInfo)

conn :: IO Connection
conn =
  connect
    defaultConnectInfo
      { connectHost = "localhost",
        connectDatabase = "brute",
        connectUser = "ksb",
        connectPassword = ""
      }

-- insert :: TypeChecker -> Term -> String
-- insert tc t = insertTemplate tn ("size" : cols) (show (size t) : vals)
--   where
--     tab = toTab tc (judge tc t)
--     tn = tabName tc tab
--     (cols, vals) = case (appArgs t, tab) of
--       ((C Ic n) : _, it@IT {}) -> ([tabName tc it ++ "_type"], [unpack n])
--       _ -> ([], [])
