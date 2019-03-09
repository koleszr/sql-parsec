module SQLColumn where

data SQLColumn = Column String
               | ColumnWithString String String
               deriving (Eq)

column :: String -> SQLColumn
column = Column

columnWithValue :: String -> String -> SQLColumn
columnWithValue = ColumnWithString

showColumnName :: SQLColumn -> String
showColumnName (Column c) = c
showColumnName (ColumnWithString c _) = c

showColumnValue :: SQLColumn -> Maybe String
showColumnValue (ColumnWithString _ v) = Just v
showColumnValue _ = Nothing
