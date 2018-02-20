--json library

module SimpleJson
(
JValue(..)
,getString
, getInt
, getDouble
, getBool
, getArray
, getObject
, isNull
  )
where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Ord, Show)

data JError

-- class JSON a where
--   toJvalue :: a -> JValue
--   fromJValue :: JValue -> Either JError a

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

isNull v = v == Nothing
