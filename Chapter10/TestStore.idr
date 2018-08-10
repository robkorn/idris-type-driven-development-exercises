
module TestStore

import DataStore

testStore : DataStore (SString .+. SString .+. SInt)
testStore = addToStore ("Mercury", "Mariner 10", 1974) $
            addToStore ("Venus", "Venera", 1961) $
            addToStore ("Uranus", "Voyager 2", 1986) $
            addToStore ("Pluto", "New Horizons", 2015) $
            empty



listItems : DataStore schema -> List (SchemaType schema)
listItems x with (storeView x)
  listItems x | SNil = []
  listItems (addToStore value store) | (SAdd rec) = value :: listItems store | rec


filterKeys : (test : SchemaType val_schema -> Bool) -> DataStore (SString .+. val_schema) -> List String
filterKeys test x with (storeView x)
  filterKeys test x | SNil = []
  filterKeys test (addToStore (key, value) store) | (SAdd rec) =
    case test value of
        False => filterKeys test store | rec
        True => key :: filterKeys test store | rec

-- 10.3 Ex. # 1
getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues x with (storeView x)
  getValues x | SNil = []
  getValues (addToStore (key, value) store) | (SAdd rec) = value :: getValues store | rec

ttestStore : DataStore (SString .+. SInt)
ttestStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty
