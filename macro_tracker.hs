-- To run: ghc macro_tracker.hs && ./macro_tracker
import Data.Char
import Data.List
import Data.Maybe
import Text.Printf
import Text.Read

-- Todo:
--  [x] Put on Github
--  [x] Put meals all on one single table, with the daily total and period total
--  [x] Better syntax error messages
--  [x] Add support for parsing fractions in meal text files
--  [x] Add comments to functions
--  [ ] Write the README.md
--      [ ] Provide a sample-meals.txt
--  [ ] Recognize imperial units, convert between them, output largest unit in grocery list
--      [ ] Perhaps have a 'units' file which is basically just a graph with how to convert between units
--  [ ] Compile to executable, take in input and ingredients files from command line
--  [ ] Add macro in meal file for currency, SI-ability, macro requirements, how long the meal prep 'week' is
--  [ ] Write tests

-- Represents one ingredient as read in from the ingredients csv
data Ingredient = Ingredient { -- TODO: include units
    -- Name of the ingredient
    ingredientName::String,
    -- The macros this ingredient has
    ingredientMacros::Macros
} deriving Show

-- Represents an entire meal as read in from the meal text file
data Meal = Meal {
    -- Name of the meal
    mealName::String,
    -- Ingredients used by this meal
    ingredients::[(Float, String)], -- TODO: include units
    -- The macros needed for this meal as defined by the ingredients CSV file
    mealMacros::Macros
} deriving Show

-- Represents the macros that are tracked in the macro tracker
data Macros = Macros {
    -- Calories, in kiloCalories
    calories::Float,
    -- Protein, in grams
    protein::Float,
    -- Total carbohydrates, in grams
    carbs::Float,
    -- Total fats, in grams
    fats::Float,
    -- Cost in local minor currency
    cents::Float -- TODO: Add localization?
} deriving Show


-- Reads in the ingredients CSV, parses the meal text file, 
main::IO ()
main = do
    ingredientContents <- readFile "ingredients.csv"
    mealContents <- readFile "meals.txt"
    case parseFiles ingredientContents mealContents of
        Left err -> putStrLn err
        Right meals -> do
            let combinedIngredientQuantities = combineGroceryLists $ concatMap ingredients meals
            let mealTableString = "## Meals:\n" ++ showMealList meals
            let groceryListString = "## Weekly Grocery List:\n| Quantity | Ingredient |\n|---|---|\n" ++ showGroceryList combinedIngredientQuantities 7
            writeFile "output.md" (mealTableString ++ "\n\n" ++ groceryListString)


-- Takes in the filenames of the ingredients CSV file and the meal text file, parses them, and returns the meals list
parseFiles::String->String -> Either String [Meal]
parseFiles ingredientContents mealContents = do
    db <- case parseIngredients (lines ingredientContents) 1 of
        Left err -> Left err
        Right db -> return db
    parseMeals (lines mealContents) 1 db

-- Creates an error message
errorMessage::String->Int->String -> Either String b
errorMessage file linenum msg =
    Left $ file ++ ":" ++ (show linenum) ++ " error: " ++ msg ++ "\n"


-- Splits a CSV rows on commas. Acc should start at ""
tokenize::String->String -> [String]
tokenize "" acc = [acc]
tokenize (c : cs) acc
    -- Encounted comma, append acc to output list, reset acc
    | c == ',' = acc : tokenize cs ""
    -- Leading space, ignore
    | isSpace c && null acc = tokenize cs ""
    -- Regular character, append to acc
    | otherwise = tokenize cs (acc ++ [c])


-- Takes a list of CSV rows as untokenized strings. Parses the ingredients, and returns either a parse error message or a list of Ingredients
parseIngredients::[String]->Int -> Either String [Ingredient]
parseIngredients [] _ = Right []
parseIngredients (first : rest) linenum
    -- Empty line, skip
    | null first = parseIngredients rest (linenum + 1)
    -- Commented out line, skip
    | head (stripLeadingWhitespace first) == '#' = parseIngredients rest (linenum + 1)
    -- CSV line doesn't correct columns when tokenized
    | length tokens /= 6 = errorMessage "ingredients.csv" linenum ("row should have 6 columns, has " ++ (show $ length tokens))
    -- CSV line, read in ingredient
    | otherwise = do
        calories <- readField tokens 1 linenum first
        protein <- readField tokens 2 linenum first
        carbs <- readField tokens 3 linenum first
        fats <- readField tokens 4 linenum first
        cents <- readField tokens 5 linenum first
        case restOutput of 
            Left err -> 
                restOutput
            Right goodRestOutput ->
                Right $ Ingredient (head tokens) (Macros calories protein carbs fats cents) : goodRestOutput
    where
        restOutput = parseIngredients rest (linenum + 1)
        tokens = tokenize first ""


-- Reads in a numeric quantity from a tokenized CSV row. Returns either a read error message or the value
readField::[String]->Int->Int->String -> Either String Float
readField tokens fieldNum linenum first = 
    case readQuantity $ tokens !! fieldNum of
        Nothing -> errorMessage "ingredients.csv" linenum ("unable to parse " ++ (fieldNames !! fieldNum) ++ " field")
        Just a -> return a
    where
        fieldNames = ["name", "calories", "protein", "fats" , "carbs", "cents"]


-- Strips off any leading whitespace
stripLeadingWhitespace::String -> String
stripLeadingWhitespace = unlines . map (dropWhile isSpace) . lines


-- Parses the list of meal text file along with the ingredient database and returns either a parse error or a list of meals
parseMeals::[String]->Int->[Ingredient] -> Either String [Meal]
parseMeals [] _ _ = Right []
parseMeals (first : rest) linenum db
    -- Empty line, skip
    | null first = parseMeals rest (linenum + 1) db
    -- Commented-out line, skip
    | head (stripLeadingWhitespace first) == '#' = parseMeals rest (linenum + 1) db
    -- Otherwise, try to parse meal
    | otherwise = do
        (meal, newRest, newLinenum) <- case parseMeal rest (linenum + 1) of
            Left err -> Left err
            Right (meal, newRest, linesParsed) -> return (meal, newRest, linesParsed)

        macros <- case calculateMealMacros meal db of
            Left err -> Left $ "meals.txt:" ++ (show linenum) ++ err ++ "\n"
            Right macros -> return macros

        case parseMeals newRest (newLinenum) db of
            Left prevErr -> Left prevErr
            Right goodRestOutput ->
                if last first == ':' 
                then Right $ Meal (take (length first - 1) first) meal macros : goodRestOutput
                else errorMessage "meals.txt" linenum "expected a meal name followed by a `:`"


-- Parses a single meal from the meal text file, returns either an error or the list of ingredients for a meal
parseMeal::[String]->Int -> Either String ([(Float, String)], [String], Int)
parseMeal [] linenum = Right ([], [], linenum)
parseMeal (first:rest) linenum
    -- Empty line, stop parsing this meal
    | null first = Right ([], rest, linenum)
    -- Commented-out line, skip this line
    | head (stripLeadingWhitespace first) == '#' = restOutput
    -- Line ends in `:`, begins new meal, stop parsing this meal
    | last first == ':' = Right ([], rest, linenum)
    -- Line doesn't end in `:`, read ingredient
    | otherwise = do
        quantity <- case readQuantity $ head $ words first of 
            Nothing -> errorMessage "meals.txt" linenum "unable to parse ingredient quantity"
            Just q -> return q
        case restOutput of
            Left err -> restOutput
            Right (restMeal, newRest, newLinenum) -> Right $ ((quantity, ingredient) : restMeal, newRest, newLinenum + 1)
    where
        restOutput = parseMeal rest (linenum + 1)
        ingredient = unwords $ drop 1 $ words first


-- Combines two macros with a scalar for the first macro
sumMacros::Macros->Macros->Float -> Macros
sumMacros a b f =
    Macros
        (f * calories a + calories b)
        (f * protein a + protein b)
        (f * carbs a + carbs b)
        (f * fats a + fats b)
        (f * cents a + cents b)


-- Uses the ingredients DB to combine a list of quantities and ingredients names into a single Macros structure
calculateMealMacros::[(Float, String)]->[Ingredient] -> Either String Macros
calculateMealMacros [] _ = Right $ Macros 0 0 0 0 0
calculateMealMacros ((quantity, name):rest) db = do
    restMacro <- case calculateMealMacros rest db of
        Left err -> Left err
        Right restMacro -> return restMacro

    case ingredientLookup db name of
        Left err -> Left err
        Right macros -> Right $ sumMacros macros restMacro quantity


-- Finds the macros associated with an ingredient
ingredientLookup::[Ingredient]->String -> Either String Macros
ingredientLookup [] ingredient = Left $ " error: unknown ingredient `" ++ ingredient ++ "`"
ingredientLookup (first:rest) name =
    if ingredientName first == name then Right $ ingredientMacros first
    else ingredientLookup rest name


-- Combines a list of macros into a single macro
-- TODO: Make macros implement Monoid?
getTotalMacros::[Macros]->Macros
getTotalMacros [] = Macros 0 0 0 0 0
getTotalMacros (thisMacro : rest) =
    sumMacros thisMacro (getTotalMacros rest) 1


-- Replaces an element in a list at an index
replace :: [a]->Int->a -> [a]
replace list index element =
    case splitAt index list of
        (before, _:after) -> 
            before ++ element: after
        _ -> 
            list


-- Finds the index of an ingredient in a meals ingredient list. Acc should be 0 by default. Returns -1 if not found 
-- TODO: return Nothing if not found?
-- TODO: change name? kinda confusing
mealLookup::[(Float, String)]->String->Int -> Int
mealLookup [] _ _ = -1
mealLookup ((_, first):rest) name acc =
    if first == name then acc
    else mealLookup rest name (acc + 1)


-- Takes in a list of quantities of ingredients, combines them so that each ingredient is in the output list once with the total quantity
combineGroceryLists::[(Float, String)] -> [(Float, String)]
combineGroceryLists [] = []
combineGroceryLists ((quantity, name):rest) =
    if i == -1 
        -- Ingredient was not found in the ingredient list, add it
        then (quantity, name):restOutput
        -- Ingredient was found in the list, increment the quantity
        else replace restOutput i (quantity + oldQuantity, name)
    where
        restOutput = combineGroceryLists rest
        i = mealLookup restOutput name 0
        (oldQuantity, _) = restOutput !! i


-- Constructs the meals table string from a list of meals
showMealList::[Meal] -> String
showMealList [] = ""
showMealList meals =
    "|    | Daily Total | " ++ (concatMap ((++ " | ") . mealName) meals) ++ "\n"
        ++ concatTimes "|----" ((length meals) + 1) "|----|\n"
        ++ "| __Calories__ | "            ++ (showCalories totalMacros)        ++ (concatMap (showCalories . mealMacros) meals) ++ "\n"
        ++ "| __Protein__ | "             ++ (showGrams $ protein totalMacros) ++ (concatMap (showGrams . protein . mealMacros) meals) ++ "\n"
        ++ "| __Total Carbohydrates__ | " ++ (showGrams $ carbs totalMacros)   ++ (concatMap (showGrams . carbs . mealMacros) meals) ++ "\n"
        ++ "| __Total Fat__ | "           ++ (showGrams $ fats totalMacros)    ++ (concatMap (showGrams . fats . mealMacros) meals) ++ "\n"
        ++ "| __Cost__ | $"               ++ (showCurrency totalMacros)        ++ (concatMap (showCurrency . mealMacros) meals) ++ "\n"
    where
        showCalories = (++ " | ")  . show . truncate . calories 
        showGrams    = (++ "g | ") . show . truncate
        showCurrency = (++ " | $") . (printf "%.2f") . (/ 100) . cents
        totalMacros  = getTotalMacros (map mealMacros meals)


-- Repeats a string n times. Acc should be "" by default
concatTimes::String->Int->String -> String
concatTimes _ 0 acc = acc
concatTimes x n acc = concatTimes x (n-1) (x ++ acc)


-- Reads either a fraction or a decimal from a string. Fractions may only contain 1 '/' character
readQuantity::String -> Maybe Float
readQuantity x
    | isUnique '/' x = do
        let (numStr, denomStr) = splitAt (fromMaybe 0 (elemIndex '/' x)) x
        num <- case readMaybe numStr of 
            Nothing -> Nothing
            Just num -> return num
        denom <- case readMaybe $ drop 1 denomStr of 
            Nothing -> Nothing
            Just denom -> return denom
        Just $ num / denom
    | otherwise = readMaybe x


-- If the input float is "close enough"  to its rounded integer, shown as an integer, otherwise shown as a float
showFloat::Float -> String
showFloat x = if abs(fromIntegral(round x) - x) < 0.001 then printf "%.0g" x else printf "%g" x


-- Constructs the grocery list table string from a list of quantities of ingredients
showGroceryList::[(Float, String)]->Float -> String
showGroceryList [] _ = ""
showGroceryList ((quantity, name):rest) scale =
    "| " ++ showFloat (scale * quantity) ++ " | " ++ (capitalized name) ++ " | \n" ++ showGroceryList rest scale


-- Capitalizes the first letter of a string, with the rest in lowercase
-- TODO: rename to toCapitalized
capitalized::String->String
capitalized [] = []
capitalized (x:xs) = toUpper x : map toLower xs


-- Returns true if the character appears exactly once in the string
-- TODO: rename to `lone`, generalize?
isUnique::Char->String->Bool
isUnique c "" = False
isUnique c (x:xs) =
    if c == x then not (c `elem` xs) else isUnique c xs