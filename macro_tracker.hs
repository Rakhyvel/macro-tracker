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
--  [ ] Add support for parsing fractions in meal text files
--  [ ] Add comments to functions
--  [ ] Recognize imperial units, convert between them, output largest unit in grocery list
--  [ ] Compile to executable, take in input and ingredients files from command line
--  [ ] Add macro in meal file for currency, SI-ability, macro requirements, how long the meal prep 'week' is
--  [ ] Finalize

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
-- TODO: Perhaps concat these two IO actions? ROP
main :: IO ()
main = do
    ingredientContents <- readFile "ingredients.csv"
    mealContents <- readFile "meals.txt"
    case parseFiles ingredientContents mealContents of
        Left err -> putStrLn err
        Right meals -> do
            let groceries = combineGroceryLists $ concatMap ingredients meals
            let mealTable = "## Meals:\n" ++ showMealList meals
            let groceryList = "## Weekly Grocery List:\n|   |   |\n|---|---|\n" ++ showGroceryList groceries 7
            writeFile "output.md" (mealTable ++ "\n\n" ++ groceryList)


parseFiles::String->String->Either String [Meal]
parseFiles ingredientContents mealContents = do
    db <- case parseIngredients (lines ingredientContents) 1 of
        Left err -> Left err
        Right db -> return db
    parseMeals (lines mealContents) 1 db


tokenize::String->String->[String]
tokenize "" acc = [acc]
tokenize (c : cs) acc
    | c == ',' = acc : tokenize cs ""
    | isSpace c && null acc = tokenize cs ""
    | otherwise = tokenize cs (acc ++ [c])


parseIngredients::[String] -> Int -> Either String [Ingredient]
parseIngredients [] _ = Right []
parseIngredients (first : rest) linenum
    -- Empty line, skip
    | null first = parseIngredients rest (linenum + 1)
    -- Commented out line, skip
    | head (stripLeadingWhitespace first) == '#' = parseIngredients rest (linenum + 1)
    -- CSV line doesn't correct columns when tokenized
    | length tokens /= 6 = case restOutput of 
        Left prevErr -> Left $ possibleErr ++ prevErr
        Right _ -> Left $ possibleErr
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
                Right $ Ingredient name (Macros calories protein carbs fats cents) : goodRestOutput
    where
        restOutput = parseIngredients rest (linenum + 1)
        possibleErr = "ingredients.csv:" ++ (show linenum) ++ " error: row should have 6 columns, has " ++ (show $ length tokens) ++ "\n" ++ (show linenum) ++ " |\t" ++ first ++ "\n"
        tokens = tokenize first ""
        name = head tokens


readField::[String]->Int->Int->String -> Either String Float
readField tokens fieldNum linenum first = 
    case readQuantity $ tokens !! fieldNum of
        Nothing -> Left $ "ingredients.csv:" ++ (show linenum) ++ " error: unable to parse " ++ (fieldNames !! fieldNum) ++ " field\n" ++ (show linenum) ++ " |" ++ first ++ "\n"
        Just a -> return a
    where
        fieldNames = ["name", "calories", "protein", "fats" , "carbs", "cents"]


stripLeadingWhitespace :: String -> String
stripLeadingWhitespace = unlines . map (dropWhile isSpace) . lines


parseMeals::[String] -> Int -> [Ingredient] -> Either String [Meal]
parseMeals [] _ _ = Right []
parseMeals (first : rest) linenum db
    -- Empty line, skip
    | null first = parseMeals rest (linenum + 1) db
    -- Commented-out line, skip
    | head (stripLeadingWhitespace first) == '#' = parseMeals rest (linenum + 1) db
    -- Otherwise, try to parse meal
    | otherwise = do
        (meal, newRest, newLinenum) <- case mealParse of
            Left err -> Left err
            Right (meal, newRest, linesParsed) -> return (meal, newRest, linesParsed)

        macros <- case calculateMealMacros meal db of
            Left err -> Left $ "meals.txt:" ++ (show linenum) ++ err ++ "\n"
            Right macros -> return macros

        case parseMeals newRest (newLinenum) db of
            Left prevErr -> Left prevErr
            Right goodRestOutput ->
                if last first == ':' 
                then Right $ Meal nameWithoutColon meal macros : goodRestOutput
                else Left  $ "meals.txt:" ++ (show linenum) ++ ": error: expected a meal name followed by a `:`\n" ++ (show linenum) ++ " |\t" ++ first ++ "\n"
    where
        mealParse = parseMeal rest (linenum + 1)
        nameWithoutColon = (take (length first - 1) first)


parseMeal::[String]->Int->Either String ([(Float, String)], [String], Int)
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
            Nothing -> Left $ "meals.txt:" ++ (show linenum) ++ " error: unable to parse ingredient quantity\n" ++ (show linenum) ++ " |" ++ first ++ "\n"
            Just q -> return q
        case restOutput of
            Left err -> restOutput
            Right (restMeal, newRest, newLinenum) -> Right $ ((quantity, ingredient) : restMeal, newRest, newLinenum + 1)
    where
        restOutput = parseMeal rest (linenum + 1)
        ingredient = unwords $ drop 1 $ words first


sumMacros::Macros->Macros->Float->Macros
sumMacros a b f =
    Macros
        (f * calories a + calories b)
        (f * protein a + protein b)
        (f * carbs a + carbs b)
        (f * fats a + fats b)
        (f * cents a + cents b)


calculateMealMacros::[(Float, String)]->[Ingredient]->Either String Macros
calculateMealMacros [] _ = Right $ Macros 0 0 0 0 0
calculateMealMacros ((quantity, name):rest) db = do
    restMacro <- case calculateMealMacros rest db of
        Left err -> Left err
        Right restMacro -> return restMacro

    case ingredientLookup db name of
        Left err -> Left err
        Right macros -> Right $ sumMacros macros restMacro quantity


ingredientLookup::[Ingredient] -> String -> Either String Macros
ingredientLookup [] ingredient = Left $ " error: unknown ingredient `" ++ ingredient ++ "`"
ingredientLookup (first:rest) name =
    if ingredientName first == name then Right $ ingredientMacros first
    else ingredientLookup rest name


mealLookup::[(Float, String)]->String->Int->Int
mealLookup [] _ _ = -1
mealLookup ((_, first):rest) name acc =
    if first == name then acc
    else mealLookup rest name (acc + 1)


getTotalMacros::[Macros]->Macros
getTotalMacros [] = Macros 0 0 0 0 0
getTotalMacros (thisMacro : rest) =
    sumMacros thisMacro restMacro 1
    where
        restMacro = getTotalMacros rest


replace :: [a] -> Int -> a -> [a]
replace xs i e =
    case splitAt i xs of
        (before, _:after) -> before ++ e: after
        _ -> xs


combineGroceryLists::[(Float, String)]->[(Float, String)]
combineGroceryLists [] = []
combineGroceryLists ((quantity, name):rest) =
    if i == -1 then (quantity, name):other
    else replace other i (quantity + oldQuantity, name)
    where
        other = combineGroceryLists rest
        i = mealLookup other name 0
        (oldQuantity, _) = other !! i


showMealList::[Meal]->String
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


concatTimes::String->Int->String -> String
concatTimes _ 0 acc = acc
concatTimes x n acc = concatTimes x (n-1) (x ++ acc)


readQuantity::String -> Maybe Float
readQuantity x
    | isUnique '/' x = do
        num <- case readMaybe numStr of 
            Nothing -> Nothing
            Just num -> return num
        denom <- case readMaybe $ drop 1 denomStr of 
            Nothing -> Nothing
            Just denom -> return denom
        Just $ num / denom
    | otherwise = readMaybe x
    where
        (numStr, denomStr) = splitAt (fromMaybe 0 (elemIndex '/' x)) x


showFloat::Float->String
showFloat x = if abs(fromIntegral(truncate x) - x) < 0.001 then printf "%.0g" x else printf "%g" x


showGroceryList::[(Float, String)]->Float->String
showGroceryList [] _ = ""
showGroceryList ((quantity, name):rest) scale =
    "| " ++ showFloat (scale * quantity) ++ " | " ++ (capitalized name) ++ " | \n" ++ showGroceryList rest scale


capitalized::String->String
capitalized [] = []
capitalized (x:xs) = toUpper x : map toLower xs

isUnique::Char->String->Bool
isUnique c "" = False
isUnique c (x:xs) =
    if c == x then not (c `elem` xs) else isUnique c xs