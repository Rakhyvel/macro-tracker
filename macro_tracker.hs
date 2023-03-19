import Data.Char
import Text.Printf

-- Todo:
--  [x] Put on Github
--  [ ] Put meals all on one single table, with the daily total and period total
--  [ ] Better syntax error messages
--  [ ] Add comments to functions
--  [ ] Recognize imperial units, convert between them, output largest unit in grocery list
--  [ ] Add support for parsing fractions in meal text files
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
-- Has a name, a list of ingredients, and a total amount of macros
data Meal = Meal {
    -- Name of the meal
    mealName::String,
    -- Ingredients used by this meal
    ingredients::[(Float, String)], -- TODO: include units
    -- The total combined macros of the ingredients TODO: Remove?
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


-- Starts off the program
main :: IO ()
main = do
    ingredient_contents <- readFile "ingredients.csv"
    meal_contents <- readFile "meals.txt"
    let db = parseIngredients $ lines ingredient_contents
    let meals = parseMeals (lines meal_contents) db
    let groceries = combineGroceryLists $ concatMap ingredients meals
    let mealTable = "## Meals:\n" ++ showMealList meals
    let groceryList = "## Weekly Grocery List:\n|   |   |\n|---|---|\n" ++ showGroceryList groceries 7
    let output = mealTable ++ "\n\n" ++ groceryList
    writeFile "output.md" output


tokenize::String->String->[String]
tokenize "" acc = [acc]
tokenize (c : cs) acc
    | c == ',' = acc : tokenize cs ""
    | isSpace c && null acc = tokenize cs ""
    | otherwise = tokenize cs (acc ++ [c])


parseIngredients::[String] -> [Ingredient]
parseIngredients [] = []
parseIngredients (first : rest)
    | null first = parseIngredients rest
    | head first == '#' = parseIngredients rest
    | otherwise = Ingredient name (Macros calories protein carbs fats cents) : parseIngredients rest
    where
        tokens = tokenize first ""
        name = head tokens
        calories = read $ tokens !! 1
        protein = read $ tokens !! 2
        carbs = read $ tokens !! 3
        fats = read $ tokens !! 4
        cents = read $ tokens !! 5

stripLeadingWhitespace :: String -> String
stripLeadingWhitespace = unlines . map (dropWhile isSpace) . lines


parseMeals::[String] -> [Ingredient] -> [Meal]
parseMeals [] _ = []
parseMeals (first : rest) db
    | null first = parseMeals rest db
    | head (stripLeadingWhitespace first) == '#' = parseMeals rest db
    | last first == ':' = Meal (take (length first - 1) first) meal (calculateMealMacros meal db) : parseMeals newRest db
    | otherwise = error "expected meal"
    where
        (meal, newRest) = parseMeal rest


parseMeal::[String]->([(Float, String)], [String])
parseMeal [] = ([], [])
parseMeal (first:rest)
    | null first = ([], rest)
    | head (stripLeadingWhitespace first) == '#' = parseMeal rest
    | last first /= ':' = ((quantity, ingredient) : restMeal, newRest)
    | otherwise = ([], rest)
    where
        (restMeal, newRest) = parseMeal rest
        quantity = read $ head $ words first
        ingredient = unwords $ drop 1 $ words first


sumMacros::Macros->Macros->Float->Macros
sumMacros a b f =
    Macros
        (f * calories a + calories b)
        (f * protein a + protein b)
        (f * carbs a + carbs b)
        (f * fats a + fats b)
        (f * cents a + cents b)


calculateMealMacros::[(Float, String)]->[Ingredient]->Macros
calculateMealMacros [] _ = Macros 0 0 0 0 0
calculateMealMacros ((quantity, name):rest) db =
    sumMacros thisMacro restMacro quantity
    where
        thisMacro = ingredientLookup db name
        restMacro = calculateMealMacros rest db


ingredientLookup::[Ingredient] -> String -> Macros
ingredientLookup [] ingredient = error $ "couldn't find ingredient " ++ ingredient
ingredientLookup (first:rest) name =
    if ingredientName first == name then ingredientMacros first
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
        showCalories = (++ " | ") . show . truncate . calories 
        showGrams = (++ "g | ") . show . truncate
        showCurrency = (++ " | $") . (printf "%.2f") . (/ 100) . cents
        totalMacros = getTotalMacros (map mealMacros meals)


concatTimes::String->Int->String->String
concatTimes _ 0 acc = acc
concatTimes x n acc = concatTimes x (n-1) (x ++ acc)


showFloat::Float->String
showFloat x = if abs(fromIntegral(truncate x) - x) < 0.001 then printf "%.0g" x else printf "%g" x


showGroceryList::[(Float, String)]->Float->String
showGroceryList [] _ = ""
showGroceryList ((quantity, name):rest) scale =
    "| " ++ showFloat (scale * quantity) ++ " | " ++ (capitalized name) ++ " | \n" ++ showGroceryList rest scale


capitalized::String->String
capitalized [] = []
capitalized (x:xs) = toUpper x : map toLower xs