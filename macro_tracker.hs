import Data.Char
import Text.Printf

-- Todo:
--  [ ] Put on Github
--  [ ] Put meals all on one single table, with the daily total and period total
--  [ ] Better syntax error messages
--  [ ] Add comments to functions
--  [ ] Recognize imperial units, convert between them, output largest unit in grocery list
--  [ ] Add support for parsing fractions in meal text files
--  [ ] Compile to executable, take in input and ingredients files from command line
--  [ ] Add macro in meal file for currency, SI-ability, macro requirements, how long the meal prep 'week' is
--  [ ] Finalize

-- Represents one ingredient as read in from the ingredients csv
-- Has a name and a list of macros
data Ingredient = Ingredient {
    ingredientName::String,
    ingredientMacros::Macros -- TODO: include units
} deriving Show

-- Represents an entire meal as read in from the meal text file
-- Has a name, a list of ingredients, and a total amount of macros
data Meal = Meal {
    mealName::String,
    ingredients::[(Float, String)], -- TODO: include units
    mealMacros::Macros
} deriving Show

-- Represents the macros that are tracked in the macro tracker
-- Calories are in kiloCalories
-- Protein, carbs, and (total) fat are in grams. (sat, unsat, trans fats are not distinguished)
-- Cents are in United States cents
data Macros = Macros {
    calories::Float,
    protein::Float,
    carbs::Float,
    fats::Float,
    cents::Float -- TODO: Add localization?
} deriving Show


-- Starts off the program
main :: IO ()
main = do
    ingredient_contents <- readFile "ingredients.csv"
    meal_contents <- readFile "meals.txt"
    let db = parseIngredients $ lines ingredient_contents
    let meals = parseMeals (lines meal_contents) db
    let groceryList = combineGroceryLists $ concatMap ingredients meals
    let output = "## Meals:\n" ++ showMealList meals ++ "## Daily Total: \n" ++ showMacros (getTotalMacros $ map mealMacros meals) ++ "\n\n" ++ "## Weekly Grocery List:\n|   |   |\n|---|---|\n" ++ showGroceryList groceryList 7
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
showMealList (Meal name ingredients macros:rest) =
    "### " ++ name ++ ":\n" ++ showMacros macros ++ "\n" ++ showMealList rest


showFloat::Float->String
showFloat x = if abs(fromIntegral(truncate x) - x) < 0.001 then printf "%.0g" x else printf "%g" x


showIngredients::[(Float, String)]->String->String
showIngredients [] _ = ""
showIngredients ((quantity, name):rest) prefix = prefix ++ showFloat quantity ++ " " ++ name ++ "\n" ++ showIngredients rest prefix


showMacros::Macros->String
showMacros (Macros calories protein carbs fats cents) =
    "|   |   |\n|---|---|\n"
        ++ "| Calories | " ++ (show (truncate calories)) ++ " | \n" 
        ++ "| Protein | " ++ (show (truncate protein)) ++ "g | \n"  
        ++ "| Total Carbohydrates | " ++ (show (truncate carbs)) ++ "g | \n" 
        ++ "| Total Fat | " ++ (show (truncate fats)) ++ "g | \n" 
        ++ "| Cost | $" ++ (printf "%.2f"  (cents / 100)) ++ " | \n" 


showGroceryList::[(Float, String)]->Float->String
showGroceryList [] _ = ""
showGroceryList ((quantity, name):rest) scale =
    "| " ++ showFloat (scale * quantity) ++ " | " ++ (capitalized name) ++ " | \n" ++ showGroceryList rest scale


capitalized::String->String
capitalized [] = []
capitalized (x:xs) = toUpper x : map toLower xs