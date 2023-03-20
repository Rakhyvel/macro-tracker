# Macro Tracker
This application helps plan meal preparations. Simply create a text file with a list of meals and run the macro tracker on it. It will produce an output Markdown file with a table that totals the calories, macros, and cost of each meal. It will also create a weekly grocery list.

## Meals file format
The tracker expects a meals text file.

Meals in the text file are specified by a name followed by a colon and newline, and then a list of quantities of ingredients. Meal names can contain any characters. Quantities can be either integers, decimals, or fractions.
```make
# Comments begin with a `#` and end at the end of the line
# Example meals only, dont take this as nutritional advice!

# Below is an example meal
Macaroni and Cheese:
    1 cup macaroni
    0.5 cup cheddar cheese
    1/2 cup monterrey jack cheese

# Another example meal!
Peanut butter and jelly sandwich:
    2 tbsp peanut butter
    2 tbsp jelly
    2 slice bread
```

## Output
The output of the tracker is a table with each meal as a column, and the macros as rows. A daily total of all meals is also provided as a column.

Here is an example output from the sample meal text file above:
|    | Daily Total | Macaroni and Cheese | Peanut butter and jelly sandwich |
|----|----|----|----|
| __Calories__ | 560 | 350 | 210 |
| __Protein__ | 16g | 10g | 6g |
| __Total Carbohydrates__ | 78g | 50g | 28g | 
| __Total Fat__ | 20g | 11g | 9g |
| __Cost__ | $1.95 | $1.00 | $0.95 |

A grocery list is also provided after the table of meals. The grocery list includes the combined quantities of the ingredients used from all meals, multiplied by 7 for a full week.

Here is an example grocery list output given the above meals text file.
| 7 cup | macaroni |
| 3.5 cup | cheddar cheese |
| 3.5 cup | monterrey jack cheese |
| 14 tbsp | peanut butter | 
| 14 tbsp | jelly |
| 14 slice | bread|

## The `ingredients.csv` file
There is a provided `ingredients.csv` file with a list of common ingredients and their macros. It is where the tracker gets the macros for each ingredient. The `cost` fields may be subject to change with inflation. Feel free to edit the ingredients file, or create your own with the same format.