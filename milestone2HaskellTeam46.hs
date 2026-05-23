data Month = Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec   deriving (Show, Eq)

type Date = (Int, Month, Int)
type Price = Float
type Quantity = Int

type Supply = (String, Quantity, Price)
--	representing the name of the ingredient, quantity needed of that ingredient, and the total price of the needed quantity

type Delivery = (Date, [Supply]) 
-- date of delivery the restaurant will make and the required supply on that date.
 

data Ingredient = 
    SimpleIngredient String  -- A basic ingredient
  | Recipe String [Ingredient] deriving (Show, Eq) 
  -- An ingredient consisting of other ingredients


data Expense = 
    Item String Price Date              
    -- A single expense item
  | Category String [Expense]     
  -- A category of expenses that could contain expenses or other categories.
  deriving (Show, Eq)
    

ingredient_info :: [(String, Int, Price)]
ingredient_info = [("rice", 20, 1.2), ("apples", 5, 5), ("flour", 1, 0.5), ("eggs",1, 2), ("butter", 3, 12), ("garlic", 11, 4.5), ("salt", 0,0.25), ("pepper", 66,0.75), ("sugar", 7, 6), ("goat_meat", 20, 1.2)]

shopping_list :: [(Date, [Ingredient])]
shopping_list = [((15,Feb,2026),
                    [SimpleIngredient "flour",
                     SimpleIngredient "eggs",
                     SimpleIngredient "rice"]),
                 ((17,Feb,2026),
                    [SimpleIngredient "sugar",
                     SimpleIngredient "butter",
                     SimpleIngredient "flour",
                     SimpleIngredient "flour",
                     (Recipe "dough" [(SimpleIngredient "flour"),
                                      (SimpleIngredient "eggs")])]),
                 ((5,Mar,2026),
                    [SimpleIngredient "salt",
                     SimpleIngredient "pepper",
                     SimpleIngredient "garlic"]) ]
-- Do not submit any code above this line
-- Do not move any data below this line
-- ////////////////////////////////////////////////////////////////////////////////

-- Start your code here
--a)
prev Jan = Dec
prev Feb = Jan
prev Mar = Feb
prev Apr = Mar
prev May = Apr
prev Jun = May
prev Jul = Jun
prev Aug = Jul
prev Sep = Aug
prev Oct = Sep
prev Nov = Oct
prev Dec = Nov
days Jan = 31
days Feb = 28
days Mar = 31
days Apr = 30
days May = 31
days Jun = 30
days Jul = 31
days Aug = 31
days Sep = 30
days Oct = 31
days Nov = 30
days Dec = 31

sub (day,month,year) x
    | x == 0 = (day, month, year)
    | day > x = (day-x, month, year)
    | month == Jan = sub (days Dec, Dec, year-1) (x - day)
    | otherwise = sub (days (prev month), prev month, year) (x - day)

getInfo x ((i,d,p):t) | x == i = (d,p)
    |otherwise = getInfo x t

flatten [] = []
flatten (SimpleIngredient h : t) = h : flatten t
flatten (Recipe _ l :t) = flatten l ++ flatten t

helper _ [] = []
helper date (h:t) =
    (sub date a,(h,b)) : helper date t
    where (a,b) = getInfo h ingredient_info

calculateDeliveryDates :: Date -> [Ingredient] -> [(Date, (String, Price))]
calculateDeliveryDates date list = helper date (flatten list)

	
--b)
dateNum (d, m, y) = y * 10000 + monthNum m * 100 + d
monthNum Jan = 1
monthNum Feb = 2
monthNum Mar = 3
monthNum Apr = 4
monthNum May = 5
monthNum Jun = 6
monthNum Jul = 7
monthNum Aug = 8
monthNum Sep = 9
monthNum Oct = 10
monthNum Nov = 11
monthNum Dec = 12

isBeforeDate d (other,_) = dateNum other < dateNum d
isAfterDate d (other,_) = dateNum other > dateNum d
isSameDate d (other,_) = dateNum other == dateNum d
isDiffDate d (other,_) = dateNum other /= dateNum d

isSameName name (other,_) = other == name
isDiffName name (other,_) = other /= name
isBeforeName name (other,_,_) = other < name
isAfterName name (other,_,_) = other > name

extractInfo (_,i) = i

intToFloat 0 = 0
intToFloat n = 1 + intToFloat (n-1)

sortByDate [] = []
sortByDate ((d,s):t) = sortByDate (filter (isBeforeDate d) t) ++ [(d,s)] ++ sortByDate (filter (isAfterDate d) t)

sortByName [] = []
sortByName ((n,q,p):t) = sortByName (filter (isBeforeName n) t) ++ [(n,q,p)] ++ sortByName (filter (isAfterName n) t)

groupByDate [] = []
groupByDate ((d,info):rest) = (d, info : map extractInfo (filter (isSameDate d) rest)) : groupByDate (filter (isDiffDate d) rest)

mergeIngredients [] = []
mergeIngredients ((name,p):rest) = (name, qty, p * intToFloat qty) : mergeIngredients remaining
  where
    duplicates = filter (isSameName name) rest
    remaining  = filter (isDiffName name) rest
    qty = 1 + length duplicates

getPairsForDate date (d,ings) = if d == date then calculateDeliveryDates date ings else []

collectAllPairs [] = []
collectAllPairs (date:rest) = foldr (++) (collectAllPairs rest) (map (getPairsForDate date) shopping_list)

buildDelivery (d,grp) = (d, sortByName (mergeIngredients grp))

summarizeAllDeliveries :: [Date] -> [Delivery]
summarizeAllDeliveries dates = sortByDate (map buildDelivery (groupByDate (collectAllPairs dates)))

--c)
getDeliveryExpenses ::[Delivery] -> Expense
getDeliveryExpenses deliveries = Category "Food supplies" (getItems deliveries)
getItems[]=[]
getItems ((date,supplies):rest) = map (toItem date) supplies ++ getItems rest
toItem date (name,qty,price) = Item name price date

--d)
mostPopularDish ::[String] -> [String]
mostPopularDish [] = []
mostPopularDish dishes = removeDups (map fst (filter isMax counts))
  where
    counts = map countDish dishes
    countDish dish =(dish, length (filter(== dish) dishes))
    isMax (dish,count)= count == maxCount
    maxCount = foldr getMax 0 counts
    getMax (dish,count) acc = if count > acc then count else acc
    removeDups []=[]
    removeDups (h:t)= h:removeDups (filter(/=h)t)
	
--e)
calculateTotalExpenses :: Expense -> Price
calculateTotalExpenses(Item _ price _ )= price
calculateTotalExpenses(Category _ expenses)= foldr (+) 0 (map calculateTotalExpenses expenses)

--f)
countCategoryItems :: String -> Expense -> Int
countCategoryItems target (Item _ _ _)= 0
countCategoryItems target (Category name expenses)
    | name==target= countItems expenses
    | otherwise =sum(map (countCategoryItems target) expenses)
  where
    countItems []= 0
    countItems (Item _ _ _ :t)= 1 + countItems t
    countItems (Category _ m :t)= countItems m + countItems t
		