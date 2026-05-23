group(a, 1, morning).
group(b, 1, evening).
group(c, 2, morning).
group(d, 5, evening).
group(e, 8, morning).
group(f, 10, evening).
group(g, 3, morning).
group(h, 3, morning).
group(i, 4, evening).
group(j, 4, evening).


% Staff availability 
staff(day(1, 3), 4).   
staff(day(2, 3), 2).   
staff(day(3, 3), 1).   
staff(day(4, 3), 3).   
staff(day(5, 3), 1).   
staff(day(15, 2), 1).
staff(day(17, 2), 5).


% Tables
tables([
    t(small, 2),
    t(medium, 4),
    t(large, 6),
    t(xlarge, 10)
]).


% Recipes
recipe(pasta, [flour, eggs, salt]).
recipe(pizza, [flour, tomato, cheese, basil]).
recipe(salad, [lettuce, tomato, cucumber, olive_oil]).
recipe(burger, [beef, bun, lettuce, tomato, cheese]).
recipe(soup, [broth, vegetables, salt]).
recipe(steak, [beef, butter, garlic]).
recipe(fish, [salmon, lemon, herbs]).
recipe(dessert, [sugar, flour, eggs, chocolate]).


% Orders
order(a, [pasta]).
order(b, [pizza, salad]).
order(c, [pasta, pasta]).
order(d, [pizza, pizza, pasta, salad, dessert]).
order(e, [steak, steak, fish, fish, salad, salad, soup, dessert]).
order(f, [pizza, pizza, pizza, burger, burger, salad, salad, pasta, soup, dessert]).
order(g, [pasta, salad]).
order(h, [pizza, salad]).
order(i, [soup, soup]).
order(j, [fish, steak]).





% a)
check_staff(Day, Time, Reservations) :-
staff(Day, StaffCount),
counttables(Day, Time, Reservations, 0, Count),  
Count =< StaffCount.                      

counttables(_, _, [], Acc, Acc).
counttables(Day, Time, [res(Day, Time, _, _) | Rest], Acc, Count) :-
NewAcc is Acc + 1,
counttables(Day, Time, Rest, NewAcc, Count).
counttables(Day, Time, [res(Dayy,Timee, _, _) | Rest], Acc, Count) :-
(Dayy \= Day ; Timee \= Time),
counttables(Day, Time, Rest, Acc, Count).





% b)	
schedule_all_reservations(Days, Schedule) :- 
setof(group(Name, Count, Time), group(Name, Count, Time), TotalGroups),
 tables(Tables), 
 scheduletabels(TotalGroups, Days, Tables, [], Schedule).
 
 scheduletabels([], _, _, S, S). 
 scheduletabels([group(Name, Count, Time) | Rest], Days, Tables, Acc, Schedule) :- 
 member(Day, Days), 
 member(t(TableName, Capacity), Tables),
 Capacity >= Count,
 \+ member(res(Day, Time, _, TableName), Acc),
 check_staff(Day, Time, [res(Day, Time, Name, TableName) | Acc]),
 scheduletabels(Rest, Days, Tables, [res(Day, Time, Name, TableName) | Acc], Schedule).	
	
	
	
	
% c)	
group_ingredients(GroupName, Ingredients) :-
order(GroupName, AllDishes),
get_allingredients(AllDishes, Ingredients).

get_allingredients([], []).
get_allingredients([Dish | RestDishes], Ingredients) :-
recipe(Dish, DishIngredients),
get_allingredients(RestDishes, RestofIngredients),
append(DishIngredients, RestofIngredients, Ingredients).
	
	
	
	
	
% d)	
needed_ingredients(Reservations, AllIngredients) :-
needed_ingredientshelper(Reservations, [], AllIngredients).

needed_ingredientshelper([], Acc, Acc).
needed_ingredientshelper([res(Day, _, Group, _) | Rest], Acc, AllIngredients) :-
group_ingredients(Group, GroupIngredients),
add_ingredients_by_day(Day, GroupIngredients, Acc, AccN),
needed_ingredientshelper(Rest, AccN, AllIngredients).

add_ingredients_by_day(Day, Ingredients, [], [(Day, Ingredients)]).
add_ingredients_by_day(Day, Ingredients, [(Day, ExistingIngredients) | Rest], [(Day, NewIngredients) | Rest]) :-
append(ExistingIngredients, Ingredients, NewIngredients).
add_ingredients_by_day(Day, Ingredients, [X | Rest], [X | NewRest]) :-
add_ingredients_by_day(Day, Ingredients, Rest, NewRest).

% e)	
	write_reservations_to_csv(Filename, Schedule) :-
    open(Filename, write, Stream),
    write(Stream, 'Day,Month,Time,Group,Table\n'),
    write_rows(Stream, Schedule),
    close(Stream).

write_rows(_, []).

write_rows(Stream, [res(day(D, M), Time, Group, Table) | Rest]) :-
    write(Stream, D),
    write(Stream, ','),
    write(Stream, M),
    write(Stream, ','),
    write(Stream, Time),
    write(Stream, ','),
    write(Stream, Group),
    write(Stream, ','),
    write(Stream, Table),
    write(Stream, '\n'),
    write_rows(Stream, Rest).
	
	
% f)	
	write_ingredients_to_csv(Filename, AllIngredients) :-
    open(Filename, write, Stream),
    write(Stream, 'Day,Month,Ingredients\n'),
    write_ingredient_rows(Stream, AllIngredients),
    close(Stream).

write_ingredient_rows(_, []):-!.

write_ingredient_rows(Stream, [(day(D, M), Ingredients) | Rest]) :-
    write(Stream, D),
    write(Stream, ','),
    write(Stream, M),
    write(Stream, ','),
    write_ingredients_list(Stream, Ingredients),
    nl(Stream),
    write_ingredient_rows(Stream, Rest),!.

write_ingredients_list(_, []:-!.
write_ingredients_list(Stream, [Ingredient]) :-
    !,write(Stream, Ingredient).
write_ingredients_list(Stream, [Ingredient | Rest]) :-
    write(Stream, Ingredient),
    write(Stream, ';'),
    write_ingredients_list(Stream, Rest).







