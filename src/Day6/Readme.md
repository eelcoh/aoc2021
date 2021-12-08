# Explanation

## Naive implementation
The first implementation was a naive one:
```elm
rollNaive : Int -> Fish -> Fish
rollNaive days fish =
    if days <= 0 then
        fish

    else
        rollNaive (days - 1) (rollOnceNaive fish)

rollOnceNaive : Fish -> Fish
rollOnceNaive fish =
    case fish of
        x :: rest ->
            let
                newX =
                    x - 1
            in
            if newX < 0 then
                6 :: 8 :: rollOnceNaive rest

            else
                newX :: rollOnceNaive rest

        [] ->
            []

```
It is a simple tail call recursion, that in the end checks if the fish value is zero, and if so, replaces it by two fish with values 6 and 8, and otherwise just decreases the value with 1.

It works, up till a certain number of days or a certain number of fish. Then it will run into an overflow.

So there must be another way. And there is.

## Less naive
The first thing to notice is that the fish spawn independently. One fish does not need other fish to spawn. That means you can map the functions to all the fish.

The second thing to notice is that the spawn values are never higher than 8.

Take one and two and it suddenly makes sense to calculate the number of offspring for spawn values [0..8], and then for any value in the input lookup the calculated offspring, and sum them.

That worked fine for 80 days. But it did not for 256 days, it would still overflow because the size of the offspring after so many generations becomes huge.

## Optimised
That brings us to the third thing to notice: if we can calculate the offspring for 80 days, we can also calculate it for 1 day. And if we have the calculation for 1 day, we can base the calculation for 2 days on that, and so on to day n+1. We do not need to keep track of the values for day n once we have calculated the values for n+1. That means we can have a very limited data structure, where we basically keep the function spawn -> offspring, starting with an initial table.

```elm
-- initial
[ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 3, 1 ), ( 4, 1 ), ( 5, 1 ), ( 6, 1 ), ( 7, 1 ), ( 8, 1 ) ]
-- after one cycle (only value changed is at 0)
[ ( 0, 2 ), ( 1, 1 ), ( 2, 1 ), ( 3, 1 ), ( 4, 1 ), ( 5, 1 ), ( 6, 1 ), ( 7, 1 ), ( 8, 1 ) ]
-- after two cycles (value 1 changed)
[ ( 0, 2 ), ( 1, 2 ), ( 2, 1 ), ( 3, 1 ), ( 4, 1 ), ( 5, 1 ), ( 6, 1 ), ( 7, 1 ), ( 8, 1 ) ]
-- after three cycles (value 2 changed)
[ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ), ( 3, 1 ), ( 4, 1 ), ( 5, 1 ), ( 6, 1 ), ( 7, 1 ), ( 8, 1 ) ]
-- ...
-- after ten cycles
[ ( 0, 4 ), ( 1, 3 ), ( 2, 3 ), ( 3, 2 ), ( 4, 2 ), ( 5, 2 ), ( 6, 2 ), ( 7, 2 ), ( 8, 2 ) ]
-- 40 cycles
[ ( 0, 48 ), ( 1, 38 ), ( 2, 38 ), ( 3, 33 ), ( 4, 32 ), ( 5, 31 ), ( 6, 27 ), ( 7, 27 ), ( 8, 21 ) ]
```

So you see the growth in the values starts at the left (0) and slowly moves to the right (8).


and after 256 days it is:
```elm
[(0,6703087164)
,(1,6206821033)
,(2,5617089148)
,(3,5217223242)
,(4,4726100874)
,(5,4368232009)
,(6,3989468462)
,(7,3649885552)
,(8,3369186778)
]
```

Then we take the input:
```
[5,1,4,1,5,1,1,5,4,...
```
and lookup the values in that table and sum these values:
```elm
[ 4368232009
, 6206821033
, 4726100874
, 6206821033
, 4368232009
, 6206821033
, 6206821033
, 4368232009
, 4726100874
,...
]
|> List.sum
```

So in the end, all we do is calculate the outcomes for the possible input values first, and then for all input values look these up, and accumulate these values.