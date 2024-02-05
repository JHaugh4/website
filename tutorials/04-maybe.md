---
title: Representing Failure
published: 2023-11-07
---

Outline:

```{.haskell file=blog-posts/RepresentingFailure.hs}
module RepresentingFailure where

<<safeDiv>>
<<safeHead>>
<<safeTail>>
<<safeHeadPair1>>
<<safeHeadPair2>>
<<caseMatch>>
<<safeHeadPair2CM>>
<<unsafeEverySecondElem>>
<<fish>>
<<flipStructure>>
<<safeEverySecondElem>>

<<safeHeadPair2Do>>
<<safeEverySecondElemTrav>>
```

Representing failure is one of the most important aspects of writing in a programming language. Thus, there are many ways of doing it but today we will focus on three:

1. Magic values
2. Null pointers
3. Exceptions

# Magic Values

In a language such as C functions often return *ints* but how then do you let the caller know that the function failed. Well one possible way is to reserve a special value that when returned signals that something went wrong. This can work but it requires coordination between the caller and callee. This is not ideal as then both must remain in sync otherwise the program might crash. It is also not ideal because you cannot give any context back to the caller about exactly what went wrong.

# Null Pointers

Often referred to as the "billion dollar mistake" by the inventor of the concept, Tony Hoare, null pointers have a nasty reputation. Why is that? Well it is much the same reason as magic values, there always must be a coordination between caller and callee. Null pointers take it a step further though since they can show up in any return type not just numbers! This means that in order for your program to be truly null-safe you must check the output of every function call to make sure it didn't return a null pointer. This is hard to do in practice and makes your program more tedious to write.

# Exceptions

Lastly you can use exceptions to represent failure. This is the best of the three methods other languages use as it does allow you to include context about what went wrong to the caller. However, it can still suffer from the problem of coordination and puts the responsibility on the programmer to check for exceptions when necessary. Some languages such as Java alleviate this with *checked* exceptions which force the programmer to handle exceptions. Although this is a good start the programmer can still choose to essentially ignore the exception by simply printing out the error.

# Haskell's Solution

How does Haskell handle failure? Well you can still use magic values and exceptions in Haskell but it is not advised. Instead we can make use of the type system to encode failure directly. Think about what this datatype might look like? Well we want it to be able to represent when the computation failed and when it succeeded. This is a perfect fit for a *Sum* type! One branch can represent failure and the other success. We will start with a type which holds no information about the failure but we will see how we can add this later. This type is called *Maybe* in Haskell and is defined as follows:

```haskell
data Maybe a = Nothing | Just a
```

One branch to represent failure (*Nothing*) and another to represent success (*Just*). Notice *Maybe* is parameterized by *a*. This is because this datatype is simple a holder for another datatype and doesn't need to know anything about what kind of data it is holding in order to be used. Now how can we use this datatype? Well a classic example is writing a function to safely do division. As we all know we cannot divide by 0 in programming, to avoid this let's write a function which checks if the divisor is 0. If it is then we return *Nothing*, otherwise we return *Just* with the result of the division inside:

```{.haskell #safeDiv}
safeDiv :: Float -> Float -> Maybe Float
safeDiv numerator divisor
    | divisor == 0 = Nothing
    | otherwise    = Just (numerator / divisor)

-- >>> safeDiv 10 5
-- Just 2.0

-- >>> safeDiv 10 0
-- Nothing

```

Hurray! No more runtime errors when dividing! What other functions can we make safer? How about the *head* function?

```{.haskell #safeHead}
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

-- >>> safeHead []
-- Nothing

-- >>> safeHead [1,2,3]
-- Just 1

```

```{.haskell #safeTail}
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

-- >>> safeTail []
-- Nothing

-- >>> safeTail [1,2,3]
-- Just [2,3]

```

Any function which was not total, has a defined output for every input, can now be made total using *Maybe*! Let's see one more example before we talk about the drawbacks of this approach. What if we wanted to take a pair of lists and call *safeHead* on both elements of the pair, what type would this function have? Well your first thought might be a function like this:

```{.haskell #safeHeadPair1}
safeHeadPair1 :: ([a], [b]) -> (Maybe a, Maybe b)
safeHeadPair1 (xs, ys) = (safeHead xs, safeHead ys)

-- >>> safeHeadPair1 ([1,2,3], [4,5])
-- (Just 1,Just 4)

-- >>> safeHeadPair1 ([], [4,5])
-- (Nothing,Just 4)

-- >>> safeHeadPair1 ([1,2,3], [])
-- (Just 1,Nothing)

```

Using this method we get more fine grained control over failure, each computation can fail *independently* of the other. This could be what we want but what if instead we wanted each computation to be *dependent* on the other? Or in other words if one fails then the whole computation fails. What type would the function then have? Perhaps something like this:

```{.haskell #safeHeadPair2}
safeHeadPair2 :: ([a], [b]) -> Maybe (a, b)
safeHeadPair2 (xs, ys) = 
    case safeHead xs of
        Nothing -> Nothing
        Just x  -> 
            case safeHead ys of
                Nothing -> Nothing
                Just y  -> Just (x, y)

-- >>> safeHeadPair2 ([1,2,3], [4,5])
-- Just (1,4)

-- >>> safeHeadPair2 ([], [4,5])
-- Nothing

-- >>> safeHeadPair2 ([1,2,3], [])
-- Nothing

```

It does work, if either list is empty then the whole function returns *Nothing* rather than just one side of the pair. The choice for which approach to use is based entirely on the context of the problem at hand. However, the second approach is more generalizable but also more tedious to write. Is there a way that we could abstract it? Notice that in the *Nothing* case that we simply returned *Nothing* and in the *Just* case we did another case expression but this could really be any arbitrary computation. What we really wanted to get access to was the *a* inside the *Maybe a*. Could we then write a function to do the case expression for us and just give us access to *a*? What type might this function have? Well it would need to take in the *Maybe a* we wish to case match over as well as a function to run on the internal *a*. What does this argument function need to return then? Well it should return a *Maybe b* because remember the *Nothing* case must return *Nothing*. Let's try to write this function:

```{.haskell #caseMatch}
caseMatch :: Maybe a -> (a -> Maybe b) -> Maybe b
caseMatch ma famb =
    case ma of
        Nothing -> Nothing
        Just a  -> famb a
```

Now could we rewrite *safeHeadPair2* using this function? Let's try it:

```{.haskell #safeHeadPair2CM}
safeHeadPair2CM :: ([a], [b]) -> Maybe (a, b)
safeHeadPair2CM (xs, ys) =
    caseMatch (safeHead xs) (\x ->
        caseMatch (safeHead ys) (\y ->
            Just (x, y)))

-- >>> safeHeadPair2CM ([1,2,3], [4,5])
-- Just (1,4)

-- >>> safeHeadPair2CM ([], [4,5])
-- Nothing

-- >>> safeHeadPair2CM ([1,2,3], [])
-- Nothing

```

It works just as before! We still get the right creep problem though, if only there was a *syntactic sugar* for writing this kind of program... but alas let's table that discussion until later. Instead let's look at another *problem* you might run into when using *Maybe*. Let's say you wanted to get the second element of every sublist in a list. First let's write this in an unsafe way:

```{.haskell #unsafeEverySecondElem}
unsafeEverySecondElem :: [[a]] -> [a]
unsafeEverySecondElem xss = map (head . tail) xss

-- >>> unsafeEverySecondElem [[1,2,3], [4,5]]
-- [2,5]

-- >>> unsafeEverySecondElem [[1,2,3], [4], [5,6]]
-- Prelude.head: empty list

```

As expected if a list is not of at least length 2 then we get an exception. How could we write this safely?

```haskell
safeEverySecondElem :: [[a]] -> Maybe [a]
safeEverySecondElem xss = map (safeHead . safeTail) xss
```

This doesn't compile! Why? Well recall the type of *(.)*:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

If we apply it to the arguments we gave it above:

```haskell
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]

(. safeTail) :: (Maybe [a] -> c) -> [a] -> c
```

Therein lies the problem, *safeHead* doesn't take an argument of type *Maybe [a]* it just takes *[a]*! Thus, we need a new way of composing. Our current way is not sufficient for functions which return this extra *structure* such as *Maybe*. What type would this new composition operator have? Perhaps something like this:

```haskell
(<=<) :: (b -> Maybe c) -> (a -> Maybe b) -> a -> Maybe c
(<=<) bmc amb a =
    case amb a of
        Nothing -> Nothing
        Just b  -> 
            case bmc b of
                Nothing -> Nothing
                Just c  -> Just c
```

Look familiar!? This is exactly the pattern we already abstracted! We can use our *caseMatch* function here:

```{.haskell #fish}
(<=<) :: (b -> Maybe c) -> (a -> Maybe b) -> a -> Maybe c
(<=<) bmc amb a = caseMatch (amb a) bmc
```

Awesome! Still not entirely satisfied with the right creep but we can fix that later. So that is 1 problem down now let's try to write our function again:

```haskell
safeEverySecondElem :: [[a]] -> Maybe [a]
safeEverySecondElem xss = map (safeHead <=< safeTail) xss
```

However, there is still an issue! Our composed function *(safeHead <=< safeTail)* is returning a *Maybe a*. We are then passing this to *map*, let's examine the types to see why this is an issue:

```haskell
map :: (a -> b) -> [a] -> [b]
(safeHead <=< safeTail) :: [a] -> Maybe a

map (safeHead <=< safeTail) :: [a] -> [Maybe a]
```

Our type is *[Maybe a]*  instead of *Maybe [a]*! How could we rectify this? Well we need a function to transform from *[Maybe a]* to *Maybe [a]*, let's try to write that:

```{.haskell #flipStructure}
flipStructure :: [Maybe a] -> Maybe [a]
flipStructure [] = Just []
flipStructure (ma:mas) =
    caseMatch ma (\a ->
        caseMatch (flipStructure mas) (\as ->
            Just (a : as)))
```

Hmm a bit subtle what is going on here but essentially we need to figure out if the current element of the list failed, is *Nothing*, and then we need to ask if any other computation in the rest of the list failed, *flipStructure mas*, then and only then can we say that the whole computation was successful and thus we can return just of the list. This function takes some getting used to but we will go over and use it more.

Let's now try to use this function to finish writing our original function:

```{.haskell #safeEverySecondElem}
safeEverySecondElem :: [[a]] -> Maybe [a]
safeEverySecondElem xss = flipStructure $ map (safeHead <=< safeTail) xss

-- >>> safeEverySecondElem [[1,2,3], [4,5], [6,7]]
-- Just [2,5,7]

-- >>> safeEverySecondElem [[1,2,3], [], [6,7]]
-- Nothing

```

Voila it works! Satisfied? You should feel at least intrigued by these ideas I hope but not entirely satisfied. It felt like a lot of work to get here and luckily these ideas can be expressed very easily in Haskell! You remember how I said it would be nice to have an easier way to use the *caseMatch* function? Well the *caseMatch* function is actually called *bind* in Haskell and is written *>>=*. This type of computation is so common that there is a built in way to chain *bind*s together called *do*. We could rewrite the functions where we used *caseMatch* with *do*:

```{.haskell #safeHeadPair2Do}
safeHeadPair2Do :: ([a], [b]) -> Maybe (a, b)
safeHeadPair2Do (xs, ys) = do
    x <- safeHead xs
    y <- safeHead ys
    Just (x, y)

-- >>> safeHeadPair2Do ([1,2,3], [4,5])
-- Just (1,4)

-- >>> safeHeadPair2Do ([], [4,5])
-- Nothing

-- >>> safeHeadPair2Do ([1,2,3], [])
-- Nothing

```

Our *flipStructure* function is also a common pattern and is called *sequence* in Haskell. Additionally the pattern of *map* then *sequence* is also so common it has a function dedicated to it, *traverse*. *traverse* goes by other names as well such as *mapM* or *forM* which is just *flip mapM*. Thus, we could rewrite *safeEverySecondElem* as:

```{.haskell #safeEverySecondElemTrav}
safeEverySecondElemTrav :: [[a]] -> Maybe [a]
safeEverySecondElemTrav xss = traverse (safeHead <=< safeTail) xss

-- >>> safeEverySecondElemTrav [[1,2,3], [4,5], [6,7]]
-- Just [2,5,7]

-- >>> safeEverySecondElemTrav [[1,2,3], [], [6,7]]
-- Nothing

```

--------------------

