---
title: Haskell Reference Sheet
published: 2023-10-12
---

## Haskell Source File Structure

Haskell files contain a collection of mutually recursive
function. This means that the order you declare your functions
does not matter. Each Haskell file should begin with a capital
letter and end with the *.hs* file extension.

## GHCI (Glasgow Haskell Compiler Interactive)

In order to test the functions in your Haskell source file you will
need to use *ghci*. *ghci* allows you to load your Haskell source file
into an interactive *REPL* (Read Eval Print Loop) in which you can test
your functions.

### Commands To Remember

- ":l" or ":load" : Attempts to load the given Haskell file. Note you do not include *.hs* if you are loading
  from within *ghci*.
- ":r" or ":reload" : Attempts to reload the currently loaded Haskell file.
- ":t" or ":type" : Tells you the type of a given function.
- ":i" or ":info" : Gives you more information about the given function.
- More detailed information can be found [here](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html)

### Loading A File {#load}

To load a file named *HaskellFile.hs* into *ghci* run the
following commands in your terminal:

```bash
cd folder/containing/file
ghci HaskellFile.hs
``` 

This will attempt to load your source file, if there are errors
it will print them out. After you have made changes to the file you must
reload it in *ghci* in order for those changes to be reflected. You can
do this by typing ":r" or ":reload" into *ghci*. This will attempt to reload your file displaying errors if they exist.

### General Workflow

1. Create a Haskell source file or open an existing one
2. Open a terminal in the same directory as the file in step 1
3. Load the file into ghci
4. Make changes to your Haskell source file
5. Attempt to reload the file in ghci
6. Fix any compile errors
7. Test your code
8. Repeat from step 4

## Haskell Language Constructs

### Writing Simple Functions

A simple Haskell function to add 1 to its argument is given as follows:

```haskell
add1 x = x + 1
```

Haskell functions consist of three key pieces:

1. Function name
2. Function arguments
3. Body expression

In this example the function name is "add1", its takes 1 argument "x"
and returns the body expression "x + 1". 

The general shape of a function is 

```haskell
<function_name> <function_arguments> = <function body>
```

You can [load](#load) this file into *ghci* and try to use the function
by typing the following:

```haskell
add1 2
```

Which should report back *3*.

### Types

Haskell is a strictly typed language so where are the types in the
example above? They are there but if we leave them off then Haskell
tries to *infer* the correct type of the function. Before we look at
what type Haskell thinks this expression has let's first give it a
type ourselves. Types in Haskell are written on a separate line above
the function. For example if we wanted to claim that *add1* takes an
*Int* as an argument and returns an *Int* then we would write the
following:

```haskell
add1 :: Int -> Int
add1 x = x + 1
```

This is called a *type signature*. A *type signature* has three key
pieces:

1. Function name
2. Constraints (optional)
3. Function type

A general *type signature* looks as follows:

```haskell
<function_name> :: <function_constraints> => <function_type>
```

Note that in the previous example we did impose any constraints on our
function and we will discuss this feature further down. (TODO: Add link
to constraints/typeclasses)

We can now [load](#load) our file into *ghci* then type:

```{.repl}
:type add1
```

Which should report:

```haskell
add1 :: Int -> Int
```

### Built-in Types

As with any other statically types language Haskell has several built-in
types. We have already seen *Int* put here is a list of most of them:

- Integer - unbounded signed integer
- Int - 32/64 bit signed integer
- Word - 32/64 bit unsigned integer
- Float - single precision IEEE floating point value
- Double - double precision IEEE floating point value
- Bool - boolean value
- Char - single character, written with ''
- String - string which is represented as a list of Char, written with ""

### Type Inference

You may be asking yourself how the original add1 example worked without
you having to explicitly give it a *type signature*. Well Haskell has a
feature called *type inference* which can *infer* the type of most
functions without you having to explicitly state it! This is an
extremely powerful feature of Haskell. So what would type would Haskell
have inferred for the *add1* function if we hadn't stated it explicitly?
Well we can easily find out by removing the *type signature* the 
[loading](#load) the file into *ghci*. Once in *ghci* we can ask for
the type of the function again by typing:

```{.repl}
:type add1
```

It should report:

```haskell
add1 :: Num a => a -> a
```

Hmm doesn't look at all like the *type signature* we gave it, 
*Int -> Int*. What is this *Num* thing?? Remember when I said previously
that constraints were optional in a *type signature*? Well *Num* is an
example of a constraint. Constraints in Haskell are achieved using a
language construct called a *typeclass*. We will go over this topic in
more detail in a later section. (TODO: put typeclass link here)
For now though it is sufficient to realize that Haskell will always
try to infer the most general type. To realize why this is the most
general type ask yourself what operations did *add1* perform on its
argument? Well it used the *+* operator on it. Then ask yourself well
what types does the *+* function expect? To find out ask *ghci*

```{.repl}
:type (+)
```

Should report:

```haskell
(+) :: Num a => a -> a -> a
```
