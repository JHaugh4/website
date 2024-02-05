# Slides Title

```{.haskell h-lines="|1-3,4|8"}
module Foo 
  ( FancyType
  , makeFancyType
  , you
  ) where

-- Test comment
data FancyType = FancyConstructor
  { getFancy :: String 
  }

makeFancyType :: String -> Maybe FancyType
makeFancyType [] = Nothing
makeFancyType s  = Just $ FancyConstructor s
```

# Next Slide

$$ J(\theta_0,\theta_1) = \sum_{i=0} $$

```{.java}
public static void main(String[] args) {
  System.out.println("Hello World!");
}
```

# Next Slide

blah blah

```{.haskell h-lines="4"}
module Foo where

add1 :: Int -> Int
add1 x = x + 1
```

# Another slide

```{.haskell h-lines="|4,5-6,   8 | 9"}
some code
```

# Yet Another Slide
Here is some java code

```{.java h-lines="1|2|3"}
public static void main(String[] args) {
  System.out.println("Hello world!");
  // More Code
  // Even More code
}
```