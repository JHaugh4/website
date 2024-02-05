# Blog Ideas

- Write a blog post explaining the process of writing this website
    - How did I create the Pandoc filter?
    - How did I create the copy code button?
    - How did I create the icon for the copy code button?

- Does where attach to all definitions or only 1? As in when using pattern matching
- How does unfold work?
- Does this recursion have a name? Using takeWhile and dropWhile to section the list.
```haskell
let curSlide = takeWhile (not . headerFilter) bs
    restSlides = dropWhile (not . headerFilter) bs
in curSlide : go restSlides
```