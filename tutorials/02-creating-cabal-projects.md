---
title: Creating Cabal Projects
published: 2023-10-12
---

# How To Create A Haskell Cabal Project

## Installing Cabal

Use [ghcup](https://www.haskell.org/ghcup/) to install *cabal* if you haven't already. Make sure to note which directory it installs into. To check if it is installed first restart your computer then open a terminal and try typing:

```bash
cabal --version
```

If that runs successfully and shows you a version number then you are good to go.

### Diagnosing Problems

If *cabal* is not showing up in your terminal then it probably means that either:

- The installation was not successful in which case try rerunning [ghcup](https://www.haskell.org/ghcup/)
- The *cabal* folder is not on your *path*.
    - Your *path* variable is used by the command line to know where to search for executables.
    - In order to add to your path variable on windows do the following:
        1. Type into the search bar "environment"
        2. Click on "Edit the system environment variables"
        3. Click on "Environment Variables..." button
        4. Click on "Path" in the upper choice box
        5. Click "Edit..."
        6. Click "New"
        7. Type in the path to the *cabal* executable. Make sure that whichever folder you enter contains the actual executable (i.e. not the folder above it). 
        8. Save/apply everything
        9. Close any existing terminals, reopen a terminal and try to run *cabal* again.
    - In order to add to your path variable on linux/mac do the following:
        1. Open .bashrc which resides in your home folder (~). If you are using another shell then open the corresponding file.
        2. Add the following lines:

            ```bash
            export PATH="path/to/cabal":"$PATH"
            ```

            *Note you need to change the path in the first string to be your systems path to the cabal executable. Also note that the syntax here is very rigid so make sure to copy that exactly. For example you cannot put spaces on either side of the equals.*

        3. Now you can restart a terminal and try running cabal again. You can verify that your path was updated correctly by running the following:

            ```bash
            echo "$PATH"
            ```

            That should show the new path you entered.

## What is cabal?

*Cabal* is a build tool and package manager for Haskell. You can view the documentation [here](https://cabal.readthedocs.io/en/stable/).

## Creating A Project

Now that you have *cabal* make a directory where you want your project to live. Then open a terminal in this directory and run the following command:

```bash
cabal init
```

You will be asked a series of questions in order to start your project. Keep all of the defaults, the only choice you have to make is which license to use. Now that your project is created run the following command:

```bash
cabal run
```

This should first build your project then print out "Hello Haskell!"

## Project Structure

Let's take a look at what was created.

### App Directory

This directory is where all of your Haskell source files should live. When you add a new source file you will need to add it to the [other-modules](#other-modules) field of your .cabal file. Since you will now have to deal with multiple files let's take a quick detour and talk about Haskell modules.

### Haskell Modules

When in a Haskell *cabal* project every file must begin with an explicit module declaration. This is similar to packages in Java. For example let's say you create a file named *Foo.hs* then the beginning of the file would look as follows:

```haskell
module Foo where

...
```

Note that you do not include the file extension here. Also note that the module name must exactly match the name of the file and must be capitalized. Thus, you need to make sure to start all of your Haskell source files with capital letters. I discuss modules more [here](03-haskell-modules.html).

### *.cabal File

Your .cabal file (which is named after the name of your project) contains all of the configuration for you Haskell project. If you accepted all of the defaults then each field should have a verbose description. Let's go over the most important fields:

- All the fields before the *executable* field generally won't need to be changed.
- *executable*: there is really only 2 fields you will likely want to change:
    - *other-modules*: When you add a new Haskell source file to the *app* directory then you also need to add it to this field. *Note that you only add the module name here which is basically the name of the file without .hs.* {#other-modules}
    - *build-depends*: this is where you can import a library. For example if you wanted to use the [gloss](https://hackage.haskell.org/package/gloss) library then you would add "gloss" to this field. Note that this field is a series of comma separated values. Thus, it would likely look like the following:

```haskell
...
executable foo
    ...
    build-depends:  base ^>=4.15.1.0
                  , gloss
    ...
...
```

*Note that the library name is always what is shown in the URL after "package/". Also note that you do not need to include version numbers like you see on base.*

## Building Your Project

Now that you have added a library it is time to rebuild your project. Run the following command at the root of your project:

```bash
cabal build
```

This will pull down any new libraries that you have added as well as compiling your code. You can then run:

```bash
cabal run
```

to rerun your project.

## General Workflow

Note that plain old *ghci* is usually not sufficient in a *cabal* project as it doesn't pull in your libraries. Thus, you should instead use *cabal repl* instead. This command acts the same as *ghci* except that it pulls in your libraries. 

```bash
cabal repl
```

1. First make changes to your source code.
2. Save all changes.
3. Go into your repl and load the file(s) you have been making changes to.
4. See if it compiles, if it doesn't then make the necessary to changes to make it compile.
5. If it does compile then test your code in the repl.
6. Once your code has been compiled and tested in the repl, go back to the command line and run:

    ```bash
    cabal build
    cabal run
    ```

7. Repeat from step 1.

## Further Reading

- [ghcup](https://www.haskell.org/ghcup/)
- [cabal homepage](https://www.haskell.org/cabal/)
- [cabal user guide](https://cabal.readthedocs.io/en/stable/)