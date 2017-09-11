
Spgen
=====

> A small Haskell utility for generating a single page website based on [Skeleton CSS](http://getskeleton.com)

**Notice:** this program was made as a credit project for the Haskell course at
uni and is _really_ simple; it cannot be considered much useful now.
All it is good for currently is that it saves some time when you need just
a very simple webpage with a scroll-down row-and-columns layout.
You can write your page's text using basic html tags and only divide
the sections and columns in them by defined horizontal rules, and
_spgen_ will generate your html and css (so at least you don't have
to deal with divs, their classes and such #yay).

## Installation
Just clone the repository and build this with `cabal install`.
If you don't have Haskell environment installed, ~~you can download
builds for your platform (Windows or UNIX/Ubuntu)~~ (not yet, I will add them).

## Input file syntax
_spgen_ currently doesn't care what you write, doesn't escape anything.
All it recognises and cares about is what is in your sections and how
you devide your text in columns. That means you can use any HTML tags
you want (but at least you don't have to care about all that `<div>`
layout hiearchy.)

Sections ("the big rows") are delimited by a horizontal rule of `:::::`
and columns within the sections by `-----` (at least 5 of them).

Additionally, you can set a colour scheme of a section by specifying
an option `:color:`, `:dark:` or `:default:` (you don't need to) on
a new line after a section delimiter.

Example:
```
::::::::::::::::::::::::::::

This will be a big jumbotron

::::::::::::::::::::::::::::
:colour:

This will be
----------------------------
three columns
----------------------------
in a colourful section.
----------------------------

::::::::::::::::::::::::::::
:dark:

This will be two
----------------------------
columns in a dark section.
```

## Usage
Running the command `spgen` will try to look for `input.txt` and if it
succeeds, it will convert the input file to `input.html` (and it won't have `<title>`).

Program has following options:
- `-t` sets the page's title
- `-a` sets the page's accent colour (note: colourful sections have
  dark text unless you edit css files)
- `-i` allows to specify a different input file name
- `-o` allows to specify a different input file name
- `-?` / `--help` displays, surprise, help

**Make sure to copy the `/css/` folder to the same location as your
output html file.**

## Current state

I plan to continuously add more features to make this thing useful a bit.
You can take it now as it is though, and adapt it to something like
your favourite markdown parser "add-on".

Program part of the documentation is written in Czech, but the code is commented
in English and I'm happy to tell you more at [klara@ksch.cz](mailto:klara@ksch.cz)
(although I doubt anyone would be interested).

I think I will have early Christmas and won't believe my eyes in case of any pull requests; in such an improbable case I'll most probably accept them.
