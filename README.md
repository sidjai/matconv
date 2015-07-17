#matconv

A Utility to convert MatLab / Octave code into R code

The time and headaches that come from migrating legacy code into a new language keeps many from transitioning to R. There are methods to call MatLab code in R, or call MatLab directly in R. There isn't a good translator for the code it self. There are times when you choose the wrong coding language for a project or when you want to migrate a large code base where doing the small differences between languages takes too much time and energy. This program is trying to make that move easier by doing the easier things automatically. R and Matlab and so similar in scope and purpose that such a translator can do more than change syntax but also migrate compatible base functions and choose new data structures for some of the variables.

The code consists of a wrapper function, 'mat2r' to do the various parts of the translation. It does syntax changes automatically but the function converters have to be passed to it directly. These are made elsewhere and are passed to the function as a list. This makes it so that a translation can be as precise and customized to each individual projects needs.

## Installation

```r
install.packages("devtools")
devtools::install_github("sidjai/matconv")
```
