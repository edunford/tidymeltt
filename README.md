
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidymeltt
=========

The goal of tidymeltt is to ...

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

To Dos
------

-   `define_ space/time`: build specific classes for each feature type.
-   `missing()`: build a singular function that reports missingness in the feature set.
-   `nearby()`: select space/time with class rather than name.
-   `partner()`:
    -   gather tax elements with class.
    -   build assessment

Consider
--------

-   Changing `define_` to `def_`

-   Organizing logic centers around a series of `def_` functions (computation/production functions) and `get_` functions (data retrieval functions)
    -   `def_`
        -   `def_time()` - what is time?
        -   `def_space()` - what is space?
        -   `def_taxonomy()` - what variables should entries be compared on?
        -   `def_close()` - what constitutes "close"?
        -   `def_match()` - which events match, given `def_close()` & `def_taxonomy()`
    -   `get_`
        -   `get_missing()` - what data is missing from the evaluation features?
        -   `get_close()` - return data with an index marking all entries that are "near" to other entries in the other bundled datasets.
        -   `get_matches()` - return all data that matched up; also, `get_duplicates()`. Same function.
        -   `get_unique()` - return all data with duplicate entries removed.
        -   `get_data()` - return unbundled data along with any requested features. Also, `unbundle()`.
