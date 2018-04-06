
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidymeltt
=========

There is a wealth of event history data out there. `tidymeltt` makes it easier to match and analyse disparate event datasets.

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

The goal of `tidymeltt` is to provide a robust and efficient toolkit for integrating, understanding, and visualizing georeferenced event data. `tidymeltt` builds off of the methodology of `meltt` (Merging Event Data by Location, Time, and Type) but disaggregates its properties, standardizes its syntax, and increases the efficiency of the integration task. More importantly, `tidymeltt` offers a library of functions to better understand spatio-temporal co-occurrence across entries from different datasets, which makes it easier to leverage co-occurrences to assess patterns regarding overlap and process.

Installation
============

``` r
devtools::install_github("edunford/tidymeltt")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

Development
===========

To Dos
------

-   `define_ space/time`: build specific classes for each feature type.
-   `missing()`: build a singular function that reports missingness in the feature set.
-   `nearby()`: select space/time with class rather than name.
-   `partner()`:
    -   gather tax elements with class.
    -   build assessment
-   rename `partner()` to `def_match()`
-   rename `nearby()` to `def_close()`

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
