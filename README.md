r2ddi
=====

r2ddi is supposed to produce DDI 2.5 compliant XML from datasets in CSV,
Stata and SPSS format.

Currently, only the import for Stata is fully implemented.

Install
-------

    library(devtools)
    devtools::install_github("ddionrails/r2ddi")

Simple to use
-------------

To extract the metadata from multiple Stata files (all located in one
directory) into DDI-compliant XML files, the following two lines of
code are sufficient:

    library("r2ddi")
    dir2xml(path_in="your/path/here", path_out="your/path/here")
