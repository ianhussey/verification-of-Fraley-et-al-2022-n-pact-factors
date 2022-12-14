## Assessment of the computational reproducibility of Fraley et al. (2022) Journal N-Pact Factors From 2011 to 2019: Evaluating the Quality of Social/Personality Journals With Respect to Sample Size and Statistical Power



## Description of files

Brief report on errors detected can be found in communications/assessment of computational reproducibility.docx

Refactored and corrected code can be found in code/analyses_refactored.Rmd

## Summary

I was interested in conducting an N-Pact Factor analysis in a different domain, and therefore downloaded Fraley et al.’s (2022) code to attempt to reuse it. Before I did so, in order to check that I understood the code I assessed its computational reproducibility by comparing the results generated by the R script to those reported in the manuscript. No attempt was made to compare Fraley’s dataset to the results reported in the original articles (i.e., to re-extract any sample sizes or designs).

The authors’ substantive conclusions are reproducible from the their data and code. However, a subset of results were not computationally reproducible, and another subset of results were reproducible but contained errors (i.e., reproducible errors in the calculation of results, not their reporting). 