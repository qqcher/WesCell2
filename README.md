![](readme_p1.jpg)

# WesCell

<!-- badges: start -->
<!-- badges: end -->

The goal of WesCell is to faciliate and acceralte the process of analyzing sample cells for Wesleyan students. This package is created for people without background knowledge of coding in hope that it will streamline the data manipulation and analysis process. 

## Installation

You can install the released version of WesCell with the following code:

``` r
if (!require(devtools)){
  install.packages("devtools")
 }
 
 devtools::install_github("qqcher/WesCell")
```

## Example

This is a basic example which shows how to import your raw RT-qPCR csv file:

``` r
library(WesCell)
result <- analyze.qPCR("your raw RT-qPCR csv file")
```
The result is in a list form. To convert the list into a dataframe and clean the dataframe, use the following code:
```r
result_df <- list.to.df(result)
result_df <- clean.data(result_df)
```
After the previous two steps, your data should be ready for further analysis. Following are some of the tests you can run:
```r
## Willcoxon Test
Wilcoxon <-  df.wilcoxon.test(result_df,"Primer","inflammatory.factor","exp.data","cell.line")

## 3sub Unique Average
sub_average <- df.3sub.unique.av(df,"Primer","inflammatory.factor","Sample","exp.data","cell.line")

## T-test
T_test <- df.t.test(df,"Primer","inflammatory.factor","exp.data","cell.line")
```

### Details
```r
Authors@R: 
    person(given = "Camille",
           family = "Chossis",
           role = c("aut", "cre"),
           email = "cchossis@wesleyan.edu",
           comment = c(ORCID = "https://orcid.org/0000-0003-4931-9015"))
    person(given = "Cher",
           family = "Qin",
           role = c("aut", "cre"),
           email = "qqin@wesleyan.edu")
Description: Steamline RT-qPCR raw data analysis.
License: MIT + file LICENSE
Encoding: UTF-8
LazyData: true
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.1.1
Imports:
  utils,
  stats
Suggests: 
    knitr,
    rmarkdown
VignetteBuilder: knitr
```
