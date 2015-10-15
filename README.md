[![Build
Status](https://travis-ci.org/Bart6114/licorice.svg?branch=master)](https://travis-ci.org/Bart6114/licorice)
[![Coverage
Status](https://coveralls.io/repos/Bart6114/licorice/badge.svg?branch=master)](https://coveralls.io/r/Bart6114/licorice?branch=master)

`licorice` is an R package that eases the plotting of *Likert-like*
data. It has been heavily inspired by the `likert`
[package](https://github.com/jbryer/likert) from Bryer and
Speerschneider.

`licorice` makes use of the `ggplot2` plotting engine in such a way that
interference by the `licorice` package in terms of theming is kept to a
minimum (the graphs diplayed belowed are themed using the `ggthemr`
package). When using the `licorice` function a `ggplot2` object is
returned which can then be added upon to your liking.

Installation
------------

For now, no CRAN version exists and you'll have to install from GitHub
using `devtools`.

    devtools::install_github("Bart6114/licorice")

Preparing the data
------------------

The `licorice` function expects a given structure of data. The example
`pisatest` dataset can be used as a reference.

A minimal requirement is the presence of the `question`, `response` and
`count` column. Additionally a `group` column can be added.

    library(licorice)

    ## Warning: replacing previous import by 'tidyr::%>%' when loading 'licorice'

    head(pisatest)

    ##   question response         group count
    ## 1  ST24Q01    Agree        Canada  5623
    ## 2  ST24Q01    Agree        Mexico 12622
    ## 3  ST24Q01    Agree United States  1755
    ## 4  ST24Q01 Disagree        Canada  7938
    ## 5  ST24Q01 Disagree        Mexico 13872
    ## 6  ST24Q01 Disagree United States  1705

For example the `gapsample` dataset is not structured as it should be.
Some simple preparations make it suited for `licorice`.

    head(gapsample)

    ##   StudentId      question          response
    ## 1         1 Financial Aid             Agree
    ## 2         2 Financial Aid Strongly disagree
    ## 3         3 Financial Aid    Agree somewhat
    ## 4         4 Financial Aid    Strongly agree
    ## 5         5 Financial Aid    Strongly agree
    ## 6         6 Financial Aid             Agree

    library(dplyr)
    gap_fixed<-
      gapsample %>%
      group_by(question, response) %>%
      summarise(count = n())

    head(gap_fixed)

    ## Source: local data frame [6 x 3]
    ## Groups: question [1]
    ## 
    ##        question          response count
    ##          (fctr)            (fctr) (int)
    ## 1 Financial Aid Strongly disagree     7
    ## 2 Financial Aid          Disagree     3
    ## 3 Financial Aid Disagree somewhat     2
    ## 4 Financial Aid         Undecided     3
    ## 5 Financial Aid    Agree somewhat     9
    ## 6 Financial Aid             Agree     7

Plotting the data
-----------------

Three main plots are available. First a centered plot is shown; here the
junction between two categories (which can be controlled by the
`middle_pos` parameter) is centered. If the factor levels of the reponse
variable are not set correctly, they can be specified using the
`answer_order` parameter. If a `middle_pos` value of e.g. 2 is given,
the results are centered at the junction between the second and third
response type.

    my_order<-
      c("Strongly disagree","Disagree", "Agree", "Strongly agree")


    licorice(pisatest, answers_order = my_order, middle_pos = 2, type = "center", sort=T)

![](http://i.imgur.com/l7HpVZC.png)

One can also fill the vertical space using a filled plot (also notice
the `sort` argument).

    licorice(pisatest, answers_order = my_order, type = "fill", sort=TRUE)

![](http://i.imgur.com/GmFECzn.png)

We can also have a look at the count data.

    licorice(pisatest, answers_order = my_order, type = "count")

![](http://i.imgur.com/OMR23Fj.png)

You can also show plots in combination with each other using existing
functionality (the `gridExtra` library).

    library(gridExtra)

    grid.arrange(
      licorice(pisatest, my_order, middle_pos = 2.5, type = "center", sort=TRUE),
      licorice(pisatest, my_order, type = "count", sort=TRUE) +
        theme(axis.text.y=element_blank()) +
        scale_fill_discrete(""),
      ncol = 2,
      widths = c(3/4,1/4)
    )

![](http://i.imgur.com/4Utiq6Q.png)

Groups (as the countries in the graph above) are shown automatically
when a `group` column is available in the data set. For example, when
using the generated `gap_fixed` data set (where not `group` column is
present), no group is shown;

    levels(gap_fixed$response)

    ## [1] "Strongly disagree" "Disagree"          "Disagree somewhat"
    ## [4] "Undecided"         "Agree somewhat"    "Agree"            
    ## [7] "Strongly agree"

    licorice(gap_fixed, middle_pos = 4, sort=TRUE) +
      theme(legend.position="right")

![](http://i.imgur.com/FKxp1NA.png)
