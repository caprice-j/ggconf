<!-- README.md is generated from README.Rmd. Please edit that file -->
ggconf
======

[![Travis-CI Build Status](https://travis-ci.org/caprice-j/ggconf.svg?branch=master)](https://travis-ci.org/caprice-j/ggconf) [![Build status](https://ci.appveyor.com/api/projects/status/0tfqjechyio538um?svg=true)](https://ci.appveyor.com/project/caprice-j/ggconf) [![codecov](https://codecov.io/gh/caprice-j/ggconf/branch/master/graph/badge.svg)](https://codecov.io/gh/caprice-j/ggconf) ![](http://www.r-pkg.org/badges/version/ggconf) <!-- [![Coverage Status](https://coveralls.io/repos/github/caprice-j/ggbash/badge.svg)](https://coveralls.io/github/caprice-j/ggbash) --> [![Issue Count](https://codeclimate.com/github/caprice-j/ggconf/badges/issue_count.svg)](https://codeclimate.com/github/caprice-j/ggconf/issues) [![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

ggconf provides `theme2()`, a flexible `ggplot2::theme()` interface.

Usage
-----

``` r
g <- ggplot(iris) + geom_point(aes(Sepal.Width, Sepal.Length))

g + theme2(ax.txt(sz=20, f="bold"), ax.ln(col='gray60', sz=2), panel.bg(fill="white"))
```

![](README-example-1.png)

The following ggplot2 command generates the same plot.

``` r
g + ggplot2::theme(axis.text = element_text(size=20, face="bold"),
                   axis.line = element_line(colour="gray60", size=2),
                   panel.background = element_rect(fill="white"))
```

### Getting Started

If you replace your `ggplot2::theme()` call with `ggconf::theme2()` call, ggconf would work. All of the followings return the same plot.

``` r
g + theme( axis.text = element_text(size=20, face="bold")) # Style 1: ggplot2 default (50 characters)
g + theme2(axis.text = element_text(size=20, face="bold")) # Style 2: ggconf
g + theme2(axis.text(size=20, face="bold"))                # Style 3: ggconf without element_text()
g + theme2(ax.txt(sz=20, face="bold"))                     # Style 4: ggconf shorter but readable
g + theme2(at(z=20, f="bold"))                             # Style 5: ggconf shortest (25 chars)
```

Features
--------

![ggconf Feature Overview](README-func.png)

### Partial Match

Even if the unique identification is not possible for specified elements (e.g. theme element names or arguments), `ggconf` tries to execute its best estimate instead of just returning an error.

For the input `theme2(ax.txt(sz=20, fc="bold"), ax.ln(col='gray60'), panel.bg(fill="white"))`, ggconf performs partial matches six times.

-   **theme element names**
    -   `ax.txt` matches `axis.text`. You can even write `a.t` or `at`.
    -   `ax.ln` matches `axis.line`. You can even write `a.l` or `al`.
    -   `panel.bg` matches `panel.background`. You can even write `pnl.bg`.
        -   `p.bg` matches `plot.background` according to edit distance.
-   **theme configuration arguments**
    -   `sz` matches `size`.
    -   `f` matches `face` (fontface).
        -   `fill` needs to write not just `f` but `fi`.
    -   `col` matches `colour`.

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("caprice-j/ggconf")
```

-   If you get `no appender.console()` error, you might need `install.packages('rly')`. `packageVersion('rly')` should be at least 1.4.2.

-   This package is still in its infancy and might contain several installation bugs.

Examples
--------

#### Raw ggplot2 plot

``` r
suppressPackageStartupMessages(library(dplyr))
gg <- ggplot(mtcars[1:20, ] %>% tibble::rownames_to_column() %>% 
             mutate(car_name = rowname, maker = gsub(" .*", "", car_name) ) ) + 
      #geom_label(aes(mpg, qsec, label = substr(car_name, 1, 13), color=maker),
      geom_point(aes(mpg, qsec, color=maker), size=8) +
      geom_text(aes(mpg, qsec, label=substr(maker, 1, 2)), color="white", fontface="bold") +
      labs(title = "Motor Trend Car Road Tests",
           subtitle = "Top 20 rows are extracted for demonstration", 
           caption = "Source: 1974 Motor Trend US magazine") + 
           scale_x_continuous(breaks=seq(10,34, 4))
gg
```

![](README-unnamed-chunk-7-1.png)

``` r
gg + 
  theme2(
       text(f="bold", z=24, fmly="Times New Roman"),      # make all text thicker/larger 
       pnl.bg(fill="white"),
       lgd.box.margin(.2, .2, .2, .2, "cm"),
       lgd.box.bg(c="black"),
       lgd.key(fill="white"),
       lgd.position("bottom"),
       lgd.title(fmly="Consolas", c="royalblue"),         # equally-spaced font
       lgd.txt(z=rel(.8)),
       axs.title(fmly="Consolas", c="royalblue"),         # colorize axis titles
       axs.title.y(angle=0, vjust=.5),                    # rotate and centerize y axis label
       axs.txt(z=rel(1.1)),
       axs.line(arrow=arrow(type="open", angle=20), z=2), # 
       axs.tick(z=1),                                     # tick or ticks? It doesn't matter
       axis.tick.len(.5, "cm"),
       plt.subttl(f="italic", hjust=1),
       plt.margin(.3, .3, .3, .1, "inch")                # adjust margins
  )
```

![](README-unnamed-chunk-9-1.png)

``` r
# If using ggplot2::theme():
# gg +
#  theme(text = element_text(face="bold", size=24, family="Times New Roman"), 
#        panel.background = element_rect(fill="white"),
#        legend.box.margin = margin(.2, .2, .2, .2,"cm"), 
#        legend.box.background = element_rect(colour="black"), 
#        legend.key = element_rect(fill="white"), 
#        legend.position = ("bottom"), 
#        legend.title = element_text(family="Consolas", colour="royalblue"), 
#        legend.text = element_text(size=rel(.8)), 
#        axis.title = element_text(family="Consolas", colour="royalblue"), 
#        axis.title.y = element_text(angle=0, vjust=0.5), 
#        axis.text = element_text(size=rel(1.1)), 
#        axis.line = element_line(arrow=arrow(type="open", angle=20), size=2), 
#        axis.ticks = element_line(size=1), 
#        axis.ticks.length = unit(.5,"cm"), 
#        plot.subtitle = element_text(face="italic", hjust=1), 
#        plot.margin = margin(.3, .3, .3, .1,"inch")
#        )
```

Goals
-----

The goal of ggconf is to make it less stressful to finalize your plots. + adjust colours or lineweights + rotate axis labels + decide tick label intervals and limits

<!--    + generate line-wrapped titles or legends -->
Learning ggconf
---------------

`ggconf` follows original ggplot2 syntax as much as possible for reducing learning costs of current ggplot2 users.

Learning ggplot2 might be the best way to understand ggbash syntax. The [document](http://docs.ggplot2.org/current/) and [book](https://github.com/hadley/ggplot2-book) of ggplot2 would be helpful.

Other Works
-----------

`ggconf` draws inspiration from some other higher level programming languages including Bash, CoffeeScript, Ruby, and Lisp. <!-- Fixit is inspired by [Fix-It Hints](http://clang.llvm.org/docs/InternalsManual.html#fix-it-hints) in clang C++ compiler. -->

Current Implementation Status
-----------------------------

ggbash is first released on August 24, 2017.

-   DONE:
    -   version 0.1 : lightweight port from ggbash
