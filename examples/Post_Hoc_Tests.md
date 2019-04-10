Post Hoc Tests From PL Model
================
Stats4SD
10 April, 2019

Use the breadwheat data from gosset library and fit a model as shown in the help menus.

``` r
library(PlackettLuce)
data("breadwheat", package = "gosset")


 R <- to_rankings(breadwheat,
                  items = c("variety_a","variety_b","variety_c"),
                  rankings = c("overall_best","overall_worst"),
                  type = "tricot")

mod1 <- PlackettLuce(R,  npseudo = 0, maxit = 7)
```

    ## Warning in PlackettLuce(R, npseudo = 0, maxit = 7): Iterations have not
    ## converged.

``` r
summary(mod1)
```

    ## Call: PlackettLuce(rankings = R, npseudo = 0, maxit = 7)
    ## 
    ## Coefficients:
    ##           Estimate Std. Error z value Pr(>|z|)    
    ## CSW18       0.0000         NA      NA       NA    
    ## DBW17      -1.1317     0.3001  -3.771 0.000163 ***
    ## DPW621-50  -1.8917     0.3112  -6.079 1.21e-09 ***
    ## HD2824     -2.8406     0.3169  -8.965  < 2e-16 ***
    ## HD2932     -2.1918     0.3076  -7.126 1.04e-12 ***
    ## HD2985     -1.6687     0.3074  -5.428 5.70e-08 ***
    ## HI1563     -2.9586     0.3143  -9.414  < 2e-16 ***
    ## HP1633     -2.8950     0.3119  -9.281  < 2e-16 ***
    ## HW2045     -2.8754     0.3128  -9.191  < 2e-16 ***
    ## K0307      -3.0081     0.3138  -9.585  < 2e-16 ***
    ## K9107       0.7055     0.3568   1.978 0.047979 *  
    ## PBW343     -0.1910     0.3128  -0.611 0.541401    
    ## PBW502     -2.5899     0.3104  -8.345  < 2e-16 ***
    ## PBW550     -2.7426     0.3091  -8.872  < 2e-16 ***
    ## RAJ4120    -2.7677     0.3178  -8.710  < 2e-16 ***
    ## WR544      -2.8108     0.3114  -9.027  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual deviance:  1339 on 1464 degrees of freedom
    ## AIC:  1369 
    ## Number of iterations: 7

Then using multiple comparison functions defined above to give CLD output for estimates of model then plotting with confidence intervals and letters on graph.

Probably need to find a better word than "mean separation" here...

``` r
mean_sep <- multcompPL(mod1)
```

    ## Loading required package: qvcalc

    ## Loading required package: multcompView

``` r
mean_sep
```

    ##             estimate        SE   quasiSE   quasiVar      term .group
    ## K9107      0.7055393 0.3567742 0.3024492 0.09147554     K9107      a
    ## CSW18      0.0000000 0.0000000 0.2495360 0.06226820     CSW18     ab
    ## PBW343    -0.1910391 0.3128219 0.2346848 0.05507698    PBW343      b
    ## DBW17     -1.1317395 0.3001397 0.1905667 0.03631565     DBW17      c
    ## HD2985    -1.6687282 0.3074321 0.1861868 0.03466553    HD2985      d
    ## DPW621-50 -1.8916504 0.3111568 0.1862905 0.03470416 DPW621-50     de
    ## HD2932    -2.1917868 0.3075898 0.1757379 0.03088382    HD2932     ef
    ## PBW502    -2.5898550 0.3103620 0.1770055 0.03133094    PBW502     fg
    ## PBW550    -2.7426351 0.3091179 0.1710423 0.02925547    PBW550      g
    ## RAJ4120   -2.7676585 0.3177575 0.1876729 0.03522113   RAJ4120      g
    ## WR544     -2.8107676 0.3113843 0.1744023 0.03041617     WR544      g
    ## HD2824    -2.8406141 0.3168505 0.1827428 0.03339493    HD2824      g
    ## HW2045    -2.8753580 0.3128359 0.1792736 0.03213904    HW2045      g
    ## HP1633    -2.8950308 0.3119201 0.1759537 0.03095972    HP1633      g
    ## HI1563    -2.9586023 0.3142683 0.1809779 0.03275298    HI1563      g
    ## K0307     -3.0081361 0.3138289 0.1797949 0.03232620     K0307      g

``` r
plot.multcompPL(mean_sep, level = 0.84)+
  ggtitle("'Mean' Separation with 84% Confidence Intervals",
      subtitle = "Non-Overlapping Confidence Intervals can be assumed to be 'significantly' different")
```

    ## Loading required package: ggplot2

![](Post_Hoc_Tests_files/figure-markdown_github/unnamed-chunk-2-1.png)
