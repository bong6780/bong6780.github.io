---
title: "Welcome to Jekyll!"
date: 2020-10-12 00:48:00 -0400
categories: jekyll update
---

Causal forest application
================

-----

# Grow a forest

``` r
( Y.forest = regression_forest(X, Y, clusters = school.id, equalize.cluster.weights = TRUE) )
```

    ## GRF forest object of type regression_forest 
    ## Number of trees: 2000 
    ## Number of training samples: 10391 
    ## Variable importance: 
    ##     1     2     3     4     5     6     7     8     9    10    11    12    13 
    ## 0.711 0.029 0.015 0.062 0.021 0.031 0.026 0.050 0.003 0.006 0.000 0.011 0.003 
    ##    14    15    16    17    18    19    20    21    22    23    24    25    26 
    ## 0.000 0.000 0.000 0.000 0.000 0.000 0.001 0.000 0.003 0.001 0.002 0.008 0.003 
    ##    27    28 
    ## 0.006 0.006

``` r
Y.hat = predict(Y.forest)$predictions
```

``` r
( W.forest = regression_forest(X, W, clusters = school.id, equalize.cluster.weights = TRUE) )
```

    ## GRF forest object of type regression_forest 
    ## Number of trees: 2000 
    ## Number of training samples: 10391 
    ## Variable importance: 
    ##     1     2     3     4     5     6     7     8     9    10    11    12    13 
    ## 0.236 0.048 0.057 0.134 0.090 0.092 0.088 0.095 0.020 0.024 0.001 0.030 0.004 
    ##    14    15    16    17    18    19    20    21    22    23    24    25    26 
    ## 0.000 0.000 0.001 0.000 0.001 0.001 0.005 0.001 0.017 0.003 0.006 0.011 0.013 
    ##    27    28 
    ## 0.011 0.011

``` r
W.hat = predict(W.forest)$predictions
```

  W.hat은 Propensity score.

``` r
cf.raw = causal_forest(X, Y, W,
                       Y.hat = Y.hat, W.hat = W.hat,
                       clusters = school.id,
                       equalize.cluster.weights = TRUE)
varimp = variable_importance(cf.raw)
selected.idx = which(varimp > mean(varimp))
```

  전체 covariate을 포함한 RF를 구축한 뒤 variable importance가 전체 평균 보다 높은 변수만 남김.

-----

# Result

  “W.hat = W.hat”부분이 Propensity score를 고려하는 부분이며, “clusters =
school.id”가 Cluster를 고려하는 부분.   Propensity score를 고려하지 않을 때는 W.hat
대신 mean(W.hat)을 사용.

## with Propensity score & with Cluster

``` r
( cf = causal_forest(X[,selected.idx], Y, W,
                   Y.hat = Y.hat, W.hat = W.hat,
                   clusters = school.id,
                   equalize.cluster.weights = TRUE,
                   tune.parameters = "all") )
```

    ## GRF forest object of type causal_forest 
    ## Number of trees: 2000 
    ## Number of training samples: 10391 
    ## Variable importance: 
    ##     1     2     3     4     5     6     7     8     9 
    ## 0.125 0.043 0.041 0.154 0.141 0.147 0.152 0.143 0.052

``` r
ATE = average_treatment_effect(cf) 
( ATE.yy = paste("95% CI for the ATE:", round(ATE[1], 3),
      "+/-", round(qnorm(0.975) * ATE[2], 3)) )
```

    ## [1] "95% CI for the ATE: 0.248 +/- 0.04"

## with Propensity score & without Cluster

``` r
cf.noclust = causal_forest(X[,selected.idx], Y, W,
                           Y.hat = Y.hat, W.hat = W.hat,
                           tune.parameters = "all")
ATE.noclust = average_treatment_effect(cf.noclust)
( ATE.yn = paste("95% CI for the ATE:", round(ATE.noclust[1], 3),
      "+/-", round(qnorm(0.975) * ATE.noclust[2], 3)) )
```

    ## [1] "95% CI for the ATE: 0.254 +/- 0.022"

## without propensity score / with cluster

``` r
cf.noprop = causal_forest(X[,selected.idx], Y, W,
                          Y.hat = Y.hat, W.hat = mean(W),
                          tune.parameters = "all",
                          equalize.cluster.weights = TRUE,
                          clusters = school.id)
ATE.noprop = average_treatment_effect(cf.noprop)
( ATE.ny = paste("95% CI for the ATE:", round(ATE.noprop[1], 3),
      "+/-", round(qnorm(0.975) * ATE.noprop[2], 3)) )
```

    ## [1] "95% CI for the ATE: 0.254 +/- 0.041"

## without propensity score / without cluster

``` r
cf.nono = causal_forest(X[,selected.idx], Y, W,
                          Y.hat = Y.hat, W.hat = mean(W),
                          tune.parameters = "all")
ATE.nono = average_treatment_effect(cf.nono)
( ATE.nn = paste("95% CI for the ATE:", round(ATE.nono[1], 3),
      "+/-", round(qnorm(0.975) * ATE.nono[2], 3)) )
```

    ## [1] "95% CI for the ATE: 0.258 +/- 0.022"

## with Propensity score & with Cluster (Logistic ver.)

``` r
logi = cbind.data.frame(W, X)
logi.res = glm(factor(W) ~ ., data = logi, family = binomial)
W.logi = logi.res$fitted.values
( cf.logi = causal_forest(X[,selected.idx], Y, W,
                   Y.hat = Y.hat, W.hat = W.logi,
                   clusters = school.id,
                   equalize.cluster.weights = TRUE,
                   tune.parameters = "all") )
```

    ## GRF forest object of type causal_forest 
    ## Number of trees: 2000 
    ## Number of training samples: 10391 
    ## Variable importance: 
    ##     1     2     3     4     5     6     7     8     9 
    ## 0.115 0.042 0.039 0.173 0.139 0.136 0.152 0.158 0.047

``` r
ATE.logi = average_treatment_effect(cf.logi) 
( ATE.logistic = paste("95% CI for the ATE:", round(ATE.logi[1], 3),
      "+/-", round(qnorm(0.975) * ATE.logi[2], 3)) )
```

    ## [1] "95% CI for the ATE: 0.237 +/- 0.042"

  Regression forest를 이용하여 Propensity score를 추정할 때 Cluster를 고려하였으나,
Logistic은 그렇지 않았다는 차이가 있음.

<img src="Causal-forest-application_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

## Standard regression model

``` r
reg <- cbind.data.frame(Y, W, X[,selected.idx])
lm.result <- lm(Y ~ W + ., data = reg)
out <- summary(lm.result)
out$coefficients["W", 1:2]
```

    ##   Estimate Std. Error 
    ## 0.25742514 0.01148065

``` r
( ATE.lm = paste("95% CI for the ATE:", round(out$coefficients["W",1], 3),
                       "+/-", round(out$coefficients["W", 2], 3)) )
```

    ## [1] "95% CI for the ATE: 0.257 +/- 0.011"

## Mixed regression model - 절편, 기울기

``` r
mix <- cbind.data.frame(school.id, Y, W, X[,selected.idx])
mix.result <- lmer(Y ~ W + (1 + W | school.id) + ., data = mix)
out <- summary(mix.result)
out$coefficients["W", 1:2]
```

    ##   Estimate Std. Error 
    ## 0.24663975 0.01828458

``` r
( ATE.mix = paste("95% CI for the ATE:", round(out$coefficients["W",1], 3),
                 "+/-", round(out$coefficients["W", 2], 3)) )
```

    ## [1] "95% CI for the ATE: 0.247 +/- 0.018"

-----

# Summary

  CRF, with ps / with cluster :  
     95% CI for the ATE: 0.248 +/- 0.04

  CRF, with ps / without cluster :  
     95% CI for the ATE: 0.254 +/- 0.022

  CRF, without ps / with cluster :  
     95% CI for the ATE: 0.254 +/- 0.041

  CRF, without ps / without cluster :  
     95% CI for the ATE: 0.258 +/- 0.022

  CRF, using logistic ps / with cluster :  
     95% CI for the ATE: 0.237 +/- 0.042

  Standard regression :  
     95% CI for the ATE: 0.257 +/- 0.011

  Mixed regression, 절편 + 기울기 :  
     95% CI for the ATE: 0.247 +/- 0.018
