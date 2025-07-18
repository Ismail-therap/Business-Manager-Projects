---
title: "Indirect Cost Forecasting"
author: "Md Ismail Hossain"
date: "2025-01-30"
output:
  pdf_document: default
  html_document:
    df_print: paged
---



## R Markdown

![](Indirect-Cost-Forecasting_files/figure-latex/unnamed-chunk-1-1.pdf)<!-- --> 








```
## [1] "RMSE for ETS: 394658.334431439"
```

![](Indirect-Cost-Forecasting_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

```
## [1] "RMSE Comparison:"
```

```
##     Model     RMSE
## 1  SARIMA 287116.1
## 2 Prophet 269539.2
## 3     ETS 394658.3
```

```
## [1] "Best Model Based on RMSE: Prophet"
```
![](Indirect-Cost-Forecasting_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

```
##          Date Forecast Lower CI (95%) Upper CI (95%)
## 43 2025-01-01  1342031        1199848        1489362
## 44 2025-02-01  1367055        1214783        1505256
## 45 2025-03-01  1332550        1199273        1480598
## 46 2025-04-01  1434650        1283088        1574845
## 47 2025-05-01  1785946        1640934        1928690
## 48 2025-06-01  2141244        2003668        2291982
```

