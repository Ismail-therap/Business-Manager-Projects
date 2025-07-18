---
title: "Indirect Cost Forecasting"
author: "Md Ismail Hossain"
date: "2025-01-30"
output: pdf_document
---



## R Markdown


```
## Registered S3 method overwritten by 'quantmod':
##   method            from
##   as.zoo.data.frame zoo
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## i Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](Indirect-Cost-Forecasting-Report_files/figure-latex/unnamed-chunk-1-1.pdf)<!-- --> 




```
## Series: ts_data 
## ARIMA(0,0,0)(1,1,0)[12] with drift 
## 
## Coefficients:
##          sar1     drift
##       -0.5272  4342.358
## s.e.   0.1834  1506.108
## 
## sigma^2 = 1.772e+10:  log likelihood = -397.46
## AIC=800.91   AICc=801.84   BIC=805.12
## 
## Training set error measures:
##                    ME     RMSE      MAE         MPE     MAPE      MASE
## Training set 1297.762 108693.3 73537.75 -0.04184224 5.003099 0.5620852
##                     ACF1
## Training set -0.04643894
```

![](Indirect-Cost-Forecasting-Report_files/figure-latex/unnamed-chunk-2-1.pdf)<!-- --> 

```
##          Point Forecast   Lo 80   Hi 80   Lo 95   Hi 95
## Jan 2025        1324539 1153937 1495141 1063625 1585452
## Feb 2025        1347334 1176732 1517936 1086421 1608248
## Mar 2025        1327454 1156852 1498056 1066540 1588367
## Apr 2025        1401024 1230421 1571626 1140110 1661937
## May 2025        1767380 1596778 1937982 1506467 2028294
## Jun 2025        2026608 1856006 2197210 1765694 2287521
```

