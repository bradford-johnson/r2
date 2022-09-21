# R Script
``` r
Q = quantile(outlier_df$var1.1, probs = c(.25,.75), na.rm = FALSE) 
iqr = IQR(outlier_df$var1.1)  
up <-  Q[2]+1.5*iqr # Upper Range  
low <- Q[1]-1.5*iqr # Lower Range  
no_outliers_p <- outlier_df %>%  
  filter(var1.1 > low & var1.1 < up)  
  ```

# R Function
``` r
outlier_r <- function(x,column,col_name) {
  
  Q <- quantile(x[[column]], probs = c(.25,.75), na.rm = FALSE)
  iqr <- IQR(x[[column]])
  
  up <- Q[2]+1.5*iqr
  low <- Q[1]-1.5*iqr
  
 outliers_removed <- x %>%
    filter(col_name > low & col_name < up)
 
 return(outliers_removed)
}
```

# example
``` r
library(tidyverse)
ages <-c(20,15,22,24,25,24,23,23,23,25,24,25,100,0)
data_df <- data.frame(ages)
outlier_r <- function(x,column,col_name) {
  
  Q <- quantile(x[[column]], probs = c(.25,.75), na.rm = FALSE)
  iqr <- IQR(x[[column]])
  
  up <- Q[2]+1.5*iqr
  low <- Q[1]-1.5*iqr
  
 outliers_removed <- x %>%
    filter(col_name > low & col_name < up)
 
 return(outliers_removed)
}
outlier_r(data_df, "ages", ages)
```
