# Tidyverse Tricks
Tidyverse Tricks
Tidyverse 骚操作集


```
library(tidyverse)

set.seed(0)
df<-data.frame(id=rep(1:5,c(7,7,8,9,9)),
               tt=c(1:7,1:7,1:8,1:9,1:9),
               xx1=runif(40),
               yy1=rep(1:5,c(7,7,8,9,9))+0.5*rnorm(40),
               yy2=rep(1:5,c(7,7,8,9,9))+0.5*rnorm(40))

# long to wide one variable

(df_wide<-df%>%select(id,tt,xx1)%>%spread(tt,xx1))

# wide to long one variable

df_wide%>%gather(id,new_xx1,2:9)
```
