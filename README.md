# Tidyverse Tricks
Tidyverse Tricks
Tidyverse 骚操作集


```
library(tidyverse)

set.seed(0)
(df<-data.frame(id=rep(1:5,c(7,7,8,9,9)),
                tt=c(1:7,1:7,1:8,1:9,1:9),
                xx1=runif(40),
                yy1=rep(1:5,c(7,7,8,9,9))+0.5*rnorm(40),
                yy2=rep(1:5,c(7,7,8,9,9))+0.5*rnorm(40)))

# sort by values

df%>%arrange(desc(id),desc(tt))

# change order

df%>%select(xx1,yy1,everything())

# long to wide one variable

(df_wide<-df%>%select(id,tt,xx1)%>%spread(tt,xx1))

# wide to long one variable

(df_temp<-df_wide%>%gather(new_tt,new_xx1,2:10))
(df_temp%>%arrange(id,new_tt))

# long to long-long

(df_long_long<-df%>%gather(var_name,var_value,3:5))

# long-long to long

df_long_long%>%spread(var_name,var_value)

# long to wide multiple variables

set.seed(0)
(df2<-data.frame(id=rep(1:3,each=6),
                 group1=rep(rep(LETTERS[1:3],2),3),
                 group2=rep(rep(letters[4:5],each=3),3),
                 yy1=rnorm(6),
                 yy2=rexp(18)))

df2%>%gather(variable,value,yy1,yy2)%>%
  unite(temp,variable,group1,group2)%>%
  spread(temp,value)

# Group mean

df%>%group_by(id)%>%summarise(yy1_mean=mean(yy1,na.rm=TRUE),yy2_mean=mean(yy2,na.rm=TRUE))

# rename variables

df%>%rename(id_new=id,tt_new=tt)

# recode levels

df%>%mutate(tt_new=recode(tt,
                          "1"="new1","2"="new2","3"="new3",
                          "4"="new4","5"="new5","6"="new6",
                          "7"="new7","8"="new8","9"="new9"))

# last observation carried forward (locf)

df%>%group_by(id)%>%mutate(yy1=zoo::na.locf(yy1))%>%ungroup()

# join by two columns

df1<-df%>%select(id,tt,yy1)
df2<-df%>%select(id,tt,yy2)
df3<-df%>%select(id,tt,yy2)%>%rename(id_new=id,tt_new=tt)

left_join(df1,df2,by=c("id"="id","tt"="tt"))
left_join(df1,df3,by=c("id"="id_new","tt"="tt_new"))

# remove duplicates

df %>% distinct(x, y, .keep_all = TRUE)


```
