# Tidyverse Tricks
Tidyverse Tricks

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

df %>% distinct(x, y, .keep_all = TRUE) ## distinct rows by x and y
df %>% distinct.keep_all = TRUE) ## distinct rows by all columns

# read multiple csv files

library(tidyverse)
library(data.table)

allfilenames<-file.path(dirdirdir,list.files(path=dirdirdir,pattern="*.csv"))
raw_df<-allfilenames%>%map_df(~fread(.,stringsAsFactors=FALSE))

# summarise all columns

...%>%summarise_all(funs(mean))

# pull out a column / get one column from data.frame

iris2%>%pull(Species)

# row means of several columns

iris%>%mutate(Sepal=rowMeans(select(.,1:2)))
iris%>%mutate(Sepal=rowMeans(select(.,contains("Sepal"))))

# select columns by number and variable names

rawdata_heart%>%select(1:3,4:(4+11),28:(28+11),52:(52+11),time)

# specify the order of measures:

dataset%>%arrange(match(measure,c("strain","strain_rate","VCAM","IL6","GAL3")))

# summarize n, mean, sd and then convert to wide data

df_BL<-df_xxxxx%>%
  filter(Event_Name=="Baseline")%>%
  group_by(Group)%>%
  summarise(n=sum(!is.na(xxxxx)),
            mean=mean(xxxxx),
            sd=sd(xxxxx))%>%
  pivot_wider(names_from="Group",values_from=c("n","mean","sd"),names_sep=" ")%>%
  mutate(variable="xxxxx")%>%select(variable,everything())

# ggplot, interaction plot, add group mean, add standard error bar

ggplot(df_xxxxx,aes(x=Event_Name,y=xxxxx,group=Record_ID,col=Group))+
  geom_line()+geom_point()+
  stat_summary(aes(group=Group),fun.y=mean,geom="line",cex=2)+
  stat_summary(aes(group=Group,width=0.2),fun.data=mean_se,geom="errorbar",cex=1.5,alpha=0.25)+
  ggtitle("xxxxx")

# ggplot, group comparison, add group mean, add standard error bar

p2<-ggplot(df_xxxxx_diff%>%na.omit(),aes(x=Group,y=diff,color=Group))+ 
  stat_summary(fun.y=mean,geom="point",cex=3)+
  stat_summary(fun.data=mean_sdl,geom="errorbar",fun.args=list(mult=1))+
  stat_summary(fun.data=mean_se,geom="errorbar",aes(width=0.25))+
  geom_jitter()+
  ggtitle("xxxxx")
  
# repair names

gsub("\\s","_",trimws(gsub("\\."," ",gsub('\\.+','.',make.names(names(rawdata),unique=T)))))
trimws(gsub("\\.+","_",make.names(names(rawdata))),whitespace="_")
trimws(gsub("_+","_",gsub('[^a-zA-Z0-9]','_',make.names(channel_cell_population))),whitespace="_")

# case_when case when

x<-c(1:100,NA,NA,NA)
dplyr::case_when(
  x %in% 1 ~ 0,
  x %in% 2:5 ~ 1)
dplyr::case_when(
  x %in% 1 ~ 0,
  x %in% 2:5 ~ 1,
  TRUE ~ as.numeric(x))
dplyr::case_when(
  x %in% 1 ~ 0,
  x %in% 2:5 ~ 1,
  is.na(x) ~ -100,
  TRUE ~ as.numeric(x))
dplyr::case_when(
  x %in% 1 ~ 0,
  x %in% 2:5 ~ 1,
  TRUE ~ as.numeric(x),
  is.na(x) ~ -100)

# mutate severl columns

now %>%
  mutate_at(q1:q5,
            funs(case_when(
              . == "None" ~ 0,
              . == "Mild" ~ 1,
              . == "Moderate" ~ 2,
              TRUE == NA)))

```

## id within group

```
df %>% group_by(cat) %>% mutate(id = row_number())
```

## lag
```
lag(xx,n=?,default=?)
```

## Extract string
```
gsub(".* -> ", "", c("a -> b","cd -> dd"))
gsub(" -> .*", "", c("a -> b","cd -> dd"))
```

# regex vs glob
```

The fast dog is fast.
The faster dogs are faster.
A sick dog should see a dogdoc.
This file is filename.doc

If you type:


grep "fast*" filename.doc

The first two lines will match. Whether you're thinking globs or regex, that makes sense. But if you type:


grep "dogs*" filename.doc

The first three lines will match, but if you're thinking in globs, that doesn't make sense. Since grep uses regular expressions (regex) when searching files, the asterisk means "zero or more occurrences of the previous character", so in the second example, it matches dog and dogs, because having zero "s" characters matches the regex.
```

## Count missing by row / across!

```
mutate(nmissing=rowSums(is.na(across(X1:X5))))
filter(rowSums(!is.na(across(X1:X5)))>=1)
```

## reformulate formula
```
reformulate(
  termlabels=c("x1","x2"),
  response="Surv(time,delta)")
```

## circle color group

```
library(ggforce)
geom_mark_ellipse
```

##

```
mutate(nmissing=rowSums(is.na(across(glucosem10:glucose120))))

df_baseline_master%>%
  rowwise()%>%
  mutate(xx=all(is.na(across(BAA_Date:HbA1c_Date))))

df_baseline_master%>%
  rowwise()%>%
  mutate(xx=all(is.na(across(c(BAA_Date,BMI_Date,OGTT_Date,HbA1c_Date)))))

```

## rowmeans rowsums

```
rowMeans(select(.,GLUM10,GLU0),na.rm=T)
```

## ggplot timestamp plots
```
result<-bind_rows(
  BAA_count_ver4_std%>%
    select(Mask_ID,BAA_Date)%>%
    mutate(Type="BAA")%>%
    rename(Date=BAA_Date),
  OGTT_Final_raw%>%
    select(Mask_ID,OGTT_Date)%>%
    mutate(Type="OGTT")%>%
    rename(Date=OGTT_Date))
temp<-result%>%
  arrange(Mask_ID,Type,Date)%>%
  group_by(Mask_ID)%>%
  mutate(
    BL_Date=first(Date),
    Day=as.numeric(Date-BL_Date))%>%
  ungroup()

temp_id<-sample(temp$Mask_ID,100)

ggplot(temp%>%filter(Mask_ID%in%temp_id),aes(x=Day,y=Mask_ID,group=interaction(Type,Mask_ID),color=Type))+
  geom_point(position=ggstance::position_dodgev(height=0.7))+
  geom_line(position=ggstance::position_dodgev(height=0.7))+
  geom_vline(xintercept=365)+
  theme_bw()
```

## circles/ellipses around groups/clusters

https://luisdva.github.io/rstats/Grouping-points/

```
library(dplyr)
library(ggplot2)
library(ggalt)
library(ggforce)

ggplot(birdsAll,aes(x=mass,y=length))+
  geom_mark_hull(concavity = 5,expand=0,radius=0,aes(fill=age))+
  geom_point()+
  theme_bw()
  
ggplot(birdsAll,aes(x=mass,y=length))+
  geom_mark_hull(expand=0.01,aes(fill=age))+
  geom_point()+
  theme_bw()
```

## check time sequence/event sequence/time check

```
paste_order<-Vectorize(function(x1,x2,x3,x4){
  x<-c(x1,x2,x3,x4)
  x_name<-c("t1d","cca","wd","last")
  x_unique<-unique(sort(x))
  result<-c()
  for(a_x in x_unique){
    result<-c(result,paste0(x_name[which(x==a_x)],collapse="="))
  }
  return(paste0(result,collapse="<"))
})
```
