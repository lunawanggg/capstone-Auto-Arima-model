# set working directory
setwd('/Users/czw/Desktop')
internal=read.csv('internal.csv', stringsAsFactors = F)
# call for libraries
library(ggplot2)
library(tidyverse)
library(lubridate)
library(reshape2)
library(fpp)
library(gridExtra)
library(tidyr)
library(dplyr)
library(lubridate)
# data first glance
head(internal)
summary(internal) 
str(internal)
str(internal$DESCRIPTION)
# 420 observations of 14 variables: items & descriptions with Jan - Dec monthly forecast value

# Detect missing values
sum(is.na(internal)) #0, there is no missing values in this dataset

# add a new column named 'mean' that compute the average monthly forecast for each items 
internal$mean = round(rowMeans(internal[3:14],na.rm = TRUE),2)
internal
# add a new column named 'max' that represent the largest monthly forecast for each items
internal$max = apply(internal[,3:14], 1, max)
internal$max
internal

# rearrange the dataset by the value of average monthly forecast by a descending order
internal=internal[order(internal$mean,decreasing = TRUE),]
# select the estimated top 10 best selling items
top10=internal[1:10,]
top10
# plot a bar chart of top10 items and mean monthly forecast in a descending order
bar=ggplot(data=top10, aes(x=reorder(DESCRIPTION,-mean), y=mean,fill=DESCRIPTION)) +
  geom_bar(stat="identity",fill="steelblue")+
  geom_text(aes(label=mean), vjust=-0.3, size=3.5)+
  theme_minimal()
bar
# Horizontal bar plot for better visualization
bar+ coord_flip()


# Look at actual data
actual=read.csv('order.csv')
str(actual)
str(actual$Item.Description)
str(internal$DESCRIPTION)
# convert date to month
actual$Transaction.Date=month(as.POSIXlt(actual$Transaction.Date, format="%d/%m/%Y"))
actual$Transaction.Date=as.factor(actual$Transaction.Date)
str(actual$Transaction.Date) # 12 factors to indicate month

actual$Transaction.Date[actual$Item.Description =='PLAY SPACE COOL GRAY']
####################################################################
# transpose the Top10 dataframe (flip x and y)
top10_2= data.frame(t(top10[,2:14][-1]))
colnames(top10_2)=top10[,2:14][, 1]
top10_2
str(top10_2)

# create a new colname for top10_2 as Month for Jan-Dec
top10_2$Month=c('JAN 2019','FEB 2019','MAR 2019','APR 2019','MAY 2019','JUN 2019','JUL 2019',
                'AUG 2019','SEP 2019','OCT 2019','NOV 2019','DEC 2019')
# convert 3-letter month character to date variables
top10_2$Month =as.Date(paste('01', top10_2$Month), format='%d %b %Y')

# plot the top 10 items time series line in one chart
colnames(top10_2)
df = top10_2 %>%
  select(Month, colnames(top10_2[,1:10])) %>%
  gather(key = "variable", value = "value", -Month)
df
# line chart visualization
ggplot(df, aes(x = Month, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal()
# we find that although there are 420 items here, some of the descriptions of them share the same 
# partial name that may compromise as on singlt value, thus we will convert them into new varibles 
# for better modelling in the future


