#read data
data=read.csv('amazon1.csv')
str(data)

#check missing values
library(tidyr)
library(dplyr)
library(ggplot2)
library(tm)
sapply(data,function(x) sum(is.na(x)))
data = data %>% mutate_if(is.character, funs(na_if(.,""))) 
data = data %>% mutate_if(is.character, funs(na_if(.,"N/A")))
data = data %>% mutate_if(is.character, funs(na_if(.,"NA")))
missing_values <- data %>% 
  summarize_all(funs(sum(is.na(.))/n())) 
missing_values <- gather(missing_values, key="feature", value="missing_pct") 
missing_values[missing_values$missing_pct > 0,] %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) + geom_bar(stat="identity",fill="red") + coord_flip()+theme_bw()
glimpse(data)

#delete missing values
data[-which(data$Title==""),]
data[-which(data$Title=="NA"),]
data[-which(data$Title=="N/A"),]

# order amount-category
#mean
order_amount_mean=aggregate(data$Order.Amount, by=list(data$Category_Family), FUN=mean)
order_amount_mean
ordermean=data.frame(order_amount_mean)
names(ordermean)=c('category','average_order_amount')
ordermean
ggplot(ordermean,aes(x=category,y=average_order_amount))+geom_bar(stat ="identity" ,fill='#AFEEEE',width = 0.5)+geom_text(aes(label=average_order_amount), vjust=0, color="black", size=3)+
  theme_minimal()

#max min
order_amount_max=aggregate(data$Order.Amount, by=list(data$Category_Family), FUN=max)
order_amount_max                
ordermax=data.frame(order_amount_max)
names(ordermax)=c('category','max_order_amount')
ordermax

order_amount_min=aggregate(data$Order.Amount, by=list(data$Category_Family), FUN=min)
order_amount_min               
ordermin=data.frame(order_amount_min)
names(ordermin)=c('category','min_order_amount')
ordermin

maxmin=merge(ordermax,ordermin,by,by.x = 'category',by.y = 'category',sort=TRUE)
maxmin

ggplot(ordermax,aes(x=category,y=max_order_amount))+geom_bar(stat ="identity" ,fill='#AFEEEE',width = 0.5)+geom_text(aes(label=max_order_amount), vjust=0, color="black", size=3)+
  theme_minimal()
                            
#sum
order_amount_sum=aggregate(data$Order.Amount, by=list(data$Category_Family), FUN=sum)
order_amount_sum
ordersum=data.frame(order_amount_sum)
names(ordersum)=c('category','sum_order_amount')
ordersum
ggplot(ordersum,aes(x=category,y=sum_order_amount))+geom_bar(stat ="identity" ,fill='#AFEEEE',width = 0.5)+geom_text(aes(label=sum_order_amount), vjust=0, color="black", size=3)+
  theme_minimal()

#mode
order_amount_mode=aggregate(data$Order.Amount, by=list(data$Category_Family), FUN=median)
order_amount_sum
ordersum=data.frame(order_amount_sum)
names(ordersum)=c('category','sum_order_amount')
ordersum
ggplot(ordersum,aes(x=category,y=sum_order_amount))+geom_bar(stat ="identity" )

# order amount-subcategory
#mean
sub_order_amount_mean=aggregate(data$Order.Amount, by=list(data$Category_Code), FUN=mean)
sub_order_amount_mean
subordermean=data.frame(sub_order_amount_mean)
names(subordermean)=c('subcategory','average_order_amount')
subordermean
ggplot(subordermean,aes(x=subcategory,y=average_order_amount))+geom_bar(stat ="identity" ,fill='#AFEEEE',width = 0.5)+geom_text(aes(label=average_order_amount), vjust=0, color="black", size=3)+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#max min
sub_order_amount_max=aggregate(data$Order.Amount, by=list(data$Category_Code), FUN=max)
sub_order_amount_max                
subordermax=data.frame(sub_order_amount_max)
names(subordermax)=c('subcategory','max_order_amount')
subordermax

sub_order_amount_min=aggregate(data$Order.Amount, by=list(data$Category_Code), FUN=min)
sub_order_amount_min               
subordermin=data.frame(sub_order_amount_min)
names(subordermin)=c('subcategory','min_order_amount')
subordermin

submaxmin=merge(subordermax,subordermin,by,by.x = 'subcategory',by.y = 'subcategory',sort=TRUE)
submaxmin

ggplot(subordermax,aes(x=subcategory,y=max_order_amount))+geom_bar(stat ="identity" ,fill='#AFEEEE',width = 0.5)+geom_text(aes(label=max_order_amount), vjust=0, color="black", size=3)+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#sum
sub_order_amount_sum=aggregate(data$Order.Amount, by=list(data$Category_Code), FUN=sum)
sub_order_amount_sum
subordersum=data.frame(sub_order_amount_sum)
names(subordersum)=c('subcategory','sum_order_amount')
subordersum
ggplot(subordersum,aes(x=subcategory,y=sum_order_amount))+geom_bar(stat ="identity" ,fill='#AFEEEE',width = 0.5)+geom_text(aes(label=sum_order_amount), vjust=0, color="black", size=3)+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
#qty-category
#mean
qty_mean=aggregate(data$Order.Qty, by=list(data$Category_Family), FUN=mean)
qty_mean
qtymean=data.frame(qty_mean)
names(qtymean)=c('category','average_qty')
qtymean
ggplot(qtymean,aes(x=category,y=average_qty))+geom_bar(stat ="identity" ,fill='#AFEEEE',width = 0.5)+geom_text(aes(label=average_qty), vjust=0, color="black", size=3)+
  theme_minimal()

#max min
qty_max=aggregate(data$Order.Qty, by=list(data$Category_Family), FUN=max)
qty_max                
qtymax=data.frame(qty_max)
names(qtymax)=c('category','max_qty')
qtymax

qty_min=aggregate(data$Order.Qty, by=list(data$Category_Family), FUN=min)
qty_min               
qtymin=data.frame(qty_min)
names(qtymin)=c('category','min_qty')
qtymin

qtymaxmin=merge(qtymax,qtymin,by,by.x = 'category',by.y = 'category',sort=TRUE)
qtymaxmin

ggplot(qtymax,aes(x=category,y=max_qty))+geom_bar(stat ="identity" ,fill='#AFEEEE',width = 0.5)+geom_text(aes(label=max_qty), vjust=0, color="black", size=3)+
  theme_minimal()

#sum
qty_sum=aggregate(data$Order.Qty, by=list(data$Category_Family), FUN=sum)
qty_sum
qtysum=data.frame(qty_sum)
names(qtysum)=c('category','sum_qty')
qtysum
ggplot(qtysum,aes(x=category,y=sum_qty))+geom_bar(stat ="identity" ,fill='#AFEEEE',width = 0.5)+geom_text(aes(label=sum_qty), vjust=0, color="black", size=3)+
  theme_minimal()

# qty amount subcategory
#mean
sub_qty_mean=aggregate(data$Order.Qty, by=list(data$Category_Code), FUN=mean)
sub_qty_mean
subqtymean=data.frame(sub_qty_mean)
names(subqtymean)=c('subcategory','average_qty')
subqtymean
ggplot(subqtymean,aes(x=subcategory,y=average_qty))+geom_bar(stat ="identity" ,fill='#AFEEEE',width = 0.5)+geom_text(aes(label=average_qty), vjust=0, color="black", size=3)+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#max min
sub_qty_max=aggregate(data$Order.Qty, by=list(data$Category_Code), FUN=max)
sub_qty_max                
subqtymax=data.frame(sub_qty_max)
names(subqtymax)=c('subcategory','max_qty')
subqtymax

sub_qty_min=aggregate(data$Order.Qty, by=list(data$Category_Code), FUN=min)
sub_qty_min               
subqtymin=data.frame(sub_qty_min)
names(subqtymin)=c('subcategory','min_qty')
subqtymin

subqtymaxmin=merge(subqtymax,subqtymin,by,by.x = 'subcategory',by.y = 'subcategory',sort=TRUE)
subqtymaxmin

ggplot(subqtymax,aes(x=subcategory,y=max_qty))+geom_bar(stat ="identity" ,fill='#AFEEEE',width = 0.5)+geom_text(aes(label=max_qty), vjust=0, color="black", size=3)+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#sum
sub_qty_sum=aggregate(data$Order.Qty, by=list(data$Category_Code), FUN=sum)
sub_qty_sum
subqtysum=data.frame(sub_qty_sum)
names(subqtysum)=c('subcategory','sum_qty')
subqtysum
ggplot(subqtysum,aes(x=subcategory,y=sum_qty))+geom_bar(stat ="identity" ,fill='#AFEEEE',width = 0.5)+geom_text(aes(label=sum_qty), vjust=0, color="black", size=3)+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))



