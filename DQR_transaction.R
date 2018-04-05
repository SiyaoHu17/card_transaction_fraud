library(dplyr)
library(ggplot2)
# Read Data
library(readxl)
data = read_excel('card transactions.xlsx')


# Basic Descriptions
## dataset information
## fields names
names(data)
## count of records
colSums(!is.na(data))
## unique number
for (variable in c(1:10)){
  print(count(unique(data[,variable])))
}
## percent pupulated
round(colSums(!is.na(data))*100/nrow(data),2)
## numeric statistics
summary(data$Amount)
sd(data$Amount)
glimpse(data)
### remove the outlier
outlier=which(grepl(3102045.5, data$Amount))
summary(data$Amount[-outlier])
sd(data$Amount[-outlier])
a=round(data[outlier,]$Amount,2)


# Plots
## Card number
par(mar = c(10,10,10,10)) 
library(scales)
head(data[,2])
data$Amount=as.factor(data$Amount)
data%>%
  group_by(Cardnum)%>%
  summarize(count=n())%>%
  arrange(desc(count))%>%
  head(20)%>%
  ggplot(aes(x=reorder(Cardnum,count),y=count))+
  geom_bar(stat = "identity")+
  labs(x='Cardnum',
       y='Count',
       title='TOP 20 Cardnum Distribution')+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_text(aes(label=count),hjust=-0.02, size=5)+  
  theme(
    plot.title = element_text(size=20),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=14)
  )+
  coord_flip()
##date
data$Date=as.Date(data$Date)
data%>%
  group_by(Date)%>%
  summarize(count=n())%>%
  arrange(desc(count))%>%
  head(20)%>%
  ggplot(aes(x=reorder(Date,count),y=count))+
  geom_bar(stat = "identity")+
  labs(x='Date',
       y='Count',
       title='TOP 20 Date Distribution')+
  geom_text(aes(label=count),hjust=-0.02, size=5)+  
  theme(
    plot.title = element_text(size=20),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=14)
  )+
  coord_flip()

data%>%
  group_by(Date)%>%
  summarize(count=n())%>%
  arrange(count)%>%
  head(20)%>%
  ggplot(aes(x=reorder(Date,count),y=count))+
  geom_bar(stat = "identity")+
  labs(x='Date',
       y='Count',
       title='Bottom 20 Date Distribution')+
  geom_text(aes(label=count),hjust=-0.02, size=5)+  
  theme(
    plot.title = element_text(size=20),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=14)
  )+
  coord_flip()

#Time seriers analytsis for transactions
##per day
data%>%
  group_by(Date)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Date,y=count))+
  geom_line()+  
  labs(x='Date',
       y='Count',
       title='Daily Transactions')+  
  theme(
    plot.title = element_text(size=25),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18)
  )
##per week
data%>%
  mutate(date_week=as.Date(cut(data$Date,breaks="week")))%>%
  group_by(date_week)%>%
  summarise(count=n())%>%
  ggplot(aes(x=date_week,y=count))+
  geom_line()+  
  labs(x='Date',
       y='Count',
       title='Weekly Transactions')+
  theme(
    plot.title = element_text(size=25),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18)
  )

##per month
data%>%
  mutate(date_month=as.Date(cut(data$Date,breaks="month")))%>%
  group_by(date_month)%>%
  summarise(count=n())%>%
  ggplot(aes(x=date_month,y=count))+
  geom_line()+  
  theme(
    plot.title = element_text(size=25),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18)
  )+
  labs(x='Date',
       y='Count',
       title='Monthly Transactions')

##Merchantnum
data%>%
  filter(!is.na(Merchantnum))%>%
  group_by(Merchantnum)%>%
  summarize(count=n())%>%
  arrange(desc(count))%>%
  head(20)%>%
  ggplot(aes(x=reorder(Merchantnum,count),y=count))+
  geom_bar(stat = "identity")+
  labs(x='Merchantnum',
       y='Count',
       title='TOP 20 Merchantnum Distribution')+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_text(aes(label=count),hjust=-0.02, size=5)+  
  theme(
    plot.title = element_text(size=20),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=14)
  )+
  coord_flip()
# Merch Description
data$`Merch Description`
data%>%
  filter(!is.na(`Merch Description`))%>%
  group_by(`Merch Description`)%>%
  summarize(count=n())%>%
  arrange(desc(count))%>%
  head(20)%>%
  ggplot(aes(x=reorder(`Merch Description`,count),y=count))+
  geom_bar(stat = "identity")+
  labs(x='Merch Description',
       y='Count',
       title='TOP 20 Merch Description Distribution')+
  geom_text(aes(label=count),hjust=-0.02, size=5)+  
  theme(
    plot.title = element_text(size=20),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=14)
  )+
  coord_flip()

# Merchant State
data%>%
  filter(!is.na(`Merchant State`))%>%
  group_by(`Merchant State`)%>%
  summarize(count=n())%>%
  arrange(desc(count))%>%
  head(15)%>%
  ggplot(aes(x=reorder(`Merchant State`,desc(count)),y=count))+
  geom_bar(stat = "identity")+
  labs(x='Merchant State',
       y='Count',
       title='TOP 15 Merchant State Distribution')+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_text(aes(label=count),vjust=0, size=5)+  
  theme(
    plot.title = element_text(size=20),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=14)
  )

# Merchant Zip
data%>%
  filter(!is.na(`Merchant Zip`))%>%
  group_by(`Merchant Zip`)%>%
  summarize(count=n())%>%
  arrange(desc(count))%>%
  head(15)%>%
  ggplot(aes(x=reorder(`Merchant Zip`,count),y=count))+
  geom_bar(stat = "identity")+
  labs(x='Merchant Zip',
       y='Count',
       title='TOP 15 Merchant Zip Distribution')+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_text(aes(label=count),hjust=-0.02, size=5)+  
  theme(
    plot.title = element_text(size=20),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=14)
  )+
  coord_flip()

# Transtype
data%>%
  filter(!is.na(Transtype))%>%
  group_by(Transtype)%>%
  summarize(count=n())%>%
  arrange(desc(count))
# Amount
data$Amount=as.numeric(as.matrix(data$Amount))
data%>%
  filter(!is.na(Amount))%>%
  group_by(Amount)%>%
  summarize(count=n())%>%
  arrange(desc(count))%>%
  head(15)%>%
  ggplot(aes(x=reorder(Amount,count),y=count))+
  geom_bar(stat = "identity")+
  labs(x='Amount',
       y='Count',
       title='TOP 15 Amount Distribution')+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_text(aes(label=count),vjust=0, size=5)+  
  theme(
    plot.title = element_text(size=20),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=14)
  )

data[-outlier,]%>%
  ggplot(aes(x=Amount))+
  geom_histogram(bins=100)+
  scale_x_continuous(limits=c(0,5000))+
  labs(y="COUNT",
       title="Distribution of Amount (Amount<5000)")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme(
    plot.title = element_text(size=20),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )

data[-outlier,]%>%
  ggplot(aes(x=Amount))+
  geom_histogram(bins=100)+
  scale_x_continuous(limits=c(0,5000))+
  labs(y="COUNT",
       title="Distribution of Amount (Amount<5000)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme(
    plot.title = element_text(size=20),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )

##fraud
data$Fraud=as.character(data$Fraud)
data%>%
  group_by(Fraud)%>%
  summarize(count=n())


#Time seriers analytsis for transactions
##per day
nFraud=nrow(data[data$Fraud==1,])
nNanFraud=nrow(data[data$Fraud==0,])
data$Fraud=as.character(data$Fraud)
data%>%
  group_by(Fraud,Date)%>%
  summarise(count=n())%>%
  mutate(Rate=ifelse(Fraud=='1',count/nFraud,count/nNanFraud))%>%
  ggplot(aes(x=Date,y=Rate,color=Fraud))+
  geom_line()+  
  labs(x='Date',
       y='Percentage',
       title='Daily Transactions by Fraud Label')+  
  theme(
    plot.title = element_text(size=25),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18)
  )
##per week
data%>%
  mutate(date_week=as.Date(cut(data$Date,breaks="week")))%>%
  group_by(date_week,Fraud)%>%
  summarise(count=n())%>%
  mutate(Rate=ifelse(Fraud=='1',count/nFraud,count/nNanFraud))%>%
  ggplot(aes(x=date_week,y=Rate,color=Fraud))+
  geom_line()+  
  labs(x='Date',
       y='Percentage',
       title='Weekly Transactions by Fraud')+
  theme(
    plot.title = element_text(size=25),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18)
  )

##per month
data%>%
  mutate(date_month=as.Date(cut(data$Date,breaks="month")))%>%
  group_by(Fraud,date_month)%>%
  summarise(count=n())%>%
  mutate(Rate=ifelse(Fraud=='1',count/nFraud,count/nNanFraud))%>%
  ggplot(aes(x=date_month,y=Rate,color=Fraud))+
  geom_line()+  
  theme(
    plot.title = element_text(size=25),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18)
  )+
  labs(x='Date',
       y='Rate',
       title='Monthly Transactions by Fraud')
#amount
data2=data[-outlier,]%>%
  group_by(Fraud,Amount)%>%
  summarise(count=n())%>%
  mutate(Rate=ifelse(Fraud=='1',count/nFraud,count/nNanFraud))

dataFraud=data2[data2$Fraud=='1',]
dataNanFraud=data2[data2$Fraud=='0',]

ggplot(dataFraud,aes(x=Amount,y=Rate))+
geom_histogram(stat = "identity")+
scale_x_continuous(limits=c(0,10))+
labs(y="COUNT",
     title="Distribution of Amount (Amount<5000)")+
theme(
  plot.title = element_text(size=20),
  axis.title.x = element_text(size=16),
  axis.title.y = element_text(size=16),
  axis.text.x = element_text(size=14),
  axis.text.y = element_text(size=14)
)


