
# time diff in CA

CA=read.csv("C:\\Users\\monis\\Downloads\\CA_avg_time.csv")

x <- strptime(as.character(CA$ARRIVAL), format="%m/%d/%Y %H:%M")
y <- strptime(as.character(CA$LU_CLEAR), format="%m/%d/%Y %H:%M")

timediff=(difftime(x,  y, units="min"))

timediff=timediff*-1
max(timediff)

CA$average_mins=timediff

write.csv(CA, file = "CA_timediff.csv",row.names=FALSE)


## time diff in FL
FL=read.csv("C:\\Users\\monis\\Downloads\\FL_avg_time.csv")

x <- strptime(as.character(FL$ARRIVAL), format="%m/%d/%Y %H:%M")
y <- strptime(as.character(FL$LU_CLEAR), format="%m/%d/%Y %H:%M")

timediff=(difftime(x,  y, units="min"))

timediff=timediff*-1
max(timediff)

FL$average_mins=timediff

write.csv(FL, file = "FL_timediff.csv",row.names=FALSE)
hist(timediff)
hist(as.numeric(FL$average_mins))


source("https://raw.githubusercontent.com/talgalili/R-code-snippets/master/boxplot.with.outlier.label.r") 
boxplot.with.outlier.label(as.numeric(FL$average_mins), lab_y, spread_text = F)
y=ca$average_minas.numeric(CA$average_mins<70)
hist(as.numeric(CA$average_mins))


# ca above 70
# fl above 


CA$INC_DATE=as.Date(CA$INC_DATE, "%m/%d/%Y")
CA

typeof(CA$INC_DATE)

x <- transform(CA, month = format(CA$INC_Date,"%m"), year = format(CA$INC_Date, "%Y"))
CA$month= format(CA$INC_Date,"%m")
counts <- ddply(x,.(month,year),nrow)
counts <- ddply(x,.(month,year),nrow)


# make a new monthly date
counts <- transform(counts, new_date = as.Date(paste(year,month,'01',sep="-")))


x=CA %>% group_by(year, month, day) %>% summarize(num_accidents=n())

library(lubridate)

x=CA %>%
  mutate(month = format(CA$INC_Date, "%m"), year = format(CA$INC_Date, "%Y")) %>%
  group_by(month, year) %>%
  summarize(num_accidents=n())

data %>% 
  group_by(month=floor_date(date, "month")) %>%
  summarize(summary_variable=sum(value))

library(ggplot2)
# now plot
ggplot(counts,aes(x=new_date,y=V1)) + geom_point() + scale_x_date()


#############

CA$INC_DATE=as.character(CA$INC_DATE)

x <- as.POSIXct(CA$INC_DATE)
=as.Date(CA$INC_DATE)
x <- as.Date(CA$INC_DATE, format = "%m/%d/%Y")

x

y=x %>%
  mutate(month = format(x, "%m"), year = format(x, "%Y")) %>%
  group_by(month, year) %>%
  summarize(num_accidents=n())

R

# library
library(tidyverse)

# Build a Time serie data set for last year
day=as.Date("2017-06-14") - 0:364
value=runif(365) + seq(-140, 224)^2 / 10000
data=data.frame(day, value)

# Calculate mean value per month
don=data %>% mutate(month = as.Date(cut(day, breaks = "month"))) %>%
  group_by(month) %>% 
  summarise(average = mean(value)) 

# And make the plot
ggplot(don, aes(x=month, y=average)) +
  geom_line() + 
  geom_point() +
  scale_x_date(date_labels = "%b-%Y", date_breaks="1 month")

# Calculate agregated data per week
don=data %>% mutate(week = as.Date(cut(day, breaks = "week"))) %>%
  group_by(week) %>% 
  summarise(average = mean(value)) 

# And make the plot
ggplot(don, aes(x=week, y=average)) +
  geom_line() + 
  geom_point() +
  geom_area(fill=alpha('slateblue',0.2)) +
  scale_x_date(date_labels = "%W-%b", date_breaks="1 week") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
# library
library(tidyverse)

# Build a Time serie data set for last year
day=as.Date("2017-06-14") - 0:364
value=1
data=data.frame(x)
data$value=1

# Calculate mean value per month
don=data %>% mutate(month = as.Date(cut(x, breaks = "month"))) %>%
  group_by(month) %>% 
  summarise(num_accidents=n()) 

# And make the plot
ggplot(don, aes(x=month, y=average)) +
  geom_line() + 
  geom_point() +
  scale_x_date(date_labels = "%b-%Y", date_breaks="1 month")


