dat <- read.csv("FAO_grains_NA.csv", header=TRUE)


### A data table's dimensions can be extracted using the dim() function

dim(Sce2_Test)

### The column names can be extracted from the table using the names() function

names(Sce2_Test)

### To check 1st or last few rows

head(Sce2_Test,n=10)
tail(Sce2_Test,n=10)

### Insight into each column's data type

str(Sce2_Test)

### A summary of each column can be displayed

summary(Sce2_Test)

### Find unique values

unique(Sce2_Test_2$CUST_P)

### Changing data type- Example: as.factor, as.integer, as.numeric, as.Date

Sce2_Test$ACCOUNT_ID<- as.character(Sce2_Test$ACCOUNT_ID)

### Add & Remove column

Sce2_Test_2$flag_test <- ''
Sce2_Test_2$flag_test <- ifelse(Sce2_Test_2$SHARE_OF_WALLET>median(Sce2_Test_2$SHARE_OF_WALLET),"1","0")

Sce2_Test_2$flag_test<- NULL

### Remove rows having NA values

Sce2_Test_2<-na.omit(Sce2_Test_2)

Sce2_Test_2_test<-Sce2_Test_2[(Sce2_Test_2$EPS=="0" | Sce2_Test_2$SHARE_OF_WALLET==0),]
Sce2_Test_2_test<-Sce2_Test_2[!(Sce2_Test_2$EPS=="0" & Sce2_Test_2$SHARE_OF_WALLET==0),]

Sce2_Test_2_test<-subset(Sce2_Test_2,EPS=="0" | SHARE_OF_WALLET==0)

### Subset values

Sce2_Test_2_test[23,5]
Sce2_Test_2_test[c(23,25,1),c(1,2,3,4)]

### Pick random values
Sce2_Test_2_test2 <- Sce2_Test_2_test[sample(nrow(Sce2_Test_2_test), 20), ]

### Split data
temp1 <- sample(nrow(Sce2_Test_2_test), 3000)
Sce2_Test_2_test2a <- Sce2_Test_2_test[temp1,]
Sce2_Test_2_test2b <- Sce2_Test_2_test[-temp1,]


### dplyr Package

install.packages("dplyr")
library(dplyr)

### filter()	Subset by row values
### arrange()	Sort rows by column values
### select()	Subset columns
### mutate()	Add columns
### summarise()	Summarize columns

temp3 <- filter(Sce2_Test_2_test, CUST_P == "0" | CUST_C == "0", FLAG_TRANSACTED_Q3 == "1")
temp4 <- arrange(Sce2_Test_2_test, SHARE_OF_WALLET,desc(CONSOLIDATED_TTL_BUY_POWER))
temp5 <- select(Sce2_Test_2_test,CUST_P,CUST_C)

temp6 <- mutate(Sce2_Test_2_test, FLAG_TEST = ifelse(CUST_C == "0", "GREENFIELD", "CUSTOMER"))
temp7   <- group_by(Sce2_Test_2_test, CUST_C)

summarise(Sce2_Test_2_test, bp_min = min(CONSOLIDATED_TTL_BUY_POWER), bp_max=max(CONSOLIDATED_TTL_BUY_POWER))

### Combine multiple codes

dat %>%
  filter(Information == "Yield (Hg/Ha)", Year >= 2005, Year <=2010, 
         Country=="United States of America") %>%
  group_by(Crop, Year) %>%
  summarise(median_yield = round(median(Value))) %>%
  arrange(Crop, desc(median_yield))

### Modify table layout : Reshape2 >> melt, dcast

library(reshape2)

dat2 <- dat %>%
  filter(Information == "Yield (Hg/Ha)", Year >= 2005, Year <=2010) %>%
  group_by(Crop, Year, Country) %>%
  summarise(median_yield = round(sum(Value))) %>%
  arrange(Crop, desc(median_yield))


dat2.cast1 <- dcast(dat2, Crop + Year  ~ Country)
dat2.cast2 <- dcast(dat2, Country + Year ~ Crop)

dat2.long2 <- melt(dat2.cast2, id = c("Country", "Year"))
dat2.long2 <- melt(dat2.cast2, id = c("Country", "Year"))
















