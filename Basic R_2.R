x <- c(1, runif(5,1,2),'a')

y <- list(a=c(1,2,3),b=("b"))
y
l2 <- list( a = matrix( c(12.3, 20, 1.2, 18.0) , nrow=2), b = y)


#--------------------------------------------------------------

x <- c(7.3, 29.4, 29.4, 2.9, 12.3, 7.5, 36.0, 4.8, 18.8, 4.2)
y <- c(5.2, 26.6, 31.2, 2.2, 13.8, 7.8, 35.2, 8.6, 20.3, 1.1)
M <- lm( y ~ x)

M
M$residuals
M$rank
M$qr
cor(dat,use="complete.obs", method="kendall")
install.packages('Hmisc')
library(Hmisc)
rcorr(dat,type="pearson")

#--------------------------------------------------------------

Country <- c("india","paki","us","uk","aus")
Score <- c(200,100,203,205,299)
year <- c("1/1/1987","10/10/1999","2/11/1990","11/11/1997","3/12/1998")

dat <- data.frame(Country, Score, year)
dat
library(dplyr)
filter(dat,year==1998)

dat$flag <- ifelse(dat$Score > 203,1,0)
dat$flag <- as.character(dat$flag)
dat$Country <- as.factor(dat$Country)
str(dat)
unique(dat$flag)

install.packages("lubridate")
require(lubridate)

dat$year<-as.character(dat$year)
dat$year<-ydm(dat$year)

a      <- c("M", "F", "F", "M", "F", "M", "M", "F", "F", "M")
a.fact <- as.factor(a)
x      <- c(166, 47, 61, 148, 62, 123, 232, 98, 93, 55)
dat    <- data.frame(x = x, gender = a.fact)

boxplot(x ~ gender, dat, horizontal = TRUE)


### Functions-------------------------------------------

Test_F <- function(x, p) {
  if(p == 0) {
    log(x)
  } else {
    if (p < 0) {
      -(x^p)
    } else {
      x^p
    }
  }
} 

batch       <- c(0.3,0.6,1.5,0.3,1.1,2.5,2.9,1,0.4,0.1,
                 4,2.3,0.9,0.6,0.4,0.7,0.2,0.3,1.1,0.2) # Create a skewed dataset
batch.trans <- Test_F(batch, 0.1)
batch.trans
boxplot(batch.trans, horizontal = TRUE)
typeof(batch.trans)
str(batch.trans)







