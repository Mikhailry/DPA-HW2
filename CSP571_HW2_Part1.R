# Please note that Homework Two consists of two parts, an R file and a
# Text document. Ensure that you submit both parts.
# Load in the Boston Housing data set using the code below.
install.packages('mlbench')
library('mlbench')
data(BostonHousing)

# 1. Create a scatterplot matrix of all variables in the data set. Save your output.
#install.packages("ggplot2") ## uncomment to install ggplot2
#install.packages("GGally") ## uncomment to install GGally
#library('GGally')
df<-data.frame(BostonHousing)
pairs(df)

# 2. For each numeric variable in BostonHousing, create a separate boxplot using
# "Method 2" listed in the class notes. Do this programmatically; meaning do
# not simply hardcode the creation of every boxplot. Instead, loop over the
# approriate columns and create the boxplots. Save your output. Ensure your boxplots
# all have proper titles
numVars <- (sapply(df, is.numeric)) #all numeric fields
df_num<-(df[,numVars]) #dataframe of numeric fields

#bx<-function(x){
  #boxplot(x, range = 1.5, main=names(df_num[x]), plot=T)
#}

#setwd("/Users/mikhailrybalchenko/Documents/MATH571/HW2") #uncomment to set working directory
pdf(file=paste0("HW2_Q2_boxplots.pdf")) 
for (i in 1:ncol(df_num)){
 boxplot(df_num[i], range = 1.5, main=names(df_num[i]), plot=T)
}
dev.off()

# 3. Create a correlation matrix and correlation plot
# for the BostonHousing data set. Save your output.
#install.packages("corrplot")#uncomment to install package
library("corrplot")
corMtx <- cor(df_num) #create correlation matrix
round(corMtx,2) #rounded values in corMtx
corrplot(corMtx, method = "circle")

# 4. Identify the top 3 strongest absolute correlations in the data set. Save your output.

corMtx[lower.tri(corMtx,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
corMtx=as.data.frame(as.table(corMtx))  #Turn into a 3-column table
corMtx=na.omit(corMtx)  #Get rid of the junk we flagged above
corMtx=corMtx[order(-abs(corMtx$Freq)),]    #Sort by highest correlation (whether +ve or -ve)
topCor <- corMtx[1:3,] 
write.csv(topCor, file = "HW2_Q4_strongest_abs_cor.csv")
topCor
#top3 absolute correlations
#https://stackoverflow.com/questions/7074246/show-correlations-as-an-ordered-list-not-as-a-large-matrix

# 5. Create a new variable call ageGroup quartiles. Divide the age variable
# into four even sections and assign it to one quartile.

quartiles <- quantile(df$age, probs=0:4/4)
df$ageGroup <- findInterval(df$age, quartiles, rightmost.closed = TRUE)

# 6. Go to the website listed below. Convert the html table into a
# dataframe with columns NO, Player, Highlights
library('rvest')
library('tidyr')
url = 'http://www.espn.com/nfl/superbowl/history/mvps'

#create a html document from url
webpage<-read_html(url)

mvp_table <- html_nodes(webpage, css = 'table')
#converting to df
mvp <- html_table(mvp_table)[[1]]

#removing first rows with garbage
mvp <- mvp[-(1:2), ]
#assigning new column names
names(mvp) <- c("NUMBER", "PLAYER", "HIGHLIGHTS") 
#replacing roman numbers in column Number
mvp$NUMBER <- 1:nrow(mvp)
head(mvp)

# 7.Extract the names of the MVPs, Position and Team into columns
# MVP1, MVP2, Position, Team

mvp <- separate(mvp, PLAYER, c("MVPs", "Position", "Team"), sep = ", ", remove = TRUE)
mvp <- separate(mvp, MVPs, c("MVP1", "MVP2"), sep = "& ", remove = TRUE)
head(mvp)


# 8. Determine the 90th%, 92.5th%, 95th%, 97.5th% and 99th% confidence intervals
# for the mean of passing yards
# (as listed in "Highlights" column) for quarterbacks.
# Note that there are a few intermediate steps you'll probably want to do
# in order to accomplish this. I am purposelly leaving that up to you, so that
# you are starting to develop the types of problem solving skills you'll need
# to tackle these problems in the wild.

#extracting passing numbers from 'Highlights' column to a separate one
mvp <- extract(mvp,HIGHLIGHTS,"PASSING",regex = "([0-9]+).*yards passing", remove = FALSE)
#converting variable into numeric type
mvp$PASSING <- as.numeric(mvp$PASSING)

t.test(x=mvp$PASSING, alternative = "two.sided", conf.level = 0.90)
t.test(x=mvp$PASSING, alternative = "two.sided", conf.level = 0.925)
t.test(x=mvp$PASSING, alternative = "two.sided", conf.level = 0.95)
t.test(x=mvp$PASSING, alternative = "two.sided", conf.level = 0.975)
t.test(x=mvp$PASSING, alternative = "two.sided", conf.level = 0.99)

# 9. The following contains data on the calorie counts of four types
# of foods. Perform an ANOVA and determine the Pr(>F)
food1 <- c(164,   172,   168,   177, 	156, 	195)
food2 <- c(178,   191, 	197, 	182, 	185, 	177)
food3 <- c(175, 	193, 	178, 	171, 	163, 	176)
food4 <- c(155, 	166, 	149, 	164, 	170, 	168)

# We need to change the data
food_type <- c(
  rep('food1', 6)
  , rep('food2', 6)
  , rep('food3', 6)
  , rep('food4', 6)
)
food_total <- c(food1, food2, food3, food4)

df_food = data.frame(food_type, food_total)

cal <- aov(food_total ~ food_type)
cal
summary(cal)
#P-value = 0.00688


# 10. Install the lubridate package and familarize yourseslf with it.
# This is the preferred package for handling
# dates in R, which is an important skill.
# Practing handling dates by using the package to determine how many
# Tuesdays fell on the first of the month
# during the 19th century (1 Jan 1801 to 31 Dec 1901).
#install.packages('lubridate')#uncomment to install
library('lubridate')

start <- dmy("1 Jan 1801")
end <- dmy("31 Dec 1901")
interval <- seq(from=start, to=end, by="month") #all 1st days of month in the interval
length(which(wday(interval)==3)) #number of Tuesdays in interval


#https://www.r-bloggers.com/count-the-mondays-in-a-time-interval-with-lubridate/

# 11. What is the relationship between crime and housing prices? 
library(ggplot2)
ggplot(df, aes(x = medv, y = crim)) + geom_point()+ ylim(0, 30)+ xlim(5, 50) + stat_smooth(formula = y ~ log(x))

