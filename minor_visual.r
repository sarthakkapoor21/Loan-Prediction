setwd("/Users/sarthakkapoor21/Desktop")

# Load required libraries
suppressPackageStartupMessages(library(readr)) # CSV file I/O, e.g. the read_csv function
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2)) # Data visualization
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(latticeExtra))
suppressPackageStartupMessages(library(vcd))
suppressPackageStartupMessages(library(lubridate)) #For date manipulation

##Importing Data

suppressMessages(loan <- read_csv("loan.csv"))

##Checking the dimension and Structure of the data

dim(loan)      #Check the dimension of data using dim().
#Analyse the structure of the data using str() and check whether any numerical vlaues need to be changed as factors and vice cersa.
#str(loan)

##Converting few relevant columns as factors

loan$id <- as.factor(loan$id)
loan$member_id <- as.factor(loan$member_id)
loan$grade <- as.factor(loan$grade)
loan$sub_grade <- as.factor(loan$sub_grade)
loan$home_ownership <- as.factor(loan$home_ownership)
loan$emp_length <- as.factor(loan$emp_length)
loan$issue_d <- dmy(paste0("01-",loan$issue_d))
loan$verification_status <- as.factor(loan$verification_status)
loan$pymnt_plan <- as.factor(loan$pymnt_plan)
loan$application_type <- as.factor(loan$application_type)
loan$loan_status <- as.factor(loan$loan_status)
loan$policy_code <- as.factor(loan$policy_code)
loan$initial_list_status <- as.factor(loan$initial_list_status)
loan$purpose <- as.factor(loan$purpose)
loan$title <- as.factor(loan$title)
loan$addr_state <- as.factor(loan$addr_state)
#loan$last_credit_pull_d <- dmy(paste0("01-",loan$last_credit_pull_d))

##Understanding the distribution of loan amount
##(1) Using histogram


ggplot(data=loan, aes(loan_amnt))+geom_histogram(bins=40,color="blue",fill="cyan")


## (2) Using density plot


ggplot(data=loan, aes(loan_amnt))+geom_density(color="blue",fill="cyan",alpha=0.2)


## (3) To distribution of the loan amount using histogram according to the type of loan grade


ggplot(data=loan,aes(loan_amnt, col=grade))+
  geom_histogram(bins=40) +
  facet_grid(grade ~ .)


## (4) To distribution of the loan amount using density plot according to the type of loan grade


ggplot(data=loan,aes(loan_amnt, fill=grade))+
  geom_density(alpha=0.25) +
  facet_grid(grade ~ .)


## (5) To examine the trend of total loan amount


ggplot(loan, aes(x=issue_d, y=loan_amnt)) +
  stat_summary(fun.y="sum", geom="bar") +
  labs(y ="Total Loan Amount")


##From the above plot, we can observe that the amount of loan amount given to the customers has increased over the years.

## (6) To examine the total loan amount based on loan grade


ggplot(loan, aes(x=grade, y=loan_amnt, fill=grade)) +
  stat_summary(fun.y="sum", geom="bar") +
  labs(y ="Total Loan Amount",title="Total loan amount based on loan grade")


##From the different plots such as histogram, density plot and the above bar chart, we can observe that maximum lending was in Grade C.

## (7) To examine the distribution of loan amount according to ownership of house


ggplot(data=loan, aes(home_ownership,loan_amnt,fill=home_ownership))+geom_boxplot(outlier.color = "blue")+labs(title="Box plot of loan amount")


## (8) To examine distribution of interest rates for different grades

ggplot(data=loan, aes(grade,int_rate,fill=grade))+geom_boxplot(outlier.color = "blue")+labs(title="Box plot of Interest rate")


##We observe the interest rates of loan grades A, B and C are relatively lower than other grades, probably there are higher number of borrowings observed.

## Creating a new column to obtain loan payment status

##We have created a column called loan payment status. It contains three levels: fully paid, late and charged off.

levels(loan$loan_status)
loan$lstatus <- "Other"
loan$lstatus[which(loan$loan_status=="Fully paid"|loan$loan_status=="Does not meet the credit policy. Status:Fully Paid"|loan$loan_status=="Current")]<-"Fully Paid"
loan$lstatus[which(loan$loan_status=="Late (16-30 days)"|loan$loan_status=="Late (31-120 days)")]<-"Late"
loan$lstatus[which(loan$loan_status=="Charged Off"|loan$loan_status=="Does not meet the credit policy. Status:Charged Off")]<-"Charged Off"
levels(loan$lstatus);
loan$lstatus<-as.factor(loan$lstatus)
table(loan$lstatus)


## To understand the distribution of loan amount based on loan payment status for each loan grade


ggplot(data=loan,aes(x=lstatus,y= loan_amnt))+
  geom_boxplot() +
  facet_grid(grade~.)


## To check whether there is any relationship between loan payment status and grade of the loan

mosaic(~lstatus+grade,
       data=loan,
       gp=shading_max,
       split_vertical=T)

##Above plot is called Mosaic plot. It helps to understand the relationship between two categorical variables. Here, we have analysed the relationship between loan payment status
##and grade of loan. The statistical test used for this analysis is Chi-Square test. From the plot, we observe that there is no relationship between loan payment status and grade of loan.
##This is confirmed the p-value (< 0.05).

## To examine the correlation among variables: loan amout, funded amount and installment amount


suppressPackageStartupMessages(library(corrplot, quietly = TRUE)) #Correlation plots

data2 <- data.frame(loanamt =loan$loan_amnt, intr =loan$int_rate,fund= loan$funded_amnt, instlmnt = loan$installment)
#head(data2)
M<- cor(data2)
corrplot(M,method = "color")

## To examine the correlation among variables: loan amout, funded amount and installment amount
## Gives the correlation value

corrplot(M, method="number")


##Two variables are said to be correlated when the correlation co-efficient value is greater than 0.5. From the plot, we can see that loan amount is highly correlated to installment amout.
##Likewise, fund and loan amount as well as fund and instalment amount are correlated.

## To compare the total payment and funded amount for different loan grades

#Obtained using doubleYScale function from lattice package. Helps to plot two types of grpahs (i.e bar chart and line plot) in a single chart area.
loan%>%group_by(grade)%>%summarise(tamt=mean(total_pymnt)) -> data1
loan%>%group_by(grade)%>%summarise(famt = mean(funded_amnt))-> data3
plot1 <- barchart(tamt ~ grade, data=data1, xlab="Grade", ylab="Average Payment")
plot2 <- xyplot(famt~as.factor(grade), data=data3, type="l", ylab = "Average funded amount")
doubleYScale(plot1, plot2, style1 = TRUE, add.ylab2 = TRUE)


## Time Series plot of loan amount (Grade wise)

ggplot(loan, aes(x=issue_d, y=loan_amnt,fill=grade)) +
     stat_summary(fun.y="sum", geom="area") +
     labs(y ="Average Loan Amount")


## Another way of displaying distribution of loan amount

suppressPackageStartupMessages(library(DescTools)) #For Desc() function to describe data

Desc(loan$loan_amnt, main = "Loan amount distribution", plotit = TRUE)


## Viewing loan amount statewise

ggplot(loan, aes(x=addr_state, y=loan_amnt, fill=addr_state)) +
  stat_summary(fun.y="sum", geom="bar") + coord_flip()+
  labs(y ="Total Loan Amount",x="State",title="Bar Chart of loan amount for each state")


## Heatmap of loan amount statewise


suppressPackageStartupMessages(library(maps))
loan$region <- loan$addr_state
loan$region <- as.factor(loan$region)
levels(loan$region)<- c("alaska", "alabama","arkansas", "arizona", "california","colorado","connecticut","district of columbia","delaware","florida","georgia","hawaii","iowa","idaho","illinois","indiana","kansas","kentucky","louisiana","massachusetts","maryland","maine","michigan","minnesota","missouri","mississippi","montana","north carolina","north dakota","nebraska","new hampshire","new jersey","new mexico","nevada","new york","ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina","south dakota","tennessee","texas","utah","virginia","vermont","washington","wisconsin","west virginia","wyoming")

all_states <- map_data("state")
state_by_loan <-loan %>% group_by(region) %>%
                summarise(value = sum(loan_amnt, na.rm=TRUE))
state_by_loan$region <- as.character(state_by_loan$region)

Total <- merge(all_states, state_by_loan, by="region")

p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$value),colour="white"
      ) + scale_fill_continuous(low = "skyblue", high = "darkblue", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Gradient of loan amount"
                            ,title = "Heat Map of loan amount in all states", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())


##From both the plots, loan amount amount lent is highest in California.
