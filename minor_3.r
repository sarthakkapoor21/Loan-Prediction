# Import Libraries

```{r, message=F, warning=F}
library(ggplot2)
library(readr)
library(data.table)
library(RColorBrewer)
library(maps)
library(plotly)
library(readr)
library(dplyr)
library(data.table)
library(lattice)
library(funModeling)
library(corrplot)
library(vcd)
library(vcdExtra)
library(zoo)
library(sqldf)
library(ggthemes)
library(scales)
library(hexbin)
library(graphics)
library(latticeExtra)
```

# Read input file

```{r}
#loan <- read_csv("loan.csv", n_max=100000)
loan <- read_csv("loan.csv")
```

#(1) Map States to Count of loans

##Intent:
To create a chart based on geographic distribution of loans across various states.

##Steps:
- Create a data table (a) with all the state abbrevations from the datase.
- Change column names of the table
- Match the state abbrevations to actual state names
- Fetch the actual state name into a variable (all_states)
- Join the variables (a and all_states) using the column region
- Use ggplot with geom_map to fill and encode colours on basis of counts

```{r}
a=data.table(table(loan$addr_state))

setnames(a,c("region","count"))

a$region=sapply(state.name[match(a$region,state.abb)],tolower)

all_states <- map_data("state")

Total <- merge(all_states, a, by="region")

ggplot(Total, aes(x=long, y=lat, map_id = region)) +
  geom_map(aes(fill= count), map = all_states)+
  labs(title="Loan counts in respective states",x="",y="")+
  scale_fill_gradientn("",colours=terrain.colors(10),guide = "legend")+
  theme_bw()
```
##Inference
From the chart above, seems California is the highest borrower


#(2)Average intrest rate vs grade

##Intent:
Create an interactive (zoomable) chart for average rate vs grade of the loan
Display the usage of sqldf library for individuals already aware of sql

##Steps:
- Extract year in format (yyyy) from the column issue_d(mon-yyyy)
- Use sqldf to get average loan amount, grouping by grade and loan issue year
- Use ggplot with geom_line to create the intended chart
- Use Plotly to covert to interactive chart

```{r}
loan$issue_year <- year(as.yearmon(loan$issue_d,"%b-%Y"))

d=sqldf("select avg(loan_amnt) as avg_amnt, grade, issue_year from loan group by grade, issue_year")
g <- ggplot(d, aes(x = issue_year , y = avg_amnt, color=grade)) +  geom_line(alpha = 0.4) + labs(x = "Year of Loan Issue", y="Average of Loan Amount issued")+theme_solarized()
ggplotly(g, tooltip = c("grade"))
```

##Inference:
Grade G has the highest average loan amount issued, consecutively since 2010.

#(3) Convert columns to factors
```{r}
cols <- c("term",
          "grade",
          "sub_grade",
          "home_ownership",
          "emp_length",
          "verification_status",
          "loan_status",
          "pymnt_plan",
          "purpose",
          "addr_state"
)
loan[,cols] <- lapply(loan[,cols],as.factor)

```

#(4) Density frequency plot - loan amount and total payment

##Intent:
To plot a density frequency loan amount and total payment

##Steps:
- Extract values to the corresponding columns as individual variables
- Now, combine your two dataframes into one.  First make a new column in each that will be a variable to identify where they came from later.
- Combine into your new data frame loan1
- Plot the result

```{r}
loanpay=data.frame(val=loan$total_pymnt)
loanamnt=data.frame(val=loan$loan_amnt)

loanpay$ind='Total Payment'
loanamnt$ind='Loaned Amount'

loan1=rbind(loanpay,loanamnt)

ggplot(loan1,aes(val, fill = ind))+ geom_density(alpha = 0.2)+ylab ("frequency")+theme_gdocs()
```

##Inference:
Highest total payment amount is 10,000

#(5) Histogram for intrest rate based on grade type

##Intent:
Create interactive plot for interest rate distribution accross various grades

##Steps:
- Get frequency grouped by grade and interest rate
- Use ggplot to create the plot
- Use plotly to convert this into an interactive plot

```{r}
hist1=sqldf("select count(*) as freq,grade,int_rate from loan group by grade,int_rate")

p <- ggplot(data = hist1, aes(x = int_rate, y = freq)) +
  geom_smooth(aes(colour = grade, fill = grade)) + facet_wrap(~ grade) + labs(x = "Interest Rate", y="Frequency")

ggplotly(p)

```

##Inference:
Interest rate for F Grade is most distribute accross the spectrum

#(6) Hexbin density plot for loan amount

##Intent:
Plot a hexbin chart based frequency of interest rate for the loan issued.

##Steps:
- select interest rate and frequency using sqldf into a variable.


```{r}
hexbin1=data.frame(sqldf("select int_rate,count(*) as freq from loan group by int_rate"))
plot(hexbin(hexbin1), colramp= colorRampPalette(rev(brewer.pal(11,'Spectral'))))
```
##Inference:
For the given dataset highest rate of occurance is between 15 and 20%.

#(7) Geom bar plot for loan grades based on purpose

##Intent:
Identify the general purpose for which loans are taken, for various grades.

##Steps:
- Plot geom_bar for Grade vs purpose.

```{r}
ggplot(data=loan, aes(x=grade, fill=purpose)) +
  geom_bar()

```
##Inference:
Debt consolidation seems to be the most general purpose for which loans are taken, followed by credit card

#(8) Box plot for interest rate according to purpose

##Intent:
Identify the highest range for interest rate based on purpose

##Steps:
- Create a geom boxplot based on purpose and interest rate.

```{r}
ggplot(data=loan, aes(x=purpose,y=int_rate)) +
  geom_boxplot(fill="lightgreen", outlier.color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```
##Inference:
The range of the interest rates is highest for 'Vacation' followed by 'moving' and 'other'


#(9) Are variables loan_amnt, int_rate and total_rec_int correlated?

##Intent:
Identify if there is a relationship between loan amount, interest rare and total received interest.

##Steps:
- Create a corrplot based on the variables.
- The variables are referenced by respective column numbers
```{r}
corrplot(cor(loan[c(3,7,42)]), type="upper", order="hclust")
```
##Inference
There is an strong correlation for total received interest and loan amount.

#(10) Which type of home ownership has least interest rate and loan amount?

##Intent:
Plot a 3D chart for loan amount vs interest rate vs home ownership

##Steps:
- Select the data from the dataset
- Convert values to numeric and factor
- Get the colour for display
- Print the graph using cloud function

```{r}


n10=sqldf("select loan_amnt as x, int_rate as y,home_ownership as z from loan order by 1 ,2")

n10$x=as.numeric(n10$x)
n10$z=as.factor(n10$z)

cols<-function(n) {
   colorRampPalette(c("#FFC0CB", "#CC0000"))(10)                # 10 distinct colors
}

cloud(y~x+z,n10, panel.3d.cloud = panel.3dbars, col="white",    # white borders for bars
  xbase = 1, ybase = 1, zlim = c(0, max(n10$y)),                # No space around the bars
  scales = list(arrows = FALSE, just = "right"),
  xlab = NULL, ylab = NULL, zlab = NULL,
  col.facet = level.colors(n10$x, at = do.breaks(range(n10$x), 100),
                           col.regions = cols,                  # color ramp for filling the bars
                           colors = TRUE),
  colorkey = list(col = cols, at = do.breaks(range(n10$x), 10)),
  screen = list(z = 65, x = -65))

```
##Inference:
From the given bar chart above, interest rate is least for Purpose "None", followed by "Other".
