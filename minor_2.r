setwd("/Users/sarthakkapoor21/Desktop")

```{r load, echo=FALSE, warning=FALSE, message=FALSE}
library(choroplethr)
library(choroplethrMaps)
library(DescTools)
library(dplyr)
library(ggplot2)
library(readr)

options(scipen=999)
loanbook <- read_csv("loan.csv")

# load the state names
data(state.regions)

# merge the loan book with the state names
loanbook <- merge(loanbook, state.regions, by.x = "addr_state", by.y = "abb")
```

## Data overview

```{r, echo = TRUE}
# print dimentions
dim(loanbook)

# print column names
colnames(loanbook)
```

I'm not printing these results for conciseness:

```{r, eval=FALSE}
str(loanbook)
summary(loanbook)
```

## Missing variables

```{r, echo=TRUE}
library(readxl)

dataDictionary <- read_excel("LCDataDictionary.xlsx")

# fields available in the data dictionary
dd_names <- as.character(na.omit(dataDictionary$LoanStatNew))

# fields available in the loan book
loanbook_names <- names(loanbook)

# show the fields described in data dictionary but not in the loan book
setdiff(dd_names, loanbook_names)
```

## Loan amounts

We created the density plot, box plot, and empirical distribution function plot.
```{r, echo=TRUE}
Desc(loanbook$loan_amnt, main = "Loan amount distribution", plotit = TRUE)
```

```{r, echo=TRUE}
loanbook$issue_d <- as.Date(gsub("^", "01-", loanbook$issue_d), format="%d-%b-%Y")

amnt_df <- loanbook %>%
  select(issue_d, loan_amnt) %>%
  group_by(issue_d) %>%
  summarise(Amount = sum(loan_amnt))

ts_amnt <- ggplot(amnt_df,
                  aes(x = issue_d, y = Amount))
ts_amnt + geom_line() + xlab("Date issued")
```

## Loan statuses

The loanbook consists of loans in various statuses so I started with exploring their frequency.
```{r, echo=TRUE}
Desc(loanbook$loan_status, plotit = T)
```

Then I checked the distribution of loan amounts by status.
```{r, echo=TRUE}
box_status <- ggplot(loanbook, aes(loan_status, loan_amnt))
box_status + geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) +
  labs(list(
    title = "Loan amount by status",
    x = "Status",
    y = "Amount"))
```

Here's how the value of loans of different grades was changing over time
```{r, echo=TRUE}
amnt_df_grade <- loanbook %>%
  select(issue_d, loan_amnt, grade) %>%
  group_by(issue_d, grade) %>%
  summarise(Amount = sum(loan_amnt))

ts_amnt_grade <- ggplot(amnt_df_grade,
                  aes(x = issue_d, y = Amount))
ts_amnt_grade + geom_area(aes(fill=grade)) + xlab("Date issued")
```

## Maps

### Loans by value
```{r, echo=TRUE}
state_by_value <-
loanbook %>% group_by(region) %>%
  summarise(value = sum(loan_amnt, na.rm=TRUE))

state_choropleth(state_by_value, title = "Value by State")
```

### Loans by volume
```{r, echo=TRUE}
state_by_volume <-
loanbook %>% group_by(region) %>%
  summarise(value = n())

state_choropleth(state_by_volume, title = "Volume by State")
```

## Loan reasons

### What's the reason for taking a loan with LendingClub?

```{r, echo=TRUE}
Desc(loanbook$purpose, main = "Loan purposes", plotit = TRUE)
Desc(loanbook$title, main = "Loan titles", plotit = TRUE)
```

## Word cloud

## Word cloud gives a good overview of the loan titles (gives by borrowers).
## This information should explain what kind of loans are being funded by LendingClub.

## In order to create a word cloud, first I loaded the necessary libraries,
## then I preprocessed the 'title' column by removing punctuation and transforming it to lower case.
## This analysis was run on the first 10000 rows to speed up the process.

## ```{r, echo=TRUE, warning=FALSE, message=FALSE}
## library(tm)
## library(RColorBrewer)
## library(wordcloud)

## loan_descriptions.corpus <- Corpus(DataframeSource(data.frame(head(loanbook[,23], n=10000))))
## loan_descriptions.corpus <- tm_map(loan_descriptions.corpus, removePunctuation)
## loan_descriptions.corpus <- tm_map(loan_descriptions.corpus, content_transformer(tolower))

## wordcloud(loan_descriptions.corpus,
##           max.words = 100,
##           random.order=FALSE,
##           rot.per=0.30,
##           use.r.layout=FALSE,
##           colors=brewer.pal(8, "Paired"))
## ```

## Loan grades

Here is the overview of the occurrence of loans of different grades:
```{r, echo=TRUE}
Desc(loanbook$grade, main = "Loan grades", plotit = TRUE)
```

The last step (so far) was checking whether the interest rates are dependent on the loan grade.
```{r, echo=TRUE}
Desc(int_rate ~ grade, loanbook, digits = 1, main = "Interest rate by grade", plotit = TRUE)
```

Unsurprisingly, the higher the grade (more risky loan), the higher the interest rates.

## Default prediction

There are several statuses which indicate that loans are not performing well.
We put them into two groups.

```{r, echo=TRUE, warning=FALSE, message=FALSE}
# 'bad' statuses
bad_indicators <- c("Charged Off ",
                    "Default",
                    "Does not meet the credit policy. Status:Charged Off",
                    "In Grace Period",
                    "Default Receiver",
                    "Late (16-30 days)",
                    "Late (31-120 days)")

# assign certain statuses to a 'bad' ('0') group
loanbook$is_bad <- ifelse(loanbook$loan_status %in% bad_indicators, 0,
                          ifelse(loanbook$loan_status=="", NA, 1)
                          )
```

Then I wanted to check whether there is a difference between the 'bad' and 'good' loans for numeric variables.

```{r, echo=TRUE, warning=FALSE, message=FALSE}
# figure out which columns are numeric so that we can look at the distribution
numeric_cols <- sapply(loanbook, is.numeric)

# turn the data into long format
library(reshape2)
loanbook.lng <- melt(loanbook[,numeric_cols], id="is_bad")

# plot the distribution for 'bad' and 'good' for each numeric variable
p <- ggplot(aes(x = value, group = is_bad, colour = factor(is_bad)),
            data = loanbook.lng)

# create the plot to check if there are any good variables that can be used in predictive models
p + geom_density() +
  facet_wrap(~variable, scales="free")
```

The plots above can be the basis for chosing variables for predictive models (note: scales need fixing).

It seems like Annual Income and Interest Rate are two variables that can be good predictors of how loans will behave.
We can have a closer look at particular cases:

```{r, echo=TRUE, warning=FALSE, message=FALSE}
library(DT)

loanbook %>%
  filter(is_bad == '0') %>%
  select(annual_inc, int_rate, loan_status) %>%
  datatable(., options = list(pageLength = 10))
```
