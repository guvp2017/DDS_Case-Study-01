---
title: "6306_Project1_Shan"
author: "Shanqing Gu"
date: "2/18/2018"
output: html_document
---



## Introduction

As discussed in our initial meeting, Zwei Analytics reviewed the available 2410 craft beers and 558 breweries in the United States. There are two common beer measurements: ABV and IBU. Rather than telling us how much alcohol there is like ABV, IBU measures the bitterness from hops in a beer on a scale of 0 to 100. Here, we attempt to answer these main questions: (1) How many breweries are present in each states? (2) What are the median ABV and IBU values for each state? (3) Which state has the maximum ABV or IBU beer? (4) Is there an relationship between the ABV and IBU?  

## Q1: How many breweries are present in each state?

In order to address this question, we use the dataset containing 558 breweries to rank and plot the breweries by state with decreasing order. From the plot, we can see the diversity of brewery distribution in each state. Briefly, Colorado (CO) state has 47 breweries, the highest number among all states. California (CA) and Michigan (MI) follows Colorado (CO) in brewery numbers, while Washington DC (DC), Sotuh Dakota (SD), North Dakota (ND), Wyoming (WY) have only one brewery in each state.  


```r
breweries <- read.csv("/Users/shanqinggu/Desktop/6306_Project_01/Breweries.csv", header=TRUE)
str(breweries)
```

```
## 'data.frame':	558 obs. of  4 variables:
##  $ Brew_ID: int  1 2 3 4 5 6 7 8 9 10 ...
##  $ Name   : Factor w/ 551 levels "10 Barrel Brewing Company",..: 355 12 266 319 201 136 227 477 59 491 ...
##  $ City   : Factor w/ 384 levels "Abingdon","Abita Springs",..: 228 200 122 299 300 62 91 48 152 136 ...
##  $ State  : Factor w/ 51 levels " AK"," AL"," AR",..: 24 18 20 5 5 41 6 23 23 23 ...
```

```r
head(breweries)
```

```
##   Brew_ID                      Name          City State
## 1       1        NorthGate Brewing    Minneapolis    MN
## 2       2 Against the Grain Brewery    Louisville    KY
## 3       3  Jack's Abby Craft Lagers    Framingham    MA
## 4       4 Mike Hess Brewing Company     San Diego    CA
## 5       5   Fort Point Beer Company San Francisco    CA
## 6       6     COAST Brewing Company    Charleston    SC
```

```r
summary(breweries)
```

```
##     Brew_ID                           Name           City    
##  Min.   :  1.0   Blackrocks Brewery     :  2   Portland: 17  
##  1st Qu.:140.2   Blue Mountain Brewery  :  2   Boulder :  9  
##  Median :279.5   Lucette Brewing Company:  2   Chicago :  9  
##  Mean   :279.5   Oskar Blues Brewery    :  2   Seattle :  9  
##  3rd Qu.:418.8   Otter Creek Brewing    :  2   Austin  :  8  
##  Max.   :558.0   Sly Fox Brewing Company:  2   Denver  :  8  
##                  (Other)                :546   (Other) :498  
##      State    
##   CO    : 47  
##   CA    : 39  
##   MI    : 32  
##   OR    : 29  
##   TX    : 28  
##   PA    : 25  
##  (Other):358
```

```r
library("plyr")

breweries.count <- as.data.frame(table(breweries$State))
head(breweries.count)
```

```
##   Var1 Freq
## 1   AK    7
## 2   AL    3
## 3   AR    2
## 4   AZ   11
## 5   CA   39
## 6   CO   47
```

```r
breweries.order <- breweries.count[order(breweries.count$Freq, decreasing=TRUE),]
head(breweries.order)
```

```
##    Var1 Freq
## 6    CO   47
## 5    CA   39
## 23   MI   32
## 38   OR   29
## 44   TX   28
## 39   PA   25
```

```r
breweries.order$Var1 <- factor(breweries.order$Var1, levels = breweries.order$Var1[order(breweries.order$Freq)])
breweries.order$Var1
```

```
##  [1]  CO  CA  MI  OR  TX  PA  MA  WA  IN  WI  NC  IL  NY  VA  FL  OH  MN
## [18]  AZ  VT  ME  MO  MT  CT  AK  GA  MD  OK  IA  ID  LA  NE  RI  HI  KY
## [35]  NM  SC  UT  WY  AL  KS  NH  NJ  TN  AR  DE  MS  NV  DC  ND  SD  WV
## 51 Levels:  DC  ND  SD  WV  AR  DE  MS  NV  AL  KS  NH  NJ  TN  HI ...  CO
```

```r
library(ggplot2)

freq.plot <- ggplot(data=breweries.order, aes(x=Var1, y=Freq))+ 
  geom_bar(stat="identity", color="grey", fill="light blue")+
  geom_text(aes(label=Freq), hjust=-0.5, color="purple", position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  labs(title="Number of Craft Breweries in the United States (by each state)", x="State", y="Number of Craft Breweries")
  
freq.plot + coord_flip()
```

![plot of chunk q1](figure/q1-1.png)

# Q2: What are the median ABV and IBU values for each state?

For this question, first we need to merge beer data with breweries data by Brew_ID. Use the firt and last six observations to check the merged file.


```r
beers <- read.csv("/Users/shanqinggu/Desktop/6306_Project_01/Beers.csv", header=TRUE)
str(beers)
```

```
## 'data.frame':	2410 obs. of  7 variables:
##  $ Name      : Factor w/ 2305 levels "#001 Golden Amber Lager",..: 1638 577 1704 1842 1819 268 1160 758 1093 486 ...
##  $ Beer_ID   : int  1436 2265 2264 2263 2262 2261 2260 2259 2258 2131 ...
##  $ ABV       : num  0.05 0.066 0.071 0.09 0.075 0.077 0.045 0.065 0.055 0.086 ...
##  $ IBU       : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ Brewery_id: int  409 178 178 178 178 178 178 178 178 178 ...
##  $ Style     : Factor w/ 100 levels "","Abbey Single Ale",..: 19 18 16 12 16 80 18 22 18 12 ...
##  $ Ounces    : num  12 12 12 12 12 12 12 12 12 12 ...
```

```r
head(beers)
```

```
##                  Name Beer_ID   ABV IBU Brewery_id
## 1            Pub Beer    1436 0.050  NA        409
## 2         Devil's Cup    2265 0.066  NA        178
## 3 Rise of the Phoenix    2264 0.071  NA        178
## 4            Sinister    2263 0.090  NA        178
## 5       Sex and Candy    2262 0.075  NA        178
## 6        Black Exodus    2261 0.077  NA        178
##                            Style Ounces
## 1            American Pale Lager     12
## 2        American Pale Ale (APA)     12
## 3                   American IPA     12
## 4 American Double / Imperial IPA     12
## 5                   American IPA     12
## 6                  Oatmeal Stout     12
```

```r
dim(beers)
```

```
## [1] 2410    7
```

```r
summary(beers)
```

```
##                      Name         Beer_ID            ABV         
##  Nonstop Hef Hop       :  12   Min.   :   1.0   Min.   :0.00100  
##  Dale's Pale Ale       :   6   1st Qu.: 808.2   1st Qu.:0.05000  
##  Oktoberfest           :   6   Median :1453.5   Median :0.05600  
##  Longboard Island Lager:   4   Mean   :1431.1   Mean   :0.05977  
##  1327 Pod's ESB        :   3   3rd Qu.:2075.8   3rd Qu.:0.06700  
##  Boston Lager          :   3   Max.   :2692.0   Max.   :0.12800  
##  (Other)               :2376                    NA's   :62       
##       IBU           Brewery_id                               Style     
##  Min.   :  4.00   Min.   :  1.0   American IPA                  : 424  
##  1st Qu.: 21.00   1st Qu.: 94.0   American Pale Ale (APA)       : 245  
##  Median : 35.00   Median :206.0   American Amber / Red Ale      : 133  
##  Mean   : 42.71   Mean   :232.7   American Blonde Ale           : 108  
##  3rd Qu.: 64.00   3rd Qu.:367.0   American Double / Imperial IPA: 105  
##  Max.   :138.00   Max.   :558.0   American Pale Wheat Ale       :  97  
##  NA's   :1005                     (Other)                       :1298  
##      Ounces     
##  Min.   : 8.40  
##  1st Qu.:12.00  
##  Median :12.00  
##  Mean   :13.59  
##  3rd Qu.:16.00  
##  Max.   :32.00  
## 
```

```r
colnames(beers)[5] <- "Brew_ID"
head(beers)
```

```
##                  Name Beer_ID   ABV IBU Brew_ID
## 1            Pub Beer    1436 0.050  NA     409
## 2         Devil's Cup    2265 0.066  NA     178
## 3 Rise of the Phoenix    2264 0.071  NA     178
## 4            Sinister    2263 0.090  NA     178
## 5       Sex and Candy    2262 0.075  NA     178
## 6        Black Exodus    2261 0.077  NA     178
##                            Style Ounces
## 1            American Pale Lager     12
## 2        American Pale Ale (APA)     12
## 3                   American IPA     12
## 4 American Double / Imperial IPA     12
## 5                   American IPA     12
## 6                  Oatmeal Stout     12
```

```r
beer.merge <- merge.data.frame(beers, breweries, by="Brew_ID", all=FALSE)
head(beer.merge, 6)
```

```
##   Brew_ID        Name.x Beer_ID   ABV IBU
## 1       1  Get Together    2692 0.045  50
## 2       1 Maggie's Leap    2691 0.049  26
## 3       1    Wall's End    2690 0.048  19
## 4       1       Pumpion    2689 0.060  38
## 5       1    Stronghold    2688 0.060  25
## 6       1   Parapet ESB    2687 0.056  47
##                                 Style Ounces             Name.y
## 1                        American IPA     16 NorthGate Brewing 
## 2                  Milk / Sweet Stout     16 NorthGate Brewing 
## 3                   English Brown Ale     16 NorthGate Brewing 
## 4                         Pumpkin Ale     16 NorthGate Brewing 
## 5                     American Porter     16 NorthGate Brewing 
## 6 Extra Special / Strong Bitter (ESB)     16 NorthGate Brewing 
##          City State
## 1 Minneapolis    MN
## 2 Minneapolis    MN
## 3 Minneapolis    MN
## 4 Minneapolis    MN
## 5 Minneapolis    MN
## 6 Minneapolis    MN
```

```r
tail(beer.merge, 6)
```

```
##      Brew_ID                    Name.x Beer_ID   ABV IBU
## 2405     556             Pilsner Ukiah      98 0.055  NA
## 2406     557  Heinnieweisse Weissebier      52 0.049  NA
## 2407     557           Snapperhead IPA      51 0.068  NA
## 2408     557         Moo Thunder Stout      50 0.049  NA
## 2409     557         Porkslap Pale Ale      49 0.043  NA
## 2410     558 Urban Wilderness Pale Ale      30 0.049  NA
##                        Style Ounces                        Name.y
## 2405         German Pilsener     12         Ukiah Brewing Company
## 2406              Hefeweizen     12       Butternuts Beer and Ale
## 2407            American IPA     12       Butternuts Beer and Ale
## 2408      Milk / Sweet Stout     12       Butternuts Beer and Ale
## 2409 American Pale Ale (APA)     12       Butternuts Beer and Ale
## 2410        English Pale Ale     12 Sleeping Lady Brewing Company
##               City State
## 2405         Ukiah    CA
## 2406 Garrattsville    NY
## 2407 Garrattsville    NY
## 2408 Garrattsville    NY
## 2409 Garrattsville    NY
## 2410     Anchorage    AK
```

```r
dim(beer.merge)
```

```
## [1] 2410   10
```

# 

Secondly, we need to report the numbers of NA's in each column. There are 62 NAs in the ABV column, while 1005 in the IBU column. 


```r
sum(is.na(beer.merge$Brew_ID))
```

```
## [1] 0
```

```r
sum(is.na(beer.merge$Name.x))
```

```
## [1] 0
```

```r
sum(is.na(beer.merge$Beer_ID))
```

```
## [1] 0
```

```r
sum(is.na(beer.merge$ABV))
```

```
## [1] 62
```

```r
sum(is.na(beer.merge$IBU))
```

```
## [1] 1005
```

```r
sum(is.na(beer.merge$Style))
```

```
## [1] 0
```

```r
sum(is.na(beer.merge$Ounces))
```

```
## [1] 0
```

```r
sum(is.na(beer.merge$Name.y))
```

```
## [1] 0
```

```r
sum(is.na(beer.merge$City))
```

```
## [1] 0
```

```r
sum(is.na(beer.merge$State))
```

```
## [1] 0
```

# 

lastly, we compute and plot the median ABV and IBU for each state to compare by the SummaryBy statement in doBy libarary.


```r
library(doBy)
```

```
## Error in library(doBy): there is no package called 'doBy'
```

```r
bar.median <- summaryBy(ABV+IBU~State,data=beer.merge, FUN=median, na.rm=TRUE)
```

```
## Error in summaryBy(ABV + IBU ~ State, data = beer.merge, FUN = median, : could not find function "summaryBy"
```

```r
dim(bar.median)
```

```
## [1] 51  3
```

```r
bar.median
```

```
##    State ABV.median IBU.median
## 1     AK     0.0560       46.0
## 2     AL     0.0600       43.0
## 3     AR     0.0520       39.0
## 4     AZ     0.0550       20.5
## 5     CA     0.0580       42.0
## 6     CO     0.0605       40.0
## 7     CT     0.0600       29.0
## 8     DC     0.0625       47.5
## 9     DE     0.0550       52.0
## 10    FL     0.0570       55.0
## 11    GA     0.0550       55.0
## 12    HI     0.0540       22.5
## 13    IA     0.0555       26.0
## 14    ID     0.0565       39.0
## 15    IL     0.0580       30.0
## 16    IN     0.0580       33.0
## 17    KS     0.0500       20.0
## 18    KY     0.0625       31.5
## 19    LA     0.0520       31.5
## 20    MA     0.0540       35.0
## 21    MD     0.0580       29.0
## 22    ME     0.0510       61.0
## 23    MI     0.0620       35.0
## 24    MN     0.0560       44.5
## 25    MO     0.0520       24.0
## 26    MS     0.0580       45.0
## 27    MT     0.0550       40.0
## 28    NC     0.0570       33.5
## 29    ND     0.0500       32.0
## 30    NE     0.0560       35.0
## 31    NH     0.0550       48.5
## 32    NJ     0.0460       34.5
## 33    NM     0.0620       51.0
## 34    NV     0.0600       41.0
## 35    NY     0.0550       47.0
## 36    OH     0.0580       40.0
## 37    OK     0.0600       35.0
## 38    OR     0.0560       40.0
## 39    PA     0.0570       30.0
## 40    RI     0.0550       24.0
## 41    SC     0.0550       30.0
## 42    SD     0.0600         NA
## 43    TN     0.0570       37.0
## 44    TX     0.0550       33.0
## 45    UT     0.0400       34.0
## 46    VA     0.0565       42.0
## 47    VT     0.0550       30.0
## 48    WA     0.0555       38.0
## 49    WI     0.0520       19.0
## 50    WV     0.0620       57.5
## 51    WY     0.0500       21.0
```

```r
bar.median.ABV <- bar.median[order(bar.median$ABV.median, decreasing=TRUE),]
bar.median.IBU <- bar.median[order(bar.median$IBU.median, decreasing=TRUE),]

require(plyr)

barplot(bar.median.ABV$ABV.median, las=2, horiz=TRUE, 
        main = "The Median Alcohol Content for Each State", 
        xlab = "The Median Alcohol Content (ABV)", 
        col=rgb(0.2,0.4,0.6,0.6),
        names.arg=(bar.median$State), font=1,
        cex.axis=0.75, cex.names =0.75,
        xlim = c( 0, 0.07))
title(ylab="State", mgp=c(3,1,0), cex.lab=1.2)
```

![plot of chunk q4](figure/q4-1.png)

```r
barplot(bar.median.IBU$IBU.median, las=2, horiz=TRUE, 
        main = "The International Bitterness Unit for Each State", 
        xlab = "The International Bitterness Unit (IBU)", 
        col=rgb(0.2,0.4,0.6,0.6),
        names.arg=(bar.median.IBU$State), font=1,
        cex.axis=0.75, cex.names =0.75,
        xlim = c( 0, 50))
title(ylab="State", mgp=c(3,1,0), cex.lab=1.2)
```

![plot of chunk q4](figure/q4-2.png)

# Q3  Which state has the maximum ABV or IBU beer? 

Use which.max function to find the state to have the maximum ABU and IBU beer.  As shown below, Colorado (CO) has the highese ABU beer and Oregon has the hight IBU beer.


```r
ABV.Max <- beer.merge[which.max(beer.merge$ABV),]
ABV.Max
```

```
##     Brew_ID                                               Name.x Beer_ID
## 375      52 Lee Hill Series Vol. 5 - Belgian Style Quadrupel Ale    2565
##       ABV IBU            Style Ounces                  Name.y    City
## 375 0.128  NA Quadrupel (Quad)   19.2 Upslope Brewing Company Boulder
##     State
## 375    CO
```

```r
IBU.Max <- beer.merge[which.max(beer.merge$IBU),]
IBU.Max
```

```
##      Brew_ID                    Name.x Beer_ID   ABV IBU
## 1857     375 Bitter Bitch Imperial IPA     980 0.082 138
##                               Style Ounces                  Name.y    City
## 1857 American Double / Imperial IPA     12 Astoria Brewing Company Astoria
##      State
## 1857    OR
```

# 
From the summary statistics for the ABV variable, we can check the deta details: n, mean, sd, median, min, max, range and so on.


```r
library(psych)
```

```
## 
## Attaching package: 'psych'
```

```
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
```

```r
describe(beer.merge$ABV)
```

```
##    vars    n mean   sd median trimmed  mad min  max range skew kurtosis se
## X1    1 2348 0.06 0.01   0.06    0.06 0.01   0 0.13  0.13 0.96     1.14  0
```

# Q4 Is there an relationship between the ABV and IBU? 

The scatterplot and regression line support a strong psotive correlation between ABV and IBU. 


```r
library(ggplot2)

beer.merge <- na.omit(beer.merge)
ggplot(data = beer.merge, na.rm=TRUE) + 
  geom_point(mapping = aes(x = ABV, y = IBU, color = beer.merge$State))+
  labs(title="Relationship between bitterness of the beer (IBU) and its alcoholic contents (ABV) with scatter plot", 
      x="Alcoholic contents (ABV)", y="Bitterness of the Beer (IBU)")
```

![plot of chunk q7](figure/q7-1.png)

```r
lm.fit <- lm(IBU ~ ABV, data = beer.merge)
summary (lm.fit)
```

```
## 
## Call:
## lm(formula = IBU ~ ABV, data = beer.merge)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -78.849 -11.977  -0.721  13.997  93.458 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -34.099      2.326  -14.66   <2e-16 ***
## ABV         1282.037     37.860   33.86   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.26 on 1403 degrees of freedom
## Multiple R-squared:  0.4497,	Adjusted R-squared:  0.4493 
## F-statistic:  1147 on 1 and 1403 DF,  p-value: < 2.2e-16
```

```r
plot(IBU~ABV, data=beer.merge)
abline(lm.fit)
```

![plot of chunk q7](figure/q7-2.png)

```r
cor.test(beer.merge$ABV, beer.merge$IBU)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  beer.merge$ABV and beer.merge$IBU
## t = 33.863, df = 1403, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.6407982 0.6984238
## sample estimates:
##       cor 
## 0.6706215
```
