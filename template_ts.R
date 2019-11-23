
library(prophet)
library(forecast)
#install.packages("prophet")

#### Load Libraries ####
library(pacman)
library(dplyr)
library(lubridate)
library(tidyr)
pacman::p_load(readr, caret, plotly, ggplot2, 
               labeling, promises, ggridges, 
               doParallel,  e1071, mlbench,# inum,
               corrplot#, ggpubr
               , rpart, rpart.plot, gbm
               , boot, dplyr,
               reshape, Rserve#, tidyvers
               , padr)

#'callr', 'pkgbuild', 'pkgload', 'rcmdcheck', 'usethis'

)
# 
# devtools::install_github("tidyverts/tsibble")
# 7

isHoliday(x, goodFriday = F, board = F, inaug = board, businessOnly = T)

setwd("D:/dropbox/Dropbox/ubiqum/25. Novartis dataton/NovartisDatath/")

TestSet <- read.csv("./info/datathon2019_test_final_csv.csv") %>%
  mutate(expenses = case_when(is.na(expenses) ~ 0, 
                              TRUE ~ expenses)) 
SubMettAgg1DFDay <- read.csv("./info/datathon2019_train.csv") %>% filter(cluster != "Product_15") #%>% 
#  filter(cluster != "Product_31" & country != "Country_f")
#%>% #filter(as.character(country) == countryvar) %>% 
#  filter(as.character(cluster) == "Product_31")

  # write.csv2(SubMettAgg1DFDay,"./info/train_tableau.csv")
# write.csv2(TestSet,"./info/test_tableau.csv")





summary(TestSet)
TestSet$date <-  as.POSIXct(TestSet$date, "%Y-%m-%d") 
SubMettAgg1DFDay$date <-  as.POSIXct(SubMettAgg1DFDay$date, "%Y-%m-%d") 
attr(SubMettAgg1DFDay$date, "tzone")  <- "Europe/Paris"

SubMettAgg1DFDay$year <- NULL
SubMettAgg1DFDay$month <- NULL
SubMettAgg1DFDay$month_in_quarter <- NULL

# SubMettAgg1DFDay$ExpXWorkDay <- 

# summary(  SubMettAgg1DFDay %>% select(-expenses_1, -expenses_2, -expenses_3, -expenses_4, -expenses_5, -expenses_6
# )       )
# 
# str(SubMettAgg1DFDay)


# library(mice)
# md.pattern(
#   SubMettAgg1DFDay %>% select(-expenses_1, -expenses_2, -expenses_3, -expenses_4, -expenses_5, -expenses_6
#                               )                          
# )
# 
# Arranged <- SubMettAgg1DFDay %>% arrange(date, cluster, country)
# 
# Arranged[is.na(SubMettAgg1DFDay$months_from_launch_date) & !is.na(SubMettAgg1DFDay$volume),]$months_from_launch_date <- 
# 
#   
#   Arranged[is.na(Arranged$months_from_launch_date) ,#& !is.na(SubMettAgg1DFDay$volume)
#            ] %>% select(country,cluster,date,months_from_launch_date)


# SubMettAgg1DFDay %>%  filter(is.na(months_from_launch_date) )  group_by(cluster, country) %>% summarize(volume = sum(volume)
#              
#                                                                                                                                                                                      
                                                                                                                                                         ,

Initialdf <- SubMettAgg1DFDay %>% select(date, cluster, country, volume, expenses, net_price) %>%
  mutate(expenses = case_when(is.na(expenses) ~ 0, 
                              TRUE ~ expenses)) 

summary(TestSet)

for (countryvar in   as.vector((TestSet  %>% select(country) %>% arrange(desc(country)) %>% unique())$country)  # c("Country_h","Country_i","Country_j") #c("Country_h","Country_i","Country_j")
     ) {  # countryvar = "Country_f"
  TmpDfLoop <- TestSet  %>% filter(as.character(country) == countryvar)
  TmpDf <- Initialdf  %>% filter(as.character(country) == countryvar) 
  
  for (product in as.vector((TmpDfLoop %>% select(cluster) %>% arrange(desc(cluster)) %>% unique())$cluster)
  ){  # product = "Product_84"
         TmpDf2 <- TmpDf %>% filter(cluster == product) %>% rename(y = volume, ds= date)
  

 
 

    
    if (!is.na(as.numeric(TmpTrainSet %>% summarize(expenses = sum(expenses))))
    ){
      if (as.numeric(TmpTrainSet %>% summarize(expenses = sum(expenses))) < -100000000 )
      {
        m <- prophet(seasonality_mode='multiplicative', growth='logistic'#, 
                     #,changepoint_prior_scale=0.5
        )
         TmpDf2$floor <- 0
         TmpDf2$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*(3/4)) 
        
        m <- add_regressor(m, 'expenses' 
                           , mode='additive'
                           )
        
      }
    }
    else 
    {
      m <- prophet(seasonality_mode='multiplicative'
                   #,changepoint_prior_scale=0.5
      )
    }
         
         #summary(TmpDf2)
         # TmpDf2$floor <- 0
         # TmpDf2$cap  <- 2000000
         train_size = as.numeric(TmpDf2 %>% count()) * 0.66
         TmpTrainSet <- TmpDf2[1:train_size,] # %>% arrange(ds)
         TmpTestSet <- TmpDf2[train_size:as.numeric(TmpDf2 %>% count()),] # %>% arrange(ds)
         
         
      #m <- add_regressor(m, 'net_price')    
    m <- fit.prophet(m,#yearly.seasonality=FALSE,
                     TmpTrainSet #%>% dplyr::select(y,ds,expenses, net_price)  #y,ds
       
      #, holidays = ProfetDF %>% filter(upper_window == 1) %>% select(ds,holiday, lower_window,upper_window)
      #, origin="2007-01-01"
    )
    
    
    #print(adf.test(TmpDf2))
    

    
    #future <- make_future_dataframe(m, periods = 24, freq = 'month')
    future <-  TmpTestSet #%>% #filter(cluster == product) %>% filter(as.character(country) == countryvar)  %>%
      #rename( ds= date) %>% 
      #select(ds,expenses,net_price)
    forecast <- predict(m, future, periods =  future %>% count()
                        )
    
    #cbind(forecast,  TmpTestSet$y)
    
    plot(m, forecast)
     prophet_plot_components(m, forecast)
    
    
    
    
  ## code for testing performance.
    acc <- accuracy(forecast$yhat, TmpTestSet$y
             )
    acc2 <- as.data.frame(acc)
    acc2$product <- product
    acc2$country <- countryvar  # colnames(acc2)[1] <- "X"

    #write.csv(acc2, file="results.csv", sep=",", row.names=FALSE)
    write.csv(rbind(acc2,read.csv("results.csv")), file="results.csv"#, sep=","#, append=TRUE#, quote=TRUE
              , row.names=FALSE )
    # #1926204942
    
    futuretest <-  TestSet %>% filter(cluster == product) %>% filter(as.character(country) == countryvar)
    TestFuture <- forecast <- predict(m, futuretest %>% rename(ds = date), periods =  24
    )
    TestFuture$brand <- product 
    TestFuture$country <- countryvar

    AllData <- 
    rbind(
      AllData,TestFuture %>% select(ds, brand, country, yhat_upper, yhat,  yhat_lower
                                    # , -yearly_upper, 
                                    # -yearly_lower, -additive_terms_upper
      ) %>% arrange(ds)
    )

  }

}



#AllData <- TestFuture %>% select(ds, brand, country, yhat_upper, yhat,  yhat_lower) %>% filter(0==1)       
# write.csv(AllData %>% arrange(ds, brand, country)  %>% mutate(trend_lower = case_when(trend_lower < 0 ~ 0 ,
#                                                       TRUE ~ trend_lower))   %>% 
#                                                     mutate(trend = case_when(trend < 0 ~ 0 ,
#                                                              TRUE ~ trend) )
          
          
          # , file="Submiss.csv"#, sep=","#, append=TRUE#, quote=TRUE
          # , row.names=FALSE )



SubTempl <- read.csv("./info/submit-checkpoint-template_csv.csv")
SubTempl$roww <-  1:nrow(SubTempl) 

#2507+142 =2649

AllData$date <- as.character(AllData$ds)


AboutToSubmit <- 
merge(SubTempl, AllData %>% arrange(ds, brand, country)  %>% mutate(yhat_lower = case_when(yhat_lower < 0 ~ 0 ,
                                                                                           TRUE ~ yhat_lower))   %>% 
        mutate(yhat = case_when(yhat < 0 ~ 0 ,
                                 TRUE ~ yhat) )  %>% 
        mutate(yhat_upper = case_when(yhat_upper < 0 ~ 0 ,
                                TRUE ~ yhat_upper) )
      
      , by.x=c("date", "cluster","country"), by.y=c("date", "brand","country"))
#2559+90

write.csv(

  AboutToSubmit %>% arrange(roww) %>% select(country, cluster, date, yhat_upper, yhat, yhat_lower) %>%
  rename(upper_bound = yhat_upper, forecast = yhat, lower_bound = yhat_lower) 

  , "submitTeam42.csv",    row.names=FALSE, quote=FALSE
)

# TotX <- rbind(XDF,X_valDF)
# TotX2 <-  TotX %>% subset(customer %in% YDF$customer) 


# write.csv2(TotX2# %>% subset(customer %in% YDF$customer)
#            ,"./datasets/X2.csv")

# TotX2$Posix <-  as.POSIXct(TotX2$timestamp, "%Y/%m/%d %H:%M")  #:%S
# TotX2
# 
# TotX2$Date <- date(TotX2$Posix)
# 
# TotX2 %>% group_by
# 
# TotX2$quantity <- as.numeric(TotX2$quantity)
# 
# TotX2$billing <- as.numeric(TotX2$billing)

# 
# products 3 & 45 
# products 30 & 56





papitu <- "AC"
colnames(SubMettAgg1DFDay) 

SubMettAgg1DFDay %>% select(date, volume, cluster, country, months_from_launch_date)

#### prophet way! ####
ProfetDF <- TempDFDaily

ProfetDF %>% arrange(ds)
ProfetDF$X <- NULL
colnames(ProfetDF)[1] <- "ds"
colnames(ProfetDF)[3] <- "y"
colnames(ProfetDF)[6] <- "upper_window"
ProfetDF$holiday <- "Papitu"
ProfetDF$lower_window <- 0


m <- prophet(
  rofetDF %>% dplyr::select(y,ds)
             , holidays = ProfetDF %>% filter(upper_window == 1) %>% select(ds,holiday, lower_window,upper_window)
             #, origin="2007-01-01"
)






future <- make_future_dataframe(m, periods = 300)

forecast <- predict(m, future)

plot(m, forecast)
prophet_plot_components(m, forecast)
