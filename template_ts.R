




#### Load Libraries ####
library(prophet)
library(forecast)
#install.packages("prophet")
library(dplyr)
library(lubridate)
library(tidyr)

#library(pacman)
# pacman::p_load(readr, caret, plotly, ggplot2, 
#                labeling, promises, ggridges, 
#                doParallel,  e1071, mlbench,# inum,
#                corrplot#, ggpubr
#                , rpart, rpart.plot, gbm
#                , boot, dplyr,
#                reshape, Rserve#, tidyvers
#                , padr)



#### Load Data and Preproc ####
# same directory as R file, setting up readiness for csv files.
setwd("D:/dropbox/Dropbox/ubiqum/25. Novartis dataton/NovartisDatath/")


TestSet <- read.csv("./info/datathon2019_test_final_csv.csv") %>%
  mutate(expenses = case_when(is.na(expenses) ~ 0,    #put Na expenses into 0.
                              TRUE ~ expenses)) 

SubMettAgg1DFDay <- read.csv("./info/jordisight.csv" #"./info/datathon2019_train.csv"
                             ) %>% filter(cluster != "Product_15") %>%
                                    select(-year,month,month_in_quarter)  #useless columns for TS modeling
#  filter(cluster != "Product_31" & country != "Country_f")
#%>% #filter(as.character(country) == countryvar) %>% 
#  filter(as.character(cluster) == "Product_31")

# write.csv2(SubMettAgg1DFDay,"./info/train_tableau2.csv")
# write.csv2(TestSet,"./info/test_tableau.csv")
# One execution code, of writing csvs for Tableau loading format. 


#Transforming into Date formats of both test/train sets
TestSet$date <-  as.POSIXct(TestSet$date, "%Y-%m-%d") 
SubMettAgg1DFDay$date <-  as.POSIXct(SubMettAgg1DFDay$date, "%Y-%m-%d") 
attr(SubMettAgg1DFDay$date, "tzone")  <- "Europe/Paris"
attr(TestSet$date, "tzone")  <- "Europe/Paris"


# Finding out about Nulls (expenses was finaly the only one to be modified)
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



#### Enable parallel processing #### 
#This is do to the fact that  loop of all models took sometime.
library(doParallel)
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl))

#### Modelling ####

# Df finaly employed for modeling and so on.
Initialdf <- SubMettAgg1DFDay %>% select(date, cluster, country, volume, expenses, net_price) %>%
  mutate(expenses = case_when(is.na(expenses) ~ 0, 
                              TRUE ~ expenses)) 

#setting up preductions Df as a empty dataframe:
AllData <- Initialdf %>% select(date, cluster, country
                                 ,  volume  ) %>% filter(0==1) %>% 
                      rename(ds = date
                             ,brand = cluster
                             ,yhat = volume) 




# The following code corresponds to 2 loops that both trains and also runs predictions
#loop over countries 
for (countryvar in   as.vector((TestSet  %>% select(country) %>% arrange(desc(country)) %>% unique())$country)  # c("Country_h","Country_i","Country_j") #c("Country_h","Country_i","Country_j")
     ) {  # countryvar = "Country_a"
  
  #for running predictions:
  TmpDfLoop <- TestSet  %>% filter(as.character(country) == countryvar)
  
  #for modeling porpuses:
  TmpDf <- Initialdf  %>% filter(as.character(country) == countryvar) 
  
#loop over Products
  for (product in as.vector((TmpDfLoop %>% select(cluster) %>% arrange(desc(cluster)) %>% unique())$cluster)
  ){  # product = "Product_3"
      TmpDf2 <- TmpDf %>% filter(cluster == product) %>% rename(y = volume, ds= date)

         
      futuretest <-  TestSet %>% filter(cluster == product) %>% filter(as.character(country) == countryvar)
         
      if (!is.na(as.numeric(TmpDf2 %>% summarize(expenses = sum(expenses))))
      ){ #filter NA expenses (already removed)
        
        if ((as.numeric(TmpDf2 %>% summarize(expenses = sum(expenses))) < -100000000 ) &
             !(product  == "Product6")
        ){  #Include casuistics of products that will add Expenses as regressor
          
            m <- prophet(seasonality_mode='multiplicative', growth='logistic'#,
                         #, mcmc.samples = 6
                         #,changepoint_prior_scale=0.5        
            )
            
            LogMaximum = (3/4)
            TmpDf2$floor <- 0
            TmpDf2$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*LogMaximum) 
            
            m <- add_regressor(m, 'expenses' 
                               , mode='additive' #seasonality_mode='multiplicative'
                               )
            
    
            futuretest$floor <- 0
            futuretest$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*LogMaximum)
            # 
          
          }
          else 
          {
          m <- prophet(seasonality_mode='multiplicative', growth='logistic'
                      # , mcmc.samples =  6
                       #,changepoint_prior_scale=0.5
          )
          LogMaximum = (5/3)
          TmpDf2$floor <- 0
          TmpDf2$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*LogMaximum) 
          futuretest$floor <- 0
          futuretest$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*LogMaximum)
          
        }
     }
     else 
     {
       LogMaximum = (5/3)
       m <- prophet(seasonality_mode='multiplicative', growth='logistic'
                   # , mcmc.samples =  6
                    #,changepoint_prior_scale=0.5
       )
       TmpDf2$floor <- 0
       TmpDf2$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*LogMaximum)
       futuretest$floor <- 0
       futuretest$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*LogMaximum)
       # 

      }
         
         #summary(TmpDf2)
         # TmpDf2$floor <- 0
         # TmpDf2$cap  <- 2000000
         train_size = (as.numeric(TmpDf2 %>% count()+1) ) #* 0.76
         TmpTrainSet <- TmpDf2[1:train_size,] %>% filter(!is.na(country)) # %>% arrange(ds)
         TmpTestSet <- TmpDf2[train_size:as.numeric(TmpDf2 %>% count()+1),] # %>% arrange(ds)
         
         
      #m <- add_regressor(m, 'net_price')    
    m <- fit.prophet(m,#yearly.seasonality=FALSE,
                     TmpTrainSet #%>% dplyr::select(y,ds,expenses, net_price)  #y,ds
    )


    # submitting results
    TestFuture <- predict(m, futuretest %>% rename(ds = date), periods =  24
    )
    TestFuture$brand <- product
    TestFuture$country <- countryvar

    # plot(m, TestFuture) + add_changepoints_to_plot(m)
    #  prophet_plot_components(m, forecast)
    

    #Recording Predictions
    AllData <-
    rbind(
      AllData,TestFuture %>% select(ds, brand, country, yhat_upper, yhat,  yhat_lower
                                    # , -yearly_upper,
                                    # -yearly_lower, -additive_terms_upper
      ) %>% arrange(ds)
    )

    
  }#END PRODUCT LOOP
}#END COUNTRY LOOP


#### Writing Predictions File ####

# Template files for TEsting submission:
 SubTempl <- read.csv("./info/submit-checkpoint-template_csv.csv")
 SubTempl$roww <-  1:nrow(SubTempl) 


#AllData %>% filter(brand == "Product_1" & country == "Country_a") 


AllData$date <- as.character(AllData$ds)  #format for writing in Submis. file.
AboutToSubmit <- 
merge(SubTempl, AllData %>% arrange(ds, brand, country)  %>% mutate(yhat_lower = case_when(yhat_lower < 0 ~ 0 ,
                                                                                           TRUE ~ yhat_lower))   %>% 
        mutate(yhat = case_when(yhat < 0 ~ 0 ,
                                 TRUE ~ yhat) )  %>% 
        mutate(yhat_upper = case_when(yhat_upper < 0 ~ 0 ,
                                TRUE ~ yhat_upper) )
      
      , by.x=c("date", "cluster","country"), by.y=c("date", "brand","country")) %>% unique()
#2559+90 checking num. records are correct
#2567+90

#2507+142 =2649  #checking number of rows are correct.



write.csv(
  AboutToSubmit %>% arrange(roww) %>% select(country, cluster, date, yhat_upper, yhat, yhat_lower) %>%
    rename(upper_bound = yhat_upper, forecast = yhat, lower_bound = yhat_lower)    
  , "submitTeam_v2.csv",    row.names=FALSE, quote=FALSE
)

#AboutToSubmit[(AboutToSubmit$yhat > AboutToSubmit$yhat_upper),]
#AboutToSubmit[(AboutToSubmit$yhat < AboutToSubmit$yhat_lower),]$yhat_lower <- (AboutToSubmit[(AboutToSubmit$yhat < AboutToSubmit$yhat_lower),]$yhat) / 4
# TotX <- rbind(XDF,X_valDF)



# Template files for final submission:
 SubFinTempl <- read.csv("./info/submit-final-template_csv.csv")
 SubFinTempl$roww <-  1:nrow(SubFinTempl) 


AboutToSubmitFin <- 
  merge(SubFinTempl, AllData %>% arrange(ds, brand, country)  %>% mutate(yhat_lower = case_when(yhat_lower < 0 ~ 0 ,
                                                                                             TRUE ~ yhat_lower))   %>% 
          mutate(yhat = case_when(yhat < 0 ~ 0 ,
                                  TRUE ~ yhat) )  %>% 
          mutate(yhat_upper = case_when(yhat_upper < 0 ~ 0 ,
                                        TRUE ~ yhat_upper) )
        
        , by.x=c("date", "cluster","country"), by.y=c("date", "brand","country"))
#2559+90

#writing submission file to upload to intranet
write.csv(
  AboutToSubmitFin %>% arrange(roww) %>% select(country, cluster, date, yhat_upper, yhat, yhat_lower) %>%
    rename(upper_bound = yhat_upper, forecast = yhat, lower_bound = yhat_lower)    
  , "submitTeamFinal_v3.csv",    row.names=FALSE, quote=FALSE
)







# tableau write for watching predictions on graph
# write.csv2(
#   
#   AboutToSubmit %>% arrange(roww) %>% #select(country, cluster, date, yhat_upper, yhat, yhat_lower) %>%
#     rename(upper_bound = yhat_upper, volume = yhat, lower_bound = yhat_lower)   
#   , "csv2_submitTeam42_v2.csv",    row.names=FALSE, quote=FALSE
# )









