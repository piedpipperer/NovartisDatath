
library(prophet)
library(forecast)
#install.packages("prophet")

#### Load Libraries ####
library(pacman)
library(dplyr)
library(lubridate)
library(tidyr)
# pacman::p_load(readr, caret, plotly, ggplot2, 
#                labeling, promises, ggridges, 
#                doParallel,  e1071, mlbench,# inum,
#                corrplot#, ggpubr
#                , rpart, rpart.plot, gbm
#                , boot, dplyr,
#                reshape, Rserve#, tidyvers
#                , padr)

)


#'callr', 'pkgbuild', 'pkgload', 'rcmdcheck', 'usethis'

# 
# devtools::install_github("tidyverts/tsibble")
# 7


setwd("D:/dropbox/Dropbox/ubiqum/25. Novartis dataton/NovartisDatath/")
# same directory as R file.

TestSet <- read.csv("./info/datathon2019_test_final_csv.csv") %>%
  mutate(expenses = case_when(is.na(expenses) ~ 0, 
                              TRUE ~ expenses)) 

SubMettAgg1DFDay <- read.csv("./info/jordisight.csv" #"./info/datathon2019_train.csv"
                             ) %>% filter(cluster != "Product_15") #%>% 
#  filter(cluster != "Product_31" & country != "Country_f")
#%>% #filter(as.character(country) == countryvar) %>% 
#  filter(as.character(cluster) == "Product_31")

  # write.csv2(SubMettAgg1DFDay,"./info/train_tableau2.csv")
# write.csv2(TestSet,"./info/test_tableau.csv")


#summary(TestSet)
TestSet$date <-  as.POSIXct(TestSet$date, "%Y-%m-%d") 
SubMettAgg1DFDay$date <-  as.POSIXct(SubMettAgg1DFDay$date, "%Y-%m-%d") 
attr(SubMettAgg1DFDay$date, "tzone")  <- "Europe/Paris"
attr(TestSet$date, "tzone")  <- "Europe/Paris"

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


library(doParallel)
#### Enable parallel processing ####
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl))


for (countryvar in   as.vector((TestSet  %>% select(country) %>% arrange(desc(country)) %>% unique())$country)  # c("Country_h","Country_i","Country_j") #c("Country_h","Country_i","Country_j")
     ) {  # countryvar = "Country_a"
  TmpDfLoop <- TestSet  %>% filter(as.character(country) == countryvar)
  TmpDf <- Initialdf  %>% filter(as.character(country) == countryvar) 
  
  for (product in as.vector((TmpDfLoop %>% select(cluster) %>% arrange(desc(cluster)) %>% unique())$cluster)
  ){  # product = "Product_3"
         TmpDf2 <- TmpDf %>% filter(cluster == product) %>% rename(y = volume, ds= date)
  
    
         
    futuretest <-  TestSet %>% filter(cluster == product) %>% filter(as.character(country) == countryvar)
         
    if (!is.na(as.numeric(TmpDf2 %>% summarize(expenses = sum(expenses))))
    ){
      if ((as.numeric(TmpDf2 %>% summarize(expenses = sum(expenses))) < -100000000 ) &
           !(product  == "Product6")
      ){  #should include Prouduct_6 , Country_i
        m <- prophet(seasonality_mode='multiplicative', growth='logistic'#,
                     #, mcmc.samples = 6
                     #,changepoint_prior_scale=0.5
        )
         TmpDf2$floor <- 0
         TmpDf2$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*(3/4)) 
        
        m <- add_regressor(m, 'expenses' 
                           , mode='additive' #seasonality_mode='multiplicative'
                           )
        

        futuretest$floor <- 0
        futuretest$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*(3/4))
        # 
        
      }
      else 
      {
        m <- prophet(seasonality_mode='multiplicative', growth='logistic'
                    # , mcmc.samples =  6
                     #,changepoint_prior_scale=0.5
        )
        TmpDf2$floor <- 0
        TmpDf2$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*(5/3)) 
        futuretest$floor <- 0
        futuretest$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*(5/3))
        
      }
    }
         else 
         {
           m <- prophet(seasonality_mode='multiplicative', growth='logistic'
                       # , mcmc.samples =  6
                        #,changepoint_prior_scale=0.5
           )
           TmpDf2$floor <- 0
           TmpDf2$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*(5/3))
           futuretest$floor <- 0
           futuretest$cap  <- as.numeric(TmpDf2 %>% summarize(y = max(y))*(5/3))
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
    
    
    
    
    
    #recording data.
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





# SubTempl <- read.csv("./info/submit-checkpoint-template_csv.csv")
# SubTempl$roww <-  1:nrow(SubTempl) 

#2507+142 =2649


#AllData %>% filter(brand == "Product_1" & country == "Country_a") 



AllData$date <- as.character(AllData$ds)
AboutToSubmit <- 
merge(SubTempl, AllData %>% arrange(ds, brand, country)  %>% mutate(yhat_lower = case_when(yhat_lower < 0 ~ 0 ,
                                                                                           TRUE ~ yhat_lower))   %>% 
        mutate(yhat = case_when(yhat < 0 ~ 0 ,
                                 TRUE ~ yhat) )  %>% 
        mutate(yhat_upper = case_when(yhat_upper < 0 ~ 0 ,
                                TRUE ~ yhat_upper) )
      
      , by.x=c("date", "cluster","country"), by.y=c("date", "brand","country")) %>% unique()
#2559+90
#2567+90


write.csv(
  AboutToSubmit %>% arrange(roww) %>% select(country, cluster, date, yhat_upper, yhat, yhat_lower) %>%
    rename(upper_bound = yhat_upper, forecast = yhat, lower_bound = yhat_lower)    
  , "submitTeam_v2.csv",    row.names=FALSE, quote=FALSE
)

#AboutToSubmit[(AboutToSubmit$yhat > AboutToSubmit$yhat_upper),]
#AboutToSubmit[(AboutToSubmit$yhat < AboutToSubmit$yhat_lower),]$yhat_lower <- (AboutToSubmit[(AboutToSubmit$yhat < AboutToSubmit$yhat_lower),]$yhat) / 4
# TotX <- rbind(XDF,X_valDF)



# 
# SubFinTempl <- read.csv("./info/submit-final-template_csv.csv")
# SubFinTempl$roww <-  1:nrow(SubFinTempl) 


AboutToSubmitFin <- 
  merge(SubFinTempl, AllData %>% arrange(ds, brand, country)  %>% mutate(yhat_lower = case_when(yhat_lower < 0 ~ 0 ,
                                                                                             TRUE ~ yhat_lower))   %>% 
          mutate(yhat = case_when(yhat < 0 ~ 0 ,
                                  TRUE ~ yhat) )  %>% 
          mutate(yhat_upper = case_when(yhat_upper < 0 ~ 0 ,
                                        TRUE ~ yhat_upper) )
        
        , by.x=c("date", "cluster","country"), by.y=c("date", "brand","country"))
#2559+90

write.csv(
  
  AboutToSubmitFin %>% arrange(roww) %>% select(country, cluster, date, yhat_upper, yhat, yhat_lower) %>%
    rename(upper_bound = yhat_upper, forecast = yhat, lower_bound = yhat_lower)    
  , "submitTeamFinal_v3.csv",    row.names=FALSE, quote=FALSE
)







# write.csv2(
#   
#   AboutToSubmit %>% arrange(roww) %>% #select(country, cluster, date, yhat_upper, yhat, yhat_lower) %>%
#     rename(upper_bound = yhat_upper, volume = yhat, lower_bound = yhat_lower)   
#   , "csv2_submitTeam42_v2.csv",    row.names=FALSE, quote=FALSE
# )









