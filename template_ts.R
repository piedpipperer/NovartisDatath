
library(prophet)

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

setwd("D:/dropbox/Dropbox/ubiqum/25. Novartis dataton")

SubMettAgg1DFDay <- read.csv("./WeeklyTS.csv")




# TotX <- rbind(XDF,X_valDF)
# TotX2 <-  TotX %>% subset(customer %in% YDF$customer) 


# write.csv2(TotX2# %>% subset(customer %in% YDF$customer)
#            ,"./datasets/X2.csv")

TotX2$Posix <-  as.POSIXct(TotX2$timestamp, "%Y/%m/%d %H:%M")  #:%S
TotX2

TotX2$Date <- date(TotX2$Posix)

TotX2 %>% group_by

TotX2$quantity <- as.numeric(TotX2$quantity)

TotX2$billing <- as.numeric(TotX2$billing)




papitu <- "AC"
TempDFDaily <- SubMettAgg1DFDay %>%  filter( #week <= MyFreq3  &
  SubMet_Part %in% papitu &
    year %in% c(2007,
                2008,2009, 2010) )    %>% 
  mutate(IsHoliday = case_when(SubMet_Energy < 35000 ~ 1, 
                               TRUE ~ 0) )

#### prophet way! ####
ProfetDF <- TempDFDaily

ProfetDF %>% arrange(ds)
ProfetDF$X <- NULL
colnames(ProfetDF)[1] <- "ds"
colnames(ProfetDF)[3] <- "y"
colnames(ProfetDF)[6] <- "upper_window"
ProfetDF$holiday <- "Papitu"
ProfetDF$lower_window <- 0


m <- prophet(ProfetDF %>% dplyr::select(y,ds)
             , holidays = ProfetDF %>% filter(upper_window == 1) %>% select(ds,holiday, lower_window,upper_window)
             #, origin="2007-01-01"
)






future <- make_future_dataframe(m, periods = 300)

forecast <- predict(m, future)

plot(m, forecast)
prophet_plot_components(m, forecast)
