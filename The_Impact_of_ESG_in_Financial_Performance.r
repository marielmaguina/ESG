# Preliminaries --------------------------------------------------------------------------------------------------------------------------------------

library(psych)
library(stargazer)
library(readxl)
library(tidyverse)
library(reshape)
library(data.table)
library(gtsummary)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(plm)
library(rpart)
library(rpart.plot)
library(zoo)

ESGdf <- read.csv("/Users/alexdasilva/Downloads/ESG_clean") %>%
          select(Identifier, GICS.Sector.Name, Year, ESG, E, S, G, AT, TA, LEV, AG, TQ, ROA, ROE) %>% 
          mutate(TA = TA/1e9)
names(ESGdf) <- c("Ticker", "Sector", "Year", "ESG", "E", "S", "G", "AT", "TA", "Lev", "AG", "TQ", "ROA", "ROE")


#Normalization -----------------------------------------------------------------------------------------------------------------------------------------

ESGdf_Norm <- ESGdf %>% group_by(Year) %>% 
    mutate(ESG_meanY = mean(ESG)) %>%
    mutate(E_meanY = mean(E)) %>% 
    mutate(S_meanY = mean(S)) %>% 
    mutate(G_meanY = mean(G)) %>%
    mutate(ESG_normY = ESG - ESG_meanY) %>%
    mutate(E_normY = E - E_meanY) %>%
    mutate(S_normY = S - S_meanY) %>%
    mutate(G_normY = G - G_meanY) %>%
    ungroup() %>% 
    group_by(Year, Sector) %>%
    mutate(ESG_meanYS = mean(ESG)) %>% 
    mutate(E_meanYS = mean(E)) %>% 
    mutate(S_meanYS = mean(S)) %>% 
    mutate(G_meanYS = mean(G)) %>%
    mutate(ESG_normYS = ESG - ESG_meanYS)  %>%
    mutate(E_normYS = E - E_meanYS) %>%
    mutate(S_normYS = S - S_meanYS) %>%
    mutate(G_normYS = G - G_meanYS)
                                              
#Notes:
#The suffix Y indicates that the column reflects the global mean per year. The suffix normY indicates that the column
#has been normalized by the global mean per year. The suffix YS indicates the same but for mean per year and sector.


#Data visualization-----------------------------------------------------------------------------------------------------

#1. Graph for the evolution of ESG scores normalized by the sector mean
ggplot(ESGdf_Norm, aes(x=Year, y=ESG_normYS, color = Sector, groupby = Ticker)) + geom_line() + facet_wrap(~Sector)

#2. Graph for the evolution of ESG scores normalized by the global mean
Ygraph_data = ESGdf_Norm %>% group_by(Year, Sector) %>% summarise(Average_ESG_Score = mean(ESG))
ggplot(Ygraph_data, aes(x=Year)) + geom_line(aes(y=Avg, color=Sector)) + 
  labs(title="Evolution of ESG Scores Standard Deviation", x="Year", y = "") + theme(plot.title = element_text(hjust = 0.5)) 
ggplot(Ygraph_data, aes(x=Year)) + geom_line(aes(y=Average_ESG_Score, color=Sector)) 

#Panel Regressions with all the data  -----------------------------------------------------------------------------------------------

pdata <- pdata.frame(ESGdf_Norm, index=c("Ticker", "Year"))

#1. Panel regression with data normalized by year and sector

#Regressions with ESG, E, S, G
preg_TQ_ESGESG_YS <- plm(TQ ~ ESG_normYS+E_normYS+S_normYS+G_normYS+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESGESG_YS <- plm(ROA ~ ESG_normYS+E_normYS+S_normYS+G_normYS+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESGESG_YS <- plm(ROE ~ ESG_normYS+E_normYS+S_normYS+G_normYS+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESGESG_YS, preg_ROA_ESGESG_YS, preg_ROE_ESGESG_YS, type = "text",
          title = "Panel Regression - ESG,E,S,G, and controls - Normalized by Sector and Year",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with ESG only
preg_TQ_ESG_YS <- plm(TQ ~ ESG_normYS+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_YS <- plm(ROA ~ ESG_normYS+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_YS <- plm(ROE ~ ESG_normYS+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_YS, preg_ROA_ESG_YS, preg_ROE_ESG_YS, type = "text",
          title = "Panel Regression - ESG and controls - Normalized by Sector and Year",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with E,S,G only
preg_TQ_ESG_sep_YS <- plm(TQ ~ E_normYS+S_normYS+G_normYS+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_sep_YS <- plm(ROA ~ E_normYS+S_normYS+G_normYS+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_sep_YS <- plm(ROE ~ E_normYS+S_normYS+G_normYS+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_sep_YS, preg_ROA_ESG_sep_YS, preg_ROE_ESG_sep_YS, type = "text",
          title = "Panel Regression - E,S,G, and controls - Normalized by Sector and Year",
          align = TRUE, dep.var.labels.include = TRUE)


#2. Panel regressions with data normalized by year (the global mean)

#Regressions with ESG, E, S, G
preg_TQ_ESGESG_Y <- plm(TQ ~ ESG_normY+E_normY+S_normY+G_normY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESGESG_Y <- plm(ROA ~ ESG_normY+E_normY+S_normY+G_normY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESGESG_Y <- plm(ROE ~ ESG_normY+E_normY+S_normY+G_normY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESGESG_Y, preg_ROA_ESGESG_Y, preg_ROE_ESGESG_Y, type = "text",
          title = "Panel Regression - ESG,E,S,G, and controls - Normalized by Year",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with ESG only
preg_TQ_ESG_Y <- plm(TQ ~ ESG_normY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_Y <- plm(ROA ~ ESG_normY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_Y <- plm(ROE ~ ESG_normY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_Y, preg_ROA_ESG_Y, preg_ROE_ESG_Y, type = "text",
          title = "Panel Regression - ESG and controls - Normalized by Year",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with E,S,G only
preg_TQ_ESG_sep_Y <- plm(TQ ~ E_normY+S_normY+G_normY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_sep_Y <- plm(ROA ~ E_normY+S_normY+G_normY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_sep_Y <- plm(ROE ~ E_normY+S_normY+G_normY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_sep_Y, preg_ROA_ESG_sep_Y, preg_ROE_ESG_sep_Y, type = "text",
          title = "Panel Regression - E,S,G, and controls - Normalized by Year",
          align = TRUE, dep.var.labels.include = TRUE)


#Worse and best average ESG scores per industry -----------------------------------------------------------------------------------

#1. Find the ranking
industries_byESG <- ESGdf_Norm %>% group_by(Sector) %>% summarize(mean_ESG=mean(ESG)) %>% arrange(mean_ESG)

print(industries_byESG)

#The best three are Consumer Staples, Utilities, Materials
#The worse three are Financials, Communication Services, Consumer Discretionary

#2. Panel Regressions for the best three 

best_Norm <- ESGdf %>% filter(Sector == "Consumer Staples" | Sector == "Utilities"  | Sector == "Materials") %>%
    select(Ticker, Sector, Year, ESG, E, S, G, AT, TA, Lev, AG, TQ, ROA, ROE) %>%
    group_by(Year) %>% 
    mutate(ESG_meanY = mean(ESG)) %>%
    mutate(E_meanY = mean(E)) %>% 
    mutate(S_meanY = mean(S)) %>% 
    mutate(G_meanY = mean(G)) %>%
    mutate(TQ_mean=mean(TQ)) %>%
    mutate(ESG_normPY = (ESG - ESG_meanY)/ESG_meanY) %>%
    mutate(E_normPY = (E - E_meanY)/E_meanY) %>%
    mutate(S_normPY = (S - S_meanY)/S_meanY) %>%
    mutate(G_normPY = (G - G_meanY)/G_meanY) %>%
    mutate(TQnorm = (TQ-TQ_mean)/TQ_mean) %>%
    ungroup()

pdata_best <- pdata.frame(best_Norm, index=c("Ticker", "Year"))

#Regressions with ESG, E, S, G 
preg_TQ_ESGESG_Y_best <- plm(TQnorm ~ ESG_normPY+E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_best, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESGESG_Y_best <- plm(ROA ~ ESG_normPY+E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_best, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESGESG_Y_best <- plm(ROE ~ ESG_normPY+E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_best, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESGESG_Y_best, preg_ROA_ESGESG_Y_best, preg_ROE_ESGESG_Y_best, type = "text",
          title = "Panel Regression - ESG,E,S,G, and controls - Best Three Sectors",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with ESG only
preg_TQ_ESG_Y_best <- plm(TQnorm ~ ESG_normPY+AT+TA+Lev+AG, data = pdata_best, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_Y_best <- plm(ROA ~ ESG_normPY+AT+TA+Lev+AG, data = pdata_best, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_Y_best <- plm(ROE ~ ESG_normPY+AT+TA+Lev+AG, data = pdata_best, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_Y_best, preg_ROA_ESG_Y_best, preg_ROE_ESG_Y_best, type = "text",
          title = "Panel Regression - ESG and controls - Best Three Sectors",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with E,S,G only
preg_TQ_ESG_sep_Y_best <- plm(TQnorm ~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_best, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_sep_Y_best <- plm(ROA ~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_best, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_sep_Y_best <- plm(ROE ~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_best, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_sep_Y_best, preg_ROA_ESG_sep_Y_best, preg_ROE_ESG_sep_Y_best, type = "text",
          title = "Panel Regression - E,S,G, and controls - Best Three Sectors",
          align = TRUE, dep.var.labels.include = TRUE)

#3. Panel Regressions for the worst three

worst_Norm <- ESGdf %>% filter(Sector == "Financials" | Sector == "Communication Services"  | Sector == "Consumer Discretionary") %>%
  select(Ticker, Sector, Year, ESG, E, S, G, AT, TA, Lev, AG, TQ, ROA, ROE) %>%
  group_by(Year) %>% 
  mutate(ESG_meanY = mean(ESG)) %>%
  mutate(E_meanY = mean(E)) %>% 
  mutate(S_meanY = mean(S)) %>% 
  mutate(G_meanY = mean(G)) %>%
  mutate(TQ_mean=mean(TQ)) %>%
  mutate(ESG_normPY = (ESG - ESG_meanY)/ESG_meanY) %>%
  mutate(E_normPY = (E - E_meanY)/E_meanY) %>%
  mutate(S_normPY = (S - S_meanY)/S_meanY) %>%
  mutate(G_normPY = (G - G_meanY)/G_meanY) %>%
  mutate(TQnorm = (TQ-TQ_mean)/TQ_mean) %>%
  ungroup()

pdata_worst <- pdata.frame(worst_Norm, index=c("Ticker", "Year"))

#Regressions with ESG, E, S, G 
preg_TQ_ESGESG_Y_worst <- plm(TQnorm ~ ESG_normPY+E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_worst, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESGESG_Y_worst <- plm(ROA ~ ESG_normPY+E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_worst, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESGESG_Y_worst <- plm(ROE ~ ESG_normPY+E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_worst, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESGESG_Y_worst, preg_ROA_ESGESG_Y_worst, preg_ROE_ESGESG_Y_worst, type = "text",
          title = "Panel Regression - ESG,E,S,G, and controls - Worst Three Sectors",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with ESG only
preg_TQ_ESG_Y_worst <- plm(TQnorm ~ ESG_normPY+AT+TA+Lev+AG, data = pdata_worst, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_Y_worst <- plm(ROA ~ ESG_normPY+AT+TA+Lev+AG, data = pdata_worst, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_Y_worst <- plm(ROE ~ ESG_normPY+AT+TA+Lev+AG, data = pdata_worst, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_Y_worst, preg_ROA_ESG_Y_worst, preg_ROE_ESG_Y_worst, type = "text",
          title = "Panel Regression - ESG and controls - Worst Three Sector",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with E,S,G only
preg_TQ_ESG_sep_Y_worst <- plm(TQnorm ~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_worst, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_sep_Y_worst <- plm(ROA ~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_worst, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_sep_Y_worst <- plm(ROE ~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_worst, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_sep_Y_worst, preg_ROA_ESG_sep_Y_worst, preg_ROE_ESG_sep_Y_worst, type = "text",
          title = "Panel Regression - E,S,G, and controls - Worst Three Sectors",
          align = TRUE, dep.var.labels.include = TRUE)

#Big and Small ----------------------------------------------------------------------------------------------------------------------------------

#1. Big TA

Big_TA <- ESGdf %>% filter(TA >= mean(TA))  %>%
  group_by(Year) %>% 
  mutate(ESG_meanY = mean(ESG)) %>%
  mutate(E_meanY = mean(E)) %>% 
  mutate(S_meanY = mean(S)) %>% 
  mutate(G_meanY = mean(G)) %>%
  mutate(TQ_mean=mean(TQ)) %>%
  mutate(ESG_normPY = (ESG - ESG_meanY)/ESG_meanY) %>%
  mutate(E_normPY = (E - E_meanY)/E_meanY) %>%
  mutate(S_normPY = (S - S_meanY)/S_meanY) %>%
  mutate(G_normPY = (G - G_meanY)/G_meanY) %>%
  mutate(TQnorm = (TQ-TQ_mean)/TQ_mean) %>%
  ungroup()

pdata_big <- pdata.frame(Big_TA, index=c("Ticker", "Year"))

#Regressions with ESG, E, S, G 
preg_TQ_ESGESG_Y_big <- plm(TQnorm ~ ESG_normPY+E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_big, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESGESG_Y_big <- plm(ROA ~ ESG_normPY+E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_big, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESGESG_Y_big <- plm(ROE ~ ESG_normPY+E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_big, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESGESG_Y_big, preg_ROA_ESGESG_Y_big, preg_ROE_ESGESG_Y_big, type = "text",
          title = "Panel Regression - ESG,E,S,G, and controls - Big TA",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with ESG only
preg_TQ_ESG_Y_big <- plm(TQnorm ~ ESG_normPY+AT+TA+Lev+AG, data = pdata_big, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_Y_big <- plm(ROA ~ ESG_normPY+AT+TA+Lev+AG, data = pdata_big, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_Y_big <- plm(ROE ~ ESG_normPY+AT+TA+Lev+AG, data = pdata_big, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_Y_big, preg_ROA_ESG_Y_big, preg_ROE_ESG_Y_big, type = "text",
          title = "Panel Regression - ESG and controls - Big TA",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with E,S,G only
preg_TQ_ESG_sep_Y_big <- plm(TQnorm ~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_big, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_sep_Y_big <- plm(ROA ~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_big, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_sep_Y_big <- plm(ROE ~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_big, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_sep_Y_big, preg_ROA_ESG_sep_Y_big, preg_ROE_ESG_sep_Y_big, type = "text",
          title = "Panel Regression - E,S,G, and controls - Big TA",
          align = TRUE, dep.var.labels.include = TRUE)

#2. Small TA
Small_TA <- ESGdf %>% filter(TA < mean(TA))  %>%
  group_by(Year) %>% 
  mutate(ESG_meanY = mean(ESG)) %>%
  mutate(E_meanY = mean(E)) %>% 
  mutate(S_meanY = mean(S)) %>% 
  mutate(G_meanY = mean(G)) %>%
  mutate(TQ_mean=mean(TQ)) %>%
  mutate(ESG_normPY = (ESG - ESG_meanY)/ESG_meanY) %>%
  mutate(E_normPY = (E - E_meanY)/E_meanY) %>%
  mutate(S_normPY = (S - S_meanY)/S_meanY) %>%
  mutate(G_normPY = (G - G_meanY)/G_meanY) %>%
  mutate(TQnorm = (TQ-TQ_mean)/TQ_mean) %>%
  ungroup()

pdata_small <- pdata.frame(Small_TA, index=c("Ticker", "Year"))

#Regressions with ESG, E, S, G 
preg_TQ_ESGESG_Y_small <- plm(TQ ~ ESG_normY+E_normY+S_normY+G_normY+AT+TA+Lev+AG, data = pdata_small, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESGESG_Y_small <- plm(ROA ~ ESG_normY+E_normY+S_normY+G_normY+AT+TA+Lev+AG, data = pdata_small, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESGESG_Y_small <- plm(ROE ~ ESG_normY+E_normY+S_normY+G_normY+AT+TA+Lev+AG, data = pdata_small, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESGESG_Y_small, preg_ROA_ESGESG_Y_small, preg_ROE_ESGESG_Y_small, type = "text",
          title = "Panel Regression - ESG,E,S,G, and controls - Small TA",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with ESG only
preg_TQ_ESG_Y_small <- plm(TQ ~ ESG_normY+AT+TA+Lev+AG, data = pdata_small, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_Y_small <- plm(ROA ~ ESG_normY+AT+TA+Lev+AG, data = pdata_small, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_Y_small <- plm(ROE ~ ESG_normY+AT+TA+Lev+AG, data = pdata_small, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_Y_small, preg_ROA_ESG_Y_small, preg_ROE_ESG_Y_small, type = "text",
          title = "Panel Regression - ESG and controls - Small TA",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with E,S,G only
preg_TQ_ESG_sep_Y_small <- plm(TQnorm ~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_small, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_sep_Y_small <- plm(ROA ~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_small, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_sep_Y_small <- plm(ROE ~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata_small, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_sep_Y_small, preg_ROA_ESG_sep_Y_small, preg_ROE_ESG_sep_Y_small, type = "text",
          title = "Panel Regression - E,S,G, and controls - Small TA",
          align = TRUE, dep.var.labels.include = TRUE)



#Other shit --------------------------------------------------------------------------------------------------------------------------------------

#This graphs show all the stocks ESG, E, S, G scores normalized by their industry year mean
#ggplot(ESGdf_Year_Sector, aes(x=Year, y=ESG_Normalized, color = Sector, groupby = Ticker)) + geom_line() + facet_wrap(~Sector)
#ggplot(ESGdf_Year_Sector, aes(x=Year, y=E_Normalized, color = Sector, groupby = Ticker)) + geom_line() + facet_wrap(~Sector)
#ggplot(ESGdf_Year_Sector, aes(x=Year, y=S_Normalized, color = Sector, groupby = Ticker)) + geom_line() + facet_wrap(~Sector)
#ggplot(ESGdf_Year_Sector, aes(x=Year, y=G_Normalized, color = Sector, groupby = Ticker)) + geom_line() + facet_wrap(~Sector)

#Research question: We want to know the impact of the ESG rating of a company relative to 
#their industry on firm performance. 

#Tree --------------------------------------------------------------------------------------------------------------------------------

pdata <- pdata.frame(ESGdf_NormP, index=c("Ticker", "Year"))

#Regressions with ESG, E, S, G 
tree_model <- rpart(TQnorm~ ESG+E+S+G+AT+TA+Lev+AG, data=ESGdf_NormP)
rpart.plot(tree_model, main = "TQ on ESG, E, S, G")

tree_model <- rpart(ROA~ ESG+E+S+G+AT+TA+Lev+AG, data=ESGdf_NormP)
rpart.plot(tree_model, main = "ROA on ESG, E, S, G")

tree_model <- rpart(ROE~ ESG+E+S+G+AT+TA+Lev+AG, data=ESGdf_NormP)
rpart.plot(tree_model, main = "ROE on ESG, E, S, G")

#Regressions with ESG only
tree_model <- rpart(TQnorm~ ESG_normPY+AT+TA+Lev+AG, data=ESGdf_NormP)
rpart.plot(tree_model, main = "TQ on ESG")

tree_model <- rpart(ROA~ ESG_normPY+AT+TA+Lev+AG, data=ESGdf_NormP)
rpart.plot(tree_model, main = "ROA on ESG")

tree_model <- rpart(ROE~ ESG_normPY+AT+TA+Lev+AG, data=ESGdf_NormP)
rpart.plot(tree_model, main = "ROE on ESG, E, S, G")

#Regressions with E, S, G only
tree_model <- rpart(TQnorm~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data=pdata)
rpart.plot(tree_model, main = "TQ on ESG, E, S, G")

tree_model <- rpart(ROA~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data=pdata)
rpart.plot(tree_model, main = "ROA on ESG, E, S, G")

tree_model <- rpart(ROE~ E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data=pdata)
rpart.plot(tree_model, main = "ROE on ESG, E, S, G")


  #ESG: The tree splits Total Assets at 1.273b for ROA, and 14.7b for TQ
  #ESG Normal: 
ESGdf_Year
#Not much to see here, other than confirming the importance of Total Assets


#Panel Regressions Normalized by Percent Deviations from the Global Mean --------------------------------------------------------------------------------------------------------------------------------------

ESGdf_NormP <- ESGdf %>% group_by(Year) %>% 
  mutate(ESG_meanY = mean(ESG)) %>%
  mutate(E_meanY = mean(E)) %>% 
  mutate(S_meanY = mean(S)) %>% 
  mutate(G_meanY = mean(G)) %>%
  mutate(TQ_mean=mean(TQ)) %>%
  mutate(ESG_normPY = (ESG - ESG_meanY)/ESG_meanY) %>%
  mutate(E_normPY = (E - E_meanY)/E_meanY) %>%
  mutate(S_normPY = (S - S_meanY)/S_meanY) %>%
  mutate(G_normPY = (G - G_meanY)/G_meanY) %>%
  mutate(TQnorm = (TQ-TQ_mean)/TQ_mean) %>%
  ungroup()

ESGdf_NormPS <- ESGdf_NormP %>% group_by(Year, Sector) %>%
  mutate(ESG_meanPYS = mean(ESG)) %>% 
  mutate(E_meanPYS = mean(E)) %>% 
  mutate(S_meanPYS = mean(S)) %>% 
  mutate(G_meanPYS = mean(G)) %>%
  mutate(ESG_normPYS = (ESG - ESG_meanPYS)/ESG_meanPYS)  %>%
  mutate(E_normYS = (E - E_meanPYS)/E_meanPYS) %>%
  mutate(S_normYS = (S - S_meanPYS)/S_meanPYS) %>%
  mutate(G_normYS = (G - G_meanPYS)/G_meanPYS) %>%
  ungroup()

#Data visualization-----------------------------------------------------------------------------------------------------

#1. Graph for the evolution of ESG scores normalized by the sector mean
ggplot(ESGdf_NormPS, aes(x=Year, y=ESG_normPYS, color = Sector, groupby = Ticker)) + geom_line() + facet_wrap(~Sector)

#2. Graph for the evolution of ESG scores normalized by the global mean
Ygraph_data = ESGdf_NormP %>% group_by(Year, Sector) %>% summarise(Avg = mean(TQnorm))
ggplot(Ygraph_data, aes(x=Year)) + geom_line(aes(y=Avg, color=Sector)) + 
  labs(title="Evolution of TQ Deviations", x="Year", y = "Mean Deviation") + theme(plot.title = element_text(hjust = 0.5,size=25,family="serif")) + theme(legend.position="none")

pdata <- pdata.frame(ESGdf_NormP, index=c("Ticker", "Year"))

#Regressions with ESG, E, S, G
preg_TQ_ESGESG_PY <- plm(TQnorm ~ ESG_normPY+E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESGESG_PY <- plm(ROA ~ ESG_normPY+E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESGESG_PY <- plm(ROE ~ ESG_normPY+E_normPY+S_normPY+G_normPY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESGESG_PY, preg_ROA_ESGESG_PY, preg_ROE_ESGESG_PY, type = "text",
          title = "Panel Regression - ESG,E,S,G, and controls - Normalized by Year",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with ESG only
preg_TQ_ESG_PY <- plm(TQnorm ~ ESG_normPY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_PY <- plm(ROA ~ ESG_normPY+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_PY <- plm(ROE ~ ESG_normPY+TA+AT+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_PY, preg_ROA_ESG_PY, preg_ROE_ESG_PY, type = "text",
          title = "Panel Regression - ESG and controls - Normalized by Year",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with E,S,G only
preg_TQ_ESG_sep_PY <- plm(TQnorm ~ E_normPY+S_normPY+G_normPY+TA+AT+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_sep_PY <- plm(ROA ~ E_normPY+S_normPY+G_normPY+TA+AT+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_sep_PY <- plm(ROE ~ E_normPY+S_normPY+G_normPY+TA+AT+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_sep_PY, preg_ROA_ESG_sep_PY, preg_ROE_ESG_sep_PY, type = "text",
          title = "Panel Regression - E,S,G, and controls - Normalized by Year",
          align = TRUE, dep.var.labels.include = TRUE)


#Dummy Variables for Size --------------------------------------------------------------------------------------------------------------------------------------

ESGdf_NormP <- ESGdf_NormP %>% group_by(Year) %>% 
  mutate(FirmSize = cut(TA, breaks = quantile(TA, probs = seq(0, 1, 0.25)),
                                      include.lowest = TRUE, labels = c("Smallest", "Small", "Big", "Biggest")))
  ungroup()

pdata <- pdata.frame(ESGdf_NormP, index=c("Ticker", "Year"))

#Regressions with ESG, E, S, G 
TQ_ESGESG_PY_FS <- plm(TQnorm ~ (ESG_normPY+E_normPY+S_normPY+G_normPY)*FirmSize+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
ROA_ESGESG_PY_FS <- plm(ROA ~ (ESG_normPY+E_normPY+S_normPY+G_normPY)*FirmSize+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
ROE_ESGESG_PY_FS <- plm(ROE ~ (ESG_normPY+E_normPY+S_normPY+G_normPY)*FirmSize+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(TQ_ESGESG_PY_FS, ROA_ESGESG_PY_FS, ROE_ESGESG_PY_FS, type = "text",
          title = "Panel Regression - ESG,E,S,G, and controls - Normalized by Year, with Firm Size Buckets",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with ESG
preg_TQ_ESG_PY_FirmSize <- plm(TQnorm ~ ESG_normPY*FirmSize+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_PY_FirmSize <- plm(ROA ~ ESG_normPY*FirmSize+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_PY_FirmSize <- plm(ROE ~ ESG_normPY*FirmSize+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_PY_FirmSize,preg_ROA_ESG_PY_FirmSize,preg_ROE_ESG_PY_FirmSize, type = "text",
          title = "Panel Regression - ESG and controls - Normalized by Year, with Firm Size Buckets",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with E, S, G 
TQ_ESG_sep_PY_FS <- plm(TQnorm ~ (E_normPY+S_normPY+G_normPY)*FirmSize+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
ROA_ESG_sep_PY_FS <- plm(ROA ~ (E_normPY+S_normPY+G_normPY)*FirmSize+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
ROE_ESG_sep_PY_FS <- plm(ROE ~ (E_normPY+S_normPY+G_normPY)*FirmSize+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")


stargazer(TQ_ESG_sep_PY_FS, ROA_ESG_sep_PY_FS, ROE_ESG_sep_PY_FS, type = "text",
          title = "Panel Regression - ESG,E,S,G, and controls - Normalized by Year, with Firm Size Buckets",
          align = TRUE, dep.var.labels.include = TRUE)

#Dummy Variables for Sector --------------------------------------------------------------------------------------------------------------------------------------

#Regressions with ESG, E, S, G 
preg_TQ_ESGESG_PY_Sector <- plm(TQnorm ~ (ESG_normPY+E_normPY+S_normPY+G_normPY)*Sector+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESGESG_PY_Sector <- plm(ROA ~ (ESG_normPY+E_normPY+S_normPY+G_normPY)*Sector+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESGESG_PY_Sector <- plm(ROE ~ (ESG_normPY+E_normPY+S_normPY+G_normPY)*Sector+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESGESG_PY_Sector, preg_ROA_ESGESG_PY_Sector, preg_ROE_ESGESG_PY_Sector, type = "text",
          title = "Panel Regression - ESG,E,S,G, and controls - Normalized by Year, with Sector",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with ESG
preg_TQ_ESG_PY_Sector <- plm(TQnorm ~ ESG_normPY*Sector+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_PY_Sector <- plm(ROA ~ ESG_normPY*Sector+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_PY_Sector <- plm(ROE ~ ESG_normPY*Sector+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_PY_Sector,preg_ROA_ESG_PY_Sector,preg_ROE_ESG_PY_Sector, type = "text",
          title = "Panel Regression - ESG and controls - Normalized by Year, with Sector",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with E, S, G 
preg_TQ_ESG_sep_PY_Sector <- plm(TQnorm ~ (E_normPY+S_normPY+G_normPY)*Sector+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_sep_PY_Sector <- plm(ROA ~ (E_normPY+S_normPY+G_normPY)*Sector+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_sep_PY_Sector <- plm(ROE ~ (E_normPY+S_normPY+G_normPY)*Sector+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_sep_PY_Sector,preg_ROA_ESG_sep_PY_Sector,preg_ROE_ESG_sep_PY_Sector, type = "text",
          title = "Panel Regression - ESG and controls - Normalized by Year, with Sector",
          align = TRUE, dep.var.labels.include = TRUE)

#Dummy Variables for Score Quartile --------------------------------------------------------------------------------------------------------------------------------------

ESGdf_NormP <- ESGdf_NormP %>% group_by(Year) %>% 
  mutate(ESGScoreBucket = cut(ESG, breaks = quantile(TA, probs = seq(0, 1, 0.25)),
                        include.lowest = TRUE, labels = c("Worse", "Bad", "Good", "Best")))
  ungroup()

pdata <- pdata.frame(ESGdf_NormP, index=c("Ticker", "Year"))

#Regressions with ESG, E, S, G 
preg_TQ_ESGESG_PY_Bucket <- plm(TQnorm ~ (ESG_normPY+E_normPY+S_normPY+G_normPY)*ESGScoreBucket+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESGESG_PY_Bucket <- plm(ROA ~ (ESG_normPY+E_normPY+S_normPY+G_normPY)*ESGScoreBucket+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESGESG_PY_Bucket <- plm(ROE ~ (ESG_normPY+E_normPY+S_normPY+G_normPY)*ESGScoreBucket+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESGESG_PY_Bucket, preg_ROA_ESGESG_PY_Bucket, preg_ROE_ESGESG_PY_Bucket, type = "text",
          title = "Panel Regression - ESG,E,S,G, and controls - Normalized by Year, with ESG Score Bucket",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with ESG
preg_TQ_ESG_PY_Bucket <- plm(TQnorm ~ ESG_normPY*ESGScoreBucket+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_PY_Bucket <- plm(ROA ~ ESG_normPY*ESGScoreBucket+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_PY_Bucket <- plm(ROE ~ ESG_normPY*ESGScoreBucket+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_PY_Bucket,preg_ROA_ESG_PY_Bucket,preg_ROE_ESG_PY_Bucket, type = "text",
          title = "Panel Regression - ESG and controls - Normalized by Year, with ESG Score Bucket",
          align = TRUE, dep.var.labels.include = TRUE)

#Regressions with E, S, G 
preg_TQ_ESG_sep_PY_Bucket <- plm(TQnorm ~ (E_normPY+S_normPY+G_normPY)*ESGScoreBucket+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROA_ESG_sep_PY_Bucket <- plm(ROA ~ (E_normPY+S_normPY+G_normPY)*ESGScoreBucket+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")
preg_ROE_ESG_sep_PY_Bucket <- plm(ROE ~ (E_normPY+S_normPY+G_normPY)*ESGScoreBucket+AT+TA+Lev+AG, data = pdata, index = c("Ticker", "Year"), model = "within")

stargazer(preg_TQ_ESG_sep_PY_Bucket,preg_ROA_ESG_sep_PY_Bucket,preg_ROE_ESG_sep_PY_Bucket, type = "text",
          title = "Panel Regression - ESG and controls - Normalized by Year, with ESG Score Bucket",
          align = TRUE, dep.var.labels.include = TRUE)
