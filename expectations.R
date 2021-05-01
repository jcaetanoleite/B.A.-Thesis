##################Part 1: EXPECTATIONS######################


###libraries
library(purrr)
library(dplyr)
library(rbcb) #rbcb is available in github. Gathers Brazilian Central Bank data
library(xts)
library(forecast)
library(ggplot2)
library(ggthemes)
library(easyGgplot2)
library(mFilter)
library(grid)
library(png)
library(urca)
library(vars)
library(svars)
library(writexl)
library(slider)
library(tibble)
library(purrr)
library(lubridate, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)


##extracting one year ahead IPCA (Brazilian CPI/inflation index) expectations##


IPCA_1_year_ahead= tibble(reference_years <- 2001:2019,
initial_dates = seq(as.Date("2000-01-01"), as.Date("2018-01-01"), by = "years"),
final_dates = seq(as.Date("2000-12-31"), as.Date("2018-12-31"), by = "years"),
ipca_expectations = pmap(.l = list(ref = reference_years,
                                       init = initial_dates,
                                       final = final_dates),
                             .f = function(ref, init, final){
                               
                               get_annual_market_expectations("IPCA",
                                                              start_date = init,
                                                              end_date = final) %>% 
                                 filter(reference_year == ref)
                               
                             }))%>%pluck("ipca_expectations")%>%
  reduce(bind_rows)%>%
  mutate(date = lubridate::ymd(date)) 

view(IPCA_1_year_ahead)


IPCAE<-IPCA_1_year_ahead %>%
  mutate(monthly = floor_date(date, "month"))%>%
  group_by(monthly)%>%summarise((across(mean:respondents, function(x) mean(x))))

view(IPCAE)
 


summary(IPCAE)




##extracting fiscal expectations one year ahead###

fiscal = get_annual_market_expectations('Fiscal',
                                        start_date = '2000-01-01')
view(fiscal)



df=tibble(reference_years <- 2001:2019,
           initial_dates = seq(as.Date("2000-01-01"), as.Date("2018-01-01"), by = "years"),
           final_dates = seq(as.Date("2000-12-31"), as.Date("2018-12-31"), by = "years"),
           fiscal_expectations = pmap(.l = list(ref = reference_years,
                                                init = initial_dates,
                                                final = final_dates),
                                      .f = function(ref, init, final){
                                        
                                        get_annual_market_expectations("Fiscal",
                                                                       start_date = init,
                                                                       end_date = final)%>% 
                                          filter(reference_year == ref)
                                      }))%>%pluck("fiscal_expectations")%>%
  reduce(bind_rows)%>%
  mutate(date = lubridate::ymd(date))
 


df_list <- split(df, f=df$indic_detail)



# Checking for order
df_list

# Changing dataset names
df_list <- set_names(df_list, nm = c("Gross_Government_Debt", "Public_Sector_Net_Debt",
                                     "Nominal_Result", "Primary_Result"))

list2env(df_list, envir = .GlobalEnv)

# To monthly

GGDE<-Gross_Government_Debt %>%
  mutate(monthly = floor_date(date, "month"))%>%
  group_by(monthly)%>%summarise((across(mean:respondents, function(x) mean(x))))

view(GGDE)

PSNE<-Public_Sector_Net_Debt%>%
  mutate(monthly = floor_date(date, "month"))%>%
  group_by(monthly)%>%summarise((across(mean:respondents, function(x) mean(x))))

view(PSNE)

PRIME<-Primary_Result%>%
  mutate(monthly = floor_date(date, "month"))%>%
  group_by(monthly)%>%summarise((across(mean:respondents, function(x) mean(x))))

view(PRIME)

NOME<-Nominal_Result%>%
  mutate(monthly = floor_date(date, "month"))%>%
  group_by(monthly)%>%summarise((across(mean:respondents, function(x) mean(x))))

view(NOME)




#exporting to excel to clean data

write_xlsx(IPCAE,"C:/Users/Joao/Desktop/séries monografia/expectativas monetária/IPCA_1year.xlsx")

write_xlsx(PRIME,"C:/Users/Joao/Desktop/séries monografia/expectativas fiscal/expectativas anuais coleta diária/um ano à frente/Primary_result.xlsx")

write_xlsx(NOME,"C:/Users/Joao/Desktop/séries monografia/expectativas fiscal/expectativas anuais coleta diária/um ano à frente/Nominal_result.xlsx")

write_xlsx(GGDE,"C:/Users/Joao/Desktop/séries monografia/expectativas fiscal/expectativas anuais coleta diária/um ano à frente/GGD.xlsx")

write_xlsx(Public_Sector_Net_Debt,"C:/Users/Joao/Desktop/séries monografia/expectativas fiscal/expectativas anuais coleta diária/um ano à frente/PSND.xlsx")


