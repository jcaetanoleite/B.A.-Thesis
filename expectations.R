##################Part 1: EXPECTATIONS######################

###libraries
library(purrr)
library(dplyr)
library(rbcb) #rbcb is available in github. Gathers Brazilian Central Bank data


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
  reduce(bind_rows)

view(IPCA_1_year_ahead)

##extracting one year ahead IPCA (Brazilian CPI/inflation index) expectations##

IPCA_2_year_ahead= tibble(reference_years <- 2002:2020,
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
  reduce(bind_rows)

view(IPCA_2_year_ahead)


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
  reduce(bind_rows)


df_list <- split(df, f = df$indic_detail)

# Checking for order
df_list

# Changing dataset names
df_list <- set_names(df_list, nm = c("Gross Government Debt", "Public Sector Net Debt",
                                     "Nominal Result", "Primary Result"))

list2env(df_list, envir = .GlobalEnv)

##extracting fiscal expectations two years ahead###

fiscal = get_annual_market_expectations('Fiscal',
                                        start_date = '2000-01-01')
view(fiscal)



df2=tibble(reference_years <- 2002:2020,
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
  reduce(bind_rows)


df_list2 <- split(df, f = df$indic_detail)

# Checking for order
df_list2

# Changing dataset names
df_list2 <- set_names(df_list2, nm = c("Gross Government Debt 2", "Public Sector Net Debt 2",
                                     "Nominal Result 2", "Primary Result 2"))

list2env(df_list, envir = .GlobalEnv)
