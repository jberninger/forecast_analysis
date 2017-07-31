library(readr)
library(dplyr)
library(data.table)
library(ggplot2)
library(Hmisc)
library(scales)
library(vars)
library(readxl)
library(stringr)
library(zoo)
library(vars)
library(reshape2)
library(tidyr)
library(plotly)


# load the data that was given by ben in the flash drive
# This is the observed values
top15mod <- read_csv("/top15mod.csv")
# these are the forecast point predictions
fc_apr_pred <- read_xlsx("Transactions_Forecast_201704.xlsx", skip = 1) # APRIL 2017
fc_nov_pred <- read_xlsx("Transactions_Forecast_201611.xlsx", skip = 1) # NOV 2016
fc_dec_pred <- read_xlsx("Transactions_Forecast_201612.xlsx", skip = 1) # DEC 2016
# these are the confidence intervals
fc_apr_ci <- read_xlsx("Transactions_Forecast_201704.xlsx", sheet = "Forecast Interval", skip = 1) # APRIL 2017
fc_nov_ci <- read_xlsx("Transactions_Forecast_201611.xlsx", sheet = "Forecast Interval", skip = 1) # Nov 2016
fc_dec_ci <- read_xlsx("Transactions_Forecast_201612.xlsx", sheet = "Forecast Interval", skip = 1) # Dec 2016
colnames(fc_apr_ci) <- tolower(colnames(fc_apr_ci))
colnames(fc_nov_ci) <- tolower(colnames(fc_nov_ci))
colnames(fc_dec_ci) <- tolower(colnames(fc_dec_ci))

# remap the column names from all the datasets
# I grabbed the mapping from one of Bens scripts
fc_apr_pred <- fc_apr_pred %>% dplyr::select("year_month" = "X__1",                          
                                             "deed" = "Deed",                          
                                             "st" = "Substitution Trustee",          
                                             "td" = "Trust Deed",                    
                                             "rcv" = "Reconveyance",                  
                                             "rgl" = "Release Government Lien",       
                                             "atd" = "Assignment Trust Deed",         
                                             "aj" = "Abstract Judgement",            
                                             "re" = "Release",                       
                                             "cnd" = "Cancellation Notice of Default",
                                             "nd" = "Notice Default" ,               
                                             "nts" = "Notice of Trustees Sales",      
                                             "su" = "Subordination",                 
                                             "ast" = "Affidavit Successor Trustee",   
                                             "bond" = "Bond",                          
                                             "mtd" = "Modify Trust Deed")

fc_nov_pred <- fc_nov_pred %>% dplyr::select("year_month" = "X__1",                          
                            "deed" = "Deed",                          
                            "st" = "Substitution Trustee",          
                            "td" = "Trust Deed",                    
                            "rcv" = "Reconveyance",                  
                            "rgl" = "Release Government Lien",       
                            "atd" = "Assignment Trust Deed",         
                            "aj" = "Abstract Judgement",            
                            "re" = "Release",                       
                            "cnd" = "Cancellation Notice of Default",
                            "nd" = "Notice Default" ,               
                            "nts" = "Notice of Trustees Sales",      
                            "su" = "Subordination",                 
                            "ast" = "Affidavit Successor Trustee",   
                            "bond" = "Bond",                          
                            "mtd" = "Modify Trust Deed")

fc_dec_pred <- fc_dec_pred %>% dplyr::select("year_month" = "X__1",                          
                            "deed" = "Deed",                          
                            "st" = "Substitution Trustee",          
                            "td" = "Trust Deed",                    
                            "rcv" = "Reconveyance",                  
                            "rgl" = "Release Government Lien",       
                            "atd" = "Assignment Trust Deed",         
                            "aj" = "Abstract Judgement",            
                            "re" = "Release",                       
                            "cnd" = "Cancellation Notice of Default",
                            "nd" = "Notice Default" ,               
                            "nts" = "Notice of Trustees Sales",      
                            "su" = "Subordination",                 
                            "ast" = "Affidavit Successor Trustee",   
                            "bond" = "Bond",                          
                            "mtd" = "Modify Trust Deed")

fc_apr_ci <- fc_apr_ci %>% dplyr::select(-17) %>% rename("year_month" = "x__1")
fc_nov_ci <- fc_nov_ci %>% dplyr::select(-17) %>% rename("year_month" = "x__1")
fc_dec_ci <- fc_dec_ci %>% dplyr::select(-17) %>% rename("year_month" = "x__1")

top15mod <- top15mod %>% dplyr::select("year_month" = "date",                          
                            "deed" = "D003",                          
                            "st" = "S003",          
                            "td" = "T008",                    
                            "rcv" = "R002",                  
                            "rgl" = "R020",       
                            "atd" = "A017",         
                            "aj" = "A002",            
                            "re" = "R009",                       
                            "cnd" = "C004",
                            "nd" = "N005" ,               
                            "nts" = "N011",      
                            "su" = "S002",                 
                            "ast" = "A025",   
                            "bond" = "B002",                          
                            "mtd" = "M054")

# now we want all the date columns to jive as date/time objects
# since the forecast predictions were by month, 
# we will coerce the full date(y-m-d) from the top15mod set just to the year and month
fc_apr_pred$year_month <- fc_apr_pred$year_month %>% str_replace_all("M", "-") %>% as.yearmon()
fc_nov_pred$year_month <- fc_nov_pred$year_month %>% str_replace_all("M", "-") %>% as.yearmon()
fc_dec_pred$year_month <- fc_dec_pred$year_month %>% str_replace_all("M", "-") %>% as.yearmon()

fc_apr_ci$year_month <- fc_apr_ci$year_month %>% str_replace_all("M", "-") %>% as.yearmon()
fc_nov_ci$year_month <- fc_nov_ci$year_month %>% str_replace_all("M", "-") %>% as.yearmon()
fc_dec_ci$year_month <- fc_dec_ci$year_month %>% str_replace_all("M", "-") %>% as.yearmon()

top15mod$year_month <- top15mod$year_month %>% as.Date("%Y-%m-%d") %>% as.yearmon()


# Now I want to join the predicted values with the confidence intervals for each forecast
# this is an inner join because the predicted value datasets have the training data in them as well

forecast_apr <- inner_join(fc_apr_pred, fc_apr_ci, by = "year_month")
forecast_nov <- inner_join(fc_nov_pred, fc_nov_ci, by = "year_month")
forecast_dec <- inner_join(fc_dec_pred, fc_dec_ci, by = "year_month")

# now i want to join the observed values with the different forecast dataset
# obviously, our date variable, year_month, is the joining variable
# NOTE: since the two datasets we are joing share some of the same column names, the first dataset in the inner_join
# function will be on the left in the new object, and all its data is appended with .x
# analgously, the second dataset is on the right, and all its data is appended with .y (on the column names)

################################################################################################
## July 18th
## import the data from April, May, June 2017 and include it in the analysis plots
# load the data
apr17 <- read_csv("E:/JordanBerninger/Transaction_Forecast (Ben, Dr Yu)/ERA_DocsbyTitle(0417).csv", skip = 3)
may17 <- read_csv("E:/JordanBerninger/Transaction_Forecast (Ben, Dr Yu)/ERA_DocsbyTitle(0517).csv", skip = 3)
jun17 <- read_csv("E:/JordanBerninger/Transaction_Forecast (Ben, Dr Yu)/ERA_DocsbyTitle(0617).csv", skip = 3)
# rbind into the master dataset of observed values
# top15mod is the dataset with the observed values
## tidy the datasets
apr17 <- apr17 %>% dplyr::select(c(1,3)) %>% spread(textbox35, textbox5) %>% mutate("year_month" = as.yearmon("Apr 2017"))
may17 <- may17 %>% dplyr::select(c(1,3)) %>% spread(textbox35, textbox5) %>% mutate("year_month" = as.yearmon("May 2017"))
jun17 <- jun17 %>% dplyr::select(c(1,3)) %>% spread(textbox35, textbox5) %>% mutate("year_month" = as.yearmon("Jun 2017"))
## now select the columns so it can be rbinded in
## use the same dplyr::select code from above (chunk that was used for top15mod)

apr17 <- apr17 %>% dplyr::select("year_month",                          
                                 "deed" = "D003",                          
                                 "st" = "S003",          
                                 "td" = "T008",                    
                                 "rcv" = "R002",                  
                                 "rgl" = "R020",       
                                 "atd" = "A017",         
                                 "aj" = "A002",            
                                 "re" = "R009",                       
                                 "cnd" = "C004",
                                 "nd" = "N005" ,               
                                 "nts" = "N011",      
                                 "su" = "S002",                 
                                 "ast" = "A025",   
                                 "bond" = "B002",                          
                                 "mtd" = "M054")

may17 <- may17 %>% dplyr::select("year_month",                          
                                 "deed" = "D003",                          
                                 "st" = "S003",          
                                 "td" = "T008",                    
                                 "rcv" = "R002",                  
                                 "rgl" = "R020",       
                                 "atd" = "A017",         
                                 "aj" = "A002",            
                                 "re" = "R009",                       
                                 "cnd" = "C004",
                                 "nd" = "N005" ,               
                                 "nts" = "N011",      
                                 "su" = "S002",                 
                                 "ast" = "A025",   
                                 "bond" = "B002",                          
                                 "mtd" = "M054")

jun17 <- jun17 %>% dplyr::select("year_month",                          
                                 "deed" = "D003",                          
                                 "st" = "S003",          
                                 "td" = "T008",                    
                                 "rcv" = "R002",                  
                                 "rgl" = "R020",       
                                 "atd" = "A017",         
                                 "aj" = "A002",            
                                 "re" = "R009",                       
                                 "cnd" = "C004",
                                 "nd" = "N005" ,               
                                 "nts" = "N011",      
                                 "su" = "S002",                 
                                 "ast" = "A025",   
                                 "bond" = "B002",                          
                                 "mtd" = "M054")

top15mod <- rbind(top15mod, apr17, may17, jun17)

forecast_obs_apr <- left_join(forecast_apr, top15mod, by = "year_month")
# this one doesnt join data because there is no date overlap (expected)
forecast_obs_nov <- inner_join(forecast_nov, top15mod, by = "year_month")
# there are 5 time periods in ^ this one
forecast_obs_dec <- inner_join(forecast_dec, top15mod, by = "year_month") %>% slice(-1)
# I had to take off the first row from the december ^ one because it included november 2016 for some reason
# this is because I relied on the joins to do the date filtering, 
# instead of explicitly defining the past (train) and the future (forecast)

## these datasets should be ready for ggplot()

#####################################################################################
### Instead of melting, we can use multiple geom_point() calls for each type
### We want all 15 documents aggregated
# this is the data frame that we want to work with forecast_obs_dec
# we need to convert a few columns from character strings to numeric values

forecast_obs_nov <- forecast_obs_nov %>% mutate(rgl.x = as.numeric(rgl.x),nts.x = as.numeric(nts.x),
                                                ast.x = as.numeric(ast.x))

forecast_obs_dec <- forecast_obs_dec %>% mutate(rgl.x = as.numeric(rgl.x),nts.x = as.numeric(nts.x),
                                                ast.x = as.numeric(ast.x))

forecast_obs_apr <- forecast_obs_apr %>% mutate(rgl.x = as.numeric(rgl.x),nts.x = as.numeric(nts.x),
                                                ast.x = as.numeric(ast.x))

#####################################################################################

forecast_obs_nov <- forecast_obs_nov %>% dplyr::mutate(pred_agg = (deed.x + st.x + td.x + rcv.x + rgl.x + atd.x +
                                                              aj.x + re.x + cnd.x + nd.x + nts.x + su.x + ast.x + bond.x + mtd.x) ,
                                                obs_agg = (deed.y + st.y + td.y + rcv.y + rgl.y + atd.y +
                                                             aj.y + re.y + cnd.y + nd.y + nts.y + su.y + ast.y + bond.y + mtd.y) ,
                                                lci_agg = (deed_l + st_l + td_l + rc_l + rgl_l + atd_l +
                                                             aj_l + re_l + cnd_l + nd_l + nts_l + su_l + ast_l + bond_l + mtd_l) ,
                                                uci_agg = (deed_u + st_u + td_u + rc_u + rgl_u + atd_u +
                                                             aj_u + re_u + cnd_u + nd_u + nts_u + su_u + ast_u + bond_u + mtd_u))


forecast_obs_dec <- forecast_obs_dec %>% dplyr::mutate(pred_agg = (deed.x + st.x + td.x + rcv.x + rgl.x + atd.x +
                                                                     aj.x + re.x + cnd.x + nd.x + nts.x + su.x + ast.x + bond.x + mtd.x) ,
                                                       obs_agg = (deed.y + st.y + td.y + rcv.y + rgl.y + atd.y +
                                                                    aj.y + re.y + cnd.y + nd.y + nts.y + su.y + ast.y + bond.y + mtd.y) ,
                                                       lci_agg = (deed_l + st_l + td_l + rc_l + rgl_l + atd_l +
                                                                    aj_l + re_l + cnd_l + nd_l + nts_l + su_l + ast_l + bond_l + mtd_l) ,
                                                       uci_agg = (deed_u + st_u + td_u + rc_u + rgl_u + atd_u +
                                                                    aj_u + re_u + cnd_u + nd_u + nts_u + su_u + ast_u + bond_u + mtd_u))

forecast_obs_apr <- forecast_obs_apr %>% dplyr::mutate(pred_agg = (deed.x + st.x + td.x + rcv.x + rgl.x + atd.x +
                                                                     aj.x + re.x + cnd.x + nd.x + nts.x + su.x + ast.x + bond.x + mtd.x) ,
                                                       obs_agg = (deed.y + st.y + td.y + rcv.y + rgl.y + atd.y +
                                                                    aj.y + re.y + cnd.y + nd.y + nts.y + su.y + ast.y + bond.y + mtd.y) ,
                                                       lci_agg = (deed_l + st_l + td_l + rc_l + rgl_l + atd_l +
                                                                    aj_l + re_l + cnd_l + nd_l + nts_l + su_l + ast_l + bond_l + mtd_l) ,
                                                       uci_agg = (deed_u + st_u + td_u + rc_u + rgl_u + atd_u +
                                                                    aj_u + re_u + cnd_u + nd_u + nts_u + su_u + ast_u + bond_u + mtd_u))


#######################################################################################

#ggplot(forecast_obs_nov, aes(x=year_month)) + geom_ribbon(aes(ymin=deed_l, ymax=deed_u), color="grey70", alpha=.2) + 
#  geom_point(aes(y=deed.x), color="black", size = 3) + geom_point(aes(y=deed.y), color="red", size = 4) + 
#  geom_point(aes(y=deed_u), color="black") + geom_point(aes(y=deed_l), color="black")

## When I convert the x-axis to character, the geom_ribbon becomes discrete
## I guess you can't have both (yet)

#ggplot(forecast_obs_nov, aes(x=as.character(year_month)))  + geom_smooth(aes(y=deed.x), color="grey70", fill="grey70") + 
#  geom_point(aes(y=deed.x), color="black", size = 3) + geom_point(aes(y=deed.y), color="red", size = 4) + 
#  geom_point(aes(y=deed_u), color="black") + geom_point(aes(y=deed_l), color="black")

fc1 <- ggplot(forecast_obs_nov, aes(x=year_month))  + geom_ribbon(aes(ymin=lci_agg,ymax=uci_agg), fill="#FFA9FD", alpha = .2) + 
  geom_point(aes(y=pred_agg), color="#FF13F9", size = 1.1) + geom_point(aes(y=obs_agg), color="black", size = 2) + 
  geom_point(aes(y=uci_agg), color="#FF57FB") + geom_point(aes(y=lci_agg), color="#FF57FB") + 
  geom_point(aes(y=deed.x), color="blue") + geom_point(aes(y=st.x), color="lightblue") + geom_point(aes(y=td.x), color="pink") + 
  geom_point(aes(y=rcv.x), color="purple") + ylim(4500, 200000) + 
  scale_x_continuous(name = "Date", breaks = seq(from = min(forecast_obs_nov$year_month), to = max(forecast_obs_nov$year_month), length.out = 8), 
                     labels=c("Nov 16", "Dec 16", "Jan 17", "Feb 17", "Mar 17", "Apr 17", "May 17", "Jun 17")) +
  ggtitle("November 2016 Forecast")

ggplotly(fc1)

# ^ repeat for december and april

fc2 <- ggplot(forecast_obs_dec, aes(x=year_month))  + geom_ribbon(aes(ymin=lci_agg,ymax=uci_agg), fill="#FFA9FD", alpha = .2) + 
  geom_point(aes(y=pred_agg), color="#FF13F9", size = 1.1) + geom_point(aes(y=obs_agg), color="black", size = 2) + 
  geom_point(aes(y=uci_agg), color="#FF57FB") + geom_point(aes(y=lci_agg), color="#FF57FB") + 
  geom_point(aes(y=deed.x), color="blue") + geom_point(aes(y=st.x), color="lightblue") + geom_point(aes(y=td.x), color="pink") + 
  geom_point(aes(y=rcv.x), color="purple") + ylim(4500, 200000) + 
  scale_x_continuous(name = "Date", breaks = seq(from = min(forecast_obs_dec$year_month), to = max(forecast_obs_dec$year_month), length.out = 7), 
                     labels=c("Dec 16", "Jan 17", "Feb 17", "Mar 17", "Apr 17", "May 17", "Jun 17")) +
  ggtitle("December 2016 Forecast")

ggplotly(fc2)


fc3 <- ggplot(forecast_obs_apr, aes(x=year_month))  + geom_ribbon(aes(ymin=lci_agg,ymax=uci_agg), fill="#FFA9FD", alpha = .2) + 
  geom_point(aes(y=pred_agg), color="#FF13F9", size = 1.1) + geom_point(aes(y=obs_agg), color="black", size = 2) + 
  geom_point(aes(y=uci_agg), color="#FF57FB") + geom_point(aes(y=lci_agg), color="#FF57FB") + 
  geom_point(aes(y=deed.x), color="blue") + geom_point(aes(y=st.x), color="lightblue") + geom_point(aes(y=td.x), color="pink") + 
  geom_point(aes(y=rcv.x), color="purple") + ylim(4500, 200000) + 
  scale_x_continuous(name = "Date", breaks = seq(from = min(forecast_obs_apr$year_month), to = max(forecast_obs_apr$year_month), length.out = 15), 
                     labels=c("Apr 17", "", "Jun 17", "", "Aug 17", "", 
                              "Oct 17", "", "Dec 17", "", "Feb 18", "" ,"Apr 18", "", "Jun 18")) +
  ggtitle("April 2017 Forecast")

ggplotly(fc3)
