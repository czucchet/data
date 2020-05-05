library(tidyverse);library(magrittr);library(RSQLite);library(stringr);library(h2o);library(caroline);library(lubridate);library(rvest);library(readr)

data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>% 
  mutate(dateRep = as.Date(dateRep, format = "%d/%m/%Y")) %>% 
  arrange(dateRep) %>% split(.$countriesAndTerritories)

n_cases = 100
n_deaths = 20

for(i in 1:length(data)){
  data[[i]]$cum_cases = cumsum(data[[i]]$cases)
  data[[i]]$cum_deaths = cumsum(data[[i]]$deaths)
  data[[i]]$has_n_cases = ifelse(data[[i]]$cum_cases >= n_cases, 1,0)
  data[[i]]$has_n_deaths = ifelse(data[[i]]$cum_deaths >= n_deaths, 1,0)
  data[[i]]$min_n_cases = data[[i]][min(which(data[[i]]$has_n_cases == 1)),"dateRep"]
  data[[i]]$min_n_deaths = data[[i]][min(which(data[[i]]$has_n_deaths == 1)),"dateRep"]
  data[[i]]$days_since_cases = as.numeric(data[[i]]$dateRep) - as.numeric(data[[i]]$min_n_cases)
  data[[i]]$days_since_deaths = as.numeric(data[[i]]$dateRep) - as.numeric(data[[i]]$min_n_deaths)
  data[[i]]$first_case = ifelse(data[[i]]$cases > 0,1,0)
  }


data_all = bind_rows(data) 
write.csv(data_all,"covid_19.csv", row.names = F)

aus_cases = data_all %>% filter(countriesAndTerritories == "South_Korea")

sum(data_all$cases)
sum(aus_cases$cases)

yahooQF()

Symbols<-c("XOM","MSFT","JNJ","GE","CVX","WFC","PG","JPM","VZ","PFE","T","IBM","MRK","BAC","DIS","ORCL","PM","INTC","WBC.AX")

Symbols<-c("WBC.AX","ANZ.AX","CBA.AX",             "NUF.AX",  "IPL.AX",      "IFN.AX","MWY.AX",       "ORG.AX","STO.AX","WPL.AX","CPU.AX","S32.AX", "ANN.AX", "AGL.AX",
           "NAB.AX", "CSL.AX", "WOW.AX", "COL.AX", "AMC.AX", "BXB.AX","WES.AX", "QBE.AX", "NST.AX", "VRT.AX","KGN.AX", "TGR.AX", "CWY.AX","TLS.AX", "ELD.AX", "AHY.AX", "SHV.AX",
           "BARC.L","LLOY.L","BT-A.L","AV.L", "HL.L","STAN.L")
Names = c("Westpac", "ANZ", "Commonwealth Bank", "Nufarm", "Incitec", "Infigen", "Midway", "Origin", "Santos", "Woodside", "Computershare", "South32", "Annsell", "AGL", "NAB", "CSL", "Woolworths",
          "Coles", "Amcor", "Brambles", "Wesfarmers", "QBE", "Northern Star", "Virtus", "Kogan", "Tassall", "Cleanaway", "Telstra", "Elders", "Asaleo", "Select Harvests",
          "Barclays", "Lloyds", "BT Group", "Aviva", "Hargreaves", "Standard Chartered")

Sector = c("Financials","Financials","Financials","Materials","Materials", "Utilities", "Utilities", "Energy","Energy","Energy","Information Technology", "Materials","Health Care","Utilities",
           "Financials", "Health Care","Consumer Staples","Consumer Staples", "Materials", "Industrials", "Consumer Discretionary", "Insurance", "Materials", "Health Care",
           "Consumer Discretionary", "Consumer Staples", "Industrials", "Telecommuniations", "Consumer Staples", "Consumer Staples", "Consumer Staples",
           "Financials","Financials","Telecommuniations", "Insurance", "Financials", "Financials"
           )

what_metrics <- yahooQF(c("Last Trade (Price Only)","Price/Sales", 
                          "P/E Ratio","Price/EPS Estimate Next Year",
                          "PEG Ratio","Dividend Yield", "Market Capitalization","Book Value",
                          "Currency","Price/Book","EPS Forward","Dividend/Share"
                          ))
metrics <- getQuote(paste(Symbols, sep="", collapse=";"), what=what_metrics)
metrics$Sector = Sector
metrics$Names = Names


db_check = try(dbGetQuery(con, "SELECT * FROM Fixture_Detail"),silent = TRUE)
if(class(db_check) == "try-error"){
  dbWriteTable(con, "Fixture_Detail", fixture,overwrite = T)
  }
if(class(db_check) != "try-error"){
  dbWriteTable(con, "Fixture_Detail", fixture,append = T)      
  }
db_clean = try(dbGetQuery(con, "SELECT * FROM Fixture_Detail"),silent = TRUE)
if(class(db_clean) != "try-error"){
  all_records_temp =  dbGetQuery(con, "SELECT * FROM Fixture_Detail")
  all_records = unique(all_records_temp)
  dbWriteTable(con, "Fixture_Detail", all_records,overwrite = T)
}






dsol_proto = dbConnect(PostgreSQL(), user = "czucchet",
                       password = "EB%bti15ICh6NYX",
                       dbname = "dev",
                       host = "nonprod-dsol-prototyping-db.ctolc6xouppg.eu-west-1.rds.amazonaws.com", port = "5432")




https://finance.yahoo.com/quote/S32.AX/financials?p=S32.AX

df <- paste0("https://finance.yahoo.com/quote/S32.AX/financials?p=S32.AX") %>% 
  read_html() %>%   html_table() 

library(rvest)
library(tidyverse)

# Define stock name
stock <- "FB"

# Extract and transform data
df <- paste0("https://finance.yahoo.com/quote/S32.AX/financials?p=S32.AX") %>% 
  read_html() %>% 
  html_table() %>% 
  map_df(bind_cols) %>% 
  # Transpose
  t() %>%
  as_tibble()

colnames(df) <- df[1,]
# Remove first row
df <- df[-1,]
# Add stock name column
df$Stock_Name <- stock








