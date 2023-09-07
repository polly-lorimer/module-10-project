library(tidyverse)
library(readxl)

setwd("D:/Users/polly.lorimer/OneDrive - MHCLG/Documents/Training/Data Science Campus/Data Science for Policy/technical-exercise")

# Import CPI Data 

cpi <- read_excel("Data/consumerpriceinflationdetailedreferencetables (1).xlsx",
                  sheet = "Table 2",
                  skip = 16,
                  n_max = 25)%>%
  select("month" = "...2",
         "restaurant_and_hotel_cpi"="L52D")
  
cpi$month[2:6] <- paste0(cpi$month[2:6]," 2021")
cpi$month[7:12] <- paste0(cpi$month[7:12]," 2022")
cpi$month[14:18] <- paste0(cpi$month[14:18]," 2022")
cpi$month[19:24] <- paste0(cpi$month[19:24]," 2023")

month_list <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
 
 for (i in month_list){
   
   num_month <- which(month_list == i)%>%as.character()
   
   if (num_month %in% c(10,11,12)){
     num_month <- num_month
   } else {
     num_month <- paste0("0",num_month)
   }
   
   month_data <- cpi %>%
     filter(grepl(i,month))
   
   month_data$month <- gsub(paste0(i," "), paste0("01/",num_month,"/"), month_data$month)
   
   cpi <- cpi %>% 
     filter(!grepl(i,month))%>%
     rbind(month_data)
 }
 
cpi$month <- as.Date(cpi$month, format = "%d/%m/%Y")

# visualise this 

ggplot(cpi, aes(x = month, y = restaurant_and_hotel_cpi, group = 1))+
  geom_line(linewidth = 1)+
  theme_bw()+
  xlab("")+
  ylab("Restaurant and Hotel CPI")

# Import Redundancy Data 

red <- read_excel("Data/red02aug2023.xlsx",
                         sheet = "Industry - levels",
                         skip = 5,
                         n_max = 175)%>%
  na.omit()%>%
  select("month"="...1",
         "a&f_red" = "Accommodation & food services")

red$month <- gsub("^.{0,4}", "", red$month)

for (i in month_list){
  
  num_month <- which(month_list == i)%>%as.character()
  
  if (num_month %in% c(10,11,12)){
    num_month <- num_month
  } else {
    num_month <- paste0("0",num_month)
  }
  
  month_data <- red %>%
    filter(grepl(i,month))
  
  month_data$month <- gsub(paste0(i," "), paste0("01/",num_month,"/"), month_data$month)
  
  red <- red %>% 
    filter(!grepl(i,month))%>%
    rbind(month_data)
}

red$month <- as.Date(red$month, format = "%d/%m/%Y")

# This is showing redundancies in 3 months before so let's divide by 3 for appoximate redundancies that month

red$`a&f_red` <- as.numeric(red$`a&f_red`)/3

# Merge data 

red_cpi <- cpi %>%
  merge(red,
        how = "left",
        by = "month")%>%
  na.omit()

ggplot(red_cpi, aes(x = month, y = `a&f_red`))+
  geom_line()

# Import earnings data 

earnings <- read_excel("Data/weekly_earnings.xlsx")

for (i in month_list){
  
  num_month <- which(month_list == i)%>%as.character()
  
  if (num_month %in% c(10,11,12)){
    num_month <- num_month
  } else {
    num_month <- paste0("0",num_month)
  }
  
  month_data <- earnings %>%
    filter(grepl(i,month))
  
  month_data$month <- gsub(paste0(" ",i),"", month_data$month)
  month_data$month <- paste0("01/",num_month,"/",month_data$month)
  
  earnings <- earnings %>% 
    filter(!grepl(i,month))%>%
    rbind(month_data)
}

earnings$month <- as.Date(earnings$month, format = "%d/%m/%Y")

# Now Adjust for inflation  

# Download overall CPI 

overall_cpi<- read_excel("Data/consumerpriceinflationdetailedreferencetables (1).xlsx",
                                 sheet = "Table 1",
                                 skip = 13,
                                 n_max = 38)%>%
  select("month" = "...1",
         "cpi" = "D7BT")

overall_cpi$month[2:6] <- paste0(overall_cpi$month[2:6]," 2020")
overall_cpi$month[7:12] <- paste0(overall_cpi$month[7:12]," 2021")
overall_cpi$month[14:18] <- paste0(overall_cpi$month[14:18]," 2021")
overall_cpi$month[19:24] <- paste0(overall_cpi$month[19:24]," 2022")
overall_cpi$month[26:30] <- paste0(overall_cpi$month[26:30]," 2022")
overall_cpi$month[31:36] <- paste0(overall_cpi$month[31:36]," 2023")


for (i in month_list){
  
  num_month <- which(month_list == i)%>%as.character()
  
  if (num_month %in% c(10,11,12)){
    num_month <- num_month
  } else {
    num_month <- paste0("0",num_month)
  }
  
  month_data <- overall_cpi %>%
    filter(grepl(i,month))
  
  month_data$month <- gsub(paste0(i," "), paste0("01/",num_month,"/"), month_data$month)
  
  overall_cpi <- overall_cpi %>% 
    filter(!grepl(i,month))%>%
    rbind(month_data)
}

overall_cpi$month <- as.Date(overall_cpi$month, format = "%d/%m/%Y")

earnings_adjusted <- earnings %>% 
  merge(overall_cpi,
        how = 'left',
        on = 'month')%>%
  mutate(weekly_adjusted = 100*`a&f_weekly_earnings`/cpi)

# Merge with other data 

hospitality_data <- red_cpi %>% 
  merge(earnings_adjusted, 
        how = 'left',
        on = 'month')

ggplot()+
  geom_line(data = hospitality_data, aes(x = month,y = `a&f_weekly_earnings`))

ggplot()+
  geom_line(data = hospitality_data, aes(x = month,y = `weekly_adjusted`)) 
  
# create a time series 

ts_data <- hospitality_data %>% 
  select("restaurant_and_hotel_cpi",
         "a&f_weekly_earnings",
         "a&f_red")

ts <- ts(ts_data,start = decimal_date(ymd("2021-07-01")),frequency = 365.25/12)

plot(ts)

restaurant_and_hotel_cpi <- ts(ts_data %>% select("restaurant_and_hotel_cpi"),start = decimal_date(ymd("2021-07-01")),frequency = 365.25/12)
af_weekly_earnings <- ts(ts_data %>% select("a&f_weekly_earnings"),start = decimal_date(ymd("2021-07-01")),frequency = 365.25/12)

ccf(as.numeric(restaurant_and_hotel_cpi), as.numeric(af_weekly_earnings)) 

d_cpi <- diff(restaurant_and_hotel_cpi)
d_earnings <- diff(af_weekly_earnings)

ccf(as.numeric(d_cpi),as.numeric(d_earnings))

af_red <- ts(ts_data %>% select("a&f_red"),start = decimal_date(ymd("2021-07-01")),frequency = 365.25/12)

d_af_red <- diff(af_red)
ccf(as.numeric(d_cpi),as.numeric(d_af_red))

plot(d_af_red)

ccf(as.numeric(d_earnings),as.numeric(d_af_red))

ad_earnings <- ts(hospitality_data$weekly_adjusted,
                  start = decimal_date(ymd("2021-07-01")),frequency = 365.25/12)

plot(ad_earnings)

d_ad_earnings <- diff(ad_earnings)

ccf(as.numeric(d_cpi),as.numeric(d_ad_earnings))

