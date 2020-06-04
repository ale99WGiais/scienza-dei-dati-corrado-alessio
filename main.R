setwd("~/Dropbox/mintos/dataset")

library(dplyr)
library(gtools)
library(lubridate)
library(readr)
library(tibble)
library(tidyr)
library(readxl)
library(ggplot2)
library(stringr)
library(viridis)
library(gganimate)
library(ggrepel)
library(dbplyr)
library(RSQLite)
library(gifski)

conn = DBI::dbConnect(RSQLite::SQLite(), dbname = "loan_book.sqlite3")

statement = read_excel("account-statement.xlsx") %>% arrange(Date) %>% drop_na(Date)
statement
current = read_excel("current-investments.xlsx")
current
finished = read_excel("finished-investments.xlsx")
finished

date_interval = statement %>% select(Date) %>% mutate(Date = as.Date(Date)) %>% summarise(min = min(Date), max = max(Date))
date_interval

#book_last = read_excel("loan-book/9500001-10000000_loan_book.xlsx")

all = bind_rows(current %>% mutate(Category = "Current"), finished %>% mutate(Category = "Finished"))

Sys.glob("loan-book/*")

#for(file in Sys.glob("loan-book/*")){
#  print(file)
#}





###### Interest rate per LO (all)

data = all %>% select(`Interest Rate`, `Loan Originator`)
data

ggplot(data) +
  geom_violin(aes(x = `Loan Originator`, y = `Interest Rate`)) +
  coord_flip()


##### transazioni cumulative

primary = statement %>%
  filter(Turnover < 0, str_detect(Details, "investment in")) %>% 
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(date_interval$min, date_interval$max, by="day"), Turnover=0) %>%
  group_by(Date) %>% 
  summarise(Turnover=-sum(Turnover)) %>%
  mutate(Category = "primary")

secondary = statement %>%
  filter(Turnover < 0, str_detect(Details, "secondary mark")) %>% 
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(date_interval$min, date_interval$max, by="day"), Turnover=0) %>%
  group_by(Date) %>% 
  summarise(Turnover=-sum(Turnover)) %>%
  mutate(Category = "secondary")

data = rbind(primary, secondary)
data

ggplot(data) +
  geom_line(aes(x = Date, y = cumsum(Turnover), color = Category), stat = "identity")

###### transazioni

primary = statement %>%
  filter(Turnover < 0, str_detect(Details, "investment in")) %>% 
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(date_interval$min, date_interval$max, by="day"), Turnover=0) %>%
  group_by(Date) %>% 
  summarise(Turnover=-sum(Turnover)) %>%
  mutate(Category = "primary")

secondary = statement %>%
  filter(Turnover < 0, str_detect(Details, "secondary mark")) %>% 
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(date_interval$min, date_interval$max, by="day"), Turnover=0) %>%
  group_by(Date) %>% 
  summarise(Turnover=-sum(Turnover)) %>%
  mutate(Category = "secondary")

data = rbind(primary, secondary)
data

ggplot(data) +
  geom_bar(aes(x = Date, y = Turnover, fill = Category), stat = "identity")

###### profitto per LO

data = all %>% 
  mutate(`Profit` = `Outstanding Principal` + `Received Payments` + `Pending Payments` - `My Investments`) %>%
  filter(Profit > 0) %>%
  group_by(`Loan Originator`) %>% 
  summarise(`Invested` = sum(`My Investments`), 
           `Average NAR` = mean(`Interest Rate`),
           Profit = sum(Profit))

ggplot(data, aes(x = `Invested`, y = `Average NAR`)) +
  geom_point(aes(size = `Profit`, col = rainbow(nrow(data))), alpha = 0.5, show.legend = FALSE) +
  scale_size(range = c(.1, 30)) +
  ggrepel::geom_text_repel(aes(label=`Loan Originator`))


######## status per LO %

data = current %>% group_by(`Status`, `Loan Originator`) %>% summarise(Outstanding = sum(`Outstanding Principal`))

ggplot(data, aes(x = "", y = Outstanding, fill = `Status` )) + 
  geom_bar(stat = "identity", position = position_fill()) +
  #geom_text(aes(label = `Outstanding`), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~ `Loan Originator`)  +
  theme_void() +
  theme(legend.position='bottom') 
  
######### status per LO 

ggplot(current %>% group_by(`Status`, `Loan Originator`) %>% summarise(Outstanding = sum(`Outstanding Principal`))) +
  geom_bar(mapping = aes(x = `Loan Originator`, y = Outstanding, fill = `Status`), stat = "identity") +
  coord_flip()

########## current status

ggplot(current %>% group_by(`Status`) %>% summarise(Outstanding = sum(`Outstanding Principal`)), 
       aes(x="", y=Outstanding, fill=`Status`))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()

########## outstanding per loan originator

ggplot(current %>% group_by(`Loan Originator`) %>% summarise(Outstanding = sum(`Outstanding Principal`)), 
  aes(x="", y=Outstanding, fill=`Loan Originator`))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()


########### deposito+profitto

deposit = statement %>%
  filter(str_detect(Details, "Deposits|Withdrawals")) %>% 
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(date_interval$min, date_interval$max, by="day"), Turnover=0) %>%
  group_by(Date) %>% 
  summarise(Turnover=sum(Turnover)) %>%
  mutate(Category = "deposit")

profit = statement %>% 
  filter(str_detect(Details, "Deposits|investment|Invest|Principal|principal|Withdrawals|- seconda|Outgoin", negate = TRUE)) %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(date_interval$min, date_interval$max, by="day"), Turnover=0) %>%
  group_by(Date) %>% 
  summarise(Turnover=sum(Turnover)) %>%
  mutate(Category = "profit")

data = bind_rows(profit, deposit)
data

ggplot(data) + 
  geom_area(mapping=aes(x = Date, y = cumsum(Turnover), fill = Category))

###### pending payments

data = full_join(all %>% group_by(`Loan Originator`, Category) %>% summarise(Pending = sum(`Pending Payments`)),
                  all %>% group_by(`Loan Originator`) %>% summarise(Investment = 
                                                                      sum(`My Investments`)),
                 by = "Loan Originator") %>% mutate(PendingPercent = 100 * Pending / max(0.001, Investment))
data


ggplot(data, aes(x = `Loan Originator`, y = Pending, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Pending payments")

ggplot(data, aes(x = `Loan Originator`, y = PendingPercent, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 100, color = "red", linetype="dashed") +
  geom_hline(yintercept = 20, color = "orange", linetype="dashed") +
  geom_hline(yintercept = 10, color = "green", linetype="dashed") +
  geom_hline(yintercept = 5, color = "blue", linetype="dashed") +
  coord_flip() +
  ggtitle("Pending payments to Invested (%)")


###### termine
if(FALSE){
data = current %>% mutate(`Remaining Term` = as.integer(replace(`Remaining Term`, `Remaining Term` == "Late", 0))) %>%
  group_by(`Remaining Term`) %>% summarise(Val = sum(`Outstanding Principal`))# %>%
  complete(`Remaining Term` = seq.int(0, max(`Remaining Term`)), Val=0) 
data  

ggplot(data, aes(x = `Remaining Term`)) + 
  geom_bar(aes(y = Val), stat = "identity", fill = "blue") +
  geom_line(aes(y = sum(Val) - cumsum(Val)), col = "red") +
  geom_text(aes(x = max(`Remaining Term`) - 2, y = 100, label = "Invested amount"), col = "red") +
  geom_text(aes(x = 1, y = 500, label = ""), col = "blue")
}


###### NAR nel tempo

start = all %>% mutate(Date = `Date of Investment`) %>% select(Date, `Interest Rate`) %>%
  mutate(Date = as.Date(Date, format = "%d.%m.%Y")) %>%
  complete(Date = seq.Date(date_interval$min, date_interval$max, by="day"), `Interest Rate` = 0) %>% 
  group_by(Date) %>%
  summarise(n = n(), Interest = sum(`Interest Rate`)) %>%
  mutate(nadd = cumsum(n), InterestAdd = cumsum(Interest)) %>% 
  select(Date, nadd, InterestAdd)

end = finished %>% mutate(Date = `Closing Date`) %>% select(Date, `Interest Rate`) %>%
  mutate(Date = as.Date(Date, format = "%d.%m.%Y")) %>%
  complete(Date = seq.Date(date_interval$min, date_interval$max, by="day"), `Interest Rate` = 0) %>% 
  group_by(Date) %>%
  summarise(n = n(), Interest = sum(`Interest Rate`)) %>%
  mutate(nsub = cumsum(n), InterestSub = cumsum(Interest)) %>%
  select(Date, nsub, InterestSub)
  
data = inner_join(start, end) %>% mutate(`Average NAR` = (InterestAdd - InterestSub) / (nadd - nsub)) %>%
  select(Date, `Average NAR`)
data

ggplot(data) + 
  geom_line(aes(x = Date, y = `Average NAR`))


######### SQLITE ######################






###### nar current vs loan book

start = all %>% mutate(Date = `Date of Investment`) %>% select(Date, `Interest Rate`) %>%
  mutate(Date = as.Date(Date, format = "%d.%m.%Y")) %>%
  complete(Date = seq.Date(date_interval$min, date_interval$max, by="day"), `Interest Rate` = 0) %>% 
  group_by(Date) %>%
  summarise(n = n(), Interest = sum(`Interest Rate`)) %>%
  mutate(nadd = cumsum(n), InterestAdd = cumsum(Interest)) %>% 
  select(Date, nadd, InterestAdd)

end = finished %>% mutate(Date = `Closing Date`) %>% select(Date, `Interest Rate`) %>%
  mutate(Date = as.Date(Date, format = "%d.%m.%Y")) %>%
  complete(Date = seq.Date(date_interval$min, date_interval$max, by="day"), `Interest Rate` = 0) %>% 
  group_by(Date) %>%
  summarise(n = n(), Interest = sum(`Interest Rate`)) %>%
  mutate(nsub = cumsum(n), InterestSub = cumsum(Interest)) %>%
  select(Date, nsub, InterestSub)

investments = inner_join(start, end) %>% mutate(`Average NAR` = (InterestAdd - InterestSub) / (nadd - nsub)) %>%
  select(Date, `Average NAR`) %>% mutate(Category = "Current Investments")

loan_book = dbGetQuery(conn, "select date(`Listing Date`) as Date, avg(`Loan Rate Percent`) as `Average NAR`, 'Loan book' as Category
           from loans where Currency = 'EUR' and Date is not NULL group by Date;") %>% mutate(Date = as.Date(Date))

loan_book_buyback = dbGetQuery(conn, "select date(`Listing Date`) as Date, avg(`Loan Rate Percent`) as `Average NAR`, 'Loan book (buyback)' as Category
           from loans where Currency = 'EUR' and Buyback = 'Yes' and Date is not NULL group by Date;") %>% mutate(Date = as.Date(Date))
      
ggplot(rbind(loan_book, investments, loan_book_buyback)) + 
  geom_line(aes(x = Date, y = `Average NAR`, col = `Category`))


###### NAR violin loan book


data = bind_rows(current %>% select(`Interest Rate`) %>% group_by(`Interest Rate`) %>% summarize(count = n()) %>% mutate(Category = "Current"),
                 all %>% select(`Interest Rate`) %>% group_by(`Interest Rate`) %>% summarize(count = n()) %>% mutate(Category = "All"),
                 dbGetQuery(conn, "select count(*) as count, `Loan Rate Percent` as `Interest Rate`, 'Loan book' as Category from loans 
 where Currency = 'EUR' group by `Interest Rate`;") )
data


ggplot(data, aes(x = Category, y = `Interest Rate`, fill = Category)) +
  geom_violin() +
  geom_boxplot(width = 0.1, show.legend = FALSE)


###### NAR violin  loan book across time

data = dbGetQuery(conn, "select date(`Listing Date`) as Date, count(*) as count, `Loan Rate Percent` as `Interest Rate` from loans 
 where Currency = 'EUR' and Date is not NULL group by `Interest Rate`;") %>% mutate(Date = as.Date(Date))

ggplot(data, aes(x = count, y = `Interest Rate`)) +
  geom_violin(fill = "blue") +
  transition_time(Date) +
  labs(title = "Year: {frame_time}")

anim_save("NAR violin across time.gif")

DBI::dbDisconnect(conn)

