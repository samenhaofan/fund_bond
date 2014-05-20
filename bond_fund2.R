library(XML)
library(RCurl)
library(lubridate)

Sys.setlocale("LC_ALL", "C")

get_fund_price <- function(t, fund_code){
  url <- paste("http://hq.sinajs.cn/list", 
               paste(t,fund_code,sep="", collapse=','), sep='=')
  fund_rt <- gsub("\"","", read.table(url, header=FALSE)[,2])
  fund_rt <- read.table(text=fund_rt, sep=",", header=FALSE, as.is = TRUE)
  fund_rt[,1] <- gsub("^.*=", "", fund_rt[,1])
  return(fund_rt)
}

# only keep the fund code, market price, net price, price incremental %, 
htl_colClasses = list(
  "character", 		# code
  NULL, 			# name
  NULL, 		# market price
  NULL, 			# net price
  NULL, 		# market price growth rate
  NULL,			# net price growth
  NULL,			# discount rate
  NULL,			# yearly discount rate
  NULL,			# accumulated price growth
  NULL,			# accumulated net price growth rate
  NULL,			# 7 days shift price growth rate
  NULL,			# 7 days shift market price growth rate
  NULL,			# 7 days shift net price growth rate
  NULL,			# last week's market price
  NULL,			# last week's net price
  NULL,			# market price of yearly beginning
  "numeric",		# net price of yearly beginning
  NULL,			# interest
  "character")		# last close day

htl <- readHTMLTable(
  "http://www.htlfund.com/stockfund.php",
  colClasses = htl_colClasses, 
  which = 3,
  StringsAsFactors = FALSE)

if(!exists("htl"))    htl <- read.csv("fund_list.csv")



colnames(htl) <- c("fund_code", "net_price_yb", "last_day")

htl[, "last_day"] <- as.character(htl[, "last_day"])

# Get real time transaction information for sina web interface
fund_rt <- get_fund_price("sz", htl[,"fund_code"])
rownames(fund_rt) <- htl[,1]
rownames(htl) <- htl[,1]

fund_rt <- fund_rt[, c(1,3,4)]
#for (i in c(33:5,2)) fund_rt[, i] <- NULL
colnames(fund_rt) <- c("fund", "yesterday_price", "market_price")
# if current price is N/A, use yesterday's price instead
#fund_rt[, "market_price"] <- mapply(
#  function(c, p) ifelse(c == 0.0, p, c), 
#  fund_rt[, "market_price"], 
#  fund_rt[, "yesterday_price"])
fund_rt[, "market_price"] <- sapply(fund_rt$market_price, function(p) ifelse(p == 0.0, NA, p))
fund_rt <- merge(htl, fund_rt, by = 0)
# correct found mistaken data
fund_rt$last_day <- mapply(
  function(e, d) ifelse(e == "150127", "20150301", d), fund_rt$fund_code, fund_rt$last_day)

fund_rt[, "Row.names"] <- NULL


## Get fund net price
fund_net <- get_fund_price("of", htl[,"fund_code"])[, c(2, 5, 6)]
colnames(fund_net) <- c("net_price", "np_gr", "date")
rownames(fund_net) <- htl[,1]
rownames(fund_rt) <- htl[,1]
#fund_net <- fund_net[fund_net$date == Sys.Date(),]

if (wday(Sys.Date()) %in% c(1:5) & Sys.time() > strptime("20:00:46", "%H:%M:%S")) {
  fund_net$net_price <- mapply(function(n, d) ifelse(d == Sys.Date(), n, NA), fund_net$net_price, fund_net$date)
  fund_net$np_gr <- mapply(function(n, d) ifelse(d == Sys.Date(), n, NA), fund_net$np_gr, fund_net$date)
}
fund_net["date"] <- NULL


dta <- merge(fund_rt, fund_net, by = 0)
dta[, "Row.names"] <- NULL

#rm(cnfund)

y1 <- as.Date(paste(format(Sys.Date(), "%Y"), "0101", sep = ""), "%Y%m%d")
y2 <- as.Date(paste(format(Sys.Date(), "%Y"), "1231", sep = ""), "%Y%m%d")
passed_days <- as.numeric(as.Date(Sys.Date()) - y1 + 1)
total_days <- as.numeric(y2 - y1 + 1)

#dta$market_price[dta$market_price == 0] <- NA
dta[, "np_gr"] <- dta[, "np_gr"] / 100
dta <- within(dta, d_mp_gr <- (market_price - yesterday_price) / yesterday_price)

#dta <- within(dta, a_net_diff <- (net_price - net_price_yb))
dta <- within(dta, a_np_gr <- (net_price - net_price_yb) / net_price_yb)
dta <- within(dta, f_np_gr <- a_np_gr / passed_days * total_days)
dta <- within(dta, disc_r <- (net_price - ifelse(is.na(market_price), yesterday_price, market_price)) / net_price)

## for those funds whose discount is too small, probably because they are approaching the end of closed period, 
## we don't pay attention to the discount because it twists the actual value

dta <- within(dta, disc_ry <- ifelse(disc_r < .01 & disc_r > 0, 0, disc_r) / as.numeric((as.Date(ymd(last_day))-Sys.Date())/365))
dta <- within(dta, forecast_r <- (1+disc_ry) * (1+f_np_gr))
dta <- within(dta, w_forecast_r <- forecast_r * (1+disc_r))


# only keep those funds with all necessary data
# dta <- dta[!is.na(dta$w_forecast_r) & !is.infinite(dta$w_forecast_r),]
dta <- dta[with(dta, !is.na(w_forecast_r) & !is.infinite(w_forecast_r)),]
rownames(dta) <- dta[, "fund_code"]
dta[, "fund_code"] <- NULL

write.csv(dta[with(dta, order(-forecast_r)), ], "C:/Users/ejjnffn/fund_realtime2.csv", na = "")

