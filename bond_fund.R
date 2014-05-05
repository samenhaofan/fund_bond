library(XML)
library(RCurl)
library(lubridate)

Sys.setlocale("LC_ALL", "C")


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

colnames(htl) <- c("fund_code", "net_price_yb", "last_day")

htl[, "last_day"] <- as.character(htl[, "last_day"])

# Get real time transaction information for sina web interface
url <- paste("http://hq.sinajs.cn/list", 
  paste("sz",htl[,"fund_code"],sep="", collapse=','), sep='=')
fund_rt <- gsub("\"","", read.table(url, header=FALSE)[,2])
fund_rt <- read.table(text=fund_rt, sep=",", header=FALSE)
fund_rt[,1] <- gsub("^.*=", "", fund_rt[,1])
rownames(fund_rt) <- htl[,1]
rownames(htl) <- htl[,1]

fund_rt <- fund_rt[, c(1,3,4)]
#for (i in c(33:5,2)) fund_rt[, i] <- NULL
colnames(fund_rt) <- c("fund", "yesterday_price", "market_price")
# if current price is N/A, use yesterday's price instead
fund_rt[, "market_price"] <- mapply(
  function(c, p) if(c == 0.0) c <- p else c, 
  fund_rt[, "market_price"], 
  fund_rt[, "yesterday_price"])
fund_rt <- merge(htl, fund_rt, by = 0)
fund_rt[, "Row.names"] <- NULL

rm(htl) ## release htl

cnfund_header <- c("fund_code", "net_price", "np_gr")
cnfund_colClasses <- list(
  "character",
  NULL, NULL, 
  "numeric", 
  NULL, NULL, "numeric", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
cnfund <- readHTMLTable(
  "http://data.cnfund.cn/DownExcel.htm",
  colClasses = cnfund_colClasses,
  StringsAsFactors = FALSE,
  which = 1)

colnames(cnfund) <- cnfund_header

dta <- merge(fund_rt, cnfund, by = "fund_code")
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
dta <- within(dta, disc_r <- (net_price - market_price) / net_price)
dta <- within(dta, disc_ry <- disc_r / as.numeric((as.Date(ymd(last_day))-Sys.Date())/365))
dta <- within(dta, forecast_r <- (1+disc_ry) * (1+f_np_gr))
dta <- within(dta, w_forecast_r <- forecast_r * (1+disc_r))


# only keep those funds with all necessary data
# dta <- dta[!is.na(dta$w_forecast_r) & !is.infinite(dta$w_forecast_r),]
dta <- dta[with(dta, !is.na(w_forecast_r) & !is.infinite(w_forecast_r)),]
rownames(dta) <- dta[, "fund_code"]
dta[, "fund_code"] <- NULL

write.csv(dta[with(dta, order(-forecast_r)), ], "fund_realtime2.csv")

