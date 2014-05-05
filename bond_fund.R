library(XML)
library(RCurl)
library(lubridate)

Sys.setlocale("LC_ALL", "C")


# only keep the fund code, market price, net price, price incremental %, 
htl_colClasses = list(
	"character", 		# code
	NULL, 			# name
	"numeric", 		# market price
	NULL, 			# net price
	"character", 		# market price growth rate
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

htl_header <- c(
	"fund_code",
	"market_price",
	"market_price_gr",
	"net_price_yb",
	"last_day")
colnames(htl) <- htl_header

htl[, "market_price_gr"] <- as.character(htl[, "market_price_gr"])
htl[, "last_day"] <- as.character(htl[, "last_day"])
htl[htl$market_price_gr == "-100.00%", "market_price_gr"] <- ""


cnfund_header <- c("fund_code", "net_price")
cnfund_colClasses <- list(
  "character",
  NULL, NULL, 
  "numeric", 
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
cnfund <- readHTMLTable(
    "http://data.cnfund.cn/DownExcel.htm",
    colClasses = cnfund_colClasses,
    StringsAsFactors = FALSE,
    which = 1)

colnames(cnfund) <- cnfund_header

dta <- merge(htl, cnfund, by = "fund_code")

y1 <- as.Date(paste(format(Sys.Date(), "%Y"), "0101", sep = ""), "%Y%m%d")
y2 <- as.Date(paste(format(Sys.Date(), "%Y"), "1231", sep = ""), "%Y%m%d")
passed_days <- as.numeric(as.Date(Sys.Date()) - y1 + 1)
total_days <- as.numeric(y2 - y1 + 1)

dta$market_price[dta$market_price == 0] <- NA
dta <- within(dta, net_gr <- (net_price - net_price_yb))
dta <- within(dta, a_np_gr <- net_gr / net_price_yb)
dta <- within(dta, f_np_gr <- a_np_gr / passed_days * total_days)
dta <- within(dta, disc_r <- (net_price - market_price) / net_price)
dta <- within(dta, disc_ry <- disc_r / as.numeric((as.Date(ymd(last_day))-Sys.Date())/365))
dta <- within(dta, forecast_r <- (1+disc_ry) * (1+f_np_gr))
dta <- within(dta, w_forecast_r <- forecast_r * (1+disc_r))

# update saved information and if there is no market real data, using the last data instead
if (!exists("dta_shadow")){ 
  dta_shadow <- readRDS("fund_up2date.Rdx") #assume this file exists!
} else {
  for (col in colnames(dta)){
    for (row in nrow(dta)){
      if (is.na(dta[row, col]) | is.infinite(dta[row, col])){
        dta[row, col] <- dta_shadow[row, col]
      } else {
        dta_shadow[row, col] <- dta[row, col]
      }
    }
  }
}

# save shadow of data into a file for later usage
saveRDS(dta_shadow, "fund_up2date.Rda")

# only keep those funds with all necessary data
# dta <- dta[!is.na(dta$w_forecast_r) & !is.infinite(dta$w_forecast_r),]
dta <- dta[with(dta, !is.na(w_forecast_r) & !is.infinite(w_forecast_r)),]
write.csv(dta[with(dta, order(-forecast_r)), ], "fund_realtime.csv")

