# used for the preparation of the pilot study paper



# Setup -------------------------------------------------------------------

library(data.table)

user <- Sys.info()[[7]]

if(user == "ben"){
  dataF <- path.expand("~/temp/serlRecruit18000.csv")
}


# Tables ----
tr <- table(all_onboards$region, all_onboards$consent)
trp <- prop.table(tr, 1)*100
totalByQuin <- table(all_onboards$quintile)

t <- table(all_onboards$region, all_onboards$quintile)
t
# we can see some strange 'over-contacting' in some IMD cells per region
# we would expect them to be the same frequency if smart meter roll-out was random
tp <- prop.table(t,1)*100

# table of row % with last row as frequency by IMD quin
rbind(tp,totalByQuin)
