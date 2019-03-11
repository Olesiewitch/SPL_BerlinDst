library(xtable)
library(xlsx)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")

 a = read.xlsx("Archive/Berlin Livibility Index Indicators.xlsx",
          sheetName = "Sheet1", encoding = "Latin-1")
 xtable(a[, 1:9], caption = "Indicators for the Liveability Index")
 