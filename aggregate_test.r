d <- read.table(text=
'Name     Month  Rate1     Rate2
Aira       1      12        23
Aira       2      18        73
Aira       3      19        45
Ben        1      53        19
Ben        2      22        87
Ben        3      19        45
Cat        1      22        87
Cat        2      67        43
Cat        3      45        32', header=TRUE)

aggregate(d[, 3:4], list(d$Name), mean)
aggregate(data$ftime, list(d$fid, d$map), mean)
