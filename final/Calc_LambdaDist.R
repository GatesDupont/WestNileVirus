require(plotrix)
plotCI(x=final$yr, y=final$avg, ui=final$U, li=final$L,
       pch=20, ylab="Relative Abundance", xlab="Year",
       main="American Crows, Northeast US", bty="n")

#----Calculating Lambda----
Nt = takes.df$V9; N0 = takes.df$V6; Time = 2004 - 2001
r = (log(Nt) - log(N0))/Time
lambda = exp(r) - 1

lambda_tuti_0401 = lambda
rm(list=setdiff(ls(), "lambda_tuti_0401"))
