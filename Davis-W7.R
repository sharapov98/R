load(url("http://michaelgastner.com/ASEAN_tourism.Rda"))
print(tourism)
tourism[7,,10]==sum(tourism[1:7,,10])

load(url("http://michaelgastner.com/percent_bachelors_degrees_women_usa.Rda"))

matplot( perc_women[1:42], perc_women[,2:18], type="b", lwd=1, pch=seq_len(nrow(perc_women[,2:18])) ,
         ylab="% degrees conferred to women by major", xlab="Year")

load(url("http://michaelgastner.com/ny_mayor.Rda"))

barplot(v, col=c("blue","red","grey"), beside = TRUE, las=1, main="NYC Mayoral Election by Borough")

legend(x = 30,
y = 12,  # y-coordinate found by trial and error.
legend = rownames(vote),
fill = c("blue","red","grey"),
bg = "ghostwhite",
title = "Region of origin",
title.adj = 0.1,  # Move legend title to the left.
xpd = TRUE)
