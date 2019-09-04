
# time diff in CA

CA=read.csv("C:\\Users\\monis\\Downloads\\CA_avg_time.csv")

x <- strptime(as.character(CA$ARRIVAL), format="%m/%d/%Y %H:%M")
y <- strptime(as.character(CA$LU_CLEAR), format="%m/%d/%Y %H:%M")

timediff=(difftime(x,  y, units="min"))

timediff=timediff*-1
max(timediff)

CA$average_mins=timediff

write.csv(CA, file = "CA_timediff.csv",row.names=FALSE)


## time diff in FL
FL=read.csv("C:\\Users\\monis\\Downloads\\FL_avg_time.csv")

x <- strptime(as.character(FL$ARRIVAL), format="%m/%d/%Y %H:%M")
y <- strptime(as.character(FL$LU_CLEAR), format="%m/%d/%Y %H:%M")

timediff=(difftime(x,  y, units="min"))

timediff=timediff*-1
max(timediff)

FL$average_mins=timediff

write.csv(FL, file = "FL_timediff.csv",row.names=FALSE)
hist(timediff)
hist(as.numeric(FL$average_mins))


source("https://raw.githubusercontent.com/talgalili/R-code-snippets/master/boxplot.with.outlier.label.r") 
boxplot.with.outlier.label(as.numeric(FL$average_mins), lab_y, spread_text = F)
y=ca$average_minas.numeric(CA$average_mins<70)
hist(as.numeric(CA$average_mins))


# ca above 70
# fl above 