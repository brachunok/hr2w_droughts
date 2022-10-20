# read data

df <- read.csv("~/Documents/__college/reseach_stuff/hr2w_droughts/data/santa_cruz/generated_drought_scenarios/baseline.csv")

library(reshape2)
library(ggplot2)
df <- df[,c(2,3,4,5,6)]
df$taitStreet <- df$taitStreet + df$feltonDiversions
df <-df[,c(1,2,3,5)]
df$total <- df[,2] + df[,3] + df[,4]
df[,c(2,3,4)] <- df[,c(2,3,4)]/df$total


df_plot <-melt(df,id.vars = "date",measure.vars = c("northCoast","taitStreet","newellInflow"))
df_plot$date <- as.Date(df_plot$date)
p1 <- ggplot(df_plot,aes(x=date,y=value,fill=variable))+geom_area()+scale_fill_brewer()+theme_bw()+ylab("%age of monthly water from each source")
p1
