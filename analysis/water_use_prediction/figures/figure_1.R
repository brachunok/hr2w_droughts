options(scipen=99999999)
library(stringr)
# figure1, paper 2: the water use and summary figure 
# you have to run the appropriate script on sherlock (figure_1)
# which will make the outputs. but because i can't get SF to work on 
# sherlock, the outputs get sent here to plot
setwd("~/Documents/__college/reseach_stuff/hr2w_droughts/analysis/water_use_prediction/figures/")
df <-read.csv("./fig_1_map_data.csv")
df <- df[,-1]

# make the FIPS what it needs to match actual geoids
df$FIPS <- str_pad(df$FIPS,width=12,side="left",pad="0")


#library('drat')
library(tidycensus)
library(ggplot2)
library('sf')
library('maps')
library("rnaturalearth")
library("ggspatial")
library(cowplot)

# it's hacky, but basically i'll just pull this data for income then
# replace it with our water uses. 
sc_income <- get_decennial(
  geography = "block group", 
  state = "CA", 
  variables = "P005003",
  county="Santa Cruz",
  year = 2010,
  geometry = TRUE
)

# merge in my summer and winter values 
plot_data <- merge(sc_income,df,by.x="GEOID",by.y="FIPS")
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))


winter<- ggplot(plot_data)+  geom_sf(fill = "antiquewhite1")+ geom_sf(data=plot_data,aes(fill=winter))+
  lims(x=c(-122.08,-121.95),y=c(36.945,37.022))+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                         size = 0.5),
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = c(.5,.5))+
  scale_fill_gradient(low = "#ffffff",  high = "#C41E3A",
                       limits = c(3, 14))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()
  )

summer <- ggplot(plot_data)+  geom_sf(fill = "antiquewhite1")+ geom_sf(data=plot_data,aes(fill=summer))+
  lims(x=c(-122.08,-121.95),y=c(36.945,37.022))+
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
  scale_fill_gradient(low = "#ffffff",  high = "#C41E3A",
                      limits = c(3, 14),oob=scales::squish )+labs(fill="CCF")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()
  )

legend <- get_legend(summer)
summer <- summer +scale_fill_gradient(low = "#ffffff",  high = "#C41E3A",
                                      limits = c(3, 14),oob=scales::squish,guide="none")

wuse <- plot_grid(winter,summer,ncol=1,axis="l")

# okay now the violin plots
load("./best_tract_model_importances.Rdata")
load("./unscaled_tract_aggregate_data.Rdata")
load("./unscaled_tract_aggregate_resp.Rdata")
load("./scaled_tract_ggregate_data.Rdata")

imp_vars <- data.frame(importance_data[,1])

# now combine all these
library(ggridges)
library(reshape2)
# do a quick manipulation of the data dataframe to wide to see if it works
df_aggregate_unscaled <- df_aggregate_unscaled[,which(colnames(df_aggregate_unscaled)%in%(imp_vars[,1]))]

# test plot data
columns_to_plot <- importance_data$variable[order(importance_data$importance,decreasing = T)[1:20]]

remove_these <- c("bill_length","or_own_oc","X..of.Units","Parcel..","Effective.Year","Bedrooms_tc")
columns_to_plot<-columns_to_plot[-which(columns_to_plot%in%remove_these)]

# custom data frame of colors based on what type of data it is
data_type_labels <- data.frame(var=columns_to_plot)
data_type_labels$type <- "End"
data_type_labels$type[which(data_type_labels$var%in%c("AET_mm","meanT_degC"))]<- "Ex_U"
data_type_labels$type[which(data_type_labels$var%in%c("cumulative_drought"))]<- "Ex_C"

# Add SFR

unscaled_plot_data <- df_aggregate_unscaled[,which(colnames(df_aggregate_unscaled)%in%columns_to_plot)]
plot_data <- data.frame(scale(unscaled_plot_data),center=TRUE,scale=TRUE)

unscaled_plot_data_melt <- melt(unscaled_plot_data)
plot_data_melt <- melt(plot_data)

#merge in the data type
plot_data_melt <-merge(plot_data_melt,data_type_labels,by.x="variable",by.y="var")

resp_melted <- data.frame(center=TRUE,scale=TRUE,value=resp_aggregate$x,variable="resp",type="Resp")
sfr_melted <- data.frame(center=TRUE,scale=TRUE,value=sfr_agg,variable="sfr",type="End")
plot_data_melt <- rbind(plot_data_melt,sfr_melted,resp_melted)

plot_data_mins <- aggregate(unscaled_plot_data_melt$value,by=list(unscaled_plot_data_melt$variable),FUN=function(x){quantile(x,0.05)})

additional_mins <- data.frame(Group.1=c("sfr","resp"),x=c(0,0))
plot_data_mins <- rbind(plot_data_mins,additional_mins)

plot_data_max <- aggregate(unscaled_plot_data_melt$value,by=list(unscaled_plot_data_melt$variable),FUN=function(x){quantile(x,0.95)})

additional_maxs <- data.frame(Group.1=c("sfr","resp"),x=c(1,24.77))
plot_data_max <- rbind(plot_data_max,additional_maxs)

VJUST= -0.5

SIZE= 3
densities <- ggplot()+geom_density_ridges(data=plot_data_melt,aes(x=value,y=variable,fill=type))+xlim(c(-2,2))+theme_bw()+ylab("")+scale_y_discrete(position = "right")+
  geom_text(data=plot_data_mins,aes(x=-2,y=Group.1,label=round(x,digits = 1)),vjust=VJUST,hjust=0,size=SIZE)+
  geom_text(data=plot_data_max,aes(x=2,y=Group.1,label=round(x,digits = 1)),vjust=VJUST,hjust=1,size=SIZE) +
  xlab("Z-score") + scale_fill_manual(values=c("End" = "#279989",
                                        "Ex_C" = "#8F993E",
                                        "Ex_U"= "#175E54",
                                        "Resp" = "#C41E3A"))+
  theme(legend.position = c(.5,.5))

densities

# TOMORROW: 

# shrink the max/min labels 
# add the %res
# add water use and color appropriately

output_plot <- plot_grid(wuse,densities,ncol=2,rel_widths = c(1,1.4))                                                                    
ggsave(output_plot,filename="./figure_1.pdf",width=8,height=6)
