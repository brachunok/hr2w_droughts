# map of the santa cruz area

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

states <- map_data("state")
ca_df <- subset(states, region == "california")

counties <- map_data("county")
ca_county <- subset(counties, region == "california")
ca_county$is_SC <- ca_county$subregion=="santa cruz"

ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray90")+
  theme_nothing() + 
  geom_polygon(data = ca_county, aes(fill = is_SC), color = "white") +
  geom_polygon(color = "black", fill = NA)+ scale_fill_manual(values=c("#D3D3D3","#FF0000"))  # get the state border back on top
ca_base
