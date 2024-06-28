#Starter script to analyze Patiria swath data
setwd("/Users/veronicapagowski/Desktop/MARINE_star_data") #or whatever directory you are using 

#Import a map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
map<-ggplot2::ggplot(data = world) +
  ggplot2::geom_sf() +
  ggplot2::coord_sf(
    xlim = c(-138, -105),
    ylim = c(20, 60),
    expand = FALSE
  )

######### Now look at Patiria miniata data   ###########
swath_data <- read.csv("swath_data_copy.csv", header = TRUE)
#Filter to just look at Patiria - we can adjust later if we want to look at associations with other species
pmin_data <- swath_data %>% filter(species_lump == "Patiria miniata")

#Map densities
map + geom_point(data=pmin_data,alpha=0.3, aes(x=longitude, y=latitude, size=density_per_m2), color="cadetblue")+
  labs(size = expression(Density~(per~m^2)))+ theme_void()

#color if searched and found no stars
pmin_data <- pmin_data %>%
  mutate(color = ifelse(density_per_m2 == 0, "black", "cadetblue"))

map +
  geom_point(data = pmin_data, alpha = 0.3, aes(x = longitude, y = latitude, size = density_per_m2, color = color)) +
  labs(size = expression(Density~(per~m^2)))+
  theme_void() +
  scale_color_identity()  

#Look at a specific years
pmin_data_preblob <- pmin_data %>% filter(year < 2013)
map +
  geom_point(data = pmin_data_preblob, alpha = 0.3, aes(x = longitude, y = latitude, size = density_per_m2, color = color)) +
  labs(size = expression(Density~(per~m^2)))+
  theme_void() +
  scale_color_identity()  

pmin_data_postblob <- pmin_data %>% filter(year > 2014)
map +
  geom_point(data = pmin_data_postblob, alpha = 0.3, aes(x = longitude, y = latitude, size = density_per_m2, color = color)) +
  labs(size = expression(Density~(per~m^2)))+
  theme_void() +
  scale_color_identity()  

#Look at trends by latitude and year
map + geom_jitter(data=pmin_data, aes(x=longitude, y=latitude, size=density_per_m2,colour=year,alpha=0.4))

# Plot density over time
ggplot(pmin_data, aes(x=year, y=density_per_m2, color=bioregion)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) #may not be the best fit but for now fit a linear trend

#Plot a different way
p1<-ggplot(pmin_data,aes(bioregion,density_per_m2,fill=as.factor(year)))+
  geom_bar(position="dodge",stat="identity")
p1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #this plot is a little ugly ~ modify as necessary :) 

