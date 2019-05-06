#TMAP by PM1
#Done by PM2

library(tmap)

data(World)

which(World$gdp_cap_est == max(World$gdp_cap_est, na.rm = TRUE))
World$gdp_cap_est[7] <- NA
World$log10gdp_cap_est <- log10(World$gdp_cap_est)

map <- tm_shape(World) +
  tm_polygons(c("HPI", "log10gdp_cap_est"), 
              title = c("Happy planet index", "Log 10 scaled \n GDP per capita"),
              palette = list("YlGnBu", "YlOrRd")) +
  tm_layout(main.title = "HPI and GDP per capita",
            main.title.position = "left",
            panel.labels = c("Happy planet index 2016", "GDP per capita in 2014"),
            bg.color = "skyblue",
            legend.bg.color = "grey",
            legend.bg.alpha = 0.5,
            legend.position = c("left", "bottom")) +
  tm_grid(projection = "longlat", labels.size = 0)

tmap_save(tm = map, filename = "tmap.pdf", width = 6, height = 8)

#SECOND PART
tmap_mode("view")
map + tm_view(text.size.variable = TRUE,
            set.view = c(15.2, 54.5, 3))


#THE MAPS SEPERATELY

# tm_shape(World, bbox = ) + 
#   tm_polygons("HPI", palette = "YlGnBu",
#               title = "Happy planet index",
#               style = "quantile") +
#   tm_layout(bg.color = "skyblue",
#             legend.bg.color = "grey",
#             legend.bg.alpha = 0.5,
#             main.title = "HPI and GDP per capita") +
#   tm_grid(projection = "longlat", labels.size = 0)


# tm_shape(World, bbox = ) + 
#   tm_polygons("log10gdp_cap_est") +
#   tm_layout(bg.color = "skyblue") +
#   tm_grid(projection = "longlat", labels.size = 0)



