rm(list = ls(all = TRUE)) #Start new session by clearing everything

# Title: "Nitrogen classification maps for India, Malawi and Ethiopia"
# Author email: "nc.konath@cgiar.org"
# Date: "December 2022"
# ---

library(raster)
library(sf)
library(rgdal)
library(rnaturalearth)
library(tmap)
library(rmapshaper)
library(ggpubr)
library(ggplot2)
getwd()


#read data for the latest year (2013)
#rice <- raster::brick("D:\\projects\\Tai-Paper\\N classification maps\\rasterstack1981rice.tif")
nClassRice <- raster("rasterstack1981rice.tif",band = 33)
nClassWheat <- raster("rasterstack1981wheat.tif",band = 33)
nClassMaize <- raster("rasterstack1981maize.tif",band = 33)

worldmap <- ne_countries(scale = 110, returnclass = "sf")
#worldMap <- read_sf("Shapefiles/gadm36_0.shp")


#countries > India, Ethiopia and Malawi

India <- read_sf("Shapefiles/India_Boundary.shp")
Ethiopia <- read_sf("Shapefiles/Ethiopia.shp")
Malawi <- read_sf("Shapefiles/Malawi.shp")


#Plots
#India
India_Rice <- tm_shape(India)+
  #tm_polygons("white")+
  tm_borders("grey20", lwd = .7) +
  tm_shape(mask(crop(nClassRice,India),India),is.master = FALSE) + 
  tm_raster(style = "fixed", n = 10, title = "N Classification, Maize",
            breaks =  c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
            palette = c("#d7301f","#fc8d59","#fdcc8a","#fef0d9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4", "#225ea8", "#253494"),
            labels = c("Surplus N, Low NUE, Low removal gap", "Surplus N, Low NUE, High removal gap","Surplus N, Med NUE, Low removal gap", "Surplus N, Med NUE, High removal gap", "Deficit N, Low NUE, Low removal gap", "Deficit N, Low NUE, High removal gap","Deficit N, Med NUE,Low removal gap", "Deficit N, Med NUE, High removal gap", "Deficit N, N mining, Low removal gap", "Deficit N, N mining, High removal gap"))+
  #legend.hist = T,
  #legend.hist.z  = NA)+
  tm_legend(outside = T, legend.outside.position =  "bottom",
            legend.position = c(0.9,1))+
  tm_layout(legend.show = FALSE,
            legend.title.fontface = "bold",
            legend.title.size = 0.7,
            legend.text.size = 0.5,
            #inner.margins=c(0, 0, 0.17, 0.),
            inner.margins=c(0.025, 0.025,0.025 , 0.025),
            #outer.margins = c(0.02, -0.3, 0, -0.5),
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            earth.boundary = TRUE,frame = FALSE)

India_Wheat <- tm_shape(India)+
  #tm_polygons("white")+
  tm_borders("grey20", lwd = .7) +
  tm_shape(mask(crop(nClassWheat,India),India),is.master = FALSE) + 
  tm_raster(style = "fixed", n = 10, title = "N Classification, Maize",
            breaks =  c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
            palette = c("#d7301f","#fc8d59","#fdcc8a","#fef0d9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4", "#225ea8", "#253494"),
            labels = c("Surplus N, Low NUE, Low removal gap", "Surplus N, Low NUE, High removal gap","Surplus N, Med NUE, Low removal gap", "Surplus N, Med NUE, High removal gap", "Deficit N, Low NUE, Low removal gap", "Deficit N, Low NUE, High removal gap","Deficit N, Med NUE,Low removal gap", "Deficit N, Med NUE, High removal gap", "Deficit N, N mining, Low removal gap", "Deficit N, N mining, High removal gap"))+
  #legend.hist = T,
  #legend.hist.z  = NA)+
  tm_legend(outside = T, hist.width = 4)+
  tm_layout(legend.show = FALSE,
            legend.title.fontface = "bold",
            legend.title.size = 0.7,
            legend.text.size = 0.5,
            #inner.margins=c(0, 0, 0.17, 0.),
            inner.margins=c(0.025, 0.025,0.025 , 0.025),
            #outer.margins = c(0.02, -0.3, 0, -0.5),
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            earth.boundary = TRUE,frame = TRUE)

India_Maize <- tm_shape(India)+
  #tm_polygons("white")+
  tm_borders("grey20", lwd = .7) +
  tm_shape(mask(crop(nClassMaize,India),India),is.master = FALSE) + 
  tm_raster(style = "fixed", n = 10, title = "N Classification, Maize",
            breaks =  c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
            palette = c("#d7301f","#fc8d59","#fdcc8a","#fef0d9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4", "#225ea8", "#253494"),
            labels = c("Surplus N, Low NUE, Low removal gap", "Surplus N, Low NUE, High removal gap","Surplus N, Med NUE, Low removal gap", "Surplus N, Med NUE, High removal gap", "Deficit N, Low NUE, Low removal gap", "Deficit N, Low NUE, High removal gap","Deficit N, Med NUE,Low removal gap", "Deficit N, Med NUE, High removal gap", "Deficit N, N mining, Low removal gap", "Deficit N, N mining, High removal gap"))+
  #legend.hist = T,
  #legend.hist.z  = NA)+
  tm_legend(outside = T, hist.width = 4)+
  tm_layout(legend.show = FALSE,
            legend.title.fontface = "bold",
            legend.title.size = 0.7,
            legend.text.size = 0.5,
            #inner.margins=c(0, 0, 0.17, 0.),
            inner.margins=c(0.025, 0.025,0.025 , 0.025),
            #outer.margins = c(0.02, -0.3, 0, -0.5),
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            earth.boundary = TRUE,frame = TRUE)

#Malawi
Malawi_Rice <- tm_shape(Malawi)+
  #tm_polygons("white")+
  tm_borders("grey20", lwd = .7) +
  tm_shape(mask(crop(nClassRice,Malawi),Malawi),is.master = FALSE) + 
  tm_raster(style = "fixed", n = 10, title = "N Classification, Maize",
            breaks =  c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
            palette = c("#d7301f","#fc8d59","#fdcc8a","#fef0d9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4", "#225ea8", "#253494"),
            labels = c("Surplus N, Low NUE, Low removal gap", "Surplus N, Low NUE, High removal gap","Surplus N, Med NUE, Low removal gap", "Surplus N, Med NUE, High removal gap", "Deficit N, Low NUE, Low removal gap", "Deficit N, Low NUE, High removal gap","Deficit N, Med NUE,Low removal gap", "Deficit N, Med NUE, High removal gap", "Deficit N, N mining, Low removal gap", "Deficit N, N mining, High removal gap"))+
  #legend.hist = T,
  #legend.hist.z  = NA)+
  tm_legend(outside = T, legend.outside.position =  "bottom",
            legend.position = c(0.1,1))+
  tm_layout(asp = 2,
            legend.show = FALSE,
            legend.title.fontface = "bold",
            legend.title.size = 0.7,
            legend.text.size = 0.5,
            #inner.margins=c(0, 0, 0.17, 0.),
            inner.margins=c(0.025, 0.025,0.025 , 0.025),
            #outer.margins = c(0.02, -0.3, 0, -0.5),
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            earth.boundary = TRUE,frame = FALSE)

Malawi_Wheat <- tm_shape(Malawi)+
  #tm_polygons("white")+
  tm_borders("grey20", lwd = .7) +
  tm_shape(mask(crop(nClassWheat,Malawi),Malawi),is.master = FALSE) + 
  tm_raster(style = "fixed", n = 10, title = "N Classification, Maize",
            breaks =  c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
            palette = c("#d7301f","#fc8d59","#fdcc8a","#fef0d9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4", "#225ea8", "#253494"),
            labels = c("Surplus N, Low NUE, Low removal gap", "Surplus N, Low NUE, High removal gap","Surplus N, Med NUE, Low removal gap", "Surplus N, Med NUE, High removal gap", "Deficit N, Low NUE, Low removal gap", "Deficit N, Low NUE, High removal gap","Deficit N, Med NUE,Low removal gap", "Deficit N, Med NUE, High removal gap", "Deficit N, N mining, Low removal gap", "Deficit N, N mining, High removal gap"))+
  #legend.hist = T,
  #legend.hist.z  = NA)+
  tm_legend(outside = T, hist.width = 4)+
  tm_layout(legend.show = FALSE,
            legend.title.fontface = "bold",
            legend.title.size = 0.7,
            legend.text.size = 0.5,
            #inner.margins=c(0, 0, 0.17, 0.),
            inner.margins=c(0.025, 0.025,0.025 , 0.025),
            #outer.margins = c(0.02, -0.3, 0, -0.5),
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            earth.boundary = TRUE,frame = TRUE)

Malawi_Maize <- tm_shape(Malawi)+
  #tm_polygons("white")+
  tm_borders("grey20", lwd = .7) +
  tm_shape(mask(crop(nClassMaize,Malawi),Malawi),is.master = FALSE) + 
  tm_raster(style = "fixed", n = 10, title = "N Classification, Maize",
            breaks =  c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
            palette = c("#d7301f","#fc8d59","#fdcc8a","#fef0d9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4", "#225ea8", "#253494"),
            labels = c("Surplus N, Low NUE, Low removal gap", "Surplus N, Low NUE, High removal gap","Surplus N, Med NUE, Low removal gap", "Surplus N, Med NUE, High removal gap", "Deficit N, Low NUE, Low removal gap", "Deficit N, Low NUE, High removal gap","Deficit N, Med NUE,Low removal gap", "Deficit N, Med NUE, High removal gap", "Deficit N, N mining, Low removal gap", "Deficit N, N mining, High removal gap"))+
  #legend.hist = T,
  #legend.hist.z  = NA)+
  tm_legend(outside = T, hist.width = 4)+
  tm_layout(legend.show = FALSE,
            legend.title.fontface = "bold",
            legend.title.size = 0.7,
            legend.text.size = 0.5,
            #inner.margins=c(0, 0, 0.17, 0.),
            inner.margins=c(0.025, 0.025,0.025 , 0.025),
            #outer.margins = c(0.02, -0.3, 0, -0.5),
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            earth.boundary = TRUE,frame = TRUE)

#Ethiopia
Ethiopia_Wheat <- tm_shape(Ethiopia)+
  #tm_polygons("white")+
  tm_borders("grey20", lwd = .7) +
  tm_shape(mask(crop(nClassWheat,Ethiopia),Ethiopia),is.master = FALSE) + 
  tm_raster(style = "fixed", n = 10, title = "N Classification, Maize",
            breaks =  c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
            palette = c("#d7301f","#fc8d59","#fdcc8a","#fef0d9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4", "#225ea8", "#253494"),
            labels = c("Surplus N, Low NUE, Low removal gap", "Surplus N, Low NUE, High removal gap","Surplus N, Med NUE, Low removal gap", "Surplus N, Med NUE, High removal gap", "Deficit N, Low NUE, Low removal gap", "Deficit N, Low NUE, High removal gap","Deficit N, Med NUE,Low removal gap", "Deficit N, Med NUE, High removal gap", "Deficit N, N mining, Low removal gap", "Deficit N, N mining, High removal gap"))+
  #legend.hist = T,
  #legend.hist.z  = NA)+
  tm_legend(outside = T, hist.width = 4)+
  tm_layout(legend.show = FALSE,
            legend.title.fontface = "bold",
            legend.title.size = 0.7,
            legend.text.size = 0.5,
            #inner.margins=c(0, 0, 0.17, 0.),
            inner.margins=c(0.025, 0.025,0.025 , 0.025),
            #outer.margins = c(0.02, -0.3, 0, -0.5),
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            earth.boundary = TRUE,frame = TRUE)

Ethiopia_Maize <- tm_shape(Ethiopia)+
  #tm_polygons("white")+
  tm_borders("grey20", lwd = .7) +
  tm_shape(mask(crop(nClassMaize,Ethiopia),Ethiopia),is.master = FALSE) + 
  tm_raster(style = "fixed", n = 10,
            breaks =  c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
            palette = c("#d7301f","#fc8d59","#fdcc8a","#fef0d9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4", "#225ea8", "#253494"),
            labels = c("Surplus N, Low NUE, Low removal gap", "Surplus N, Low NUE, High removal gap","Surplus N, Med NUE, Low removal gap", "Surplus N, Med NUE, High removal gap", "Deficit N, Low NUE, Low removal gap", "Deficit N, Low NUE, High removal gap","Deficit N, Med NUE,Low removal gap", "Deficit N, Med NUE, High removal gap", "Deficit N, N mining, Low removal gap", "Deficit N, N mining, High removal gap"))+
  #legend.hist = T,
  #legend.hist.z  = NA)+
  tm_legend(outside = T, hist.width = 4)+
  tm_layout(legend.show = FALSE,
            legend.title.fontface = "bold",
            legend.title.size = 0.7,
            legend.text.size = 0.5,
            #inner.margins=c(0, 0, 0.17, 0.),
            inner.margins=c(0.025, 0.025,0.025 , 0.025),
            #outer.margins = c(0.02, -0.3, 0, -0.5),
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            earth.boundary = TRUE,frame = TRUE)

#Create standalone legends
legend.map1 <- tm_shape(mask(crop(nClassMaize,Ethiopia),Ethiopia)) + 
                         tm_raster(style = "fixed", n = 10,title = "",
                                   breaks =  c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
                                   palette = c("#d7301f","#fc8d59","#fdcc8a","#fef0d9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4", "#225ea8", "#253494"),
                                   labels = c("Surplus N, Low NUE, Low removal gap", "Surplus N, Low NUE, High removal gap","Surplus N, Med NUE, Low removal gap", "Surplus N, Med NUE, High removal gap", "Deficit N, Low NUE, Low removal gap", "Deficit N, Low NUE, High removal gap","Deficit N, Med NUE,Low removal gap", "Deficit N, Med NUE, High removal gap", "Deficit N, N mining, Low removal gap", "Deficit N, N mining, High removal gap"))+
                        tm_layout(#legend.title.fontface = "bold",
                                  #legend.title.size = 0.8,
                                  legend.text.size = 0.8,legend.only = TRUE)+
                        tm_legend(legend.position = c(0,0.6))

legend.map2 <- tm_shape(mask(crop(nClassMaize,Ethiopia),Ethiopia)) + 
  tm_raster(style = "fixed", n = 10,title = "",
            breaks =  c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
            palette = c("#d7301f","#fc8d59","#fdcc8a","#fef0d9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4", "#225ea8", "#253494"),
            labels = c("Surplus N, Low NUE, Low removal gap", "Surplus N, Low NUE, High removal gap","Surplus N, Med NUE, Low removal gap", "Surplus N, Med NUE, High removal gap", "Deficit N, Low NUE, Low removal gap", "Deficit N, Low NUE, High removal gap","Deficit N, Med NUE,Low removal gap", "Deficit N, Med NUE, High removal gap", "Deficit N, N mining, Low removal gap", "Deficit N, N mining, High removal gap"))+
  tm_layout(legend.title.fontface = "bold",
            #legend.title.size = 0.9,
            legend.text.size = 0.8,legend.only = TRUE)+
  tm_legend(outside = FALSE,
            legend.outside.position =  "bottom",
            legend.position = c(0,0.2),
            #position = c("LEFT", "center"),
            frame = TRUE,
            bg.color=NA)
#Arrange maps
Maize_nClass <- tmap_arrange(India_Maize, Malawi_Maize, Ethiopia_Maize ,legend.map1,
             ncol = 2, nrow = 2,
             #outer.margins = c(0.02, -0.3, 0, -0.5),
             outer.margins = c(0.01, 0.01, 0.01, 0.01))

Wheat_nClass <- tmap_arrange(India_Wheat, Malawi_Wheat, Ethiopia_Wheat ,legend.map1,
                             ncol = 2, nrow = 2,
                             #outer.margins = c(0.02, -0.3, 0, -0.5),
                             outer.margins = c(0.01, 0.01, 0.01, 0.01))

Rice_nClass <- tmap_arrange(India_Rice,legend.map2,Malawi_Rice ,
             ncol = 3, nrow = 1,widths = c(1,0.5, 0.5),heights = c(1,0.5,0.75))

#Export maps
tmap_save(Maize_nClass,
          filename = "Plots/Maize_nClass.jpg",
          width = 8.8, height = 8.8, units = "in",
          dpi = 1200)
tmap_save(Wheat_nClass,
          filename = "Plots/Wheat_nClass.jpg",
          width = 8.8, height = 8.8, units = "in",
          dpi = 1200)
tmap_save(Rice_nClass,
          filename = "Plots/Rice_nClass.jpg",
          width = 8.8, height = 6.5, units = "in",
          dpi = 1200)
