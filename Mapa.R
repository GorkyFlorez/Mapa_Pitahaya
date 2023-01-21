#------------------------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, rnaturalearth, rnaturalearthdata, sf, 
               raster, reticulate,  maptools,maps, ggpubr, gridExtra,
               ggplot2, ggspatial,rgeos, tmap,grid, rgbif, 
               rgrass7, sp, mapr, rgdal, RColorBrewer, cowplot)

library(png)
library(grid)
library(ggimage)
#------------------------------------------------------------------------
Sur_America     <- st_read ("SHP/SurAmerica.geojson")  
SurAmerica_utm  <- st_transform(Sur_America ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Per   <- getData('GADM', country='Peru', level=1) %>% st_as_sf() 
Peru   <- getData('GADM', country='Peru', level=3) %>% st_as_sf() 
Amazonas  =  subset(Peru , NAME_1 == "Amazonas")

sites <- data.frame(longitude = c(-77.9166666666666,-77.9166666666666, -77.9780333333333, -77.9777, -77.9683833333333,-77.92835, -77.51965  ),
                    latitude = c(-6.033333333,-6.03333333333333, -5.9809,-5.98075, -5.9086667, -6.0592167,-6.4241167  ),
                    Ecotipos= c("Ecotipo 1", "Ecotipo 2", "Ecotipo 3","Ecotipo 4", "Ecotipo 5","Ecotipo 6", "Ecotipo 7"),
                    Ecotipos= c("Amarilla", "Rojo", "Amarilla","Rojo", "Amarilla","Rojo", "Amarilla"),
                    SPP= c("PNG/PITAHAYA AMARILLO.png", "PNG/PITAHAYA ROJO.png","PNG/PITAHAYA AMARILLO.png", "PNG/PITAHAYA ROJO.png","PNG/PITAHAYA AMARILLO.png", "PNG/PITAHAYA ROJO.png","PNG/PITAHAYA AMARILLO.png"))

ROJO <- readPNG("PNG/PITAHAYA ROJO.png", FALSE)
ROJO_gg<- rasterGrob(ROJO, x = unit(0.2, "npc"),y = unit(0.15, "npc"), width = unit(0.1, "npc"))


AMARILLO <- readPNG("PNG/PITAHAYA AMARILLO.png", FALSE)
AMARILLO_gg<- rasterGrob(AMARILLO, x = unit(0.1, "npc"),y = unit(0.1, "npc"), width = unit(0.1, "npc"))


my_bbox <- c(xmin = min(-78.2), xmax = max(-77.3),  ymin = min(-6.6),  ymax = max(-5.8))
my_bbox.m <- matrix(c(my_bbox['xmin'], my_bbox['xmin'], my_bbox['xmax'], my_bbox['xmax'], my_bbox['xmin'],  my_bbox['ymax'], my_bbox['ymin'], my_bbox['ymin'], my_bbox['ymax'], my_bbox['ymax']), ncol = 2)

my_bbox.sf <- st_geometry(st_polygon(x = list(my_bbox.m)))
st_crs(my_bbox.sf) <- 4326

Zona<- st_transform(my_bbox.sf ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
write_sf(Zona, "SHP/Zona.shp")
Zon= st_read("SHP/Zona.shp")  %>% st_as_sf()
Zona <- st_transform(Zon ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

SurA= ggplot()+
  geom_sf(data = SurAmerica_utm , fill="white", color="black", size=0.01)+
  geom_sf(data = Per , fill="gray", color="black", size=0.05)+
  geom_sf(data = Amazonas, fill="black", color="black", size=0.01)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 12, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
             annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
                      label = "Pacific ocean",size = 3, family="serif", color = 
                        "black",  fontface="italic", angle=90)+
                        annotate(geom = "text", x = -60, y = -50, hjust = 0, vjust = 1, 
                                 label = "Atlantic ocean",size = 3, family="serif", color = 
                                   "black",  fontface="italic")+
                                   annotate(geom = "text", x = -70, y = -4, hjust = 0, vjust = 1, 
                                            label = "Peru",size = 3, family="serif", color = 
                                              "black",  fontface="italic")
                                            
SurA

Zona_box = st_as_sfc(st_bbox(Zona ))

MDD_map= ggplot()+
  geom_sf(data = SurAmerica_utm, fill="white", color="black", size=0.01)+

  geom_sf(data = Per, fill="black", color="black", size=0.01, alpha=0.3)+
  geom_sf_text(data = Per, aes(label=NAME_1), size=2)+
  geom_sf(data = Zona_box , fill=NA, size=1, color="black")+
  geom_point(data = sites, aes(x=longitude, y=latitude))+
  coord_sf(xlim = c(-79.2, -76.5), ylim = c(-6.987491 ,-2.984379)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -79, y = -4, hjust = 0, vjust = 1, angle=45,
           label = "Ecuador",size = 3, family="serif", color = 
             "black",  fontface="italic")+
             annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")
           


library(elevatr)
elev = get_elev_raster(Zona  , z=12)
plot(elev)
Poligo_alt    <- crop(elev, Zona )                           #
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Zona )
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope")
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")
cortes <- c(200, 500,1000,2000,3000,4000,5000, 6500)


Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")


library(ggnewscale)

library(ggrepel)
Mapa= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores ,
                       breaks = cortes ,
                       na.value = 'white',
                       labels = c("[menor a - 270] ","[270 - 499]", "[500 - 999]", "[1000 - 1999]", "[2000 - 2999]",
                                  "[3000 - 3999]", "[4000 - 4999]", "[5000 -6500]"),
                       name='Elevacion \n(msnm)')+
  geom_sf(data = Peru, fill=NA, color="black", size=1 )+
  geom_sf_text(data = Peru, aes(label=NAME_3), size=2)+
  geom_image(data = sites, aes( x=longitude, y = latitude, image = SPP), size = 0.02)+
  #geom_point(data = sites, aes(x=longitude, y=latitude))+
  coord_sf(xlim = c(-78.1,-77.45), ylim = c(-6.5,-5.85))+
  annotation_custom(AMARILLO_gg)+
  annotation_custom(ROJO_gg)+
  annotate(geom = "text", x = -78.08, y = -6.52, label = "Amarillo", 
           family="serif", color = "black", size = 4, fontface = "bold")+
  annotate(geom = "text", x = -77.97, y = -6.52, label = "Rojo", 
           family="serif", color = "black", size = 4, fontface = "bold")+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme(
        
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text( family="Anton",color ="#012D5A",face='bold',hjust=0.5,
                                   size = 14),
        plot.subtitle = element_text(size = 11,  face = "italic", family="serif"),
        plot.caption = element_text(size = 9, family="serif", face = "italic"),
        
        plot.background = element_rect(fill = "white", color = NA),
        
        legend.position = c(0.25,0.85),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"),
        
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(x = 'Longitud', y = 'Latitud',  title="Mapa de distribución de ecotipos de pitahaya (Hylocereus spp) en Amazonas")+
  guides(fill = guide_legend(
    title = " Deforestación ",
    direction = "horizontal",
    nrow = 1,
    keywidth = 1.75,
    keyheight = 0.5,
    label.position = "bottom",
    title.position = "right",
    override.aes = list(alpha = 1)
  ))+
  geom_label_repel(data = sites, aes(x = longitude, y =latitude, label = Ecotipos), 
                   family="serif", box.padding = unit(0.9, "lines"), size =2, face = "bold",color = 'black',
                   point.padding = unit(0.5, "lines"))

library(ggpubr)
legend <- get_legend(Mapa)

Mapa_final= Mapa +  theme(legend.position = "nene")


library(cowplot)
Expo = ggdraw() +
  coord_equal(xlim = c(0, 25), ylim = c(0, 25), expand = FALSE) +
  
  
  draw_plot(Mapa_final , width = 25, height = 25,x = 0, y = 0)+
  draw_plot(legend , width = 5, height = 5,x = 7, y = 19)+
  draw_plot(SurA , width = 6, height = 6,x = 19.4, y = 17.8)+
  draw_plot(MDD_map , width = 6, height = 6,x = 19.4, y = 11.8)



ggsave(plot=Expo ,"Mapa de pitahaya.png",units = "cm",width = 25, #alto
       height = 25, #ancho
       dpi=1200)
































