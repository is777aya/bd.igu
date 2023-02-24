library(tidyverse)
library(dplyr)

# filtrar  por cultivo & departamento
cania <- agricola2013_2021 %>%
            filter(producto == 'Soya' & desc_departamento == 'Santa Cruz') 

#agrupar por municipios: produccion, superfice 
ganado <-data.frame(group_by(pecuario_2013_2021,  desc_municipio) %>% 
                             summarise(
                               cod_mun = max(cod_mun),
                               Prod2013 = sum(prod2013),
                               Sup2013 = sum(sup2013),
                               Prod2014 = sum(prod2014),
                               Sup2014 = sum(sup2014),
                               Prod2015 = sum(prod2015),
                               Sup2015 = sum(sup2015),
                               Prod2016 = sum(prod2016),
                               Sup2016 = sum(sup2016),
                               Prod2017 = sum(prod2017),
                               Sup2017 = sum(sup2017),
                               Prod2018 = sum(prod2018),
                               Sup2018 = sum(sup2018),
                               Prod2019 = sum(prod2019),
                               Sup2019 = sum(sup2019),
                               Prod2020 = sum(prod2020),
                               Sup2020 = sum(sup2020),
                               Prod2021 = sum(prod2021),
                               Sup2021 = sum(sup2021)
                                      ) %>%  
                             arrange(desc(Prod2021))
                           )



# filtrar  por ganado & departamento & gestion
pollo <- pecuario_2013_2021 %>%
  filter(ganado == 'aves granja' & desc_departamento == 'Potosi') 

#agrupar por municipios y comunidades: produccion, superfice 
papa.mun.com.cbba <-data.frame(group_by(papa.cbba,  desc_municipio) %>% 
                             summarise(
                               cod_mun = max(codigo_municipal),
                               count=n(),
                               Prod2013 = sum(prod2013),
                               Sup2013 = sum(sup2013),
                               Prod2014 = sum(prod2014),
                               Sup2014 = sum(sup2014),
                               Prod2015 = sum(prod2015),
                               Sup2015 = sum(sup2015),
                               Prod2016 = sum(prod2016),
                               Sup2016 = sum(sup2016),
                               Prod2017 = sum(prod2017),
                               Sup2017 = sum(sup2017),
                               Prod2018 = sum(prod2018),
                               Sup2018 = sum(sup2018),
                               Prod2019 = sum(prod2019),
                               Sup2019 = sum(sup2019)
                             ) %>%  
                             arrange(desc(Prod2019))
                          )


# filtrar  por UPAs piscicolas
pescado.cbba <- bd_piscicola %>%
  filter(desc_depar == 'Cochabamba') %>%
  arrange(desc(UPAs))

cania <-read.table("clipboard", sep = "\t" , header = T)

#exportar a hoja Excel
library(xlsx)

write.xlsx(papa.cbba, "papa_cbba.xlsx")


#rankin municipal * produccion (diez primeras filas)
rank.arroz.mun.bn <- data.frame(cania[1:10,])  # filas
#papa.mun.cbba[, 3:4]        # columnas 

#grafico barras rankin 10 municipios productores
library(esquisse)
library(ggplot2)

options(scipen=999, digits=2)

ggplot(rank.papa.mun.cbba) +
  aes(x = reorder(desc_municipio,Prod2019) , weight = Prod2019) + 
  geom_bar(aes(color="Produccion")) +
  geom_text(aes(x = desc_municipio, y = Prod2019, label = round(Prod2019, digits = 0), hjust = 1.1), color = "white", size=4) +
  geom_line(aes(y=Sup2019, color="Superficie"), group = 1, size=2) + 
  geom_text_repel(aes(x = desc_municipio, y = Sup2019, label = round(Sup2019, digits = 0), hjust = 1.5), color = "green",  size=4) +
  coord_flip() +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank()
    #axis.title.y=element_blank(), 
    #       axis.text.y=element_blank(), 
    #      axis.ticks.y=element_blank()
  ) +
  
  scale_y_continuous(trans='log10') +
  #scale_y_continuous(trans='identity')
  
  labs(x = "  ")


ggplot(rank.arroz.mun.bn, aes(x = reorder(desc_municipio,prod2021), y = prod2021)) +
  geom_col(aes(fill = factor(prod2021))) +
  geom_text(aes(label = round(prod2021, digits = 0), hjust = 0), color = "black", size=5) +
  #geom_line(aes(y=sup2021), group = 1, size=2, col="red") + 
  #geom_text_repel(aes(y = Sup2019, label = round(Sup2019, digits = 0), hjust = 0), color = "black", bg.color = "white", size=4) +
  coord_flip() +
  labs(title = "Rankin municipios productores (En Toneladas") +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position='none'
  ) +
  #scale_y_continuous(trans='log10') + 
  scale_fill_viridis_d() 

#llamar shp - municipios
library(sp)
library(sf)
library(rgdal)

lim_mun <- st_read("2_1_Límite Municipal.shp")


#filtrar municipios por departamento
filter.mun <-lim_mun %>%
                filter(DEPARTAMEN == "Tarija")

shp.filter <- st_write(filter.mun, "18_mun_tj.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)


#unir tabla geografica Y tabla tabular
join_bn_mun_arroz <- merge(shp.filter, rank.arroz.mun.bn, by.x = "cod_mun", by.y = "cod_mun")

#unir tabla geografica Y tabla tabular PISCICOLA
join_cbba_mun_pez <- merge(shp.filter, pescado.bn, by.x = "cod_mun", by.y = "cod_mun")


#guardar nuevo shape
st_write(join_bn_mun_arroz, "35_mun_maiz_tj.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE)

#plotear produccion por municipio
library(ggplot2)
library(ggspatial)
library(gridExtra)
library(viridisLite)
library(viridis)
library(ggrepel)
library(pacman)
library(maps)

pacman::p_load(raster, gridExtra, grid, lattice, rgdal, ggpubr, ggspatial, rgeos, stringr, ggrepel, hrbrthemes, ghibli, colorspace, gtools, sf, fs, glue, tidyverse)


#llamar a los muncipios seleccionados
arroz.bn <-st_read("35_mun_maiz_tj.shp") 

m1 <-ggplot(arroz.bn) +
  geom_sf(aes(fill = prd2013), size = 0.0000000000001, show.legend = FALSE) +
  geom_sf(data = shp.filter, size=0.7, color='#626567', alpha=0.01) +
  scale_fill_viridis_c(option = "viridis", trans="log") +
  labs(title = "Año: 2013 ", fill = "Prod. (Tn)") +
  theme_void()

g1.1 <-ggplot(rank.arroz.mun.bn, aes(x = reorder(desc_municipio,prod2013), y = prod2013)) +
  geom_col(aes(fill = factor(prod2013))) +
  geom_label(aes(label = round(prod2013, digits = 0), hjust = 1), color = "black", size=3.5, nudge_y = 80, alpha = 0.5) +
  coord_flip() +
  labs(title = "Rank Prod. (Tn)") +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position='none',
    plot.title = element_text(color="red", size=10, face="bold.italic"),
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(trans = "sqrt")

m2 <-ggplot(arroz.bn) +
  geom_sf(aes(fill = prd2014), size = 0.0000000000001, show.legend = FALSE) +
  geom_sf(data = shp.filter, color='#626567', size=0.7, alpha=0.01) +
  scale_fill_viridis_c(option = "viridis", trans="log") +
  labs(title = "Año: 2014 ", fill = "Prod. (Tn)") +
  theme_void()

g2.1 <-ggplot(rank.arroz.mun.bn, aes(x = reorder(desc_municipio,prod2014), y = prod2014)) +
  geom_col(aes(fill = factor(prod2014))) +
  geom_label(aes(label = round(prod2014, digits = 0), hjust = 1), color = "black", size=3.5, nudge_y = 80, alpha = 0.5) +
  coord_flip() +
  labs(title = "Rank Prod. (Tn)") +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position='none',
    plot.title = element_text(color="red", size=10, face="bold.italic"),
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(trans = "sqrt")

m3 <-ggplot(arroz.bn) +
  geom_sf(aes(fill = prd2015), size = 0.0000000000001, show.legend = FALSE) +
  geom_sf(data = shp.filter, color='#626567', size=0.7, alpha=0.01) +
  scale_fill_viridis_c(option = "viridis", trans="log") +
  labs(title = "Año: 2015 ", fill = "Prod. (Tm)") +
  theme_void()

g3.1 <-ggplot(rank.arroz.mun.bn, aes(x = reorder(desc_municipio,prod2015), y = prod2015)) +
  geom_col(aes(fill = factor(prod2015))) +
  geom_label(aes(label = round(prod2015, digits = 0), hjust = 1), color = "black", size=3.5, nudge_y = 80, alpha = 0.5) +
  coord_flip() +
  labs(title = "Rank Prod. (Tn)") +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position='none',
    plot.title = element_text(color="red", size=10, face="bold.italic"),
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(trans = "sqrt")

m4 <-ggplot(arroz.bn) +
  geom_sf(aes(fill = prd2016), size = 0.0000000000001, show.legend = FALSE) +
  geom_sf(data = shp.filter, color='#626567', size=0.7, alpha=0.01) +
  scale_fill_viridis_c(option = "viridis", trans="log") +
  labs(title = "Año: 2016 ", fill = "Prod. (Tm)") +
  theme_void()

g4.1 <-ggplot(rank.arroz.mun.bn, aes(x = reorder(desc_municipio,prod2016), y = prod2016)) +
  geom_col(aes(fill = factor(prod2016))) +
  geom_label(aes(label = round(prod2016, digits = 0), hjust = 1), color = "black", size=3.5, nudge_y = 80, alpha = 0.5) +
  coord_flip() +
  labs(title = "Rank Prod. (Tn)") +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position='none',
    plot.title = element_text(color="red", size=10, face="bold.italic"),
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(trans = "sqrt")

m5 <-ggplot(arroz.bn) +
  geom_sf(aes(fill = prd2017), size = 0.0000000000001, show.legend = FALSE) +
  geom_sf(data = shp.filter, color='#626567', size=0.7, alpha=0.01) +
  scale_fill_viridis_c(option = "viridis", trans="log") +
  labs(title = "Año: 2017 ", fill = "Prod. (Tm)") +
  theme_void()

g5.1 <-ggplot(rank.arroz.mun.bn, aes(x = reorder(desc_municipio,prod2017), y = prod2017)) +
  geom_col(aes(fill = factor(prod2017))) +
  geom_label(aes(label = round(prod2017, digits = 0), hjust = 1), color = "black", size=3.5, nudge_y = 80, alpha = 0.5) +
  coord_flip() +
  labs(title = "Rank Prod. (Tn)") +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position='none',
    plot.title = element_text(color="red", size=10, face="bold.italic"),
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(trans = "sqrt")

m6 <-ggplot(arroz.bn) +
  geom_sf(aes(fill = prd2018), size = 0.0000000000001, show.legend = FALSE) +
  geom_sf(data = shp.filter, color='#626567', size=0.7, alpha=0.01) +
  scale_fill_viridis_c(option = "viridis", trans="log") +
  labs(title = "Año: 2018 ", fill = "Prod. (Tm)") +
  theme_void()

g6.1 <-ggplot(rank.arroz.mun.bn, aes(x = reorder(desc_municipio,prod2018), y = prod2018)) +
  geom_col(aes(fill = factor(prod2018))) +
  geom_label(aes(label = round(prod2018, digits = 0), hjust = 1), color = "black", size=3.5, nudge_y = 80, alpha = 0.5) +
  coord_flip() +
  labs(title = "Rank Prod. (Tn)") +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position='none',
    plot.title = element_text(color="red", size=10, face="bold.italic"),
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(trans = "sqrt")

m7 <-ggplot(arroz.bn) +
  geom_sf(aes(fill = prd2019), size = 0.0000000000001, show.legend = FALSE) +
  geom_sf(data = shp.filter, color='#626567', size=0.7, alpha=0.01) +
  scale_fill_viridis_c(option = "D", trans="log") +
  labs(title = "Año: 2019 ", fill = "Prod. (Tm)") +
  theme_void()

g7.1 <-ggplot(rank.arroz.mun.bn, aes(x = reorder(desc_municipio,prod2019), y = prod2019)) +
  geom_col(aes(fill = factor(prod2019))) +
  geom_label(aes(label = round(prod2019, digits = 0), hjust = 1), color = "black", size=3.5, nudge_y = 80, alpha = 0.5) +
  coord_flip() +
  labs(title = "Rank Prod. (Tn)") +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position='none',
    plot.title = element_text(color="red", size=10, face="bold.italic"),
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(trans = "sqrt")

m8 <-ggplot(arroz.bn) +
  geom_sf(aes(fill = prd2020), size = 0.0000000000001, show.legend = FALSE) +
  geom_sf(data = shp.filter, color='#626567', size=0.7, alpha=0.01) +
  scale_fill_viridis_c(option = "D", trans="log") +
  labs(title = "Año: 2020 ", fill = "Prod. (Tm)") +
  theme_void()

g8.1 <-ggplot(rank.arroz.mun.bn, aes(x = reorder(desc_municipio,prod2020), y = prod2020)) +
  geom_col(aes(fill = factor(prod2020))) +
  geom_label(aes(label = round(prod2020, digits = 0), hjust = 1), color = "black", size=3.5, nudge_y = 80, alpha = 0.5) +
  coord_flip() +
  labs(title = "Rank Prod. (Tn)") +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position='none',
    plot.title = element_text(color="red", size=10, face="bold.italic"),
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(trans = "sqrt")

m9 <-ggplot(arroz.bn) +
  geom_sf(aes(fill = prd2021), size = 0.0000000000001, show.legend = FALSE) +
  #geom_text_repel(aes(x=long, y=lat, label=dsc_mnc), point.padding = 0.5, min.segment.length = 1, box.padding = 0.1, color = "white", bg.color = "black", bg.r = 0.15, size=2.8) +
  geom_sf(data = shp.filter, color='#626567', size=0.7, alpha=0.01) +
  scale_fill_viridis_c(option = "D", trans="sqrt") +
  labs(title = "Año: 2021 ", fill = "Prod. (Tm)") +
  theme_void()

g9.1 <-ggplot(rank.arroz.mun.bn, aes(x = reorder(desc_municipio,prod2021), y = prod2021)) +
  geom_col(aes(fill = factor(prod2021))) +
  geom_label(aes(label = round(prod2021, digits = 0), hjust = 1), color = "black", size=5, nudge_y = 500, label = format(rank.arroz.mun.bn$prod2021, big.mark = " ")) +
  coord_flip() +
  labs(title = "Rank Prod. (Tn)") +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(),
    axis.text.y=element_text(size=12),  ###cambiar tamanio texto eje y
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position='none',
    plot.title = element_text(color="red", size=10, face="bold.italic"),
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(trans = "sqrt")

####################################
#################################### GRAFICO PECUARIO ULTIMO YEAR
####################################

m10 <-ggplot(arroz.bn) +
  geom_sf(aes(fill = cab2021), size = 0.0000000000001, show.legend = FALSE) +
  #geom_text_repel(aes(x=long, y=lat, label=dsc_mnc), point.padding = 0.5, min.segment.length = 1, box.padding = 0.1, color = "white", bg.color = "black", bg.r = 0.15, size=2.8) +
  geom_sf(data = shp.filter, color='#626567', size=0.7, alpha=0.01) +
  scale_fill_viridis_c(option = "E", trans="sqrt") +
  labs(title = "Año: 2021 ", fill = "No. Cabezas") +
  theme_void()

g10.1 <-ggplot(rank.arroz.mun.bn, aes(x = reorder(desc_municipio,cab2021), y = cab2021)) +
  geom_col(aes(fill = factor(cab2021))) +
  geom_label(aes(label = round(cab2021, digits = 0), hjust = 1), color = "black", size=5, nudge_y = 500, label = format(rank.arroz.mun.bn$cab2021, big.mark = " ")) +
  coord_flip() +
  labs(title = "Rank No. Cabezas") +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(),
    axis.text.y=element_text(size=12),  ###cambiar tamanio texto eje y
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position='none',
    plot.title = element_text(color="red", size=10, face="bold.italic"),
  ) +
  scale_fill_viridis_d(option = "E") +
  scale_y_continuous(trans = "sqrt")


####################################
#################################### GRAFICO PISCICOLA ULTIMO YEAR
####################################

m11 <-ggplot(arroz.bn) +
  geom_sf(aes(fill = UPAs), size = 0.0000000000001, show.legend = FALSE) +
  #geom_text_repel(aes(x=long, y=lat, label=dsc_mnc), point.padding = 0.5, min.segment.length = 1, box.padding = 0.1, color = "white", bg.color = "black", bg.r = 0.15, size=2.8) +
  geom_sf(data = shp.filter, color='#626567', size=0.7, alpha=0.01) +
  scale_fill_viridis_c(option = "G", trans="identity") +
  labs(title = "Año: 2021 ", fill = "UPA's") +
  theme_void()

g11.1 <-ggplot(pescado.bn, aes(x = reorder(desc_munic,UPAs), y = UPAs)) +
  geom_col(aes(fill = factor(UPAs))) +
  geom_label(aes(label = round(UPAs), hjust = 1), color = "black", size=5, nudge_y = 80, label = format(pescado.bn$UPAs, big.mark = " ")) +
  coord_flip() +
  labs(title = "Rank Municipios (UPA's)") +
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(),
    axis.text.y=element_text(size=12),  ###cambiar tamanio texto eje y
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position='none',
    plot.title = element_text(color="black", size=10, face="bold.italic"),
  ) +
  scale_fill_viridis_d(option = "G") +
  scale_y_continuous(trans = "identity")


###############plotear para toda la serie de mapas 

#grid.arrange(m1, g1.1, m2, g2.1, m3, g3.1, m4, g4.1, m5, g5.1, m6, g6.1, m7, g7.1, m8, g8.1, m9, g9.1)

layout <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18), nrow = 3, ncol = 6, byrow = TRUE)

all.maps <- grid.arrange(m1, g1.1, m2, g2.1, m3, g3.1, m4, g4.1, m5, g5.1, m6, g6.1, m7, g7.1, m8, g8.1, m9, g9.1, layout_matrix = layout)

ggsave(plot = all.maps, filename = '../mapa/soya_beni_2013_2021.png', units = 'cm', width = 40, height = 20, dpi = 950) 

###############plotear para toda la ultima de serie de mapa AGRICOLA y PECUARIO
layout <- matrix(c(1, 2), nrow = 1, ncol = 2, byrow = TRUE)

all.maps <- grid.arrange(m9, g9.1, layout_matrix = layout)

ggsave(plot = all.maps, filename = '../mapa/maiz.tj.2021.png', units = 'cm', width = 20, height = 20, dpi = 900)  #AGRICOLA width = 20, height = 20

################################################################################################################


GraficoCultivos <- 
  ggplot(subset(agricola2013_2021_vertical, producto == 'Caña de azúcar' & desc_departamento == 'Beni' & gestion == '2021'), 
         aes(x=reorder(desc_municipio,-prod), y=prod, fill=desc_municipio)) +
  geom_bar(stat="identity") +
  theme_bw() +
  labs(title="Superficie en España de Cereales",
       x="Cultivo",
       y="Superficie (Millones ha)") +
  theme(text = element_text(size=12), # Tamaño de fuente del gráfico por defecto
        plot.title = element_text(size=rel(2), # Tamaño del título, doble del establecido por defecto
                                  vjust=2, #Justificación vertical, para separarlo del gráfico
                                  face="bold", #Letra negrilla
                                  color="darkgreen", #Color del texto
                                  lineheight=1.5), #Separación entre líneas)
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.title.x = element_text(face="bold", vjust=1.5, colour="darkgreen", size=rel(1.5)),
        axis.title.y = element_text(face="bold", vjust=1.5, colour="darkgreen", size=rel(1.5)),
        axis.text = element_text(colour = "black")) #+
  #coord_cartesian(ylim=c(0, 3)) +
  #scale_y_continuous(breaks =seq(0, 3, 0.5))

GraficoCultivos  


######################
###################### CODIGO PARA CREAR DATA FRAME, FILTRAR y GUARDAR EN .SHP
######################

#filtrar cultivo y gestion de la base de datos por comunidad Vertical
filter.cultivo <- subset(bd_agricola_comunidad_2013_2021_vertical,  
                         desc_departamento == 'Beni' & gestion == '2021') %>%  
  arrange(desc(prod))

#seleccionar las coordenadas de la bd
coords <- filter.cultivo %>% select(long, lat)

#establecer el sist de referencia y o proyeccion
prj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#convertir el data frame a estrucura espacial
shape <- st_as_sf(filter.cultivo, coords = c("long", "lat"), 
                 crs = prj)

#guardar en formato shape
st_write(shape, "potosi.shp", layer_options = "ENCODING=UTF-8", driver = "ESRI Shapefile")

                     