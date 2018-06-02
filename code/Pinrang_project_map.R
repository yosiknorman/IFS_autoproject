#!/usr/bin/Rscript
library(ncdf4)
library(rasterVis)
library(rgdal)



setwd("~/cuda-workspace/transmet/Code/")
rm(list = ls())
nc = nc_open("../Data/A_YXER09ECMA080000_C_ECMF_20171208000000_D1D12080000120809001.nc")
names(nc$var)
lat = rev(ncvar_get(nc,"g0_lat_0"))
lon = ncvar_get(nc,"g0_lon_1")

TP = ncvar_get(nc,"TP_GDS0_SFC")
TP_backup = TP
TP = t(apply(t(TP), 2, FUN = rev))

# backToRaster = apply(t(TP1), 2, FUN = rev)

TP = TP*1000  #(meter to milimeter)
Sgn = quantile(TP, 0.99)  # Using Significance of 0.01
TP[TP > Sgn] = Sgn
cc = colorRampPalette(colors = c(NA, "white", "red", "purple"))
# image(lon, y = lat, TP,col = cc(30))
# Filterspasial

ix_new = which(lon > 118 & lon < 124)
iy_new = which(lat > -8 & lat < -1)
TP_new = TP[ix_new, iy_new]
x_new = lon[ix_new]
y_new = lat[iy_new]


# maps::map("world",add = T)
# Rasterize
backToRaster = apply(t(TP_new), 2, FUN = rev)
# e = extent(c(xmn = range(x_new)[1],xmx = range(x_new)[2], ymn = range(y_new)[1],ymx = range(y_new)[2]))
TPR = raster(backToRaster)
extent(TPR) = c(xmn = range(x_new)[1],xmx = range(x_new)[2], ymn = range(y_new)[1],ymx = range(y_new)[2])
# plot(TPR)
# maps::map("world",add = T)


# >>>>>>>>>>>>>>>>>> Open Polygion data <<<<<<<<<<<<<<<<<
load("~/cuda-workspace/mews_app/polygon_function/polygon_data/kec2.Rda")
OGR = readOGR("~/Data_riset/IDN_adm/IDN_adm2.shp")
OGR_indo = readOGR("~/Data_riset/IDN_adm/IDN_adm1.shp")
OGR_sulsel = OGR[OGR$NAME_1 == "Sulawesi Selatan",]
OGR_pinrang = OGR[OGR$NAME_2 == "Pinrang",]

OGR_indo = OGR_indo[OGR_indo$NAME_2 %in% c("Sulawesi Tenggara", "Sulawesi Tengah", "Sulawesi Barat"),]


plot(OGR_pinrang)
cek_bone = which(kec$Kabupaten == "KAB. PINRANG")
Bone = kec$Kecamatan[cek_bone]
kec_bone = kec[cek_bone,]
# plot(kec_bone, add = T, border = "blue", lwd = 0.5)

dat_cl = TP_new
dat_cl[TP_new > 5 ] = 1
threshold = 1
nama_kec = kec_bone$Kecamatan
izir = which( dat_cl == threshold , arr.ind=T )

xlon = x_new[izir[,1]]
ylat = y_new[izir[,2]]

dat <- data.frame(Longitude = xlon,
                  Latitude = ylat)
coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(kec_bone)

# over(dat, kec_bone[6,])
ADA_HUJAN = c()
for( i in 1:length(kec_bone)){
  ADA_HUJAN[i] = any(!is.na(over(dat, kec_bone[i,])))
}

col_hujan = 1:length(kec_bone)
col_hujan[ADA_HUJAN] = "red"
col_hujan[!ADA_HUJAN] = "blue"

kec_bone[ADA_HUJAN,]
# plot(OGR_sulsel, add=T, lty = 3, lwd = 0.7, border = "black")
# plot(OGR_pinrang,  lty = 3, lwd = 1, border = "black")
# image(x_new, y_new, TP_new,col = cc(10), add = T)
# plot(OGR_pinrang,  lty = 3, lwd = 1, border = "black", add = T)
# plot(TPR)
# maps::map("world", add=T)
xy = kec_bone[ADA_HUJAN,]@polygons[[1]]@labpt
plot(kec_bone, border = col_hujan, lwd = 0.5)
plot(kec_bone[ADA_HUJAN,], add = T, border = "red", lwd = 2, col = "red")
points(x = xy[1], y = xy[2], col = "black", lwd = 2)
# kec_bone[ADA_HUJAN,]$Kecamatan
title(main = "02 Jun 2018 \nPkl. 00:03 WITA")
text(x = xy[1], y = xy[2]+0.02, kec_bone[ADA_HUJAN,]$Kecamatan, cex = 0.5)
box()



library(leaflet)
library(leaflet.extras)
library(ggmap)
library(htmlwidgets)

m = leaflet() %>% addProviderTiles(providers$MtbMap) %>%
  addPolygons(data = kec_bone[!ADA_HUJAN,],color = "grey") %>%
  addPolygons(data = kec_bone[ADA_HUJAN,],color = "red") %>%
  addPopups(lng  = xy[1], lat = xy[2], kec_bone$Kecamatan[ADA_HUJAN])

  saveWidget(file = "pinrang.html", m, selfcontained = F)










