library(sf)
library(httr)
library(tidyverse)
library(ows4R)
library(raster)
library(lwgeom) #st_split !!!
library(ggspatial)
library(ggsflabel)
library(parzer)

#area of interest
m = rbind(c(-2,49), c(-2,50), c(1,50), c(1,49), c(-2,49))
p = st_sfc(st_polygon(list(m)))
st_crs(p)<-4326

coast <- st_read("data/FRA_adm2.shp") #https://www.diva-gis.org/datadown
coast <- st_cast(coast,to="MULTILINESTRING")
coast<-st_split(p,coast)%>%st_collection_extract(c("POLYGON"))
coast<-st_sfc(list(coast[[2]],coast[[3]],coast[[4]],coast[[5]],coast[[6]]))
st_crs(coast)<-4326

coastm12<-st_read('data/coastm12.shp')%>%mutate(id=as.numeric(1:5))%>%filter(id%in%c(2,4,5))%>%
	mutate(id=ifelse(id==2,"Manche",id))%>%
	mutate(id=ifelse(id==4,"Calvados",id))%>%
	mutate(id=ifelse(id==5,"Seine-Maritime",id))


gethumact<-function(nom="emodnet:aggregateareas",request="GetFeature"){
  url<-parse_url("https://ows.emodnet-humanactivities.eu/wfs")
  #nom="emodnet:aggregateareas"
  url$query <- list(service = "WFS",
                    version = "1.1.0",
                    request = "",
                    typename="",
                    bbox= "49.3,-2,49.8,0.2",
                    outputFormat = "application/json")
  url$query$typename<-nom
  url$query$request<-request
  #url$query<-list(typename =nom)
  request <- build_url(url)
  uu<-read_sf(request)
  return(uu)
}

nat2000<-gethumact("natura2000areas")

haraport<-st_read("data/haraport.json")
haraport<-haraport%>%mutate(sitename="Circonscription GPMH")


xy<-read.table("data/Point-recepteur-BDS.txt",header=T,sep="\t")%>%
	filter(keep==1)
coordinates(xy)<-~long+lat
xysf<-st_as_sf(xy)
st_crs(xysf)<-st_crs(nat2000)


rez1<-st_join(xysf,coastm12)%>%transmute(num,name,zone_de_compétence=id,geometry)
xyrez1<-st_coordinates(rez1)%>%as.data.frame()
tmp1<-parse_parts_lon(as.character(xyrez1$X))%>%
	mutate(Lon=paste0(paste(paste(deg,min,sep="°"),round(sec,5),sep="'"),"''"))
tmp2<-parse_parts_lon(as.character(xyrez1$Y))%>%
	mutate(Lat=paste0(paste(paste(deg,min,sep="°"),round(sec,5),sep="'"),"''"))
rez1<-rez1%>%mutate(Lat=tmp2$Lat,Lon=tmp1$Lon)%>%transmute(num,name,zone_de_compétence,Lat,Lon)%>%
	st_drop_geometry()

rez2<-st_join(xysf,nat2000)%>% 
	transmute(num,sitename,sitecode,sitedesc)%>%
	st_drop_geometry()%>%
	filter(!is.na(sitename))
rez3<-st_join(xysf,haraport)%>%
	transmute(num,sitename,sitecode=NA,sitedesc=NA)%>%
	st_drop_geometry()%>%
	filter(!is.na(sitename))

rezall<-full_join(rez1,rbind(rez2,rez3))

selsite<-na.omit(unique(rezall$sitename))


write.csv(rezall,file="output/rezall.csv",na="")

#a general map

x11(h=8.3,w=11.7)
ggplot()+
	geom_sf(data=nat2000,aes(geometry=geometry),fill=NA)+#,fill=sitename))
	geom_sf(data=nat2000%>%filter(sitename%in%selsite),aes(geometry=geometry,fill=sitename),alpha=0.5)+
	geom_sf(data=haraport,aes(fill=sitename),alpha=0.5)+
	geom_sf(data=coastm12,aes(geometry=geometry,fill=id),alpha=0.1)+
	geom_sf(data=coast,aes(geometry=geometry),fill="light grey")+
	geom_sf(data=xysf,aes(label=num))+
	geom_sf_text_repel(data=xysf,aes(label=num))+
	coord_sf(xlim=c(-2,1),ylim=c(49,50),expand=F)+
	theme_bw()+xlab("Longitude")+ylab("Latitude")+
	theme(legend.position="bottom")+
	guides(color=guide_legend(nrow=1,ncol=3,byrow=T),
	       fill=guide_legend(ncol=4,byrow=T,title="Zone"),
	)+
	 annotation_scale(
    location = "bl",
    width_hint = 0.2,
    pad_x = unit(21, "cm")
  ) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(25, "cm"),
    pad_y = unit(10, "cm"),
    style = north_arrow_fancy_orienteering
  )
  ggsave("output/map.png")
	#geom_sf(data=coast)
