data<-readxl::read_xlsx("gp.xlsx", skip = 1)
colnames(data)[c(1,2)]<-c("Sno", "Geo_fea")
names<-c(t(data[2]))
library(ggplot2)
library(reshape2)
data1<-as.data.frame(t(data[c(-1,-2,-9,-10,-11,-12)]))
names(data1)<-names
data1$year<-row.names(data1)


meltdata<-melt(data1,id.vars = "year", measure.vars = c(names), variable.name = "geo_fea", value.name = "area")

meltdata$area<-round(meltdata$area,2)
ggplot(data=meltdata[meltdata$year==2004,], aes(x=reorder(geo_fea, -area), y=area)) +
  geom_bar(stat="identity", fill="blue")+
  geom_text(data=data.frame(meltdata[meltdata$year==2004,]), aes(label=area), col="black", size=3, vjust=-1.2, face="bolt")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=35, hjust=1),text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10))+
  labs(title="Areal extent of Coastal Geomorphology units based on Landsat ETM+-2004, Tamil Nadu Coast",
       x ="Coastal Geomorphologic Units", y = "Area in Sq. KM")


ggplot(data=meltdata[meltdata$year==2004,], aes(x=reorder(geo_fea, -area), y=area)) +
  geom_bar(stat="identity", fill="blue")+
  geom_text(data=data.frame(meltdata[meltdata$year==2004,]), aes(label=area), col="black", size=3, vjust=-1.2, face="bold")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=35, hjust=1),text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10))+ylim(0, 600)+
  labs(title="Areal extent of Geological Features - 2004, Tamil Nadu",
       x ="Geological Features", y = "Area in sq.km")

ggplot(data=meltdata[meltdata$year==2005,], aes(x=reorder(geo_fea, -area), y=area)) +
  geom_bar(stat="identity", fill="blue")+
  geom_text(data=data.frame(meltdata[meltdata$year==2005,]), aes(label=area), col="black", size=3, vjust=-1.2, face="bolt")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=35, hjust=1),text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10))+ylim(0, 600)+
  labs(title="Areal extent of Geological Features - 2005, Tamil Nadu",
       x ="Geological Features", y = "Area in sq.km")

ggplot(data=meltdata[meltdata$year==2015,], aes(x=reorder(geo_fea, -area), y=area)) +
  geom_bar(stat="identity", fill="blue")+
  geom_text(data=data.frame(meltdata[meltdata$year==2015,]), aes(label=area), col="black", size=3, vjust=-1.2, face="bolt")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=35, hjust=1),text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10))+ylim(0, 600)+
  labs(title="Areal extent of Geological Features - 2015, Tamil Nadu",
       x ="Geological Features", y = "Area in sq.km")


ggplot(data=meltdata[meltdata$year==2016,], aes(x=reorder(geo_fea, -area), y=area)) +
  geom_bar(stat="identity", fill="blue")+
  geom_text(data=data.frame(meltdata[meltdata$year==2016,]), aes(label=area), col="black", size=3, vjust=-1.2, face="bolt")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=35, hjust=1),text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10))+ylim(0, 600)+
  labs(title="Areal extent of Geological Features - 2016, Tamil Nadu",
       x ="Geological Features", y = "Area in sq.km")

ggplot(data=meltdata[meltdata$year==2017,], aes(x=reorder(geo_fea, -area), y=area)) +
  geom_bar(stat="identity", fill="blue")+
  geom_text(data=data.frame(meltdata[meltdata$year==2017,]), aes(label=area), col="black", size=3, vjust=-1.2, face="bolt")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=35, hjust=1),text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10))+ylim(0, 600)+
  labs(title="Areal extent of Geological Features - 2017, Tamil Nadu",
       x ="Geological Features", y = "Area in sq.km")

ggplot(data=meltdata[meltdata$year==2018,], aes(x=reorder(geo_fea, -area), y=area)) +
  geom_bar(stat="identity", fill="blue")+
  geom_text(data=data.frame(meltdata[meltdata$year==2018,]), aes(label=area), col="black", size=3, vjust=-1.2, face="bolt")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=35, hjust=1),text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10))+ylim(0, 600)+
  labs(title="Areal extent of Geological Features - 2018, Tamil Nadu",
       x ="Geological Features", y = "Area in sq.km")

meltdata$geo_fea<-as.character(meltdata$geo_fea)

meltdata$geo_fea_f <- factor(meltdata$geo_fea, levels=c("Beach ridges","Alluvial plains","Beach ridge plains","Mud flats","Marine terrace","Sandy plains","Backwater","Pediments","Beaches","Rivers","Valley","Mangroves","Water bodies","Sand dunes","Beach rocks"))

library(RColorBrewer)
well<-colorRampPalette(colors = c("red", "green", "blue", "yellow"))
well(n = 6)
ggplot(data=meltdata, aes(x=year, y=area, fill=year)) +
  geom_bar(stat="identity")+ 
  facet_grid(.~geo_fea_f)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, hjust=1),text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10), strip.text.x = element_text(size = 6.3))+
  ylim(0, 600)+
  labs(title="Areal extent of Geological Features - 2004, 2005, 2015 - 2018, Tamil Nadu",
       x ="Year", y = "Area in sq.km")+
  scale_fill_manual("Year", values = c("2004" = "#FF0000", "2005" = "#659900", "2015" = "#00CB33", "2016"="#0032CC", "2017"="#666698", "2018"="#FFFF00"))


ggplot(meltdata[meltdata$year %in% c(2004, 2005),], aes(reorder(geo_fea, -area), area, fill=year))+ geom_bar(stat = "identity", position = 'dodge')+
theme_classic()+
  theme(axis.text.x=element_text(angle=35, hjust=1),text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10))+
  ylim(0, 600)+
  labs(title="Comparision of Areal extent of Geological Features - 2004 and 2005, Tamil Nadu",
       x ="Geological Features", y = "Area in sq.km")+
  scale_fill_manual("Year", values = c("2004" = "#FF0000", "2005" = "#0032CC"))

ggplot(meltdata[meltdata$year %in% c(2015, 2016),], aes(reorder(geo_fea, -area), area, fill=year))+ geom_bar(stat = "identity", position = 'dodge')+
  theme_classic()+
  theme(axis.text.x=element_text(angle=35, hjust=1),text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10))+
  ylim(0, 600)+
  labs(title="Comparision of Areal extent of Geological Features - 2015 and 2016, Tamil Nadu",
       x ="Geological Features", y = "Area in sq.km")+
  scale_fill_manual("Year", values = c("2015" = "#FF0000", "2016" = "#0032CC"))

ggplot(meltdata[meltdata$year %in% c(2017, 2018),], aes(reorder(geo_fea, -area), area, fill=year))+ geom_bar(stat = "identity", position = 'dodge')+
  theme_classic()+
  theme(axis.text.x=element_text(angle=35, hjust=1),text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10))+
  ylim(0, 600)+
  labs(title="Comparision of Areal extent of Geological Features - 2017 and 2018, Tamil Nadu",
       x ="Geological Features", y = "Area in sq.km")+
  scale_fill_manual("Year", values = c("2017" = "#FF0000", "2018" = "#0032CC"))

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

well<-colorRampPalette(colors = c("red", "green", "blue", "yellow"))
new<-well(n = 15)
nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

ggplot(meltdata, aes(year, area, group=geo_fea, col=factor(geo_fea)))+
  geom_line(alpha=0.5,lwd=1.5)+
  scale_x_continuous(n.breaks = 15, limits = c(2003, 2019))+
  scale_color_manual(values = c(new))+
  theme_classic()+
  theme(text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10))+labs(title=paste0("Trendline over the years for various geological features"), x = "Year", y = "Area in sq.km")
  
  
meltdata$year<-as.numeric(meltdata$year)

meltdata[meltdata$geo_fea=="Water bodies",]

ggplot(meltdata[meltdata$geo_fea=="Water bodies",], aes(x=year, y=area))+geom_point()+
  geom_smooth(method='lm',formula = y ~ x, se = FALSE)+
  labs(title="Regression line W, Tamil Nadu",
       x ="Geological Features", y = "Area in sq.km")
i=NULL

for (i in 1:length(unique(meltdata$geo_fea))){
uni<-unique(meltdata$geo_fea)[15]
png(paste0("rplot",uni,".png"), width = 650, height = 440)
ggplot(meltdata[meltdata$geo_fea==uni,], aes(x=year, y=area))+geom_point(size=3)+
    geom_smooth(method='lm',formula = y ~ x, se = FALSE)+
    labs(title=paste0("Trendline over the years for ",uni),
         x = "Year", y = "Area in sq.km")+
  theme_classic()+
  theme(text=element_text(family="Times New Roman", face="bold") ,plot.title = element_text(size=10))+scale_x_continuous(n.breaks = 15, limits = c(2003, 2019))
dev.off()  
}
unique(meltdata$geo_fea)

