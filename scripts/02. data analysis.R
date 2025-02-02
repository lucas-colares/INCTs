##### Resende et al. (2025) #####
# # # # # By LF Colares # # # # #

##### Load packages ----
library(ggplot2)
library(ggpubr)
library(tabulizer)
library(sf)
library(MoMAColors)
library(scatterpie)
library(ggnewscale)
library(gtable)
library(stringr)
library(rgbif)
library(dplyr)

##### Approved vs. Not Approved in 2022 -----
extract_tables(file = "datasets/pdf/res_2022.pdf")->tabs
ne_tabs<-lapply(1:length(tabs),function(x){
  colnames(tabs[[x]])<-tabs[[x]][1,]
  gsub("\\\r"," ",colnames(tabs[[x]]))->colnames(tabs[[x]])
  tabs[[x]]<-tabs[[x]][-1,]
  return(tabs[[x]])
})
do.call("rbind",ne_tabs)->ne_tabs

as.data.frame(ne_tabs)->ne_tabs
regs<-data.frame(UF=unique(ne_tabs$UF),Region=c("Southeast","South","Southeast","Southeast","Northeast","South","Northeast","South","Northeast","Midwest","North","North","Northeast","Northeast","North","Northeast","Southeast","Midwest","Midwest","Midwest","North","Northeast"))

regs[match(ne_tabs$UF,regs$UF),2]->ne_tabs$Region

count_proposals<-ggplot(data = ne_tabs,aes(y = Region,color=`Resultado Final`,fill=`Resultado Final`))+
  geom_bar()+
  theme_bw()+
  labs(x="Number of submitted proposals",fill="Result",color="Result",y="Brazilian region")+
  scale_fill_manual(labels=c("Approved","Declined"),values = c("#5CB338","#FB4141"))+
  scale_color_manual(labels=c("Approved","Declined"),values = c("#5CB338","#FB4141"))+
  theme(legend.text.align = 0,legend.title.align = 0,legend.justification = "top",legend.position ="right", legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(family = "sans",size=12, face="bold"), legend.text = element_text(family = "sans",size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14), panel.border = element_rect(colour = "black", fill = "NA")); count_proposals

prop_proposals<-ggplot(data = ne_tabs,aes(y = Region,color=`Resultado Final`,fill=`Resultado Final`))+
  geom_bar(position = "fill")+
  theme_bw()+
  labs(x="Proportion of submitted proposals",fill="Result",color="Result",y="Brazilian region")+
  scale_fill_manual(labels=c("Approved","Declined"),values = c("#5CB338","#FB4141"))+
  scale_color_manual(labels=c("Approved","Declined"),values = c("#5CB338","#FB4141"))+
  theme(legend.text.align = 0,legend.title.align = 0,legend.justification = "top",legend.position ="right", legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(family = "sans",size=12, face="bold"), legend.text = element_text(family = "sans",size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14), panel.border = element_rect(colour = "black", fill = "NA")); prop_proposals

tiff(filename = "figures/2022_proposals.tif",width = 5.5,height = 5,units = "in",res = 600)
ggarrange(count_proposals,prop_proposals,ncol = 1,nrow = 2,common.legend = T,legend = "right")
dev.off()

##### Map of distribution - INCTs in Brazil ----
read_sf("datasets/spatial/BR_Regioes_2023.shp")->BR
head(BR)
gsub("\\\n","",BR$NM_REGIAO)->BR$NM_REGIAO
gsub("oeste","Oeste",BR$NM_REGIAO)->BR$NM_REGIAO

read.csv("datasets/csv/incts.csv",header = T,fileEncoding = "latin1",sep = ";")->incts
read.csv("datasets/csv/bolsistas_incts.csv",h=T,fileEncoding = "latin1",sep = ";")->bolsa
read.csv("datasets/csv/neo_sex.csv",h=T,fileEncoding = "latin1",sep = ";")->sex
sex[match(word(bolsa$X..Nome.Beneficiário,1),sex$Nome),2]->bolsa$sex
bolsa[bolsa$X..Cod.Modalidade=="APQ",]->coords1
bolsa[!bolsa$X..Cod.Modalidade=="APQ",]->bolsa

table(incts$Regiões)->n_incts
n_incts[match(BR$NM_REGIAO,names(n_incts))]->n_incts
n_incts->BR$N_INCT
BR$N_INCT[is.na(BR$N_INCT)]<-0

table(incts$Regiões,incts$Genre)->genre_incts
colSums(genre_incts)/sum(colSums(genre_incts))
genre_incts/rowSums(genre_incts)->prop_genre
as.numeric(prop_genre[match(BR$NM_REGIAO,rownames(prop_genre)),1])->BR$PROP_F
as.numeric(prop_genre[match(BR$NM_REGIAO,rownames(prop_genre)),2])->BR$PROP_M
BR[is.na(BR$PROP_F),]$PROP_F<-0
BR[is.na(BR$PROP_M),]$PROP_M<-0

table(incts$Regiões)->n_incts_reg
gsub("Oeste","oeste",names(n_incts_reg))->names(n_incts_reg)
as.numeric(n_incts_reg[match(BR$NM_REGIAO,names(n_incts_reg))])->BR$N_INCT_REG

table(bolsa$X..Nome.Região)->N_BOLSA
#names(N_BOLSA)<-c("AM","BA","CE","DF","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RS","RO","SC","SP","SE","TO")
as.numeric(N_BOLSA[match(BR$NM_REGIAO,names(N_BOLSA))])->BR$N_BOLSA

table(bolsa$X..Nome.Região,bolsa$sex)->genre_bolsa
#rownames(genre_bolsa)<-c("AM","BA","CE","DF","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI/","RJ","RS","RO","SC","SP","SE","TO")
genre_bolsa/rowSums(genre_bolsa)->prop_genre_bolsa
as.numeric(prop_genre_bolsa[match(BR$NM_REGIAO,rownames(prop_genre_bolsa)),1])->BR$BOLSA_F
as.numeric(prop_genre_bolsa[match(BR$NM_REGIAO,rownames(prop_genre_bolsa)),2])->BR$BOLSA_M
BR[is.na(BR$BOLSA_F),]$BOLSA_F<-0
BR[is.na(BR$BOLSA_M),]$BOLSA_M<-0

BR[BR$N_INCT==0,]$N_BOLSA<-NA

aggregate(BR$N_BOLSA,list(BR$NM_REGIAO),sum,na.rm=T)->bolsa_reg
bolsa_reg[match(BR$NM_REGIAO,bolsa_reg$Group.1),2]->BR$BOLSA_REG

st_centroid(BR)->centroids_BR
data.frame(as.data.frame(centroids_BR),st_coordinates(centroids_BR))->centroids_BR

map_incts<-ggplot()+
  geom_sf(data = BR,mapping = aes(fill=log10(as.numeric(N_INCT))),color="black")+
  scale_fill_moma_c(palette_name = "Exter",labels=round(c(10^1.25,10^1.5,10^1.75,10^2,10^2.25),0),name="Number of\nINCTs in\nthe region")+
  new_scale_fill()+
  geom_scatterpie(data = centroids_BR,aes(x = X, y = Y,r=2),cols=c("PROP_M","PROP_F"),linewidth=0.25)+
  #geom_scatterpie_legend(log10(as.numeric(centroids_BR$N_INCT))+0.4, x=-70, y=-30,labeller = function(x) round(10^(x-0.4),0))+
  #annotate(geom="text", x=-72.5, y=-23.75, label="Number of\nINCTs in\nthe state",hjust=0,size=4.5,color="black",fontface="bold",lineheight = .8)+
  scale_fill_manual(values=c("#77CDFF","#C62E2E"),labels=c("Male","Female"),name="Sex")+
  facet_wrap("(a) Coordinators:"~.)+
  guides(fill = "none")+
  theme_minimal()+
  labs(x=NULL,y=NULL)+
  theme(strip.text = element_text(size=12,family="sans",face = "italic",hjust = 0),strip.background = element_rect(fill="white",color = "white"),legend.text.align = 0,legend.title.align = 0,legend.justification = "left",legend.position=c(0.02,0.28), legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(family = "sans",size=12, face="bold"), legend.text = element_text(family = "sans",size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14));map_incts

only_leg1<-{ggplot()+
  geom_sf(data = BR)+
  geom_scatterpie(data = centroids_BR,aes(x = X, y = Y,r=2),cols=c("PROP_M","PROP_F"),linewidth=0.25)+
  scale_fill_manual(values=c("#77CDFF","#C62E2E"),labels=c("Male","Female"),name="Sex")+
  theme(strip.text = element_text(size=12,family="sans",face = "italic",hjust = 0),strip.background = element_rect(fill="white",color = "white"),legend.text.align = 1,legend.title.align = 1,legend.justification = "right",legend.position="right", legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(family = "sans",size=12, face="bold"), legend.text = element_text(family = "sans",size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14))}

leg2 <- gtable_filter(ggplot_gtable(ggplot_build(only_leg1)), "guide-box") 

map_incts=map_incts+annotation_custom(grob = leg2,xmin = -50, xmax = -27, ymin = 5, ymax = -5)

# tiff(filename = 'figures/map_coords.tif',width = 7,height = 6.5,units = "in",res = 600)
# map_incts
# dev.off()

## Bolsistas
  
map_bolsa<-ggplot()+
  geom_sf(data = BR,mapping = aes(fill=log10(BOLSA_REG)),color="black")+
  scale_fill_moma_c(palette_name = "Exter",labels=round(c(10^2.2,10^2.4,10^2.6,10^2.8,10^3),0),name="Human\nresources in\nthe region",breaks=c(2.2,2.4,2.6,2.8,3))+
  new_scale_fill()+
  geom_scatterpie(data = centroids_BR,aes(x = X, y = Y,r=2),cols=c("BOLSA_M","BOLSA_F"),linewidth=0.25)+
  #geom_scatterpie_legend(log10(as.numeric(centroids_BR$N_BOLSA)), x=-70, y=-30,labeller = function(x) round(10^(x),0))+
  #annotate(geom="text", x=-72.5, y=-23.5, label="Human\nresources in\nthe state",hjust=0,size=4.5,color="black",fontface="bold",lineheight = .8)+
  scale_fill_manual(values=c("#77CDFF","#C62E2E"),labels=c("Male","Female"),name="Sex")+
  facet_wrap("(b) Students and technicians:"~.)+
  guides(fill = "none")+
  theme_minimal()+
  labs(x=NULL,y=NULL)+
  theme(strip.text = element_text(size=12,family="sans",face = "italic",hjust = 0),strip.background = element_rect(fill="white",color = "white"),legend.text.align = 0,legend.title.align = 0,legend.justification = "left",legend.position=c(0.02,0.28), legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(family = "sans",size=12, face="bold"), legend.text = element_text(family = "sans",size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14))#;map_bolsa

map_bolsa=map_bolsa+annotation_custom(grob = leg2,xmin = -50, xmax = -27, ymin = 5, ymax = -5)

tiff(filename = 'figures/map_coords&students.tif',width = 14,height = 6.5,units = "in",res = 600)
ggarrange(map_incts,map_bolsa,ncol = 2)
dev.off()

summary(lm(BR$N_BOLSA~as.numeric(BR$N_INCT)))
plot(as.numeric(BR$N_INCT),BR$N_BOLSA)

##### Map only for biodiversity ----
coords1[grepl("Zoologia|Ecologia|Botânica",coords1$X..Nome.Área),]->coords_bio
bolsa[grepl("Zoologia|Ecologia|Botânica",bolsa$X..Nome.Área),]->bolsas_bio

gsub("oeste","Oeste",BR$NM_REGIAO)->BR$NM_REGIAO

table(coords_bio$X..Nome.Região)->N_BIO_COORD
as.numeric(N_BIO_COORD[match(BR$NM_REGIAO,names(N_BIO_COORD))])->BR$N_BIO_COORD
table(bolsas_bio$X..Nome.Região)->N_BIO_BOLSA
as.numeric(N_BIO_BOLSA[match(BR$NM_REGIAO,names(N_BIO_BOLSA))])->BR$N_BIO_BOLSA

table(coords_bio$X..Nome.Região)->REG_BIO_COORD
as.numeric(REG_BIO_COORD[match(BR$NM_REGIAO,names(REG_BIO_COORD))])->BR$REG_BIO_COORD
table(bolsas_bio$X..Nome.Região)->REG_BIO_BOLSA
as.numeric(REG_BIO_BOLSA[match(BR$NM_REGIAO,names(REG_BIO_BOLSA))])->BR$REG_BIO_BOLSA

table(coords_bio$X..Nome.Região,coords_bio$sex)->N_SEX_COORD
N_SEX_COORD/rowSums(N_SEX_COORD)->N_SEX_COORD
as.numeric(N_SEX_COORD[match(BR$NM_REGIAO,rownames(N_SEX_COORD)),1])->BR$BIO_COORD_F
as.numeric(N_SEX_COORD[match(BR$NM_REGIAO,rownames(N_SEX_COORD)),2])->BR$BIO_COORD_M
BR[is.na(BR$BIO_COORD_F),]$BIO_COORD_F<-0
BR[is.na(BR$BIO_COORD_M),]$BIO_COORD_M<-0

table(bolsas_bio$X..Nome.Região,bolsas_bio$sex)->N_SEX_BOLSA
N_SEX_BOLSA/rowSums(N_SEX_BOLSA)->N_SEX_BOLSA
as.numeric(N_SEX_BOLSA[match(BR$NM_REGIAO,rownames(N_SEX_BOLSA)),1])->BR$BIO_BOLSA_F
as.numeric(N_SEX_BOLSA[match(BR$NM_REGIAO,rownames(N_SEX_BOLSA)),2])->BR$BIO_BOLSA_M
BR[is.na(BR$BIO_BOLSA_F),]$BIO_BOLSA_F<-0
BR[is.na(BR$BIO_BOLSA_M),]$BIO_BOLSA_M<-0

st_centroid(BR)->centroids_BR
data.frame(as.data.frame(centroids_BR),st_coordinates(centroids_BR))->centroids_BR

#Coordinators
map_bio_coord<-ggplot()+
  geom_sf(data = BR,mapping = aes(fill=(REG_BIO_COORD)),color="black")+
  scale_fill_moma_c(palette_name = "Exter",name="Biodiversity\nINCTs in\nthe region",na.value = "white")+
  new_scale_fill()+
  geom_scatterpie(data = centroids_BR,aes(x = X, y = Y,r=2),cols=c("BIO_COORD_M","BIO_COORD_F"),linewidth=0.25)+
  #geom_scatterpie_legend((as.numeric(centroids_BR$N_BIO_COORD)), x=-70, y=-30)+
  #annotate(geom="text", x=-72.5, y=-23.5, label="Biodiversity\nINCTs in\nthe state",hjust=0,size=4.5,color="black",fontface="bold",lineheight = .8)+
  scale_fill_manual(values=c("#77CDFF","#C62E2E"),labels=c("Male","Female"),name="Sex")+
  facet_wrap("(c) Biodiversity coordinators:"~.)+
  guides(fill = "none")+
  theme_minimal()+
  labs(x=NULL,y=NULL)+
  theme(strip.text = element_text(size=12,family="sans",face = "italic",hjust = 0),strip.background = element_rect(fill="white",color = "white"),legend.text.align = 0,legend.title.align = 0,legend.justification = "left",legend.position=c(0.02,0.28), legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(family = "sans",size=12, face="bold"), legend.text = element_text(family = "sans",size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14))#;map_bio_coord

map_bio_coord=map_bio_coord+annotation_custom(grob = leg2,xmin = -50, xmax = -27, ymin = 5, ymax = -5)

#Students
map_bio_bolsas<-ggplot()+
  geom_sf(data = BR,mapping = aes(fill=log10(REG_BIO_BOLSA)),color="black")+
  scale_fill_moma_c(palette_name = "Exter",name="Biodiversity\nstudents in\nthe region",na.value = "white",labels=round(c(10^1.5,10^1.6,10^1.7,10^1.8),0),breaks=c(1.5,1.6,1.7,1.8))+
  new_scale_fill()+
  geom_scatterpie(data = centroids_BR,aes(x = X, y = Y,r=2),cols=c("BIO_BOLSA_M","BIO_BOLSA_F"),linewidth=0.25)+
  #geom_scatterpie_legend(log10(as.numeric(centroids_BR$N_BIO_BOLSA))+0.5, x=-70, y=-30,labeller = function(x) round(10^(x-0.5),0),n = 3)+
  #annotate(geom="text", x=-72.5, y=-24.25, label="Biodiversity\nstudents in\nthe state",hjust=0,size=4.5,color="black",fontface="bold",lineheight = .8)+
  scale_fill_manual(values=c("#77CDFF","#C62E2E"),labels=c("Male","Female"),name="Sex")+
  facet_wrap("(d) Biodiversity students and technicians:"~.)+
  guides(fill = "none")+
  theme_minimal()+
  labs(x=NULL,y=NULL)+
  theme(strip.text = element_text(size=12,family="sans",face = "italic",hjust = 0),strip.background = element_rect(fill="white",color = "white"),legend.text.align = 0,legend.title.align = 0,legend.justification = "left",legend.position=c(0.02,0.28), legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(family = "sans",size=12, face="bold"), legend.text = element_text(family = "sans",size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14))#;map_bio_bolsas

map_bio_bolsas=map_bio_bolsas+annotation_custom(grob = leg2,xmin = -50, xmax = -27, ymin = 5, ymax = -5)

tiff(filename = 'figures/all_maps.tif',width = 9.75,height = 8.75,units = "in",res = 600)
ggarrange(map_incts,map_bolsa,map_bio_coord,map_bio_bolsas,ncol = 2,nrow = 2)
dev.off()

map_bio_coord=map_bio_coord+facet_wrap("(a) Biodiversity coordinators:"~.)
map_bio_bolsas=map_bio_bolsas+facet_wrap("(b) Biodiversity students and technicians:"~.)

tiff(filename = 'figures/map_biodiversity.tif',width = 14,height = 6.5,units = "in",res = 600)
ggarrange(map_bio_coord,map_bio_bolsas,ncol = 2)
dev.off()

##### Congruence TAOCA vs. GBIF ----
read_sf("datasets/spatial/brazilian_legal_amazon.shp")->amz
read.csv("datasets/csv/taoca_data.csv",h=T,sep=";")->taoca
gsub(" ", "",taoca$Class)->taoca$Class
gsub(" ", "",taoca$Order)->taoca$Order

st_transform(amz,crs = 4326)->amz

unique(taoca$Class)
gsub("Bird","Aves",taoca$Class)->taoca$Class
gsub("Cichliformes|Gobiiformes|Eupercaria|Ovalentaria","Perciformes",taoca$Order)->taoca$Order

search_taxa<-c(unique(taoca[taoca$Class=="Actinopteri",]$Order),"Coleoptera","Formicidae","Aves","Hemiptera","Odonata","Megaloptera","Ephemeroptera","Plecoptera","Trichoptera","Diptera","Elasmobranchii","Dipneusti")

macrophytes<-unique(taoca[grepl("Equisetopsida|Charophyceae|Marchantiopsida",taoca$Class),]$Genus)

# match the names 
gbif_taxon_keys <- search_taxa %>% 
  head(1000) %>% # only first 1000 names 
  name_backbone_checklist() %>% # match to backbone 
  filter(!matchType == "NONE") %>% # get matched names
  pull(usageKey) 

gbif_taxon_keys<-c(gbif_taxon_keys[-21],1451,787,811)

st_as_sfc(amz)->amz_sfc

# download the data
occ_download(
  pred_in("taxonKey", gbif_taxon_keys), # important to use pred_in
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_within(st_as_text(st_reverse(amz_sfc))),
  format = "SPECIES_LIST",
  user = "",
  pwd = "",
  email = ""
)
occ_download_wait('0057084-241126133413365')
d <- occ_download_get('0057084-241126133413365') %>%
  occ_download_import()

occ_download(
  pred_in("taxonKey", 6), # important to use pred_in
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_within(st_as_text(st_reverse(amz_sfc))),
  format = "SPECIES_LIST",
  user = "",
  pwd = "",
  email = ""
)

occ_download_wait('0057111-241126133413365')
d <- occ_download_get('0057111-241126133413365')

unzip("0057111-241126133413365.zip")
file.copy("0057111-241126133413365.csv",'datasets/csv/gbif_macrophyte.csv')

unzip("0057084-241126133413365.zip")
file.copy("0057084-241126133413365.csv",'datasets/csv/gbif_data.csv')

read.csv("datasets/csv/gbif_data.csv",h=T,sep="\t")->gbif
gbif[!gbif$species=="",]->gbif

read.csv("datasets/csv/gbif_macrophyte.csv",h=T,sep="\t")->gbif_macrophyte
gbif_macrophyte[!gbif_macrophyte$species=="",]->gbif_macrophyte

#Macrophyte
gbif_macrophyte[grepl(paste0(macrophytes,collapse = "|"),gbif_macrophyte$genus),]->phyte_data
unique(phyte_data$species)->phyte_spp

taoca[grepl(paste0(macrophytes,collapse = "|"),taoca$Genus),]->phyte_tao
unique(phyte_tao$Binomial.sp)->phyte_tao

sum(duplicated(c(phyte_spp,phyte_tao)))->phyte_shared
length(match(phyte_spp,phyte_tao)[is.na(match(phyte_spp,phyte_tao))])->phyte_gbif_exclusive
length(match(phyte_tao,phyte_spp)[is.na(match(phyte_tao,phyte_spp))])->phyte_tao_exclusive

#Fish
gbif[grepl(paste0(search_taxa[c(1:14,25:26)],collapse = "|"),gbif$order),]->fish_data
unique(fish_data$species)->fish_spp

taoca[grepl(paste0(search_taxa[c(1:14,25:26)],collapse = "|"),taoca$Order),]->fish_tao
unique(fish_tao$Binomial.sp)->fish_tao

sum(duplicated(c(fish_spp,fish_tao)))->fish_shared
length(match(fish_spp,fish_tao)[is.na(match(fish_spp,fish_tao))])->fish_gbif_exclusive
length(match(fish_tao,fish_spp)[is.na(match(fish_tao,fish_spp))])->fish_tao_exclusive

#Beetles
gbif[grepl("Coleoptera",gbif$order),]->beetles_data
unique(beetles_data$species)->beetles_spp

taoca[grepl("Coleoptera",taoca$Order),]->beetles_tao
unique(beetles_tao$Binomial.sp)->beetles_tao

sum(duplicated(c(beetles_spp,beetles_tao)))->beetles_shared
length(match(beetles_spp,beetles_tao)[is.na(match(beetles_spp,beetles_tao))])->beetles_gbif_exclusive
length(match(beetles_tao,beetles_spp)[is.na(match(beetles_tao,beetles_spp))])->beetles_tao_exclusive

#Ants
gbif[grepl("Formicidae",gbif$family),]->ant_data
unique(ant_data$species)->ant_spp

taoca[grepl("Formicidae",taoca$Family),]->ant_tao
unique(ant_tao$Binomial.sp)->ant_tao

sum(duplicated(c(ant_spp,ant_tao)))->ant_shared
length(match(ant_spp,ant_tao)[is.na(match(ant_spp,ant_tao))])->ant_gbif_exclusive
length(match(ant_tao,ant_spp)[is.na(match(ant_tao,ant_spp))])->ant_tao_exclusive

#Aves
gbif[grepl("^Aves$",gbif$class),]->bird_data
unique(bird_data$species)->bird_spp

taoca[grepl("^Aves$",taoca$Class),]->bird_tao
unique(bird_tao$Binomial.sp)->bird_tao

sum(duplicated(c(bird_spp,bird_tao)))->bird_shared
length(match(bird_spp,bird_tao)[is.na(match(bird_spp,bird_tao))])->bird_gbif_exclusive
length(match(bird_tao,bird_spp)[is.na(match(bird_tao,bird_spp))])->bird_tao_exclusive

#Aquatic macroinvertebrates
gbif[grepl(paste0(search_taxa[18:24],collapse = "|"),gbif$order),]->macro_data
unique(macro_data$species)->macro_spp

taoca[grepl(paste0(search_taxa[18:24],collapse = "|"),taoca$Order),]->macro_tao
unique(macro_tao$Binomial.sp)->macro_tao

sum(duplicated(c(macro_spp,macro_tao)))->macro_shared
length(match(macro_spp,macro_tao)[is.na(match(macro_spp,macro_tao))])->macro_gbif_exclusive
length(match(macro_tao,macro_spp)[is.na(match(macro_tao,macro_spp))])->macro_tao_exclusive

# All taxa
data.frame(Taxa=c(rep("Fish",3),rep("Beetles",3),rep("Ants",3),rep("Birds",3),rep("Aquatic macroinvertebrates",3),rep("Macrophytes",3)),
           Dataset=c("Shared","GBIF exclusive","TAOCA exclusive"),
           N=c(fish_shared,fish_gbif_exclusive,fish_tao_exclusive,
               beetles_shared,beetles_gbif_exclusive,beetles_tao_exclusive,
               ant_shared,ant_gbif_exclusive,ant_tao_exclusive,
               bird_shared,bird_gbif_exclusive,bird_tao_exclusive,
               macro_shared,macro_gbif_exclusive,macro_tao_exclusive,
               phyte_shared,phyte_gbif_exclusive,phyte_tao_exclusive))->full_datasets

prop_taxa<-ggplot(data = full_datasets,aes(y = factor(Taxa,levels = rev(c("Ants","Fish","Birds","Aquatic macroinvertebrates","Beetles","Macrophytes"))),x=N,color=Dataset,fill=Dataset))+
  geom_bar(stat = "identity",position = "fill")+
  theme_bw()+
  scale_fill_moma_d(palette_name = "Exter")+
  scale_color_moma_d(palette_name = "Exter")+
  scale_y_discrete(labels=rev(c("Ants","Fish","Birds","Freshwater\nmacroinvertebrates","Beetles","Macrophytes")))+
  labs(x="Proportion of species",fill="Dataset",color="Dataset",y="Taxa")+
  theme(legend.text.align = 0,legend.title.align = 0,legend.justification = "top",legend.position ="right", legend.background = element_rect(fill="NA", colour = "NA"), legend.title = element_text(family = "sans",size=12, face="bold"), legend.text = element_text(family = "sans",size=10), axis.text.x = element_text(family = "sans", colour = "black", size=12), axis.text.y = element_text(family = "sans", colour = "black", size=12), axis.title = element_text(face="bold",family = "sans", size = 14), panel.border = element_rect(colour = "black", fill = "NA"));prop_taxa

tiff("figures/proportion_taxa.tif",width = 7,height = 3,units = "in",res = 600)
prop_taxa
dev.off()

full_datasets$prop<-NA
for(x in unique(full_datasets$Taxa)){
  full_datasets[full_datasets$Taxa==x,]$N/sum(full_datasets[full_datasets$Taxa==x,]$N)->full_datasets[full_datasets$Taxa==x,]$prop
}
full_datasets
