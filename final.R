# I. Ambiente
requiredPackages  <- c("dplyr","stringr","stringi","strex")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(requiredPackages)

# II. Datos

#Se importan los datos completos
new_data<-read.csv("new_data.csv", encoding="UTF-8")
new_data$TipoValorStr<-as.factor(new_data$TipoValorStr)
new_data$TipoDerecho<-as.factor(new_data$TipoDerecho)
new_data$id <- 1:nrow(new_data)
new_data<-new_data%>% select(id,PROPORCION_O_IMPORTE,TipoValorStr,TipoDerecho )

subset_div<-c("0",
          "1 ",
          "1B",
          "1E",
          "3 ",
          "41",
          "CF",
          "FE",
          "FF",
          "FH")

subset_derecho<-c("2","9","16","21")
dividendos<-new_data%>% filter(TipoValorStr %in% subset_div) %>% filter(TipoDerecho %in% subset_derecho)

# III. Pre limpieza
# Se quitan palabras ruidosas
limpieza<-dividendos %>%
  mutate(cleaning=sub("DE DONDE.*", "", PROPORCION_O_IMPORTE),
         cleaning=sub("EN DONDE.*", "", cleaning),
         cleaning=sub("DONDE.*", "", cleaning),
         cleaning=sub("DE LOS CUALES.*", "", cleaning),
         cleaning=sub("DIVIDI.*", "", cleaning),
         cleaning=sub("CONFOR.*", "", cleaning),
         cleaning=sub("CADA UNO.*", "", cleaning),
         cleaning=sub("COMPUEST.*", "", cleaning),
         cleaning=sub("LO QUE REP.*", "", cleaning),
         cleaning=sub("DI[ ]*VIDIDO.*", "", cleaning),
         cleaning=sub("POR CERT.*", "", cleaning),
         cleaning=sub("POR EL PER.*", "", cleaning),
         cleaning=sub("DISTRIBUC.*", "", cleaning),
         
         q_montos=str_count(cleaning,pattern ="\\$"), 
         q_cupones=str_count(cleaning,pattern ="CUPON|CUPÓN"),
         q_series=str_count(cleaning,pattern ="SERIE"),
         extraord=str_count(cleaning,pattern ="EXTRAORD"),
         importe=str_count(cleaning,pattern ="BRUTO"),
         cleaning=str_remove_all(cleaning,pattern = "[ ]*-[ ]*PESO[ ]*MEXICANO"),
         cleaning=str_remove_all(cleaning,pattern = " DE"),
         cleaning=str_remove_all(cleaning,pattern = " - DÓLAR AMERICANO"),
         cleaning=str_remove_all(cleaning,pattern = " - EURO"))
limpieza$PROPORCION_O_IMPORTE<-NULL

# IV EXTRAIGO VECTORES de valores con expresiones regulares

## MONTOS
### EXTRA
arpoon_cifras<- "[0-9]*[.][ ]*[0-9]{1,15}"
limpieza<-limpieza %>% mutate(vector_montos=(str_extract_all(cleaning,arpoon_cifras)))
arpooon_curr<- "MXN |EUR|USD |M.N |MXV|M.N."
limpieza<-limpieza %>% mutate(vector_curr=(str_extract_all(cleaning,arpooon_curr)))

limpieza<-limpieza %>% mutate(monto_extraord=ifelse(extraord==1 & q_montos==1,str_extract(cleaning,arpoon_cifras),NA),
                              monto_extraord=ifelse(extraord==1 & q_montos==2,stri_extract_last(cleaning,regex=arpoon_cifras),monto_extraord),
                              monto_extraord=ifelse(extraord==1 & q_montos==3,
                                                    as.numeric(as.character(lapply(vector_montos, `[`, 2)))
                                                    ,monto_extraord),
                              monto_extraor2=ifelse(extraord==1 & q_montos==3,
                                                    as.numeric(as.character(lapply(vector_montos, `[`, 3)))
                                                    ,NA),# cifras extraordinarias
                              curr=str_extract(cleaning,arpooon_curr),# currencies
                              #importes
                              importe_brut=ifelse(str_detect(cleaning, "BRUTO"),as.numeric(as.character(lapply(vector_montos, `[`, 1))),NA),
                              importe_net=ifelse(str_detect(cleaning, "BRUTO"),as.numeric(as.character(lapply(vector_montos, `[`, 2))),NA),
                              importe_net=ifelse(str_detect(cleaning, "NETO") & is.na(importe_net),as.numeric(as.character(lapply(vector_montos, `[`, 1))),importe_net))

# CUPONES
arpoon_cupones<- "[A-Z][ ]{1,}[0-9]{1,3} |[A-Z][ ]{1,}[0-9]{1,3}\\.$|[A-Z][ ]{1,}[0-9]{1,3}\\.[ ]{1,}|[A-Z][ ]{1,}[0-9]{1,3}$"
arpoon_integer<-"[0-9]{1,3}"
limpieza<-limpieza %>% mutate(
  vector_cupones=(str_extract_all(cleaning,arpoon_cupones)))


limpieza<-limpieza%>% mutate(
  cup_vig1= ifelse(q_cupones>0,as.numeric(str_extract(as.character(lapply(vector_cupones, `[`, 1)),arpoon_integer)),NA),
  cup_vig2= ifelse(q_cupones>0,as.numeric(str_extract(as.character(lapply(vector_cupones, `[`, 3)),arpoon_integer)),NA),
  cup_vig3= ifelse(q_cupones>0,as.numeric(str_extract(as.character(lapply(vector_cupones, `[`, 5)),arpoon_integer)),NA),
  cup_pag1= ifelse(q_cupones>0,as.numeric(str_extract(as.character(lapply(vector_cupones, `[`, 2)),arpoon_integer)),NA),
  cup_pag2= ifelse(q_cupones>0,as.numeric(str_extract(as.character(lapply(vector_cupones, `[`, 4)),arpoon_integer)),NA),
  cup_pag3= ifelse(q_cupones>0,as.numeric(str_extract(as.character(lapply(vector_cupones, `[`, 6)),arpoon_integer)),NA),
  cup_aux=cup_vig1,
  cup_aux2=cup_pag1,
  cup_vig1=ifelse(!is.na(cup_aux)&is.na(cup_aux2),NA,cup_vig1),
  cup_pag1=ifelse(!is.na(cup_aux)&is.na(cup_aux2),cup_aux,cup_pag1) # aquellas obs con un solo cuponse asume que son de pago
)

# SERIES

arpoon_series <- "['].{1,5}[']|[\"].{1,5}[\"]"
arpoon_string<-".{1,5}"
limpieza<-limpieza %>% mutate(
  vector_series=(str_extract_all(cleaning,arpoon_series)))

limpieza<-limpieza%>% mutate(
  serie1= (str_extract(as.character(lapply(vector_series, `[`, 1)),arpoon_string)),
  serie2= (str_extract(as.character(lapply(vector_series, `[`, 2)),arpoon_string)),
  serie3= (str_extract(as.character(lapply(vector_series, `[`, 3)),arpoon_string))
)

a<-limpieza%>% filter(is.na(importe_net))%>% filter( is.na(monto_extraord))
# MONTOs

limpieza<-limpieza%>% mutate(
  monto1= ifelse( is.na(importe_net) | is.na(monto_extraord), as.numeric(as.character(lapply(vector_montos, `[`, 1))),NA),
  monto2= ifelse( is.na(importe_net) | is.na(monto_extraord), as.numeric(as.character(lapply(vector_montos, `[`, 2))),NA),
  monto3= ifelse( is.na(importe_net) | is.na(monto_extraord), as.numeric(as.character(lapply(vector_montos, `[`, 3))),NA))

# VI. CORRECCIONES

## CORRECCION CPO (si menciona explicitamente por cpo poner como serie 1 y quitar el monto 1 por el que se menciona como monto por CPO)
limpieza<-limpieza %>% mutate(serie1=ifelse(is.na(serie1) & str_detect(cleaning,"POR CPO"),"'CPO'",serie1),
  
  monto1=ifelse((serie1== "'CPO'" |serie1=="\"CPO\"") & is.na(serie2) & !is.na(monto2),
                              monto2,monto1),
                              monto2=ifelse((serie1== "'CPO'" |serie1=="\"CPO\"") & is.na(serie2) & !is.na(monto2),
                                            NA,monto2))
##SUPUESTO MXN (si no se ha dicho una currency si no es del tipo de derecho 1E se le asginan pesos mexicanos)
limpieza<-limpieza %>% mutate(curr=ifelse(TipoDerecho!=2 & is.na(curr),"MXN",curr))
summary(factor(limpieza$serie1))

#VII FINALIZACION

## Selecciono las variables finales
final_clean<-limpieza %>% select(id,curr, importe_brut,importe_net,
                                 monto1,serie1,cup_vig1,cup_pag1,
                                 monto2,serie2,cup_vig2,cup_pag2,
                                 monto3,serie3,cup_vig3,cup_pag3,
                                
                                 monto_extraord)

## Renombro 
final_clean<-final_clean%>%rename(div1=monto1,div2=monto2,div3=monto3)

## REGRESO BASES DE DATOS FILTRADA Y COMPLETA
new_data<-read.csv("new_data.csv", encoding="UTF-8")
new_data$TipoValorStr<-as.factor(new_data$TipoValorStr)
new_data$TipoDerecho<-as.factor(new_data$TipoDerecho)
new_data$id <- 1:nrow(new_data)
dividendos<-new_data%>% filter(TipoValorStr %in% subset_div) %>% filter(TipoDerecho %in% subset_derecho)

subset_data<-merge(dividendos,final_clean, by="id")
subset_data$id<-NULL
subset_data$X.U.FEFF.tm<-NULL

write.csv(subset_data,"subset_data.csv")

complete_data<-merge(new_data,final_clean, by="id", all.x = T)
write.csv(complete_data,"complete_data.csv")
