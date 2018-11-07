#Aprile 2018
#Questo programma legge i netCDF e li converte in un GeoTiff. Abbandonata la creazione di un file netCdf. Il problema è che
#quando si corregge l'asse temporale con cdo settaxis il grigliato viene shiftato rispetto al grigliato di riferimento. Il problema
#sembra essere l'attributo axis nelle coordinate longitude e latitude che vengono prodotte da CDO. Al momento la soluzione è
#convertire i netCDf di ERA5 in GeoTiff.
rm(list=objects())
library("raster")
library("rasterVis")
library("purrr")
library("stringr")
library("cairoDevice")
library("ncdf4")
library("rgdal")
library("gtools")
options(error=recover,warn = 2)


###########################################################################
# Main --------------------------------------------------------------------
###########################################################################

#file di input: attenzione, non vi debbono essere file netCDF che non si vogliono convertire
#I file di input hanno nome del tipo: nomeparametro_it.nc
#I file standardizzati hanno estensione "_daily.s.nc"
list.files(pattern="^.+_daily\\.s?.?nc$")->lista.file
stopifnot(length(lista.file)!=0)
purrr::walk(lista.file,~(print(sprintf("---->> FILE DA CONVERTIRE: %s",.))))

#griglia di 1 sopra l'Italia e NA sul mare. Questo è il raster che utilizziamo come "template" per riproiettare il file netCDF
#Ad esempio: volendo passare da epsg:4326 a 32632 il file "griglia.tif" deve essere una maschera con epsg:32632 e risoluzione spaziale desiderata
tryCatch({
  raster("./griglia.tif")
},error=function(e){
  stop("File griglia di supporto non trovata")
})->grigliaTemplate

#pre-fill della funzione projectRaster utilizzando come metodo bilineare e grigliaTemplate come raster di riferimento
cambiaProiezione<-purrr::partial(...f=projectRaster,to=grigliaTemplate,method="bilinear")
cambiaProiezione<-purrr::partial(...f=projectRaster,crs="+init=epsg:32632",res=1000,method="bilinear")


#pre-fill della funzione mask utilizzando come maskvalue=NA e mask=grigliaTemplate come raster di riferimento
purrr::partial(...f=mask,mask = grigliaTemplate,maskvalue=NA)->maschera

print("********************************")
print("------->> I dati verranno mascherati utilizzando come maskvalue NA, assicurarsi che la griglia di maschera sia corretta")
print("********************************")


ANNOI<-2015
ANNOF<-2015

seq.Date(from=as.Date(sprintf("%s-01-01",ANNOI)),to=as.Date(sprintf("%s-12-31",ANNOI)),by="day")->calendario
numeroGiorni<-length(calendario)
#numeroGiorni<-3

purrr::walk(lista.file,.f=function(ffile){

    unlist(stringr::str_split(ffile,"_"))[1]->VARIABILE

    dir.create(VARIABILE)
  
    if(VARIABILE=="tp"){
      dlname <- "Total precipitation"
      units<-"mm"
    }else if(VARIABILE=="t2m"){
      dlname <- "2 m temperature"
      units<-"°C"
    }else if(VARIABILE=="sp"){
      dlname <- "surface_air_pressure"
      units<-"hPa"
    }else if(VARIABILE=="pbl00" || VARIABILE=="pbl12" || VARIABILE=="lnpbl00" || VARIABILE=="lnpbl12" ){
      dlname <- "Boundary layer height"
      units<-"m"
    }else if(VARIABILE=="u10"){
      dlname <- "10 metre U wind component"
      units<-"m s**-1"
    }else if(VARIABILE=="v10"){
      dlname <- "10 metre V wind component"
      units<-"m s**-1"
    }else if(VARIABILE=="wdir"){  
      dlname <- "10 metre wind direction"
      units<-"degrees"      
    }else if(VARIABILE=="wspeed"){
      dlname <- "10 metre wind speed"
      units<-"m/s"      
    }else{
      stop(sprintf("%s: variabile non riconosciuta!",VARIABILE))
    }  
  
    #leggiamo mediante raster le informazioni sul netCDF
    brick(ffile)->myBrick
    extent(c(xmin(myBrick), xmax(myBrick),ymin(myBrick),ymax(myBrick)))->estensioneBrick

    nc_open(ffile)->oggetto_nc
    
    VARIABILE->VARIABILE2 
    if(length(grep("pbl",VARIABILE))!=0) VARIABILE2<-"blh" #il pbl in realtà ha come variabile blh (boundary layer height)
    
    ncvar_get(oggetto_nc,VARIABILE2)->dati
    
    #le coordinate vano invertite per poter lavorare con raster
    aperm(dati,c(2,1,3))->dati
    
    purrr::walk(1:numeroGiorni,.f=function(qualeLayer){
    
        r2<-raster(dati[,,qualeLayer])    
        setExtent(r2,estensioneBrick)->r2
        crs(r2)<-"+init=epsg:4326"
        cambiaProiezione(from=r2)->r2_utm

        resample(r2_utm,grigliaTemplate)->r2_utm

        #crop(r2_utm,grigliaTemplate,snap="near")->prova
        #mascheriamo secondo la maschera grigliaRiferimento
        maschera(x = r2_utm)->r2_utm

        writeRaster(r2_utm,paste0("./",VARIABILE,"/",VARIABILE,"_",stringr::str_pad(qualeLayer,pad="0",width=3,side="left"),".tif"),overwrite=TRUE)
    
    })
    
    list.files(pattern=paste0("^",VARIABILE,"_[0-9]+",".tif$"),path=paste0("./",VARIABILE))->elencoFile
    stopifnot(length(elencoFile)==length(calendario))
    mixedsort(elencoFile)->elencoFile
    print(elencoFile)
    stack(paste0("./",VARIABILE,"/",elencoFile))->mystack #mixedsort fondamentale per avere i nomi dei file ordinati secondo la numerazione interna

    writeRaster(mystack,paste0("./",VARIABILE,"/",VARIABILE,"_daily_utm.tif"),format="GTiff",overwrite=TRUE)

    

}) #fine ciclo walk
