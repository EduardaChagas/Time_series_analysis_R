#Leitura de dados em imagens/texturas

#Bibliotecas
library(jpeg)
library(png)
library(bmp)
library(tiff)

Read_jpeg<-function(){
	myImage<-readJPEG(file.choose())
	return(myImage)
}

Read_png<-function(){
	myImage<-readPNG(file.choose())
	return(myImage)
}

Read_tiff<-function(){
	myImage<-readTIFF(file.choose())
	return(myImage)
}

Read_bmp<-function(){
	myImage<-readBMP(file.choose())
	return(myImage)
}