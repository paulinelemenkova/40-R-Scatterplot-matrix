# Scatterplot matrix // матрица графиков диаграммы рассеяния (точечной диаграммы)
# ЧАСТЬ 1: делаем data.frame
	# шаг-1. вчитываем таблицу. делаем из нее датафрейм.
MDF <- read.csv("Morphology.csv", header=TRUE, sep = ",")
	# шаг-2. чистим датафрейм от NA значений
MDF <- na.omit(MDF) 
row.has.na <- apply(MDF, 1, function(x){any(is.na(x))}) 
sum(row.has.na) 
head(MDF) 
	# шаг-3. связываем значения 4-х плит в одну категорию "тектонические плиты"
MDFT = melt(setDT(MDF), measure = patterns("^plate"), value.name = c("tectonics"))
head(MDFT)
levels(MDFT$variable) = c("Philippine Plate" , "Pacific Plate", "Mariana Plate", "Caroline Plate") # переименовываем значения чтобы отображались полными именами (а не напр., "plate_pacif")
head(MDFT)

library(car)
library(RColorBrewer)
 
data= MDFT # шаг-4. присваеваем датафрейм объекту data
 
# шаг-5. задаем цвета палитры Set2 по категории "тектоника"
my_colors <- brewer.pal(nlevels(as.factor(data$variable)), "Set2")


# шаг-6. Рисуем график // Make the plot

################# 1 вариант: тектоника + угол крутизны + подв. вулканы
scatterplotMatrix(~ tectonics + slope_angle + igneous_volc | variable, data=data ,  	
	smoother="", col=my_colors , smoother.args=list(col="grey") ,  
	regLine = list(method=lm, lty=1, lwd=1),
	lwd=0.5, pch=c(15,16,17) , 
	main="Mariana trench scatter plot (tectonics, geomorphology, magmatism) \nwith four tectonic plates options: Mariana, Caroline, Philippine, Pacific", 
	cex=1.0, cex.axis = 1.0, # cex.axis - насечки осей, cex - шрифт легенды
	legend = TRUE, cex.labels =1.3, cex.main = 1.0, ellipse=F,
	var.labels = c("tectonics", "geomorphology \n(slope angle)", "magmatism \n(igneous volcanic zones)")
	)

################# 2 вариант: глубины + мор.отложения + подв. вулканы
scatterplotMatrix(~ tectonics + Min + sedim_thick | variable, data=data , 
	smoother="", col=my_colors , smoother.args=list(col="grey") ,  
	regLine = list(method=lm, lty=1, lwd=1),
	lwd=0.5, pch=c(15,16,17) , 
	main="Mariana trench scatter plot (tectonics, bathymetry, geology) \nwith four tectonic plates options: Mariana, Caroline, Philippine, Pacific", 
	cex=1.0, cex.axis = 1.0, # cex.axis - насечки осей, cex - шрифт легенды
	legend = TRUE, cex.labels =1.3, cex.main = 1.0, ellipse=F,
	var.labels = c("tectonics", "bathymetry \n(minimal depth)", "geology \n(sedimental thickness)")
	)


################# 3 вариант: самый простой
plot(data , pch=1 , cex=0.3 , col=rgb(0.5, 0.8, 0.9, 0.7))








