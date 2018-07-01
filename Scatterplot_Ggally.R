# Scatterplot matrix // матрица графиков диаграммы рассеяния (точечной диаграммы) через библиотеку GGally
require(GGally)

# ЧАСТЬ 1: делаем data.frame
	# шаг-1. вчитываем таблицу. делаем из нее датафрейм.
MDFGeo <- read.csv("GeoMorphology.csv", header=TRUE, sep = ",")
	# шаг-2. чистим датафрейм от NA значений
MDFGeo <- na.omit(MDFGeo) 
row.has.na <- apply(MDFGeo, 1, function(x){any(is.na(x))}) 
sum(row.has.na) 
head(MDFGeo) 
 
# Check correlation between variables
cor(MDF) 
 
# Check correlations (as scatterplots), distribution and print corrleation coefficient 
pi<- ggpairs(MDFGeo, axisLabels = "show") 

pair<- pi + theme(axis.text.x = element_text(face = 3, color = "gray24", size = 6, angle = 15),
		axis.text.y = element_text(face = 3, color = "gray24", size = 6, angle = 15))
pair

 
# простой вариант визуализации Visualization of correlations
ggcorr(data=MDF, method = c("everything", "pearson")) 
        

 ####################### ЧАСТЬ-2. Корреляционные матрицы 3-мя методами
MDF <- read.csv("Morphology.csv", header=TRUE, sep = ",")
	# шаг-2. чистим датафрейм от NA значений
MDF <- na.omit(MDF) 
row.has.na <- apply(MDF, 1, function(x){any(is.na(x))}) 
sum(row.has.na) 
head(MDF) 
 
# Check correlation between variables
cor(MDF) 
 
# Visualization of correlations
	# вариант-1. Pearson correlation coefficients, using pairwise observations (default method)	
gp<- ggcorr(data=MDF, method = c("everything", "pearson"), 
	name = "\nPearson \ncorrelation \nmethod \n(parametric)",
	label = TRUE, label_size = 2, label_round = 2, label_alpha = TRUE, 
	hjust = 0.75, size = 3, color = "grey50", legend.position = "left")
gpt<- gp + labs(title="Mariana Trench", 
	subtitle = "Correlation of Geomorphlogical Impact Factors \nPearson correlation method (parametric)",
	caption = "Statistics Processing and Graphs: \nR Programming. Data Source: QGIS") +
	theme(plot.title = element_text(family = "Times New Roman", face = 2, size = 12),
		plot.subtitle = element_text(family = "Times New Roman", face = 1, size = 10),
		plot.caption = element_text(family = "Times New Roman", face = 2, size = 8))
gpt

	# вариант-2. Spearman correlation coefficients, using strictly complete observations
gs<-ggcorr(data=MDF, method = c("everything", "spearman"), geom = "circle", nbreaks = 5, 
	min_size = 3, max_size = 9, palette = "PiYG",
	name = "\nSpearman \ncorrelation \nmethod \n(non-parametric)",
	label = TRUE, label_size = 2, label_round = 2, label_alpha = TRUE, 
	hjust = 0.75, size = 3, color = "grey50", legend.position = "left")
gs	
gst<- gs + labs(title="Mariana Trench", 
	subtitle = "Correlation of Geomorphlogical Impact Factors \nSpearman correlation method \n(nonparametric measure of rank using monotonic function)",
	caption = "Statistics Processing and Graphs: \nR Programming. Data Source: QGIS") +
	theme(plot.title = element_text(family = "Times New Roman", face = 2, size = 12),
		plot.subtitle = element_text(family = "Times New Roman", face = 1, size = 10),
		plot.caption = element_text(family = "Times New Roman", face = 2, size = 8))
gst

	# вариант-3. Kendall correlation coefficients, using complete observations
gk<- ggcorr(data=MDF, method = c("complete", "kendall"), 
	geom = "text", nbreaks = 5, palette = "RdYlBu", hjust = 1, label = TRUE, label_alpha = 0.4)
gk
gkt<- gk + labs(title="Mariana Trench", 
	subtitle = "Correlation of Geomorphlogical Impact Factors \nKendall correlation coefficients, \nusing complete observations",
	caption = "Statistics Processing and Graphs: \nR Programming. Data Source: QGIS")
gkt

figure <-plot_grid(gpt, gst, labels = c("1", "2"), ncol = 2, nrow = 1)
figure

