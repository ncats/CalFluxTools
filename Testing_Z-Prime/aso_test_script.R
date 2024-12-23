library(aso)
library(DT)
library(ggplot2)
library(readr)
library(dplyr)
library(plotly)
library(lme4)
library(janitor)
library(devtools)
library(plotly)
library(kpodclustr)
library(tidyverse)
library(ggcorrplot)
library(caret)
library(randomForest)
library(readxl)
library(Boruta)
library(umap)
library(ComplexHeatmap)
library(forcats)
library(ggpubr)
library(gridExtra)
library(tibble)
library(glmnet)


# edit this working directory location for your directory that has the data
setwd("/Users/danyalepic/Desktop/NIH_VSOAR_2023/aso/Testing_Z-Prime")
parameterFile30 <-"FLIPR_Analysis_parameters_Run_9.2_BL_30_New_Replacement_Options.xlsx"
parameterFile60 = "FLIPR_Analysis_parameters_Run_9.2_BL_60_New_Replacement_Options.xlsx"
parameterFile90 = "FLIPR_Analysis_parameters_Run_9.2_BL_90_New_Replacement_Options.xlsx"

# this builds the aso data and exports all the qc files and transformed data
aso30 <- aso:::runDataProcessing(parameterFile30)
aso60 = aso:::runDataProcessing(parameterFile60)
aso90 = aso:::runDataProcessing(parameterFile90)

plateTransformedData = aso30@plateSet@transformedPlateData

plateMap <- aso30@plateSet@plates[[1]]@plateAnnotMap

dataType = "transformed"

if(dataType == 'transformed'){
  testData <- plateTransformedData[['60min_vs_Ref_(log2ratio)']]
} else if (dataType == "raw"){
  testData <- aso@plateSet@plates[[2]]@plateData
}

tableWithAnnotation <- cbind(plateMap, testData)

splicedData <- tableWithAnnotation[tableWithAnnotation$Compound != 'Empty',]

splicedDataClean = splicedData[splicedData$WellType == 'Test',]

#Begin LASSO regression testing
splicedDataTraining = splicedData[splicedData$WellType == 'Positive Control' |
                                    splicedData$WellType == 'Negative Control',] %>%
  subset(select = -c(Well, Compound, Spheroid, Mask, Concentration))

for(i in 1:length(splicedDataTraining$WellType)){
  if(splicedDataTraining$WellType[i] == "Positive Control"){
    splicedDataTraining$WellType[i] = as.numeric(1)
  } else {
    splicedDataTraining$WellType[i] = as.numeric(0)
  }
}

y = splicedDataTraining$WellType %>% as.numeric()
x = data.matrix(splicedDataTraining[, -1])

cv_model = cv.glmnet(x, y, alpha = 1)

#Begin Logistic Regression Testing --> NOT WORKING, USING LASSO REGRESSION INSTEAD
splicedDataTraining = splicedData[splicedData$WellType == 'Positive Control' |
                                    splicedData$WellType == 'Negative Control',] %>%
  subset(select = -c(Well, Compound, Spheroid, Mask, Concentration)) %>%
  mutate_at(vars(-WellType), as.numeric)

randomTesting = sample(c(1:100), size  = length(splicedDataTraining$WellType))
splicedDataTrainingRandom = cbind(splicedDataTraining, randomTesting)


splicedDataTesting = splicedData[splicedData$WellType == "Test",] %>%
  subset(select = -c(Well, Compound, Spheroid, Mask, Concentration))


splicedDataTrainingRandom$WellType = as.factor(splicedDataTrainingRandom$WellType)

completelyRandom = data.frame(replicate(10,sample(0:100,20,rep=TRUE)))
completelyRandom = cbind(splicedDataTraining$WellType, completelyRandom)
names(completelyRandom)[1] = "WellType"
completelyRandom$WellType = as.factor(completelyRandom$WellType)


logisticModel = glm(WellType ~ ., family = "binomial", data = completelyRandom)

logisticPredictions = predict(logisticModel, newdata = splicedDataTesting, type = "response")

test_df = cbind(splicedDataTesting, logisticPredictions)

#Testing pca for new data set
new_pca = aso:::runPca(aso90, dataType = "test", compoundIDList = c('M1_QIA', 'Veh'))
new_pca_plot = aso:::plotPCA(new_pca, pcaType = 1)
new_pca_plot = new_pca_plot + labs(title = "Wells PCA - 90 Minute") +
  theme(axis.text = element_text(size=15)) + theme(axis.title = element_text(size=15)) +
  theme(title = element_text(size = 15))

#Testing ZPrime for new data set
aso:::zFactor(aso)

#Creating ZPrime Plot
zPrimeList30 = aso:::zFactor(aso30) %>% rownames_to_column("Parameter")
zPrimeList30 = zPrimeList30[, c("Parameter", "zFactor")]
colnames(zPrimeList30)[2] = "zFactor30"

zPrimeList60 = aso:::zFactor(aso60)
zPrimeList60 = zPrimeList60[, "zFactor"] %>% as.data.frame()
colnames(zPrimeList60)[1] = "zFactor60 Original"

zPrimeList60Mask = aso:::zFactor(aso60, masking = "H15")
zPrimeList60Mask = zPrimeList60Mask[, "zFactor"] %>% as.data.frame()
colnames(zPrimeList60Mask)[1] = "zFactor60 QC"

zPrimeList90 = aso:::zFactor(aso90)
zPrimeList90 = zPrimeList90[, "zFactor"] %>% as.data.frame()
colnames(zPrimeList90)[1] = "zFactor90"

zPrimeList = cbind(zPrimeList30, zPrimeList60, zPrimeList60Mask, zPrimeList90)
zPrimeData = data.frame(matrix(NA, nrow = 4* nrow(zPrimeList), ncol = 3))

zPrimeData[1:16, 1] = "30'"
zPrimeData[17:32, 1] = "60'"
zPrimeData[33:48, 1] = "60' QC"
zPrimeData[49:64, 1] = "90'"

colnames(zPrimeData)[1] = "Time"
colnames(zPrimeData)[2] = "Value"
colnames(zPrimeData)[3] = "Parameter"

zPrimeData[1:16, 2] = zPrimeList[,2]
zPrimeData[17:32, 2] = zPrimeList[,3]
zPrimeData[33:48, 2] = zPrimeList[,4]
zPrimeData[49:64, 2] = zPrimeList[,5]

zPrimeData[1:16, 3] = zPrimeList[,1]
zPrimeData[17:32, 3] = zPrimeList[,1]
zPrimeData[33:48, 3] = zPrimeList[,1]
zPrimeData[49:64, 3] = zPrimeList[,1]

zPrimePlot = ggplot(data = zPrimeData, aes(x = Time, y = Value, color = Parameter)) +
  geom_point(size = 3) + ylab("Z Prime Value") + ggtitle("Z Prime Statistic - Quality Control") +
  annotate('rect', xmin = 0, xmax = 5, ymin = 0.5, ymax = 1, fill = 'green', alpha = 0.2) +
  annotate('rect', xmin = 0, xmax = 5, ymin = 0, ymax = 0.5, fill = 'yellow', alpha = 0.2) +
  annotate('rect', xmin = 0, xmax = 5, ymin = -2, ymax = 0, fill = 'red', alpha = 0.2) +
  geom_jitter(width = 0.2, size = 3) + theme(axis.text = element_text(size=15)) +
  theme(axis.title = element_text(size=15)) + theme(title = element_text(size = 15))


#Testing implementation of random forest function, NOT WORKING RIGHT NOW, NEED HELP
aso:::asoRandomForest(aso60)

#Begin method implementation for random forest
data_imputed<-aso60@plateSet@transformedPlateData[[1]]
sample_info <- aso60@plateSet@plates[[1]]@plateAnnotMap

data_imputed <- data_imputed[which(sample_info$Compound!="Empty"),]
sample_info <- sample_info %>%
  filter(Compound!="Empty")
data_imputed <- data_imputed %>% select_if(~ !any(is.na(.)))

#Variable Correlations
corr <- round(cor(data_imputed), 1)
ggcorrplot(corr, method = "circle", hc.order="TRUE")

#Andy's PCA, FIGURE OUT HOW TO IMPLEMENT THIS IN THE CODE
pca_res <- prcomp(data_imputed, scale.=TRUE)
pca_df <- pca_res$x[,1:2] %>%
  as.data.frame() %>%
  mutate(WellType = sample_info$WellType) %>%
  mutate(Compound = sample_info$Compound) %>%
  mutate(Well = sample_info$Well) %>%
  mutate(Concentration = as.character(sample_info$Concentration))

p <- ggplot(pca_df, aes(x=PC1,y=PC2,color=Compound)) +
  geom_point(aes(text=paste0("Well Type: ",WellType, "\nConcentration: ",Concentration, "\nWell Number: ", Well))) + theme_classic()
ggplotly(p,tooltip=c("text","color","shape"))


#Danyal's Random Forest that includes concentration as a predictor

data_labels <- pca_df$WellType
training_rows <- which(data_labels == "Positive Control" | data_labels == "Negative Control")
training_labels <- factor(data_labels[training_rows])
training_data <- data_imputed[training_rows,] %>% as.data.frame
training_data$label <-training_labels
concentration_labels <- as.numeric(pca_df$Concentration[training_rows])
new_training_data = cbind(concentration_labels, training_data)

#Generate new labels that are numeric
cont_training_data = new_training_data
updated_labels = data.frame()
for(i in 1:length(cont_training_data$concentration_labels)){
  if(cont_training_data$label[i] == "Positive Control"){
    updated_labels[i,1] = as.numeric(1)
  } else {
    updated_labels[i,1] = as.numeric(0)
  }
}

#Remove old label column and add new label column
colnames(updated_labels) = "label"
cont_training_data = subset(cont_training_data, select = -label)
cont_training_data = cbind(cont_training_data, updated_labels)

#If we want to revert to old method, change data to new_training_data
new_model <- train(label ~ .,
               data =  cont_training_data,
               method = "rf",
               trControl = trainControl(method = "cv",number=10))

#needed to change  here because vehicle and control don't have same concentrations
#can revert to original if they have the same concentrations
testing_rows = which(data_labels == 'Test' & pca_df$Compound != 'PBS')
testing_data <-  data_imputed[testing_rows,] %>% as.data.frame
clean_conc_labels = as.numeric(pca_df$Concentration[testing_rows])
new_testing_data = cbind(clean_conc_labels, testing_data)
colnames(new_testing_data)[1] = "concentration_labels"


new_test_labels <- predict(new_model, newdata = new_testing_data)
new_predictions <- data.frame(aso=pca_df$Compound[testing_rows],Concentration = pca_df$Concentration[testing_rows],
                          class=new_test_labels)
#new_predictions_pos = new_predictions %>% filter(class == 'Positive Control')


#Below is the new implementation (continuous prediction)
#List the diff compounds in aso object
compound_list = unique(new_predictions$aso)

prediction_mean = data.frame()
prediction_sd = data.frame()
prediction_stats = data.frame()

for(i in compound_list){
  #create concentration list for each compound
  conc_list = unique(new_predictions[new_predictions$aso == i, 'Concentration'])

  for(j in conc_list){
    #create class list for each concentration of each compound
    class_df = new_predictions[new_predictions$aso == i & new_predictions$Concentration == j, 'class'] %>%
      as.numeric %>%
      as.data.frame()

    #calculate average prediction and standard deviation
    mean_class = mean(class_df$.)
    sd_class = sd(class_df$.)

    prediction_mean[i,j] = mean_class
    prediction_sd[i,j] = sd_class

  }
}

#Making a combined predictions data frame that looks pretty if we want to export
aso_predictions = prediction_mean

for(i in 1:nrow(prediction_mean)){
  for(j in 1:ncol(prediction_mean)){
    aso_predictions[i,j] = paste0(format(round(prediction_mean[i,j], 4), nsmall = 4),
                                  " (", format(round(prediction_sd[i,j], 4), nsmall = 4), ")")

  }
}


#Heatmap
pred_heatmap = Heatmap(as.matrix(prediction_mean), rect_gp = gpar(col = "white", lwd = 2),
        column_title = "Concentration", column_title_side = "bottom", name = 'Prediction',
        row_title = "ASO", cluster_rows = FALSE, show_column_dend = FALSE,
        column_order = order(as.numeric(gsub("column", "", colnames(prediction_mean)))))


#Making the ggplots with error bars (can also do in excel)

pred_plot_mean = t(prediction_mean) %>% as.data.frame()
pred_plot_mean = setNames(cbind(rownames(pred_plot_mean), pred_plot_mean,
                                row.names = NULL), c("Concentration", colnames(pred_plot_mean)))

pred_plot_sd = t(prediction_sd) %>% as.data.frame()
pred_plot_sd = setNames(cbind(rownames(pred_plot_sd), pred_plot_sd,
                              row.names = NULL), c("Concentration", colnames(pred_plot_sd)))
pred_plot_sd = subset(pred_plot_sd, select = -Concentration)


for(i in 1:length(unique(colnames(pred_plot_sd)))){
  if(!grepl("sd", colnames(pred_plot_sd)[i])){
    colnames(pred_plot_sd)[i] = paste0("sd",colnames(pred_plot_sd)[i])
  }
}

#Combined data frame that has mean and sd in separate columns, can export if needed
pred_plot_df = cbind(pred_plot_mean, pred_plot_sd)

aso_plot_list = list()
for(i in 1:length(unique(new_predictions$aso))){

  concDotPlot = ggplot(data = new_predictions[new_predictions$aso == paste0('ASO', i),],
                       aes(x = Concentration, y = class)) +
    geom_dotplot(binaxis = 'y', stackdir = 'center') +
    stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                 geom="errorbar", color="red", width=0.2) +
    ggtitle(paste0('ASO', i)) + aes(x = fct_inorder(Concentration)) + xlab("Concentration") +
    coord_cartesian(ylim=c(-0.1, 1.1)) + scale_y_continuous(breaks=seq(0, 1, 0.25)) +
    geom_point(size=2) + ylab('Prediction')

  aso_plot_list[[i]] = concDotPlot

}

nrow_plot_grid = ceiling(length(aso_plot_list)/2)

pdf("ASOConcPlots.pdf")
do.call(grid.arrange, c(aso_plot_list[1:6], nrow=3, ncol=2))
do.call(grid.arrange, c(aso_plot_list[7:12], nrow=3, ncol=2))
dev.off()

#email: prediction paramater is now continuous instead of class, made Heatmap and DotPlots for each ASO
#Might work for when there are fewer replicates and any number of ASOs in plate
#Benchmark Dose Tools or Point of Departure stuff
#Curve Fitting Stuff in R resources (or python), just do some research


#Can generalize this as well, but what's the point, the dot plot is better anyways

concBarPlot = ggplot(data = pred_plot_df, aes(x = Concentration, y = ASO1)) +
  geom_bar(stat = 'identity', color = 'black', fill = 'blue', position = position_dodge()) +
  geom_errorbar(aes(ymin = ASO1 - sdASO1, ymax = ASO1 + sdASO1),
                width = 0.2, position = position_dodge(0.9)) +
  ggtitle(paste0('ASO', 1)) + aes(x = fct_inorder(Concentration)) + xlab("Concentration") +
  ylim(0, 1)


#This would be very easy to generalize with a for loop, and would create a bunch of plots
#Not exactly sure how to organize them, though, or whether that can even be automated
#It's automated now yay

concDotPlotTest = ggplot(data = new_predictions[new_predictions$aso == 'ASO1',],
                     aes(x = Concentration, y = class)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center') +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
               geom="errorbar", color="red", width=0.2) +
  ggtitle("ASO1") + aes(x = fct_inorder(Concentration))

#Below is implementation for the old method (fraction positive control if multiple replicates)

#initialize a dataframe
prediction_fraction = data.frame()

counter = 1

for(i in compound_list){
  #create concentration list for each compound
  conc_list = unique(new_predictions[new_predictions$aso == i, 'Concentration'])

  for(j in conc_list){
    #create class list for each concentration of each compound
    class_list = new_predictions[new_predictions$aso == i & new_predictions$Concentration == j, 'class'] %>%
      as.data.frame()

    #count the number of positive and negative controls
    num_pos = length(which(class_list == 'Positive Control'))
    num_neg = length(which(class_list == 'Negative Control'))

    #determine fraction of positive controls
    fraction_positive = num_pos / (num_pos+num_neg)

    prediction_fraction[i,j] = fraction_positive
}
}


#output a df, each row is an ASO, columns are concentration and then for each concentration give a fraction positive count
#essentially show a dose response outcome to summarize, at which concentration is the aso classified as toxic
#Think of the "point of departure" thing, at what point is there a significant change
#Do a heatmap of that has fraction of replicated that are positive
#Maybe aggregate is the best way to go, but if that doesn't work, manually reformat




#Determining (assess/evaluate) Anti Sense Oligonucleotides (ASOs) toxicity in neuronal organoid culture
#Automated evaluation of Anti Sense Oligonucleotide toxicity in neuronal organoid culture

#An R Package to Evaluate Anti-sense Oligonucleotide Toxicity Data in Neuronal Organoid Cultures

#keywords: automated, ASO, toxicity, neuron,






#Andy's original Random Forests Positive Predictions
data_labels <- pca_df$WellType
training_rows <- which(data_labels == "Positive Control" | data_labels == "Negative Control")
training_labels <- factor(data_labels[training_rows])
training_data <- data_imputed[training_rows,] %>% as.data.frame
training_data$label <-training_labels

model <- train(label ~ .,
               data = training_data,
               method = "rf",
               trControl = trainControl(method = "cv",number=10))

testing_data <-  data_imputed[-training_rows,] %>% as.data.frame
test_labels <- predict(model, newdata = testing_data)
predictions <- data.frame(aso=pca_df$Compound[-training_rows],Concentration = pca_df$Concentration[-training_rows],
                          class=test_labels)
predictions %>% filter(class == "Positive Control")

#variable importance
BorutaModel <- Boruta(label ~ .,
                      data = training_data)
plot(BorutaModel,las=2)

# Andy's UMAP
umap_res <- umap(data_imputed)
umap_df <- umap_res$layout %>%
  as.data.frame() %>%
  mutate(WellType = sample_info$WellType) %>%
  mutate(Compound = sample_info$Compound) %>%
  mutate(Concentration = as.character(sample_info$Concentration))

p <- ggplot(umap_df, aes(x=V1,y=V2,color=Compound)) +
  geom_point(aes(text=paste0("Well: ",Well, "\nConcentration: ",Concentration))) + theme_classic()
ggplotly(p,tooltip=c("text","color","shape"))


# all of the below is the working progress for the LME
plateTransformedData = aso@plateSet@transformedPlateData

plateMap <- aso@plateSet@plates[[1]]@plateAnnotMap

dataType = "transformed"

if(dataType == 'transformed'){
  testData <- plateTransformedData[['60min_vs_Ref_(log2ratio)']]
} else if (dataType == "raw"){
  testData <- aso@plateSet@plates[[2]]@plateData
}

tableWithAnnotation <- cbind(plateMap, testData)

splicedData <- tableWithAnnotation[tableWithAnnotation$Compound != 'Empty',]

splicedDataClean = splicedData[splicedData$WellType == 'Test',]

splicedDataCleanT = t(splicedDataClean) %>% as.data.frame()

splicedDataCleanT = splicedDataCleanT %>% row_to_names(row_number = 1)

splicedDataClean = t(splicedDataCleanT) %>% as.data.frame()

pval_thresh = 0.05

plateMapClean = plateMap[plateMap$Compound != 'Empty' & plateMap$WellType == 'Test',]

aso_concentration_3 = plateMapClean %>% filter(Concentration == 3) %>%
  dplyr::select(Well) %>%
  as.matrix() %>% as.vector

aso_concentration_10 = plateMapClean %>% filter(Concentration == 10) %>%
  dplyr::select(Well) %>%
  as.matrix() %>% as.vector

aso_concentration_30 = plateMapClean %>% filter(Concentration == 30) %>%
  dplyr::select(Well) %>%
  as.matrix() %>% as.vector

aso_concentration_60 = plateMapClean %>% filter(Concentration == 60) %>%
  dplyr::select(Well) %>%
  as.matrix() %>% as.vector

aso_compound_3 = plateMapClean %>% filter(Concentration == 3) %>%
  dplyr::select(Compound) %>%
  as.matrix() %>% as.vector

aso_compound_10 = plateMapClean %>% filter(Concentration == 10) %>%
  dplyr::select(Compound) %>%
  as.matrix() %>% as.vector

aso_compound_30 = plateMapClean %>% filter(Concentration == 30) %>%
  dplyr::select(Compound) %>%
  as.matrix() %>% as.vector

aso_compound_60 = plateMapClean %>% filter(Concentration == 60) %>%
  dplyr::select(Compound) %>%
  as.matrix() %>% as.vector

#Current problem, column names are not actually the well names
#Actually fixed this now!
fit = NULL
#if(!exists('asoConcLME')){
  asoConcLME = sapply(c(ncol(plateMap):nrow(splicedDataCleanT)), function(x){
    df = data.frame(t(splicedDataCleanT[x,c(aso_concentration_3, aso_concentration_10,
                                            aso_concentration_30, aso_concentration_60)]),
                    categ = c(aso_compound_3, aso_compound_10, aso_compound_30, aso_compound_60),
                    status = c(rep('Conc3', length(aso_concentration_3)),
                               rep('Conc10', length(aso_concentration_10)),
                               rep('Conc30', length(aso_concentration_30)),
                               rep('Conc60', length(aso_concentration_60))))
    colnames(df)[1] = "y"
    df$y = as.numeric(df$y)
    fit.null = lmer(y ~ (1|categ), data = df, REML = FALSE)
    fit = lmer(y ~ status + (1|categ), data = df, REML = FALSE)
    anovaFit = anova(fit.null, fit)
    return(fit)
    #return(anovaFit$'Pr(>Chisq)'[2])
  })
#}




for(i in 1:length(unique(plateMapClean$Concentration))){
  #Maybe think about making this more generalizable to potentially different concentrations
  #thoughts for later
}










#Calls for Danyal's PCA method below

pcaList = aso:::runPca(aso)

testPcaPlot = aso:::plotPCA(pcaList, 1)

testPcaPlot







# all of the below is my test for the PCA method
plateTransformedData = aso@plateSet@transformedPlateData
dataType = 'transformed'

if(dataType == 'transformed'){
  testData <- plateTransformedData[['60min_vs_Ref_(log2ratio)']]
} else {
  testData <- aso@plateSet@plates[[2]]@plateData
}

plateMap <- aso@plateSet@plates[[1]]@plateAnnotMap

tableWithAnnotation <- cbind(plateMap, testData)

splicedData <- tableWithAnnotation[tableWithAnnotation$Compound != 'Empty',]

#create dataframe with test data that does not include plate map
testDataSplice = splicedData %>% select(last_col(15):last_col())
testDataSpliceVar = testDataSplice[ , which(apply(testDataSplice, 2, var) != 0)]
#transpose
testDataSpliceT = t(testDataSplice)
#transposition caused columns with zero variance, remove those
testDataSpliceTvar = testDataSpliceT[ , which(apply(testDataSpliceT, 2, var) != 0)]

pcaASO = prcomp(na.omit(testDataSplice), center = TRUE, scale. = TRUE, retx = TRUE)
pcaASOT = prcomp(na.omit(testDataSpliceTvar), center = TRUE, scale. = TRUE, retx = TRUE)

pcaList = list()
pcaList[['Raw']] = pcaASO
pcaList[['Transposed']] = pcaASOT

pcaPlot = ggplot(data.frame(pcaList[['Raw']]$x), aes(x=PC1, y=PC2)) + geom_point(size = 3) + labs(title = "Raw")
pcaPlotT = ggplot(data.frame(pcaASOT$x), aes(x=PC1, y=PC2)) + geom_point(size = 3) + labs(title = "Transposed")


pcaPlot
pcaPlotT


zFactorTable <- aso:::zFactor(aso)
# Return two data frame as a result using new method

  #aso:::zFactor(aso)
  #aso:::zFactor(aso, 'experimental')

  #integrated below in analysis.R as function = zFactorXlsx
  #zPrimeList <- list()
  #zPrimeList[['transformedDataResults']] <- aso:::zFactor(aso)
  #zPrimeList[['rawDataResults']] <- aso:::zFactor(aso, 'experimental')
  #openxlsx::write.xlsx(zPrimeList,
  #                   file = paste0('zPrimeResults_', Sys.time(),'.xlsx'),
  #                                               rowNames = TRUE)
  # Figure out a way to add time/date to end of file

  # Look into Principle component analysis (PCA)
  #PCA is going to run on transformed data matrix
  #Should be it's own method, called prcomp() on the numeric matrix from transformed data
  #Two parameters that we typically set: scale and center
  #Will get a data object --> Look at str(), one list object and two matrix objects, help(prcomp)
  #Within prcomp, call na.omit(dataframe) to omit any NA values, center=TRUE, scale=TRUE, create a pca object
  #p2 <- pca$x, plot(x= , y= ), plots first column against the second column
  #use ggplot2 to visualize the data
  #Run once with and once without the "t" (for transpose), basically flips columns
  #Generate separate results
  #Mark positive and negative controls in the plot
  #Use plotly package for plotting in 3d if you need to
  #Also would like to set up the package so that after we load the data, we put all the files in a separate directory
  #Look for where we call for "dir" to export to, at that point in the code, create output directory
  #Write to output directory that would be specified in the Excel file
  #In parse --> readASOParameter() --> reads as df --> calls parseAnalysisSettings
  #Create an output directory right under root directory
  #allParamsList is a property of aso object, then we can reference output directory


# All of the below is code I initially used in this file, but is now shuttled
# to the analysis file under the zFactor function.

# The @ is used to dereference
# Here we have the aso object, dereference the plateset object, and finally get the transformedPlateData
# it's designed as a list or dictionary
plateTransformedData <- aso@plateSet@transformedPlateData

# in this case theres only one transformed data frame
# this will list the names of the transformations
names(plateTransformedData)

# an R list object is dereferenced using double brackets
# you can refer to a member of the list by name or by index. *R indexing starts at 1
transformedData <- plateTransformedData[['60min_vs_Ref_(log2ratio)']]
#transformedData <- plateTransformedData[[1]]

# dimensions of the transformed data
# this currently has the empty data
# we might rework the data transformation to not include masked wells and emtpy wells
dim(transformedData)

# column names of the tranformed data
colnames(transformedData)

# the platemap holds well annotations, it's a dataframe
# this gets the plateset from the aso object, plate list, takes the first plate, and then
# the plateAnnotationMap field.
plateMap <- aso@plateSet@plates[[1]]@plateAnnotMap


# DT is just a library for showing data tables.
DT::datatable(plateMap)
# an alternative to viewing a table is using the 'Global Envionment' in the upper right window.
# if the variable is a data.frame, click on the table icon on the far right to view the data.






zFactor <- function(aso, analysisParameter){

  # Combines plateMap and transformedData
  tableWithAnnotation <- cbind(plateMap, transformedData)

  # Created a data frame that does not contain empty wells
  splicedData <- tableWithAnnotation[tableWithAnnotation$Compound != 'Empty',]

  # Create a data frame that only contains positive controls
  PosCtrl = splicedData[splicedData$WellType == "Positive Control",]
  # unique(PosCtrl$WellType)

  # Create a data frame that only contains negative controls
  NegCtrl = splicedData[splicedData$WellType == "Negative Control",]
  # unique(NegCtrl$WellType)

  #define parameters for means and standard deviations
  #Need to pass argument in as a string
  sdNegCtrl = sd(NegCtrl[[analysisParameter]])
  sdPosCtrl = sd(PosCtrl[[analysisParameter]])
  uNegCtrl = mean(NegCtrl[[analysisParameter]])
  uPosCtrl = mean(PosCtrl[[analysisParameter]])

  zFactor = 1 - ((3*(sdPosCtrl+sdNegCtrl))/(abs(uPosCtrl - uNegCtrl)))

  return(zFactor)
}

zFactor(aso, "PkAmp")

#Titanic testing
training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""))

sapply(training.data.raw,function(x) sum(is.na(x)))

sapply(training.data.raw, function(x) length(unique(x)))

missmap(training.data.raw, main = "Missing values vs observed")

data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))

data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

data$Sex = as.factor(data$Sex)
data$Embarked = as.factor(data$Embarked)

is.factor(data$Sex)

is.factor(data$Embarked)

contrasts(data$Sex)
contrasts(data$Embarked)

data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

train <- data[1:800,]
test <- data[801:889,]

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)

summary(model)
