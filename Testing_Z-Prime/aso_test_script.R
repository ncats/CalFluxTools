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


# edit this working directory location for your directory that has the data
setwd("/Users/danyalepic/Desktop/NIH_VSOAR_2023/aso/Testing_Z-Prime")
parameterFile <-"FLIPR_Analysis_parameters_Run_7_New_Replacement_Options.xlsx"

# this builds the aso data and exports all the qc files and transformed data
aso <- aso:::runDataProcessing(parameterFile)

#all of the below is using Andy's Random Forest but adding concentration

data_imputed<-aso@plateSet@transformedPlateData[[1]]
sample_info <- aso@plateSet@plates[[1]]@plateAnnotMap

data_imputed <- data_imputed[which(sample_info$Compound!="Empty"),]
sample_info <- sample_info %>%
  filter(Compound!="Empty")
data_imputed <- data_imputed %>% select_if(~ !any(is.na(.)))

#Variable Correlations
corr <- round(cor(data_imputed), 1)
ggcorrplot(corr, method = "circle", hc.order="TRUE")

#Andy's PCA
pca_res <- prcomp(data_imputed, scale.=TRUE)
pca_df <- pca_res$x[,1:2] %>%
  as.data.frame() %>%
  mutate(WellType = sample_info$WellType) %>%
  mutate(Compound = sample_info$Compound) %>%
  mutate(Concentration = as.character(sample_info$Concentration))

p <- ggplot(pca_df, aes(x=PC1,y=PC2,color=Compound)) +
  geom_point(aes(text=paste0("Well Type: ",WellType, "\nConcentration: ",Concentration))) + theme_classic()
ggplotly(p,tooltip=c("text","color","shape"))

#UMAP
umap_res <- umap(data_imputed)
umap_df <- umap_res$layout %>%
  as.data.frame() %>%
  mutate(WellType = sample_info$WellType) %>%
  mutate(Compound = sample_info$Compound) %>%
  mutate(Concentration = as.character(sample_info$Concentration))

p <- ggplot(umap_df, aes(x=V1,y=V2,color=Compound)) +
  geom_point(aes(text=paste0("Well: ",Well, "\nConcentration: ",Concentration))) + theme_classic()
ggplotly(p,tooltip=c("text","color","shape"))


#Danyal's Random Forest that includes concentration as a predictor

data_labels <- pca_df$WellType
training_rows <- which(data_labels == "Positive Control" | data_labels == "Negative Control")
training_labels <- factor(data_labels[training_rows])
training_data <- data_imputed[training_rows,] %>% as.data.frame
training_data$label <-training_labels
concentration_labels <- pca_df$Concentration[training_rows]
new_training_data = cbind(concentration_labels, training_data)

new_model <- train(label ~ .,
               data = new_training_data,
               method = "rf",
               trControl = trainControl(method = "cv",number=10))

testing_data <-  data_imputed[-training_rows,] %>% as.data.frame
clean_conc_labels = pca_df$Concentration[-training_rows]
new_testing_data = cbind(clean_conc_labels, testing_data)

test_labels <- predict(new_model, newdata = new_testing_data)
predictions <- data.frame(aso=pca_df$Compound[-training_rows],Concentration = pca_df$Concentration[-training_rows],
                          class=test_labels)
predictions %>% filter(class == "Positive Control")

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
if(!exists('asoConcLME')){
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
    return(anovaFit$'Pr(>Chisq)'[2])
  })
}




for(i in 1:length(unique(plateMapClean$Concentration))){
  #Maybe think about making this more generalizable to potentially different concentrations
  #thoughts for later
}










#Calls for PCA method below

pcaList = aso:::runPca(aso, dataType = 'transformed', compoundIDList = c('IDT', 'PBS', 'QIA', 'Veh'))

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
