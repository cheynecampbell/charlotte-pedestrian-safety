# Cheyne Campbell 
# Charlotte, NC Pedestrian Safety
# UCL Dissertation 2020

# RANDOM FOREST AND NEGATIVE BINOMIAL REGRESSION

#---------------------------------------------------------------------------------------------------------------

# PREP ROAD VARIABLES DATA

# show decimal values
options(scipen = 999)

# extract continuous and categorical variables seperately
variablesCont <- roadsUptownFiltDF[ , grepl("variable_cont", names(roadsUptownFiltDF))] %>% as.data.frame()
variablesCat  <- roadsUptownFiltDF[ , grepl("variable_cat" , names(roadsUptownFiltDF))] %>% as.data.frame()

# get all variables and rename columns
roadVariables <- roadsUptownFiltDF[ , grepl("variable_", names(roadsUptownFiltDF))] %>% as.data.frame()
roadVariables <- roadVariables %>% dplyr::rename_all(funs(stringr::str_replace_all(., "variable_cont_", "")))
roadVariables <- roadVariables %>% dplyr::rename_all(funs(stringr::str_replace_all(., "variable_cat_", "")))

# make sure no variables have empty factor levels
roadVariables <- droplevels(roadVariables, exclude = if(anyNA(levels(roadVariables))) NULL else NA)
str(roadVariables)

# get imputed values for correlation matrix
variablesCont.imputed <- randomForest::rfImpute(variable_cont_crashes ~., data = variablesCont, iter = 6)
variablesCont.imputed <- sapply(variablesCont.imputed, as.numeric) %>% as.data.frame()
variablesCont.imputed <- variablesCont.imputed %>% dplyr::rename_all(funs(stringr::str_replace_all(., "variable_cont_", "")))

# get correlation matrix for continuous variables
corMatrix <- stats::cor(variablesCont.imputed, method = "pearson") 

# plot and save correlation matrix
png(file = "./Figures/corrMatrix.png", height = 6, width = 8, units = "in", res = 300)
ggcorrplot(corMatrix, method = "circle", colors = c("orangered", "#FFFFBF", "deepskyblue3"))
dev.off()

#---------------------------------------------------------------------------------------------------------------

# RANDOM FOREST FOR FEATURE IMPORTANCE

# allow for reproducible results
set.seed(1234)

# seperate training and testing data
sepRoads <- sample(2, nrow(roadVariables), replace = TRUE, prob = c(0.9, 0.1))
trainData <- roadVariables[sepRoads == 1, ]
testData  <- roadVariables[sepRoads == 2, ]

# make model and generate predictions
roads_cforest <- party::cforest(crashes ~ ., data = trainData, controls = cforest_unbiased(ntree = 500, mtry = 8))
testData$predictions <-predict(roads_cforest, newdata = testData, OOB = TRUE, type = "response")

# get metrics
signif(MLmetrics::MAE(testData$predictions, testData$crashes), 3)  
signif(MLmetrics::MSE(testData$predictions, testData$crashes), 3)  
signif(MLmetrics::RMSE(testData$predictions, testData$crashes), 3) 

# get variable importance measures (mean decrease in accuracy)
roads_varimp <- party::varimp(roads_cforest) %>% as.data.frame()

# edit data frame
roads_varimp$predictor <- rownames(roads_varimp)
colnames(roads_varimp) <- c("mean_decrease_in_accuracy", "predictor")

# plot variable importance
varimpPlot <- ggplot(data = roads_varimp, aes(x = predictor, y = mean_decrease_in_accuracy)) +
  geom_segment( aes(x = predictor, xend = predictor, y = 0, yend = mean_decrease_in_accuracy), color = "skyblue") +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  theme_light() +
  coord_flip() +
  labs(y = "Mean Decrease in Accuracy", x = "Predictor") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank())

# save variable important plot
png(file = "./Figures/variableImportance.png", height = 6, width = 8, units = "in", res = 300)
varimpPlot
dev.off()

# get table of variable importance values 
roads_varimp_table <- roads_varimp %>% dplyr::select(predictor, mean_decrease_in_accuracy)
roads_varimp_table$mean_decrease_in_accuracy <- signif(roads_varimp_table$mean_decrease_in_accuracy, 3)
write.table(roads_varimp_table, "./Data/variableImportance.csv", row.names = FALSE, sep = "&", eol = "\\\n", append = FALSE)

#---------------------------------------------------------------------------------------------------------------

# PLOT TREE EXTRACTED FROM RANDOM FOREST
# FUNCTIONS FROM SOURCE: https://stackoverflow.com/questions/19924402/cforest-prints-empty-tree

get_cTree <- function(cf, k=1) {
  dt <- cf@data@get("input")
  tr <- party:::prettytree(cf@ensemble[[k]], names(dt))
  tr_updated <- update_tree(tr, dt)
  new("BinaryTree", tree=tr_updated, data=cf@data, responses=cf@responses, 
      cond_distr_response=cf@cond_distr_response, predict_response=cf@predict_response)
}

update_tree <- function(x, dt) {
  x <- update_weights(x, dt)
  if(!x$terminal) {
    x$left <- update_tree(x$left, dt)
    x$right <- update_tree(x$right, dt)   
  } 
  x
}

update_weights <- function(x, dt) {
  splt <- x$psplit
  spltClass <- attr(splt,"class")
  spltVarName <- splt$variableName
  spltVar <- dt[,spltVarName]
  spltVarLev <- levels(spltVar)
  if (!is.null(spltClass)) {
    if (spltClass=="nominalSplit") {
      attr(x$psplit$splitpoint,"levels") <- spltVarLev   
      filt <- spltVar %in% spltVarLev[as.logical(x$psplit$splitpoint)] 
    } else {
      filt <- (spltVar <= splt$splitpoint)
    }
    x$left$weights <- as.numeric(filt)
    x$right$weights <- as.numeric(!filt)
  }
  x
}

# save tree plot 
png(file = "./Figures/decisionTree96.png", height = 10000, width = 15000, res = 300)
plot(get_cTree(roads_cforest, 96)) 
dev.off()

#---------------------------------------------------------------------------------------------------------------

# NEGATIVE BINOMIAL REGRESSION 

# factor variables must have at least 2 levels
levels(roadVariables$leftshoulder) <- c(levels(roadVariables$leftshoulder), "Curb-Bit")

# compare unconditional mean and variance - mean is lower than variance so go with neg. binom. reg.
signif(mean(roadVariables$crashes), 3)
signif(var(roadVariables$crashes), 3)

# get winning variables (mean decrease in accuracy greater than 0) 
winVar <- roads_varimp[roads_varimp$mean_decrease_in_accuracy > 0, 2]

#-----------------------------------------------------------------------------

# NOTE: uncocomment code in this secton to run process for finding best formula 
# combination based on AIC; takes a while so outcome is also stored in variable below

# list to save unique formula combinations
# formulaCombinations <- c()

# get all unique formula combinations - include 2-14 variables
# for (x in 2:15) {
#   newCombs <- combn(winVar, x, simplify = FALSE)
#   for (comb in newCombs) {
#     form <- as.formula( paste( "crashes~", paste(comb, collapse = "+"), sep = ""))
#     formulaCombinations <- list.append(formulaCombinations, form)
#   }
# }

# function to get AIC values for NBR formula combination
# getAIC <- function(formula){
#  selectData <- model.frame(formula, data = roadVariables, na.action = na.pass)
#  NBR <- tryCatch(glm.nb(formula, data = selectData, control = glm.control(maxit = 500), na.action = na.omit),
#                  error = function(e) NULL)
#  if (!is.null(NBR)) {
#    AIC <- extractAIC(NBR)
#    return(AIC[[2]])
#  } else {
#      return(NA)
#    }
# }

# get AIC values for each formula combinationas a list
# AICValues_NBR <- c()
# AICValues_NBR <- lapply(formulaCombinations, getAIC)

# get formula used to achieve lowest AIC value - outcomes saved below 
# formula_NBR <- formulaCombinations[[which.min(AICValues_NBR)]]
# AIC_NBR <- AICValues_NBR[[which.min(AICValues_NBR)]]

#-----------------------------------------------------------------------------

# best formula and corresponding AIC value
formula_NBR <- crashes ~ traveldirection + thrulanes + length_ft + housingunits + lightrail + trafficsignals + commercial + edgebetw
AIC_NBR <- 833.3409

# final model and summary
NBR_FINAL <- glm.nb(formula_NBR, data = roadVariables, control = glm.control(maxit = 500))
summary(NBR_FINAL)

#---------------------------------------------------------------------------------------------------------------

# USE NBR MODEL TO PREDICT CRASHES

# get imputed values to deal with missing observations 
roadVariables.imputed <- randomForest::rfImpute(crashes ~., data = roadVariables, iter = 6)

# get negative binomial model predictions
roadsUptownFilt$NBR_predictions <- predict(NBR_FINAL, roadVariables.imputed, type = "response")

# get metrics
signif(MLmetrics::MAE(roadsUptownFilt$NBR_predictions, roadsUptownFilt$variable_cont_crashes), 3)  
signif(MLmetrics::MSE(roadsUptownFilt$NBR_predictions, roadsUptownFilt$variable_cont_crashes), 3)  
signif(MLmetrics::RMSE(roadsUptownFilt$NBR_predictions, roadsUptownFilt$variable_cont_crashes), 3) 

# adjusted mapping function - same style are variable maps
networkMapCont <- function(variable, variableDescr) {
  variableMap <- ggplot() +
    geom_sf(data = st_as_sf(roadsUptownFilt), aes(color = variable, size = variable)) + 
    scale_size(range = c(0.5, 1.0), guide = "none") + 
    scale_fill_distiller(palette = "Spectral", aesthetics = c("colour", "fill"), 
                         guide = guide_colorbar(barheight = 5, ticks = FALSE), na.value = "darkgrey") +
    labs(colour = variableDescr) + 
    theme_classic() + 
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
          axis.line = element_blank(),
          axis.ticks = element_blank(), 
          axis.text = element_text(size = 6),
          legend.background = element_rect(fill = "transparent"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.justification = c(0, 0), legend.position = c(0, 0),
          legend.box.margin = ggplot2::margin(c(10,10,10,10)))
  
  # check output
  print(variableMap)
}

# plot actual crash data
png(file = "./Figures/actualCrashes.png", height = 6, width = 8, units = "in", res = 300)
networkMapCont(roadsUptownFilt$variable_cont_crashes, "Actual Crashes")
dev.off()

# plot modeled crash data
png(file = "./Figures/predictedNBR.png", height = 6, width = 8, units = "in", res = 300)
networkMapCont(roadsUptownFilt$NBR_predictions, "Negative Binomial\nPredicted Crashes")
dev.off()

#---------------------------------------------------------------------------------------------------------------

# PLOT MOST DANGEROUS ROADS AND HIGH INJURY NETWORK

# get top 1% and top 25% most dangerous roads
top1 <- top_frac(st_as_sf(roadsUptownFilt), 0.01, NBR_predictions)
top25 <-top_frac(st_as_sf(roadsUptownFilt), 0.25, NBR_predictions)

# upload High Injury Network
# SOURCE: https://data.charlottenc.gov/datasets/4a27df737ba24be7acf1314f38925a4f_87
HIN <- st_read("./Data/High_Injury_Network/High_Injury_Network.shp")
HIN_Uptown <- HIN[Uptown, ]

# plot top 1% and 25% most dangerous road segments and HIN
mostDangerous <- ggplot() +
  geom_sf(data = st_as_sf(roadsUptownFilt), aes(color = "d"), size = 0.25, key_glyph = draw_key_path) +
  geom_sf(data = HIN_Uptown, aes(color = "c"), size = 1.25, key_glyph = draw_key_path) +
  geom_sf(data = top25, aes(color = "b"), size = 0.75, key_glyph = draw_key_path) +
  geom_sf(data = top1, aes(color = "a"), size = 0.50, key_glyph = draw_key_path) +
  scale_colour_manual("", 
                      values = c(a = "yellow", b = "orangered", d = "darkgrey", c = "slateblue4"), 
                      labels = c("Top 1%", "Top 25%", "High Injury Network", "All Roads")) +
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_text(size = 6),
        legend.background = element_rect(fill = "transparent"), legend.title = element_text(size = 10),
        legend.text = element_text(size = 8), legend.justification = c(0, 0), legend.position = c(0, 0),
        legend.box.margin = ggplot2::margin(c(10,10,10,10)))

# save map of most dangerous roads
png(file = "./Figures/mostDangerous.png", height = 6, width = 8, units = "in", res = 300)
mostDangerous
dev.off()

# get table of most dangerous roads
dangerous_roads_table_og <- top1 %>% dplyr::select(StreetName, variable_cont_crashes, NBR_predictions)
dangerous_roads_table_og %>% mutate_at(vars(NBR_predictions), funs(round(., 0)))
write.table(dangerous_roads_table_og, "./Data/dangerousRoads.csv", row.names = FALSE, sep = "&", eol = "\\\n", append = FALSE)

#---------------------------------------------------------------------------------------------------------------

# MAKE CHANGES HYPOTHETICAL CHANGES TO ROADS AND PREDICT CRASHES

# give roads one direction and one thru lane
hypothetical_roadVariables.imputed <- roadVariables.imputed
hypothetical_roadVariables.imputed$traveldirection <- "One-way"
hypothetical_roadVariables.imputed$thrulanes <- 1

# generate predictions
roadsUptownFilt$NBR_predictions_hypo <- predict(NBR_FINAL, hypothetical_roadVariables.imputed, type = "response")

# extract needed roads 
top1_names <- c("W TRADE ST", "E 6TH ST", "E 4TH ST")
top1_ids <- c(683027, 683037, 827182)
hypothetical_top1 <- roadsUptownFilt[is.element(roadsUptownFilt$StreetName, top1_names) & is.element(roadsUptownFilt$OBJECTID, top1_ids), ] %>%
  as.data.frame()

# get table of most dangerous roads with hypothetical adjustments
dangerous_roads_table <- hypothetical_top1 %>% dplyr::select(StreetName, variable_cont_crashes, NBR_predictions, NBR_predictions_hypo)
dangerous_roads_table$perc_change <- ((dangerous_roads_table$NBR_predictions - dangerous_roads_table$NBR_predictions_hypo) / dangerous_roads_table$NBR_predictions) * 100 
dangerous_roads_table$perc_change <- signif(dangerous_roads_table$perc_change, 3)  

dangerous_roads_table %>% mutate_at(vars(NBR_predictions, NBR_predictions_hypo), funs(round(., 0)))
write.table(dangerous_roads_table, "./Data/dangerousRoads_hypothetical.csv", row.names = FALSE, sep = "&", eol = "\\\n", append = FALSE)

#---------------------------------------------------------------------------------------------------------------

