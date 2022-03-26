#Download and clean the datasets in R

library(rsma)
library(rsma.eclipse)
library(rsma.nasa)

data(eclipse_ckoo)
data(nasa_new1, nasa_new2, nasa_new3, nasa_new4, nasa_new5)

library(dplyr)
eclipse_ckoo <- eclipse_ckoo[, 1:20]
for (i in 1 : 5371) {
    if (eclipse_ckoo[i, 20] > 0) {
        eclipse_ckoo[i, 20] <- 1
    }
}
rm(i)

eclipse_ckoo <- rename(eclipse_ckoo, Defective = bugs)
nasa_new1 <- nasa_new1 %>% mutate(Defective = ifelse(Defective == "N", 0, 1))
nasa_new2 <- rename(nasa_new2, Defective = label)
nasa_new2 <- nasa_new2 %>% mutate(Defective = ifelse(Defective == "N", 0, 1))
nasa_new3 <- nasa_new3 %>% mutate(Defective = ifelse(Defective == "N", 0, 1))
nasa_new4 <- nasa_new4 %>% mutate(Defective = ifelse(Defective == "N", 0, 1))
nasa_new5 <- nasa_new5 %>% mutate(Defective = ifelse(Defective == "N", 0, 1))

eclipse <- eclipse_ckoo %>% filter(project == "eclipse")
equinox <- eclipse_ckoo %>% filter(project == "equinox")
lucene <- eclipse_ckoo %>% filter(project == "lucene")
mylyn <- eclipse_ckoo %>% filter(project == "mylyn")
pde <- eclipse_ckoo %>% filter(project == "pde")
rm(eclipse_ckoo)

unique(nasa_new1$project)
cm1 <- nasa_new1 %>% filter(project == "cm1")
mw2 <- nasa_new1 %>% filter(project == "mw2")
pc1 <- nasa_new1 %>% filter(project == "pc1")
pc3 <- nasa_new1 %>% filter(project == "pc3")
pc4 <- nasa_new1 %>% filter(project == "pc4")
unique(nasa_new2$project)
jm1 <- nasa_new2 %>% filter(project == "jm1")
kc1 <- nasa_new2 %>% filter(project == "kc1")
unique(nasa_new3$project)
kc3 <- nasa_new3 %>% filter(project == "kc3")
mc2 <- nasa_new3 %>% filter(project == "mc2")
unique(nasa_new4$project)
mc1 <- nasa_new4 %>% filter(project == "mc1")
pc5 <- nasa_new4 %>% filter(project == "pc5")
unique(nasa_new5$project)
pc2 <- nasa_new5 %>% filter(project == "pc2")
rm(nasa_new1, nasa_new2, nasa_new3, nasa_new4, nasa_new5)

# To download the datasets refer to Chapter 2, Methodology.
# The links are available at [1] and [2].

library(dplyr)

eclipse <- eclipse[, 2:19]
equinox <- equinox[, 2:19]
lucene <- lucene[, 2:19]
mylyn <- mylyn[, 2:19]
pde <- pde[, 2:19]

list <- list(eclipse, equinox, lucene, mylyn, pde,
             cm1, mw2, pc1, pc3, pc4, jm1, kc1, kc3, mc2, mc1, pc5, pc2)



# Produce overview table for Eclipse projects

eclipse_projects <- list(eclipse, equinox, lucene, mylyn, pde)

n_classes <- c()
n_defective <- c()
pct_defective <- c()

for (i in 1:5) {
  
  n_classes[i] <- nrow(eclipse_projects[[i]])
  n_defective[i] <- table(eclipse_projects[[i]]$Defective)[2]
  pct_defective[i] <- n_defective[i] / n_classes[i]
  
}



# Shapiro-Wilk test on the Eclipse projects}

w_eclipse <- c()
w_equinox <- c()
w_lucene <- c()
w_mylyn <- c()
w_pde <- c()

pvalue_eclipse <- c()
pvalue_equinox <- c()
pvalue_lucene <- c()
pvalue_mylyn <- c()
pvalue_pde <- c()


for(i in 1:17) {
  
  shap_result <- shapiro.test(eclipse[, i])
  w_eclipse[i] <- shap_result$statistic
  pvalue_eclipse[i] <- shap_result$p.value
  
  shap_result <- shapiro.test(equinox[, i])
  w_equinox[i] <- shap_result$statistic
  pvalue_equinox[i] <- shap_result$p.value
  
  shap_result <- shapiro.test(lucene[, i])
  w_lucene[i] <- shap_result$statistic
  pvalue_lucene[i] <- shap_result$p.value
  
  shap_result <- shapiro.test(mylyn[, i])
  w_mylyn[i] <- shap_result$statistic
  pvalue_mylyn[i] <- shap_result$p.value
  
  shap_result <- shapiro.test(pde[, i])
  w_pde[i] <- shap_result$statistic
  pvalue_pde[i] <- shap_result$p.value
  
}

variable_names <- names(eclipse)[-18]
shapiro_data <- data.frame(variable_names, w_eclipse, pvalue_eclipse,
                           w_equinox, pvalue_equinox,
                           w_lucene, pvalue_lucene,
                           w_mylyn, pvalue_mylyn,
                           w_pde, pvalue_pde)

table_eclipse_overview <- data.frame(n_classes, n_defective, pct_defective)


# Correlations of Eclipse datasets} 

library(PerformanceAnalytics)
library(corrplot)

cor_eclipse <- cor(eclipse[, -18], method = "spearman")
chart.Correlation(cor_eclipse)
heatmap(cor_eclipse)
cor_equinox <- cor(equinox[, -18], method = "spearman")
heatmap(cor_equinox)
cor_lucene <- cor(lucene[, -18], method = "spearman")
heatmap(cor_lucene)
cor_mylyn <- cor(mylyn[, -18], method = "spearman")
heatmap(cor_mylyn)
cor_pde <- cor(pde[, -18], method = "spearman")
heatmap(cor_pde)


# Produce overview table for NASA projects} 

Group <- c("1", "1", "1", "1", "1", "2", "2","3", "3", "4", "4", "5")
Project <- c("cm1", "mw2", "pc1", "pc3", "pc4", "jm1", "kc1", "kc3", "mc2",
             "mc1", "pc5", "pc2")
n_variables <- c(37, 37, 37, 37, 37, 21, 21, 39, 39, 38, 38, 36) # excluding response variable
nasa_projects <- list(cm1, mw2, pc1, pc3, pc4, jm1, kc1, kc3, mc2, mc1, pc5, pc2)

n_obs <- c()
n_defective <- c()
pct_defective <- c()

for (i in 1:12) {
  
  n_obs[i] <- nrow(nasa_projects[[i]])
  n_defective[i] <- table(nasa_projects[[i]]$Defective)[2]
  pct_defective[i] <- n_defective[i] / n_obs[i]
  
}

table_nasa_overview <- data.frame(Group, Project, n_variables, 
                                  n_obs, n_defective, pct_defective)


# Shapiro-Wilk test on the NASA projects

# group 1

group1_names <- names(cm1)[-38]

shapiro <- function(data) {
  
  W <- c()
  p <- ncol(data)
  
  for (i in 1 : (p - 1)) {
    
    W_test <- shapiro.test(data[, i])
    W[i] <- W_test$statistic
    
  }
  
  return(W)
  
}

W_CM1 <- shapiro(cm1)
W_MW2 <- shapiro(mw2)
W_PC1 <- shapiro(pc1)
W_PC3 <- shapiro(pc3)
W_PC4 <- shapiro(pc4)

group1_table <- data.frame(group1_names, W_CM1, W_MW2, W_PC1, W_PC3, W_PC4)
print(xtable(group1_table, type = "latex"), file = "group1_table.tex")

# group 2

group2_names <- names(jm1)[-22]
W_JM1 <- shapiro(jm1[1:5000,])
W_KC1 <- shapiro(kc1)
group2_table <- data.frame(group2_names, W_JM1, W_KC1)
print(xtable(group2_table, type = "latex"), file = "group2_table.tex")

# group 3

group3_names <- names(kc3)[-40]
W_KC3 <- shapiro(kc3)
W_MC2 <- shapiro(mc2)
group3_table <- data.frame(group3_names, W_KC3, W_MC2)
print(xtable(group3_table, type = "latex"), file = "group3_table.tex")

# group 4

group4_names <- names(mc1)[-39]
W_MC1 <- shapiro(mc1)
W_PC5 <- shapiro(pc5)
group4_table <- data.frame(group4_names, W_MC1, W_PC5)
print(xtable(group4_table, type = "latex"), file = "group4_table.tex")

# group 5

group5_names <- names(pc2)[-37]
W_PC2 <- shapiro(pc2)
group5_table <- data.frame(group5_names, W_PC2)
print(xtable(group5_table, type = "latex"), file = "group5_table.tex")


# Correlations of NASA datasets} 

library(PerformanceAnalytics)
library(corrplot)

cor_cm1 <- cor(cm1[, -38], method = "spearman")
cor_mw2 <- cor(mw2[, -38], method = "spearman")
cor_pc1 <- cor(pc1[, -38], method = "spearman")
cor_pc3 <- cor(pc3[, -38], method = "spearman")
cor_pc4 <- cor(pc4[, -38], method = "spearman")
cor_jm1 <- cor(jm1[, -22], method = "spearman")
cor_kc1 <- cor(eclipse[, -22], method = "spearman")
cor_kc3 <- cor(kc3[, -40], method = "spearman")
cor_mc2 <- cor(mc2[, -40], method = "spearman")
cor_mc1 <- cor(mc1[, -39], method = "spearman")
cor_pc5 <- cor(pc5[, -39], method = "spearman")
cor_pc2 <- cor(pc2[, -37], method = "spearman")

chart.Correlation(cor_cm1[18:25,18:25])
chart.Correlation(cor_cm1[7:15,7:15])


# R software metrics analysis functions

# The following functions are used to proceed further in the reproduction of 
# results. They are necessary before reproducing most of the R code of the 
# following sections, both for feature selection and application of supervised 
# and unsupervised methods.

add.response <- function(pca_data, data){
  
  p <- dim(data)[2]
  pca_p <- dim(pca_data)[2] + 1
  
  pca_data <- data.frame(pca_data, data[, p])
  names(pca_data)[pca_p] <- "bugs"
  
  return(pca_data)
}


binary.trans <- function(data, response){
  
  n <- dim(data)[1]
  p <- dim(data)[2]
  
  if(missing(response)){
    response <- p
  }
  
  for(i in 1 : n){
    if(data[i, response] > 0){
      data[i, response] <- 1
    }
    i <- i + 1
  }
  
  # data[, response] <- as.factor(data[, response])
  return(data)
}


select.backward <- function(data, response, type){
  
  # Type is linear by default
  if(missing(type)){
    type <- "linear"
  }
  
  ncol <- dim(data)[2]
  # By default the response variable is the last one
  if(missing(response)){
    data <- data
    response <- dim(data)[2]
    name_response <- names(data)[response]
  }else{
    if(dim(data)[2] == response){
      data <- data
      name_response <- names(data)[response]
    }else{
      name_response <- names(data)[response]
      data <- data.frame(data[,-response], data[,response])
      names(data)[dim(data)[2]] <- name_response
    }
  }
  
  data_old <- data
  
  # response <- dim(data_old)[2]
  
  # Save the names
  varnames <- names(data_old)[-ncol]
  respname <- names(data_old)[ncol]
  full.formula <- as.formula(paste(respname, "~", paste(varnames, collapse = "+")))
  
  if(type == "logistic"){
    full.model <- glm(full.formula, data=data_old, family = "binomial")
  }else{
    full.model <- lm(full.formula,data=data_old)
  }
  
  # summary(full.model)
  # coef(full.model)
  
  backward.sel <- step(full.model,direction="backward", data=data_old)
  # backward.sel
  # summary(backward.sel)
  # coef(backward.sel)
  
  results <- data.frame(coef(backward.sel))
  tresults <- data.frame(t(results))
  newnames <- names(tresults)[2:dim(tresults)[2]]
  
  data_new <- data_old[, c(newnames, respname)]
  y <- data_new
  
  return(y)
}

select.forward <- function(data, response, type){
  
  # Type is linear by default
  if(missing(type)){
    type <- "linear"
  }
  
  ncol <- dim(data)[2]
  # By default the response variable is the last one
  if(missing(response)){
    data <- data
    response <- dim(data)[2]
    name_response <- names(data)[response]
  }else{
    if(dim(data)[2] == response){
      data <- data
      name_response <- names(data)[response]
    }else{
      name_response <- names(data)[response]
      data <- data.frame(data[, -response], data[, response])
      names(data)[dim(data)[2]] <- name_response
    }
  }
  
  data_old <- data
  
  # Save the names
  varnames <- names(data_old)[-ncol]
  respname <- names(data_old)[ncol]
  
  # This is the starting model with only the response variable and the intercept
  starting.formula <- as.formula(paste(respname, "~", 1))
  empty <- lm(starting.formula, data=data_old)
  # This is the full model to define the scope of the forward selection
  full.formula <- as.formula(paste(respname, "~", paste(varnames, collapse = "+")))
  
  # Starting model
  if(type == "logistic"){
    starting.model <- glm(empty, data=data_old, family="binomial")
  }else{
    starting.model <- lm(empty, data=data_old)
  }
  
  forward.sel <- step(starting.model, scope=full.formula, direction="forward", data=data_old)
  
  results <- data.frame(coef(forward.sel))
  tresults <- data.frame(t(results))
  newnames <- names(tresults)[2:dim(tresults)[2]]
  
  data_new <- data_old[, c(newnames, respname)]
  y <- data_new
  
  return(y)
}

select.lasso <- function(data, response, nfolds, seed){
  
  # req
  library(glmnet)
  
  # If the seed hasn't been selected, the function selects a random seed by default.
  # Then it sets the seed.
  if(missing(seed)){
    # NB: do not add here the option to select a random seed, because a random seed
    # is selected automatically already. A message is printed telling
    # that the seed argument is not there, therefore the lasso will go random
    # and will output different selections at each time.
    print("seed argument missing")
  }else{
    set.seed(seed)
  }
  
  # Set the selected number of folds. Default is 10.
  if(missing(nfolds)){
    nf <- 10
  }else{
    nf <- nfolds
  }
  
  # If the position of the response variable is not selected, then the last column
  # is considered as the response variable.
  # If the position of the response variable is selected and is different than the
  # last column, then the function moves the response variable to the end of the
  # dataset (so it can work better on it later).
  if(missing(response)){
    data <- data
    response <- dim(data)[2]
    name_response <- names(data)[response]
  }else{
    if(dim(data)[2] == response){
      data <- data
      name_response <- names(data)[response]
    }else{
      name_response <- names(data)[response]
      data <- data.frame(data[,-response], data[,response])
      names(data)[dim(data)[2]] <- name_response
    }
  }
  
  s_data_old <- data.frame(scale(data))
  
  # Save names of the predictors
  last <- dim(data)[2]
  varnames <- names(data)[-last]
  
  # Run the lasso with nfold CV:
  lasso_s_data_old <- cv.glmnet(x=as.matrix(s_data_old[varnames]), y=s_data_old[, last],
                                family = "binomial", grouped=FALSE, nfolds=nf)
  
  # Check the best model obtained
  lasso_s_data_old$nzero[which.min(sqrt(lasso_s_data_old$cvm))]
  coef(lasso_s_data_old, s=lasso_s_data_old$lambda.min)
  
  # Now that the variables are selected, create a new dataset y with only these
  # variables and the response variable. The dataset y will be the output of 
  # function select.lasso()
  results <- as.matrix(coef(lasso_s_data_old, s=lasso_s_data_old$lambda.min))
  results <- data.frame(results)
  
  j <- 1
  p <- dim(t(results))[2]
  columns <- c()
  for(i in 2:p){
    if(results[i,1] != 0){
      columns[j] <- i - 1
      j <- j+1
    }
  }
  
  # Creates the new dataset with variables selected by lasso, and puts it into
  # the global environment
  # dataset_name <- deparse(substitute(data))
  # assign("lasso_data", y, envir=.GlobalEnv)
  
  # Resets the seed to null in case it is necessary
  # set.seed(NULL)
  
  # Returns the reduced dataset with only the variable selected via lasso
  y <- data[, c(columns, last)]
  return(y)
}

select.lasso.conv <- function(data, iterations){
  
  if(missing(iterations)){
    iterations <- 50
  }
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  num_vars <- c()
  
  for(i in 1 : iterations){
    current_data <- select.lasso(data)
    num_vars[i] <- dim(current_data)[2]
  }
  
  best <- getmode(num_vars)
  
  current_data <- select.lasso(data)
  current_dim <- dim(current_data)[2]
  while(current_dim != best){
    current_data <- select.lasso(data)
    current_dim <- dim(current_data)[2]
  }
  
  return(current_data)
}

select.pca <- function(data, response, scale = c(TRUE, FALSE), variability,
                       loadings = c(TRUE, FALSE)){
  
  # Clean the dataset by omitting all the NAs
  data <- na.omit(data)
  
  # Percentage of variability explained by the components. Default is 95%. This
  # is the threshold by which the number of components to use is selected
  if(missing(variability)){
    variability <- 0.95
  }else{
    variability <- variability
  }
  
  if(missing(response)){
    data_old <- data
  }else{
    data_old <- data[, -response]
  }
  
  # Scale the dataset if the option is selected
  if(scale != FALSE){
    data_old <- scale(data_old)
  }else{
    data_old <- data_old
  }
  
  # Run the PCA and save the number of columns (components)
  prdata_old <- princomp(data_old)
  ncol <- dim(data_old)[2]
  variances <- c()
  explained_var <- c()
  
  # Compute the variance of every principal component
  for(i in 1:ncol){
    variances[i] <- summary(prdata_old)$sdev[i]^2
  }
  
  # Compute the pct of explained variance of every principal component
  for(i in 1:ncol){
    explained_var[i] <- variances[i]/sum(variances)
  }
  
  # These are all the explained variances, for every PC
  # explained_var
  
  # Find the number of principal components to include in the model (according
  # to the variability threshold selected, where default is 95%)
  i <- 1
  sumpct <- 0
  while(sumpct < variability){
    sumpct <- sumpct + explained_var[i]
    i <- i+1
  }
  number_comp_selected <- i-1
  
  # Create a new data set with only the selected components and return it
  data_new <- data.frame(summary(prdata_old)$scores[, 1:number_comp_selected])
  loadings_prdata_old <- loadings(prdata_old)
  if(missing(loadings)){
    return(data_new)
  }else{
    if(loadings == TRUE){
      loadings_prdata_old <- loadings_prdata_old[, 1:number_comp_selected]
      return(loadings_prdata_old)
    }else{
      return(data_new)
    }
  }
  
}

spectral.clustering <- function(data) {
  
  # Normalize the data and build the Laplacian matrix
  norm_data <- apply(data, 2, function(x){(x - mean(x)) / sd(x)})
  W <- norm_data %*% t(norm_data)
  W[W < 0] <- 0
  W <- W - diag(diag(W))
  Dnsqrt <- diag(1 / sqrt(rowSums(W)))
  I <- diag(rep(1, nrow(W)))
  Lsym <- I - Dnsqrt %*% W %*% Dnsqrt
  
  # Now perform the eigendecomposition on the Laplacian matrix, and select v1
  # (the second smallest eigenvector)
  egn <- eigen(Lsym, symmetric = TRUE)
  v1 <- Dnsqrt %*% egn $ vectors[, nrow(W) - 1]
  v1 <- v1 / sqrt(sum(v1 ^ 2))
  
  # Divide the dataset into two clusters, and decide which one is the defective
  # cluster
  defect_proneness <- (v1 > 0)
  rs <- rowSums(norm_data)
  if (mean(rs[v1 > 0]) < mean(rs[v1 < 0])) {
    defect_proneness <- (v1 < 0)
  }
  
  return(defect_proneness)
}

# This is modified to use prcomp instead of princomp, which works also when
# eigenvalues are negative (because it turns them to 0):
select.pca.prcomp <- function(data, response, scale = c(TRUE, FALSE), variability,
                              loadings = c(TRUE, FALSE)){
  
  # Clean the dataset by omitting all the NAs
  data <- na.omit(data)
  
  # Percentage of variability explained by the components. Default is 95%. This
  # is the threshold by which the number of components to use is selected
  if(missing(variability)){
    variability <- 0.95
  }else{
    variability <- variability
  }
  
  if(missing(response)){
    data_old <- data
  }else{
    data_old <- data[, -response]
  }
  
  # Scale the dataset if the option is selected
  if(scale != FALSE){
    data_old <- scale(data_old, center = FALSE, scale = TRUE)
  }else{
    data_old <- data_old
  }
  
  # Run the PCA and save the number of columns (components)
  prdata_old <- prcomp(data_old)
  ncol <- dim(data_old)[2]
  variances <- c()
  explained_var <- c()
  
  # Compute the variance of every principal component
  for(i in 1:ncol){
    variances[i] <- summary(prdata_old)$sdev[i]^2
  }
  
  # Compute the pct of explained variance of every principal component
  for(i in 1:ncol){
    explained_var[i] <- variances[i]/sum(variances)
  }
  
  # These are all the explained variances, for every PC
  # explained_var
  
  # Find the number of principal components to include in the model (according
  # to the variability threshold selected, where default is 95%)
  i <- 1
  sumpct <- 0
  while(sumpct < variability){
    sumpct <- sumpct + explained_var[i]
    i <- i+1
  }
  number_comp_selected <- i-1
  
  # Create a new data set with only the selected components and return it
  data_new <- data.frame(summary(prdata_old)$x[, 1:number_comp_selected])
  loadings_prdata_old <- loadings(prdata_old)
  if(missing(loadings)){
    return(data_new)
  }else{
    if(loadings == TRUE){
      loadings_prdata_old <- loadings_prdata_old[, 1:number_comp_selected]
      return(loadings_prdata_old)
    }else{
      return(data_new)
    }
  }
  
  
  # if(loadings == TRUE){
  #   loadings_prdata_old <- loadings_prdata_old[, 1:number_comp_selected]
  #   return(loadings_prdata_old)
  # }else{
  #   return(data_new)
  # }
  
}




# Application of feature selection methods} 

# List containing the 17 datasets 
list <- list(eclipse, equinox, lucene, mylyn, pde, cm1, mw2, pc1, pc3, pc4, jm1,
             kc1, kc3, mc2, mc1, pc5, pc2)

# Apply feature selection methods

# Backward
back_list <- list()
for (i in 1 : 17) {
  back_list[[i]] <- select.backward(list[[i]], type = "logistic")
  print(i)
}

# Forward
forw_list <- list()
for (i in 1 : 17) {
  forw_list[[i]] <- select.forward(list[[i]], type = "logistic")
  print(i)
}

# LASSO
lasso_list <- list()
for(i in 1 : 17) {
  lasso_list[[i]] <- select.lasso.conv(list[[i]])
  print(i)
}
# Fix the 17th lasso selection (on dataset pc2)
lasso_list[[17]] <- select.lasso(list[[17]]) # Take the one with 3 predictors + 1 response

# PCA
list
cf_st_slist <- list()

for (i in 1:17) {
  
  p <- ncol(list[[i]])
  cf_st_slist[[i]] <- scale(list[[i]][, -p], center = FALSE, scale = TRUE)
  
}
pca_cf_st_list <- list()

for(i in 1 : 17) {
  
  pca_cf_st_list[[i]] <- select.pca.prcomp(cf_st_slist[[i]], scale = FALSE)
  pca_cf_st_list[[i]] <- add.response(pca_cf_st_list[[i]], list[[i]])
  
  print(i)
  
}
pca_list <- pca_cf_st_list

# List contaning the complete datasets and the selected datasets (total 85 datasets)
all <- c(list, back_list, forw_list, lasso_list, pca_list)



# Produce table of selected features} 

# Table with number of variables selected by each feature selection method

n_features <- c()

for(i in 1 : 85) {
  
  n_features[i] <- ncol(all[[i]])
  
}

names <- c("eclipse", "equinox", "lucene", "mylyn", "pde", "cm1", "mw2", "pc1",
           "pc3", "pc4", "jm1", "kc1", "kc3", "mc2", "mc1", "pc5", "pc2")

table_n_features <- data.frame(names, n_features[1:17], n_features[18:34],
                               n_features[35:51], n_features[52:68], n_features[69:85])

names(table_n_features) <- c("Dataset", "Complete", "Backward", "Forward", "LASSO", "PCA")



# Application of supervised and unsupervised algorithms} 

# Produce Naive Bayes results

library(caret)
library(pROC)
library(klaR)

nb_accuracy <- c()
nb_sensitivity_precision <- c()
nb_specificity <- c()
nb_recall <- c()
nb_f1 <- c()
nb_auc <- c()

for(i in 1 : 85) {
  
  p <- ncol(all[[i]])
  
  train_proportion <- 0.7
  test_proportion <- 1 - train_proportion
  
  id <- sample(2, nrow(all[[i]]), prob = c(train_proportion, test_proportion), replace = T)
  train <- all[[i]][id == 1, ]
  test <- all[[i]][id == 2, ]
  
  model <- NaiveBayes(x = train[, -p], grouping = as.factor(train[, p]), usekernel=TRUE)
  
  predicted_model <- predict(model, test)
  
  confusion_matrix <- confusionMatrix(table(predicted_model$class, test[, p]))
  print(confusion_matrix$table)
  
  nb_accuracy[i] <- confusion_matrix$overall[1]
  nb_sensitivity_precision[i] <- confusion_matrix$byClass[1]
  nb_specificity[i] <- confusion_matrix$byClass[2]
  nb_recall[i] <- confusion_matrix$table[1] / (confusion_matrix$table[1] 
                                               + confusion_matrix$table[3])
  
  precision <- confusion_matrix$table[1] / (confusion_matrix$table[1] 
                                            + confusion_matrix$table[2])
  nb_f1[i] <- 2 * ((precision * nb_recall[i]) / (precision + nb_recall[i]))
  
  nb_auc[i] <- auc(test[, p], as.integer(predicted_model$class))
  
  print(i)
}

rm(i, id, p, precision, test_proportion, train_proportion, test, train,
   predicted_model, model)

names <- c("eclipse", "equinox", "lucene", "mylyn", "pde", "cm1", "mw2", "pc1",
           "pc3", "pc4", "jm1", "kc1", "kc3", "mc2", "mc1", "pc5", "pc2")
nb_accuracy <- data.frame(project = names,
                          complete = nb_accuracy[1:17],
                          backward = nb_accuracy[18:34],
                          forward = nb_accuracy[35:51],
                          lasso = nb_accuracy[52:68],
                          pca = nb_accuracy[69:85])
nb_sensitivity_precision <- data.frame(project = names,
                                       complete = nb_sensitivity_precision[1:17],
                                       backward = nb_sensitivity_precision[18:34],
                                       forward = nb_sensitivity_precision[35:51],
                                       lasso = nb_sensitivity_precision[52:68],
                                       pca = nb_sensitivity_precision[69:85])                      
nb_specificity <- data.frame(project = names,
                             complete = nb_specificity[1:17],
                             backward = nb_specificity[18:34],
                             forward = nb_specificity[35:51],
                             lasso = nb_specificity[52:68],
                             pca = nb_specificity[69:85])
nb_recall <- data.frame(project = names,
                        complete = nb_recall[1:17],
                        backward = nb_recall[18:34],
                        forward = nb_recall[35:51],
                        lasso = nb_recall[52:68],
                        pca = nb_recall[69:85])
nb_f1 <- data.frame(project = names,
                    complete = nb_f1[1:17],
                    backward = nb_f1[18:34],
                    forward = nb_f1[35:51],
                    lasso = nb_f1[52:68],
                    pca = nb_f1[69:85])
nb_auc <- data.frame(project = names,
                     complete = nb_auc[1:17],
                     backward = nb_auc[18:34],
                     forward = nb_auc[35:51],
                     lasso = nb_auc[52:68],
                     pca = nb_auc[69:85])



# Produce Random Forest results} 

library(randomForest)
library(caret)
library(pROC)

rf_accuracy <- c()
rf_sensitivity_precision <- c()
rf_specificity <- c()
rf_recall <- c()
rf_f1 <- c()
rf_auc <- c()

for(i in 1 : 85) {
  
  p <- ncol(all[[i]])
  
  train_proportion <- 0.7
  test_proportion <- 1 - train_proportion
  
  id <- sample(2, nrow(all[[i]]), prob = c(train_proportion, test_proportion), replace = T)
  train <- all[[i]][id == 1, ]
  test <- all[[i]][id == 2, ]
  
  model <- randomForest(x = train[, -p], y = as.factor(train[, p]))
  
  predicted_model <- predict(model, test)
  
  confusion_matrix <- confusionMatrix(table(predicted_model, test[, p]))
  print(confusion_matrix$table)
  
  rf_accuracy[i] <- confusion_matrix$overall[1]
  rf_sensitivity_precision[i] <- confusion_matrix$byClass[1]
  rf_specificity[i] <- confusion_matrix$byClass[2]
  rf_recall[i] <- confusion_matrix$table[1] / (confusion_matrix$table[1] 
                                               + confusion_matrix$table[3])
  
  precision <- confusion_matrix$table[1] / (confusion_matrix$table[1] 
                                            + confusion_matrix$table[2])
  rf_f1[i] <- 2 * ((precision * rf_recall[i]) / (precision + rf_recall[i]))
  
  rf_auc[i] <- auc(test[, p], as.integer(predicted_model))
  
  print(i)
}

rm(i, id, p, precision, test_proportion, train_proportion, test, train,
   predicted_model, model)

names <- c("eclipse", "equinox", "lucene", "mylyn", "pde", "cm1", "mw2", "pc1",
           "pc3", "pc4", "jm1", "kc1", "kc3", "mc2", "mc1", "pc5", "pc2")

rf_accuracy <- data.frame(project = names,
                          complete = rf_accuracy[1:17],
                          backward = rf_accuracy[18:34],
                          forward = rf_accuracy[35:51],
                          lasso = rf_accuracy[52:68],
                          pca = rf_accuracy[69:85])
rf_sensitivity_precision <- data.frame(project = names,
                                       complete = rf_sensitivity_precision[1:17],
                                       backward = rf_sensitivity_precision[18:34],
                                       forward = rf_sensitivity_precision[35:51],
                                       lasso = rf_sensitivity_precision[52:68],
                                       pca = rf_sensitivity_precision[69:85])                      
rf_specificity <- data.frame(project = names,
                             complete = rf_specificity[1:17],
                             backward = rf_specificity[18:34],
                             forward = rf_specificity[35:51],
                             lasso = rf_specificity[52:68],
                             pca = rf_specificity[69:85])
rf_recall <- data.frame(project = names,
                        complete = rf_recall[1:17],
                        backward = rf_recall[18:34],
                        forward = rf_recall[35:51],
                        lasso = rf_recall[52:68],
                        pca = rf_recall[69:85])
rf_f1 <- data.frame(project = names,
                    complete = rf_f1[1:17],
                    backward = rf_f1[18:34],
                    forward = rf_f1[35:51],
                    lasso = rf_f1[52:68],
                    pca = rf_f1[69:85])
rf_auc <- data.frame(project = names,
                     complete = rf_auc[1:17],
                     backward = rf_auc[18:34],
                     forward = rf_auc[35:51],
                     lasso = rf_auc[52:68],
                     pca = rf_auc[69:85])


# Produce Neural Gas clustering results

library(cclust)

ng_accuracy <- c()
ng_sensitivity_precision <- c()
ng_specificity <- c()
ng_recall <- c()
ng_f1 <- c()
ng_auc <- c()

for(i in 1 : 85) {
  
  p <- ncol(all[[i]])
  
  # Apply neural clustering algorithm
  clustering <- cclust(x = scale(all[[i]][, -p]), centers = 2, method = "neuralgas")
  
  # Transform the results of clustering$cluster into binary 0/1 values
  if (sum(clustering$cluster == 2) > sum(clustering$cluster == 1)) { 
    # If the number of 2s is bigger than the number of 1s
    
    for(j in 1 : length(clustering$cluster)) {
      if(clustering$cluster[j] == 2) {
        clustering$cluster[j] <- 0
      } else {
        clustering$cluster[j] <- 1
      }
    }
    
  } else {
    
    for(j in 1 : length(clustering$cluster)) {
      if(clustering$cluster[j] == 1) {
        clustering$cluster[j] <- 0
      } else {
        clustering$cluster[j] <- 1
      }
    }
  }
  # print(clustering$cluster)
  
  # Compute the confusion matrix 
  confusion_matrix <- confusionMatrix(as.factor(all[[i]][, p]),
                                      as.factor(as.integer(clustering$cluster)))
  print(confusion_matrix$table)
  
  ng_accuracy[i] <- confusion_matrix$overall[1]
  ng_sensitivity_precision[i] <- confusion_matrix$byClass[1]
  ng_specificity[i] <- confusion_matrix$byClass[2]
  ng_recall[i] <- confusion_matrix$table[1] / (confusion_matrix$table[1] 
                                               + confusion_matrix$table[3])
  
  precision <- confusion_matrix$table[1] / (confusion_matrix$table[1] 
                                            + confusion_matrix$table[2])
  ng_f1[i] <- 2 * ((precision * ng_recall[i]) / (precision + ng_recall[i]))
  
  ng_auc[i] <- auc(all[[i]][, p], as.integer(clustering$cluster))
  
  print(i)
}

rm(i, id, p, precision, test_proportion, train_proportion, test, train,
   predicted_model, model)

names <- c("eclipse", "equinox", "lucene", "mylyn", "pde", "cm1", "mw2", "pc1",
           "pc3", "pc4", "jm1", "kc1", "kc3", "mc2", "mc1", "pc5", "pc2")

ng_accuracy <- data.frame(project = names,
                          complete = ng_accuracy[1:17],
                          backward = ng_accuracy[18:34],
                          forward = ng_accuracy[35:51],
                          lasso = ng_accuracy[52:68],
                          pca = ng_accuracy[69:85])
ng_sensitivity_precision <- data.frame(project = names,
                                       complete = ng_sensitivity_precision[1:17],
                                       backward = ng_sensitivity_precision[18:34],
                                       forward = ng_sensitivity_precision[35:51],
                                       lasso = ng_sensitivity_precision[52:68],
                                       pca = ng_sensitivity_precision[69:85])                      
ng_specificity <- data.frame(project = names,
                             complete = ng_specificity[1:17],
                             backward = ng_specificity[18:34],
                             forward = ng_specificity[35:51],
                             lasso = ng_specificity[52:68],
                             pca = ng_specificity[69:85])
ng_recall <- data.frame(project = names,
                        complete = ng_recall[1:17],
                        backward = ng_recall[18:34],
                        forward = ng_recall[35:51],
                        lasso = ng_recall[52:68],
                        pca = ng_recall[69:85])
ng_f1 <- data.frame(project = names,
                    complete = ng_f1[1:17],
                    backward = ng_f1[18:34],
                    forward = ng_f1[35:51],
                    lasso = ng_f1[52:68],
                    pca = ng_f1[69:85])
ng_auc <- data.frame(project = names,
                     complete = ng_auc[1:17],
                     backward = ng_auc[18:34],
                     forward = ng_auc[35:51],
                     lasso = ng_auc[52:68],
                     pca = ng_auc[69:85])


# Produce Spectral Clustering results

sc_accuracy <- c()
sc_sensitivity_precision <- c()
sc_specificity <- c()
sc_recall <- c()
sc_f1 <- c()
sc_auc <- c()

for(i in 1 : 85) {
  
  p <- ncol(all[[i]])
  
  # Apply spectral clustering algorithm
  clustering <- spectral.clustering(all[[i]][, -p])
  
  # Compute the confusion matrix 
  confusion_matrix <- confusionMatrix(as.factor(all[[i]][, p]),
                                      as.factor(as.integer(clustering)))
  print(confusion_matrix$table)
  
  sc_accuracy[i] <- confusion_matrix$overall[1]
  sc_sensitivity_precision[i] <- confusion_matrix$byClass[1]
  sc_specificity[i] <- confusion_matrix$byClass[2]
  sc_recall[i] <- confusion_matrix$table[1] / (confusion_matrix$table[1] 
                                               + confusion_matrix$table[3])
  
  precision <- confusion_matrix$table[1] / (confusion_matrix$table[1] 
                                            + confusion_matrix$table[2])
  sc_f1[i] <- 2 * ((precision * sc_recall[i]) / (precision + sc_recall[i]))
  
  sc_auc[i] <- auc(all[[i]][, p], as.integer(clustering))
  
}

rm(i, id, p, precision, test_proportion, train_proportion, test, train,
   predicted_model, model)

names <- c("eclipse", "equinox", "lucene", "mylyn", "pde", "cm1", "mw2", "pc1",
           "pc3", "pc4", "jm1", "kc1", "kc3", "mc2", "mc1", "pc5", "pc2")

sc_accuracy <- data.frame(project = names,
                          complete = sc_accuracy[1:17],
                          backward = sc_accuracy[18:34],
                          forward = sc_accuracy[35:51],
                          lasso = sc_accuracy[52:68],
                          pca = sc_accuracy[69:85])
sc_sensitivity_precision <- data.frame(project = names,
                                       complete = sc_sensitivity_precision[1:17],
                                       backward = sc_sensitivity_precision[18:34],
                                       forward = sc_sensitivity_precision[35:51],
                                       lasso = sc_sensitivity_precision[52:68],
                                       pca = sc_sensitivity_precision[69:85])                      
sc_specificity <- data.frame(project = names,
                             complete = sc_specificity[1:17],
                             backward = sc_specificity[18:34],
                             forward = sc_specificity[35:51],
                             lasso = sc_specificity[52:68],
                             pca = sc_specificity[69:85])
sc_recall <- data.frame(project = names,
                        complete = sc_recall[1:17],
                        backward = sc_recall[18:34],
                        forward = sc_recall[35:51],
                        lasso = sc_recall[52:68],
                        pca = sc_recall[69:85])
sc_f1 <- data.frame(project = names,
                    complete = sc_f1[1:17],
                    backward = sc_f1[18:34],
                    forward = sc_f1[35:51],
                    lasso = sc_f1[52:68],
                    pca = sc_f1[69:85])
sc_auc <- data.frame(project = names,
                     complete = sc_auc[1:17],
                     backward = sc_auc[18:34],
                     forward = sc_auc[35:51],
                     lasso = sc_auc[52:68],
                     pca = sc_auc[69:85])


# Comparison of classifiers

# Assumptions on the data (normality and spherical data)
qqnorm(sc_accuracy$complete)
qqnorm(sc_accuracy$backward)
qqnorm(sc_accuracy$forward)
qqnorm(sc_accuracy$lasso)
qqnorm(sc_accuracy$pca)

shapiro.test(sc_accuracy$complete)
shapiro.test(sc_accuracy$backward)
shapiro.test(sc_accuracy$forward)
shapiro.test(sc_accuracy$lasso)
shapiro.test(sc_accuracy$pca)

var(sc_accuracy$complete)
var(sc_accuracy$backward)
var(sc_accuracy$forward)
var(sc_accuracy$lasso)
var(sc_accuracy$pca)

sd(sc_accuracy$complete)
sd(sc_accuracy$backward)
sd(sc_accuracy$forward)
sd(sc_accuracy$lasso)
sd(sc_accuracy$pca)

###

# Produce the Friedman test tables for feature selection methods:
nb_pvalues <- c()
nb_pvalues[1] <- friedman.test(as.matrix(nb_accuracy[, -1]))[3]
nb_pvalues[2] <-friedman.test(as.matrix(nb_f1[, -1]))[3]
nb_pvalues[3] <-friedman.test(as.matrix(nb_auc[, -1]))[3]
nb_pvalues[4] <-friedman.test(as.matrix(nb_recall[, -1]))[3]
nb_pvalues[5] <-friedman.test(as.matrix(nb_sensitivity_precision[, -1]))[3]
nb_pvalues[6] <-friedman.test(as.matrix(nb_specificity[, -1]))[3]

rf_pvalues <- c()
rf_pvalues[1] <- friedman.test(as.matrix(rf_accuracy[, -1]))[3]
rf_pvalues[2] <-friedman.test(as.matrix(rf_f1[, -1]))[3]
rf_pvalues[3] <-friedman.test(as.matrix(rf_auc[, -1]))[3]
rf_pvalues[4] <-friedman.test(as.matrix(rf_recall[, -1]))[3]
rf_pvalues[5] <-friedman.test(as.matrix(rf_sensitivity_precision[, -1]))[3]
rf_pvalues[6] <-friedman.test(as.matrix(rf_specificity[, -1]))[3]

ng_pvalues <- c()
ng_pvalues[1] <- friedman.test(as.matrix(ng_accuracy[, -1]))[3]
ng_pvalues[2] <- friedman.test(as.matrix(ng_f1[, -1]))[3]
ng_pvalues[3] <- friedman.test(as.matrix(ng_auc[, -1]))[3]
ng_pvalues[4] <- friedman.test(as.matrix(ng_recall[, -1]))[3]
ng_pvalues[5] <- friedman.test(as.matrix(ng_sensitivity_precision[, -1]))[3]
ng_pvalues[6] <- friedman.test(as.matrix(ng_specificity[, -1]))[3]

sc_pvalues <- c()
sc_pvalues[1] <- friedman.test(as.matrix(sc_accuracy[, -1]))[3]
sc_pvalues[2] <- friedman.test(as.matrix(sc_f1[, -1]))[3]
sc_pvalues[3] <- friedman.test(as.matrix(sc_auc[, -1]))[3]
sc_pvalues[4] <- friedman.test(as.matrix(sc_recall[, -1]))[3]
sc_pvalues[5] <- friedman.test(as.matrix(sc_sensitivity_precision[, -1]))[3]
sc_pvalues[6] <- friedman.test(as.matrix(sc_specificity[, -1]))[3]

evaluation_measures <- c("Accuracy", "F-score", "AUC", "Recall", "Precision", "specificity")
friedman_pvalues <- data.frame(evaluation_measures, as.matrix(nb_pvalues), 
                               as.matrix(rf_pvalues), as.matrix(ng_pvalues),
                               as.matrix(sc_pvalues))
names(friedman_pvalues) <- c("Evaluation Measures", "Naive Bayes", "Random Forest", 
                             "Neural Gas Clustering", "Spectral Clustering")


# Nemenyi multiple comparisons

# Multiple comparisons with Nemenyi test
library(tsutils)

nemenyi(as.matrix(ng_accuracy[,-1]))[1]
nemenyi(as.matrix(ng_f1[,-1]))[1]
nemenyi(as.matrix(ng_auc[,-1]))[1]
nemenyi(as.matrix(ng_recall[,-1]))[1]
nemenyi(as.matrix(ng_sensitivity_precision[,-1]))[1]
nemenyi(as.matrix(ng_specificity[,-1]))[1]

nemenyi(as.matrix(sc_accuracy[,-1]))[1]
nemenyi(as.matrix(sc_f1[,-1]))[1]
nemenyi(as.matrix(sc_auc[,-1]))[1]
nemenyi(as.matrix(sc_recall[,-1]))[1]
nemenyi(as.matrix(sc_sensitivity_precision[,-1]))[1]
nemenyi(as.matrix(sc_specificity[,-1]))[1]


# AUC comparison of classifiers

# Final table for comparison of classifiers through AUC values:

names <- c( "eclipse", "equinox", "lucene",
            "mylyn" ,  "pde"  ,   "cm1"   ,  "mw2" ,    "pc1" ,    "pc3"  ,   "pc4",
            "jm1"   ,  "kc1"    , "kc3"  ,   "mc2" ,    "mc1"  ,   "pc5"   ,  "pc2")

nb_lasso_col <- nb_auc[,3]
rf_forward_col <- rf_auc[,4]
ng_backward_col <- ng_auc[,3]
sc_complete_col <- sc_auc[,2]

classifiers_comparison <- data.frame(names, nb_lasso_col, rf_forward_col, 
                                     ng_backward_col, sc_complete_col)
names(classifiers_comparison) <- c("Project", "NB - LASSO", "RF - Forward",
                                   "NG - Backward", "SC - Complete")

friedman.test(as.matrix(classifiers_comparison[,-1]))
nemenyi(as.matrix(classifiers_comparison[,-1]))