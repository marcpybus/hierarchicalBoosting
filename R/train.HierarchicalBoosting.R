train.HierarchicalBoosting <-
function(input, config_table, bootstrap_iterations, bootstrap_sampling, boosting_iterations=1000) {

options(stringsAsFactors=FALSE)

# Loss function : Negative binomial log-likelihood
rho <- function(y ,f, w=1) {
    p <- pmax(pmin(1 - 1e-5, f), 1e-5)
   -y * log(p) - (1-y) * log(1-p)
}
# ngradient: a function with arguments ‘y’, ‘f’ and ‘w’ implementing the
# negative gradient of the ‘loss’ function (which is to be minimized).
ngradient <- function(y, f, w=1){y - f}

# offset: a function with argument ‘y’ and ‘w’ (weights) for computing a scalar offset.
offset <- function(y, w){weighted.mean(y, w)}

L2fm <- Family(ngradient=ngradient, loss=rho, offset=offset)

ctrl <- boost_control(mstop=boosting_iterations, nu = 0.1 )

matrix_classifiers<-matrix(list(),nrow=length(config_table[,1]),ncol=bootstrap_iterations)
matrix_coefficients<-matrix(list(),nrow=length(config_table[,1]),ncol=2)
matrix_thresholds<-matrix(c(NA),nrow=length(config_table[,1]),ncol=2)

cat("\n")

for(i in 1:length(config_table[,1])){

cat(paste("Training Boosting #",i,"\n\n",sep=""))

coeficient_matrix<-data.frame()
std_coeficient_matrix<-data.frame()

competing_A<-input[0,]
competing_B<-input[0,]

competing_A_scenarios<-strsplit(config_table[i,]$scenario_A,",")[[1]]

for(scen in competing_A_scenarios){
competing_A <- rbind(competing_A,input[input$scenario==scen,][-1])
}
competing_A$y<-0

competing_B_scenarios<-strsplit(config_table[i,]$scenario_B,",")[[1]]

for(scen in competing_B_scenarios){
competing_B <- rbind(competing_B,input[input$scenario==scen,][-1])
}
competing_B$y<-1

competing_A <- na.omit(competing_A)
competing_B <- na.omit(competing_B)

text_to_plot<-""

if(config_table$input_data[i] == "original"){
text_to_plot<-"original dataset"
}else{
relation<-strsplit(config_table$input_data[i],":")[[1]]
text_to_plot<-paste(relation[2]," from ",relation[1],sep="")
}

cat(paste(paste(competing_A_scenarios,collapse="/")," vs ",paste(competing_B_scenarios,collapse="/")," \n",sep=""))
cat(paste("input data: ",text_to_plot,"\n",sep=""))
cat(paste("number of replicates    => scenario A: ",dim(competing_A)[1],"\t scenario B: ",dim(competing_B)[1],"\n",sep=""))
cat(paste("significance thresholds => scenario A: ",config_table$threshold_scenario_A[i],"\t scenario B: ",config_table$threshold_scenario_B[i],"\n",sep=""))
cat("boostrapping.")

for (n in 1:bootstrap_iterations){

cat(".")

training_set <- rbind(sample.df(competing_A, round(length(competing_A[,1])*bootstrap_sampling)),sample.df(competing_B, round(length(competing_B[,1])*bootstrap_sampling)))

matrix_classifiers[[i,n]] <- glmboost(y ~ ., data = training_set, family = L2fm , control = ctrl , center = TRUE)

coeficient_matrix<-rbind(coeficient_matrix,coef(matrix_classifiers[[i,n]],off2int=TRUE,aggregate="sum",which=""))
std_coeficient_matrix<-rbind(std_coeficient_matrix,std_coef(matrix_classifiers[[i,n]], training_set) )

colnames(coeficient_matrix)<-names(coef(matrix_classifiers[[i,n]],off2int=TRUE,aggregate="sum",which=""))
colnames(std_coeficient_matrix)<-names(coef(matrix_classifiers[[i,n]],off2int=TRUE,aggregate="sum",which="")[-1])
}

cat("done\n\n")

matrix_coefficients[[i,1]]<-coeficient_matrix
matrix_coefficients[[i,2]]<-std_coeficient_matrix

mean_coefficients<-colMeans(matrix_coefficients[[i,1]])
scores_A<-apply(competing_A[-length(competing_A[1,])], 1, function(x) sum(mean_coefficients[-1]*x[])+mean_coefficients[1])
scores_B<-apply(competing_B[-length(competing_B[1,])], 1, function(x) sum(mean_coefficients[-1]*x[])+mean_coefficients[1])

if(config_table$threshold_scenario_A[i] != FALSE){
matrix_thresholds[i,1]=quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i]))

}
if(config_table$threshold_scenario_B[i] != FALSE){
matrix_thresholds[i,2]=quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i]))
}


}

return(list("matrix_classifiers" = matrix_classifiers, "matrix_coefficients" = matrix_coefficients, "matrix_thresholds" = matrix_thresholds))
}

std_coef <-
function(boostingClassifier, trainingData) {

    statistics <- colnames(trainingData)[-length(trainingData[1,])]
    coefficients <- coef(boostingClassifier,off2int=TRUE,aggregate="sum",which="")
    variances <- apply(trainingData[-length(trainingData)], 2, var)

    stdCoefficients <- c()

    for (stat in statistics) {
        if (stat %in% names(coefficients)) {
            var <- variances[[stat]]
            stdCoefficients[[stat]] <- coefficients[[stat]] * sqrt(var)
        }
        else {
            stdCoefficients[[stat]] <- 0
        }
    }
    return (stdCoefficients)
}
