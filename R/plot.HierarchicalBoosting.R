plot.HierarchicalBoosting <-
function(hierarchicalBoosting, config_table, input, name="") {

cat("Generating validation plots...\n")

for(i in 1:length(config_table[,1])){

## SELECTING SCENARIOS

competing_A<-input[0,]
competing_B<-input[0,]

competing_A_scenarios<-strsplit(config_table[i,]$scenario_A,",")[[1]]

for(scen in competing_A_scenarios){
competing_A <- input[input$scenario==scen,][-1]
}
competing_A$y<-0

competing_B_scenarios<-strsplit(config_table[i,]$scenario_B,",")[[1]]

for(scen in competing_B_scenarios){
competing_B <- input[input$scenario==scen,][-1]
}
competing_B$y<-1

competing_A <- na.omit(competing_A)
competing_B <- na.omit(competing_B)

## PLOTING AND VALIDATION

outFile<-paste(name,"boosting_",i,sep="")
pdf_name<-paste(outFile,".plots.pdf",sep="")
pdf(pdf_name)

## COEFFICIENT PATH

par(las=0)
par(mar=c(5,5,2,2))

cp <- coef(hierarchicalBoosting$matrix_classifiers[[i,1]], aggregate = "cumsum")
ncp <- names(cp)

plot.new()
legend("center",legend=ncp, col=rainbow(length(ncp)), pch=rep.int(15, length(ncp)))

plot.glmboost_nolabel(hierarchicalBoosting$matrix_classifiers[[i,1]],col="grey",main=paste(outFile," - coefficient paths",sep=" "))

for (n in 1:length(hierarchicalBoosting$matrix_coefficients[[i,1]][,1])){
plot.glmboost_nolabel(hierarchicalBoosting$matrix_classifiers[[i,n]],add=TRUE,col="grey")
}

plot.glmboost_label(hierarchicalBoosting$matrix_classifiers[[i,1]],col=rainbow(length(ncp)),lwd=2,add=TRUE)

## COEFFICIENT BOXPLOT

par(las=2)
par(mar=c(9,4,4,4))
boxplot(hierarchicalBoosting$matrix_coefficients[[i,2]],main="standardized coefficient boxplots")
abline(h=0,col="blue",lty=2)

## DISTRIBUTIONS AND THRESHOLDS

mean_coefficients<-colMeans(hierarchicalBoosting$matrix_coefficients[[i,1]])
scores_A<-apply(competing_A[-length(competing_A[1,])], 1, function(x) sum(mean_coefficients[-1]*x[])+mean_coefficients[1])
scores_B<-apply(competing_B[-length(competing_B[1,])], 1, function(x) sum(mean_coefficients[-1]*x[])+mean_coefficients[1])

xlim<-c(min(density(scores_A)$x,density(scores_B)$x),max(density(scores_A)$x,density(scores_B)$x))
ylim<-c(min(density(scores_A)$y,density(scores_B)$y),max(density(scores_A)$y,density(scores_B)$y)+0.5)

plot(density(scores_A),main="boosting scores distribution",col="blue",xlim=xlim,ylim=ylim)
lines(density(scores_B),col="red")

if(config_table$threshold_scenario_B[i] != FALSE & config_table$threshold_scenario_A[i] != FALSE){
if(quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i])) < quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i]))){

text((quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i]))-min(density(scores_A)$x))/2+min(density(scores_A)$x),max(density(scores_A)$y,density(scores_B)$y)+0.25,"scenario\nA",col="blue")
text((max(density(scores_B)$x)-quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i])))/2+quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i])),max(density(scores_A)$y,density(scores_B)$y)+0.25,"scenario\nB",col="red")
text((quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i]))-quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i])))/2+quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i])),max(density(scores_A)$y,density(scores_B)$y)+0.25,"scenario\nC",col="darkgreen")

abline(v=quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i])),col="blue",lty=2)
text(quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i])),max(density(scores_A)$y,density(scores_B)$y),paste("threshold A: ",(100*as.numeric(config_table$threshold_scenario_A[i])),"%",sep=""), srt = 90,col="blue", pos=2)

abline(v=quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i])),col="red",lty=2)
text(quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i]))+0.11,max(density(scores_A)$y,density(scores_B)$y),paste("threshold B: ",(100*as.numeric(config_table$threshold_scenario_B[i])),"%",sep=""), srt = 90,col="red", pos=2)

}else{
text((quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i]))-min(density(scores_A)$x))/2+min(density(scores_A)$x),max(density(scores_A)$y,density(scores_B)$y)+0.25,"scenario\nA",col="blue")
text((max(density(scores_B)$x)-quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i])))/2+quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i])),max(density(scores_A)$y,density(scores_B)$y)+0.25,"scenario\nB",col="red")
text((quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i]))-quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i])))/2+quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i])),max(density(scores_A)$y,density(scores_B)$y)+0.25,"scenario\nC",col="darkgreen")

abline(v=quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i])),col="blue",lty=2)
text(quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i]))+0.11,max(density(scores_A)$y,density(scores_B)$y),paste("threshold A: ",(100*as.numeric(config_table$threshold_scenario_A[i])),"%",sep=""), srt = 90,col="blue", pos=2)

abline(v=quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i])),col="red",lty=2)
text(quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i])),max(density(scores_A)$y,density(scores_B)$y),paste("threshold B: ",(100*as.numeric(config_table$threshold_scenario_B[i])),"%",sep=""), srt = 90,col="red", pos=2)

}
}

if(config_table$threshold_scenario_B[i] != FALSE & config_table$threshold_scenario_A[i] == FALSE){

text((quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i]))-min(density(scores_A)$x))/2+min(density(scores_A)$x),max(density(scores_A)$y,density(scores_B)$y)+0.25,"scenario\nA",col="blue")
text((max(density(scores_B)$x)-quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i])))/2+quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i])),max(density(scores_A)$y,density(scores_B)$y)+0.25,"scenario\nB",col="red")

abline(v=quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i])),col="red",lty=2)
text(quantile(scores_B,as.numeric(config_table$threshold_scenario_B[i])),max(density(scores_A)$y,density(scores_B)$y),paste("threshold B: ",(100*as.numeric(config_table$threshold_scenario_B[i])),"%",sep=""), srt = 90,col="red", pos=2)

}

if(config_table$threshold_scenario_B[i] == FALSE & config_table$threshold_scenario_A[i] != FALSE){
text((quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i]))-min(density(scores_A)$x))/2+min(density(scores_A)$x),max(density(scores_A)$y,density(scores_B)$y)+0.25,"scenario\nA",col="blue")
text((max(density(scores_B)$x)-quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i])))/2+quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i])),max(density(scores_A)$y,density(scores_B)$y)+0.25,"scenario\nB",col="red")

abline(v=quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i])),col="blue",lty=2)
text(quantile(scores_A,as.numeric(config_table$threshold_scenario_A[i])),max(density(scores_A)$y,density(scores_B)$y),paste("threshold A: ",(100*as.numeric(config_table$threshold_scenario_A[i])),"%",sep=""), srt = 90,col="blue", pos=2)

}

text(density(scores_A)$x[which.max(density(scores_A)$y)],mean(density(scores_A)$y),paste(competing_A_scenarios,collapse="\n"),col="blue",pos=3)
text(density(scores_B)$x[which.max(density(scores_B)$y)],mean(density(scores_B)$y),paste(competing_B_scenarios,collapse="\n"),col="red",pos=3)

dev.off()

cat(paste("   - ",pdf_name,"\n",sep=""))

}

cat("\n")

}

plot.glmboost_nolabel <-
function(x, main = deparse(x$call), col = NULL,
                          off2int = FALSE, ...) {

    cp <- coef(x, aggregate = "cumsum", off2int = off2int)
    ncp <- names(cp)
    cp <- matrix(unlist(cp), nrow = length(cp), byrow = TRUE)
    cf <- cp[, ncol(cp)]
    if (is.null(col))
        col <- hcl(h = 40, l = 50, c= abs(cf) / max(abs(cf)) * 490)
    matplot(t(cp), type = "l", lty = 1, xlab = "Number of boosting iterations",
            ylab = "Unstandarized Coefficients", main = main, col = col, ...)
    abline(h = 0, lty = 1, col = "lightgray")
    #axis(4, at = cf, labels = ncp, las = 1)
}

plot.glmboost_label <-
function(x, main = deparse(x$call), col = NULL,
                          off2int = FALSE, ...) {

    cp <- coef(x, aggregate = "cumsum", off2int = off2int)
    ncp <- names(cp)
    cp <- matrix(unlist(cp), nrow = length(cp), byrow = TRUE)
    cf <- cp[, ncol(cp)]
    if (is.null(col))
        col <- hcl(h = 40, l = 50, c= abs(cf) / max(abs(cf)) * 490)
    matplot(t(cp), type = "l", lty = 1, xlab = "Number of boosting iterations",
            ylab = "Unstandarized Coefficients", main = main, col = col, ...)
    abline(h = 0, lty = 1, col = "lightgray")

    #axis(4, at = cf, labels = ncp ,las = 1, cex.axis=0.7)
 
}
