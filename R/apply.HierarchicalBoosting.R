apply.HierarchicalBoosting <-
function(input, config_table, hierarchicalBoosting) {

cat("Applying hierarchicalBoosting...\n")

results<-data.frame(chromosome=input$chromosome,start=input$start,end=input$end,stringsAsFactors=FALSE)

for(i in 1:length(config_table[,1])){

cat(paste(" ",i,sep=""))

dataset<-input[-1:-3]

relation<-strsplit(config_table$input_data[i],":")[[1]]

if(relation[1]=="original"){
dataset<-input[-1:-3]
}else{

scenario<-strsplit(relation[2],"_")[[1]][2]
boosting<-strsplit(relation[1],"_")[[1]][2]

selection<-ifelse(results[[eval(paste("boosting_",boosting,"_classification",sep=""))]]==scenario,TRUE,NA)

dataset<-input[selection,][-1:-3]
rownames(dataset)<-rownames(input)

}

# BOOSTING SCORE

mean_coefficients<-colMeans(hierarchicalBoosting$matrix_coefficients[[i,1]])
scores<-apply(dataset, 1, function(x) sum(mean_coefficients[-1]*x[])+mean_coefficients[1])

scores_dataframe<-data.frame(scores,stringsAsFactors=F)
colnames(scores_dataframe)<-paste("boosting_",i,sep="")

results<-merge(results,scores_dataframe, by="row.names", sort=F, all.x=T)[-1]


if(any(is.na(hierarchicalBoosting$matrix_thresholds[i,]))){
if(is.na(hierarchicalBoosting$matrix_thresholds[i,1])){

classification<-ifelse(scores_dataframe <= hierarchicalBoosting$matrix_thresholds[i,2], "A","B")
colnames(classification)<-paste("boosting_",i,"_classification",sep="")

results<-merge(results,classification, by="row.names", sort=F, all.x=T)[-1]

}else{
classification<-ifelse(scores_dataframe <= hierarchicalBoosting$matrix_thresholds[i,1], "A","B")
colnames(classification)<-paste("boosting_",i,"_classification",sep="")

results<-merge(results,classification, by="row.names", sort=F, all.x=T)[-1]

}
}else{
if(hierarchicalBoosting$matrix_thresholds[i,1] >= hierarchicalBoosting$matrix_thresholds[i,2]){

classification<-ifelse(scores_dataframe <= hierarchicalBoosting$matrix_thresholds[i,2], "A","C")
index<-classification=="C"
index[is.na(index)]<-FALSE
classification[index]<-ifelse(scores_dataframe[index] >= hierarchicalBoosting$matrix_thresholds[i,1], "B","C")

colnames(classification)<-paste("boosting_",i,"_classification",sep="")
results<-merge(results,classification, by="row.names", sort=F, all.x=T)[-1]

}else{

classification<-ifelse(scores_dataframe <= hierarchicalBoosting$matrix_thresholds[i,1], "A","C")
index<-classification=="C"
index[is.na(index)]<-FALSE
classification[index]<-ifelse(scores_dataframe[index] >= hierarchicalBoosting$matrix_thresholds[i,2], "B","C")

colnames(classification)<-paste("boosting_",i,"_classification",sep="")
results<-merge(results,classification, by="row.names", sort=F, all.x=T)[-1]

}
}

}

cat("\nClassifying data...\n")

for(i in 1:length(config_table[,1])){

A_scenario<-gsub(",", "/", config_table[i,2])
B_scenario<-gsub(",", "/", config_table[i,3])

C_scenario<-NA

index<-results[[eval(paste("boosting_",i,"_classification",sep=""))]]=="A"
index[is.na(index)]<-FALSE
results[[eval(paste("boosting_",i,"_classification",sep=""))]][index]<-A_scenario

index<-results[[eval(paste("boosting_",i,"_classification",sep=""))]]=="B"
index[is.na(index)]<-FALSE
results[[eval(paste("boosting_",i,"_classification",sep=""))]][index]<-B_scenario

index<-results[[eval(paste("boosting_",i,"_classification",sep=""))]]=="C"
index[is.na(index)]<-FALSE
results[[eval(paste("boosting_",i,"_classification",sep=""))]][index]<-C_scenario

}

return(results)

}
