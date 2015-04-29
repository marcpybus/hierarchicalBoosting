check.HierarchicalBoosting <-
function(config_table,input,hierarchicalBoosting=NULL){

boosting_names <- rownames(config_table)
col_names <- colnames(config_table)

if(!all(col_names==c("input_data","scenario_A","scenario_B","threshold_scenario_A","threshold_scenario_B"))){
stop("config_table wrong. colnames must be: input_data scenario_A scenario_B threshold_scenario_A threshold_scenario_B")
}

if(sum(match("boosting_1",boosting_names,nomatch=0))==0){
stop("config_table wrong. check that boostings are merged properly")
}

scenarios <- unique(sort(input$scenario))

cat("checking scenarios names...")
for(i in 1:length(config_table[,1])){
competing_A_scenarios<-strsplit(config_table[i,]$scenario_A,",")[[1]]
competing_B_scenarios<-strsplit(config_table[i,]$scenario_B,",")[[1]]
for(scen in competing_B_scenarios){
if(sum(match(scenarios,scen,nomatch=0))==0){
stop(paste("scenario '",scen,"' in scenario_B from ",boosting_names[i]," not found in input table scenarios (",paste(scenarios,collapse=","),")",sep=""))
}
}
for(scen in competing_A_scenarios){
if(sum(match(scenarios,scen,nomatch=0))==0){
stop(paste("scenario '",scen,"' in scenario_A from ",boosting_names[i]," not found in input table scenarios (",paste(scenarios,collapse=","),")",sep=""))
}
}
}
cat("ok\n")
cat("checking significance thresholds...")
for(i in 1:length(config_table[,1])){
if(config_table[i,]$threshold_scenario_A == FALSE | (config_table[i,]$threshold_scenario_A) >= 0 & as.numeric(config_table[i,]$threshold_scenario_A <=1)){
}else{
stop(paste("threshold for scenario A in ",boosting_names[i]," must be between 0 and 1 or FALSE",sep=""))
}
if(config_table[i,]$threshold_scenario_B == FALSE | (config_table[i,]$threshold_scenario_B) >= 0 & as.numeric(config_table[i,]$threshold_scenario_B <=1)){
}else{
stop(paste("threshold for scenario B in ",boosting_names[i]," must be between 0 and 1 or FALSE",sep=""))
}
}
cat("ok\n")
cat("checking classification tree structure...")
if(config_table[1,]$input_data != "original"){
stop("first boosting should use as input_data the original dataset.")
}
original_present<-FALSE
prev_boostings<-c()

for(i in 1:length(config_table[,1])){
if(config_table[i,]$input_data == "original"){
if(original_present == FALSE){
original_present<-TRUE
prev_boostings<-c(prev_boostings,boosting_names[i])
}else{
warning("'original' input_data parameter found in more than one boosting function. for a truly hierarchical structure only one must be present")
}
}else{
if(!grep(":",config_table$input_data[i])){
stop(paste("input_data parameters for ",boosting_names[i]," incorrectly defined. structure should be: boosting_X:scenario_Y",sep=""))
}
relation<-strsplit(config_table$input_data[i],":")[[1]]
if(is.na(pmatch("boosting_",relation[1])) | is.na(pmatch("scenario_",relation[2]))){
stop(paste("input_data parameters for ",boosting_names[i]," incorrectly defined. structure should be: boosting_X:scenario_Y",sep=""))
}
boosting<-strsplit(relation[1],"_")[[1]][2]
if(boosting>=1 & boosting<=length(config_table[,1])){}else{
stop(paste("boosting number in input_data parameter for ",boosting_names[i]," must be: ",paste(seq(1:length(config_table[,1])),sep=" "),sep=""))
}
if(sum(match(prev_boostings,relation[1],nomatch=0))==0){
stop(paste("input_data for ",boosting_names[i]," (",config_table$input_data[i],") must be generated before use it. check boosting order",sep=""))
}

scenario<-strsplit(relation[2],"_")[[1]][2]
if(scenario == "A" | scenario == "B" | scenario == "C"){}else{
stop(paste("scenario name in input_data parameter for ",boosting_names[i]," must be: A, B or C",sep=""))
}

if(scenario == "C" & (config_table[as.numeric(boosting),]$threshold_scenario_A == FALSE | config_table[as.numeric(boosting),]$threshold_scenario_B == FALSE)){
stop("to use the scenario C input_data must come from a boosting with two thresholds")
}
prev_boostings<-c(prev_boostings,boosting_names[i])
}
}
cat("ok\n")

if(!is.null(hierarchicalBoosting)){

cat("checking compatibility: input data and hierarchicalBoosting object...")

if(is.list(hierarchicalBoosting) & is.list(hierarchicalBoosting$matrix_classifiers) & is.list(hierarchicalBoosting$matrix_coefficients) & is.matrix(hierarchicalBoosting$matrix_coefficients)){

boost_stats <- colnames(hierarchicalBoosting$matrix_coefficients[[1,1]])[-1]
input_stats <- colnames(input)[-1]

if(all(boost_stats==input_stats)){
cat("ok\n")
}else{
cat("\n")
stop("different statistics used to train the algorithm and those in input data")
}
}else{
stop("hierarchicalBoosting object does not come from train.HierarchicalBoosting function")
}
}
}
