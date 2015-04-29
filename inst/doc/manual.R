### R code from vignette source 'manual.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: manual.Rnw:155-156
###################################################
options(width=60)


###################################################
### code chunk number 2: manual.Rnw:158-159
###################################################
library("hierarchicalBoosting")


###################################################
### code chunk number 3: manual.Rnw:162-165
###################################################
input_path <- system.file("exdata/training_data.txt", 
	package="hierarchicalBoosting")
input <- read.table(input_path, header=T, stringsAsFactors=F)


###################################################
### code chunk number 4: manual.Rnw:168-174
###################################################
boosting_1 <- c()
boosting_1["input_data"] <- "original"
boosting_1["scenario_A"] <- "recent_complete,ancient_complete"
boosting_1["scenario_B"] <- "neutral,recent_incomplete,ancient_incomplete"
boosting_1["threshold_scenario_A"] <- FALSE
boosting_1["threshold_scenario_B"] <- 0.01


###################################################
### code chunk number 5: manual.Rnw:176-182
###################################################
boosting_2 <- c()
boosting_2["input_data"] <- "boosting_1:scenario_B"
boosting_2["scenario_A"] <- "neutral"
boosting_2["scenario_B"] <- "recent_incomplete,ancient_incomplete"
boosting_2["threshold_scenario_A"] <- 0.99
boosting_2["threshold_scenario_B"] <- FALSE


###################################################
### code chunk number 6: manual.Rnw:184-190
###################################################
boosting_3 <- c()
boosting_3["input_data"] <- "boosting_1:scenario_A"
boosting_3["scenario_A"] <- "recent_complete"
boosting_3["scenario_B"] <- "ancient_complete"
boosting_3["threshold_scenario_A"] <- 0.95
boosting_3["threshold_scenario_B"] <- 0.05


###################################################
### code chunk number 7: manual.Rnw:192-198
###################################################
boosting_4 <- c()
boosting_4["input_data"] <- "boosting_2:scenario_B"
boosting_4["scenario_A"] <- "recent_incomplete"
boosting_4["scenario_B"] <- "ancient_incomplete"
boosting_4["threshold_scenario_A"] <- 0.95
boosting_4["threshold_scenario_B"] <- 0.05


###################################################
### code chunk number 8: manual.Rnw:201-203
###################################################
config_table <- data.frame(rbind(boosting_1, boosting_2, boosting_3, 
	boosting_4), stringsAsFactors=F)


###################################################
### code chunk number 9: manual.Rnw:206-208
###################################################
bootstrap_iterations <- 20
bootstrap_sampling <- 0.9


###################################################
### code chunk number 10: manual.Rnw:211-212
###################################################
check.HierarchicalBoosting(config_table, input)


###################################################
### code chunk number 11: manual.Rnw:215-217
###################################################
hierarchicalBoosting <- train.HierarchicalBoosting(input, config_table,
	bootstrap_iterations, bootstrap_sampling)


###################################################
### code chunk number 12: manual.Rnw:220-221
###################################################
plot.HierarchicalBoosting(hierarchicalBoosting, config_table, input)


###################################################
### code chunk number 13: manual.Rnw:224-227
###################################################
empirical_path <- system.file("exdata/SLC45A2_CEU_sweep.txt",
	 package="hierarchicalBoosting")
empirical <- read.table(empirical_path, header=T, stringsAsFactors=F)


###################################################
### code chunk number 14: manual.Rnw:230-232
###################################################
check.HierarchicalBoosting(config_table, input, 
	hierarchicalBoosting=hierarchicalBoosting)


###################################################
### code chunk number 15: manual.Rnw:235-237
###################################################
hierarchicalBoosting_results <- apply.HierarchicalBoosting(empirical, 
	config_table, hierarchicalBoosting)


###################################################
### code chunk number 16: manual.Rnw:240-242
###################################################
classification <- summarize.HierarchicalBoosting(hierarchicalBoosting_results)
print(classification)


