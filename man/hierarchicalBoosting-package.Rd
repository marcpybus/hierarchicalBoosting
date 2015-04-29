\name{hierarchicalBoosting-package}
\alias{hierarchicalBoosting-package}
\alias{hierarchicalBoosting}
\docType{package}
\title{
Classification of selective sweeps through boosting-based decision trees
}
\description{
This package uses scores from several selection tests in simulated population genetics datasets to estimate parameters of a classfication algorithm (hierarchical boosting). The resulting algorithm then can be used to classify real genomic regions into differnt types of selective sweeps. The package does not simulate the training datasets nor calculate the selection tests, these need to be calculated prior to the use of this package.
}
\details{
\tabular{ll}{
Package: \tab hierarchicalBoosting\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-04-20\cr
License: \tab GPL (>=2)\cr
}
}
\author{
Marc Pybus

Maintainer: Marc Pybus <marc.pybus@upf.edu>
}
\examples{
# load input dataset
input_path <- system.file("exdata/training_data.txt", package="hierarchicalBoosting")
input <- read.table(input_path,header=TRUE,stringsAsFactors=FALSE)

# define individual boosting parameters
boosting_1<-c()
boosting_1["input_data"]<-"original"
boosting_1["scenario_A"]<-"recent_complete,ancient_complete"
boosting_1["scenario_B"]<-"neutral,recent_incomplete,ancient_incomplete"
boosting_1["threshold_scenario_A"]<-FALSE
boosting_1["threshold_scenario_B"]<-0.01

boosting_2<-c()
boosting_2["input_data"]<-"boosting_1:scenario_B"
boosting_2["scenario_A"]<-"neutral"
boosting_2["scenario_B"]<-"recent_incomplete,ancient_incomplete"
boosting_2["threshold_scenario_A"]<-0.99
boosting_2["threshold_scenario_B"]<-FALSE

boosting_3<-c()
boosting_3["input_data"]<-"boosting_1:scenario_A"
boosting_3["scenario_A"]<-"recent_complete"
boosting_3["scenario_B"]<-"ancient_complete"
boosting_3["threshold_scenario_A"]<-0.95
boosting_3["threshold_scenario_B"]<-0.05

boosting_4<-c()
boosting_4["input_data"]<-"boosting_2:scenario_B"
boosting_4["scenario_A"]<-"recent_incomplete"
boosting_4["scenario_B"]<-"ancient_incomplete"
boosting_4["threshold_scenario_A"]<-0.95
boosting_4["threshold_scenario_B"]<-0.05

# create configuration table
config_table<-data.frame(rbind(boosting_1,boosting_2,boosting_3,boosting_4),stringsAsFactors=FALSE)

# define bootstrapping parameters
bootstrap_iterations<-10
bootstrap_sampling<-0.9

# check compatibility 
check.HierarchicalBoosting(config_table,input)

# create hierarchicalBoosting object
hierarchicalBoosting <- train.HierarchicalBoosting(input, config_table, bootstrap_iterations, bootstrap_sampling)

# create validation plots
plot.HierarchicalBoosting(hierarchicalBoosting, config_table, input)

#load empirical dataset
empirical_path <- system.file("exdata/SLC45A2_CEU_sweep.txt", package="hierarchicalBoosting")
empirical <- read.table(empirical_path,header=TRUE,stringsAsFactors=FALSE)

# check compatibility
check.HierarchicalBoosting(config_table,input,hierarchicalBoosting=hierarchicalBoosting)

# apply hierarchicalBoosting object to empirical dataset
hierarchicalBoosting_results <- apply.HierarchicalBoosting(empirical, config_table, hierarchicalBoosting)

# summarize hierarchicalBoosting results
classification <- summarize.HierarchicalBoosting(hierarchicalBoosting_results)

}
