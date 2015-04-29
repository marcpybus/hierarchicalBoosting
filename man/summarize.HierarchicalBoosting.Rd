\name{summarize.HierarchicalBoosting}
\alias{summarize.HierarchicalBoosting}
\title{
	Summarize a hierarchicalBoosting results table
}
\description{
	This function summarize a hierarchicalBoosting results table by combining all the results of the estimated boosting functions to simplify its results.
}
\usage{
	summarize.HierarchicalBoosting(results)
}

\arguments{
  \item{results}{
	a hierarchicalBoosting results table
}
}
\value{
	a table with the final classification results for the empirical dataset analized.
}
\author{
	Marc Pybus
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

# create hierarchicalBoosting object
hierarchicalBoosting <- train.HierarchicalBoosting(input, config_table, bootstrap_iterations, bootstrap_sampling)

#load empirical dataset
input_empirical <- system.file("exdata/SLC45A2_CEU_sweep.txt", package="hierarchicalBoosting")
empirical <- read.table(input_empirical,header=TRUE,stringsAsFactors=FALSE)

# apply hierarchicalBoosting object to empirical dataset
hierarchicalBoosting_results <- apply.HierarchicalBoosting(empirical, config_table, hierarchicalBoosting)

# summarize hierarchicalBoosting results
classification <- summarize.HierarchicalBoosting(hierarchicalBoosting_results)
}

