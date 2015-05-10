\name{plot.HierarchicalBoosting}
\alias{plot.HierarchicalBoosting}
\title{
	Generate validation plots for a estimated hierarchicalBoosting object.
}
\description{
	This function generates three different validation plots for each estimated boosting function: (1) coefficient convergence plot: tracks the coefficient values at each iteration, one must control that coefficients reach convergence before stopping the algorithm, (2) standardized coefficient boxplot: shows the relevance of each input selection test at the estimeted boosting function and (3) boosting scores distributions: shows the score distributions for each competing scenarios, it also shows the significance thresholds used. 
}
\usage{
       plot.HierarchicalBoosting(hierarchicalBoosting, config_table, input, name = "")
}
\arguments{
  \item{hierarchicalBoosting}{
	a hierarchicalBoosting object estimated with the train.HierarchicalBoosting function.
}
  \item{config_table}{
	the configuration table describing the features for a hierarchicalBoosting object estimation.
}
  \item{input}{
	training dataset containing the selection tests scores (columns) for different simulated selective scenarios and replicates (rows). 
}
  \item{name}{
	(optional) filename prefix for the validation plots generated.
}
}
\value{
	a pdf file containing the generated validation plots.
}
\author{
	Marc Pybus
}
\examples{
# load input dataset
simulations.data.path <- system.file("exdata/training_data.txt", package="hierarchicalBoosting")
simulations.data <- read.table(simulations.data.path,header=TRUE,stringsAsFactors=FALSE)

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
hierarchicalBoosting <- train.HierarchicalBoosting(simulations.data, config_table, bootstrap_iterations, bootstrap_sampling)

# create validation plots
plot.HierarchicalBoosting(hierarchicalBoosting, config_table, simulations.data)

}

