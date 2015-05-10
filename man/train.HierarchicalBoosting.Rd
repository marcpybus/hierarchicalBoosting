\name{train.HierarchicalBoosting}
\alias{train.HierarchicalBoosting}
\title{
	Training of a hierarchicalBoosting object 
}
\description{
	This function training the boosting functions described in the configuration table using the training dataset that contain different simulated selective scenarios.
}
\usage{
	train.HierarchicalBoosting(input, config_table, bootstrap_iterations, bootstrap_sampling, boosting_iterations = 1000)
}
\arguments{
  \item{input}{
	training dataset containing the selection tests scores (columns) for different simulated selective scenarios and replicates (rows). 
}
  \item{config_table}{
	the configuration table describing the features for a hierarchicalBoosting object estimation.
}
  \item{bootstrap_iterations}{
	number of bootstrap iterations performed by the algorithm (min 10).
}
  \item{bootstrap_sampling}{
	fraction of each training dataset used to estimate the boosting functions (use 0.9 or less).
}
  \item{boosting_iterations}{
	number of iterations performed by the boosting algorithm. default is 1000, if convergence is not reached you can increase this value.
}
}

\value{
	a hierarchicalBoosting object
  \item{matrix_classifiers}{
	a matrix containing all the estimated boosting functions.
  }
  \item{matrix_coefficients}{
	a matrix containing all the coefficients estimated.
  }
  \item{matrix_thresholds}{
	a matrix containing significance thresholds for the scenarios to classify.
  }

}
\author{
	Marc pybus
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

}

