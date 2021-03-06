% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline_ordinal.R
\name{pipeline_ordinal}
\alias{pipeline_ordinal}
\title{Ordinal Regression Pipeline}
\usage{
pipeline_ordinal(learner)
}
\arguments{
\item{learner}{:: \code{\link[mlr3:LearnerClassif]{mlr3::LearnerClassif}} or \code{\link[mlr3:LearnerRegr]{mlr3::LearnerRegr}}.}
}
\value{
\code{\link[mlr3pipelines:Graph]{Graph}}.
The resulting graph can used as \code{\link[mlr3pipelines:mlr_learners_graph]{GraphLearner}}
}
\description{
Creates a graph of an ordinal regression pipeline for a regression or classification learner
}
\examples{
library(mlr3)
library(mlr3pipelines)
task = tsk("winerating")
learner = lrn("regr.rpart")
graph = ppl("ordinal", learner)
glearner = GraphLearner$new(graph, task_type = "ordinal")
resampling = rsmp("cv")

# explicitly instantiate the resampling for this task for reproduciblity
set.seed(123)
resampling$instantiate(task)

rr = resample(task, glearner, resampling)
print(rr)

# retrieve performance
rr$aggregate(msr("ordinal.ce"))
}
