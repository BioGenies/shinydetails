% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elements_h.R
\name{sliderInput_h}
\alias{sliderInput_h}
\title{Slider input with helper}
\usage{
sliderInput_h(inputId, helpfiles = "helpfiles", ...)
}
\arguments{
\item{inputId}{The \code{input} slot that will be used to access the value.}

\item{helpfiles}{A character string denoting directory to save empty help files.
Default \code{'helpfiles'}.}

\item{...}{Optional arguments for \code{sliderInput}.}
}
\description{
Constructs a slider widget with helper.
Same as \code{helper(sliderInput(outputId, ...))}. Creates help file and/or
directory if it does not exist.
}
\details{
This function uses \code{\link[shinyhelper]{helper}}.
}
\seealso{
For more elements with helpers see \code{\link[shinydetails]{numericInput_h}},
\code{\link[shinydetails]{plotOutput_h}}, \code{\link[shinydetails]{tableOutput_h}}
}
