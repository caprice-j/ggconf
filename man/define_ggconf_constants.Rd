% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prefix_match.R
\name{define_ggconf_constants}
\alias{define_ggconf_constants}
\title{define constant values used in ggconf}
\usage{
define_ggconf_constants()
}
\description{
\code{define_ggconf_constants} has no side effect.
}
\details{
One thing to note is \code{define_ggconf_constants} set implicitly
the preference order of geom_name in ggplot2.
For example, 'a.txt' ambiguously matches to 'axis.text' and 'axis.title',
but ggconf automatically uses 'axis.text'
with or without a warning message about the ambiguity.
}
\seealso{
The preference order is used
         when doing partial match in GgplotParser.
}
