% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metab_model_interface.R,
%   R/metab_model.get_param_names.R
\name{get_param_names}
\alias{get_param_names}
\alias{get_param_names.character}
\alias{get_param_names.metab_model}
\title{Extract the daily parameter names from a metabolism model.}
\usage{
get_param_names(metab_model, ...)

\method{get_param_names}{character}(metab_model, ...)

\method{get_param_names}{metab_model}(metab_model, ...)
}
\arguments{
\item{metab_model}{A metabolism model object or model name for which to
return the list of required and optional metabolism parameters.}

\item{...}{Placeholder for future arguments}
}
\value{
Returns a list of two vectors, the names of the required and optional
  daily metabolism parameters, respectively.
}
\description{
A function in the metab_model_interface. Returns vectors of the required and
optional daily metabolism parameters for the model.
}
\section{Methods (by class)}{
\itemize{
\item \code{get_param_names(character)}: This implementation is shared by many model types

\item \code{get_param_names(metab_model)}: Lets you pass in a model object rather than a
character string

}}
\examples{

# pass in a character string:
get_param_names(mm_name('mle', GPP_fun='satlight'))
get_param_names(mm_name('bayes'))
get_param_names(mm_name('Kmodel'))
get_param_names(mm_name('night'))
get_param_names(mm_name('sim'))

# or pass in a metab_model object:
dat <- data_metab('1','30')
get_param_names(metab(specs(mm_name('mle', ER_fun='q10temp')), data=dat))
get_param_names(metab(specs('night'), data=dat))
get_param_names(metab(specs('sim'), data=dat))
}
\seealso{
Other metab_model_interface: 
\code{\link{get_data}()},
\code{\link{get_data_daily}()},
\code{\link{get_fit}()},
\code{\link{get_fitting_time}()},
\code{\link{get_info}()},
\code{\link{get_params}()},
\code{\link{get_specs}()},
\code{\link{get_version}()},
\code{\link{predict_DO}()},
\code{\link{predict_metab}()}
}
\concept{metab_model_interface}
