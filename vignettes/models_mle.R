## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----libs, warning=FALSE, message=FALSE---------------------------------------
library(streamMetabolizer)
library(dplyr)

## ----data---------------------------------------------------------------------
dat <- data_metab(num_days='3', res='15', day_start=4, day_end=28)

## ----mle_name-----------------------------------------------------------------
mle_name <- mm_name(type='mle')
mle_name

## ----mle_specs----------------------------------------------------------------
mle_specs <- specs(mle_name)
mle_specs

## ----specs_details------------------------------------------------------------
mle_specs <- specs(mle_name, init.GPP.daily=2, init.ER.daily=-1, init.K600.daily=3)

## ----mle_fit, warning=FALSE---------------------------------------------------
mm <- metab(mle_specs, data=dat, info=c(site='French Creek, WY', source='Bob Hall'))

## ----show---------------------------------------------------------------------
mm

## ----info---------------------------------------------------------------------
get_info(mm)
head(get_data(mm))

## ----info2--------------------------------------------------------------------
get_fitting_time(mm) # the time it took to fit the model
get_version(mm) # the streamMetabolizer version used to fit the model
get_specs(mm) # the specifications we passed in

## ----plot_metab1, fig.width=7, fig.height=4.5---------------------------------
plot_metab_preds(mm)

## ----plot_metab2, fig.width=7, fig.height=6-----------------------------------
plot_DO_preds(mm)

## ----pred_dfs-----------------------------------------------------------------
predict_metab(mm)
head(predict_DO(mm))

