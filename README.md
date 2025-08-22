# CrosswindLake_Laketrout

## Overview

This study will use 2 different models to estimate sustainable yield of lake trout Salvelinus namaycush from Crosswind Lake. The lake area (LA) model was developed from data compiled from 43 lakes located in Ontario, Canada and predicts yield potential (YP) from the surface area of a given lake. The Lester model is a newer, more comprehensive model that considers information from 456 lakes throughout Canada including the Yukon Territories and Northwest Territories. Unlike the LA model, the Lester model estimates maximum sustained yield (MSY), which is much different from YP, and of more use to fishery managers. The output from both models is in kg biomass, so the mean weight of lake trout subject to removal from Crosswind Lake needs to be estimated to convert biomass into total number of fish. This study will use lake trout catch data from winter (March) and summer (June) sampling to estimate mean weight, and the bottom of Crosswind Lake will be mapped to provide specific input variables for the models. The data collected will be used to apply the Lester model to information from an Alaska lake, which may provide additional insights to its suitability for management of lake trout in Alaska, and suggest further refinements to its adaptation . Estimates of yield will be used to evaluate the sustainability of the current sport fishery and to address any potential proposals to liberalize and/or restrict this fishery.

## Directory Structure

### **/OP_2024** contains materials relevant to the 2024 Operational Plan

-   **/R** contains R code for sample size and relative precision calculation

-   **/flat_data** contains relevant data as .csv, in this case historic sampling data for all lakes

-   **/comparable_op** contains miscellaneous materials from a similar Operational Plan, which most of the R code was adapted from

-   **/Lester_model_background** contains some background material specific to the Lester model.  It is likely that many of the materials contained here are not on the Github repository.  Note: this folder contains two subfolders that are git submodules on their own:

  - **/Lester_model_background/lake_trout_data** was an exploration of the lake trout database available at the time, and

  - **/Lester_model_background/Lake_trout_Lester** was a numerical and conceptual exploration of the Lester model.

### **/Analysis_2024** contains materials relevant to the 2024 analysis

-   **/R** contains R code

-   **/flat_data** contains data reformatted as .csv files

-   **/raw_data** contains original .xlsx files, which might not be here on the Github repository

### **/BOF_2024** contains materials related to a specific request for LAM results in preparation for Board of Fish.

- **/flat_data** contains data reformatted as a .csv file.  This dataset was received by email from Corey Schwanke, and is the subset of fish sampling data consisting of the March sample plus the subset of the June sample caught without bait, in order to be representative of the population available to harvest under current regulations.  This was actually equivalent to an equivalent subset of the data found in /Analysis_2024/flat_data.

- **/R ** contains R code

### **Output_for_2025_OPmeeting.Rmd** is a Mardown report containing the final analyses.

It was originally written in preparation for the Operational Plan meeting in Spring 2025, but was repurposed to serve as the final analysis reported in the **2025 FDS report**.

### BOF_2024.Rmd is a Markdown report summarizing the work within **/BOF_2024**
