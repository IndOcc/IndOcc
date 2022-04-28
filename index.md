# Longitudinal Classification and Predictive Modeling for Historical CPS Data Using Random Forests
  
|                       **Cecile K. Johnson**                       |               **Hannah E. Schmuckler**              |
|:-----------------------------------------------------------------:|:---------------------------------------------------:|
|           _MS Data Science, University of Virginia '22_           |    _MS Data Science, University of Virginia '22_    |
|              [GitHub](https://github.com/ckjohnson7)              |       [GitHub](https://github.com/hschmuckler)      |
| [LinkedIn](https://www.linkedin.com/in/cecile-johnson-291a43108/) | [LinkedIn](https://www.linkedin.com/in/schmuckler/) |


## Our Work
- [Paper](https://ssrn.com/abstract=4094304)
- Our [Libra Dataverse Repository](https://doi.org/10.18130/V3/6RUBPV) contains trained forests and over 450 million predictions. ([README](https://dataverse.lib.virginia.edu/file.xhtml?fileId=57998&version=2.0))
- The machine readable [Industry](https://github.com/IndOcc/CPScrosswalks/blob/main/IND_crosswalk_FULL.csv) and [Occupation](https://github.com/IndOcc/CPScrosswalks/blob/main/OCC_crosswalk_FULL.csv) crosswalks between census codes encompassing the 70s to current are available for other researchers. ([GitHub](https://github.com/IndOcc/CPScrosswalks))


### Example Code
- Building Random Forests: [Industry](https://github.com/IndOcc/IndOcc/blob/main/01_BuildRandomForestsIND.R) and [Occupation](https://github.com/IndOcc/IndOcc/blob/main/02_BuildRandomForestsOCC.R)
- Making Predictions: [Industry](https://github.com/IndOcc/IndOcc/blob/main/03_PredictionIND.R) and [Occupation](https://github.com/IndOcc/IndOcc/blob/main/04_PredictionOCC.R)
- Vignette: [Predicting Industry Using Random Forests Built with Crosswalked Data](https://rpubs.com/mmc4cv/pred_ind_rf_cw)
- Vignette: [Utilizing Resources from Random Forest CPS Predictions](https://rpubs.com/mmc4cv/utilizing-resources_rf_preds)

### Appendix
- [Generating Random Forests and Predictions Using Crosswalked Data](https://rpubs.com/mmc4cv/appendix)

### Other
In the course of our work, we cleaned the "Treiman File" dual-coded Industry & Occupation dataset (1970-1980) for use. We make it available for the benefit of other researchers. 
- [Treiman Dual-Coded File](https://github.com/IndOcc/TreimanCleaned)
