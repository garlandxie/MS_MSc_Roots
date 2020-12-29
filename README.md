# MS_MSc_Roots

##### Manuscript: Root traits influence storm-water performance in a green roof microcosm
##### Authors: Garland Xie and Jeremy T. Lundholm

This project is current in preparation for New Phytologist

Code and data for this project are additionally available on [OSF](https://osf.io/57xym/)

### Abstract: 

Water uptake from vegetation can govern plant productivity through different mechanisms in natural terrestrial ecosystems. 
However, the ability of plants to regulate water can be viewed as an ecosystem service in urban regions, where vegetation in constructed ecosystems can help reduce stormwater runoff through their root systems. 
However, there is no knowledge on how root traits operate in extensive green roofs, an increasingly popular form of green infrastructure, as the substrate environment has an extremely shallow substrate depth (~20 cm) and a lack of biological legacy. 
In this study, we found that stormwater performance is not solely a function of rooting depth; rather, a multitude of belowground traits govern water regulation in this novel ecosystem that may optimize transport capacity, soil exploration and root construction costs. 
Overall, this study parallels findings from natural ecosystems and provides practitioners with new tools to create a more functional green roof.

### Using the code in this repository:
To use the code in this repository to reproduce the manuscript's results,
please follow the following steps:
1. `git clone` this repository or download it as a zip folder
2. Open `Rstudio`, go to `file > Open project` and open the `Msc.Rproj`
Rproject associated with this repository
3. Run `renv::restore()` in your R console. Requires `renv` package (see [THIS](https://rstudio.github.io/renv/articles/renv.html) vignette)

## Repo structure

The repository structure is described below

```
├── output
│   ├── figures: Directory of main figures in the manuscript and supplementary figures
├── data                          
│   ├── original: Directory of raw data with a master metadata file (.txt)
│   ├── working: Directory of intermediary files used in the analysis (see scripts in the src directory)
│   └── final: Directory of cleaned data sets for subsequent statistical analyses and figure creation
|
├── renv: Directory of information on package dependencies used in this project
│
├── src
│   ├── 01-01-data_clean_ij_rhizo.R: R script to clean dat on IJ Rhizo output files 
│   ├── 01-02-data_clean-below-biomass.R: R script to clean data on belowground biomass
│   ├── 01-03-data_clean-above_biomass.R: R script to clean data on aboveground biomass
│   ├── 01-04-data_clean-root_depth.R: R script to clean data on rooting depth
│   ├── 01-05-data_clean-pot_weights.R R script to clean data on pot weights
│   ├── 01-06-data_clean-joins.R: R script to create a data frame used in subsequent data analysis
|   ├── 02-03-LMM_tot_water_ret.R: R script to analyze regression models on stormwater capture
|   ├── 02-03-LMM_tot_water_loss: R script to analyze regression models on evaporative loss
|   ├── 03-01-PCA.R: R script to analyze a principal component analysis
|   ├── 04-ANOVA.R: R script to analyze an analysis of variance test
|
├── .Rprofile             : Contains R code to be run in each session.
├── .gitignore            : Files and directories to be ignored by git
├── Msc.Rproj             : RStudio project file
├── renv.lock             : A text log of package dependencies
└── README.md             : Description of this repository
```
