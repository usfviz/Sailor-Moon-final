# Project: Sailor Moon


| **Name**  | Donny Chen  | Yixin Zhang |
|----------:|:-------------|:-------------|
| **Email** | dchen25@usfca.edu | yzhang285@usfca.edu |


## Global Violence Visualizer

“I am Sailor Moon, champion of justice! On behalf of the moon, I will right wrongs and triumph over evil, and that means you!" - Sailor Moon


## Instructions

The following packages must be installed prior to running this code:

- `gdeltr2`
- `shiny`
- `GGally`
- `reshape`
- `sp`
- `rworldmap`
- `tidyr`
- `dplyr`
- `googleVis`
- `countrycode`
- `leaflet`
- `magrittr`

To run this code, please enter the following commands in R:

```
shiny::runGitHub('usfviz/Sailor-Moon-final')
```


## Discussion

Below are three screenshots of the interface of the shiny app.

![IMAGE](/screenshots/shinyapp1.png)

![IMAGE](/screenshots/shinyapp2.png)

![IMAGE](/screenshots/shinyapp3.png)

![IMAGE](/screenshots/shinyapp4.png)


### Dataset

The GDELT database is the largest, most comprehensive, and highest resolution open database of human society. It is also 100% free and accessible via Google BigQuery. The database contains geographic, temporal, network, and textual information for media events across the globe. We think that it is a good idea either to visualize the events on a geographic and temporal map or to visualize the global knowledge graph network on the map. Moreover, an interactive visualization can enable us to grasp the intuitive understanding of the dynamic connection between different parts of the world from a media perspective.

### UI

All the avaiable plots can currently interact with user-select date and news/news source type. Ideally, we expect to implement more interactions given the mightiness of GDELT dataset. However, due to the limitation of the `gdeltr2` package we currently used to query GDELT database, there is limited information suitable for visualization. We expect to offer more visualization as we explore more tables provided by the `gdeltr2` or other means to query the database.
