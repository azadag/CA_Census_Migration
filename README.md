# ACS Migration In-Out Table Creator

## Data

This uses ACS input tables from iPUMS... the variables that you need are:
* "State", "PUMA", "MIGPLAC1", "MIGPUMA"
Or any variables that you want to filter and create demographic filters by.

There's a slight discrepancy from previous year and current year location, this 
is noted and mostly affects some counties in Alabama, IPUMS has chosen to bury 
its cross-walk table while it makes the correction, but you can still find it
and its still good for most of the country. 

The second cross-walk that's required is a PUMA geography to county one, the 
University of Missouri State Data Center web page has a set of wonderful tools
to help with this translation depending on what type of geographic roll-up is
required, areal weighting (apportioning the PUMA or division over multiple areas) 
but keep that in mind depending on the area that you want to roll up to, 
especially if you need to apportion a PUMA across METROS that cross  state boundaries
for example. Census and geographic data can get complicated!


## Quick Visualization

Some quick visualizations - there are honestly so many: using the R circalize package
<img src="\charts_out\Rplot-circalize.png">

This chart filters out migration cells with less than 4000 households, just to simplify
the data. There is lots of migration between Los Angeles and the Inland Empire, but not as much
between the Inland-Empire and the Bay Area, San Diego is largely self-contained.

Other ways of visualizaing this data: sankey/ flow chart types which are better
for one-way or net flows. In general migration data can get very busy, so it
helps to consolidate areas into properly generalized groupings. For example,
inner city to suburbs, to out-of-state flows depending on what you want to
emphasize.

## Map Visualizations

I also tried my hand at "great (well maybe not so great?) circle" gis maps using
QGIS  with Anita Grasser (sp?)'s flow package. It takes some work to make it look good, 
but it's much easier to use this to show net-migration change by arrow-thickness and shape.

The general pattern for migration in the bay area, is from the 6 or 7 central bay counties into 
peripheral counties as people get priced out of the bay. Then, as people get priced out 
of the outer-bay counties they simply move out of the state.

These charts still get busy, animated charts could help simplify some of that.

### Core to Periphery

<img src="\charts_out\_pdf_core_out.png">


<img src="\charts_out\Periphery-100.jpg">

### Periphery To Out of State

<img src="\charts_out\Core-100.jpg">


### Citations

Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics. DOI: 10.1093/bioinformatics/btu393

IPUMS cite - Ruggle 2014
