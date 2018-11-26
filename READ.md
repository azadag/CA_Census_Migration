# ACS Migration In-Out Table Creator

## Data

This uses ACS input tables from iPUMS... the variables that you need are:

There's a slight discrepancy from previous year and current year location, this 
is noted and mostly affects some counties in Alabama, IPUMS has chosen to bury 
its cross-walk table but its still good for most other locations. 

The second cross-walk that's required is a PUMA geography to county one, the 
University of Missouri State Data Center web page has a set of wonderful tools
to help with this translation depending on what type of geographic roll-up is
required, I don't do any areal weighting or division here so I don't get caught
in deciding how to apportion a PUMA across METROS that cross  state boundaries
for example. Census and geographic data can get complicated!


## Visualization



QGIS visualization created with Anita Grasser (sp?)'s flow package. It takes some work to make it look good, but its still relatively easier


Other ways of visualizaing this data: sankey/ flow chart types which are better
for one-way or net flows. In general migration data can get very busy, so it
helps to consolidate areas into properly generalized groupings. For example,
inner city to suburbs, to out-of-state flows depending on what you want to
emphasize.

### Citations

Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics. DOI: 10.1093/bioinformatics/btu393

IPUMS cite - Ruggle 2014