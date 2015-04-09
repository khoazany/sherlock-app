attribute_id <- c(
  "Income" = 1,
  "Employment" = 2,
  "Health deprivation and disability" = 3,
  "Education skills and training" = 4,
  "Barriers to housing and services" = 5,
  "Living environment score" = 6,
  "Transaction prices (2013)" = 7,
  "Population" = 8,
  "% of land used for residential" = 9,
  "No. of police stations" = 10,
  "Anti-social behaviour" = 11, 
  "Burglary" = 12, "Criminal damange and arson" = 13,
  "Drugs" = 14, "Public disorder and weapons" = 15,
  "Robbery" = 16, "Shoplifting" = 17,
  "Vehicle crime" = 18, "Violent crime" = 19, 
  "Other theft" = 20, "Other crime" = 21
)

attribute_url <- c(
  "Income" = "Factors affecting crime (by MSOA)/MSOA_Income.geojson",
  "Employment" = "Factors affecting crime (by MSOA)/MSOA_Employment.geojson",
  "Health deprivation and disability" = "Factors affecting crime (by MSOA)/MSOA_Health.geojson",
  "Education skills and training" = "Factors affecting crime (by MSOA)/MSOA_Education.geojson",
  "Barriers to housing and services" = "Factors affecting crime (by MSOA)/MSOA_Barriers_to_Housing.geojson",
  "Living environment score" = "Factors affecting crime (by MSOA)/MSOA_Living Env.geojson",
  "Transaction prices (2013)" = "Factors affecting crime (by MSOA)/MSOA_Propertyprice2013.geojson",
  "Population" = "Factors affecting crime (by MSOA)/MSOA_Pop_Area_popden.geojson",
  "% of land used for residential" = "Factors affecting crime (by MSOA)/MSOA_domestic_area.geojson",
  "No. of police stations" = "Factors affecting crime (by MSOA)/MSOA_Police_Point.geojson",
  "Anti-social behaviour" = "Crime-polygon/antisocial_behaviour.geojson", 
  "Bike theft" = "Crime-polygon/bike_theft.geojson",
  "Burglary" = "Crime-polygon/burglary.geojson", 
  "Criminal damange and arson" = "Crime-polygon/criminal_damage_arson.geojson",
  "Drugs" = "Crime-polygon/drugs.geojson",
  "Public disorder and weapons" = "Crime-polygon/public_order.geojson",
  "Robbery" = "Crime-polygon/robbery.geojson", "Shoplifting" = "Crime-polygon/shop_lifting.geojson",
  "Vehicle crime" = "Crime-polygon/vehicle_crime.geojson", 
  "Violent crime" = "Crime-polygon/violent_and_sexual_offences.geojson", 
  "Other theft" = "Crime-polygon/other_theft.geojson", 
  "Other crime" = "Crime-polygon/other_crime.geojson"
)

attribute_column = c(
  "All" = "CRDENSITY",
  "Income" = "MSOA_Incom",
  "Employment" = "MSOA_Emplo",
  "Health deprivation and disability" = "MSOA_Healt",
  "Education skills and training" = "MSOA_Educa",
  "Barriers to housing and services" = "MSOA_Barri",
  "Living environment score" = "MSOA_do",
  "Transaction prices (2013)" = "MSOA_Prope",
  "Population" = "MSOA_Pop_1",
  "% of land used for residential" = "MSOA_Zonin",
  "No. of police stations" = "PSDensity",
  "Anti-social behaviour" = "CRDENSITY",
  "Bike theft" = "CRDENSITY",
  "Burglary" = "CRDENSITY",
  "Criminal damange and arson" = "CRDENSITY",
  "Drugs" = "CRDENSITY", 
  "Public disorder and weapons" = "CRDENSITY",
  "Robbery" = "CRDENSITY", "Shoplifting" = "CRDENSITY",
  "Vehicle crime" = "CRDENSITY", "Violent crime" = "CRDENSITY", 
  "Other theft" = "CRDENSITY", "Other crime" = "CRDENSITY"
)

attribute_user <- c(
  "Income" = "",
  "Employment" = "",
  "Health deprivation and disability" = "",
  "Education skills and training" = "",
  "Barriers to housing and services" = "",
  "Living environment score" = "",
  "Transaction prices (2013)" = "",
  "Population" = "",
  "% of land used for residential" = "",
  "No. of police stations" = "",
  "Anti-social behaviour" = "", 
  "Bike theft" = "",
  "Burglary" = "",
  "Criminal damange and arson" = "",
  "Drugs" = "", 
  "Public disorder and weapons" = "",
  "Robbery" = "", "Shoplifting" = "",
  "Vehicle crime" = "", "Violent crime" = "", 
  "Other theft" = "", "Other crime" = ""
)

attributes <- c(
  "Income" = "Income",
  "Employment" = "Employment",
  "Health deprivation and disability" = "Health deprivation and disability",
  "Education skills and training" = "Education skills and training",
  "Barriers to housing and services" = "Barriers to housing and services",
  "Living environment score" = "Living environment score",
  "Transaction prices (2013)" = "Transaction prices (2013)",
  "Population" = "Population",
  "% of land used for residential" = "% of land used for residential",
  "No. of police stations" = "No. of police stations"
)

crimeTypes <- c(
  "Anti-social behaviour" = "Anti-social behaviour", 
  "Bike theft" = "Bike theft",
  "Burglary" = "Burglary", "Criminal damage and arson" = "Criminal damage and arson",
  "Drugs" = "Drugs", "Public disorder and weapons" = "Public disorder and weapons",
  "Robbery" = "Robbery", "Shoplifting" = "Shoplifting",
  "Vehicle crime" = "Vehicle crime", "Violent crime" = "Violent crime", 
  "Other theft" = "Other theft", "Other crime" = "Other crime"
)

userData = c()

getColor <- function(d) {
  if (d > 0.875) {
    return("#800026")
  } else if (d > 0.75) {
    return("#BD0026")
  } else if (d > 0.625) {
    return("#E31A1C")
  } else if (d > 0.5) {
    return("#FC4E2A")
  } else if (d > 0.375) {
    return("#FD8D3C")
  } else if (d > 0.25) {
    return("#FEB24C")
  } else if (d > 0.125) {
    return("#FED976")
  } else {
    return("#FFEDA0")
  }
}

getColorNaturalBreak <- function(d) {
  if (d == 8) {
    return("#800026")
  } else if (d == 7) {
    return("#BD0026")
  } else if (d == 6) {
    return("#E31A1C")
  } else if (d == 5) {
    return("#FC4E2A")
  } else if (d == 4) {
    return("#FD8D3C")
  } else if (d == 3) {
    return("#FEB24C")
  } else if (d == 2) {
    return("#FED976")
  } else {
    return("#FFEDA0")
  }
}