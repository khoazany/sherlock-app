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
  "Burglary" = 12, "Criminal damage and arson" = 13,
  "Drugs" = 14, "Public disorder and weapons" = 15,
  "Robbery" = 16, "Shoplifting" = 17,
  "Vehicle crime" = 18, "Violent crime" = 19, 
  "Other theft" = 20, "Other crime" = 21
)

attribute_url <- c(
  "Income" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Factors/MSOA_Income.geojson",
  "Employment" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Factors/MSOA_Employment.geojson",
  "Health deprivation and disability" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Factors/MSOA_Health.geojson",
  "Education skills and training" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Factors/MSOA_Education.geojson",
  "Barriers to housing and services" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Factors/MSOA_Barriers_to_Housing.geojson",
  "Living environment score" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Factors/MSOA_Living_Env.geojson",
  "Transaction prices (2013)" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Factors/MSOA_Propertyprice2013.geojson",
  "Population" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Factors/MSOA_Pop_Area_popden.geojson",
  "% of land used for residential" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Factors/MSOA_domestic_area.geojson",
  "No. of police stations" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Factors/MSOA_Police_Point.geojson",
  "Anti-social behaviour" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Crime-polygon/antisocial_behaviour.geojson", 
  "Bike theft" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Crime-polygon/bike_theft.geojson",
  "Burglary" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Crime-polygon/burglary.geojson", 
  "Criminal damage and arson" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Crime-polygon/criminal_damage_arson.geojson",
  "Drugs" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Crime-polygon/drugs.geojson",
  "Public disorder and weapons" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Crime-polygon/public_order.geojson",
  "Robbery" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Crime-polygon/robbery.geojson", 
  "Shoplifting" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Crime-polygon/shop_lifting.geojson",
  "Vehicle crime" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Crime-polygon/vehicle_crime.geojson", 
  "Violent crime" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Crime-polygon/violent_and_sexual_offences.geojson", 
  "Other theft" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Crime-polygon/other_theft.geojson", 
  "Other crime" = "https://raw.githubusercontent.com/khoazany/sherlock-app/master/Crime-polygon/other_crime.geojson"
)

attribute_column = c(
  "All" = "CRDENSITY",
  "Income" = "MSOA_Incom",
  "Employment" = "MSOA_Emplo",
  "Health deprivation and disability" = "MSOA_Healt",
  "Education skills and training" = "MSOA_Educa",
  "Barriers to housing and services" = "MSOA_Barri",
  "Living environment score" = "MSOA_Livin",
  "Transaction prices (2013)" = "MSOA_Prope",
  "Population" = "MSOA_Pop_1",
  "% of land used for residential" = "MSOA_Zonin",
  "No. of police stations" = "PSDensity",
  "Anti-social behaviour" = "CRDENSITY",
  "Bike theft" = "CRDENSITY",
  "Burglary" = "CRDENSITY",
  "Criminal damage and arson" = "CRDENSITY",
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
  "Criminal damage and arson" = "",
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