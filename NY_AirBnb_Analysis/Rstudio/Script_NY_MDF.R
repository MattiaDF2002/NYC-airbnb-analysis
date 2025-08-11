# --- Setup ---------------------------------------------------------
# Pacchetti necessari: tidyverse, dplyr, moments, corrplot
suppressPackageStartupMessages({
  library(tidyverse)
  library(dplyr)
  library(moments)
  library(corrplot)
})

options(scipen = 999)

# Paths
data_path <- file.path("data", "new_york_listings_2024.csv")
out_plots <- file.path("outputs", "plots")
dir.create(out_plots, showWarnings = FALSE, recursive = TRUE)

# --- Load ----------------------------------------------------------
NewYork <- read.csv(data_path, stringsAsFactors = TRUE)

# --- Region mapping ------------------------------------------------
NewYork$region <- NA
NY_North <- c("Marble Hill","Riverdale","Spuyten Duyvil","Fieldston","Kingsbridge","Norwood",
              "Woodlawn","Wakefield","Williamsbridge","Olinville","Edenwald","Baychester",
              "Eastchester","Bronxdale","Morris Park","Schuylerville","Pelham Bay","Pelham Gardens",
              "City Island","Throgs Neck","Co-op City","Hunts Point","Port Morris","Mott Haven",
              "Morrisania","Longwood","Melrose","Mount Eden","Mount Hope","Tremont","University Heights",
              "Highbridge","Concourse","Concourse Village","Claremont Village","East Morrisania","West Farms")
NewYork$region[NewYork$neighbourhood %in% NY_North] <- "North"

NY_South <- c("Battery Park City","Financial District","Tribeca","Chinatown","Two Bridges","Lower East Side",
              "SoHo","Nolita","Little Italy","Greenwich Village","West Village","NoHo","East Village","Gramercy",
              "Kips Bay","Murray Hill","Stuyvesant Town","Chelsea","Flatiron District","Midtown","Hell's Kitchen",
              "Theater District","Upper West Side","Upper East Side","Morningside Heights","Harlem","Civic Center",
              "DUMBO","Brooklyn Heights","Cobble Hill","Boerum Hill","Carroll Gardens","Red Hook","Gowanus",
              "Park Slope","Prospect Heights","Prospect-Lefferts Gardens","Windsor Terrace","South Slope","Greenpoint",
              "Williamsburg","Bushwick","Bedford-Stuyvesant","Clinton Hill","Fort Greene","Downtown Brooklyn",
              "Vinegar Hill","Navy Yard","Borough Park","Bensonhurst","Bath Beach","Dyker Heights","Bay Ridge",
              "Sunset Park","Breezy Point","Brighton Beach","Coney Island","Sea Gate","Gravesend","Sheepshead Bay",
              "Manhattan Beach","Mill Basin","Bergen Beach","Canarsie","Flatlands","East Flatbush","Brownsville",
              "East New York","Springfield Gardens","Rosedale","Laurelton","Cambria Heights","St. Albans",
              "South Ozone Park","Ozone Park","Howard Beach","Lighthouse Hill","Arden Heights","Great Kills",
              "Tottenville","Rossville","Huguenot","Prince's Bay","Woodrow","Eltingville","New Dorp Beach","Grant City",
              "Emerson Hill","Todt Hill","Dongan Hills","Annadale","Richmondtown","Oakwood","South Beach","Midland Beach",
              "Shore Acres","Rosebank","St. George","Tompkinsville","Stapleton","Arrochar","New Brighton","Silver Lake",
              "Grymes Hill","Randall Manor","Port Richmond","West Brighton","Mariners Harbor","Howland Hook",
              "Graniteville","New Springville","Travis","Bloomfield","Chelsea, Staten Island","Willowbrook")
NewYork$region[NewYork$neighbourhood %in% NY_South] <- "South"

NY_East <- c("Astoria","Long Island City","Ditmars Steinway","Woodside","Sunnyside","Elmhurst",
             "Jackson Heights","Corona","Rego Park","Middle Village","Maspeth","Glendale","Ridgewood",
             "Cypress Hills","East Elmhurst","College Point","Whitestone","Bayside","Little Neck","Fresh Meadows",
             "Kew Gardens Hills","Kew Gardens","Briarwood","Jamaica Hills","Jamaica Estates","Jamaica","Queens Village",
             "Bellerose","Holliswood","Hollis","Springfield Gardens","Rosedale","Laurelton","Cambria Heights","St. Albans",
             "South Ozone Park","Ozone Park","Howard Beach","Edgemere","Arverne","Rockaway Beach","Far Rockaway",
             "Bayswater","Belle Harbor","Neponsit")
NewYork$region[NewYork$neighbourhood %in% NY_East] <- "East"

NY_West <- c("Columbia St","Concord","Clifton","Westerleigh")
NewYork$region[NewYork$neighbourhood %in% NY_West] <- "West"

NewYork$region <- factor(NewYork$region)

# --- Drop cols not used --------------------------------------------
drop_cols <- c("neighbourhood","neighbourhood_group","host_name","name",
               "host_id","id","longitude","latitude")
NewYork <- NewYork[ , setdiff(names(NewYork), drop_cols)]

# --- Types & cleaning ----------------------------------------------
NewYork$bedrooms <- as.integer(as.character(replace(NewYork$bedrooms, is.na(NewYork$bedrooms), 1)))
NewYork$baths    <- as.numeric(as.character(NewYork$baths))
NewYork$rating   <- as.numeric(as.character(NewYork$rating))

NewYork$last_review <- as.Date(NewYork$last_review)
NewYork$last_review <- cut(
  NewYork$last_review,
  breaks = as.Date(c("2011-01-01","2016-01-01","2021-01-01","2025-01-01")),
  labels = c("2011-2015","2016-2020","2021-2024"),
  right = FALSE
)

NewYork$license <- ifelse(grepl("OSE-ST[Rr]REG-", NewYork$license), "License",
                          ifelse(NewYork$license == "License", "No License", "Exempt"))
NewYork$license <- factor(NewYork$license, levels = c("License","No License","Exempt"))

NewYork <- na.omit(NewYork)

# --- Basic filtering (outlier rules come dal tuo lavoro) ------------
NewYork <- NewYork |> filter(price <= 1000)
NewYork <- NewYork |> filter(reviews_per_month <= 7)
NewYork <- NewYork |> filter(baths <= 4.5, baths != 0)
NewYork$baths <- recode(NewYork$baths, `1.5`=1, `2.5`=2, `3.5`=3, `4.5`=4, .default = NewYork$baths)
NewYork$baths <- as.integer(NewYork$baths)
NewYork <- NewYork |> filter(baths <= 3)
NewYork <- NewYork |> filter(minimum_nights <= 90)
NewYork <- NewYork |> filter(calculated_host_listings_count <= 83)
NewYork <- NewYork |> filter(bedrooms <= 4)
NewYork <- NewYork |> filter(beds <= 6)
NewYork <- NewYork |> filter(number_of_reviews <= 294)
NewYork <- NewYork |> filter(number_of_reviews_ltm <= 44)
NewYork <- NewYork |> filter(room_type != "Hotel room")

# --- Transforms -----------------------------------------------------
NewYork <- NewYork |>
  mutate(
    price1 = log(price),
    numberofreviews1 = log(number_of_reviews),
    reviews_per_month1 = log(reviews_per_month),
    rating_wins = {
      p1 <- quantile(rating, 0.01, na.rm = TRUE)
      p99 <- quantile(rating, 0.99, na.rm = TRUE)
      pmin(pmax(rating, p1), p99)
    },
    rating1 = log(rating_wins)
  )

# --- Example plot saved to outputs ---------------------------------
png(file.path(out_plots, "price_by_roomtype.png"), width=900, height=600)
boxplot(price1 ~ room_type, data = NewYork,
        main = "log(Price) by Room Type", xlab = "Room Type", ylab = "log(Price)")
dev.off()

# --- Simple linear model (versione finale snella) -------------------
NY <- NewYork |>
  select(price1, room_type, bedrooms, beds, baths, region)

lm_final <- lm(price1 ~ room_type + bedrooms + beds + baths + region, data = NY)
summary(lm_final)

# Salva summary su file
capture.output(summary(lm_final), file = file.path("outputs", "lm_summary.txt"))
