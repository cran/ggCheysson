## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 7,
  fig.align = "center",
  warning = FALSE,
  message = FALSE
)

## ----packages-----------------------------------------------------------------
library(ggCheysson)
library(ggplot2)
library(Guerry)      # Historical data on France
library(sf)          # Modern spatial data handling
library(ggpattern)   # For Cheysson-style hatching patterns

## ----load-fonts, eval=FALSE---------------------------------------------------
# # Load Cheysson fonts
# load_cheysson_fonts(method = "showtext")
# showtext::showtext_auto()

## ----load-fonts-actual, include=FALSE-----------------------------------------
# Actual font loading (hidden from output)
if (requireNamespace("showtext", quietly = TRUE) &&
    requireNamespace("sysfonts", quietly = TRUE)) {
  load_cheysson_fonts(method = "showtext")
  showtext::showtext_auto()
  fonts_available <- TRUE
} else {
  fonts_available <- FALSE
}

## ----load-data----------------------------------------------------------------
# Load the dataset
data(Guerry, package = "Guerry")

# Key variables for mapping
vars_of_interest <- c("Crime_pers", "Crime_prop", "Literacy",
                      "Donations", "Infants", "Suicides")

# View summary
str(Guerry[, c("dept", "Department", vars_of_interest)])

## ----load-map-----------------------------------------------------------------
# Load the map
data(gfrance85, package = "Guerry")

# Convert to sf object (simple features)
france_sf <- st_as_sf(gfrance85)

# Check structure
head(france_sf[, c("Department", "Region")])

## ----prepare-data-------------------------------------------------------------
# Convert variables to ranks (since they're on different scales)
guerry_ranked <- Guerry
for (var in vars_of_interest) {
  guerry_ranked[[paste0(var, "_rank")]] <- rank(guerry_ranked[[var]], na.last = "keep")
}

# Join with spatial data
france_data <- merge(france_sf, guerry_ranked,
                     by = "Department",
                     all.x = TRUE)

# Check the join
cat("Departments in map:", nrow(france_sf), "\n")
cat("Departments with data:", sum(!is.na(france_data$Crime_pers)), "\n")

## ----map-crime-pers, fig.height=7, fig.width=8--------------------------------
# Map of crimes against persons
p1 <- ggplot(france_data) +
  geom_sf(aes(fill = Crime_pers_rank), color = "black", linewidth = 0.3) +
  scale_fill_cheysson("1895_04", discrete = FALSE,
                      name = "Rank") +
  labs(
    title = "Crimes Against Persons",
    subtitle = "France, 1830s (ranked by department)",
    caption = "Data: André-Michel Guerry (1833)"
  ) +
  theme_cheysson_map() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )

print(p1)

## ----map-crime-prop, fig.height=7, fig.width=8--------------------------------
p2 <- ggplot(france_data) +
  geom_sf(aes(fill = Crime_prop_rank), color = "black", linewidth = 0.3) +
  scale_fill_cheysson("1895_04", discrete = FALSE,
                      name = "Rank") +
  labs(
    title = "Crimes Against Property",
    subtitle = "France, 1830s (ranked by department)",
    caption = "Data: André-Michel Guerry (1833)"
  ) +
  theme_cheysson_map() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )

print(p2)

## ----map-literacy, fig.height=7, fig.width=8----------------------------------
# Create quintiles for discrete display
france_data$Literacy_quint <- cut(france_data$Literacy_rank,
                                  breaks = quantile(france_data$Literacy_rank,
                                                   probs = seq(0, 1, 0.2),
                                                   na.rm = TRUE),
                                  include.lowest = TRUE,
                                  labels = c("Lowest", "Low", "Medium", "High", "Highest"))

p3 <- ggplot(france_data) +
  geom_sf(aes(fill = Literacy_quint), color = "black", linewidth = 0.3) +
  scale_fill_cheysson("1881_04",
                      name = "Literacy\nQuintile",
                      na.value = "grey80") +
  labs(
    title = "Literacy Rates",
    subtitle = "Percent of military conscripts who can read & write (quintiles)",
    caption = "Data: André-Michel Guerry (1833)"
  ) +
  theme_cheysson_map() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "right"
  )

print(p3)

## ----map-literacy-pattern, fig.height=7, fig.width=8--------------------------
# Literacy with patterns - quintessential Cheysson style
p3b <- ggplot(france_data) +
  geom_sf_pattern(
    aes(fill = Literacy_quint,
        pattern_type = Literacy_quint,
        pattern_fill = Literacy_quint),
    pattern = "stripe",
    pattern_density = 0.3,
    pattern_spacing = 0.02,
    color = "black",
    linewidth = 0.4
  ) +
  scale_fill_cheysson_pattern("1881_04", na.value = "grey90") +
  scale_pattern_fill_cheysson("1881_04", na.value = "grey90") +
  scale_pattern_type_cheysson("1881_04") +
  labs(
    title = "Literacy Rates with Cheysson Patterns",
    subtitle = "Combining colors and hatching patterns (quintiles)",
    caption = "Data: André-Michel Guerry (1833)"
  ) +
  theme_cheysson_map() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "right"
  ) +
  guides(
    fill = guide_legend(title = "Literacy\nQuintile"),
    pattern_type = guide_legend(title = "Literacy\nQuintile"),
    pattern_fill = "none"
  )

print(p3b)

## ----map-donations, fig.height=7, fig.width=8---------------------------------
# Create categories
france_data$Donations_cat <- cut(france_data$Donations_rank,
                                 breaks = quantile(france_data$Donations_rank,
                                                  probs = seq(0, 1, 0.25),
                                                  na.rm = TRUE),
                                 include.lowest = TRUE,
                                 labels = c("Low", "Medium-Low", "Medium-High", "High"))

p4 <- ggplot(france_data) +
  geom_sf(aes(fill = Donations_cat), color = "black", linewidth = 0.3) +
  scale_fill_cheysson("1883_04",
                      name = "Donations\nLevel",
                      na.value = "grey80") +
  labs(
    title = "Charitable Donations",
    subtitle = "Donations to the poor (quartiles)",
    caption = "Data: André-Michel Guerry (1833)"
  ) +
  theme_cheysson_map() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )

print(p4)

## ----map-donations-pattern, fig.height=7, fig.width=8-------------------------
# Donations with varied pattern types
p4b <- ggplot(france_data) +
  geom_sf_pattern(
    aes(fill = Donations_cat,
        pattern_type = Donations_cat,
        pattern_fill = Donations_cat),
    pattern = "stripe",
    pattern_density = 0.35,
    pattern_spacing = 0.025,
    color = "black",
    linewidth = 0.4
  ) +
  scale_fill_cheysson_pattern("1883_04", na.value = "grey90") +
  scale_pattern_fill_cheysson("1883_04", na.value = "grey90") +
  scale_pattern_type_cheysson("1883_04") +
  labs(
    title = "Charitable Donations with Hatching Patterns",
    subtitle = "Authentic Cheysson-style patterns and colors (quartiles)",
    caption = "Data: André-Michel Guerry (1833)"
  ) +
  theme_cheysson_map() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "right"
  ) +
  guides(
    fill = guide_legend(title = "Donations\nLevel"),
    pattern_type = guide_legend(title = "Donations\nLevel"),
    pattern_fill = "none"
  )

print(p4b)

## ----map-infants, fig.height=7, fig.width=8-----------------------------------
p5 <- ggplot(france_data) +
  geom_sf(aes(fill = Infants_rank), color = "black", linewidth = 0.3) +
  scale_fill_cheysson("1891_07", discrete = FALSE,
                      name = "Rank") +
  labs(
    title = "Illegitimate Births",
    subtitle = "Population per illegitimate birth (ranked by department)",
    caption = "Data: André-Michel Guerry (1833)"
  ) +
  theme_cheysson_map() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )

print(p5)

## ----map-suicides, fig.height=7, fig.width=8----------------------------------
p6 <- ggplot(france_data) +
  geom_sf(aes(fill = Suicides_rank), color = "black", linewidth = 0.3) +
  scale_fill_cheysson("1887_06", discrete = FALSE,
                      name = "Rank") +
  labs(
    title = "Suicides",
    subtitle = "Annual suicides per population (ranked by department)",
    caption = "Data: André-Michel Guerry (1833)"
  ) +
  theme_cheysson_map() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )

print(p6)

## ----map-faceted, fig.height=8, fig.width=10----------------------------------
# Prepare data in long format for faceting
library(tidyr)
library(dplyr)

crime_long <- france_data |>
  st_as_sf() |>
  select(Department, Crime_pers_rank, Crime_prop_rank,
         Literacy_rank, Suicides_rank) |>
  pivot_longer(cols = ends_with("_rank"),
               names_to = "Variable",
               values_to = "Rank") |>
  mutate(Variable = recode(Variable,
                          "Crime_pers_rank" = "Crimes Against Persons",
                          "Crime_prop_rank" = "Property Crimes",
                          "Literacy_rank" = "Literacy Rate",
                          "Suicides_rank" = "Suicides"))

p7 <- ggplot(crime_long) +
  geom_sf(aes(fill = Rank), color = "grey30", linewidth = 0.2) +
  scale_fill_cheysson("1895_04", discrete = FALSE,
                      name = "Rank") +
  facet_wrap(~ Variable, ncol = 2) +
  labs(
    title = "Social Statistics of France, 1830s",
    subtitle = "Four measures of moral statistics (ranked by department)",
    caption = "Data: André-Michel Guerry (1833)"
  ) +
  theme_cheysson_map() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.background = element_rect(fill = "#edd493", color = "black"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

print(p7)

## ----map-by-region, fig.height=7, fig.width=8---------------------------------
# Map showing regions
# Note: After merge, Region column may be duplicated as Region.x or Region.y
# We'll use the spatial data version (Region.x) or check which exists
region_col <- if("Region" %in% names(france_data)) {
  "Region"
} else if("Region.x" %in% names(france_data)) {
  "Region.x"
} else {
  "Region.y"
}

p8 <- ggplot(france_data) +
  geom_sf(aes(fill = .data[[region_col]]), color = "black", linewidth = 0.4) +
  scale_fill_cheysson("category",
                      name = "Region") +
  labs(
    title = "Regions of France",
    subtitle = "Administrative divisions circa 1830",
    caption = "Source: Guerry package"
  ) +
  theme_cheysson_map() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )

print(p8)

## ----map-regions-pattern, fig.height=7, fig.width=8---------------------------
# Regions with distinctive patterns - very characteristic of Cheysson
p8b <- ggplot(france_data) +
  geom_sf_pattern(
    aes(fill = .data[[region_col]],
        pattern_type = .data[[region_col]],
        pattern_fill = .data[[region_col]]),
    pattern = "stripe",
    pattern_density = 0.3,
    pattern_spacing = 0.02,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_cheysson_pattern("category") +
  scale_pattern_fill_cheysson("category") +
  scale_pattern_type_cheysson("category") +
  labs(
    title = "Regions of France with Cheysson Patterns",
    subtitle = "Distinctive hatching patterns for each region - authentic Albums style",
    caption = "Source: Guerry package"
  ) +
  theme_cheysson_map() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "right"
  ) +
  guides(
    fill = guide_legend(title = "Region"),
    pattern_type = guide_legend(title = "Region"),
    pattern_fill = "none"
  )

print(p8b)

## ----map-bivariate, fig.height=7, fig.width=9---------------------------------
# Create categories for both variables
france_data$Crime_cat <- cut(france_data$Crime_pers_rank,
                             breaks = 3,
                             labels = c("Low", "Medium", "High"))

france_data$Lit_cat <- cut(france_data$Literacy_rank,
                           breaks = 3,
                           labels = c("Low", "Medium", "High"))

# Create bivariate category
france_data$Bivariate <- paste0(france_data$Crime_cat, "\n",
                                france_data$Lit_cat, " Literacy")

# Plot
p9 <- ggplot(france_data) +
  geom_sf(aes(fill = Crime_pers_rank), color = "black", linewidth = 0.5) +
  scale_fill_cheysson("1895_04", discrete = FALSE, name = "Crime\nRank") +
  # Add point symbols sized by literacy
  geom_sf_text(aes(label = ifelse(Literacy_rank > 70, "H",
                                  ifelse(Literacy_rank < 25, "L", ""))),
               size = 3, fontface = "bold") +
  labs(
    title = "Crime vs. Literacy",
    subtitle = "Crime Against Persons (color) and Literacy (H=High, L=Low)",
    caption = "Data: André-Michel Guerry (1833)"
  ) +
  theme_cheysson_map() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

print(p9)

## ----show-palettes------------------------------------------------------------
# Sequential palettes (good for continuous rankings)
list_cheysson_pals("sequential")

# Grouped palettes (good for categories)
list_cheysson_pals("grouped")

# Category palettes (good for discrete regions)
list_cheysson_pals("category")

## ----cleanup, include=FALSE---------------------------------------------------
# Clean up
if (exists("fonts_available") && fonts_available) {
  showtext::showtext_auto(FALSE)
}

