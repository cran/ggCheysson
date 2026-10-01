## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5,
  fig.align = "center",
  out.width = "75%",
  # ragg, not the default png(): on Intel macOS the Quartz png() device
  # segfaults drawing ggpattern's grid masks at >= 96 dpi (CRAN check ERROR on 1.0.1)
  dev = if (requireNamespace("ragg", quietly = TRUE)) "ragg_png" else "png",
  # draw showtext fonts at the device's real dpi (pkgdown renders retina at 2x)
  fig.showtext = TRUE,
  warning = FALSE,
  message = FALSE,
  # everything here needs ggpattern (in Suggests)
  eval = requireNamespace("ggpattern", quietly = TRUE)
)

## ----load-packages------------------------------------------------------------
library(ggCheysson)
library(ggplot2)
library(ggpattern)

## ----load-fonts, include=FALSE------------------------------------------------
if (requireNamespace("showtext", quietly = TRUE) &&
    requireNamespace("sysfonts", quietly = TRUE)) {
  load_cheysson_fonts(method = "showtext")
  showtext::showtext_auto()
}

## ----bundle-------------------------------------------------------------------
pats <- cheysson_pattern("1886_28")
data.frame(
  pattern       = cheysson_pattern_params(pats, "pattern_type"),
  fill          = cheysson_pattern_params(pats, "fill"),
  pattern_fill  = cheysson_pattern_params(pats, "pattern_fill"),
  pattern_fill2 = cheysson_pattern_params(pats, "pattern_fill2"),
  pattern_angle = cheysson_pattern_params(pats, "pattern_angle")
)

## ----long-form----------------------------------------------------------------
trade <- data.frame(
  country = c("France", "England", "Germany", "Italy"),
  exports = c(2350, 3120, 2680, 1890)
)

ggplot(trade, aes(country, exports)) +
  geom_col_pattern(
    aes(fill = country, pattern = country, pattern_fill = country,
        pattern_fill2 = country, pattern_angle = country),
    pattern_colour = NA, pattern_density = 0.3, pattern_spacing = 0.025,
    colour = "black"
  ) +
  scale_fill_cheysson_pattern("1886_28") +
  scale_pattern_type_cheysson("1886_28") +
  scale_pattern_fill_cheysson("1886_28") +
  scale_pattern_fill2_cheysson("1886_28") +
  scale_pattern_angle_cheysson("1886_28") +
  labs(title = "Exports by Nation, 1885", x = NULL, y = "Thousands of francs") +
  theme_cheysson() +
  theme(legend.position = "none")

## ----short-form---------------------------------------------------------------
ggplot(trade, aes(country, exports)) +
  geom_col_pattern(
    aes_cheysson(country),
    pattern_colour = NA, pattern_density = 0.3, pattern_spacing = 0.025,
    colour = "black"
  ) +
  scale_cheysson("1886_28") +
  labs(title = "Exports by Nation, 1885", x = NULL, y = "Thousands of francs") +
  theme_cheysson() +
  theme(legend.position = "none")

## ----partial-mapping----------------------------------------------------------
ggplot(trade, aes(country, exports, fill = country, pattern = country)) +
  geom_col_pattern(pattern_colour = NA, pattern_density = 0.3, pattern_spacing = 0.025,
                   colour = "black") +
  scale_cheysson("1886_28") +
  labs(title = "Only fill and pattern mapped", x = NULL, y = "Thousands of francs") +
  theme_cheysson() +
  theme(legend.position = "none")

## ----legend, fig.width=8------------------------------------------------------
infrastructure <- data.frame(
  region = rep(c("North", "South", "East", "West"), each = 3),
  type   = factor(rep(c("Road", "Canal", "Rail"), 4), levels = c("Road", "Canal", "Rail")),
  length = c(250, 300, 450,  250, 400, 350,  300, 200, 500,  250, 350, 400)
)

ggplot(infrastructure, aes(region, length)) +
  geom_col_pattern(
    aes_cheysson(type),
    position = "dodge",
    pattern_colour = NA, pattern_density = 0.35, pattern_spacing = 0.02,
    colour = "black"
  ) +
  scale_cheysson("1881_12", name = "Network") +
  labs(title = "Transportation Networks by Region", x = NULL, y = "Hundreds of km") +
  theme_cheysson()

## ----diverging----------------------------------------------------------------
opinion <- data.frame(
  response = factor(c("Strongly against", "Against", "For", "Strongly for"),
                    levels = c("Strongly against", "Against", "For", "Strongly for")),
  percent = c(18, 27, 34, 21)
)

ggplot(opinion, aes(response, percent)) +
  geom_col_pattern(
    aes_cheysson(response),
    pattern_colour = NA, pattern_density = 0.35, pattern_spacing = 0.025,
    colour = "black"
  ) +
  scale_cheysson("1883_31") +
  labs(title = "A Diverging Palette", x = NULL, y = "Percent") +
  theme_cheysson() +
  theme(legend.position = "none")

## ----missing------------------------------------------------------------------
trade_na <- rbind(trade, data.frame(country = NA, exports = 1500))

ggplot(trade_na, aes(country, exports)) +
  geom_col_pattern(
    aes_cheysson(country),
    pattern_colour = NA, pattern_density = 0.3, pattern_spacing = 0.025,
    colour = "black"
  ) +
  scale_cheysson("1886_28", na.value = "grey90") +
  labs(title = "Exports by Nation, 1885", x = NULL, y = "Thousands of francs") +
  theme_cheysson() +
  theme(legend.position = "none")

## ----subset-------------------------------------------------------------------
ggplot(trade, aes(country, exports, fill = country, pattern = country,
                  pattern_fill = country)) +
  geom_col_pattern(pattern_colour = NA, pattern_density = 0.3, pattern_spacing = 0.025,
                   colour = "black") +
  scale_cheysson("1886_28", aesthetics = c("fill", "pattern", "pattern_fill")) +
  labs(title = "Without the palette's angles", x = NULL, y = "Thousands of francs") +
  theme_cheysson() +
  theme(legend.position = "none")

