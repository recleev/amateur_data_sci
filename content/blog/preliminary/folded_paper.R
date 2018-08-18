
# Load Library ------------------------------------------------------------

library(tidyverse)

# Create Domain -----------------------------------------------------------

domain <- 
  # the assumption is the thickness of the paper is one unit
  tibble(width = seq(0.50001, 1, by = 0.00001))

# Find Minimum Area -------------------------------------------------------

domain %>% 
  mutate(
    area = (width ** 2) * sqrt(2 * width - 1) /
      (4 * width - 2)
  ) %>% 
  filter(area == min(area))

# Find Minimum Crease Length ----------------------------------------------

domain %>% 
  mutate(
    crease_length = sqrt(
      (2 * (width ** 3)) / (2 * width - 1)
    )
  ) %>% 
  filter(crease_length == min(crease_length))

# Create Illustration of Folded Paper -------------------------------------

paper_width <- 40

paper_height <- 60

fold <- 25

ggplot() +
  aes(
    c(0, fold, paper_width, paper_width),
    c(0, paper_height, paper_height, 0)
  ) +
  geom_polygon(
    fill = "palegreen"
  ) +
  geom_polygon(
    aes(
      c(0, fold, paper_width),
      c(0, paper_height, paper_height - sqrt(fold**2 - (paper_width - fold)**2))
    ),
    fill = "seagreen"
  ) +
  geom_polygon(
    aes(
      c(0, 0, fold),
      c(0, paper_height, paper_height)
    ),
    linetype = 2,
    fill = "white",
    size = 2,
    color = "forestgreen"
  ) +
  annotate(
    "text", 
    x = -1, 
    y = 0, 
    label = "C", 
    color = "darkred", 
    size = 6
  ) +
  annotate(
    "text", 
    x = fold + 1.5, 
    y = paper_height + 0.75, 
    label = "A", 
    color = "darkred", 
    size = 6
  ) +
  annotate(
    "text", 
    x = paper_width + 0.75, 
    y = paper_height - sqrt(fold**2 - (paper_width - fold)**2), 
    label = "B", 
    color = "darkred", 
    size = 6
  ) +
  annotate(
    "segment",
    x = 0, 
    y = paper_height + 2,
    xend = fold,
    yend = paper_height + 2,
    color = "orange", 
    size = 2
  ) +
  annotate(
    "text",
    x = fold / 2,
    y = paper_height + 4,
    label = "x"
  ) +
  annotate(
    "segment",
    x = fold, 
    y = paper_height + 2,
    xend = paper_width,
    yend = paper_height + 2,
    color = "yellow", 
    size = 2
  ) +
  annotate(
    "text",
    x = fold + (paper_width - fold) / 2, 
    y = paper_height + 4,
    label = "a - x"
  ) +
  annotate(
    "segment",
    x = 0, 
    y = -2,
    xend = paper_width,
    yend = -2,
    color = "darkblue", 
    size = 2
  ) +
  annotate(
    "text",
    x = paper_width / 2, 
    y = -4,
    label = "a"
  ) +
  theme_void() +
  coord_fixed() +
  labs(title = "A Folded Paper")

# Sine and Cosine Interesect at Right Angles ------------------------------

factor = 1 / sin(pi / 4)

tibble(
  theta = seq(0, 1, by = 0.05) * pi
) %>% 
  mutate(
    sin_y = sin(factor * theta),
    cos_y = cos(factor * theta)
  ) %>% 
  ggplot(
    aes(
      theta,
      sin_y
    )
  ) +
  geom_line(color = "red3") +
  geom_line(
    aes(
      y = cos_y
    ),
    color = "forestgreen"
  ) +
  coord_fixed() +
  ggthemes::theme_pander(gl = 0) +
  labs(
    title = "Intersecting Sine and Cosine Functions",
    x = "x",
    y = "y"
  )
  
