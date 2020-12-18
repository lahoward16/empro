library(tidyverse)

data <- tibble(x= 1:100, y= 1:100)

data %>%
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
  theme_linedraw() ->
  p

# TOP BOX:
p +
  geom_rect(xmin = 36, xmax=64, ymin=74, ymax=80, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=77,label= "Total Patients (n=1430) \nTotal Treatments (n=2433)", size=3.5) ->
  p


## Exclusion / Inclusion Boxes
p +
  geom_rect(xmin = 32, xmax=68, ymin=53, ymax=63, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=58,label= "Include all mCRPC \n(n=503)", size=3.5) +
  geom_rect(xmin = 70, xmax=97, ymin=60, ymax=78, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=69,label= "Exclude all mHSPC
 (n=825)
 AND All Missing
 (n=3)
 AND nonmetastatic All CRPC
 (n=99)", size=3.5) ->
  p


## Arrows between inclusion / exclusion
p +
  geom_segment(
    x=50, xend=50, y=74, yend=63.3,
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
    geom_segment(
    x=50, xend=69.7, y=69, yend=69,
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
  p


## Let's detail the number of adt, steroid, missing, and non-adt(steroid))
# p +
#   geom_segment(
#   #middle arrow first
#     x=50, xend=50, y=53, yend=45,
#     size=0.15, linejoin = "mitre", lineend = "butt",
#     arrow = arrow(length = unit(1, "mm"), type= "closed")) +
#   #then leftmost arrow, x and xend=10
#   geom_segment(
#     x=10, xend=10, y=45, yend=38.3,
#     size=0.15, linejoin = "mitre", lineend = "butt",
#     arrow = arrow(length = unit(1, "mm"), type= "closed")) +
#   # then 2nd arrow from the left, x and xend=30
#   geom_segment(
#     x=35, xend=35, y=45, yend=38.3,
#     size=0.15, linejoin = "mitre", lineend = "butt",
#     arrow = arrow(length = unit(1, "mm"), type= "closed")) +
#   # then 4th arrow from the left, x and xend=70
#   geom_segment(
#     x=65, xend=65, y=45, yend=38.3,
#     size=0.15, linejoin = "mitre", lineend = "butt",
#     arrow = arrow(length = unit(1, "mm"), type= "closed")) +
# # then rightmost arrow, x and xend=90
#   geom_segment(
#     x=90, xend=90, y=45, yend=38.3,
#     size=0.15, linejoin = "mitre", lineend = "butt",
#     arrow = arrow(length = unit(1, "mm"), type= "closed")) +
#   # then horizontal line, but remove the arrow
#   geom_segment(
#     x=10, xend=90, y=45, yend=45,
#     size=0.15, linejoin = "mitre", lineend = "butt")->
#   p

# Boxes for subset
# p +
#   #first box on left
#   geom_rect(xmin = 1, xmax=19, ymin=29, ymax=38,
#               color='black', fill='white', size=0.25) +
#   annotate('text', x= 10, y=34, size=3.5,
#              label= consort_numbers[4,]$Label) +
#   #2nd box on left
#   geom_rect(xmin = 26, xmax=44, ymin=29, ymax=38,
#               color='black', fill='white', size=0.25) +
#   annotate('text', x= 35, y=34, size=3.5,
#              label= consort_numbers[5,]$Label) +
#3rd box on left
  # geom_rect(xmin = 41, xmax=59, ymin=49, ymax=58,
  #             color='black', fill='white', size=0.25) +
  # annotate('text', x= 50, y=54, size=3.5,
  #            label= '49 Patients assigned\nto UPA 15 mg QD') +
#4th box on left
  # geom_rect(xmin = 56, xmax=74, ymin=29, ymax=38,
  #             color='black', fill='white', size=0.25) +
  # annotate('text', x= 65, y=34, size=3.5,
  #            label= consort_numbers[6,]$Label) +
#5th box on left
  # geom_rect(xmin = 81, xmax=99, ymin=29, ymax=38,
  #             color='black', fill='white', size=0.25) +
  # annotate('text', x= 90, y=34, size=3.5,
  #            label= consort_numbers[7,]$Label)->
  # p


# p +
#   geom_segment(
#     x=35, xend=35, y=29, yend=21.3,
#     size=0.15, linejoin = "mitre", lineend = "butt",
#     arrow = arrow(length = unit(1, "mm"), type= "closed")) +
#   geom_rect(xmin = 26, xmax=44, ymin=12, ymax=21,
#               color='black', fill='white', size=0.25) +
#   annotate('text', x= 35, y=16, size=3.5,
#              label= consort_numbers[8,]$Label)  ->
#   p