
# Using website https://www.r-bloggers.com/2019/01/extracting-colours-from-your-images-with-image-quantization/ 

## Packages
library(tidyverse)
library(magick) 
library(scales)
library(imager) 
library(cowplot)
library(waffle)
library(patchwork)
library(showtext)

im <- image_read("C:\\Users\\simon\\Downloads\\Day10.jpg")


font_add_google("Montserrat", "Montserrat")
font_add_google("Sacramento", "Sacramento")
font_add_google("Catamaran", "Catamaran")
showtext_auto()

## Reduce the colour used in image with image_quantize.  For example, let's say I want to reduce to 24 colours.
im %>%
  image_resize("500") %>%
  image_quantize(max=24)

## Function to get n number of colours out of your image.
get_colorPal <- function(im, n=15, cs="RGB"){
  # resize image to 100px 
  tmp <-im %>% image_resize("100") %>% 
    # set max colours to 8, and colorspace to RGB
    image_quantize(max=n, colorspace=cs) %>%  ## reducing colours! different colorspace gives you different result
    # convert to use as.data.frame
    magick2cimg() %>%  ## I'm converting, becauase I want to use as.data.frame function in imager package.
    # sort by colour hue 
    RGBtoHSV() %>% ## i like sorting colour by hue rather than RGB (red green blue)
    # made dataframe wide by colour
    as.data.frame(wide="c") %>%  #3 making it wide makes it easier to output hex colour
    # rescale colours
    mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           value = c.3) %>%
    count(hex, hue, sat,value, sort=T) %>% 
    mutate(colorspace = cs)
  
  return(tmp %>% select(colorspace,hex,hue,sat,value,n)) ## I want data frame as a result.
  
}

# Return 50 of the colours in the image
colors <- get_colorPal(im, n=50)

#  Plot the colours found (https://github.com/linda-bennett/30-day-chart-challenge/blob/main/Distributions/Day%2010%20-%20Abstract/day10-abstract.R)
plot <-colors %>%  
  group_by(colorspace) %>%
  mutate(ypos=row_number(hue)) %>%  ## alter stacking order
  ggplot(aes(x=ypos, y=colorspace, fill=hex)) +
  #annotate("text", aes(x=colorspace, y=ypos, label=hex)) +
  geom_tile() +
  scale_fill_identity() +
  scale_x_continuous(breaks=NULL) +
  theme_void() +
  expand_limits(y=-1) 

# Show image 
plot_image <- ggplot() +
  draw_image("C:\\Users\\simon\\Downloads\\Day10.jpg") +
  theme_void()

# Display plots with the image 
p <- plot_image / plot +
  plot_annotation(
    title = "Colours from Sunset Image", caption= "30daychartchallenge-Day 10 |",
    theme = theme(plot.title = element_text(size = 40,
                                            hjust=0.5,
                                            vjust = 0,
                                            family="Montserrat"),
                  plot.caption=element_text(size=20, family="Montserrat", hjust=0.5),
                  plot.background = element_rect(fill = "#E5E5E3", colour = 'black', size = 3)) 
  )

# Output Image 
ggsave("C:\\Users\\simon\\Downloads\\day10.png", p, dpi=300)



# Day 11- Hurricanes
storms <- dplyr::storms

# Remove any records without hu_diameter
storms <- storms[!is.na(storms$hu_diameter),]

# Group by name and keep only the maximum hu_diameter 
storms<- storms %>% group_by(name) %>% mutate("max"= max(hu_diameter)) %>% ungroup()

# Keep only one record for each storm 
storms <- storms[!duplicated(storms$name),]

# Remove records with max of 0 
storms <- storms[storms$max!=0,]

# Add column to label only top 10 hurricanes 
storms$label_max <- NA
storms$label_max <- ifelse(storms$max>150,storms$name,NA)
# Proportional symbol plot over space
p<- ggplot(storms, aes(x=long, y=lat, size=max, color=status)) + geom_point(alpha=0.6)  +  scale_color_viridis_d()  + theme(plot.background=element_rect(fill="white", color="#E5E5E3"),panel.background=element_rect(fill="#E5E5E3", color="#E5E5E3"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), plot.caption = element_text(family="Montserrat", face="italic", size=20), plot.title=element_text(family="Montserrat", size=30), axis.text=element_text(family="Monserrat", size=30), axis.title=element_text(size=20),legend.position="none") + 
  labs(title="Hurricane Location", caption="#30daychartchallenge | Source: dplyr::storms") + xlab("Longitude")  + ylab("Latitude") +  scale_size_continuous(range = c(2, 12), breaks=pretty_breaks(7)) +
  geom_text(aes(label=label_max),hjust=0.5, vjust=-0.5, size=6, color="white", alpha=0.3)

p

# Distribution plot
p1<- ggplot(storms, aes(x=max, y=status, fill=status)) + ggridges::geom_density_ridges() + scale_fill_viridis_d() + theme(plot.background=element_rect(fill="white", color="#E5E5E3"),panel.background=element_rect(fill="#E5E5E3", color="#E5E5E3"), panel.grid.major = element_blank(), panel.grid.minor=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank(), plot.caption = element_text(family="Montserrat", face="italic", size=20), plot.title=element_text(family="Montserrat", size=30), axis.text=element_text(family="Monserrat", size=30), axis.title=element_text(size=20), legend.text=element_text(size=20), legend.position="bottom") + 
  labs(title="Distribution of Maximum Diameter of Area Experiencing Hurricane Winds (64 knots+) from Hurricanes", caption="#30daychartchallenge | Source: dplyr::storms", fill="") + xlab("Maximum Diameter of Area")

p1

# Plot together 
plot_grid(p, p1, align="h")

ggsave("C:\\Users\\simon\\Downloads\\day11.png", width=20, height=10, dpi=300)


