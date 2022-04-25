

### Creating a dataframe of multiple animals

df1 <- read.csv("Habituation/Post/Cohort1_hab3_1_LocationOutput.csv")
df2 <- read.csv("Habituation/Post/Cohort1_hab3_2_LocationOutput.csv")
df3 <- read.csv("Habituation/Post/Cohort1_hab3_3_LocationOutput.csv")
df4 <- read.csv("Habituation/Post/Cohort1_hab3_4_LocationOutput.csv")
df6 <- read.csv("Habituation/Post/Cohort1_hab3_6_LocationOutput.csv")
df7 <- read.csv("Habituation/Post/Cohort1_hab3_7_LocationOutput.csv")



## Convert each to rescaled x/y data:
  
xy1 <- get_coords_habit(df1)
xy2 <- get_coords_habit(df2)
xy3 <- get_coords_habit(df3)
xy4 <- get_coords_habit(df4)
xy6 <- get_coords_habit(df6)
xy7 <- get_coords_habit(df7)

## Make into one dataframe

xy.df <- 
  rbind(
    xy1$xy %>% select(Frame, x=rescaleX, y=rescaleY) %>% mutate(id=1),
    xy2$xy %>% select(Frame, x=rescaleX, y=rescaleY) %>% mutate(id=2),
    xy3$xy %>% select(Frame, x=rescaleX, y=rescaleY) %>% mutate(id=3),
    xy4$xy %>% select(Frame, x=rescaleX, y=rescaleY) %>% mutate(id=4),
    xy6$xy %>% select(Frame, x=rescaleX, y=rescaleY) %>% mutate(id=6),
    xy7$xy %>% select(Frame, x=rescaleX, y=rescaleY) %>% mutate(id=7)
  )
head(xy.df)
tail(xy.df)



## Plot all animals, whole time

xy.df %>% 
  ggplot(aes(x, y, color = factor(id))) +
     geom_point(size=0.5) +
     geom_segment(aes(
                    xend=c(tail(x, n=-1), NA), 
                    yend=c(tail(y, n=-1), NA)
                  )
      ) +
  theme_void()



### Animate tracks.


## Silly mouse image one:

# Just do for a subset of frames.
xy.sub <- xy.df %>% filter(Frame > 1000, Frame <= 1200)


head(bats)


library(rsvg)

mice_image_link <- 
  "https://png2.cleanpng.com/sh/04841ce7d7c3b75fa23e63d407a18a38/L0KzQYm3VMExN5R6iZH0aYP2gLBuTfNwdaF6jNd7LX3yhcTsTgBwcZ95feQ2Y3BwgMb7hgIucZR0huU2aXPyfn7rhgNqb58yitN9LX3yhcTsTcVia5RofqdsOUK2doqBTsc2PGk1SqM8MUW1Q4SAUcg3QGYAUaM3cH7q/kisspng-computer-mouse-pointer-computer-icons-icon-design-rat-mouse-5acccf5c923f98.7548021315233718685991.png"

mice_colors <- c("darkorange", "orangered", "violetred", "purple", "black", "darkseagreen")

xy.sub$id <- factor(xy.sub$id)

xy.sub %>%
  mutate(image = mice_image_link) %>%
  ggplot(aes(x = x, y = y, group = id, color = id)) +
  geom_path() +
  geom_image(aes(image = image), size = 0.1) +
  scale_color_manual(values = mice_colors) +
  transition_reveal(along = Frame)


## Fading lines
# add box

# can't get line to fade nicely:
xy.sub %>%
  ggplot(aes(x = x, y = y, group = id, color = id)) +
  geom_path() +
  #geom_point()+
  scale_color_manual(values = mice_colors) +
  transition_reveal(along = Frame) + 
  shadow_wake(0.05, size = 2, alpha = TRUE, wrap = FALSE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') +
  theme_void()


# this doesn't work either

library(transformr)

xy.sub %>%
  ggplot(aes(x = x, y = y, group = id, color = id)) +
  geom_path() +
  geom_point()+
  scale_color_manual(values = mice_colors) +
  transition_reveal(along = Frame) +
  shadow_mark(size = 0.75) +
  theme_void()


### also doesn't work.

xy.sub %>%
  ggplot(aes(x = x, y = y, group = id, color = id)) +
  geom_path() +
  geom_point()+
  scale_color_manual(values = mice_colors) +
  transition_reveal(along = Frame) +
  shadow_trail(max_frames=10) +
  theme_void()



### Trying with geom segment

xy.sub <- xy.sub %>%
  group_by(id) %>%
  mutate(next_x = lead(x),
         next_y = lead(y))















### Example dataset:
set.seed(27)
df <- data.frame(Frame = rep(1:10, 3),
                 id = factor(rep(1:3, each = 10)),
                 x = runif(30),
                 y = runif(30))

df

df %>%
  ggplot(aes(x = x, y = y, group = id, color = id)) +
  geom_path() +
  geom_point()+
  scale_color_manual(values=c("red","blue","green")) +
  transition_reveal(along = Frame) +
  shadow_mark(size = 0.75) +
  theme_void()

