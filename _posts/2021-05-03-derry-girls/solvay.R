library(data.table)
library(jsonlite)
library(httr)
library(tidyverse)
library(showtext)

font_add_google("Amiri", "Amiri")

showtext_auto()

myauth <- readRDS("../myauth_faceplusplus")

mypaths <- c("images/far-left.png", 
             "images/herriot-herzen.png",
             "images/ehrenfest.png", 
             "images/dedonder-schroddinger.png",
             "images/versh-pauli.png",
             "images/heisenberg-brill.png",
             "images/wilson-bohr.png")

mypaths <- "images/friends.jpg"

face_plus_plus <- function(fullpath) {
  face <- httr::RETRY("POST", "https://api-us.faceplusplus.com/facepp/v3/detect",
                      body = list(api_key  = myauth$api_key,
                                  api_secret = myauth$api_secret,
                                  image_file = upload_file(fullpath),
                                  return_landmark = 0,
                                  return_attributes = "emotion,gender"),
                      times = 2, 
                      encode = "multipart") %>% 
    as.character
  
  anger <- fromJSON(face)$faces$attributes$emotion$anger
  disgust <- fromJSON(face)$faces$attributes$emotion$disgust
  fear <- fromJSON(face)$faces$attributes$emotion$fear
  happiness <- fromJSON(face)$faces$attributes$emotion$happiness
  neutral <- fromJSON(face)$faces$attributes$emotion$neutral
  sadness <- fromJSON(face)$faces$attributes$emotion$sadness
  surprise <- fromJSON(face)$faces$attributes$emotion$surprise
  gender <- fromJSON(face)$faces$attributes$gender
  top <- fromJSON(face)$faces$face_rectangle$top
  left <- fromJSON(face)$faces$face_rectangle$left
  tibble(anger, disgust, fear, happiness, neutral,
         sadness, surprise, top, left, 
         gender = gender$value, image = fullpath)
}

mypaths <- "images/derry-girls3.jpg"
derry_girls <- magick::image_read(mypaths)
image_dims <- magick::image_info(derry_girls)
width <- image_dims$width[1]
height <- image_dims$height[1]
centre <- c(width/2, height/2)

solvay <- map_df(mypaths, face_plus_plus) %>% 
  arrange(left) %>% 
  mutate(name = c("Michelle", "James", "Erin", "Orla", "Claire"),
         x = left,
         y = height - top) %>% 
  select(-c(image, top, left))

z <- solvay %>% 
  select(-c(gender, x, y)) %>% 
  pivot_longer(cols = -c(name), 
               names_to = "emotion", 
               values_to = "percentage") %>% 
  dplyr::filter(percentage > 10) %>% 
  mutate(percentage = round(percentage, 1)) %>% 
  unite("emotion", emotion:percentage, sep = ": ") %>% 
  mutate(emotion = glue::glue("{emotion}%")) %>% 
  group_by(name) %>% 
  summarise(emotion = paste(emotion, collapse = "<br>")) %>% 
  ungroup() %>% 
  mutate(name1 = glue::glue("<b>{name}</b>")) %>% 
  unite("emotion", name1:emotion, sep = "<br>") %>% 
  mutate(emotion = glue::glue("<p style = 'color:#004400; font-family:Amiri';>{emotion}</p>"))

solvay %>% 
  left_join(z) %>% 
  ggplot(aes(x, y)) +
  coord_cartesian(xlim = c(0, width), 
                  ylim = c(0, height)) +
  background_image(derry_girls) +
  ggtext::geom_richtext(aes(x = x + sign(x-centre[1])*50 + 50, 
                            y = y + sign(y-centre[2])*70,
                            label = emotion)) +
  theme_void()


names <- c("A. Piccard", 
           "E. Henriot", 
           "P. Ehrenfest", 
           "E. Herzen",
           "Th. de Donder",
           "E. Schrödinger",
           "J. E. Verschaffelt",
           "W. Pauli",
           "W. Heisenberg",
           "R. H. Fowler",
           "L. Brillouin","P. Debye",
           "M. Knudsen",
           "W.L. Bragg",
           "H. A. Kramers",
           "P. A. M. Dirac",
           "A. H. Compton",
           "L. de Broglie",
           "M. Born",
           "N. Bohr",
           "I. Langmuir",
           "M. Planck",
           "M. Curie",
           "H.A . Lorentz",
           "A. Einstein",
           "P. Langevin",
           "Ch.-E. Guye",
           "C. T. R. Wilson",
           "O. W. Richardson"
)
