library(magick)
library(magrittr)

# read the image
meme <- image_read("cat_meme.png") %>%
  image_resize("700x700")

# add top words
meme <- meme %>%
  image_annotate(
    text = "WHEN I OPEN TIKTOK\nBEFORE STUDYING",
    size = 60,
    color = "white",
    strokecolor = "black",
    strokewidth = 2.5,
    gravity = "north",
    location = "+0+20"
  )

# add bottom words
meme <- meme %>%
  image_annotate(
    text = "AND MY WHOLE EVENING\nIS GONE",
    size = 54,
    color = "white",
    strokecolor = "black",
    strokewidth = 2.5,
    gravity = "south",
    location = "+0+20"
  )

# save the cat meme
meme %>%
  image_write("my_meme.png")


# frame 1
frame1 <- image_read("cat_meme.png") %>%
  image_resize("700x700") %>%
  image_annotate(
    text = "JUST 5 MINUTES\nON TIKTOK",
    size = 60,
    color = "white",
    strokecolor = "black",
    strokewidth = 2,
    gravity = "north",
    location = "+0+20"
  ) %>%
  image_annotate(
    text = "NO BIG DEAL",
    size = 58,
    color = "white",
    strokecolor = "black",
    strokewidth = 2,
    gravity = "south",
    location = "+0+20"
  )

# frame 2
frame2 <- image_read("cat_meme.png") %>%
  image_resize("700x700") %>%
  image_annotate(
    text = "OKAY... \nMAYBE 20 MINUTES",
    size = 60,
    color = "white",
    strokecolor = "black",
    strokewidth = 2,
    gravity = "north",
    location = "+0+20"
  ) %>%
  image_annotate(
    text = "STILL FINE",
    size = 60,
    color = "white",
    strokecolor = "black",
    strokewidth = 2,
    gravity = "south",
    location = "+0+20"
  )

# frame 3
frame3 <- image_read("cat_meme.png") %>%
  image_resize("700x700") %>%
  image_annotate(
    text = "WAIT, WHY IS IT\nDARK OUTSIDE",
    size = 60,
    color = "white",
    strokecolor = "black",
    strokewidth = 2,
    gravity = "north",
    location = "+0+20"
  ) %>%
  image_annotate(
    text = "WHAT HAPPENED",
    size = 60,
    color = "white",
    strokecolor = "black",
    strokewidth = 2,
    gravity = "south",
    location = "+0+20"
  )

# frame 4
frame4 <- image_read("cat_meme.png") %>%
  image_resize("700x700") %>%
  image_annotate(
    text = "HOW IS IT\nALREADY MIDNIGHT",
    size = 60,
    color = "white",
    strokecolor = "black",
    strokewidth = 2,
    gravity = "north",
    location = "+0+20"
  ) %>%
  image_annotate(
    text = "WELL, I WAS JUST\nTAKING A BREAK!",
    size = 60,
    color = "white",
    strokecolor = "black",
    strokewidth = 2,
    gravity = "south",
    location = "+0+20"
  )

# combine the frames
animation <- c(frame1, frame2, frame3, frame4) %>%
  image_animate(fps = 1)

# save the animated meme
animation %>%
  image_write("my_animated_meme.gif")

