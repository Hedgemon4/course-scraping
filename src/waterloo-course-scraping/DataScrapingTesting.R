# File for testing

test1 <- get_text_css("http://ugradcalendar.uwaterloo.ca/courses/CS/136", "body > main > center > div > div > strong :contains(\"CS 136\")")

test2 <- read_html("http://ugradcalendar.uwaterloo.ca/courses/CS/136") %>% html_text() %>% str_squish()
