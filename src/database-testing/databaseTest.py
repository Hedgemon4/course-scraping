import pandas, sqlite3

connection = sqlite3.connect("courseScraping.db")

cursor = connection.cursor()

# df = pandas.read_csv("C:/Users/spenc/PycharmProjects/course-scraping/src/web-scraping-testing/compsci_courses.csv")

# df.to_sql("Computer Science Courses", connection)
