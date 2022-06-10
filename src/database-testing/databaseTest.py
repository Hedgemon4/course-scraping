import csv
import sqlite3

connection = sqlite3.connect(":memory:")

cursor = connection.cursor()

cursor.execute("""
        CREATE TABLE waterloo_courses (
            course_id    INTEGER PRIMARY KEY UNIQUE,
            course_code  text NOT NULL,
            course_name  text NOT NULL, 
            course_description text NOT NULL
        )""")

# Get CSV files
file1 = open("C:/Users/spenc/PycharmProjects/course-scraping/src/csv-files/compsci_courses_waterloo.csv")
file2 = open("C:/Users/spenc/PycharmProjects/course-scraping/src/csv-files/math_courses_waterloo.csv")
file3 = open("C:/Users/spenc/PycharmProjects/course-scraping/src/csv-files/stat_courses_waterloo.csv")

compsciCSV = csv.reader(file1, delimiter=",")
mathCSV = csv.reader(file2, delimiter=",")
statCSV = csv.reader(file3, delimiter=",")

# Put CSV Files into waterloo_courses database
for row in compsciCSV:
    if row[1] == 'Course Code':
        continue
    else:
        cursor.execute("INSERT INTO waterloo_courses VALUES(NULL,?,?,?)", (row[1], row[2], row[3],))

for row in mathCSV:
    if row[1] == 'Course Code':
        continue
    else:
        cursor.execute("INSERT INTO waterloo_courses VALUES(NULL,?,?,?)", (row[1], row[2], row[3],))

for row in statCSV:
    if row[1] == 'Course Code':
        continue
    else:
        cursor.execute("INSERT INTO waterloo_courses VALUES(NULL,?,?,?)", (row[1], row[2], row[3],))

connection.commit()

# Print Files from waterloo_courses database
cursor.execute("SELECT * FROM waterloo_courses WHERE course_id = 8")

print(cursor.fetchall())

# Make database for waterloo_datascience_major_requirements

cursor.execute("""
        CREATE TABLE waterloo_datascience_major_requirements (
            course_id_requirements INTEGER PRIMARY KEY, 
            course_code TEXT NOT NULL,
            course_NAME TEXT NOT NULL,
            course_id INTEGER,
            FOREIGN KEY(course_id) REFERENCES waterloo_courses(course_id)
        )
        """)

file4 = open("C:/Users/spenc/PycharmProjects/course-scraping/src/csv-files/courses_waterloo.csv")
coursesCSV = csv.reader(file4, delimiter=',')

for row in coursesCSV:
    cursor.execute("SELECT course_id FROM waterloo_courses WHERE course_code = ? AND course_name = ? ", (row[1], row[2]))
    i = str(cursor.fetchone())
    i = i.removeprefix("(").removesuffix(",)")
    cursor.execute("INSERT INTO waterloo_datascience_major_requirements VALUES (NULL, ?, ?, ?)", (row[1], row[2], i))

connection.commit()

cursor.execute("SELECT * FROM waterloo_datascience_major_requirements")

print(cursor.fetchall())

connection.close()
