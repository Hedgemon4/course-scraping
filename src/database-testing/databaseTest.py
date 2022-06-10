import pandas, sqlite3, csv

connection = sqlite3.connect(":memory:")

cursor = connection.cursor()

cursor.execute("""
        CREATE TABLE waterloo_courses (
            coruse_id    INTEGER PRIMARY KEY,
            course_code  text NOT NULL,
            course_name  text NOT NULL, 
            course_description text NOT NULL
        )""")

file1 = open("C:/Users/spenc/PycharmProjects/course-scraping/src/csv-files/compsci_courses_waterloo.csv")
file2 = open("C:/Users/spenc/PycharmProjects/course-scraping/src/csv-files/math_courses_waterloo.csv")
file3 = open("C:/Users/spenc/PycharmProjects/course-scraping/src/csv-files/stat_courses_waterloo.csv")
compsciCSV = csv.reader(file1, delimiter=",")
mathCSV = csv.reader(file2, delimiter=",")
statCSV = csv.reader(file3, delimiter=",")
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

cursor.execute("SELECT * FROM waterloo_courses WHERE course_description LIKE '%algorithm%'")

print(cursor.fetchall())

connection.close()

