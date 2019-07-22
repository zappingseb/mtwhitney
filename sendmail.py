# Import smtplib for the actual sending function
import smtplib

# Import the email modules we'll need
from email.mime.text import MIMEText

# Open a plain text file for reading.  For this example, assume that
# the text file contains only ASCII characters.
with open("output.txt", 'rb') as fp:
    # Create a text/plain message
    msg = MIMEText(fp.read())

# me == the sender's email address
# you == the recipient's email address
msg['Subject'] = 'mtWhitney'
msg['From'] = "info@mail-wolf.de"
msg['To'] = "zappingseb@googlemail.com"

# Send the message via our own SMTP server, but don't include the
# envelope header.
s = smtplib.SMTP('wp1097223.server-he.de', 587).login("wp1097223-157553", "password")
s.sendmail(me, [you], msg.as_string())
s.quit()
