
#!/usr/bin/python

__author__ = 'alexf4'
import sys
import datetime
import subprocess
import os


#This is the entry point into the python program
def main():


    #check to see if temp file exists
    if os.path.exists("running") is False:
        tempfile = open("running", "wb")

    #This will be an infinite loop
    #while(True):


        #run command the command line. This would be the R commands you need to give out
        subprocess.call("echo Hello World", shell=True)

        #simulate creating a file with a time stamp
        name = datetime.datetime.now().isoformat() + ".txt"
        fo = open(name, "wb")

        #Build the correct command string
        command = "aws s3 mv " + name +" s3://alex-ec2-usage-data"

        #New command
        #command = "aws s3 mv . s3://alex-ec2-usage-data --exclude \"*\" --include \"*.txt\" "

        #send file to S3 with AWS CLI
        subprocess.call(command , shell=True)

    else:
        return 0

if __name__ == "__main__":
    main()