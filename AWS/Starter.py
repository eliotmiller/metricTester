#!/usr/bin/python

__author__ = 'alexf4'
import sys
import datetime
import subprocess
import os
import time

#This is the entry point into the python program
def main():

    sys.path.append("/usr/local/bin/aws")
    #check to see if temp file exists
    if os.path.exists("/home/ubuntu/running") is False:
        tempfile = open("/home/ubuntu/running", "wb")

    #This will be an infinite loop
    #while(True):


        #run command the command line
        subprocess.call("echo Hello World", shell=True)

        subprocess.call("nohup /usr/bin/R --vanilla < simpleScript.R >& output.Rout &", shell=True)

        #Hack to make sure the correct file has enough time to be made
        time.sleep(1)

        #Set the command var to empty
        command = ""

        # a for loop that will create a list of all the files and send the .RDS file to S3
        for subdir, dirs, files in os.walk('./'):
            for file in files:
                if file.endswith(".RDS"):
                    command = "/usr/local/bin/aws s3 mv /home/ubuntu/" + file +" s3://alex-ec2-usage-data"


        #send file to S3 with AWS CLI
        subprocess.call(command , shell=True)

        #run command the command line
        subprocess.call("echo Hello World2", shell=True)

    return 0

if __name__ == "__main__":
    main()
