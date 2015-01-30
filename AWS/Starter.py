
#!/usr/bin/python

__author__ = 'alexf4'
import sys
import datetime
import subprocess



#This is the entry point into the python program
def main():

    #run command the command line
    subprocess.call("echo Hello World", shell=True)

    #simulate creating a file with a time stamp
    name = datetime.datetime.now().isoformat() + ".txt"
    fo = open(name, "wb")

    #Build the correct command string
    command = "aws s3 mv " + name +" s3://alex-ec2-usage-data"

    #send file to S3 with AWS CLI
    subprocess.call(command , shell=True)


    return 0

if __name__ == "__main__":
    main()