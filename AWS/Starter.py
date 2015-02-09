
#!/usr/bin/python


__author__ = 'alexf4'
import sys
import datetime
import subprocess
import os


#This is the entry point into the python program
def main():

    sys.path.append("/usr/local/bin/aws")
    #check to see if temp file exists
    if os.path.exists("running") is False:
        tempfile = open("running", "wb")

        #run command the command line. This would be the R commands you need to give out
        subprocess.call("echo Hello World", shell=True)

        #simulate creating a file with a time stamp
        name = datetime.datetime.now().isoformat() + ".txt"
        fo = open(name, "wb")


        #Build the correct command string
        command = "/usr/local/bin/aws s3 mv " + name +" s3://alex-ec2-usage-data"

        #send file to S3 with AWS CLI
        subprocess.call(command , shell=True)

        #rm the running file so the cron job will start this script again.
        subprocess.call("rm running" , shell=True)


if __name__ == "__main__":
    main()