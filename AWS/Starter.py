#!/usr/bin/python

__author__ = 'alexf4'
import sys
import subprocess
import os

#This is the entry point into the python program
def main():

    sys.path.append("/usr/local/bin/aws")
    #check to see if temp file exists
    if os.path.exists("/home/ubuntu/running") is False:
        tempfile = open("/home/ubuntu/running", "wb")

        #run command the command line
        subprocess.call("echo Hello World", shell=True)

        subprocess.call("nohup /usr/bin/R --vanilla < simpleScript.R > output.Rout ", shell=True)

        #Set the command var to empty
        command = ""

        # a for loop that will create a list of all the files and send the .RDS file to S3
        for subdir, dirs, files in os.walk('./'):
            for file in files:
                if file.endswith(".RDS"):
                    command = "/usr/local/bin/aws s3 mv /home/ubuntu/" + file +" s3://alex-ec2-usage-data"


        #send file to S3 with AWS CLI
        subprocess.call(command , shell=True)


         #run command the command line if you still need this
        subprocess.call("rm running", shell=True)


        #find the instance id of this ami
        #MAy need the full path of curl
        process = subprocess.Popen(["curl http://169.254.169.254/latest/meta-data/ami-id"], stdout=subprocess.PIPE)
        result = process.communicate()[0]

        #Result should be the instance id - ami-2bb65342

        #might be able to do this - ec2-terminate-instances $(curl -s http://169.254.169.254/latest/meta-data/instance-id)

        #Shut down this instance
        subprocess.call("/usr/local/bin/aws ec2-terminate-instances " + result , shell=True)





    return 0

if __name__ == "__main__":
    main()
