library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tibble)
library(magick)
library(aws.s3)

# This is a list of the commands you call call 
ls("package:aws.s3")


# use this it test if you get back the correct code
bucket_exists(
  bucket = "s3://herbariumnsw-pds/", 
  region = "ap-southeast-2"
)
# [1] TRUE
# attr(,"x-amz-id-2")

#Change this based on how deep you want to go
Bucket <- "s3://whale-recordings/CPhydrophone/Avila/" #check if you can access the bucket

Bucket_Deployment_2 <- "s3://whale-recordings/CPhydrophone/Avila/Deployment 2/wav-files/fullsize_files/"


bucket_exists(
  bucket = Bucket,
  key = AWS_ACCESS_KEY_ID,
  secret = AWS_SECRET_ACCESS_KEY,
  region = AWS_DEFAULT_REGION
)

get_bucket_df(
  bucket = Bucket,
  key = AWS_ACCESS_KEY_ID,
  secret = AWS_SECRET_ACCESS_KEY,
  region = AWS_DEFAULT_REGION
)
