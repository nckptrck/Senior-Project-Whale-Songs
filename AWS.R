
#This is an explanation on how we were able to call the aws files into r without having to go to the aws s3 bucket and download manually. 
#You will need to set up an .Renviron file that contains your access key and secret access key.
#This can be created in the IAM section of the aws server after clicking on your user. 


# This is a list of the commands you can call 
###### ls("package:aws.s3")

###### bucketlist()
# should see this in the console after creating that .Renviron file and restarting R assuming your credentials are correct

#                                      Bucket             CreationDate
# 1 sagemaker-studio-959616474350-5j0o6n5im4x 2022-02-24T05:00:27.000Z
# 2 sagemaker-studio-959616474350-9bazi65sp9o 2022-02-24T05:00:06.000Z
# 3 sagemaker-studio-959616474350-nb4ahw4xrai 2022-02-24T05:03:21.000Z
# 4          sagemaker-us-west-2-959616474350 2024-11-10T07:13:32.000Z
# 5          summer2024-sagemaker-data-bucket 2024-11-13T21:47:28.000Z
# 6                          whale-recordings 2024-11-14T01:46:38.000Z



# example object  <- "CPhydrophone/Avila/Deployment 2/selection-tables/6805.230201090825-SS.txt"
# you only need to grab the name when using the functions below ex.   6805.230201090825-SS  


# this one is for txt files 
# it does not save the file locally just in the r environment

grab_txt_files <- function(name, Bucket = "s3://whale-recordings/", Place = "Avila", number = "2"){
  Object <- paste0("CPhydrophone/", Place, "/Deployment ", number, "/selection-tables/", name, ".txt")
  
  if (object_exists(object = Object, bucket = Bucket,
    key = Sys.getenv("AWS_ACCESS_KEY_ID"), secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    region = Sys.getenv("AWS_DEFAULT_REGION")) == TRUE ){
    
    table <- get_object(object = Object, bucket = Bucket,
                        key = Sys.getenv("AWS_ACCESS_KEY_ID"), secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                        region = Sys.getenv("AWS_DEFAULT_REGION"))
    
    file <- rawToChar(table)
    txt <- read_tsv(file)
    
    colnames(txt) <- colnames(txt) %>%
      gsub("Begin Time \\(s\\)", "begin_time", .) %>%
      gsub("End Time \\(s\\)", "end_time", .)
    
    return(txt)
  }
  return ("The name is not correct")
}

# how to use 

#  file_name_you_want <- grab_txt_files("6805.230201090825-SS")

# This one is for the wav files 

grab_wav_files <- function(name, Bucket = "s3://whale-recordings/", Place = "Avila", number = "2"){
  Object <- paste0("CPhydrophone/",Place, "/Deployment ", number, "/wav-files/fullsize_files/", name)
  
  if (object_exists(object = Object, bucket = Bucket,
                    key = Sys.getenv("AWS_ACCESS_KEY_ID"), secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                    region = Sys.getenv("AWS_DEFAULT_REGION")) == TRUE ){
    
    table <- get_object(object = Object, bucket = Bucket, as = "raw",
                        key = Sys.getenv("AWS_ACCESS_KEY_ID"), secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                        region = Sys.getenv("AWS_DEFAULT_REGION"))
    writeBin(table, name)
    return("Done")
  }
  return ("The name is not correct")
}

# how to use 
#   grab_wav_files("6805.230201090825.wav") just downloads the files


#Testing 


# Bucket <- "s3://whale-recordings/"
# table <- get_object(
#   object = "CPhydrophone/Avila/Deployment 2/selection-tables/6805.230201090825-SS.txt",
#   bucket = Bucket,
#   key = Sys.getenv("AWS_ACCESS_KEY_ID"),
#   secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
#   region = Sys.getenv("AWS_DEFAULT_REGION")
# )
# 
# file <- rawToChar(table)
# txt <- read_tsv(file)


# files <- get_bucket_df(
#   bucket = Bucket,
#   key = Sys.getenv("AWS_ACCESS_KEY_ID"),
#   secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
#   region = Sys.getenv("AWS_DEFAULT_REGION"),
#   max = 20
# ) |> 
#   as_tibble()