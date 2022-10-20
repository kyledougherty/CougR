import ee
import os
from google.cloud import storage

def gcs_ee_setup(gcs_project):  
  # Initialize earth engine session and set 
  # default GCS project
  ee.Initialize()
  os.environ.setdefault("GCLOUD_PROJECT", gcs_project)

def local_shp_to_ee(gcs_bucket, file_name, full_file_path, asset_ID): 
  # Define bucket to store uploaded files
  storage_client = storage.Client()
  bucket = storage_client.bucket(gcs_bucket)

  # Instantiate a blob object in the defined GCS bucket
  blob = bucket.blob(file_name)

  # Upload local file to GCS
  blob.upload_from_filename(full_file_path)

  os.system(" ".join([
    "earthengine upload table", 
    "--asset_id=" + asset_ID,
    "gs://" + gcs_bucket + "/" + file_name, 
    "--force"]))

def local_raster_to_ee(gcs_bucket, file_name, full_file_path, asset_ID): 
  # Define bucket to store uploaded files
  storage_client = storage.Client()
  bucket = storage_client.bucket(gcs_bucket)

  # Instantiate a blob object in the defined GCS bucket
  blob = bucket.blob(file_name)

  # Upload local file to GCS
  blob.upload_from_filename(full_file_path)

  os.system(" ".join([
    "earthengine upload image", 
    "--asset_id=" + asset_ID,
    "gs://" + gcs_bucket + "/" + file_name, 
    "--force"]))
