def local_to_ee(gcs_project, bucket, file_name, full_file_path, asset_ID): 
  # Import ee and geemap
  # Then initialize earth engine session
  import ee
  ee.Initialize()
  # Import storage from google.cloud.
  # Import os to set "Google-Earth-Engine-Project" as default 
  from google.cloud import storage
  import os
  
  os.environ.setdefault("GCLOUD_PROJECT", str(gcs_project))

  # Define bucket to store uploaded files
  storage_client = storage.Client()
  bucket = storage_client.bucket(str(bucket))

  # Instantiate a blob object in the defined GCS bucket
  blob = bucket.blob(str(file_name))

  # Upload local file to GCS
  blob.upload_from_filename(str(full_file_path))

  os.system(" ".join([
    "earthengine upload table", 
    "--asset_id=" + str(asset_ID),
    "gs://" + str(bucket) + str(file_name), 
    "--force"]))
