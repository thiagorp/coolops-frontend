#!/bin/sh

set -e

echo $GCLOUD_KEY_JSON | base64 -d > gcloud-service-key.json
gcloud auth activate-service-account --key-file gcloud-service-key.json

gcloud config set project $PROJECT_ID
gcloud config set compute/zone $COMPUTE_ZONE
gcloud container clusters get-credentials $CLUSTER_NAME

kubectl set image deployment/app-frontend -n $CLUSTER_NAMESPACE app-frontend=$IMAGE_NAME
