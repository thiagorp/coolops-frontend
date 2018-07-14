PROJECT_ID=fluent-cosine-206514
RELEASE_DOCKER_IMG = gcr.io/${PROJECT_ID}/coolops-app:${VERSION}
BUILD_DOCKER_IMG = coolops-app-build:latest

build_base:
	docker build -t coolopsio/app-buildbase:latest -f Dockerfile.buildbase .
	docker push coolopsio/app-buildbase:latest

auth_container_registry:
	echo ${GCLOUD_KEY_JSON} | base64 --decode --ignore-garbage > ${HOME}/gcloud-service-key.json
	docker login -u _json_key --password-stdin https://gcr.io < ${HOME}/gcloud-service-key.json

build:
	elm-app build

release:
	docker build -t ${RELEASE_DOCKER_IMG} -f Dockerfile.release .

push: auth_container_registry
	docker push ${RELEASE_DOCKER_IMG}
