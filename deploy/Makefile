DOCKER_IMAGE=coolopsio/app-deployment:${VERSION}

publish: build push

build:
	docker build -t ${DOCKER_IMAGE} .

push:
	docker push ${DOCKER_IMAGE}
