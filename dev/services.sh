#!/bin/bash

err() {
    echo $1 2>&1
    exit 1
}

if [ "${OSTYPE:0:6}" = "darwin" ]; then
	GL=$(which greadlink)
	BASEDIR="$(dirname $0)/.."
	if [ $? -eq 0 ]; then
		if [ $(greadlink --version | head -c 8) = "readlink" ]; then
			BASEDIR=$(greadlink -f "$(dirname $0)/..")
		fi
	fi
else
	BASEDIR=$(readlink -f "$(dirname $0)/..")
fi

if [ "" = "$(which docker)" ]; then
    err "docker not found (https://docs.docker.com/)"
fi

if [ "" = "$(docker info | grep 'Containers:')" ]; then
    err "docker-machine not running. Run 'docker-machine start <name>' (https://docs.docker.com/machine/install-machine/)"
fi

if [ "" = "$(which terraform)" ]; then
    err "terraform not found (https://terraform.io/docs/index.html)"
fi

cd $BASEDIR/dev

case "$1" in
    "start")
        terraform apply
        echo ""
        echo "Docker containers graylog2/allinone and mongo have been started using terraform"
        echo "You can find the documentation here: https://docs.docker.com/ and https://terraform.io/docs/index.html"
        echo "To remove the services from your docker host, you can use '$0 destroy'"
        ;;
    "destroy")
        terraform destroy
        echo ""
        echo "Done"
        ;;
    "help")
        echo "Use this script to start a graylog and a mongo docker container, which you can use for development."
        echo "You must have installed the following software to use this script:"
        echo "  terraform - https://terraform.io"
        echo "  docker - https://www.docker.com/"
        echo ""
        echo "To start the services use: $0 start"
        echo "To stop and destroy the service containers use: $0 stop"
        echo "Or use the usual docker commands"
        ;;
    *)
        echo "Invalid command"
        exit 1
        ;;
esac