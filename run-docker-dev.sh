#!/usr/bin/env sh

#####  VOLUME for cachine (faster setup time)  #####
volume_name="stack-cache"

docker volume create $volume_name



#####  Dev containter  #####

container_name="traze-dev"
rebuild=""
command=""
entrypoint=""

# Parsing parameters
while [ "$1" != "" ]; do
  case $1 in
    "-b" | "--build")
      rebuild="true"
      ;;
    "--repl" | "--ghci")
      command="stack ghci"
      entrypoint="--entrypoint="
      ;;
    "-s" | "--shell")
      command="bash"
      entrypoint="--entrypoint="
      ;;
    "-h" | "--help")
      echo "Usage $0 [OPTIONS]"
      echo ""
      echo "   -b   --build   Rebuild the docker image"
      echo "        --repl"
      echo "        --ghci    Start directly into the Stack interactive session"
      echo "   -s   --shell   Start in a shell (bash) in the working directory"
      echo "   -h   --help    Show this message"
      exit 0
  esac
  shift # next parameter
done

# Build the dev image if not done yet
if [ $rebuild ] || [ -z "$(docker image ls -f reference=$container_name -q)" ]; then
  docker build \
    -t $container_name \
    -f Dockerfile.dev \
    --build-arg APP_USER=$(whoami) \
    --build-arg APP_USER_ID=$(id -u $(whoami)) \
    --build-arg APP_USER_GROUP=$(id -gn $(whoami)) \
    --build-arg APP_USER_GROUP_ID=$(id -g $(whoami)) \
    .

  if [ -n "$(docker ps -a -f name=$container_name -q)" ]; then
    # delete the old container
    docker rm -f $container_name
  fi
fi

if [ -z "$(docker ps -a -f name=$container_name -q)" ]; then
  # Container has not been run yet
  docker run \
    -it \
    -v $(pwd):/usr/src/traze \
    -v $volume_name:/home/$(whoami)/.stack \
    --name $container_name \
    $entrypoint \
    $container_name \
    $command
elif [ -n "$(docker ps -f name=$container_name -q)" ]; then
  # Container is already running
  if [ -n "$command" ]; then
    # Execute the command
    docker exec -it $container_name $command
  else
    # Attach to the already running container
    docker attach $container_name
  fi
else
  # Container is here but not running
  if [ -n "$command" ]; then
    # Execute the command
    docker start $container_name
    docker exec -it $container_name $command
  else
    docker start -ai $container_name
  fi
fi
