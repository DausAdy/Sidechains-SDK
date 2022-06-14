#!/bin/bash

set -e

export CONTAINER_JAVA_VER="openjdk-11-jdk-headless"
export CONTAINER_SCALA_VER="2.12.12"
export CONTAINER_SCALA_DEB_SHA256SUM="7ecbc3850d8186c0084be37c01cdd987e97328fdd74eb781bf6dc050dba95276"

source ci/setup_env.sh
docker build --pull --no-cache -t zencash/sidechains-sdk-builder ./ci/docker_ci_builder/
bash -c "docker run --rm -v $(pwd):/build -v ${HOME}/key.asc:/key.asc --tmpfs /tmp:uid=$(id -u),gid=$(id -g),exec,mode=1777 \
    --tmpfs /run:uid=$(id -u),gid=$(id -g),exec,mode=1777 -e LOCAL_USER_ID=$(id -u) -e LOCAL_GRP_ID=$(id -g) \
    $(env | grep -E '^CONTAINER_' | sed -n '/^[^\t]/s/=.*//p' | sed '/^$/d' | sed 's/^/-e /g' | tr '\n' ' ') \
    zencash/sidechains-sdk-builder /build/ci/start_ci.sh"
