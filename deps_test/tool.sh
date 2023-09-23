#!/bin/sh
# Replace environment variables
IMAGE="deps_test"
DOCKER_PORT="-p 8081:8081"
DOCKER_PROXY=""
if [ ${HTTP_PROXY} ]; then
DOCKER_PROXY=${HTTP_PROXY}
fi
GIT_HOST="github.com"
GIT_IP="140.82.114.4"
APP_VERSION="$(cat VERSION)"
SCP_SRC_FILE="_build/prod/rel/deps_test/deps_test-${APP_VERSION}.tar.gz"
PROJ_DIR="/app"
. ./scripts/docker.sh
. ./scripts/ssh.sh
if [ ${RESTART} ] && [ ${RESTART} -eq 1 ]; then
SCP_DST_DIR=/data/deps_test
APP=deps_test
APP_CMD="${SCP_DST_DIR}/${APP}/bin/$APP"
APP_START="${APP_CMD} daemon"
APP_PID="${APP_CMD} pid"
APP_VER="${APP_CMD} versions"
APP_BAK_NAME=$(date +${APP}-${APP_VERSION}_%Y%m%d.tar.gz)
APP_TAR="cd $SCP_DST_DIR && tar -xf ${APP}-$APP_VERSION.tar.gz -C $APP"
APP_TAR_BAK="mv ${APP}-$APP_VERSION.tar.gz $APP_BAK_NAME"
echo $APP_TAR_BAK;
APP_STOP="${APP_CMD} stop"
APP_BAK="cd $SCP_DST_DIR/$APP && rm -rf releases_bak && mv releases releases_bak"
APP_BAK_RECOVER="mv releases_bak releases"
print "deps_test will be restart!"
else
SCP_DST_DIR=/tmp/
APP=deps_test
print "deps_test will not be restart!"
APP_START="echo ignore"
APP_PID="echo ignore"
APP_VER="echo ignore"
APP_TAR="cd /tmp/ && mkdir -p $APP && tar -xf ${APP}-$APP_VERSION.tar.gz -C $APP"
APP_TAR_BAK="echo ignore"
APP_STOP="echo ignore"
APP_BAK="echo ignore"
APP_BAK_RECOVER="echo ignore"
fi
cp_static(){
     echo_eval cp -r assets/dist/* apps/deps_test/priv/static/
     echo_eval cp -r assets/lib/dist/* apps/deps_test/priv/static/
}
cp_doc(){
    echo_eval cp doc/deps_test_api.html apps/deps_test/priv/static/
}
set_var(){
    # cp_static
    # cp_doc
    export DEPS_TEST_LOG_ROOT="logs"
	export DEPS_TEST_LOG_LEVEL="error"
    export DEPS_TEST_PORT="5001"
    export DEPS_TEST_NODE_NAME="deps_test@127.0.0.1"
    export DEPS_TEST_COOKIE="xxx"
    export DEPS_TEST_REDIS_IP="127.0.0.1"
    export DEPS_TEST_REDIS_PORT="6379"
    export DEPS_TEST_REDIS_PASSWORD="123456"
    export DEPS_TEST_MYSQL_IP="localhost"
    export DEPS_TEST_MYSQL_PORT="3306"
    export DEPS_TEST_MYSQL_DB="deps_test"
    export DEPS_TEST_MYSQL_PASSWORD="123456"
    export DEPS_TEST_MYSQL_USER="root"
}
gen_commit_id(){
    git rev-parse HEAD > GIT_COMMIT_IDS
    git submodule foreach git rev-parse HEAD >> GIT_COMMIT_IDS
}
pull_frontend(){
    echo_eval git submodule update --init --recursive
    echo_eval git submodule foreach git checkout $1
    echo_eval git submodule foreach git pull
}
pull_backend(){
    if [ "$1" != "" ]; then
    echo_eval git checkout $1
    echo_eval git pull
    fi
    gen_commit_id
}
pull(){
    pull_frontend $1
    pull_backend $1
}
# sh tool.sh relup OLD_RELEASE NEW_RELEASE
relup()
{
	rm -rf _build/prod
	git checkout $1 
    cp_static
	rebar3 as prod release
	git checkout $2
    cp_static
	rebar3 as prod release
	rebar3 as prod appup generate
	rebar3 as prod relup tar	
}
replace_os_vars() {
    awk '{
        while(match($0,"[$]{[^}]*}")) {
            var=substr($0,RSTART+2,RLENGTH -3)
            slen=split(var,arr,":-")
            v=arr[1]
            e=ENVIRON[v]
            if(slen > 1 && e=="") {
                i=index(var, ":-"arr[2])
                def=substr(var,i+2)
                gsub("[$]{"var"}",def)
            } else {
                gsub("[$]{"var"}",e)
            }
        }
    }1' < "$1" > "$2"
}
replace_config(){
    set_var
    replace_os_vars config/sys.config.src config/sys.config 
    replace_os_vars config/vm.args.src config/vm.args
}
release(){
    rm -rf _build/prod
    pull $1
    # cp_static
   if [ $1 == "develop" ];then
      cp_doc
    fi
    make tar
}
case $1 in
replace_config) replace_config;;
build_docker) build_docker;;
run_docker) run_docker "sh tool.sh before_script && sh tool.sh consul && make shell";;
tar_docker) tar_docker;;
before_script) before_script;;
sh_docker) sh_docker;;
make_docker) make_docker $2;;
release) release $2;;
tar) release $2
     croc send --code xxxx _build/prod/rel/deps_test/deps_test-*.tar.gz;;
ssh_restart) ssh_restart;;
git_proxy) git_proxy;;
relup) relup $2 $3;;
rebar3)
    CMD="rebar3"
	i=0
	for par in $@; do
	if [ $i -ne 0 ];then
		CMD="$CMD $par"
	fi
	let i=i+1
	done
    CMD=$CMD" author_name='' apache_license=''"
	print "$CMD"
	eval $CMD
	if [ $? -ne 0 ]; then
		echo "eval {$CMD} ERROR !!!!!"
		exit 1
	else
		echo "eval {$CMD} SUCCESS "
	fi
;;
*)
echo "Usage: sh tool.sh [command]"
echo "command:"
echo "replace_config       override sys.config and vm.args"
echo "relup release-0.1.0 release-0.1.1               genarate appup tar"
;;
esac