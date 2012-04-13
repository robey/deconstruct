#!/bin/bash

sbt7_jar=sbt-launch-0.7.5.jar
sbt11_jar=sbt-launch-0.11.2.jar
binaries_url=http://www.lag.net/

if test -f ./build.sbt -o -f ./project/Build.scala; then
  echo "(sbt 11)"
  sbt_jar=$sbt11_jar
else
  sbt_jar=$sbt7_jar
fi

sbt_bin=./$sbt_jar

test \! -f $sbt_bin && curl -o $sbt_bin $binaries_url/$sbt_jar
test -f $sbt_bin || exit 1

java -ea                          \
  $JAVA_OPTS                      \
  -Djava.net.preferIPv4Stack=true \
  -XX:+AggressiveOpts             \
  -XX:+UseParNewGC                \
  -XX:+UseConcMarkSweepGC         \
  -XX:+CMSParallelRemarkEnabled   \
  -XX:+CMSClassUnloadingEnabled   \
  -XX:MaxPermSize=1024m           \
  -XX:SurvivorRatio=128           \
  -XX:MaxTenuringThreshold=0      \
  -Xss8M                          \
  -Xms512M                        \
  -Xmx3G                          \
  -server                         \
  -jar $sbt_bin "$@"
