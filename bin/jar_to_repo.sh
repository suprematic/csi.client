#!/bin/bash
cd ..
lein uberjar
mvn deploy:deploy-file -Dfile=./target/csi-0.1.9.jar -DartifactId=csi -Dversion=0.1.9 -DgroupId=csi -Dpackaging=jar -Durl=file:maven_repo