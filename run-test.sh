#!/bin/sh
for i in $*
do
	echo "Processing $i"
	java -classpath /home/margus/.m2/repository/org/antlr/antlr/3.2/antlr-3.2.jar:/home/margus/.m2/repository/org/antlr/antlr-runtime/3.2/antlr-runtime-3.2.jar:/home/margus/.m2/repository/org/antlr/stringtemplate/3.2.1/stringtemplate-3.2.1.jar:/home/margus/.m2/repository/antlr/antlr/2.7.7/antlr-2.7.7.jar:/home/margus/.m2/repository/org/scala-lang/scala-library/2.8.0/scala-library-2.8.0.jar:/home/margus/.m2/repository/org/scala-lang/scala-compiler/2.8.0/scala-compiler-2.8.0.jar:/home/margus/.m2/repository/simplicitas/simplicitas-tool/1.0.1/simplicitas-tool-1.0.1.jar:/home/margus/dev/dsl/puf/target/puf-lang-tool.jar puf.PufMain --dest target/test $i
done