# MJC MiniJava Compiler [![CircleCI](https://circleci.com/gh/hugoflug/compiler-scala.svg?style=shield&circle-token=26e387d2ddaa9615827de121fb7fec450bf46415)](https://circleci.com/gh/hugoflug/compiler-scala)
Compiler for a small Java subset (MiniJava), written in Scala. Targets JVM bytecode.

## Build instructions
Requirements: [sbt](https://www.scala-sbt.org/1.0/docs/Setup.html) must be installed

Increase JVM stack size:
```bash
export SBT_OPTS="-Xss5M"
```

Build and run tests:
```bash
sbt assembly
```

Alternatively, build without running tests:
```bash
sbt "set test in assembly := {}" assembly
```

If the build succeeds, you should now have a `target/scala-2.12/mjc.jar`.

## Run instructions

```bash
java -jar mjc.jar <file> [<file> ...]
```
