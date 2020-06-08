plugins {
    java
    scala
}

group = "PPS Hnefatafl"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("junit", "junit", "4.12")
    implementation("org.scala-lang:scala-library:2.12.8")
    implementation("it.unibo.alice.tuprolog:tuprolog:3.1")
}

tasks.withType<ScalaCompile> {
    sourceCompatibility = "1.8"
    targetCompatibility = "1.8"
}