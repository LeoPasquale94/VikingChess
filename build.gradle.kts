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
    testImplementation("org.scalacheck:scalacheck_2.11:1.12.5")
    testImplementation("com.novocode", "junit-interface", "0.11")
    testImplementation("org.scalamock:scalamock-scalatest-support_2.8.1:2.1")
    testImplementation("org.scalatest:scalatest_2.11:3.0.5")
    testRuntimeOnly("org.pegdown:pegdown:1.4.2")
}

tasks.withType<ScalaCompile> {
    sourceCompatibility = "1.8"
    targetCompatibility = "1.8"
}

