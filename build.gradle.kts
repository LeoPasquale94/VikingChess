import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    java
    scala
    kotlin("jvm") version "1.3.72"
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
    implementation(kotlin("stdlib-jdk8"))
    testCompile("org.scalacheck:scalacheck_2.11:1.12.5")
    testCompile("com.novocode", "junit-interface", "0.11")
    testCompile("org.scalamock:scalamock-scalatest-support_2.8.1:2.1")
    testCompile("org.scalatest:scalatest_2.11:3.0.5")
}

tasks.withType<ScalaCompile> {
    sourceCompatibility = "1.8"
    targetCompatibility = "1.8"
}

val compileKotlin: KotlinCompile by tasks
compileKotlin.kotlinOptions {
    jvmTarget = "1.8"
}

val compileTestKotlin: KotlinCompile by tasks
compileTestKotlin.kotlinOptions {
    jvmTarget = "1.8"
}