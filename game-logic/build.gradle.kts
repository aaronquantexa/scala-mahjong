plugins {
    scala
    id("com.github.maiflai.scalatest") version "0.30"
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.scala-lang:scala-library:2.13.5")
    implementation(project(":tile-model"))

    testImplementation("org.scalatest:scalatest_2.13:3.2.7")
    testRuntimeOnly ("com.vladsch.flexmark:flexmark-all:0.35.10")
}