plugins {
    scala
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.scala-lang:scala-library:2.13.3")
    implementation(project(":tile-model"))

    testImplementation("org.scalatest:scalatest_2.13:3.2.7")
}