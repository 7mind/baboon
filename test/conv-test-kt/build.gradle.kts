plugins {
    kotlin("jvm") version "2.1.0"
    application
}

group = "io.septimalmind"
version = "0.1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.8.0")

    testImplementation(kotlin("test"))
    testImplementation("org.junit.jupiter:junit-jupiter:5.11.4")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

tasks.test {
    useJUnitPlatform()
}

kotlin {
    jvmToolchain(21)
}

application {
    mainClass.set("example.CompatMainKt")
}

sourceSets {
    main {
        kotlin {
            srcDir("src/main/kotlin/generated-main")
        }
    }
}
