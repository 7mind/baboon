plugins {
    kotlin("jvm") version "2.1.0"
    kotlin("plugin.serialization") version "2.1.0"
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.8.0")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.10.1")
}

application {
    mainClass.set("example.DriverKt")
}

kotlin {
    jvmToolchain(21)
}

sourceSets {
    main {
        kotlin {
            srcDir("src/main/kotlin/generated")
        }
    }
}
