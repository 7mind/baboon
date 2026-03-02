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

application {
    mainClass.set("example.MainKt")
}
