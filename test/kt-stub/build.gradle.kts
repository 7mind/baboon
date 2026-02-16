plugins {
    kotlin("jvm") version "2.1.0"
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
    compilerOptions {
        jvmTarget.set(org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_21)
    }
}

java {
    sourceCompatibility = JavaVersion.VERSION_21
    targetCompatibility = JavaVersion.VERSION_21
}

sourceSets {
    main {
        kotlin {
            srcDir("src/main/kotlin/generated-main")
            srcDir("src/main/kotlin/generated-fixtures")
        }
    }
    test {
        kotlin {
            srcDir("src/test/kotlin/generated-tests")
        }
    }
}
